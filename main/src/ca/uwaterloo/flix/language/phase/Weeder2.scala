/*
 * Copyright 2023 Herluf Baggesen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.UnstructuredTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SemanticOp, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import org.parboiled2.ParserInput

import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}
import java.util.regex.{PatternSyntaxException, Pattern => JPattern}
import scala.collection.immutable.{::, Nil}

// TODO: Add change set support

/**
 * Weeder2 produces a [[WeededAst.Root]] by transforming [[UnstructuredTree.Tree]]s into [[WeededAst.CompilationUnit]]s.
 * [[UnstructuredTree.Tree]] provides few guarantees, which must be kept in mind when reading/modifying Weeder2:
 * 1. Each Tree node will have a [[TreeKind]], [[SourceLocation]] and a list of __zero or more__ children.
 * 2. Each child may be a token or another tree.
 * 3. There is __no__ guarantee on the presence of children, and therefore no guarantee on the amount either.
 * 4. There __is__ a guarantee on the order of children, so if TreeKind.Documentation and TreeKind.Annotation are expected, they will always appear in the same order.
 *
 * Function names in Weeder2 follow this pattern:
 * 1. visit* : These assume that they are called on a Tree of a specified kind and will assert that this is the case.
 * 2. pick* : These will pick __the first__ Child.Tree of a specified kind and run a visitor on it.
 * 3. tryPick* : Works like pick* but only runs the visitor if the child of kind is found. Returns an option containing the result.
 * 3. pickAll* : These will pick __all__ subtrees of a specified kind and run a visitor on it.
 */
object Weeder2 {

  import WeededAst._

  private class State(val src: Ast.Source) {
    // Compute a `ParserInput` when initializing a state for lexing a source.
    // This is necessary to display source code in error messages.
    // See `sourceLocationAtStart` for usage and `SourceLocation` for more information.
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  def run(readRoot: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], trees: Map[Ast.Source, Tree])(implicit flix: Flix): Validation[WeededAst.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.Failure(LazyList.empty)
    }

    flix.phase("Weeder2") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(trees) {
        case (src, tree) => mapN(weed(src, tree))(tree => src -> tree)
      }

      mapN(sequence(results))(_.toMap).map(m => WeededAst.Root(m, entryPoint, readRoot.names))
    }
  }

  def weed(src: Ast.Source, tree: Tree): Validation[CompilationUnit, CompilationMessage] = {
    implicit val s: State = new State(src)
    mapN(
      pickAllUsesAndImports(tree),
      pickAllDeclarations(tree)
    ) {
      case (usesAndImports, declarations) => CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def pickAllUsesAndImports(tree: Tree): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def pickAllDeclarations(tree: Tree)(implicit s: State): Validation[List[Declaration], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    val definitions = pickAll(TreeKind.Def, tree.children)
    val classes = pickAll(TreeKind.Class, tree.children)
    val instances = pickAll(TreeKind.Instance, tree.children)
    mapN(
      traverse(definitions)(visitDefinition),
      traverse(classes)(visitTypeClass),
      traverse(instances)(visitInstance)
    ) {
      case (definitions, classes, instances) => definitions ++ classes ++ instances
    }
  }

  private def visitTypeClass(tree: Tree)(implicit s: State): Validation[Declaration.Class, CompilationMessage] = {
    assert(tree.kind == TreeKind.Class)
    flatMapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree),
      pickNameIdent(tree),
      pickSingleTypeParameter(tree),
      pickAllSignatures(tree),
    ) {
      case (doc, annotations, modifiers, ident, tparam, sigs) =>
        mapN(pickAllAssociatedTypesSig(tree, tparam)) {
          assocs => Declaration.Class(doc, annotations, modifiers, ident, tparam, List.empty, assocs, sigs, List.empty, tree.loc)
        }

    }
  }

  private def visitInstance(tree: Tree)(implicit s: State): Validation[Declaration.Instance, CompilationMessage] = {
    assert(tree.kind == TreeKind.Instance)
    flatMapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree),
      pickQName(tree),
      pickType(tree),
      pickTypeConstraints(tree),
      traverse(pickAll(TreeKind.Def, tree.children))(visitDefinition),
    ) {
      case (doc, annotations, modifiers, clazz, tpe, tconstrs, defs) =>
        mapN(pickAllAssociatedTypesDef(tree, tpe)) {
          assocs => Declaration.Instance(doc, annotations, modifiers, clazz, tpe, tconstrs, assocs, defs, tree.loc)
        }
    }
  }

  private def visitDefinition(tree: Tree)(implicit s: State): Validation[Declaration.Def, CompilationMessage] = {
    assert(tree.kind == TreeKind.Def)
    mapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree),
      pickNameIdent(tree),
      pickKindedTypeParameters(tree),
      pickFormalParameters(tree),
      pickExpression(tree),
      pickType(tree),
      pickTypeConstraints(tree),
      pickConstraints(tree),
      tryPickEffect(tree)
    ) {
      case (doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
        Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }
  }

  private def pickDocumentation(tree: Tree): Validation[Ast.Doc, CompilationMessage] = {
    val docTree = tryPick(TreeKind.Doc, tree.children)
    val loc = docTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    val comments = docTree
      // strip prefixing `///` and trim
      .map(tree => text(tree).map(_.stripPrefix("///").trim))
      // Drop first/last line if it is empty
      .map(lines => if (lines.headOption.exists(_.isEmpty)) lines.tail else lines)
      .map(lines => if (lines.lastOption.exists(_.isEmpty)) lines.dropRight(1) else lines)
      .getOrElse(List.empty)
    Ast.Doc(comments, loc).toSuccess
  }

  private def pickAnnotations(tree: Tree)(implicit s: State): Validation[Ast.Annotations, CompilationMessage] = {
    val maybeAnnotations = tryPick(TreeKind.Annotations, tree.children)
    val annotations = maybeAnnotations.map(
        tree => {
          val tokens = pickAllTokens(tree)
          // Check for duplicate annotations
          val errors = getDuplicates(tokens.toSeq, (t: Token) => t.text).map(pair => {
            val name = pair._1.text
            val loc1 = pair._1.mkSourceLocation(s.src, Some(s.parserInput))
            val loc2 = pair._2.mkSourceLocation(s.src, Some(s.parserInput))
            DuplicateAnnotation(name.stripPrefix("@"), loc1, loc2)
          })
          traverse(tokens)(visitAnnotation).withSoftFailures(errors)
        })
      .getOrElse(List.empty.toSuccess)

    mapN(annotations)(Ast.Annotations(_))
  }

  private def visitAnnotation(token: Token)(implicit s: State): Validation[Ast.Annotation, CompilationMessage] = {
    val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
    import Ast.Annotation._
    token.text match {
      case "@benchmark" | "@Benchmark" => Benchmark(loc).toSuccess
      case "@Deprecated" => Deprecated(loc).toSuccess
      case "@Experimental" => Experimental(loc).toSuccess
      case "@Internal" => Internal(loc).toSuccess
      case "@Parallel" => Parallel(loc).toSuccess
      case "@ParallelWhenPure" => ParallelWhenPure(loc).toSuccess
      case "@Lazy" => Lazy(loc).toSuccess
      case "@LazyWhenPure" => LazyWhenPure(loc).toSuccess
      case "@MustUse" => MustUse(loc).toSuccess
      case "@Skip" => Skip(loc).toSuccess
      case "@Test" | "@test" => Test(loc).toSuccess
      case "@TailRec" => TailRecursive(loc).toSuccess
      case other => softFailWith(Ast.Annotation.Error(other.stripPrefix("@"), loc))
    }
  }

  private val ALL_MODIFIERS: Set[TokenKind] = Set(
    TokenKind.KeywordSealed,
    TokenKind.KeywordLawful,
    TokenKind.KeywordPub,
    TokenKind.KeywordOverride,
    TokenKind.KeywordInline)

  private def pickModifiers(tree: Tree, allowed: Set[TokenKind] = ALL_MODIFIERS, mustBePublic: Boolean = false)(implicit s: State): Validation[Ast.Modifiers, CompilationMessage] = {
    tryPick(TreeKind.Modifiers, tree.children) match {
      case None => Ast.Modifiers(List.empty).toSuccess
      case Some(modTree) =>
        var errors: List[CompilationMessage] = List.empty
        val tokens = pickAllTokens(modTree)
        // Check if pub is missing
        if (mustBePublic && !tokens.exists(_.kind == TokenKind.KeywordPub)) {
          mapN(pickNameIdent(tree)) {
            ident => errors :+= IllegalPrivateDeclaration(ident, ident.loc)
          }
        }

        // Check for duplicate modifiers
        errors = errors ++ getDuplicates(tokens.toSeq, (t: Token) => t.kind).map(pair => {
          val name = pair._1.text
          val loc1 = pair._1.mkSourceLocation(s.src, Some(s.parserInput))
          val loc2 = pair._2.mkSourceLocation(s.src, Some(s.parserInput))
          DuplicateModifier(name, loc2, loc1)
        })

        traverse(tokens)(visitModifier(_, allowed))
          .withSoftFailures(errors)
          .map(Ast.Modifiers(_))
    }
  }

  private def visitModifier(token: Token, allowed: Set[TokenKind])(implicit s: State): Validation[Ast.Modifier, CompilationMessage] = {
    val mod = token.kind match {
      // TODO: there is no Ast.Modifier for 'inline'
      case TokenKind.KeywordSealed => Ast.Modifier.Sealed.toSuccess
      case TokenKind.KeywordLawful => Ast.Modifier.Lawful.toSuccess
      case TokenKind.KeywordPub => Ast.Modifier.Public.toSuccess
      case TokenKind.KeywordOverride => Ast.Modifier.Override.toSuccess
      // TODO: This could be a SoftFailure if we had Ast.Modifier.Error
      case kind => failWith(s"unsupported modifier $kind")
    }
    if (!allowed.contains(token.kind)) {
      mod.withSoftFailure(IllegalModifier(token.mkSourceLocation(s.src, Some(s.parserInput))))
    } else {
      mod
    }
  }

  private def pickTypeParameters(tree: Tree)(implicit s: State): Validation[TypeParams, CompilationMessage] = {
    tryPick(TreeKind.TypeParameters, tree.children) match {
      case None => TypeParams.Elided.toSuccess
      case Some(tparamsTree) =>
        val parameters = pickAll(TreeKind.Parameter, tparamsTree.children)
        flatMapN(traverse(parameters)(visitTypeParameter)) {
          tparams =>
            val kinded = tparams.collect { case t: TypeParam.Kinded => t }
            val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
            (kinded, unkinded) match {
              // Only unkinded type parameters
              case (Nil, _ :: _) => TypeParams.Unkinded(unkinded).toSuccess
              // Only kinded type parameters
              case (_ :: _, Nil) => TypeParams.Kinded(kinded).toSuccess
              // Some kinded and some unkinded type parameters. We recover by kinding the unkinded ones as Ambiguous.
              case (_ :: _, _ :: _) =>
                val syntheticallyKinded = tparams.map {
                  case p: TypeParam.Kinded => p
                  case TypeParam.Unkinded(ident) => TypeParam.Kinded(ident, defaultKind(ident))
                }
                toSoftFailure(TypeParams.Kinded(syntheticallyKinded), MismatchedTypeParameters(tparamsTree.loc))
              case (Nil, Nil) =>
                throw InternalCompilerException("Parser produced empty type parameter tree", tparamsTree.loc)
            }
        }
    }
  }

  private def pickKindedTypeParameters(tree: Tree)(implicit s: State): Validation[KindedTypeParams, CompilationMessage] = {
    tryPick(TreeKind.TypeParameters, tree.children) match {
      case None => TypeParams.Elided.toSuccess
      case Some(tparamsTree) =>
        val parameters = pickAll(TreeKind.Parameter, tparamsTree.children)
        flatMapN(traverse(parameters)(visitTypeParameter)) {
          tparams =>
            val kinded = tparams.collect { case t: TypeParam.Kinded => t }
            val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
            (kinded, unkinded) match {
              // Only kinded type parameters
              case (_ :: _, Nil) => TypeParams.Kinded(kinded).toSuccess
              // Some kinded and some unkinded type parameters. We recover by kinding the unkinded ones as Ambiguous.
              case (_, _ :: _) =>
                val errors = unkinded.map {
                  case TypeParam.Unkinded(ident) => MissingTypeParamKind(ident.loc)
                }
                val syntheticallyKinded = tparams.map {
                  case p: TypeParam.Kinded => p
                  case TypeParam.Unkinded(ident) =>
                    TypeParam.Kinded(ident, Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic))
                }
                toSuccessOrSoftFailure(TypeParams.Kinded(syntheticallyKinded), errors)
              case (Nil, Nil) =>
                throw InternalCompilerException("Parser produced empty type parameter tree", tparamsTree.loc)
            }
        }
    }
  }

  private def pickSingleTypeParameter(tree: Tree)(implicit s: State): Validation[TypeParam, CompilationMessage] = {
    val tparams = pick(TreeKind.TypeParameters, tree.children)
    flatMapN(tparams) {
      tparams => flatMapN(pick(TreeKind.Parameter, tparams.children))(visitTypeParameter)
    }
  }

  private def visitTypeParameter(tree: Tree)(implicit s: State): Validation[TypeParam, CompilationMessage] = {
    assert(tree.kind == TreeKind.Parameter)
    mapN(pickNameIdent(tree)) {
      ident =>
        tryPickKind(tree)
          .map(kind => TypeParam.Kinded(ident, kind))
          .getOrElse(TypeParam.Unkinded(ident))
    }
  }

  private def tryPickKind(tree: Tree)(implicit s: State): Option[Kind] = {
    tryPick(TreeKind.Kind, tree.children).flatMap(
      kindTree => {
        val ident = pickNameIdent(kindTree) match {
          case Success(t) => Some(t)
          case SoftFailure(t, _) => Some(t) // TODO: This should maybe result in None instead
          case Failure(_) => None
        }

        ident.map(ident => {
          val k = Kind.Ambiguous(Name.mkQName(ident), ident.loc)
          tryPickKind(kindTree)
            .map(Kind.Arrow(k, _, kindTree.loc))
            .getOrElse(k)
        })
      })
  }

  private def pickAllAssociatedTypesDef(tree: Tree, instType: Type)(implicit s: State): Validation[List[Declaration.AssocTypeDef], CompilationMessage] = {
    val assocs = pickAll(TreeKind.AssociatedTypeDef, tree.children)
    traverse(assocs)(visitAssociatedTypeDef(_, instType))
  }

  private def pickAllAssociatedTypesSig(tree: Tree, classTypeParam: TypeParam)(implicit s: State): Validation[List[Declaration.AssocTypeSig], CompilationMessage] = {
    val assocs = pickAll(TreeKind.AssociatedTypeSig, tree.children)
    traverse(assocs)(visitAssociatedTypeSig(_, classTypeParam))
  }

  private def visitAssociatedTypeDef(tree: Tree, instType: Type)(implicit s: State): Validation[Declaration.AssocTypeDef, CompilationMessage] = {
    assert(tree.kind == TreeKind.AssociatedTypeDef)
    flatMapN(
      pickDocumentation(tree),
      pickModifiers(tree),
      pickNameIdent(tree),
      pickTypeArguments(tree),
      pickType(tree)
    ) {
      case (doc, mods, ident, typeArgs, tpe) =>
        val typeArg = typeArgs match {
          // Use instance type if type arguments were elided
          case Nil => instType.toSuccess
          // Single argument: use that
          case head :: Nil => head.toSuccess
          // Multiple type arguments: recover by arbitrarily picking the first one
          case types => Validation.toSoftFailure(types.head, NonUnaryAssocType(types.length, ident.loc))
        }

        mapN(typeArg) {
          typeArg => Declaration.AssocTypeDef(doc, mods, ident, typeArg, tpe, tree.loc)
        }
    }
  }

  private def visitAssociatedTypeSig(tree: Tree, classTypeParam: TypeParam)(implicit s: State): Validation[Declaration.AssocTypeSig, CompilationMessage] = {
    assert(tree.kind == TreeKind.AssociatedTypeSig)
    flatMapN(
      pickDocumentation(tree),
      pickModifiers(tree),
      pickNameIdent(tree),
      pickTypeParameters(tree),
    ) {
      case (doc, mods, ident, tparams) =>
        val kind = tryPickKind(tree).getOrElse(defaultKind(ident))
        val tparam = tparams match {
          // Elided: Use class type parameter
          case TypeParams.Elided => classTypeParam.toSuccess
          // Single type parameter
          case TypeParams.Unkinded(head :: Nil) => head.toSuccess
          case TypeParams.Kinded(head :: Nil) => head.toSuccess
          // Multiple type parameters. Soft fail by picking the first parameter
          case TypeParams.Unkinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
          case TypeParams.Kinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
        }
        mapN(tparam) {
          tparam => Declaration.AssocTypeSig(doc, mods, ident, tparam, kind, tree.loc)
        }
    }
  }

  private def pickAllSignatures(tree: Tree)(implicit s: State): Validation[List[Declaration.Sig], CompilationMessage] = {
    val sigs = pickAll(TreeKind.Signature, tree.children)
    traverse(sigs)(visitSignature)
  }

  private def visitSignature(tree: Tree)(implicit s: State): Validation[Declaration.Sig, CompilationMessage] = {
    assert(tree.kind == TreeKind.Signature)
    val maybeExpression = tryPick(TreeKind.Expr.Expr, tree.children)
    mapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree, allowed = Set(TokenKind.KeywordSealed, TokenKind.KeywordLawful, TokenKind.KeywordPub), mustBePublic = true),
      pickNameIdent(tree),
      pickKindedTypeParameters(tree),
      pickFormalParameters(tree),
      pickType(tree),
      tryPickEffect(tree),
      pickTypeConstraints(tree),
      traverseOpt(maybeExpression)(visitExpression)
    ) {
      case (doc, annotations, modifiers, ident, tparams, fparams, tpe, eff, tconstrs, expr) =>
        Declaration.Sig(doc, annotations, modifiers, ident, tparams, fparams, expr, tpe, eff, tconstrs, tree.loc)
    }
  }

  private def unitFormalParams(loc: SourceLocation): List[FormalParam] = List(
    FormalParam(
      Name.Ident(SourcePosition.Unknown, "_unit", SourcePosition.Unknown),
      Ast.Modifiers(List.empty),
      Some(Type.Unit(loc)),
      loc
    )
  )

  private def pickFormalParameters(tree: Tree, presence: Presence = Presence.Required)(implicit s: State): Validation[List[FormalParam], CompilationMessage] = {
    val paramTree = tryPick(TreeKind.Parameters, tree.children)
    paramTree.map(
      t => {
        val params = pickAll(TreeKind.Parameter, t.children)
        if (params.isEmpty)
          unitFormalParams(t.loc).toSuccess
        else {
          flatMapN(traverse(params)(visitParameter(_, presence))) {
            params =>
              // Check for duplicates
              val paramsWithoutWildcards = params.filter(_.ident.name.startsWith("_"))
              val errors = getDuplicates(paramsWithoutWildcards, (p: FormalParam) => p.ident.name)
                .map(pair => DuplicateFormalParam(pair._1.ident.name, pair._1.loc, pair._2.loc))

              // Check missing or illegal type ascription

              params.toSuccess.withSoftFailures(errors)
          }
        }
      }
    ).getOrElse(
      // Soft fail by pretending there were no arguments
      softFailWith(unitFormalParams(SourceLocation.Unknown))
    )
  }

  private def visitParameter(tree: Tree, presence: Presence)(implicit s: State): Validation[FormalParam, CompilationMessage] = {
    assert(tree.kind == TreeKind.Parameter)
    flatMapN(
      pickNameIdent(tree),
      pickModifiers(tree)
    ) {
      case (ident, mods) =>
        val maybeType = tryPick(TreeKind.Type.Type, tree.children)
        // Check for missing or illegal type ascription
        (maybeType, presence) match {
          case (None, Presence.Required) => Validation.toHardFailure(MissingFormalParamAscription(ident.name, tree.loc))
          case (Some(_), Presence.Forbidden) => Validation.toHardFailure(IllegalFormalParamAscription(tree.loc))
          case (Some(typeTree), _) => mapN(visitType(typeTree)) { tpe => FormalParam(ident, mods, Some(tpe), tree.loc) }
          case (None, _) => FormalParam(ident, mods, None, tree.loc).toSuccess
        }
    }
  }

  private def pickTypeConstraints(tree: Tree)(implicit s: State): Validation[List[TypeConstraint], CompilationMessage] = {
    val maybeWithClause = tryPick(TreeKind.WithClause, tree.children)
    maybeWithClause
      .map(withClauseTree => {
        val constraints = pickAll(TreeKind.Type.Constraint, withClauseTree.children)
        traverse(constraints)(visitTypeConstraint)
      }).getOrElse(List.empty.toSuccess)
  }

  private def visitTypeConstraint(tree: Tree)(implicit s: State): Validation[TypeConstraint, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Constraint)
    mapN(
      pickQName(tree),
      pickType(tree)
    ) {
      (qname, tpe) => TypeConstraint(qname, tpe, tree.loc)
    }
  }

  private def pickConstraints(tree: Tree): Validation[List[EqualityConstraint], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def pickExpression(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    val maybeExpression = tryPick(TreeKind.Expr.Expr, tree.children)
    flatMapN(
      traverseOpt(maybeExpression)(visitExpression)
    ) {
      case Some(expr) => expr.toSuccess
      case None => softFailWith(Expr.Error(Parse2Error.DevErr(tree.loc, "expected expression")))
    }
  }

  private def visitExpression(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Expr)
    tree.children(0) match {
      case Child.Tree(tree) => tree.kind match {
        case TreeKind.Expr.Literal => visitLiteral(tree)
        case TreeKind.Expr.Tuple => visitExprTuple(tree)
        case TreeKind.Expr.Call => visitExprCall(tree)
        case TreeKind.Expr.LetImport => visitLetImport(tree)
        case TreeKind.Expr.Binary => visitExprBinary(tree)
        case TreeKind.Expr.Paren => visitExprParen(tree)
        case TreeKind.Expr.IfThenElse => visitExprIfThenElse(tree)
        case TreeKind.Ident => mapN(tokenToIdent(tree)) { ident => Expr.Ambiguous(Name.mkQName(ident), tree.loc) }
        case TreeKind.QName => mapN(visitQName(tree))(qname => Expr.Ambiguous(qname, tree.loc))
        case kind => failWith(s"TODO: implement expression of kind '$kind'", tree.loc)
      }
      case _ => throw InternalCompilerException(s"Expr.Expr had token child", tree.loc)
    }
  }

  private def visitExprTuple(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Tuple)
    val expressions = pickAll(TreeKind.Expr.Expr, tree.children)
    mapN(traverse(expressions)(visitExpression))(Expr.Tuple(_, tree.loc))
  }

  private def visitExprIfThenElse(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.IfThenElse)
    val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
    exprs match {
      case exprCondition :: exprThen :: exprElse :: Nil =>
        mapN(
          visitExpression(exprCondition),
          visitExpression(exprThen),
          visitExpression(exprElse)
        ) {
          case (condition, tthen, eelse) => Expr.IfThenElse(condition, tthen, eelse, tree.loc)
        }
      case _ => softFailWith(Expr.Error(Parse2Error.DevErr(tree.loc, "Malformed if-then-else expression")))
    }
  }

  private def visitExprBinary(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Binary)
    val exprs = traverse(pickAll(TreeKind.Expr.Expr, tree.children))(visitExpression)
    val op = pick(TreeKind.Operator, tree.children)
    mapN(op, exprs) {
      case (op, e1 :: e2 :: Nil) =>
        val isInfix = op.children.head match {
          case Child.Token(token) => token.kind == TokenKind.InfixFunction
          case _ => false
        }

        if (isInfix) {
          val infixName = text(op).head.stripPrefix("`").stripSuffix("`")
          val opExpr = Expr.Ambiguous(Name.mkQName(infixName, op.loc.sp1, op.loc.sp2), op.loc)
          return Expr.Infix(e1, opExpr, e2, tree.loc).toSuccess
        }

        visitBinaryOperator(op) match {
          case OperatorResult.BuiltIn(name) => Expr.Apply(Expr.Ambiguous(name, name.loc), List(e1, e2), tree.loc)
          case OperatorResult.Operator(o) => Expr.Binary(o, e1, e2, tree.loc)
          case OperatorResult.Unrecognized(ident) => Expr.Apply(Expr.Ambiguous(Name.mkQName(ident), ident.loc), List(e1, e2), tree.loc)
        }

      case (op, operands) => throw InternalCompilerException(s"Expr.Binary tree with ${operands.length} operands", tree.loc)
    }
  }

  private def visitBinaryOperator(tree: Tree)(implicit s: State): OperatorResult = {
    assert(tree.kind == TreeKind.Operator)
    val op = text(tree).head
    val sp1 = tree.loc.sp1
    val sp2 = tree.loc.sp2
    op match {
      case "+" => OperatorResult.BuiltIn(Name.mkQName("Add.add", sp1, sp2))
      case "-" => OperatorResult.BuiltIn(Name.mkQName("Sub.sub", sp1, sp2))
      case "*" => OperatorResult.BuiltIn(Name.mkQName("Mul.mul", sp1, sp2))
      case "/" => OperatorResult.BuiltIn(Name.mkQName("Div.div", sp1, sp2))
      case "<" => OperatorResult.BuiltIn(Name.mkQName("Order.less", sp1, sp2))
      case "<=" => OperatorResult.BuiltIn(Name.mkQName("Order.lessEqual", sp1, sp2))
      case ">" => OperatorResult.BuiltIn(Name.mkQName("Order.greater", sp1, sp2))
      case ">=" => OperatorResult.BuiltIn(Name.mkQName("Order.greaterEqual", sp1, sp2))
      case "==" => OperatorResult.BuiltIn(Name.mkQName("Eq.eq", sp1, sp2))
      case "!=" => OperatorResult.BuiltIn(Name.mkQName("Eq.neq", sp1, sp2))
      case "<=>" => OperatorResult.BuiltIn(Name.mkQName("Order.compare", sp1, sp2))
      case "and" => OperatorResult.Operator(SemanticOp.BoolOp.And)
      case "or" => OperatorResult.Operator(SemanticOp.BoolOp.Or)
      case _ => OperatorResult.Unrecognized(Name.Ident(sp1, op, sp2))
    }
  }

  private def visitExprParen(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Paren)
    val expr = pickExpression(tree)
    mapN(expr)(expr => Expr.Tuple(List(expr), tree.loc))
  }

  private def visitLetImport(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.LetImport)
    val jvmOp = flatMapN(pick(TreeKind.JvmOp.JvmOp, tree.children))(JvmOp.visitJvmOp)
    val expr = pickExpression(tree)
    mapN(jvmOp, expr) {
      (jvmOp, expr) => Expr.LetImport(jvmOp, expr, tree.loc)
    }
  }

  private object JvmOp {
    private def pickQNameIdents(tree: Tree)(implicit s: State): Validation[List[String], CompilationMessage] = {
      flatMapN(pick(TreeKind.QName, tree.children)) {
        qname => mapN(traverse(pickAll(TreeKind.Ident, qname.children))(t => text(t).toSuccess))(_.flatten)
      }
    }

    private def pickJavaClassMember(tree: Tree)(implicit s: State): Validation[JavaClassMember, CompilationMessage] = {
      val idents = pickQNameIdents(tree)
      mapN(idents) {
        case prefix :: suffix => JavaClassMember(prefix, suffix, tree.loc)
        case Nil => throw InternalCompilerException("JvmOp empty name", tree.loc)
      }
    }

    private def pickJavaName(tree: Tree)(implicit s: State): Validation[Name.JavaName, CompilationMessage] = {
      val idents = pickQNameIdents(tree)
      mapN(idents) {
        idents => Name.JavaName(tree.loc.sp1, idents, tree.loc.sp2)
      }
    }

    private def pickSignature(tree: Tree)(implicit s: State): Validation[List[Type], CompilationMessage] = {
      flatMapN(pick(TreeKind.JvmOp.Signature, tree.children)) {
        sig => traverse(pickAll(TreeKind.Type.Type, sig.children))(visitType)
      }
    }

    private def pickAscription(tree: Tree)(implicit s: State): Validation[(Type, Option[Type]), CompilationMessage] = {
      val ascription = pick(TreeKind.JvmOp.Ascription, tree.children)
      flatMapN(ascription)(
        ascTree => mapN(pickType(ascTree), tryPickEffect(ascTree))((tpe, eff) => (tpe, eff))
      )
    }

    private def visitMethod(tree: Tree, isStatic: Boolean = false)(implicit s: State): Validation[JvmOp, CompilationMessage] = {
      val fqn = pickJavaClassMember(tree)
      val signature = pickSignature(tree)
      val ascription = pickAscription(tree)
      mapN(fqn, signature, ascription) {
        case (fqn, signature, (tpe, eff)) => if (isStatic)
          WeededAst.JvmOp.StaticMethod(fqn, signature, tpe, eff, None) // TODO: This is not always None
        else
          WeededAst.JvmOp.Method(fqn, signature, tpe, eff, None) // TODO: This is not always None
      }
    }

    private def visitConstructor(tree: Tree)(implicit s: State): Validation[JvmOp, CompilationMessage] = {
      val fqn = pickJavaName(tree)
      val signature = pickSignature(tree)
      val ascription = pickAscription(tree)
      val ident = pickNameIdent(tree)
      mapN(fqn, signature, ascription, ident) {
        case (fqn, signature, (tpe, eff), ident) => WeededAst.JvmOp.Constructor(fqn, signature, tpe, eff, ident)
      }
    }

    private type AstField = (WeededAst.JavaClassMember, Type, Option[WeededAst.Type], Name.Ident) => JvmOp

    private def visitField(tree: Tree, astKind: AstField)(implicit s: State): Validation[JvmOp, CompilationMessage] = {
      val fqn = pickJavaClassMember(tree)
      val ascription = pickAscription(tree)
      val ident = pickNameIdent(tree)
      mapN(fqn, ascription, ident) {
        case (fqn, (tpe, eff), ident) => astKind(fqn, tpe, eff, ident)
      }
    }

    def visitJvmOp(tree: Tree)(implicit s: State): Validation[JvmOp, CompilationMessage] = {
      assert(tree.kind == TreeKind.JvmOp.JvmOp)
      val inner = unfold(tree)
      inner.kind match {
        case TreeKind.JvmOp.Method => visitMethod(inner)
        case TreeKind.JvmOp.Constructor => visitConstructor(inner)
        case TreeKind.JvmOp.StaticMethod => visitMethod(inner, isStatic = true)
        case TreeKind.JvmOp.GetField => visitField(inner, WeededAst.JvmOp.GetField)
        case TreeKind.JvmOp.StaticGetField => visitField(inner, WeededAst.JvmOp.GetStaticField)
        case TreeKind.JvmOp.PutField => visitField(inner, WeededAst.JvmOp.PutField)
        case TreeKind.JvmOp.StaticPutField => visitField(inner, WeededAst.JvmOp.PutStaticField)
        case kind => throw InternalCompilerException(s"child of kind '$kind' under JvmOp.JvmOp", tree.loc)
      }
    }
  }

  private def visitExprCall(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Call)
    flatMapN(pick(TreeKind.Expr.Expr, tree.children), pickArguments(tree)) {
      case (expr, args) =>
        val maybeIntrinsic = tryPick(TreeKind.Expr.Intrinsic, expr.children)
        maybeIntrinsic match {
          case Some(intrinsic) => visitIntrinsic(intrinsic, args)
          case None => mapN(visitExpression(expr))(Expr.Apply(_, args, tree.loc))
        }
    }
  }

  private def visitIntrinsic(tree: Tree, args: List[Expr])(implicit s: State): Validation[Expr, CompilationMessage] = {
    val intrinsic = text(tree).head.stripPrefix("$").stripSuffix("$")
    val loc = tree.loc
    (intrinsic, args) match {
      case ("BOOL_NOT", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.BoolOp.Not, e1, loc).toSuccess
      case ("BOOL_AND", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.BoolOp.And, e1, e2, loc).toSuccess
      case ("BOOL_OR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, loc).toSuccess
      case ("BOOL_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.BoolOp.Eq, e1, e2, loc).toSuccess
      case ("BOOL_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.BoolOp.Neq, e1, e2, loc).toSuccess

      case ("CHAR_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Eq, e1, e2, loc).toSuccess
      case ("CHAR_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Neq, e1, e2, loc).toSuccess
      case ("CHAR_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Lt, e1, e2, loc).toSuccess
      case ("CHAR_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Le, e1, e2, loc).toSuccess
      case ("CHAR_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Gt, e1, e2, loc).toSuccess
      case ("CHAR_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.CharOp.Ge, e1, e2, loc).toSuccess

      case ("FLOAT32_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Float32Op.Neg, e1, loc).toSuccess
      case ("FLOAT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Add, e1, e2, loc).toSuccess
      case ("FLOAT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Sub, e1, e2, loc).toSuccess
      case ("FLOAT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Mul, e1, e2, loc).toSuccess
      case ("FLOAT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Div, e1, e2, loc).toSuccess
      case ("FLOAT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Exp, e1, e2, loc).toSuccess
      case ("FLOAT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Eq, e1, e2, loc).toSuccess
      case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Neq, e1, e2, loc).toSuccess
      case ("FLOAT32_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Lt, e1, e2, loc).toSuccess
      case ("FLOAT32_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Le, e1, e2, loc).toSuccess
      case ("FLOAT32_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Gt, e1, e2, loc).toSuccess
      case ("FLOAT32_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float32Op.Ge, e1, e2, loc).toSuccess

      case ("FLOAT64_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Float64Op.Neg, e1, loc).toSuccess
      case ("FLOAT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Add, e1, e2, loc).toSuccess
      case ("FLOAT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Sub, e1, e2, loc).toSuccess
      case ("FLOAT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Mul, e1, e2, loc).toSuccess
      case ("FLOAT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Div, e1, e2, loc).toSuccess
      case ("FLOAT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Exp, e1, e2, loc).toSuccess
      case ("FLOAT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Eq, e1, e2, loc).toSuccess
      case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Neq, e1, e2, loc).toSuccess
      case ("FLOAT64_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Lt, e1, e2, loc).toSuccess
      case ("FLOAT64_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Le, e1, e2, loc).toSuccess
      case ("FLOAT64_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Gt, e1, e2, loc).toSuccess
      case ("FLOAT64_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Float64Op.Ge, e1, e2, loc).toSuccess

      case ("INT8_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int8Op.Neg, e1, loc).toSuccess
      case ("INT8_NOT", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int8Op.Not, e1, loc).toSuccess
      case ("INT8_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Add, e1, e2, loc).toSuccess
      case ("INT8_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Sub, e1, e2, loc).toSuccess
      case ("INT8_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Mul, e1, e2, loc).toSuccess
      case ("INT8_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Div, e1, e2, loc).toSuccess
      case ("INT8_REM", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Rem, e1, e2, loc).toSuccess
      case ("INT8_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Exp, e1, e2, loc).toSuccess
      case ("INT8_AND", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.And, e1, e2, loc).toSuccess
      case ("INT8_OR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Or, e1, e2, loc).toSuccess
      case ("INT8_XOR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Xor, e1, e2, loc).toSuccess
      case ("INT8_SHL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Shl, e1, e2, loc).toSuccess
      case ("INT8_SHR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Shr, e1, e2, loc).toSuccess
      case ("INT8_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Eq, e1, e2, loc).toSuccess
      case ("INT8_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Neq, e1, e2, loc).toSuccess
      case ("INT8_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Lt, e1, e2, loc).toSuccess
      case ("INT8_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Le, e1, e2, loc).toSuccess
      case ("INT8_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Gt, e1, e2, loc).toSuccess
      case ("INT8_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int8Op.Ge, e1, e2, loc).toSuccess

      case ("INT16_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int16Op.Neg, e1, loc).toSuccess
      case ("INT16_NOT", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int16Op.Not, e1, loc).toSuccess
      case ("INT16_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Add, e1, e2, loc).toSuccess
      case ("INT16_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Sub, e1, e2, loc).toSuccess
      case ("INT16_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Mul, e1, e2, loc).toSuccess
      case ("INT16_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Div, e1, e2, loc).toSuccess
      case ("INT16_REM", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Rem, e1, e2, loc).toSuccess
      case ("INT16_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Exp, e1, e2, loc).toSuccess
      case ("INT16_AND", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.And, e1, e2, loc).toSuccess
      case ("INT16_OR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Or, e1, e2, loc).toSuccess
      case ("INT16_XOR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Xor, e1, e2, loc).toSuccess
      case ("INT16_SHL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Shl, e1, e2, loc).toSuccess
      case ("INT16_SHR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Shr, e1, e2, loc).toSuccess
      case ("INT16_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Eq, e1, e2, loc).toSuccess
      case ("INT16_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Neq, e1, e2, loc).toSuccess
      case ("INT16_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Lt, e1, e2, loc).toSuccess
      case ("INT16_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Le, e1, e2, loc).toSuccess
      case ("INT16_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Gt, e1, e2, loc).toSuccess
      case ("INT16_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int16Op.Ge, e1, e2, loc).toSuccess

      case ("INT32_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int32Op.Neg, e1, loc).toSuccess
      case ("INT32_NOT", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int32Op.Not, e1, loc).toSuccess
      case ("INT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Add, e1, e2, loc).toSuccess
      case ("INT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Sub, e1, e2, loc).toSuccess
      case ("INT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Mul, e1, e2, loc).toSuccess
      case ("INT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Div, e1, e2, loc).toSuccess
      case ("INT32_REM", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Rem, e1, e2, loc).toSuccess
      case ("INT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Exp, e1, e2, loc).toSuccess
      case ("INT32_AND", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.And, e1, e2, loc).toSuccess
      case ("INT32_OR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Or, e1, e2, loc).toSuccess
      case ("INT32_XOR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Xor, e1, e2, loc).toSuccess
      case ("INT32_SHL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Shl, e1, e2, loc).toSuccess
      case ("INT32_SHR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Shr, e1, e2, loc).toSuccess
      case ("INT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Eq, e1, e2, loc).toSuccess
      case ("INT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Neq, e1, e2, loc).toSuccess
      case ("INT32_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Lt, e1, e2, loc).toSuccess
      case ("INT32_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Le, e1, e2, loc).toSuccess
      case ("INT32_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Gt, e1, e2, loc).toSuccess
      case ("INT32_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int32Op.Ge, e1, e2, loc).toSuccess

      case ("INT64_NEG", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int64Op.Neg, e1, loc).toSuccess
      case ("INT64_NOT", e1 :: Nil) => WeededAst.Expr.Unary(SemanticOp.Int64Op.Not, e1, loc).toSuccess
      case ("INT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Add, e1, e2, loc).toSuccess
      case ("INT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Sub, e1, e2, loc).toSuccess
      case ("INT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Mul, e1, e2, loc).toSuccess
      case ("INT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Div, e1, e2, loc).toSuccess
      case ("INT64_REM", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Rem, e1, e2, loc).toSuccess
      case ("INT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Exp, e1, e2, loc).toSuccess
      case ("INT64_AND", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.And, e1, e2, loc).toSuccess
      case ("INT64_OR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Or, e1, e2, loc).toSuccess
      case ("INT64_XOR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Xor, e1, e2, loc).toSuccess
      case ("INT64_SHL", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Shl, e1, e2, loc).toSuccess
      case ("INT64_SHR", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Shr, e1, e2, loc).toSuccess
      case ("INT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Eq, e1, e2, loc).toSuccess
      case ("INT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Neq, e1, e2, loc).toSuccess
      case ("INT64_LT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Lt, e1, e2, loc).toSuccess
      case ("INT64_LE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Le, e1, e2, loc).toSuccess
      case ("INT64_GT", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Gt, e1, e2, loc).toSuccess
      case ("INT64_GE", e1 :: e2 :: Nil) => WeededAst.Expr.Binary(SemanticOp.Int64Op.Ge, e1, e2, loc).toSuccess

      case ("CHANNEL_GET", e1 :: Nil) => WeededAst.Expr.GetChannel(e1, loc).toSuccess
      case ("CHANNEL_PUT", e1 :: e2 :: Nil) => WeededAst.Expr.PutChannel(e1, e2, loc).toSuccess
      case ("CHANNEL_NEW", e1 :: e2 :: Nil) => WeededAst.Expr.NewChannel(e1, e2, loc).toSuccess

      case ("ARRAY_NEW", e1 :: e2 :: e3 :: Nil) => WeededAst.Expr.ArrayNew(e1, e2, e3, loc).toSuccess
      case ("ARRAY_LENGTH", e1 :: Nil) => WeededAst.Expr.ArrayLength(e1, loc).toSuccess
      case ("ARRAY_LOAD", e1 :: e2 :: Nil) => WeededAst.Expr.ArrayLoad(e1, e2, loc).toSuccess
      case ("ARRAY_STORE", e1 :: e2 :: e3 :: Nil) => WeededAst.Expr.ArrayStore(e1, e2, e3, loc).toSuccess

      case ("VECTOR_GET", e1 :: e2 :: Nil) => WeededAst.Expr.VectorLoad(e1, e2, loc).toSuccess
      case ("VECTOR_LENGTH", e1 :: Nil) => WeededAst.Expr.VectorLength(e1, loc).toSuccess

      case ("SCOPE_EXIT", e1 :: e2 :: Nil) => WeededAst.Expr.ScopeExit(e1, e2, loc).toSuccess

      case _ =>
        val err = UndefinedIntrinsic(loc)
        Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
    }
  }

  private def pickArguments(tree: Tree)(implicit s: State): Validation[List[Expr], CompilationMessage] = {
    flatMapN(
      pick(TreeKind.Arguments, tree.children)
    ) {
      argTree =>
        mapN(traverse(pickAll(TreeKind.Argument, argTree.children))(pickExpression)) {
          // Add synthetic unit arguments if there are none
          case Nil => List(Expr.Cst(Ast.Constant.Unit, tree.loc))
          case args => args
        }
    }
  }

  private def visitLiteral(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Literal)
    tree.children(0) match {
      case Child.Token(token) => token.kind match {
        // TODO: chars, strings, booleans, set, list, map
        case TokenKind.LiteralInt8 => Constants.toInt8(token)
        case TokenKind.LiteralInt16 => Constants.toInt16(token)
        case TokenKind.LiteralInt32 => Constants.toInt32(token)
        case TokenKind.LiteralInt64 => Constants.toInt64(token)
        case TokenKind.LiteralBigInt => Constants.toBigInt(token)
        case TokenKind.LiteralFloat32 => Constants.toFloat32(token)
        case TokenKind.LiteralFloat64 => Constants.toFloat64(token)
        case TokenKind.LiteralBigDecimal => Constants.toBigDecimal(token)
        case TokenKind.LiteralRegex => Constants.toRegex(token)
        case TokenKind.NameLowerCase
             | TokenKind.NameUpperCase
             | TokenKind.NameMath
             | TokenKind.NameGreek => mapN(pickNameIdent(tree))(ident => Expr.Ambiguous(Name.mkQName(ident), tree.loc))
        case _ => softFailWith(Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal")))
      }
      case _ => softFailWith(Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal")))
    }
  }

  private object Constants {
    /**
     * Removes underscores from the given string of digits.
     */
    private def stripUnderscores(digits: String): String = {
      digits.filterNot(_ == '_')
    }

    private def tryParseFloat(token: Token, after: (String, SourceLocation) => Expr)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      try {
        after(stripUnderscores(token.text), loc).toSuccess
      } catch {
        case _: NumberFormatException =>
          val err = MalformedFloat(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
      }
    }

    private def tryParseInt(token: Token, after: (String, SourceLocation) => Expr)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      try {
        after(stripUnderscores(token.text), loc).toSuccess
      } catch {
        case _: NumberFormatException =>
          val err = MalformedInt(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
      }
    }

    /**
     * Attempts to parse the given tree to a float32.
     */
    def toFloat32(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseFloat(token, (text, loc) => Expr.Cst(Ast.Constant.Float32(text.stripSuffix("f32").toFloat), loc))

    /**
     * Attempts to parse the given tree to a float32.
     */
    def toFloat64(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseFloat(token, (text, loc) => Expr.Cst(Ast.Constant.Float64(text.stripSuffix("f64").toDouble), loc))

    /**
     * Attempts to parse the given tree to a big decimal.
     */
    def toBigDecimal(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseFloat(token, (text, loc) => {
        val bigDecimal = new java.math.BigDecimal(text.stripSuffix("ff"))
        Expr.Cst(Ast.Constant.BigDecimal(bigDecimal), loc)
      })

    /**
     * Attempts to parse the given tree to a int8.
     */
    def toInt8(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseInt(token, (text, loc) => {
        val radix = if (text.contains("0x")) 16 else 10
        val digits = text.replaceFirst("0x", "").stripSuffix("i8")
        val int = JByte.parseByte(digits, radix)
        Expr.Cst(Ast.Constant.Int8(int), loc)
      })

    /**
     * Attempts to parse the given tree to a int16.
     */
    def toInt16(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseInt(token, (text, loc) => {
        val radix = if (text.contains("0x")) 16 else 10
        val digits = text.replaceFirst("0x", "").stripSuffix("i16")
        val int = JShort.parseShort(digits, radix)
        Expr.Cst(Ast.Constant.Int16(int), loc)
      })

    /**
     * Attempts to parse the given tree to a int32.
     */
    def toInt32(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseInt(token, (text, loc) => {
        val radix = if (text.contains("0x")) 16 else 10
        val digits = text.replaceFirst("0x", "").stripSuffix("i32")
        val int = JInt.parseInt(digits, radix)
        Expr.Cst(Ast.Constant.Int32(int), loc)
      })

    /**
     * Attempts to parse the given tree to a int64.
     */
    def toInt64(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseInt(token, (text, loc) => {
        val radix = if (text.contains("0x")) 16 else 10
        val digits = text.replaceFirst("0x", "").stripSuffix("i64")
        val int = JLong.parseLong(digits, radix)
        Expr.Cst(Ast.Constant.Int64(int), loc)
      })

    /**
     * Attempts to parse the given tree to a int64.
     */
    def toBigInt(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] =
      tryParseInt(token, (text, loc) => {
        val radix = if (text.contains("0x")) 16 else 10
        val digits = text.replaceFirst("0x", "").stripSuffix("ii")
        val int = new java.math.BigInteger(digits, radix)
        Expr.Cst(Ast.Constant.BigInt(int), loc)
      })

    /**
     * Attempts to compile the given regular expression into a Pattern.
     */
    def toRegex(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      try {
        val pattern = JPattern.compile(token.text.stripPrefix("regex\"").stripSuffix("\""))
        Expr.Cst(Ast.Constant.Regex(pattern), loc).toSuccess
      } catch {
        case ex: PatternSyntaxException =>
          val err = MalformedRegex(token.text, ex.getMessage, loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
      }

    }
  }

  private def pickTypeArguments(tree: Tree)(implicit s: State): Validation[List[Type], CompilationMessage] = {
    tryPick(TreeKind.Type.Arguments, tree.children)
      .map(argTree => traverse(pickAll(TreeKind.Argument, argTree.children))(pickType))
      .getOrElse(List.empty.toSuccess)
  }

  private def pickType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    flatMapN(pick(TreeKind.Type.Type, tree.children))(visitType)
  }

  private def visitType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Type)
    // Visit first child and match its kind to know what to to
    val inner = unfold(tree)
    inner.kind match {
      case TreeKind.Type.Variable => visitTypeVariable(inner)
      case TreeKind.Type.Apply => visitTypeApply(inner)
      case TreeKind.QName => mapN(visitQName(inner))(Type.Ambiguous(_, inner.loc))
      case TreeKind.Ident => mapN(tokenToIdent(inner))(ident => Type.Ambiguous(Name.mkQName(ident), inner.loc))
      case kind => failWith(s"'$kind' used as type")
    }
  }

  private def visitTypeVariable(tree: Tree)(implicit s: State): Validation[Type.Var, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Variable)
    mapN(tokenToIdent(tree)) { ident => Type.Var(ident, tree.loc) }
  }

  private def visitTypeApply(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Apply)
    flatMapN(pickType(tree), pick(TreeKind.Type.Arguments, tree.children)) {
      (tpe, argsTree) =>
        // Curry type arguments
        val arguments = pickAll(TreeKind.Type.Argument, argsTree.children)
        mapN(traverse(arguments)(pickType)) {
          args => args.foldLeft(tpe) { case (acc, t2) => Type.Apply(acc, t2, tree.loc) }
        }
    }
  }

  private def tryPickEffect(tree: Tree)(implicit s: State): Validation[Option[Type], CompilationMessage] = {
    val maybeEffectSet = tryPick(TreeKind.Type.EffectSet, tree.children)
    traverseOpt(maybeEffectSet)(setTree => {
      val effects = traverse(pickAll(TreeKind.Type.Type, setTree.children))(visitType)
      mapN(effects) {
        // Default to Pure
        case Nil => Type.Pure(setTree.loc)
        // Otherwise reduce effects into a union type
        case effects => effects.reduceLeft({
          case (acc, tpe) => Type.Union(acc, tpe, setTree.loc)
        }: (Type, Type) => Type)
      }
    })
  }

  private def pickQName(tree: Tree)(implicit s: State): Validation[Name.QName, CompilationMessage] = {
    flatMapN(pick(TreeKind.QName, tree.children))(visitQName)
  }

  private def visitQName(tree: Tree)(implicit s: State): Validation[Name.QName, CompilationMessage] = {
    assert(tree.kind == TreeKind.QName)
    val idents = pickAll(TreeKind.Ident, tree.children)
    mapN(traverse(idents)(tokenToIdent)) {
      idents =>
        val first = idents.head
        val ident = idents.last
        val nnameIdents = idents.dropRight(1)
        val nname = Name.NName(first.sp1, nnameIdents, ident.sp2)
        Name.QName(first.sp1, nname, ident, ident.sp2)
    }
  }

  private def pickNameIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    flatMapN(pick(TreeKind.Ident, tree.children))(tokenToIdent)
  }

  ////////// HELPERS //////////////////
  private def tokenToIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    tree.children.headOption match {
      case Some(Child.Token(token)) => Name.Ident(
        token.mkSourcePosition(s.src, Some(s.parserInput)),
        token.text,
        token.mkSourcePositionEnd(s.src, Some(s.parserInput))
      ).toSuccess
      case _ => failWith(s"expected first child of '${tree.kind}' to be Child.Token")
    }
  }

  /**
   * When kinds are elided they default to the kind `Type`.
   */
  private def defaultKind(ident: Name.Ident): Kind = Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)

  /**
   * plucks the first inner tree in children.
   * NB: This is intended to used to unfold the inner tree on special marker [[TreeKinds]],
   * such as [[TreeKind.Type.Type]] and [[TreeKind.Expr.Expr]].
   * The parser should guarantee that these tree kinds have at least a single child.
   */
  private def unfold(tree: Tree): Tree = {
    assert(tree.kind match {
      case TreeKind.Type.Type | TreeKind.Expr.Expr | TreeKind.JvmOp.JvmOp => true
      case _ => false
    })

    tree.children.headOption.map {
      case Child.Tree(tree) => tree
      case Child.Token(_) => throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child", tree.loc)
    }.getOrElse(throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child", tree.loc))
  }

  // A helper that collects the text in token children
  private def text(tree: Tree): List[String] = {
    tree.children.foldLeft[List[String]](List.empty)((acc, c) => c match {
      case Child.Token(token) => acc :+ token.text
      case Child.Tree(_) => acc
    })
  }

  private def pickAllTokens(tree: Tree): Array[Token] = {
    tree.children.collect { case Child.Token(token) => token }
  }

  // A helper that picks out the first tree of a specific kind from a list of children
  private def pick(kind: TreeKind, children: Array[Child]): Validation[Tree, CompilationMessage] = {
    tryPick(kind, children)
      .map(_.toSuccess)
      .getOrElse(failWith(s"expected to find at least one '$kind'"))
  }

  private def tryPick(kind: TreeKind, children: Array[Child]): Option[Tree] = {
    children.find {
      case Child.Tree(tree) if tree.kind == kind => true
      case _ => false
    } match {
      case Some(Child.Tree(tree)) => Some(tree)
      case _ => None
    }
  }

  // A helper that picks out trees of a specific kind from a list of children
  private def pickAll(kind: TreeKind, children: Array[Child]): List[Tree] = {
    children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case Child.Tree(tree) if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }

  // A helper that returns the unique pairs of duplicates from an array of items
  private def getDuplicates[A, K](items: Seq[A], groupBy: A => K): List[(A, A)] = {
    val duplicates = items.groupBy(groupBy).collect { case (_, is) if is.length > 1 => is }
    val pairs = duplicates.map(dups => {
      for {
        (x, idxX) <- dups.zipWithIndex
        (y, idxY) <- dups.zipWithIndex
        if (idxX + 1) == idxY
      } yield (x, y)
    })
    List.from(pairs.flatten)
  }

  private def failWith[T](message: String, loc: SourceLocation = SourceLocation.Unknown): Validation[T, CompilationMessage] = {
    Validation.Failure(LazyList(Parse2Error.DevErr(loc, message)))
  }

  private def softFailWith[T](result: T): Validation[T, CompilationMessage] = {
    Validation.SoftFailure(result, LazyList())
  }

  /**
   * Ternary enumeration of constraints on the presence of something.
   */
  private sealed trait Presence

  private object Presence {
    /**
     * Indicates that the thing is required.
     */
    case object Required extends Presence

    /**
     * Indicates that the thing is optional.
     */
    case object Optional extends Presence

    /**
     * Indicates that the thing is forbidden.
     */
    case object Forbidden extends Presence
  }

  /**
   * The result of weeding an operator.
   */
  private sealed trait OperatorResult

  private object OperatorResult {
    /**
     * The operator represents a signature or definition from the core library.
     */
    case class BuiltIn(name: Name.QName) extends OperatorResult

    /**
     * The operator represents a semantic operator.
     */
    case class Operator(op: SemanticOp) extends OperatorResult

    /**
     * The operator is unrecognized: it must have been defined elsewhere.
     */
    case class Unrecognized(ident: Name.Ident) extends OperatorResult
  }
}
