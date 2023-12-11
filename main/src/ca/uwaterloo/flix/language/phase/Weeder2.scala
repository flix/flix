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
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.language.errors.WeederError.{MismatchedTypeParameters, MissingTypeParamKind, NonUnaryAssocType}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import org.parboiled2.ParserInput

// TODO: Add change set support


/**
 * Weeder2 produces a [[WeededAst.Root]] by transforming [[UnstructuredTree.Tree]]s into [[WeededAst.CompilationUnit]]s.
 * [[UnstructuredTree.Tree]] provides few guarantees, which must be kept in mind when reading/modifying Weeder2:
 * 1. Each Tree node will have a [[TreeKind]], [[SourceLocation]] and a list of __zero or more__ children.
 * 2. Each child may be a token or another tree.
 * NB: There is __no__ guarantee on the amount or order of children.
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

    mapN(pickAllUsesAndImports(tree), pickAllDeclarations(tree)) {
      case (usesAndImports, declarations) =>
        CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def pickAllUsesAndImports(tree: Tree): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def pickAllDeclarations(tree: Tree)(implicit s: State): Validation[List[Declaration], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    mapN(
      traverse(pickAll(TreeKind.Def)(tree.children))(visitDefinition),
      traverse(pickAll(TreeKind.Class)(tree.children))(visitTypeClass)
    ) {
      case (definitions, classes) => definitions ++ classes
    }
  }

  private def visitTypeClass(tree: Tree)(implicit s: State): Validation[Declaration, CompilationMessage] = {
    assert(tree.kind == TreeKind.Class)
    flatMapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree),
      pickNameIdent(tree),
      pickSingleTypeParameter(tree),
      pickAllSignatures(tree),
      //TODO: visitLaws(tree),
    ) {
      case (doc, annotations, modifiers, ident, tparam, sigs) =>
        mapN(pickAllAssociatedTypesSig(tree, tparam)) {
          assocs => Declaration.Class(doc, annotations, modifiers, ident, tparam, List.empty, assocs, sigs, List.empty, tree.loc)
        }

    }
  }

  private def visitDefinition(tree: Tree)(implicit s: State): Validation[Declaration, CompilationMessage] = {
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
      visitEffect(tree)
    ) {
      case (doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
        Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }
  }

  private def pickDocumentation(tree: Tree): Validation[Ast.Doc, CompilationMessage] = {
    val docTree = tryPick(TreeKind.Doc, tree.children)
    val loc = docTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    // strip prefixing `///` and remove empty lines after that
    val comments = docTree.map(text).map(
      _.map(_.stripPrefix("///").trim())
        .filter(_ != "")
    ).getOrElse(List.empty)
    Ast.Doc(comments, loc).toSuccess
  }

  private def pickAnnotations(tree: Tree)(implicit s: State): Validation[Ast.Annotations, CompilationMessage] = {
    val annotationTree = tryPick(TreeKind.Annotations, tree.children)
    val annotations = annotationTree.map(
      t => traverse(pickAllTokens(t))(visitAnnotation)
    ).getOrElse(List.empty.toSuccess)
    annotations.map(annotations => Ast.Annotations(annotations))
  }

  private def visitAnnotation(token: Token)(implicit s: State): Validation[Ast.Annotation, CompilationMessage] = {
    val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
    import Ast.Annotation._
    // TODO: annotation.Error is a soft failure
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

  private def pickModifiers(tree: Tree): Validation[Ast.Modifiers, CompilationMessage] = {
    tryPick(TreeKind.Modifiers, tree.children) match {
      case Some(modTree) => mapN(traverse(pickAllTokens(modTree))(visitModifier))(Ast.Modifiers(_))
      case None => Ast.Modifiers(List.empty).toSuccess
    }
  }

  private def visitModifier(token: Token): Validation[Ast.Modifier, CompilationMessage] = {
    token.kind match {
      // TODO: there is no Ast.Modifier for 'inline'
      case TokenKind.KeywordSealed => Ast.Modifier.Sealed.toSuccess
      case TokenKind.KeywordLawful => Ast.Modifier.Lawful.toSuccess
      case TokenKind.KeywordPub => Ast.Modifier.Public.toSuccess
      case TokenKind.KeywordOverride => Ast.Modifier.Override.toSuccess
      // TODO: Can this be a softFailure?
      case kind => failWith(s"unsupported modifier $kind")
    }
  }

  private def pickTypeParameters(tree: Tree)(implicit s: State): Validation[TypeParams, CompilationMessage] = {
    tryPick(TreeKind.TypeParameters, tree.children) match {
      case Some(tparamsTree) =>
        flatMapN(traverse(pickAll(TreeKind.Parameter)(tparamsTree.children))(visitTypeParameter)) {
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
      case None => TypeParams.Elided.toSuccess
    }
  }

  private def pickKindedTypeParameters(tree: Tree)(implicit s: State): Validation[KindedTypeParams, CompilationMessage] = {
    tryPick(TreeKind.TypeParameters, tree.children) match {
      case Some(tparamsTree) =>
        flatMapN(traverse(pickAll(TreeKind.Parameter)(tparamsTree.children))(visitTypeParameter)) {
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
      case None => TypeParams.Elided.toSuccess
    }
  }

  private def pickSingleTypeParameter(tree: Tree)(implicit s: State): Validation[TypeParam, CompilationMessage] = {
    flatMapN(pick(TreeKind.TypeParameters, tree.children)) {
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
    tryPick(TreeKind.Kind, tree.children).flatMap(kindTree => {
      val ident = pickNameIdent(kindTree) match {
        case Success(t) => Some(t)
        case SoftFailure(t, _) => Some(t)
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

  private def pickAllAssociatedTypesSig(tree: Tree, classTypeParam: TypeParam)(implicit s: State): Validation[List[Declaration.AssocTypeSig], CompilationMessage] = {
    traverse(pickAll(TreeKind.AssociatedType)(tree.children))(visitAssociatedType(_, classTypeParam))
  }

  private def visitAssociatedType(tree: Tree, classTypeParam: TypeParam)(implicit s: State): Validation[Declaration.AssocTypeSig, CompilationMessage] = {
    assert(tree.kind == TreeKind.AssociatedType)
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
    traverse(pickAll(TreeKind.Signature)(tree.children))(visitSignature)
  }

  private def visitSignature(tree: Tree)(implicit s: State): Validation[Declaration.Sig, CompilationMessage] = {
    assert(tree.kind == TreeKind.Signature)
    mapN(
      pickDocumentation(tree),
      pickAnnotations(tree),
      pickModifiers(tree),
      // TODO: require public modifier
      pickNameIdent(tree),
      pickKindedTypeParameters(tree),
      //      visitFormalParameters()
      pickType(tree),
      visitEffect(tree),
      pickTypeConstraints(tree),
      //      pickExpression(tree) // TODO: This is optional
    ) {
      case (doc, annotations, modifiers, ident, tparams, tpe, eff, tconstrs) =>
        Declaration.Sig(doc, annotations, modifiers, ident, tparams, List.empty, None, tpe, eff, tconstrs, tree.loc)
    }
  }

  private def pickFormalParameters(tree: Tree): Validation[List[FormalParam], CompilationMessage] = {
    val paramTree = tryPick(TreeKind.Parameters, tree.children)
    val params = paramTree.map(
      t => {
        val parameters = if (t.children.length == 2) {
          // only ParenL and ParenR are in parameters, so produce synthetic unit parameter
          List(
            FormalParam(
              Name.Ident(SourcePosition.Unknown, "_unit", SourcePosition.Unknown),
              Ast.Modifiers(List.empty),
              Some(Type.Unit(t.loc)),
              t.loc
            )
          )
        } else {
          List.empty // TODO: Map parameters
        }
        parameters.toSuccess
      }
    ).getOrElse(failWith("expected formal parameters", tree.loc))

    params
  }

  private def pickTypeConstraints(tree: Tree): Validation[List[TypeConstraint], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def pickConstraints(tree: Tree): Validation[List[EqualityConstraint], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def pickExpression(tree: Tree): Validation[Expr, CompilationMessage] = {
    val exprTree = tryPick(TreeKind.Expr.Expr, tree.children)
    val loc = exprTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    val notFoundError = softFailWith(Expr.Error(Parse2Error.DevErr(loc, "expected expression")))

    exprTree.map(_.children(0) match {
      case Child.Tree(tree) => tree.kind match {
        case TreeKind.Expr.Literal => visitLiteral(tree)
        case _ => notFoundError
      }
      case _ => notFoundError
    }).getOrElse(notFoundError)
  }

  private def visitLiteral(tree: Tree): Validation[Expr, CompilationMessage] = {
    assert(tree.kind == TreeKind.Expr.Literal)
    val literal = tree.children(0) match {
      case Child.Token(token) => token.kind match {
        case TokenKind.ParenL => Expr.Tuple(List.empty, tree.loc)
        // TODO: Everything other than unit literals
        case _ => Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal"))
      }
      case _ => Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal"))
    }
    literal.toSuccess
  }

  private def pickType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    flatMapN(pick(TreeKind.Type.Type, tree.children)) {
      typeTree =>
        // Visit first child and match its kind to know what to to
        val inner = unfold(typeTree)
        inner.kind match {
          // TODO: Type.Var
          case TreeKind.Type.Variable => visitTypeVariable(inner)
          case TreeKind.Type.Apply => visitTypeApply(inner)
          case TreeKind.Ident => mapN(firstIdent(inner)) { ident => Type.Ambiguous(Name.mkQName(ident), ident.loc) }
          case TreeKind.QName => mapN(visitQName(inner))(Type.Ambiguous(_, inner.loc))
          case kind => failWith(s"'$kind' used as type")
        }
    }
    // TODO: Handle effects here too?
  }

  private def visitTypeVariable(tree: Tree)(implicit s: State): Validation[Type.Var, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Variable)
    mapN(firstIdent(tree)) { ident => Type.Var(ident, tree.loc) }
  }

  private def visitTypeApply(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    assert(tree.kind == TreeKind.Type.Apply)
    flatMapN(pickType(tree), pick(TreeKind.Type.Arguments, tree.children)) {
      (tpe, argsTree) =>
        val argsVal = traverse(pickAll(TreeKind.Type.Argument)(argsTree.children))(pickType)
        mapN(argsVal) { args => args.foldLeft(tpe) { case (acc, t2) => Type.Apply(acc, t2, tree.loc) } }
    }
  }

  private def visitEffect(tree: Tree): Validation[Option[Type], CompilationMessage] = {
    // TODO
    None.toSuccess
  }

  private def visitQName(tree: Tree)(implicit s: State): Validation[Name.QName, CompilationMessage] = {
    assert(tree.kind == TreeKind.QName)
    mapN(traverse(pickAll(TreeKind.Ident)(tree.children))(firstIdent)) {
      idents =>
        val first = idents.head
        val ident = idents.last
        val nnameIdents = idents.dropRight(1)
        val nname = Name.NName(first.sp1, nnameIdents, ident.sp2)
        Name.QName(first.sp1, nname, ident, ident.sp2)
    }
  }

  private def pickNameIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    flatMapN(pick(TreeKind.Ident, tree.children))(firstIdent)
  }

  ////////// HELPERS //////////////////
  private def firstIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
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
      case TreeKind.Type.Type | TreeKind.Expr.Expr => true
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
  private def pickAll(kind: TreeKind)(children: Array[Child]): List[Tree] = {
    children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case Child.Tree(tree) if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }

  private def failWith[T](message: String, loc: SourceLocation = SourceLocation.Unknown): Validation[T, CompilationMessage] = {
    Validation.Failure(LazyList(Parse2Error.DevErr(loc, message)))
  }

  private def softFailWith[T](result: T): Validation[T, CompilationMessage] = {
    Validation.SoftFailure(result, LazyList())
  }
}
