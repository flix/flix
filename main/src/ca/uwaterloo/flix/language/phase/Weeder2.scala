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
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Denotation, Fixity, Polarity}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SemanticOp, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst, Type => AstType}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import org.parboiled2.ParserInput

import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}
import java.util.regex.{PatternSyntaxException, Pattern => JPattern}
import scala.collection.immutable.{::, List, Nil}
import scala.math.Ordered.orderingToOrdered

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
      return Validation.HardFailure(Chain.empty)
    }

    flix.phase("Weeder2") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(trees) {
        case (src, tree) => mapN(weed(src, tree))(tree => src -> tree)
      }

      mapN(mapN(sequence(results))(_.toMap))(m => WeededAst.Root(m, entryPoint, readRoot.names))
    }
  }

  def weed(src: Ast.Source, tree: Tree): Validation[CompilationUnit, CompilationMessage] = {
    implicit val s: State = new State(src)
    mapN(
      pickAllUsesAndImports(tree),
      Decls.pickAllDeclarations(tree)
    ) {
      case (usesAndImports, declarations) => CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def pickAllUsesAndImports(tree: Tree)(implicit s: State): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source || tree.kind == TreeKind.Decl.Module)
    val maybeTree = tryPick(TreeKind.UsesOrImports.UsesOrImports, tree.children)
    maybeTree
      .map(tree => {
        val uses = pickAll(TreeKind.UsesOrImports.Use, tree.children)
        val imports = pickAll(TreeKind.UsesOrImports.Import, tree.children)
        mapN(
          traverse(uses)(visitUse),
          traverse(imports)(visitImport)
        )((uses, imports) => uses.flatten ++ imports.flatten)
      })
      .getOrElse(Validation.success(List.empty))
  }

  private def visitUse(tree: Tree)(implicit s: State): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.UsesOrImports.Use)
    val maybeUseMany = tryPick(TreeKind.UsesOrImports.UseMany, tree.children)
    flatMapN(
      pickQName(tree))(qname => {
      val nname = Name.NName(qname.sp1, qname.namespace.idents :+ qname.ident, qname.sp2)
      mapN(traverseOpt(maybeUseMany)(tree => visitUseMany(tree, nname))) {
        // case: use one, use the qname
        case None | Some(Nil) => List(UseOrImport.Use(qname, qname.ident, qname.loc))
        // case: use many
        case Some(uses) => uses
      }
    })
  }

  private def visitUseMany(tree: Tree, namespace: Name.NName)(implicit s: State): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.UsesOrImports.UseMany)
    val identUses = traverse(pickAll(TreeKind.Ident, tree.children))(visitUseIdent(_, namespace))
    val aliasedUses = traverse(pickAll(TreeKind.UsesOrImports.Alias, tree.children))(tree => visitUseAlias(tree, namespace))
    mapN(identUses, aliasedUses)((identUses, aliasedUses) => (identUses ++ aliasedUses).sortBy(_.loc))
  }

  private def visitUseIdent(tree: Tree, namespace: Name.NName)(implicit s: State): Validation[UseOrImport.Use, CompilationMessage] = {
    mapN(tokenToIdent(tree)) {
      ident =>
        val qname = Name.QName(tree.loc.sp1, namespace, ident, tree.loc.sp2)
        UseOrImport.Use(qname, ident, ident.loc)
    }
  }

  private def visitUseAlias(tree: Tree, namespace: Name.NName)(implicit s: State): Validation[UseOrImport.Use, CompilationMessage] = {
    val idents = traverse(pickAll(TreeKind.Ident, tree.children))(tokenToIdent)
    flatMapN(idents) {
      case ident :: alias :: _ =>
        val qname = Name.QName(tree.loc.sp1, namespace, ident, tree.loc.sp2)
        Validation.success(UseOrImport.Use(qname, alias, tree.loc))
      // recover from missing alias by using ident
      case ident :: _ => softFailWith(UseOrImport.Use(Name.mkQName(ident), ident, ident.loc))
      case _ => failWith("Malformed alias", tree.loc)
    }
  }

  private def visitImport(tree: Tree)(implicit s: State): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.UsesOrImports.Import)
    val maybeImportMany = tryPick(TreeKind.UsesOrImports.ImportMany, tree.children)
    flatMapN(
      JvmOp.pickJavaName(tree))(jname => {
      mapN(traverseOpt(maybeImportMany)(tree => visitImportMany(tree, jname.fqn))) {
        // case: import one, use the java name
        case None | Some(Nil) => List(UseOrImport.Import(jname, Name.Ident(jname.sp1, jname.fqn.last, jname.sp2), tree.loc))
        // case: import many
        case Some(imports) => imports
      }
    })
  }

  private def visitImportMany(tree: Tree, namespace: Seq[String])(implicit s: State): Validation[List[UseOrImport.Import], CompilationMessage] = {
    assert(tree.kind == TreeKind.UsesOrImports.ImportMany)
    val identImports = traverse(pickAll(TreeKind.Ident, tree.children))(visitImportIdent(_, namespace))
    val aliasedImports = traverse(pickAll(TreeKind.UsesOrImports.Alias, tree.children))(tree => visitImportAlias(tree, namespace))
    mapN(identImports, aliasedImports)((identImports, aliasedImports) => (identImports ++ aliasedImports).sortBy(_.loc))
  }

  private def visitImportIdent(tree: Tree, namespace: Seq[String])(implicit s: State): Validation[UseOrImport.Import, CompilationMessage] = {
    mapN(tokenToJavaName(tree)) {
      ident => UseOrImport.Import(Name.JavaName(tree.loc.sp1, namespace ++ Seq(ident.name), tree.loc.sp2), ident, ident.loc)
    }
  }

  private def visitImportAlias(tree: Tree, namespace: Seq[String])(implicit s: State): Validation[UseOrImport.Import, CompilationMessage] = {
    val idents = traverse(pickAll(TreeKind.Ident, tree.children))(tokenToIdent)
    flatMapN(idents) {
      case ident :: alias :: _ =>
        val jname = Name.JavaName(tree.loc.sp1, namespace ++ Seq(ident.name), tree.loc.sp2)
        Validation.success(UseOrImport.Import(jname, alias, tree.loc))
      // recover from missing alias by using ident
      case ident :: _ => softFailWith(UseOrImport.Import(Name.JavaName(tree.loc.sp1, Seq(ident.name), tree.loc.sp2), ident, ident.loc))
      case _ => failWith("Malformed alias", tree.loc)
    }
  }

  private object Decls {
    def pickAllDeclarations(tree: Tree)(implicit s: State): Validation[List[Declaration], CompilationMessage] = {
      assert(tree.kind == TreeKind.Source || tree.kind == TreeKind.Decl.Module)
      val modules = pickAll(TreeKind.Decl.Module, tree.children)
      val definitions = pickAll(TreeKind.Decl.Def, tree.children)
      val classes = pickAll(TreeKind.Decl.Class, tree.children)
      val instances = pickAll(TreeKind.Decl.Instance, tree.children)
      val enums = pickAll(TreeKind.Decl.Enum, tree.children)
      val typeAliases = pickAll(TreeKind.Decl.TypeAlias, tree.children)
      val effects = pickAll(TreeKind.Decl.Effect, tree.children)
      // TODO: Restrictable enums
      mapN(
        traverse(modules)(visitModule),
        traverse(definitions)(visitDefinition),
        traverse(classes)(visitTypeClass),
        traverse(instances)(visitInstance),
        traverse(enums)(visitEnum),
        traverse(typeAliases)(visitTypeAlias),
        traverse(effects)(visitEffect)
      ) {
        case (modules, definitions, classes, instances, enums, typeAliases, effects) =>
          val all: List[Declaration] = definitions ++ classes ++ modules ++ enums ++ instances ++ typeAliases ++ effects

          // TODO: We need to sort these by source position for comparison with old parser. Can be removed for production
          def getLoc(d: Declaration): SourceLocation = d match {
            case Declaration.Namespace(_, _, _, loc) => loc
            case Declaration.Def(_, _, _, _, _, _, _, _, _, _, _, loc) => loc
            case Declaration.Class(_, _, _, _, _, _, _, _, _, loc) => loc
            case Declaration.Instance(_, _, _, _, _, _, _, _, loc) => loc
            case Declaration.Enum(_, _, _, _, _, _, _, loc) => loc
            case Declaration.TypeAlias(_, _, _, _, _, _, loc) => loc
            case Declaration.Effect(_, _, _, _, _, loc) => loc
            case d => throw InternalCompilerException(s"TODO: handle $d", tree.loc)
          }

          all.sortBy(getLoc)
      }
    }

    private def visitModule(tree: Tree)(implicit s: State): Validation[Declaration.Namespace, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Module)
      mapN(
        pickQName(tree),
        pickAllUsesAndImports(tree),
        pickAllDeclarations(tree)
      ) {
        (qname, usesAndImports, declarations) =>
          val base = Declaration.Namespace(qname.ident, usesAndImports, declarations, tree.loc)
          qname.namespace.idents.foldRight(base: Declaration.Namespace) {
            case (ident, acc) => Declaration.Namespace(ident, Nil, List(acc), tree.loc)
          }
      }
    }

    private def visitEffect(tree: Tree)(implicit s: State): Validation[Declaration.Effect, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Effect)
      val ops = pickAll(TreeKind.Decl.Operation, tree.children)
      mapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        traverse(ops)(visitOperation)
      ) {
        (doc, ann, mods, ident, ops) => Declaration.Effect(doc, ann, mods, ident, ops, tree.loc)
      }
    }

    private def visitOperation(tree: Tree)(implicit s: State): Validation[Declaration.Op, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Operation)
      mapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
        pickFormalParameters(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree)
      ) {
        (doc, ann, mods, ident, tparams, fparams, tpe, tconstrs) => Declaration.Op(doc, ann, mods, ident, fparams, tpe, tconstrs, loc = tree.loc)
      }
    }


    private def visitTypeAlias(tree: Tree)(implicit s: State): Validation[Declaration.TypeAlias, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.TypeAlias)
      mapN(
        pickDocumentation(tree),
        pickModifiers(tree),
        pickAnnotations(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
        Types.pickType(tree)
      ) {
        (doc, mods, ann, ident, tparams, tpe) => Declaration.TypeAlias(doc, ann, mods, ident, tparams, tpe, tree.loc)
      }
    }

    private def visitEnum(tree: Tree)(implicit s: State): Validation[Declaration.Enum, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Enum)
      val shorthandTuple = tryPick(TreeKind.Type.Type, tree.children)
      val cases = pickAll(TreeKind.Case, tree.children)
      flatMapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree, allowed = Set(TokenKind.KeywordPub)),
        pickNameIdent(tree),
        Types.pickDerivations(tree),
        Types.pickParameters(tree),
        traverseOpt(shorthandTuple)(Types.visitType),
        traverse(cases)(visitEnumCase)
      ) {
        (doc, ann, mods, ident, derivations, tparams, tpe, cases) =>
          val casesVal = (tpe, cases) match {
            // Error: both singleton shorthand and cases provided
            case (Some(_), _ :: _) => Validation.toHardFailure(IllegalEnum(ident.loc))
            // Empty enum
            case (None, Nil) => Validation.success(List.empty)
            // Singleton enum
            case (Some(t), Nil) =>
              Validation.success(List(WeededAst.Case(ident, flattenEnumCaseType(t, ident.loc), ident.loc)))
            // Multiton enum
            case (None, cs) =>
              val errors = getDuplicates(cs, (c: Case) => c.ident.name).map(pair =>
                DuplicateTag(ident.name, pair._1.ident, pair._1.loc, pair._2.loc)
              )
              Validation.success(cases).withSoftFailures(errors)
          }
          mapN(casesVal) {
            cases => Declaration.Enum(doc, ann, mods, ident, tparams, derivations, cases, tree.loc)
          }
      }
    }

    private def visitEnumCase(tree: Tree)(implicit s: State): Validation[Case, CompilationMessage] = {
      assert(tree.kind == TreeKind.Case)
      val maybeType = tryPick(TreeKind.Type.Type, tree.children)
      mapN(
        pickNameIdent(tree),
        traverseOpt(maybeType)(Types.visitType),
        // TODO: Doc comments on enum cases. It is not available on [[Case]] yet.
      ) {
        (ident, maybeType) =>
          val tpe = maybeType
            .map(flattenEnumCaseType(_, tree.loc))
            .getOrElse(Type.Unit(ident.loc))
          Case(ident, tpe, tree.loc)
      }
    }

    private def flattenEnumCaseType(tpe: Type, loc: SourceLocation): Type = {
      tpe match {
        // Single type in case -> flatten to ambiguous.
        case Type.Tuple(t :: Nil, _) => t
        // Multiple types in case -> do nothing
        case tpe => tpe
      }
    }

    private def visitTypeClass(tree: Tree)(implicit s: State): Validation[Declaration.Class, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Class)
      val sigs = pickAll(TreeKind.Decl.Signature, tree.children)
      val laws = pickAll(TreeKind.Decl.Law, tree.children)
      flatMapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        Types.pickSingleParameter(tree),
        Types.pickConstraints(tree),
        traverse(sigs)(visitSignature),
        traverse(laws)(visitLaw)
      ) {
        case (doc, annotations, modifiers, ident, tparam, tconstr, sigs, laws) =>
          val assocs = pickAll(TreeKind.Decl.AssociatedTypeSig, tree.children)
          mapN(traverse(assocs)(visitAssociatedTypeSig(_, tparam))) {
            assocs => Declaration.Class(doc, annotations, modifiers, ident, tparam, tconstr, assocs, sigs, laws, tree.loc)
          }
      }
    }

    private def visitInstance(tree: Tree)(implicit s: State): Validation[Declaration.Instance, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Instance)
      flatMapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree),
        pickQName(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
        traverse(pickAll(TreeKind.Decl.Def, tree.children))(visitDefinition),
      ) {
        case (doc, annotations, modifiers, clazz, tpe, tconstrs, defs) =>
          val assocs = pickAll(TreeKind.Decl.AssociatedTypeDef, tree.children)
          mapN(traverse(assocs)(visitAssociatedTypeDef(_, tpe))) {
            assocs => Declaration.Instance(doc, annotations, modifiers, clazz, tpe, tconstrs, assocs, defs, tree.loc)
          }
      }
    }

    private def visitDefinition(tree: Tree)(implicit s: State): Validation[Declaration.Def, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Def)
      mapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Exprs.pickExpression(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
        pickConstraints(tree),
        Types.tryPickEffect(tree)
      ) {
        case (doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
          Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
      }
    }

    private def visitLaw(tree: Tree)(implicit s: State): Validation[Declaration.Def, CompilationMessage] = {
      mapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree, allowed = Set.empty),
        pickNameIdent(tree),
        Types.pickConstraints(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Exprs.pickExpression(tree)
      ) {
        (doc, ann, mods, ident, tconstrs, tparams, fparams, expr) =>
          val eff = None
          val tpe = WeededAst.Type.Ambiguous(Name.mkQName("Bool"), ident.loc)
          // TODO: There is a `Declaration.Law` but old Weeder produces a Def
          Declaration.Def(doc, ann, mods, ident, tparams, fparams, expr, tpe, eff, tconstrs, Nil, tree.loc)
      }
    }

    private def visitAssociatedTypeDef(tree: Tree, instType: Type)(implicit s: State): Validation[Declaration.AssocTypeDef, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.AssociatedTypeDef)
      flatMapN(
        pickDocumentation(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        Types.pickArguments(tree),
        Types.pickType(tree)
      ) {
        case (doc, mods, ident, typeArgs, tpe) =>
          val typeArg = typeArgs match {
            // Use instance type if type arguments were elided
            case Nil => Validation.success(instType)
            // Single argument: use that
            case head :: Nil => Validation.success(head)
            // Multiple type arguments: recover by arbitrarily picking the first one
            case types => Validation.toSoftFailure(types.head, NonUnaryAssocType(types.length, ident.loc))
          }

          mapN(typeArg) {
            typeArg => Declaration.AssocTypeDef(doc, mods, ident, typeArg, tpe, tree.loc)
          }
      }
    }

    private def visitAssociatedTypeSig(tree: Tree, classTypeParam: TypeParam)(implicit s: State): Validation[Declaration.AssocTypeSig, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.AssociatedTypeSig)
      flatMapN(
        pickDocumentation(tree),
        pickModifiers(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
      ) {
        case (doc, mods, ident, tparams) =>
          val kind = Types.tryPickKind(tree).getOrElse(defaultKind(ident))
          val tparam = tparams match {
            // Elided: Use class type parameter
            case TypeParams.Elided => Validation.success(classTypeParam)
            // Single type parameter
            case TypeParams.Unkinded(head :: Nil) => Validation.success(head)
            case TypeParams.Kinded(head :: Nil) => Validation.success(head)
            // Multiple type parameters. Soft fail by picking the first parameter
            case TypeParams.Unkinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
            case TypeParams.Kinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
          }
          mapN(tparam) {
            tparam => Declaration.AssocTypeSig(doc, mods, ident, tparam, kind, tree.loc)
          }
      }
    }

    private def visitSignature(tree: Tree)(implicit s: State): Validation[Declaration.Sig, CompilationMessage] = {
      assert(tree.kind == TreeKind.Decl.Signature)
      val maybeExpression = tryPick(TreeKind.Expr.Expr, tree.children)
      mapN(
        pickDocumentation(tree),
        pickAnnotations(tree),
        pickModifiers(tree, allowed = Set(TokenKind.KeywordSealed, TokenKind.KeywordLawful, TokenKind.KeywordPub), mustBePublic = true),
        pickNameIdent(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Types.pickType(tree),
        Types.tryPickEffect(tree),
        Types.pickConstraints(tree),
        traverseOpt(maybeExpression)(Exprs.visitExpression)
      ) {
        case (doc, annotations, modifiers, ident, tparams, fparams, tpe, eff, tconstrs, expr) =>
          Declaration.Sig(doc, annotations, modifiers, ident, tparams, fparams, expr, tpe, eff, tconstrs, tree.loc)
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
      Validation.success(Ast.Doc(comments, loc))
    }

    def pickAnnotations(tree: Tree)(implicit s: State): Validation[Ast.Annotations, CompilationMessage] = {
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
        .getOrElse(Validation.success(List.empty))

      mapN(annotations)(Ast.Annotations(_))
    }

    private def visitAnnotation(token: Token)(implicit s: State): Validation[Ast.Annotation, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      import Ast.Annotation._
      token.text match {
        case "@benchmark" | "@Benchmark" => Validation.success(Benchmark(loc))
        case "@Deprecated" => Validation.success(Deprecated(loc))
        case "@Experimental" => Validation.success(Experimental(loc))
        case "@Internal" => Validation.success(Internal(loc))
        case "@Parallel" => Validation.success(Parallel(loc))
        case "@ParallelWhenPure" => Validation.success(ParallelWhenPure(loc))
        case "@Lazy" => Validation.success(Lazy(loc))
        case "@LazyWhenPure" => Validation.success(LazyWhenPure(loc))
        case "@MustUse" => Validation.success(MustUse(loc))
        case "@Skip" => Validation.success(Skip(loc))
        case "@Test" | "@test" => Validation.success(Test(loc))
        case "@TailRec" => Validation.success(TailRecursive(loc))
        case other => softFailWith(Ast.Annotation.Error(other.stripPrefix("@"), loc))
      }
    }

    private def pickConstraints(tree: Tree): Validation[List[EqualityConstraint], CompilationMessage] = {
      // TODO
      Validation.success(List.empty)
    }

    val ALL_MODIFIERS: Set[TokenKind] = Set(
      TokenKind.KeywordSealed,
      TokenKind.KeywordLawful,
      TokenKind.KeywordPub,
      TokenKind.KeywordOverride,
      TokenKind.KeywordInline)

    def pickModifiers(tree: Tree, allowed: Set[TokenKind] = ALL_MODIFIERS, mustBePublic: Boolean = false)(implicit s: State): Validation[Ast.Modifiers, CompilationMessage] = {
      tryPick(TreeKind.Modifiers, tree.children) match {
        case None => Validation.success(Ast.Modifiers(List.empty))
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

          val mods = traverse(tokens)(visitModifier(_, allowed))
            .withSoftFailures(errors)
          mapN(mods)(Ast.Modifiers(_))
      }
    }

    private def visitModifier(token: Token, allowed: Set[TokenKind])(implicit s: State): Validation[Ast.Modifier, CompilationMessage] = {
      val mod = token.kind match {
        // TODO: there is no Ast.Modifier for 'inline'
        case TokenKind.KeywordSealed => Validation.success(Ast.Modifier.Sealed)
        case TokenKind.KeywordLawful => Validation.success(Ast.Modifier.Lawful)
        case TokenKind.KeywordPub => Validation.success(Ast.Modifier.Public)
        case TokenKind.KeywordOverride => Validation.success(Ast.Modifier.Override)
        // TODO: This could be a SoftFailure if we had Ast.Modifier.Error
        case kind => failWith(s"unsupported modifier $kind")
      }
      if (!allowed.contains(token.kind)) {
        mod.withSoftFailure(IllegalModifier(token.mkSourceLocation(s.src, Some(s.parserInput))))
      } else {
        mod
      }
    }

    def unitFormalParameter(loc: SourceLocation): FormalParam = FormalParam(
      Name.Ident(SourcePosition.Unknown, "_unit", SourcePosition.Unknown),
      Ast.Modifiers(List.empty),
      Some(Type.Unit(loc)),
      loc
    )

    def pickFormalParameters(tree: Tree, presence: Presence = Presence.Required)(implicit s: State): Validation[List[FormalParam], CompilationMessage] = {
      val paramTree = tryPick(TreeKind.Parameters, tree.children)
      paramTree.map(
        t => {
          val params = pickAll(TreeKind.Parameter, t.children)
          if (params.isEmpty) {
            Validation.success(List(unitFormalParameter(t.loc)))
          } else {
            flatMapN(traverse(params)(visitParameter(_, presence))) {
              params =>
                // Check for duplicates
                val paramsWithoutWildcards = params.filter(!_.ident.isWild)
                val errors = getDuplicates(paramsWithoutWildcards, (p: FormalParam) => p.ident.name)
                  .map(pair => DuplicateFormalParam(pair._1.ident.name, pair._1.loc, pair._2.loc))

                // Check missing or illegal type ascription
                Validation.success(params).withSoftFailures(errors)
            }
          }
        }
      ).getOrElse(
        // Soft fail by pretending there were no arguments
        softFailWith(List(unitFormalParameter(SourceLocation.Unknown)))
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
            case (Some(typeTree), _) => mapN(Types.visitType(typeTree)) { tpe => FormalParam(ident, mods, Some(tpe), tree.loc) }
            case (None, _) => Validation.success(FormalParam(ident, mods, None, tree.loc))
          }
      }
    }
  }

  private object Exprs {
    def pickExpression(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val maybeExpression = tryPick(TreeKind.Expr.Expr, tree.children)
      flatMapN(
        traverseOpt(maybeExpression)(visitExpression)
      ) {
        case Some(expr) => Validation.success(expr)
        case None => softFailWith(Expr.Error(Parse2Error.DevErr(tree.loc, "expected expression")))
      }
    }

    def visitExpression(exprTree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(exprTree.kind == TreeKind.Expr.Expr)
      val tree = unfold(exprTree)
      tree.kind match {
        case TreeKind.Expr.Literal => visitLiteral(tree)
        case TreeKind.Expr.Tuple => visitTuple(tree)
        case TreeKind.Expr.Call => visitCall(tree)
        case TreeKind.Expr.LetImport => visitLetImport(tree)
        case TreeKind.Expr.Binary => visitBinary(tree)
        case TreeKind.Expr.Unary => visitUnary(tree)
        case TreeKind.Expr.Paren => visitParen(tree)
        case TreeKind.Expr.IfThenElse => visitIfThenElse(tree)
        case TreeKind.Expr.Block => visitBlock(tree)
        case TreeKind.Expr.Statement => visitStatement(tree)
        case TreeKind.Expr.Use => visitExprUse(tree)
        case TreeKind.Expr.Try => visitTry(tree)
        case TreeKind.Expr.Without => visitWithout(tree)
        case TreeKind.Expr.FixpointQuery => visitFixpointQuery(tree)
        case TreeKind.Expr.FixpointConstraintSet => visitFixpointConstraintSet(tree)
        case TreeKind.Expr.FixpointSolve => visitFixpointSolve(tree)
        case TreeKind.Expr.FixpointInject => visitFixpointInject(tree)
        case TreeKind.Expr.NewObject => visitNewObject(tree)
        case TreeKind.Expr.LetRecDef => visitLetRecDef(tree)
        case TreeKind.Expr.Ascribe => visitAscribe(tree)
        case TreeKind.Expr.Match => visitMatch(tree)
        case TreeKind.Expr.Do => visitDo(tree)
        case TreeKind.Expr.Open => visitOpen(tree)
        case TreeKind.Expr.OpenAs => visitOpenAs(tree)
        case TreeKind.Expr.ParYield => visitParYield(tree)
        case TreeKind.Expr.TypeMatch => visitTypeMatch(tree)
        case TreeKind.Expr.CheckedTypeCast => visitCheckedTypeCast(tree)
        case TreeKind.Expr.CheckedEffectCast => visitCheckedEffectCast(tree)
        case TreeKind.Expr.UncheckedCast => visitUncheckedCast(tree)
        case TreeKind.Expr.UncheckedMaskingCast => visitUncheckedMaskingCast(tree)
        case TreeKind.Expr.StringInterpolation => visitStringInterpolation(tree)
        case TreeKind.Expr.LetMatch => visitLetMatch(tree)
        case TreeKind.Expr.Foreach => visitForeach(tree)
        case TreeKind.Expr.ForeachYield => visitForeachYield(tree)
        case TreeKind.Expr.ForMonadic => visitForMonadic(tree)
        case TreeKind.Expr.ForApplicative => visitForApplicative(tree)
        case TreeKind.Expr.Hole => visitHole(tree)
        case TreeKind.Expr.Scope => visitScope(tree)
        case TreeKind.Expr.Lambda => visitLambda(tree)
        case TreeKind.Expr.LambdaMatch => visitLambdaMatch(tree)
        case TreeKind.Expr.Ref => visitReference(tree)
        case TreeKind.Expr.Spawn => visitSpawn(tree)
        case TreeKind.Expr.Static => visitStatic(tree)
        case TreeKind.Expr.LiteralRecord => visitLiteralRecord(tree)
        case TreeKind.Expr.LiteralList => visitLiteralList(tree)
        case TreeKind.Expr.LiteralSet => visitLiteralSet(tree)
        case TreeKind.Expr.LiteralVector => visitLiteralVector(tree)
        case TreeKind.Expr.LiteralArray => visitLiteralArray(tree)
        case TreeKind.Expr.LiteralMap => visitLiteralMap(tree)
        case TreeKind.Expr.RecordSelect => visitRecordSelect(tree)
        case TreeKind.Ident => mapN(tokenToIdent(tree)) { ident => Expr.Ambiguous(Name.mkQName(ident), tree.loc) }
        case TreeKind.QName => visitExprQname(tree)
        case kind => failWith(s"TODO: implement expression of kind '$kind'", tree.loc)
      }
    }

    private def visitWithout(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Without)
      val effectSet = pick(TreeKind.Type.EffectSet, tree.children)
      val effects = flatMapN(effectSet)(effectSetTree => traverse(pickAll(TreeKind.QName, effectSetTree.children))(visitQName))
      mapN(
        pickExpression(tree),
        effects
      )((expr, effects) => {
        val base = Expr.Without(expr, effects.head, tree.loc)
        effects.tail.foldLeft(base) {
          case (acc, eff) => Expr.Without(acc, eff, tree.loc.asSynthetic)
        }
      })
    }

    private def visitOpen(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Open)
      mapN(pickQName(tree))(Expr.Open(_, tree.loc))
    }

    private def visitOpenAs(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.OpenAs)
      mapN(pickQName(tree), pickExpression(tree))((name, expr) => Expr.OpenAs(name, expr, tree.loc))
    }

    private def visitDo(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Do)
      mapN(pickQName(tree), pickArguments(tree))((op, args) => Expr.Do(op, args, tree.loc))
    }

    private def visitFixpointInject(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.FixpointInject)
      val expressions = pickAll(TreeKind.Expr.Expr, tree.children)
      val idents = pickAll(TreeKind.Ident, tree.children)
      flatMapN(
        traverse(expressions)(visitExpression),
        traverse(idents)(tokenToIdent)
      ) {
        case (exprs, idents) if exprs.length != idents.length =>
          // Check for mismatched arity
          val err = MismatchedArity(exprs.length, idents.length, tree.loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (exprs, idents) => Validation.success(Expr.FixpointInjectInto(exprs, idents, tree.loc))
      }
    }

    private def visitFixpointSolve(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.FixpointSolve)
      val expressions = pickAll(TreeKind.Expr.Expr, tree.children)
      val idents = pickAll(TreeKind.Ident, tree.children)
      mapN(
        traverse(expressions)(visitExpression),
        traverse(idents)(tokenToIdent)
      ) {
        (exprs, idents) =>
          val optIdents = if (idents.isEmpty) None else Some(idents)
          Expr.FixpointSolveWithProject(exprs, optIdents, tree.loc)
      }
    }

    private def visitFixpointQuery(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.FixpointQuery)
      val expressions = traverse(pickAll(TreeKind.Expr.Expr, tree.children))(visitExpression)
      val selects = flatMapN(pick(TreeKind.Expr.FixpointSelect, tree.children))(
        selectTree => traverse(pickAll(TreeKind.Expr.Expr, selectTree.children))(visitExpression)
      )
      val froms = flatMapN(pick(TreeKind.Expr.FixpointFrom, tree.children))(
        fromTree => traverse(pickAll(TreeKind.Predicate.Atom, fromTree.children))(Predicates.visitAtom)
      )
      val where = traverseOpt(tryPick(TreeKind.Expr.FixpointWhere, tree.children))(pickExpression)
      mapN(expressions, selects, froms, where) {
        (expressions, selects, froms, where) =>
          val whereList = where.map(w => List(w)).getOrElse(List.empty)
          Expr.FixpointQueryWithSelect(expressions, selects, froms, whereList, tree.loc)
      }
    }

    private def visitFixpointConstraintSet(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.FixpointConstraintSet)
      val constraints = pickAll(TreeKind.Expr.FixpointConstraint, tree.children)
      mapN(traverse(constraints)(visitFixpointConstraint)) {
        constraints => Expr.FixpointConstraintSet(constraints, tree.loc)
      }
    }

    private def visitFixpointConstraint(tree: Tree)(implicit s: State): Validation[Constraint, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.FixpointConstraint)
      val bodyItems = pickAll(TreeKind.Predicate.Body, tree.children)
      mapN(
        Predicates.pickHead(tree),
        traverse(bodyItems)(Predicates.visitBody),
      ) {
        (head, body) => Constraint(head, body, tree.loc)
      }
    }

    private def visitNewObject(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.NewObject)
      val methods = pickAll(TreeKind.Expr.JvmMethod, tree.children)
      mapN(
        Types.pickType(tree),
        traverse(methods)(visitJvmMethod),
      )((tpe, methods) => Expr.NewObject(tpe, methods, tree.loc))
    }

    private def visitJvmMethod(tree: Tree)(implicit s: State): Validation[JvmMethod, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.JvmMethod)
      mapN(
        pickNameIdent(tree),
        pickExpression(tree),
        Decls.pickFormalParameters(tree),
        Types.pickType(tree),
        Types.tryPickEffect(tree),
      )((ident, expr, fparams, tpe, eff) => JvmMethod(ident, fparams, expr, tpe, eff, tree.loc))
    }

    private def visitParYield(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ParYield)
      val fragments = pickAll(TreeKind.Expr.ParYieldFragment, tree.children)
      mapN(
        traverse(fragments)(visitParYieldFragment),
        pickExpression(tree)
      )((fragments, expr) => Expr.ParYield(fragments, expr, tree.loc))
    }

    private def visitParYieldFragment(tree: Tree)(implicit s: State): Validation[ParYieldFragment, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ParYieldFragment)
      mapN(
        Patterns.pickPattern(tree),
        pickExpression(tree)
      )((pat, expr) => ParYieldFragment(pat, expr, tree.loc))
    }

    private def visitForeach(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Foreach)
      mapN(
        pickForFragments(tree),
        pickExpression(tree)
      )((fragments, expr) => {
        Expr.ForEach(fragments, expr, tree.loc)
      })
    }

    private def visitForeachYield(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForeachYield)
      mapN(
        pickForFragments(tree),
        pickExpression(tree)
      )((fragments, expr) => {
        Expr.ForEachYield(fragments, expr, tree.loc)
      })
    }

    private def visitForMonadic(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForMonadic)
      mapN(
        pickForFragments(tree),
        pickExpression(tree)
      )((fragments, expr) => {
        Expr.MonadicFor(fragments, expr, tree.loc)
      })
    }

    private def visitForApplicative(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForApplicative)
      val generators = pickAll(TreeKind.Expr.ForFragmentGenerator, tree.children)
      mapN(
        traverse(generators)(visitForFragmentGenerator),
        pickExpression(tree)
      )((fragments, expr) => Expr.ApplicativeFor(fragments, expr, tree.loc))
    }


    private def forFragmentLoc(frag: ForFragment): SourceLocation = frag match {
      case ForFragment.Generator(_, _, loc) => loc
      case ForFragment.Guard(_, loc) => loc
      case ForFragment.Let(_, _, loc) => loc
    }
    private def pickForFragments(tree: Tree)(implicit s: State): Validation[List[ForFragment], CompilationMessage] = {
      val guards = pickAll(TreeKind.Expr.ForFragmentGuard, tree.children)
      val generators = pickAll(TreeKind.Expr.ForFragmentGenerator, tree.children)
      val lets = pickAll(TreeKind.Expr.ForFragmentLet, tree.children)
      mapN(
        traverse(guards)(visitForFragmentGuard),
        traverse(generators)(visitForFragmentGenerator),
        traverse(lets)(visitForFragmentLet),
      )((guards, generators, lets) => {
        (generators ++ guards ++ lets).sortBy(forFragmentLoc)
      })
    }

    private def visitForFragmentGuard(tree: Tree)(implicit s: State): Validation[ForFragment.Guard, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForFragmentGuard)
      mapN(pickExpression(tree))(ForFragment.Guard(_, tree.loc))
    }

    private def visitForFragmentGenerator(tree: Tree)(implicit s: State): Validation[ForFragment.Generator, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForFragmentGenerator)
      mapN(
        Patterns.pickPattern(tree),
        pickExpression(tree)
      )((pat, expr) => ForFragment.Generator(pat, expr, tree.loc))
    }

    private def visitForFragmentLet(tree: Tree)(implicit s: State): Validation[ForFragment.Let, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ForFragmentLet)
      mapN(
        Patterns.pickPattern(tree),
        pickExpression(tree)
      )((pat, expr) => ForFragment.Let(pat, expr, tree.loc))
    }

    private def visitExprQname(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.QName)
      val idents = pickAll(TreeKind.Ident, tree.children)
      flatMapN(traverse(idents)(tokenToIdent)) {
        idents =>
          val first = idents.head
          val ident = idents.last
          val nnameIdents = idents.dropRight(1)
          val nname = Name.NName(first.sp1, nnameIdents, ident.sp2)
          val qname = Name.QName(first.sp1, nname, ident, ident.sp2)
          val prefix = idents.takeWhile(_.isUpper)
          val suffix = idents.dropWhile(_.isUpper)

          suffix match {
            // Case 1: upper qualified name
            case Nil =>
              // NB: We only use the source location of the identifier itself.
              Validation.success(Expr.Ambiguous(qname, qname.ident.loc))
            // Case 1: basic qualified name
            case ident :: Nil =>
              // NB: We only use the source location of the identifier itself.
              Validation.success(Expr.Ambiguous(qname, ident.loc))
            // Case 2: actually a record access
            case ident :: labels =>
              // NB: We only use the source location of the identifier itself.
              val base = Expr.Ambiguous(Name.mkQName(prefix.map(_.toString), ident.name, ident.sp1, ident.sp2), ident.loc)
              Validation.success(labels.foldLeft(base: Expr) {
                case (acc, label) => Expr.RecordSelect(acc, Name.mkLabel(label), label.loc)
              })
          }
      }
    }

    private def visitTuple(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Tuple)
      val expressions = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(traverse(expressions)(visitExpression))(Expr.Tuple(_, tree.loc))
    }

    private def visitTry(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Try)
      val maybeCatch = pickAll(TreeKind.Expr.TryCatchBody, tree.children)
      val maybeWith = pickAll(TreeKind.Expr.TryWithBody, tree.children)
      flatMapN(
        pickExpression(tree),
        traverse(maybeCatch)(visitTryCatchBody),
        traverse(maybeWith)(visitTryWithBody),
      ) {
        // Bad case: try expr
        case (expr, Nil, Nil) => Validation.SoftFailure(
          Expr.TryCatch(expr, List.empty, tree.loc),
          Chain(Parse2Error.DevErr(tree.loc, s"Missing `catch` on try-catch expression"))
        )
        // Bad case: try expr catch { rules... } with eff { handlers... }
        case (expr, _ :: _, _ :: _) => Validation.SoftFailure(
          Expr.TryCatch(expr, List.empty, tree.loc),
          Chain(Parse2Error.DevErr(tree.loc, s"Cannot use both `catch` and `with` on try-catch expression"))
        )
        // Case: try expr catch { rules... }
        case (expr, catches, Nil) => Validation.success(Expr.TryCatch(expr, catches.flatten, tree.loc))
        // Case: try expr with eff { handlers... }
        case (expr, Nil, withs) => Validation.success(Expr.TryWith(expr, withs, tree.loc))
      }
    }

    private def visitTryCatchBody(tree: Tree)(implicit s: State): Validation[List[CatchRule], CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TryCatchBody)
      val rules = pickAll(TreeKind.Expr.TryCatchRule, tree.children)
      traverse(rules)(visitTryCatchRule)
    }

    private def visitTryCatchRule(tree: Tree)(implicit s: State): Validation[CatchRule, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TryCatchRule)
      mapN(
        pickNameIdent(tree),
        pickQName(tree),
        pickExpression(tree)
      )((ident, qname, expr) => CatchRule(ident, javaQnameToFqn(qname), expr))
    }

    private def visitTryWithBody(tree: Tree)(implicit s: State): Validation[WithHandler, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TryWithBody)
      val rules = pickAll(TreeKind.Expr.TryWithRule, tree.children)
      mapN(
        pickQName(tree), // This qname is an effect
        traverse(rules)(visitTryWithRule)
      )((eff, handlers) => WithHandler(eff, handlers))
    }

    private def visitTryWithRule(tree: Tree)(implicit s: State): Validation[HandlerRule, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TryWithRule)
      mapN(
        pickNameIdent(tree),
        Decls.pickFormalParameters(tree, Presence.Forbidden),
        pickExpression(tree)
      )((ident, fparams, expr) => {
        // Add extra resumption argument as a synthetic unit parameter when there is exatly one parameter.
        val hasSingleNonUnitParam = fparams.sizeIs == 1 && fparams.exists(_.ident.name != "_unit")
        val syntheticUnitParam = if (hasSingleNonUnitParam) List(Decls.unitFormalParameter(tree.loc.asSynthetic)) else List.empty
        HandlerRule(ident, (syntheticUnitParam ++ fparams).sortBy(_.loc), expr)
      })
    }

    private def visitRecordSelect(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.RecordSelect)
      val idents = pickAll(TreeKind.Ident, tree.children)
      mapN(pickExpression(tree), traverse(idents)(tokenToIdent))(
        (expr, idents) =>
          idents.foldLeft(expr) {
            case (acc, ident) => Expr.RecordSelect(acc, Name.mkLabel(ident), ident.loc)
          }
      )
    }

    private def visitReference(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Ref)
      val scopeName = tryPick(TreeKind.Expr.ScopeName, tree.children)
      flatMapN(
        pickExpression(tree),
        traverseOpt(scopeName)(visitScopeName)
      ) {
        case (expr1, Some(expr2)) => Validation.success(Expr.Ref(expr1, expr2, tree.loc))
        case (expr1, None) =>
          val err = Parse2Error.DevErr(tree.loc, "Missing scope in ref")
          Validation.SoftFailure(Expr.Ref(expr1, Expr.Error(err), tree.loc), Chain(err))
      }
    }

    private def visitSpawn(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Spawn)
      val scopeName = tryPick(TreeKind.Expr.ScopeName, tree.children)
      flatMapN(
        pickExpression(tree),
        traverseOpt(scopeName)(visitScopeName)
      ) {
        case (expr1, Some(expr2)) => Validation.success(Expr.Spawn(expr1, expr2, tree.loc))
        case (expr1, None) =>
          val err = Parse2Error.DevErr(tree.loc, "Missing scope in spawn")
          Validation.SoftFailure(Expr.Spawn(expr1, Expr.Error(err), tree.loc), Chain(err))
      }
    }

    private def visitStatic(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Static)
      Validation.success(Expr.Static(tree.loc))
    }

    private def visitScopeName(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.ScopeName)
      pickExpression(tree)
    }

    private def visitLiteralRecord(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralRecord)
      val fields = pickAll(TreeKind.Expr.LiteralRecordField, tree.children)
      mapN(
        traverse(fields)(visitLiteralRecordField)
      )(fields => fields.foldRight(Expr.RecordEmpty(tree.loc.asSynthetic): Expr) {
        case ((label, expr, loc), acc) => Expr.RecordExtend(label, expr, acc, loc)
      })
    }

    private def visitLiteralRecordField(tree: Tree)(implicit s: State): Validation[(Name.Label, Expr, SourceLocation), CompilationMessage] = {
      mapN(
        pickNameIdent(tree),
        pickExpression(tree)
      )((ident, expr) => (Name.mkLabel(ident), expr, tree.loc))
    }

    private def visitLiteralList(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralList)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(traverse(exprs)(visitExpression))(Expr.ListLit(_, tree.loc))
    }

    private def visitLiteralSet(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralSet)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(traverse(exprs)(visitExpression))(Expr.SetLit(_, tree.loc))
    }

    private def visitLiteralVector(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralVector)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(traverse(exprs)(visitExpression))(Expr.VectorLit(_, tree.loc))
    }

    private def visitLiteralArray(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralArray)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      val scopeName = tryPick(TreeKind.Expr.ScopeName, tree.children)
      flatMapN(
        traverse(exprs)(visitExpression),
        traverseOpt(scopeName)(visitScopeName)
      ) {
        case (exprs, Some(scope)) => Validation.success(Expr.ArrayLit(exprs, scope, tree.loc))
        case (exprs, None) =>
          val err = Parse2Error.DevErr(tree.loc, "Missing scope in array literal")
          Validation.SoftFailure(Expr.ArrayLit(exprs, Expr.Error(err), tree.loc), Chain(err))
      }
    }

    private def visitLiteralMap(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LiteralMap)
      val pairs = pickAll(TreeKind.Expr.KeyValue, tree.children)
      mapN(traverse(pairs)(visitKeyValuePair))(Expr.MapLit(_, tree.loc))
    }

    private def visitKeyValuePair(tree: Tree)(implicit s: State): Validation[(Expr, Expr), CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.KeyValue)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      flatMapN(traverse(exprs)(visitExpression)) {
        // case: k => v
        case k :: v :: Nil => Validation.success((k, v))
        // case: k =>
        case k :: Nil =>
          val err = Parse2Error.DevErr(tree.loc, "Missing value in key-value pair")
          Validation.SoftFailure((k, Expr.Error(err)), Chain(err))
        case xs => throw InternalCompilerException(s"Malformed KeyValue pair, found ${xs.length} expressions", tree.loc)
      }
    }

    private def visitScope(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Scope)
      val block = flatMapN(pick(TreeKind.Expr.Block, tree.children))(visitBlock)
      mapN(pickNameIdent(tree), block)((ident, block) => Expr.Scope(ident, block, tree.loc))
    }

    private def visitUnary(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Unary)
      mapN(
        pick(TreeKind.Operator, tree.children),
        pickExpression(tree)
      )(
        (opTree, expr) => {
          val op = text(opTree).head
          val sp1 = tree.loc.sp1
          val sp2 = tree.loc.sp2
          op match {
            case "discard" => Expr.Discard(expr, tree.loc)
            case "lazy" => Expr.Lazy(expr, tree.loc)
            case "force" => Expr.Force(expr, tree.loc)
            case "deref" => Expr.Deref(expr, tree.loc)
            case "not" => Expr.Unary(SemanticOp.BoolOp.Not, expr, tree.loc)
            case "+" => expr
            case "-" =>
              // Map unary minus into a constant if it is used on a number literal
              val maybeCst: Option[Expr] = expr match {
                case Expr.Cst(cst, loc) => cst match {
                  case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(-lit), loc))
                  case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(-lit), loc))
                  case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.negate()), loc))
                  case Constant.Int8(lit) => try {
                    val num = JByte.parseByte(s"-$lit")
                    Some(Expr.Cst(Constant.Int8(num), loc))
                  } catch {
                    case _: NumberFormatException => Some(WeededAst.Expr.Error(MalformedInt(loc)))
                  }
                  case Constant.Int16(lit) =>
                    try {
                      val num = JShort.parseShort(s"-$lit")
                      Some(Expr.Cst(Constant.Int16(num), loc))
                    } catch {
                      case _: NumberFormatException => Some(WeededAst.Expr.Error(MalformedInt(loc)))
                    }
                  case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(-lit), loc))
                  case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(-lit), loc))
                  case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.negate()), loc))
                  case _ => None
                }
                case _ => None
              }
              maybeCst.getOrElse(Expr.Apply(Expr.Ambiguous(Name.mkQName("Neg.neg", sp1, sp2), opTree.loc), List(expr), tree.loc))

            case op => Expr.Apply(Expr.Ambiguous(Name.mkQName(op, sp1, sp2), opTree.loc), List(expr), tree.loc)
          }
        }
      )
    }

    private def visitLetRecDef(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LetRecDef)
      val annVal = flatMapN(Decls.pickAnnotations(tree)) {
        case Ast.Annotations(as) =>
          // Check for [[IllegalAnnotation]]
          val errors = collection.mutable.ArrayBuffer.empty[IllegalAnnotation]
          for (a <- as) {
            a match {
              case Ast.Annotation.TailRecursive(_) => // OK
              case otherAnn => errors += IllegalAnnotation(otherAnn.loc)
            }
          }
          Validation.toSuccessOrSoftFailure(Ast.Annotations(as), errors)
      }

      val exprs = flatMapN(pickExpression(tree)) {
        case Expr.Stm(exp1, exp2, _) => Validation.success((exp1, exp2))
        case e => softFailWith(e, Expr.Error(Parse2Error.DevErr(tree.loc, "A internal definition must be followed by an expression")))
      }

      mapN(
        annVal,
        exprs,
        Decls.pickFormalParameters(tree, Presence.Optional),
        pickNameIdent(tree),
        Types.tryPickType(tree),
        Types.tryPickEffect(tree),
      ) {
        case (ann, (exp1, exp2), fparams, ident, tpe, eff) =>
          val e = if (tpe.isDefined || eff.isDefined) Expr.Ascribe(exp1, tpe, eff, exp1.loc) else exp1
          val lambda = fparams.foldRight(e) {
            case (fparam, acc) => WeededAst.Expr.Lambda(fparam, acc, exp1.loc.asSynthetic)
          }
          Expr.LetRec(ident, ann, Ast.Modifiers.Empty, lambda, exp2, tree.loc)
      }
    }

    private def visitLetMatch(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LetMatch)
      flatMapN(
        Patterns.pickPattern(tree),
        Types.tryPickType(tree),
        pickExpression(tree)
      ) {
        (patrn, tpe, expr) =>
          // get expr1 and expr2 from the nested statement within expr.
          val exprs = expr match {
            case Expr.Stm(exp1, exp2, _) => Validation.success((exp1, exp2))
            case e => softFailWith(
              e,
              Expr.Error(Parse2Error.DevErr(tree.loc, "A let-binding must be followed by an expression"))
            )
          }
          mapN(exprs)(exprs => Expr.LetMatch(patrn, Ast.Modifiers.Empty, tpe, exprs._1, exprs._2, tree.loc))
      }
    }

    private def visitIfThenElse(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
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

    private def visitLambda(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Lambda)
      mapN(
        pickExpression(tree),
        Decls.pickFormalParameters(tree, presence = Presence.Optional)
      )((expr, fparams) => {
        val l = tree.loc.asSynthetic
        fparams.foldRight(expr) {
          case (fparam, acc) => WeededAst.Expr.Lambda(fparam, acc, l)
        }
      })
    }

    private def visitLambdaMatch(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LambdaMatch)
      mapN(
        Patterns.pickPattern(tree),
        pickExpression(tree)
      )((pat, expr) => Expr.LambdaMatch(pat, expr, tree.loc))
    }

    private def visitAscribe(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Ascribe)
      mapN(
        pickExpression(tree),
        Types.tryPickTypeNoWild(tree),
        Types.tryPickEffect(tree)
      ) {
        (expr, tpe, eff) => Expr.Ascribe(expr, tpe, eff, tree.loc)
      }
    }

    private def visitMatch(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Match)
      val rules = pickAll(TreeKind.Expr.MatchRule, tree.children)
      mapN(
        pickExpression(tree),
        traverse(rules)(visitMatchRule)
      ) {
        (expr, rules) => Expr.Match(expr, rules, tree.loc)
      }
    }

    private def visitMatchRule(tree: Tree)(implicit s: State): Validation[MatchRule, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.MatchRule)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(
        Patterns.pickPattern(tree),
        traverse(exprs)(visitExpression)
      ) {
        // case pattern => expr
        case (pat, expr :: Nil) => MatchRule(pat, None, expr)
        // case pattern if expr => expr
        case (pat, expr1 :: expr2 :: Nil) => MatchRule(pat, Some(expr1), expr2)
        //BAD: case pattern if => expr
        case (pat, exprs) => throw InternalCompilerException(s"Malformed MatchRule: expected 1 or 2 expressions but found ${exprs.length}", tree.loc)
      }
    }

    private def visitTypeMatch(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TypeMatch)
      val rules = pickAll(TreeKind.Expr.TypeMatchRule, tree.children)
      mapN(
        pickExpression(tree),
        traverse(rules)(visitTypeMatchRule)
      ) {
        (expr, rules) => Expr.TypeMatch(expr, rules, tree.loc)
      }
    }

    private def visitTypeMatchRule(tree: Tree)(implicit s: State): Validation[TypeMatchRule, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.TypeMatchRule)
      mapN(
        pickNameIdent(tree),
        pickExpression(tree),
        Types.pickType(tree)
      ) {
        (ident, expr, ttype) => TypeMatchRule(ident, ttype, expr)
      }
    }

    private def visitBinary(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Binary)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      val op = pick(TreeKind.Operator, tree.children)
      flatMapN(op, traverse(exprs)(visitExpression)) {
        case (op, e1 :: e2 :: Nil) =>
          val isInfix = op.children.head match {
            case Child.Token(token) => token.kind == TokenKind.InfixFunction
            case _ => false
          }

          if (isInfix) {
            val infixName = text(op).head.stripPrefix("`").stripSuffix("`")
            val opExpr = Expr.Ambiguous(Name.mkQName(infixName, op.loc.sp1, op.loc.sp2), op.loc)
            return Validation.success(Expr.Infix(e1, opExpr, e2, tree.loc))
          }

          def mkApply(name: String): Expr.Apply = Expr.Apply(
            Expr.Ambiguous(Name.mkQName(name, tree.loc.sp1, tree.loc.sp2), tree.loc), List(e1, e2),
            tree.loc
          )

          text(op).head match {
            // BUILTINS
            case "+" => Validation.success(mkApply("Add.add"))
            case "-" => Validation.success(mkApply("Sub.sub"))
            case "*" => Validation.success(mkApply("Mul.mul"))
            case "/" => Validation.success(mkApply("Div.div"))
            case "<" => Validation.success(mkApply("Order.less"))
            case "<=" => Validation.success(mkApply("Order.lessEqual"))
            case ">" => Validation.success(mkApply("Order.greater"))
            case ">=" => Validation.success(mkApply("Order.greaterEqual"))
            case "==" => Validation.success(mkApply("Eq.eq"))
            case "!=" => Validation.success(mkApply("Eq.neq"))
            case "<=>" => Validation.success(mkApply("Order.compare"))
            // SEMANTIC OPS
            case "and" => Validation.success(Expr.Binary(SemanticOp.BoolOp.And, e1, e2, tree.loc))
            case "or" => Validation.success(Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, tree.loc))
            // SPECIAL
            case ":=" => Validation.success(Expr.Assign(e1, e2, tree.loc))
            case "::" => Validation.success(Expr.FCons(e1, e2, tree.loc))
            case ":::" => Validation.success(Expr.FAppend(e1, e2, tree.loc))
            case "instanceof" =>
              val classname = mapN(pickQName(exprs(1)))(javaQnameToFqn)
              mapN(classname)(Expr.InstanceOf(e1, _, tree.loc))

            // UNRECOGNIZED
            case id =>
              val ident = Name.Ident(tree.loc.sp1, id, tree.loc.sp2)
              Validation.success(Expr.Apply(Expr.Ambiguous(Name.mkQName(ident), ident.loc), List(e1, e2), tree.loc))
          }

        case (_, operands) => throw InternalCompilerException(s"Expr.Binary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitParen(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Paren)
      val expr = pickExpression(tree)
      mapN(expr)(expr => Expr.Tuple(List(expr), tree.loc))
    }

    private def visitStringInterpolation(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.StringInterpolation)
      val init = WeededAst.Expr.Cst(Ast.Constant.Str(""), tree.loc)
      var isDebug = false
      Validation.fold(tree.children, init: WeededAst.Expr) {
        // A string part: Concat it onto the result
        case (acc, Child.Token(token)) =>
          isDebug = token.kind == TokenKind.LiteralDebugStringL
          val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
          val lit0 = token.text.stripPrefix("\"").stripSuffix("\"").stripPrefix("}")
          val lit = if (isDebug) lit0.stripSuffix("%{") else lit0.stripSuffix("${")
          if (lit == "") {
            Validation.success(acc)
          } else {
            val unescapedLit = StringContext.processEscapes(lit)
            val cst = Expr.Cst(Ast.Constant.Str(unescapedLit), loc)
            Validation.success(Expr.Binary(SemanticOp.StringOp.Concat, acc, cst, tree.loc.asSynthetic))
          }
        // An expression part: Apply 'toString' to it and concat the result
        case (acc, Child.Tree(tree)) if tree.kind == TreeKind.Expr.Expr =>
          mapN(visitExpression(tree))(expr => {
            val loc = tree.loc.asSynthetic
            val funcName = if (isDebug) {
              isDebug = false
              "Debug.stringify"
            } else "ToString.toString"
            val str = Expr.Apply(Expr.Ambiguous(Name.mkQName(funcName), loc), List(expr), loc)
            Expr.Binary(SemanticOp.StringOp.Concat, acc, str, loc)
          })
        case (_, Child.Tree(t)) => throw InternalCompilerException(s"Parser placed child of kind '${t.kind}' in string interpolation", tree.loc)
      }
    }


    private def visitCheckedTypeCast(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.CheckedTypeCast)
      val expr = pickExpression(tree)
      mapN(expr)(expr => Expr.CheckedCast(Ast.CheckedCastType.TypeCast, expr, tree.loc))
    }


    private def visitCheckedEffectCast(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.CheckedEffectCast)
      val expr = pickExpression(tree)
      mapN(expr)(expr => Expr.CheckedCast(Ast.CheckedCastType.EffectCast, expr, tree.loc))
    }


    private def visitUncheckedCast(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.UncheckedCast)
      mapN(
        pickExpression(tree),
        Types.tryPickTypeNoWild(tree),
        Types.tryPickEffect(tree)
      ) {
        (expr, tpe, eff) => Expr.UncheckedCast(expr, tpe, eff, tree.loc)
      }
    }

    private def visitUncheckedMaskingCast(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.UncheckedMaskingCast)
      val expr = pickExpression(tree)
      mapN(expr)(expr => Expr.UncheckedMaskingCast(expr, tree.loc))
    }

    private def visitHole(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Hole)
      mapN(tryPickNameIdent(tree)) {
        ident =>
          val strippedIdent = ident.map(id => Name.Ident(id.sp1, id.name.stripPrefix("?"), id.sp2))
          Expr.Hole(strippedIdent, tree.loc)
      }
    }

    private def visitExprUse(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Use)
      val use = flatMapN(pick(TreeKind.UsesOrImports.Use, tree.children))(visitUse)
      val expr = pickExpression(tree)
      mapN(use, expr)((use, expr) => Expr.Use(use, expr, tree.loc))
    }

    private def visitBlock(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Block)
      pickExpression(tree)
    }

    private def visitStatement(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.Statement)
      val exprs = pickAll(TreeKind.Expr.Expr, tree.children)
      mapN(traverse(exprs)(visitExpression)) {
        case ex1 :: ex2 :: Nil => Expr.Stm(ex1, ex2, tree.loc)
        case exprs => throw InternalCompilerException(s"Parser error. Expected 2 expressions in statement but found '${exprs.length}'.", tree.loc)
      }
    }

    private def visitLetImport(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.Expr.LetImport)
      val jvmOp = flatMapN(pick(TreeKind.JvmOp.JvmOp, tree.children))(JvmOp.visitJvmOp)
      val expr = pickExpression(tree)
      mapN(jvmOp, expr) {
        (jvmOp, expr) => Expr.LetImport(jvmOp, expr, tree.loc)
      }
    }

    private def visitCall(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
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

    private def pickArguments(tree: Tree)(implicit s: State): Validation[List[Expr], CompilationMessage] = {
      flatMapN(
        pick(TreeKind.Arguments, tree.children)
      ) {
        argTree =>
          mapN(
            traverse(pickAll(TreeKind.Argument, argTree.children))(pickExpression),
            traverse(pickAll(TreeKind.ArgumentNamed, argTree.children))(visitArgumentNamed)
          )((unnamed, named) => unnamed ++ named match {
            // Add synthetic unit arguments if there are none
            case Nil => List(Expr.Cst(Ast.Constant.Unit, tree.loc))
            // TODO: sort by SourceLocation is only used for comparison with Weeder
            case args => args.sortBy(_.loc)
          })
      }
    }

    private def visitArgumentNamed(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      assert(tree.kind == TreeKind.ArgumentNamed)
      flatMapN(
        traverse(pickAll(TreeKind.Expr.Expr, tree.children))(visitExpression)
      ) {
        case e1 :: e2 :: Nil =>
          // First expression must be a name
          e1 match {
            case Expr.Ambiguous(qname, loc) =>
              Validation.success(Expr.RecordExtend(Name.mkLabel(qname.ident), e2, Expr.RecordEmpty(tree.loc), tree.loc))
            case _ =>
              val err = Parse2Error.DevErr(tree.loc, s"NamedArgument does not have a name")
              Validation.SoftFailure(Expr.Error(err), Chain(err))
          }
        case exprs =>
          val err = Parse2Error.DevErr(tree.loc, s"Found ${exprs.length} expressions under NamedArgument")
          Validation.SoftFailure(WeededAst.Expr.Error(err), Chain(err))
      }
    }

    private def visitIntrinsic(tree: Tree, args: List[Expr])(implicit s: State): Validation[Expr, CompilationMessage] = {
      val intrinsic = text(tree).head.stripPrefix("$").stripSuffix("$")
      val loc = tree.loc
      (intrinsic, args) match {
        case ("BOOL_NOT", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.BoolOp.Not, e1, loc))
        case ("BOOL_AND", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.BoolOp.And, e1, e2, loc))
        case ("BOOL_OR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, loc))
        case ("BOOL_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.BoolOp.Eq, e1, e2, loc))
        case ("BOOL_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.BoolOp.Neq, e1, e2, loc))

        case ("CHAR_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Eq, e1, e2, loc))
        case ("CHAR_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Neq, e1, e2, loc))
        case ("CHAR_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Lt, e1, e2, loc))
        case ("CHAR_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Le, e1, e2, loc))
        case ("CHAR_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Gt, e1, e2, loc))
        case ("CHAR_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.CharOp.Ge, e1, e2, loc))

        case ("FLOAT32_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Float32Op.Neg, e1, loc))
        case ("FLOAT32_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Add, e1, e2, loc))
        case ("FLOAT32_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Sub, e1, e2, loc))
        case ("FLOAT32_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Mul, e1, e2, loc))
        case ("FLOAT32_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Div, e1, e2, loc))
        case ("FLOAT32_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Exp, e1, e2, loc))
        case ("FLOAT32_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Eq, e1, e2, loc))
        case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Neq, e1, e2, loc))
        case ("FLOAT32_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Lt, e1, e2, loc))
        case ("FLOAT32_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Le, e1, e2, loc))
        case ("FLOAT32_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Gt, e1, e2, loc))
        case ("FLOAT32_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float32Op.Ge, e1, e2, loc))

        case ("FLOAT64_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Float64Op.Neg, e1, loc))
        case ("FLOAT64_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Add, e1, e2, loc))
        case ("FLOAT64_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Sub, e1, e2, loc))
        case ("FLOAT64_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Mul, e1, e2, loc))
        case ("FLOAT64_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Div, e1, e2, loc))
        case ("FLOAT64_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Exp, e1, e2, loc))
        case ("FLOAT64_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Eq, e1, e2, loc))
        case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Neq, e1, e2, loc))
        case ("FLOAT64_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Lt, e1, e2, loc))
        case ("FLOAT64_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Le, e1, e2, loc))
        case ("FLOAT64_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Gt, e1, e2, loc))
        case ("FLOAT64_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Float64Op.Ge, e1, e2, loc))

        case ("INT8_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int8Op.Neg, e1, loc))
        case ("INT8_NOT", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int8Op.Not, e1, loc))
        case ("INT8_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Add, e1, e2, loc))
        case ("INT8_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Sub, e1, e2, loc))
        case ("INT8_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Mul, e1, e2, loc))
        case ("INT8_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Div, e1, e2, loc))
        case ("INT8_REM", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Rem, e1, e2, loc))
        case ("INT8_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Exp, e1, e2, loc))
        case ("INT8_AND", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.And, e1, e2, loc))
        case ("INT8_OR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Or, e1, e2, loc))
        case ("INT8_XOR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Xor, e1, e2, loc))
        case ("INT8_SHL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Shl, e1, e2, loc))
        case ("INT8_SHR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Shr, e1, e2, loc))
        case ("INT8_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Eq, e1, e2, loc))
        case ("INT8_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Neq, e1, e2, loc))
        case ("INT8_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Lt, e1, e2, loc))
        case ("INT8_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Le, e1, e2, loc))
        case ("INT8_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Gt, e1, e2, loc))
        case ("INT8_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int8Op.Ge, e1, e2, loc))

        case ("INT16_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int16Op.Neg, e1, loc))
        case ("INT16_NOT", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int16Op.Not, e1, loc))
        case ("INT16_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Add, e1, e2, loc))
        case ("INT16_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Sub, e1, e2, loc))
        case ("INT16_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Mul, e1, e2, loc))
        case ("INT16_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Div, e1, e2, loc))
        case ("INT16_REM", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Rem, e1, e2, loc))
        case ("INT16_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Exp, e1, e2, loc))
        case ("INT16_AND", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.And, e1, e2, loc))
        case ("INT16_OR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Or, e1, e2, loc))
        case ("INT16_XOR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Xor, e1, e2, loc))
        case ("INT16_SHL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Shl, e1, e2, loc))
        case ("INT16_SHR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Shr, e1, e2, loc))
        case ("INT16_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Eq, e1, e2, loc))
        case ("INT16_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Neq, e1, e2, loc))
        case ("INT16_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Lt, e1, e2, loc))
        case ("INT16_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Le, e1, e2, loc))
        case ("INT16_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Gt, e1, e2, loc))
        case ("INT16_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int16Op.Ge, e1, e2, loc))

        case ("INT32_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int32Op.Neg, e1, loc))
        case ("INT32_NOT", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int32Op.Not, e1, loc))
        case ("INT32_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Add, e1, e2, loc))
        case ("INT32_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Sub, e1, e2, loc))
        case ("INT32_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Mul, e1, e2, loc))
        case ("INT32_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Div, e1, e2, loc))
        case ("INT32_REM", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Rem, e1, e2, loc))
        case ("INT32_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Exp, e1, e2, loc))
        case ("INT32_AND", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.And, e1, e2, loc))
        case ("INT32_OR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Or, e1, e2, loc))
        case ("INT32_XOR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Xor, e1, e2, loc))
        case ("INT32_SHL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Shl, e1, e2, loc))
        case ("INT32_SHR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Shr, e1, e2, loc))
        case ("INT32_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Eq, e1, e2, loc))
        case ("INT32_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Neq, e1, e2, loc))
        case ("INT32_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Lt, e1, e2, loc))
        case ("INT32_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Le, e1, e2, loc))
        case ("INT32_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Gt, e1, e2, loc))
        case ("INT32_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int32Op.Ge, e1, e2, loc))

        case ("INT64_NEG", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int64Op.Neg, e1, loc))
        case ("INT64_NOT", e1 :: Nil) => Validation.success(Expr.Unary(SemanticOp.Int64Op.Not, e1, loc))
        case ("INT64_ADD", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Add, e1, e2, loc))
        case ("INT64_SUB", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Sub, e1, e2, loc))
        case ("INT64_MUL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Mul, e1, e2, loc))
        case ("INT64_DIV", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Div, e1, e2, loc))
        case ("INT64_REM", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Rem, e1, e2, loc))
        case ("INT64_EXP", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Exp, e1, e2, loc))
        case ("INT64_AND", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.And, e1, e2, loc))
        case ("INT64_OR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Or, e1, e2, loc))
        case ("INT64_XOR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Xor, e1, e2, loc))
        case ("INT64_SHL", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Shl, e1, e2, loc))
        case ("INT64_SHR", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Shr, e1, e2, loc))
        case ("INT64_EQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Eq, e1, e2, loc))
        case ("INT64_NEQ", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Neq, e1, e2, loc))
        case ("INT64_LT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Lt, e1, e2, loc))
        case ("INT64_LE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Le, e1, e2, loc))
        case ("INT64_GT", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Gt, e1, e2, loc))
        case ("INT64_GE", e1 :: e2 :: Nil) => Validation.success(Expr.Binary(SemanticOp.Int64Op.Ge, e1, e2, loc))

        case ("CHANNEL_GET", e1 :: Nil) => Validation.success(Expr.GetChannel(e1, loc))
        case ("CHANNEL_PUT", e1 :: e2 :: Nil) => Validation.success(Expr.PutChannel(e1, e2, loc))
        case ("CHANNEL_NEW", e1 :: e2 :: Nil) => Validation.success(Expr.NewChannel(e1, e2, loc))

        case ("ARRAY_NEW", e1 :: e2 :: e3 :: Nil) => Validation.success(Expr.ArrayNew(e1, e2, e3, loc))
        case ("ARRAY_LENGTH", e1 :: Nil) => Validation.success(Expr.ArrayLength(e1, loc))
        case ("ARRAY_LOAD", e1 :: e2 :: Nil) => Validation.success(Expr.ArrayLoad(e1, e2, loc))
        case ("ARRAY_STORE", e1 :: e2 :: e3 :: Nil) => Validation.success(Expr.ArrayStore(e1, e2, e3, loc))

        case ("VECTOR_GET", e1 :: e2 :: Nil) => Validation.success(Expr.VectorLoad(e1, e2, loc))
        case ("VECTOR_LENGTH", e1 :: Nil) => Validation.success(Expr.VectorLength(e1, loc))

        case _ =>
          val err = UndefinedIntrinsic(loc)
          Validation.toSoftFailure(Expr.Error(err), err)
      }
    }

    def visitLiteral(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
      // Note: This visitor is used by both expression literals and pattern literals.
      // Hence this assert:
      assert(tree.kind == TreeKind.Expr.Literal || tree.kind == TreeKind.Pattern.Literal)

      tree.children(0) match {
        case Child.Token(token) => token.kind match {
          case TokenKind.KeywordNull => Validation.success(Expr.Cst(Ast.Constant.Null, token.mkSourceLocation(s.src, Some(s.parserInput))))
          case TokenKind.KeywordTrue => Validation.success(Expr.Cst(Ast.Constant.Bool(true), token.mkSourceLocation(s.src, Some(s.parserInput))))
          case TokenKind.KeywordFalse => Validation.success(Expr.Cst(Ast.Constant.Bool(false), token.mkSourceLocation(s.src, Some(s.parserInput))))
          case TokenKind.LiteralString => Constants.toStringCst(token)
          case TokenKind.LiteralChar => Constants.toChar(token)
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
  }

  private object Patterns {
    def pickPattern(tree: Tree)(implicit s: State): Validation[Pattern, CompilationMessage] = {
      flatMapN(pick(TreeKind.Pattern.Pattern, tree.children))(visitPattern(_))
    }

    def visitPattern(tree: Tree, seen: collection.mutable.Map[String, Name.Ident] = collection.mutable.Map.empty)(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Pattern)
      tree.children(0) match {
        case Child.Tree(tree) => tree.kind match {
          case TreeKind.Pattern.FCons => visitFCons(tree, seen)
          case TreeKind.Pattern.Variable => visitVariable(tree, seen)
          case TreeKind.Pattern.Tag => visitTag(tree, seen)
          case TreeKind.Pattern.Literal => visitLiteral(tree)
          case TreeKind.Pattern.Tuple => visitTuple(tree, seen)
          case TreeKind.Pattern.Record => visitRecord(tree, seen)
          case kind => throw InternalCompilerException(s"Invalid Pattern kind '$kind'", tree.loc)
        }
        case _ => throw InternalCompilerException(s"Pattern.Pattern had token child", tree.loc)
      }
    }

    private def visitFCons(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.FCons)
      // FCons are rewritten into tag patterns
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree.children)
      mapN(
        traverse(patterns)(visitPattern(_, seen))
      ) {
        case pat1 :: pat2 :: Nil =>
          val qname = Name.mkQName("List.Cons", tree.loc.sp1, tree.loc.sp2)
          val pat = Pattern.Tuple(List(pat1, pat2), tree.loc)
          Pattern.Tag(qname, pat, tree.loc)
        case pats => throw InternalCompilerException(s"${s.src.name} Pattern.FCons expected 2 but found '${pats.length}' sub-patterns", tree.loc)
      }
    }

    private def visitVariable(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Variable)
      flatMapN(pickNameIdent(tree))(
        ident => {
          if (ident.name == "_")
            Validation.success(Pattern.Wild(tree.loc))
          else {
            seen.get(ident.name) match {
              case Some(other) => Validation.toSoftFailure(
                Pattern.Var(ident, tree.loc),
                NonLinearPattern(ident.name, other.loc, tree.loc)
              )
              case None =>
                seen += (ident.name -> ident)
                Validation.success(Pattern.Var(ident, tree.loc))
            }
          }
        }
      )
    }

    private def visitTag(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Tag)
      val maybePat = tryPick(TreeKind.Pattern.Tuple, tree.children)
      mapN(
        pickQName(tree),
        traverseOpt(maybePat)(visitTuple(_, seen))
      ) {
        (qname, maybePat) =>
          maybePat match {
            case None =>
              // Synthetically add unit pattern to tag
              val lit = Pattern.Cst(Ast.Constant.Unit, tree.loc.asSynthetic)
              Pattern.Tag(qname, lit, tree.loc)
            case Some(pat) => Pattern.Tag(qname, pat, tree.loc)
          }
      }
    }

    private def visitLiteral(tree: Tree)(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Literal)
      flatMapN(Exprs.visitLiteral(tree)) {
        case Expr.Cst(cst, _) => cst match {
          case Constant.Null =>
            Validation.toSoftFailure(WeededAst.Pattern.Error(tree.loc), IllegalNullPattern(tree.loc))
          case Constant.Regex(_) =>
            Validation.toSoftFailure(WeededAst.Pattern.Error(tree.loc), IllegalRegexPattern(tree.loc))
          case c => Validation.success(Pattern.Cst(c, tree.loc))
        }
        case e => throw InternalCompilerException(s"Malformed Pattern.Literal. Expected literal but found $e", e.loc)
      }
    }

    private def visitTuple(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Tuple)
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree.children)
      mapN(traverse(patterns)(visitPattern(_, seen))) {
        case Nil => Pattern.Cst(Ast.Constant.Unit, tree.loc)
        case x :: Nil => x
        case xs => Pattern.Tuple(xs, tree.loc)
      }
    }

    private def visitRecord(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.Record)
      val fields = pickAll(TreeKind.Pattern.RecordField, tree.children)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree.children)
      flatMapN(
        traverse(fields)(visitRecordField(_, seen)),
        traverseOpt(maybePattern)(visitPattern(_, seen))
      ) {
        // Pattern { ... }
        case (fs, None) => Validation.success(Pattern.Record(fs, Pattern.RecordEmpty(tree.loc.asSynthetic), tree.loc))
        // Pattern { x, ... | r }
        case (x :: xs, Some(Pattern.Var(v, l))) => Validation.success(Pattern.Record(x :: xs, Pattern.Var(v, l), tree.loc))
        // Pattern { x, ... | _ }
        case (x :: xs, Some(Pattern.Wild(l))) => Validation.success(Pattern.Record(x :: xs, Pattern.Wild(l), tree.loc))
        // Illegal pattern: { | r}
        case (Nil, Some(r)) => Validation.toSoftFailure(r, EmptyRecordExtensionPattern(r.loc))
        // Illegal pattern: { x, ... | (1, 2, 3) }
        case (_, Some(r)) => Validation.toSoftFailure(Pattern.Error(r.loc), IllegalRecordExtensionPattern(r.loc))
      }
    }

    private def visitRecordField(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit s: State): Validation[Pattern.Record.RecordLabelPattern, CompilationMessage] = {
      assert(tree.kind == TreeKind.Pattern.RecordField)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree.children)
      flatMapN(pickNameIdent(tree), traverseOpt(maybePattern)(visitPattern(_, seen))) {
        case (ident, None) =>
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              Validation.success(Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc))
            case Some(other) =>
              val pat = Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc)
              Validation.toSoftFailure(pat, NonLinearPattern(ident.name, other.loc, ident.loc))
          }
        case (ident, pat) => Validation.success(Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), pat, tree.loc))
      }
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
        Validation.success(after(stripUnderscores(token.text), loc))
      } catch {
        case _: NumberFormatException =>
          val err = MalformedFloat(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
      }
    }

    private def tryParseInt(token: Token, after: (String, SourceLocation) => Expr)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      try {
        Validation.success(after(stripUnderscores(token.text), loc))
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
        val lit = token.text.stripPrefix("regex\"").stripSuffix("\"")
        val unescapedLit = StringContext.processEscapes(lit)
        val pattern = JPattern.compile(unescapedLit)
        Validation.success(Expr.Cst(Ast.Constant.Regex(pattern), loc))
      } catch {
        case ex: PatternSyntaxException =>
          val err = MalformedRegex(token.text, ex.getMessage, loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
      }
    }

    def toChar(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      val lit = token.text.stripPrefix("\'").stripSuffix("\'")
      if (lit.startsWith("\\u")) {
        // Translate hex code
        try {
          val c = Integer.parseInt(lit.stripPrefix("\\u"), 16).toChar
          Validation.success(Expr.Cst(Ast.Constant.Char(c), loc))
        } catch {
          case _: NumberFormatException =>
            val err = MalformedUnicodeEscapeSequence(token.text, loc)
            Validation.toSoftFailure(Expr.Error(err), err)
        }
      } else {
        Validation.success(Expr.Cst(Ast.Constant.Char(lit.head), loc))
      }

    }

    def toStringCst(token: Token)(implicit s: State): Validation[Expr, CompilationMessage] = {
      val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
      val lit = token.text.stripPrefix("\"").stripSuffix("\"")
      try {
        val unescapedLit = StringContext.processEscapes(lit)
        Validation.success(Expr.Cst(Ast.Constant.Str(unescapedLit), loc))
      } catch {
        case e: StringContext.InvalidEscapeException => {
          // Scala will throw when trying to process an unknown escape such as '\1'.
          // In that case we can just pass the literal string as is.
          // TODO: Should this be a SoftFailure, pointing to the unecessary escape?
          Validation.success(Expr.Cst(Ast.Constant.Str(lit), loc))
        }
      }

    }
  }

  private object Predicates {
    def pickHead(tree: Tree)(implicit s: State): Validation[Predicate.Head.Atom, CompilationMessage] = {
      flatMapN(
        pick(TreeKind.Predicate.Head, tree.children)
      )(tree => {
        flatMapN(
          pickNameIdent(tree),
          pick(TreeKind.Predicate.TermList, tree.children)
        )(
          (ident, tree) => {
            val exprs = traverse(pickAll(TreeKind.Expr.Expr, tree.children))(Exprs.visitExpression)
            val maybeLatTerm = tryPickLatticeTermExpr(tree)
            mapN(exprs, maybeLatTerm) {
              case (exprs, None) => Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, exprs, tree.loc)
              case (exprs, Some(term)) => Predicate.Head.Atom(Name.mkPred(ident), Denotation.Latticenal, exprs ::: term :: Nil, tree.loc)
            }
          })
      })
    }


    def visitBody(parentTree: Tree)(implicit s: State): Validation[Predicate.Body, CompilationMessage] = {
      assert(parentTree.kind == TreeKind.Predicate.Body)
      val tree = unfold(parentTree)
      tree.kind match {
        case TreeKind.Predicate.Atom => visitAtom(tree)
        case TreeKind.Predicate.Guard => visitGuard(tree)
        case TreeKind.Predicate.Functional => visitFunctional(tree)
        case kind => throw InternalCompilerException(s"expected predicate body but found '$kind'", tree.loc)
      }
    }

    def visitAtom(tree: Tree)(implicit s: State): Validation[Predicate.Body.Atom, CompilationMessage] = {
      assert(tree.kind == TreeKind.Predicate.Atom)
      val fixity = if (hasToken(TokenKind.KeywordFix, tree)) Fixity.Fixed else Fixity.Loose
      val polarity = if (hasToken(TokenKind.KeywordNot, tree)) Polarity.Negative else Polarity.Positive

      flatMapN(
        pickNameIdent(tree),
        pick(TreeKind.Predicate.PatternList, tree.children)
      )(
        (ident, tree) => {
          val exprs = traverse(pickAll(TreeKind.Pattern.Pattern, tree.children))(tree => Patterns.visitPattern(tree))
          val maybeLatTerm = tryPickLatticeTermPattern(tree)
          flatMapN(exprs, maybeLatTerm) {
            case (pats, None) =>
              // Check for `[[IllegalFixedAtom]]`.
              val errors = (polarity, fixity) match {
                case (Ast.Polarity.Negative, Ast.Fixity.Fixed) => Some(IllegalFixedAtom(tree.loc))
                case _ => None
              }
              Validation.success(Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, pats, tree.loc)
                )
                .withSoftFailures(errors)
            case (pats, Some(term)) => Validation.success(Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, fixity, pats ::: term :: Nil, tree.loc))
          }
        })
    }

    private def visitGuard(tree: Tree)(implicit s: State): Validation[Predicate.Body.Guard, CompilationMessage] = {
      assert(tree.kind == TreeKind.Predicate.Guard)
      mapN(Exprs.pickExpression(tree))(Predicate.Body.Guard(_, tree.loc))
    }

    private def visitFunctional(tree: Tree)(implicit s: State): Validation[Predicate.Body.Functional, CompilationMessage] = {
      assert(tree.kind == TreeKind.Predicate.Functional)
      val idents = pickAll(TreeKind.Ident, tree.children)
      mapN(
        traverse(idents)(tokenToIdent),
        Exprs.pickExpression(tree)
      ) {
        (idents, expr) => Predicate.Body.Functional(idents, expr, tree.loc)
      }
    }

    private def tryPickLatticeTermExpr(tree: Tree)(implicit s: State): Validation[Option[Expr], CompilationMessage] = {
      traverseOpt(tryPick(TreeKind.Predicate.LatticeTerm, tree.children))(Exprs.pickExpression)
    }

    private def tryPickLatticeTermPattern(tree: Tree)(implicit s: State): Validation[Option[Pattern], CompilationMessage] = {
      traverseOpt(tryPick(TreeKind.Predicate.LatticeTerm, tree.children))(t => Patterns.pickPattern(t))
    }

  }

  private object Types {
    def pickType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      flatMapN(pick(TreeKind.Type.Type, tree.children))(visitType)
    }

    def tryPickTypeNoWild(tree: Tree)(implicit s: State): Validation[Option[Type], CompilationMessage] = {
      mapN(pickType(tree)) {
        case Type.Var(ident, _) if ident.isWild => None
        case t => Some(t)
      }
    }

    def tryPickType(tree: Tree)(implicit s: State): Validation[Option[Type], CompilationMessage] = {
      val maybeType = tryPick(TreeKind.Type.Type, tree.children)
      traverseOpt(maybeType)(visitType)
    }

    def visitType(tree: Tree)(implicit s: State): Validation[WeededAst.Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Type)
      // Visit first child and match its kind to know what to to
      val inner = unfold(tree)
      inner.kind match {
        case TreeKind.Type.Variable => visitVariable(inner)
        case TreeKind.Type.Binary => visitBinary(inner)
        case TreeKind.Type.Unary => visitUnary(inner)
        case TreeKind.Type.Apply => visitApply(inner)
        case TreeKind.Type.Tuple => visitTuple(inner)
        case TreeKind.Type.Native => visitNative(inner)
        case TreeKind.Type.CaseSet => visitCaseSet(inner)
        case TreeKind.Type.Record => visitRecord(inner)
        case TreeKind.Type.RecordRow => visitRecordRow(inner)
        case TreeKind.Type.Schema => visitSchema(inner)
        case TreeKind.Type.SchemaRow => visitSchemaRow(inner)
        case TreeKind.Type.Constant => visitConstant(inner)
        case TreeKind.QName => visitName(inner)
        case TreeKind.Ident => mapN(tokenToIdent(inner)) {
          case ident if ident.isWild => Type.Var(ident, tree.loc)
          case ident => Type.Ambiguous(Name.mkQName(ident), inner.loc)
        }
        case kind => failWith(s"TODO: implement type of kind '$kind'", tree.loc)
      }
    }

    private def visitName(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      mapN(visitQName(tree))(Type.Ambiguous(_, tree.loc))
    }

    private def visitBinary(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Binary)
      val types = traverse(pickAll(TreeKind.Type.Type, tree.children))(visitType)
      val op = pick(TreeKind.Operator, tree.children)
      flatMapN(op, types) {
        case (op, t1 :: t2 :: Nil) =>
          text(op).head match {
            // ARROW FUNCTIONS
            case "->" => mapN(tryPickEffect(tree))(eff => {
              val l = tree.loc.asSynthetic
              val lastParam = t1 match {
                case Type.Tuple(inners, _) => inners.last
                case t => t
              }
              val initParams = t1 match {
                case Type.Tuple(inners, _) => inners.init
                case _ => List()
              }
              val base = Type.Arrow(List(lastParam), eff, t2, l)
              initParams.foldRight(base)((acc, tpe) => Type.Arrow(List(acc), None, tpe, l))
            })
            // REGULAR TYPE OPERATORS
            case "+" => Validation.success(Type.Union(t1, t2, tree.loc))
            case "-" => Validation.success(Type.Intersection(t1, Type.Complement(t2, tree.loc.asSynthetic), tree.loc))
            case "++" => Validation.success(Type.CaseUnion(t1, t2, tree.loc))
            case "&&" => Validation.success(Type.CaseIntersection(t1, t2, tree.loc))
            case "--" => Validation.success(Type.CaseIntersection(t1, Type.Complement(t2, tree.loc.asSynthetic), tree.loc))
            case "&" => Validation.success(Type.Intersection(t1, t2, tree.loc))
            case "xor" => Validation.success(Type.Or(
              Type.And(t1, Type.Not(t2, tree.loc), tree.loc),
              Type.And(Type.Not(t1, tree.loc), t2, tree.loc),
              tree.loc
            ))
            case "and" => Validation.success(Type.And(t1, t2, tree.loc))
            case "or" => Validation.success(Type.Or(t1, t2, tree.loc))
            // UNRECOGNIZED
            // TODO: Use Type.Error here
            case op => failWith(s"Unrecognized type operator '$op' ", tree.loc)
          }

        case (op, operands) => throw InternalCompilerException(s"Type.Binary tree with ${operands.length} operands\n ${text(op).head}\n\t${operands.mkString("\n\t")}\n${tree.toDebugString()}", tree.loc)
      }
    }

    private def visitUnary(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Unary)
      val types = traverse(pickAll(TreeKind.Type.Type, tree.children))(visitType)
      val op = pick(TreeKind.Operator, tree.children)
      flatMapN(op, types) {
        case (op, t :: Nil) =>
          text(op).head match {
            case "~" => Validation.success(Type.Complement(t, tree.loc))
            case "~~" => Validation.success(Type.CaseComplement(t, tree.loc))
            case "not" => Validation.success(Type.Not(t, tree.loc))
            // UNRECOGNIZED
            // TODO: Use Type.Error here
            case op => failWith(s"Unrecognized type operator '$op' ", tree.loc)
          }

        case (_, operands) => throw InternalCompilerException(s"Type.Unary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitSchema(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Schema)
      val row = visitSchemaRow(tree)
      mapN(row)(Type.Schema(_, tree.loc))
    }

    private def visitCaseSet(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.CaseSet)
      val cases = traverse(pickAll(TreeKind.QName, tree.children))(visitQName)
      mapN(cases)(Type.CaseSet(_, tree.loc))
    }

    private def visitSchemaRow(parentTree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      val maybeRest = tryPick(TreeKind.Ident, parentTree.children)
      flatMapN(
        traverseOpt(maybeRest)(tokenToIdent)
      ) { maybeRest =>
        val rest = maybeRest match {
          case None => WeededAst.Type.SchemaRowEmpty(parentTree.loc)
          case Some(name) => WeededAst.Type.Var(name, name.loc)
        }

        Validation.foldRight(pickAllTrees(parentTree))(Validation.success(rest)) {
          case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithAlias =>
            mapN(
              pickQName(parentTree),
              Types.pickArguments(tree)
            ) {
              (qname, targs) => Type.SchemaRowExtendByAlias(qname, targs, acc, tree.loc)
            }

          case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithTypes =>
            val types = pickAll(TreeKind.Type.Type, tree.children)
            val maybeLatTerm = tryPick(TreeKind.Predicate.LatticeTerm, tree.children)
            mapN(
              pickQName(tree),
              traverse(types)(Types.visitType),
              traverseOpt(maybeLatTerm)(Types.pickType)
            ) {
              case (qname, tps, None) => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Relational, tps, acc, tree.loc)
              case (qname, tps, Some(t)) => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Latticenal, tps :+ t, acc, tree.loc)
            }

          case (_, acc) => Validation.success(acc)
        }
      }
    }

    private def visitConstant(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Constant)

      text(tree).head match {
        case "false" => Validation.success(Type.False(tree.loc))
        case "true" => Validation.success(Type.True(tree.loc))
        case "Pure" => Validation.success(Type.Pure(tree.loc))
        // TODO EFF-MIGRATION create dedicated Impure type
        case "Impure" => Validation.success(Type.Complement(Type.Pure(tree.loc), tree.loc))
        case other => throw InternalCompilerException(s"'$other' used as Type.Constant", tree.loc)
      }
    }

    private def visitTuple(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Tuple)
      mapN(
        traverse(pickAll(TreeKind.Type.Type, tree.children))(visitType)
      ) {
        case tpe :: Nil => tpe // flatten singleton tuple types
        case types => Type.Tuple(types, tree.loc)
      }
    }

    private def visitRecord(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Record)
      val row = visitRecordRow(tree)
      mapN(row)(Type.Record(_, tree.loc))
    }

    private def visitRecordRow(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      val maybeVar = tryPick(TreeKind.Type.Variable, tree.children)
      val fields = pickAll(TreeKind.Type.RecordField, tree.children)
      mapN(traverseOpt(maybeVar)(visitVariable), traverse(fields)(visitRecordField)) {
        (maybeVar, fields) =>
          val variable = maybeVar.getOrElse(Type.RecordRowEmpty(tree.loc))
          fields.foldRight(variable) { case ((label, tpe), acc) => Type.RecordRowExtend(label, tpe, acc, tree.loc) }
      }
    }

    private def visitRecordField(tree: Tree)(implicit s: State): Validation[(Name.Label, Type), CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.RecordField)
      mapN(pickNameIdent(tree), pickType(tree))((ident, tpe) => (Name.mkLabel(ident), tpe))
    }

    private def visitNative(tree: Tree)(implicit s: State): Validation[Type.Native, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Native)
      text(tree) match {
        case head :: rest =>
          val fqn = (List(head.stripPrefix("##")) ++ rest).mkString("")
          Validation.success(Type.Native(fqn, tree.loc))
        case Nil => throw InternalCompilerException("Parser passed empty Type.Native", tree.loc)
      }
    }

    private def visitVariable(tree: Tree)(implicit s: State): Validation[Type.Var, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Variable)
      mapN(tokenToIdent(tree)) { ident => Type.Var(ident, tree.loc) }
    }

    private def visitApply(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
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

    def pickEffect(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      flatMapN(pick(TreeKind.Type.EffectSet, tree.children))(visitEffect)
    }

    def tryPickEffect(tree: Tree)(implicit s: State): Validation[Option[Type], CompilationMessage] = {
      val maybeEffectSet = tryPick(TreeKind.Type.EffectSet, tree.children)
      traverseOpt(maybeEffectSet)(visitEffect)
    }

    def visitEffect(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.EffectSet)
      val effects = traverse(pickAll(TreeKind.Type.Type, tree.children))(visitType)
      mapN(effects) {
        // Default to Pure
        case Nil => Type.Pure(tree.loc)
        // Otherwise reduce effects into a union type
        case effects => effects.reduceLeft({
          case (acc, tpe) => Type.Union(acc, tpe, tree.loc)
        }: (Type, Type) => Type)
      }
    }

    def pickArguments(tree: Tree)(implicit s: State): Validation[List[Type], CompilationMessage] = {
      tryPick(TreeKind.Type.Arguments, tree.children)
        .map(argTree => traverse(pickAll(TreeKind.Argument, argTree.children))(pickType))
        .getOrElse(Validation.success(List.empty))
    }

    def pickDerivations(tree: Tree)(implicit s: State): Validation[Derivations, CompilationMessage] = {
      val maybeDerivations = tryPick(TreeKind.Type.Derivations, tree.children)
      val loc = maybeDerivations.map(_.loc).getOrElse(SourceLocation.Unknown)
      val derivations = maybeDerivations
        .map(tree => traverse(pickAll(TreeKind.QName, tree.children))(visitQName))
        .getOrElse(Validation.success(List.empty))

      mapN(derivations)(Derivations(_, loc))
    }

    def pickParameters(tree: Tree)(implicit s: State): Validation[TypeParams, CompilationMessage] = {
      tryPick(TreeKind.TypeParameters, tree.children) match {
        case None => Validation.success(TypeParams.Elided)
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree.children)
          flatMapN(traverse(parameters)(visitParameter)) {
            tparams =>
              val kinded = tparams.collect { case t: TypeParam.Kinded => t }
              val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
              (kinded, unkinded) match {
                // Only unkinded type parameters
                case (Nil, _ :: _) => Validation.success(TypeParams.Unkinded(unkinded))
                // Only kinded type parameters
                case (_ :: _, Nil) => Validation.success(TypeParams.Kinded(kinded))
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

    def pickKindedParameters(tree: Tree)(implicit s: State): Validation[KindedTypeParams, CompilationMessage] = {
      tryPick(TreeKind.TypeParameters, tree.children) match {
        case None => Validation.success(TypeParams.Elided)
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree.children)
          flatMapN(traverse(parameters)(visitParameter)) {
            tparams =>
              val kinded = tparams.collect { case t: TypeParam.Kinded => t }
              val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
              (kinded, unkinded) match {
                // Only kinded type parameters
                case (_ :: _, Nil) => Validation.success(TypeParams.Kinded(kinded))
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

    def pickSingleParameter(tree: Tree)(implicit s: State): Validation[TypeParam, CompilationMessage] = {
      val tparams = pick(TreeKind.TypeParameters, tree.children)
      flatMapN(tparams) {
        tparams => flatMapN(pick(TreeKind.Parameter, tparams.children))(visitParameter)
      }
    }

    private def visitParameter(tree: Tree)(implicit s: State): Validation[TypeParam, CompilationMessage] = {
      assert(tree.kind == TreeKind.Parameter)
      mapN(pickNameIdent(tree)) {
        ident =>
          tryPickKind(tree)
            .map(kind => TypeParam.Kinded(ident, kind))
            .getOrElse(TypeParam.Unkinded(ident))
      }
    }

    def pickConstraints(tree: Tree)(implicit s: State): Validation[List[TypeConstraint], CompilationMessage] = {
      val maybeWithClause = tryPick(TreeKind.Type.Constraints, tree.children)
      maybeWithClause
        .map(withClauseTree => {
          val constraints = pickAll(TreeKind.Type.Constraint, withClauseTree.children)
          traverse(constraints)(visitConstraint)
        }).getOrElse(Validation.success(List.empty))
    }

    private def visitConstraint(tree: Tree)(implicit s: State): Validation[TypeConstraint, CompilationMessage] = {
      assert(tree.kind == TreeKind.Type.Constraint)
      mapN(
        pickQName(tree),
        Types.pickType(tree)
      ) {
        (qname, tpe) => TypeConstraint(qname, tpe, tree.loc)
      }
    }

    def tryPickKind(tree: Tree)(implicit s: State): Option[Kind] = {
      tryPick(TreeKind.Kind, tree.children).flatMap(
        kindTree => {
          val ident = pickNameIdent(kindTree).toHardResult.toOption
          ident.map(ident => {
            val k = Kind.Ambiguous(Name.mkQName(ident), ident.loc)
            tryPickKind(kindTree)
              .map(Kind.Arrow(k, _, kindTree.loc))
              .getOrElse(k)
          })
        })
    }
  }

  private object JvmOp {
    def pickQNameIdents(tree: Tree)(implicit s: State): Validation[List[String], CompilationMessage] = {
      flatMapN(pick(TreeKind.QName, tree.children)) {
        qname => mapN(traverse(pickAll(TreeKind.Ident, qname.children))(t => Validation.success(text(t))))(_.flatten)
      }
    }

    private def pickJavaClassMember(tree: Tree)(implicit s: State): Validation[JavaClassMember, CompilationMessage] = {
      val idents = pickQNameIdents(tree)
      mapN(idents) {
        case prefix :: suffix => JavaClassMember(prefix, suffix, tree.loc)
        case Nil => throw InternalCompilerException("JvmOp empty name", tree.loc)
      }
    }

    def pickJavaName(tree: Tree)(implicit s: State): Validation[Name.JavaName, CompilationMessage] = {
      val idents = pickQNameIdents(tree)
      mapN(idents) {
        idents => Name.JavaName(tree.loc.sp1, idents, tree.loc.sp2)
      }
    }

    private def pickSignature(tree: Tree)(implicit s: State): Validation[List[Type], CompilationMessage] = {
      flatMapN(pick(TreeKind.JvmOp.Signature, tree.children)) {
        sig => traverse(pickAll(TreeKind.Type.Type, sig.children))(Types.visitType)
      }
    }

    private def pickAscription(tree: Tree)(implicit s: State): Validation[(Type, Option[Type]), CompilationMessage] = {
      val ascription = pick(TreeKind.JvmOp.Ascription, tree.children)
      flatMapN(ascription)(
        ascTree => mapN(Types.pickType(ascTree), Types.tryPickEffect(ascTree))((tpe, eff) => (tpe, eff))
      )
    }

    private def visitMethod(tree: Tree, isStatic: Boolean = false)(implicit s: State): Validation[JvmOp, CompilationMessage] = {
      val fqn = pickJavaClassMember(tree)
      val signature = pickSignature(tree)
      val ascription = pickAscription(tree)
      val ident = tryPickNameIdent(tree)
      mapN(fqn, signature, ascription, ident) {
        case (fqn, signature, (tpe, eff), ident) => if (isStatic)
          WeededAst.JvmOp.StaticMethod(fqn, signature, tpe, eff, ident)
        else
          WeededAst.JvmOp.Method(fqn, signature, tpe, eff, ident)
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

  private def tryPickNameIdent(tree: Tree)(implicit s: State): Validation[Option[Name.Ident], CompilationMessage] = {
    traverseOpt(tryPick(TreeKind.Ident, tree.children))(tokenToIdent)
  }

  ////////// HELPERS //////////////////
  private def tokenToIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    tree.children.headOption match {
      case Some(Child.Token(token)) => Validation.success(Name.Ident(
        token.mkSourcePosition(s.src, Some(s.parserInput)),
        token.text,
        token.mkSourcePositionEnd(s.src, Some(s.parserInput))
      ))
      case _ => failWith(s"expected first child of '${tree.kind}' to be Child.Token")
    }
  }

  private def tokenToJavaName(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    tree.children.headOption match {
      case Some(Child.Token(token)) => Validation.success(Name.Ident(
        token.mkSourcePosition(s.src, Some(s.parserInput)),
        token.text,
        token.mkSourcePositionEnd(s.src, Some(s.parserInput))
      ))
      case _ => failWith(s"expected first child of '${tree.kind}' to be Child.Token")
    }
  }

  private def javaQnameToFqn(qname: Name.QName): String = {
    (qname.namespace.idents.map(_.name.stripPrefix("##")) :+ qname.ident.name).mkString(".")
  }

  /**
   * When kinds are elided they default to the kind `Type`.
   */
  private def defaultKind(ident: Name.Ident): Kind = Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)

  /**
   * plucks the first inner tree in children.
   * NB: This is intended to used to unfold the inner tree on special marker [[TreeKind]],
   * such as [[TreeKind.Type.Type]] and [[TreeKind.Expr.Expr]].
   * The parser should guarantee that these tree kinds have at least a single child.
   */
  private def unfold(tree: Tree): Tree = {
    assert(tree.kind match {
      case TreeKind.Type.Type | TreeKind.Expr.Expr | TreeKind.JvmOp.JvmOp | TreeKind.Predicate.Body => true
      case _ => false
    })

    // Find the first sub-tree that isn't a comment
    tree.children.find {
      case Child.Tree(tree) if tree.kind != TreeKind.Comments => true
      case _ => false
    }.map {
      case Child.Tree(tree) => tree
      case _ => throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    }.getOrElse(
      throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    )
  }

  // A helper that tries to find a token child of a specific kind
  private def hasToken(kind: TokenKind, tree: Tree): Boolean = {
    tree.children.exists {
      case Child.Token(t) => t.kind == kind
      case _ => false
    }
  }

  // A helper that picks all subtrees
  private def pickAllTrees(tree: Tree): List[Tree] = {
    tree.children.collect {
      case Child.Tree(t) => t
    }.toList
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
      .map(Validation.success(_))
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
    Validation.HardFailure(Chain(Parse2Error.DevErr(loc, message)))
  }

  private def softFailWith[T](result: T): Validation[T, CompilationMessage] = {
    Validation.SoftFailure(result, Chain())
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
}
