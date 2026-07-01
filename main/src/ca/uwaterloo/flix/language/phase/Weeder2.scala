/*
 * Copyright 2024 Herluf Baggesen
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
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{ChangeSet, Name, ReadAst, SemanticOp, SourceLocation, SourcePosition, Symbol, SyntaxTree, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.ParseError.*
import ca.uwaterloo.flix.language.errors.WeederError.*
import ca.uwaterloo.flix.language.errors.{ParseError, WeederError}
import ca.uwaterloo.flix.util.Validation.*
import ca.uwaterloo.flix.util.collection.{ArrayOps, Chain, Nel, SeqOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

import java.lang.{Byte as JByte, Integer as JInt, Long as JLong, Short as JShort}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.regex.{PatternSyntaxException, Pattern as JPattern}
import scala.annotation.tailrec
import scala.collection.immutable.{::, List, Nil}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Weeder2 walks a [[Tree]], validates it and thereby transforms it into a [[WeededAst]].
  *
  * Function names in Weeder2 follow this pattern:
  *   1. visit* : Visits a Tree of a known kind. These functions assert that the kind is indeed known.
  *   1. pick* : Picks first sub-Tree of a kind and visits it.
  *   1. tryPick* : Works like pick* but only runs the visitor if the child of kind is found. Returns an option containing the result.
  *   1. pickAll* : These will pick all subtrees of a specified kind and run a visitor on it.
  */
object Weeder2 {

  import WeededAst.*

  def run(readRoot: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], root: SyntaxTree.Root, oldRoot: WeededAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (Validation[WeededAst.Root, CompilationMessage], List[CompilationMessage]) = {
    flix.phaseNew("Weeder2") {
      implicit val sctx: SharedContext = SharedContext.mk()
      val (stale, fresh) = changeSet.partition(root.units, oldRoot.units)

      // Schedule the biggest sources first to increase throughput.
      def sortBy(p: (Source, SyntaxTree.Tree)): Int = -p._1.data.length

      // Parse each source file in parallel and join them into a WeededAst.Root
      val refreshed = ParOps.parMapWithPriority(stale, sortBy) {
        case (src, tree) => mapN(weed(tree))(tree => src -> tree)
      }

      val compilationUnits = mapN(sequence(refreshed))(_.toMap ++ fresh)
      (mapN(compilationUnits)(WeededAst.Root(_, entryPoint, readRoot.availableClasses, root.tokens)), sctx.errors.asScala.toList)
    }(DebugValidation())
  }

  private def weed(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[CompilationUnit, CompilationMessage] = {
    try {
      val usesAndImports = pickAllUsesAndImports(tree)
      val declarations = Decls.pickAllDeclarations(tree)
      Validation.Success(CompilationUnit(usesAndImports, declarations, tree.loc))
    } catch {
      case PickException(error) => Validation.Failure(Chain(error))
    }
  }

  private def pickAllUsesAndImports(tree: Tree)(implicit sctx: SharedContext): List[UseOrImport] = {
    expectAny(tree, List(TreeKind.Root, TreeKind.Decl.Module))
    val maybeTree = tryPick(TreeKind.UsesOrImports.UseOrImportList, tree)
    maybeTree.map { tree =>
      val uses = pickAll(TreeKind.UsesOrImports.Use, tree)
      val imports = pickAll(TreeKind.UsesOrImports.Import, tree)
      uses.flatMap(visitUse) ++ imports.flatMap(visitImport)
    }.getOrElse(List.empty)
  }

  private def visitUse(tree: Tree)(implicit sctx: SharedContext): List[UseOrImport] = {
    expect(tree, TreeKind.UsesOrImports.Use)
    val maybeUseMany = tryPick(TreeKind.UsesOrImports.UseMany, tree)
    val qname = pickQName(tree)
    val isTopLevelName = qname.namespace.idents.isEmpty
    val isNotImportedByUse = maybeUseMany.isEmpty
    val isUnqualifiedUse = isTopLevelName && isNotImportedByUse
    if (isUnqualifiedUse) {
      val error = UnqualifiedUse(qname, qname.loc)
      sctx.errors.add(error)
      List.empty
    } else {
      val nname = Name.NName(qname.namespace.idents :+ qname.ident, qname.loc)
      maybeUseMany match {
        // case: Use many.
        case Some(useMany) =>
          val uses = visitUseMany(useMany, nname)
          // Issue an error if it's empty.
          if (uses.isEmpty) {
            val error = NeedAtleastOne(NamedTokenSet.Name, SyntacticContext.Unknown, None, useMany.loc)
            sctx.errors.add(error)
          }
          uses
        // case: Use one. Use the qname.
        case None =>
          List(UseOrImport.Use(qname, qname.ident, qname.loc))
      }
    }
  }

  private def visitUseMany(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): List[UseOrImport] = {
    expect(tree, TreeKind.UsesOrImports.UseMany)
    pickAllMulti(tree, TreeKind.Ident, TreeKind.UsesOrImports.Alias).map { t =>
      t.kind match {
        case TreeKind.Ident => visitUseIdent(t, namespace)
        case TreeKind.UsesOrImports.Alias => visitUseAlias(t, namespace)
        case k => throw InternalCompilerException(s"unexpected tree kind '$k'", t.loc)
      }
    }
  }

  private def visitUseIdent(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): UseOrImport.Use = {
    val ident = tokenToIdent(tree)
    UseOrImport.Use(Name.QName(namespace, ident, tree.loc), ident, ident.loc)
  }

  private def visitUseAlias(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): UseOrImport.Use = {
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    idents match {
      case ident :: alias :: _ =>
        // Check for illegal alias
        val isIllegalAlias = (ident.name.nonEmpty && alias.name.nonEmpty) && ident.name.charAt(0).isUpper != alias.name.charAt(0).isUpper
        if (isIllegalAlias) {
          val error = IllegalUse(ident.name, alias.name, tree.loc)
          sctx.errors.add(error)
        }
        val qname = Name.QName(namespace, ident, tree.loc)
        UseOrImport.Use(qname, alias, tree.loc)

      // recover from missing alias by using ident
      case ident :: _ =>
        val error = Malformed(NamedTokenSet.Alias, SyntacticContext.Unknown, hint = Some(s"Give an alias after ${TokenKind.ArrowThickR.display}."), loc = tree.loc)
        sctx.errors.add(error)
        val qname = Name.QName(namespace, ident, tree.loc)
        UseOrImport.Use(qname, ident, ident.loc)

      case _ => throw InternalCompilerException("Parser passed malformed use with alias", tree.loc)
    }
  }

  private def visitImport(tree: Tree)(implicit sctx: SharedContext): List[UseOrImport] = {
    expect(tree, TreeKind.UsesOrImports.Import)
    val jname = pickJavaName(tree)
    val maybeImportMany = tryPick(TreeKind.UsesOrImports.ImportMany, tree)
    maybeImportMany match {
      // case: Import many.
      case Some(importMany) =>
        val imports = visitImportMany(importMany, jname.fqn)
        // Issue an error if it's empty.
        if (imports.isEmpty) {
          val error = NeedAtleastOne(NamedTokenSet.Name, SyntacticContext.Unknown, None, importMany.loc)
          sctx.errors.add(error)
        }
        imports
      // case: Import one. Use the Java name.
      case None =>
        val ident = Name.Ident(jname.fqn.lastOption.getOrElse(""), jname.loc)
        List(UseOrImport.Import(jname, ident, tree.loc))
    }
  }

  private def visitImportMany(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): List[UseOrImport.Import] = {
    expect(tree, TreeKind.UsesOrImports.ImportMany)
    pickAllMulti(tree, TreeKind.Ident, TreeKind.UsesOrImports.Alias).map { t =>
      t.kind match {
        case TreeKind.Ident => visitImportIdent(t, namespace)
        case TreeKind.UsesOrImports.Alias => visitImportAlias(t, namespace)
        case k => throw InternalCompilerException(s"unexpected tree kind '$k'", t.loc)
      }
    }
  }

  private def visitImportIdent(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): UseOrImport.Import = {
    val ident = tokenToIdent(tree)
    UseOrImport.Import(Name.JavaName(namespace ++ Seq(ident.name), tree.loc), ident, ident.loc)
  }

  private def visitImportAlias(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): UseOrImport.Import = {
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    idents match {
      case ident :: alias :: _ =>
        val jname = Name.JavaName(namespace :+ ident.name, tree.loc)
        UseOrImport.Import(jname, alias, tree.loc)
      // recover from missing alias by using ident
      case ident :: _ =>
        val error = Malformed(NamedTokenSet.Alias, SyntacticContext.Unknown, hint = Some(s"Give an alias after ${TokenKind.ArrowThickR.display}."), loc = tree.loc)
        sctx.errors.add(error)
        UseOrImport.Import(Name.JavaName(Seq(ident.name), tree.loc), ident, ident.loc)
      case _ => throw InternalCompilerException("Parser passed malformed use with alias", tree.loc)
    }
  }

  private object Decls {
    def pickAllDeclarations(tree: Tree)(implicit sctx: SharedContext, flix: Flix): List[Declaration] = {
      expectAny(tree, List(TreeKind.Root, TreeKind.Decl.Module))
      pickAllMulti(tree,
        TreeKind.Decl.Module,
        TreeKind.Decl.Trait,
        TreeKind.Decl.Instance,
        TreeKind.Decl.Def,
        TreeKind.Decl.Enum,
        TreeKind.Decl.RestrictableEnum,
        TreeKind.Decl.Struct,
        TreeKind.Decl.TypeAlias,
        TreeKind.Decl.Effect
      ).map { t =>
        t.kind match {
          case TreeKind.Decl.Module => visitModuleDecl(t)
          case TreeKind.Decl.Trait => visitTraitDecl(t)
          case TreeKind.Decl.Instance => visitInstanceDecl(t)
          case TreeKind.Decl.Def => visitDefinitionDecl(t)
          case TreeKind.Decl.Enum => visitEnumDecl(t)
          case TreeKind.Decl.RestrictableEnum => visitRestrictableEnumDecl(t)
          case TreeKind.Decl.Struct => visitStructDecl(t)
          case TreeKind.Decl.TypeAlias => visitTypeAliasDecl(t)
          case TreeKind.Decl.Effect => visitEffectDecl(t)
          case k => throw InternalCompilerException(s"unexpected declaration kind '$k'", t.loc)
        }
      }
    }

    private def visitModuleDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Declaration.Mod = {
      expect(tree, TreeKind.Decl.Module)
      val doc = pickDocumentation(tree)
      val annotations = pickAnnotations(tree)
      for (ann <- annotations.annotations) {
        sctx.errors.add(WeederError.IllegalAnnotation(ann.toString, ann.loc))
      }
      val modifiers = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val qname = pickQName(tree)
      val usesAndImports = pickAllUsesAndImports(tree)
      val declarations = pickAllDeclarations(tree)
      Declaration.Mod(doc, annotations, modifiers, qname, usesAndImports, declarations, tree.loc)
    }

    private def visitTraitDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Trait = {
      expect(tree, TreeKind.Decl.Trait)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub, TokenKind.KeywordSealed))
      val ident = pickNameIdent(tree)
      val tparamsList = pick(TreeKind.TypeParameterList, tree)
      if (pickAll(TreeKind.Parameter, tparamsList).length != 1) {
        sctx.errors.add(IllegalNumberOfTraitParameters(tparamsList.loc))
      }
      val tparam = Types.pickSingleParameter(tree)
      val tconstr = Types.pickConstraints(tree)
      val assocs = pickAll(TreeKind.Decl.AssociatedTypeSig, tree).map(visitAssociatedTypeSigDecl(_, tparam))
      val sigs = pickAll(TreeKind.Decl.Signature, tree)
      val sigs0 = sigs.map(visitSignatureDecl)
      Declaration.Trait(doc, ann, mod, ident, tparam, tconstr, assocs, sigs0, tree.loc)
    }

    private def visitInstanceDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Declaration.Instance = {
      expect(tree, TreeKind.Decl.Instance)
      val allowedDefModifiers: Set[TokenKind] = Set(TokenKind.KeywordPub)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set.empty)
      val clazz = pickQName(tree)
      val tpe = Types.pickType(tree)
      val tconstrs = Types.pickConstraints(tree)
      val econstrs = pickEqualityConstraints(tree)
      val assocs = pickAll(TreeKind.Decl.AssociatedTypeDef, tree).map(visitAssociatedTypeDefDecl(_, tpe))
      val defs = pickAll(TreeKind.Decl.Def, tree).map(visitDefinitionDecl(_, allowedModifiers = allowedDefModifiers, mustBePublic = true))
      val redefs = pickAll(TreeKind.Decl.Redef, tree).map(visitRedefinitionDecl)
      Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, econstrs, assocs, defs, redefs, tree.loc)
    }

    private def visitSignatureDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Sig = {
      expect(tree, TreeKind.Decl.Signature)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub), mustBePublic = true)
      val ident = pickNameIdent(tree)
      val tparams = Types.pickKindedParameters(tree)
      val fparams = pickFormalParameters(tree)
      val maybeExpression = tryPick(TreeKind.Expr.Expr, tree)
      val expr = maybeExpression.map(Exprs.visitExpr)
      val tpe = Types.pickType(tree)
      val eff = Types.tryPickEffect(tree)
      val tconstrs = Types.pickConstraints(tree)
      val econstrs = pickEqualityConstraints(tree)
      Declaration.Sig(doc, ann, mod, ident, tparams, fparams, expr, tpe, eff, tconstrs, econstrs, tree.loc)
    }

    private def visitDefinitionDecl(tree: Tree, allowedModifiers: Set[TokenKind] = Set(TokenKind.KeywordPub), mustBePublic: Boolean = false)(implicit sctx: SharedContext): Declaration.Def = {
      expect(tree, TreeKind.Decl.Def)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = allowedModifiers, mustBePublic)
      val ident = pickNameIdent(tree)
      val tparams = Types.pickKindedParameters(tree)
      val fparams = pickFormalParameters(tree)
      val exp = Exprs.pickExpr(tree)
      val ttype = Types.pickType(tree)
      val eff = Types.tryPickEffect(tree)
      val tconstrs = Types.pickConstraints(tree)
      val constrs = pickEqualityConstraints(tree)
      Declaration.Def(doc, ann, mod, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }

    private def visitRedefinitionDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Declaration.Redef = {
      expect(tree, TreeKind.Decl.Redef)
      val allowedModifiers: Set[TokenKind] = if (flix.options.xnodeprecated) Set.empty else Set(TokenKind.KeywordPub)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = allowedModifiers)
      val ident = pickNameIdent(tree)
      val tparams = Types.pickKindedParameters(tree)
      val fparams = pickFormalParameters(tree)
      val exp = Exprs.pickExpr(tree)
      val ttype = Types.pickType(tree)
      val eff = Types.tryPickEffect(tree)
      val tconstrs = Types.pickConstraints(tree)
      val constrs = pickEqualityConstraints(tree)
      Declaration.Redef(doc, ann, mod, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }

    private def visitEnumDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Enum = {
      expect(tree, TreeKind.Decl.Enum)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val tparams = Types.pickParameters(tree)
      val derivations = Types.pickDerivations(tree)
      val shorthandBody = tryPick(TreeKind.CaseBody, tree)
      val tpe = shorthandBody.map(Types.visitCaseType)
      val cases = pickAll(TreeKind.Case, tree)
      val visitedCases = cases.map(visitEnumCase)
      val finalCases = (tpe, visitedCases) match {
        // Illegal empty singleton enum (`enum A()`)
        case (Some(List(Type.Error(_))), Nil) =>
          // Fall back on no cases. Whoever generated the error must have reported it.
          List.empty
        // Singleton enum
        case (Some(ts), cs) =>
          // Error if both singleton shorthand and cases provided
          // Treat this as an implicit case with the type t, e.g.,
          // enum Foo(Int32) { case Bar, case Baz }
          // ===>
          // enum Foo { case Foo(Int32), case Bar, case Baz }
          val syntheticCase = WeededAst.Case(ident, ts, ident.loc)
          syntheticCase :: cs
        // Empty or Multiton enum
        case (None, cs) =>
          cs
      }
      Declaration.Enum(doc, ann, mod, ident, tparams, derivations, finalCases, tree.loc)
    }

    private def visitEnumCase(tree: Tree)(implicit sctx: SharedContext): Case = {
      expect(tree, TreeKind.Case)
      val maybeType = tryPick(TreeKind.CaseBody, tree)
      val ident = pickNameIdent(tree)
      // TODO: Doc comments on enum cases. It is not available on [[Case]] yet.
      val tpes = maybeType.map(Types.visitCaseType).getOrElse(Nil)
      // Make a source location that spans the name and type, excluding 'case'.
      val loc = SourceLocation(isReal = true, ident.loc.source, ident.loc.start, tree.loc.end)
      Case(ident, tpes, loc)
    }

    private def visitRestrictableEnumDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.RestrictableEnum = {
      expect(tree, TreeKind.Decl.RestrictableEnum)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val restrictionParam = Types.visitParameter(pick(TreeKind.Parameter, tree))
      val tparams = Types.pickParameters(tree)
      val derivations = Types.pickDerivations(tree)
      val shorthandBody = tryPick(TreeKind.CaseBody, tree)
      val tpe = shorthandBody.map(Types.visitCaseType)
      val cases = pickAll(TreeKind.Case, tree)
      val visitedCases = cases.map(visitRestrictableEnumCase)
      val finalCases = (tpe, visitedCases) match {
        // Illegal empty singleton enum (`enum A()`)
        case (Some(List(Type.Error(_))), Nil) =>
          // Fall back on no cases. Whoever generated the error must have reported it.
          List.empty
        // Singleton enum
        case (Some(ts), cs) =>
          // Error if both singleton shorthand and cases provided
          // Treat this as an implicit case with the type t, e.g.,
          // enum Foo(Int32) { case Bar, case Baz }
          // ===>
          // enum Foo { case Foo(Int32), case Bar, case Baz }
          val syntheticCase = WeededAst.RestrictableCase(ident, ts, ident.loc)
          syntheticCase :: cs
        // Empty or Multiton enum
        case (None, cs) =>
          cs
      }
      Declaration.RestrictableEnum(doc, ann, mod, ident, restrictionParam, tparams, derivations, finalCases, tree.loc)
    }

    private def visitRestrictableEnumCase(tree: Tree)(implicit sctx: SharedContext): RestrictableCase = {
      expect(tree, TreeKind.Case)
      val maybeType = tryPick(TreeKind.CaseBody, tree)
      val ident = pickNameIdent(tree)
      // TODO: Doc comments on enum cases. It is not available on [[Case]] yet.
      val tpes = maybeType.map(Types.visitCaseType).getOrElse(Nil)
      RestrictableCase(ident, tpes, tree.loc)
    }

    private def visitStructDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Struct = {
      expect(tree, TreeKind.Decl.Struct)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val tparams = Types.pickParameters(tree)
      val fields = pickAll(TreeKind.StructField, tree).map(visitStructField)
      // Ensure that each name is unique
      val errors = SeqOps.getDuplicates(fields, (f: StructField) => f.name.name).map {
        case (field1, field2) => DuplicateStructField(ident.name, field1.name.name, field1.name.loc, field2.name.loc, ident.loc)
      }
      errors.foreach(sctx.errors.add)
      // For each field, only keep the first occurrence of the name
      val filteredFields = fields.distinctBy(_.name.name)
      Declaration.Struct(doc, ann, Modifiers(Modifier.Mutable :: mod.mod), ident, tparams, filteredFields, tree.loc)
    }

    private def visitStructField(tree: Tree)(implicit sctx: SharedContext): StructField = {
      expect(tree, TreeKind.StructField)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub, TokenKind.KeywordMut))
      val ident = pickNameIdent(tree)
      val ttype = Types.pickType(tree)
      // Make a source location that spans the name and type
      val loc = SourceLocation(isReal = true, ident.loc.source, ident.loc.start, tree.loc.end)
      StructField(mod, Name.mkLabel(ident), ttype, loc)
    }

    private def visitTypeAliasDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.TypeAlias = {
      expect(tree, TreeKind.Decl.TypeAlias)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val tparams = Types.pickParameters(tree)
      val tpe = Types.pickType(tree)
      Declaration.TypeAlias(doc, ann, mod, ident, tparams, tpe, tree.loc)
    }

    private def visitAssociatedTypeSigDecl(tree: Tree, classTypeParam: TypeParam)(implicit sctx: SharedContext): Declaration.AssocTypeSig = {
      expect(tree, TreeKind.Decl.AssociatedTypeSig)
      val doc = pickDocumentation(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val tparams = Types.pickParameters(tree)
      val tparam = tparams match {
        // Elided: Use class type parameter
        case Nil => classTypeParam
        // Single type parameter
        case head :: Nil => head
        // Multiple type parameters. Soft fail by picking the first parameter
        case ts@head :: _ :: _ =>
          val error = NonUnaryAssocType(ts.length, ident.loc)
          sctx.errors.add(error)
          head
      }
      val kind = Types.tryPickKind(tree).getOrElse(defaultKind(ident))
      val tpe = Types.tryPickTypeNoWild(tree)
      Declaration.AssocTypeSig(doc, mod, ident, tparam, kind, tpe, tree.loc)
    }

    private def visitAssociatedTypeDefDecl(tree: Tree, instType: Type)(implicit sctx: SharedContext): Declaration.AssocTypeDef = {
      expect(tree, TreeKind.Decl.AssociatedTypeDef)
      val doc = pickDocumentation(tree)
      val mod = pickModifiers(tree, Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val typeArgs = Types.pickArguments(tree)
      val typeArg = typeArgs match {
        // Use instance type if type arguments were elided
        case Nil => instType
        // Single argument: use that
        case head :: Nil => head
        // Multiple type arguments: recover by arbitrarily picking the first one
        case types =>
          val error = NonUnaryAssocType(types.length, ident.loc)
          sctx.errors.add(error)
          types.head
      }
      val tpe = Types.pickType(tree)
      Declaration.AssocTypeDef(doc, mod, ident, typeArg, tpe, tree.loc)
    }

    private def visitEffectDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Effect = {
      expect(tree, TreeKind.Decl.Effect)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val ident = pickNameIdent(tree)
      val tparams = Types.pickParameters(tree)
      val ops = pickAll(TreeKind.Decl.Op, tree).map(visitOperationDecl)
      Declaration.Effect(doc, ann, mod, ident, tparams, ops, tree.loc)
    }

    private def visitOperationDecl(tree: Tree)(implicit sctx: SharedContext): Declaration.Op = {
      expect(tree, TreeKind.Decl.Op)
      val doc = pickDocumentation(tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set.empty)
      val ident = pickNameIdent(tree)
      val fparams = pickFormalParameters(tree)
      val tpe = Types.pickType(tree)
      val tconstrs = Types.pickConstraints(tree)
      Declaration.Op(doc, ann, mod, ident, fparams, tpe, tconstrs, tree.loc)
    }

    private def pickDocumentation(tree0: Tree): Doc = {
      val docTree: Option[Tree] = tryPick(TreeKind.Doc, tree0).flatMap(tryPick(TreeKind.CommentList, _))
      docTree match {
        case None => Doc(List.empty, tree0.loc)
        case Some(tree) =>
          // Strip the `///` prefix, one optional separator space, and trailing whitespace.
          // Internal indentation is preserved so that code blocks and ASCII layout survive.
          var lines = text(tree).map { s =>
            val s1 = s.stripPrefix("///")
            val s2 = if (s1.startsWith(" ")) s1.drop(1) else s1
            s2.stripTrailing()
          }
          // Drop first/last line if it is empty
          if (lines.headOption.exists(_.isEmpty)) {
            lines = lines.tail
          }
          if (lines.lastOption.exists(_.isEmpty)) {
            lines = lines.dropRight(1)
          }
          Doc(lines, tree.loc)
      }
    }

    def pickAnnotations(tree: Tree)(implicit sctx: SharedContext): Annotations = {
      val optAnn = tryPick(TreeKind.AnnotationList, tree)
      val ann = optAnn.map(
          tree => {
            val tokens = pickAllTokens(tree)
            // Check for duplicate annotations
            val errors = SeqOps.getDuplicates(tokens.toSeq, (t: Token) => t.text).map(pair => {
              val name = pair._1.text
              val loc1 = pair._1.mkSourceLocation()
              val loc2 = pair._2.mkSourceLocation()
              DuplicateAnnotation(name.stripPrefix("@"), loc1, loc2)
            })
            errors.foreach(sctx.errors.add)
            val result = tokens.toList.map(visitAnnotation)
            checkInlineAndDontInline(result)
            result
          })
        .getOrElse(List.empty)

      Annotations(ann)
    }

    private def checkInlineAndDontInline(annotations: List[Annotation])(implicit sctx: SharedContext): Unit = {
      val (optInline, optDontInline) = annotations.foldLeft((None: Option[SourceLocation], None: Option[SourceLocation])) {
        case ((None, right), Annotation.Inline(loc)) => (Some(loc), right)
        case ((left, None), Annotation.DontInline(loc)) => (left, Some(loc))
        case (acc, _) => acc
      }
      (optInline, optDontInline) match {
        case (Some(leftLoc), Some(rightLoc)) =>
          sctx.errors.add(InlineAndDontInline(leftLoc, rightLoc))

        case _ =>
      }
    }

    private def visitAnnotation(token: Token)(implicit sctx: SharedContext): Annotation = {
      val loc = token.mkSourceLocation()
      import Annotation.*
      token.text match {
        case "@CompileTest" => CompileTest(loc)
        case "@DefaultHandler" => DefaultHandler(loc)
        case "@Deprecated" => Deprecated(loc)
        case "@DontInline" => DontInline(loc)
        case "@Experimental" => Experimental(loc)
        case "@Export" => Export(loc)
        case "@Inline" => Inline(loc)
        case "@Parallel" => Parallel(loc)
        case "@ParallelWhenPure" => ParallelWhenPure(loc)
        case "@LoweringTarget" => LoweringTarget(loc)
        case "@Lazy" => Lazy(loc)
        case "@LazyWhenPure" => LazyWhenPure(loc)
        case "@Skip" => Skip(loc)
        case "@Test" => Test(loc)
        case "@Tailrec" => TailRecursive(loc)
        case "@Terminates" => Terminates(loc)
        case other =>
          val name = other.stripPrefix("@")
          val error = UndefinedAnnotation(name, loc)
          sctx.errors.add(error)
          Annotation.Error(name, loc)
      }
    }

    private def pickEqualityConstraints(tree: Tree)(implicit sctx: SharedContext): List[EqualityConstraint] = {
      val maybeConstraintList = tryPick(TreeKind.Decl.EqualityConstraintList, tree)
      val constraints = maybeConstraintList.map(t => {
        val constraintTrees = pickAll(TreeKind.Decl.EqualityConstraintFragment, t)
        constraintTrees.map(visitEqualityConstraint)
      })
      constraints.getOrElse(List.empty).flatten
    }

    private def visitEqualityConstraint(tree: Tree)(implicit sctx: SharedContext): Option[EqualityConstraint] = {
      pickAll(TreeKind.Type.Type, tree).map(Types.visitType) match {
        case Type.Apply(Type.Ambiguous(qname, _), t1, _) :: t2 :: Nil =>
          Some(EqualityConstraint(qname, t1, t2, tree.loc))

        case _ =>
          val error = IllegalEqualityConstraint(tree.loc)
          sctx.errors.add(error)
          None
      }
    }

    private val ALL_MODIFIERS: Set[TokenKind] = Set(
      TokenKind.KeywordSealed,
      TokenKind.KeywordPub,
      TokenKind.KeywordMut)

    private def pickModifiers(tree: Tree, allowed: Set[TokenKind] = ALL_MODIFIERS, mustBePublic: Boolean = false)(implicit sctx: SharedContext): Modifiers = {
      tryPick(TreeKind.ModifierList, tree) match {
        case None => Modifiers(List.empty)
        case Some(modTree) =>
          var errors: List[CompilationMessage] = List.empty
          val tokens = pickAllTokens(modTree)
          // Check if pub is missing
          if (mustBePublic && !tokens.exists(_.kind == TokenKind.KeywordPub)) {
            val ident = pickNameIdent(tree)
            errors :+= IllegalNonPublicSignature(ident, ident.loc)
          }
          // Check for duplicate modifiers
          errors = errors ++ SeqOps.getDuplicates(tokens.toSeq, (t: Token) => t.kind).map(pair => {
            val name = pair._1.text
            val loc1 = pair._1.mkSourceLocation()
            val loc2 = pair._2.mkSourceLocation()
            DuplicateModifier(name, loc2, loc1)
          })
          errors.foreach(sctx.errors.add)

          val mod = tokens.toList.map(visitModifier(_, allowed))
          Modifiers(mod)
      }
    }

    private def visitModifier(token: Token, allowed: Set[TokenKind])(implicit sctx: SharedContext): Modifier = {
      if (!allowed.contains(token.kind)) {
        val error = IllegalModifier(token.mkSourceLocation())
        sctx.errors.add(error)
      }
      token.kind match {
        case TokenKind.KeywordSealed => Modifier.Sealed
        case TokenKind.KeywordPub => Modifier.Public
        case TokenKind.KeywordMut => Modifier.Mutable
        case kind => throw InternalCompilerException(s"Parser passed unknown modifier '$kind'", token.mkSourceLocation())
      }
    }

    def unitFormalParameter(loc: SourceLocation): FormalParam = FormalParam(
      Name.Ident("_unit", SourceLocation.Unknown),
      Some(Type.Unit(loc)),
      loc
    )

    def pickFormalParameters(tree: Tree, presence: Presence = Presence.Required)(implicit sctx: SharedContext): List[FormalParam] = {
      tryPick(TreeKind.ParameterList, tree) match {
        case Some(t) =>
          val params = pickAll(TreeKind.Parameter, t)
          if (params.isEmpty) {
            List(unitFormalParameter(t.loc))
          } else {
            val fparams = params.map(visitParameter(_, presence))
            // Check for duplicates
            val paramsWithoutWildcards = fparams.filter(!_.ident.isWild)
            val errors = SeqOps.getDuplicates(paramsWithoutWildcards, (p: FormalParam) => p.ident.name)
              .map(pair => DuplicateFormalParam(pair._1.ident.name, pair._1.loc, pair._2.loc))
            errors.foreach(sctx.errors.add)

            // Check missing or illegal type ascription
            fparams
          }
        case None =>
          val error = UnexpectedToken(NamedTokenSet.FromKinds(Set(TokenKind.ParenL)), actual = None, SyntacticContext.Decl.Module, loc = tree.loc)
          sctx.errors.add(error)
          List(unitFormalParameter(tree.loc))
      }
    }

    private def visitParameter(tree: Tree, presence: Presence)(implicit sctx: SharedContext): FormalParam = {
      expect(tree, TreeKind.Parameter)
      val ident = pickNameIdent(tree)
      val maybeType = tryPick(TreeKind.Type.Type, tree)
      // Check for missing or illegal type ascription
      (maybeType, presence) match {
        case (None, Presence.Required) =>
          val error = MissingTypeAscription(ident.name, tree.loc)
          sctx.errors.add(error)
          FormalParam(ident, Some(Type.Error(tree.loc.asSynthetic)), tree.loc)
        case (Some(_), Presence.Forbidden) =>
          val error = IllegalFormalParamAscription(tree.loc)
          sctx.errors.add(error)
          FormalParam(ident, None, tree.loc)
        case (Some(typeTree), _) => FormalParam(ident, Some(Types.visitType(typeTree)), tree.loc)
        case (None, _) => FormalParam(ident, None, tree.loc)
      }
    }
  }

  private object Exprs {
    def pickExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      tryPick(TreeKind.Expr.Expr, tree).map(visitExpr) match {
        case Some(expr) => expr
        case None =>
          // Fall back on Expr.Error. Parser has reported an error here.
          val err = UnexpectedToken(expected = NamedTokenSet.Expression, actual = None, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          Expr.Error(err)
      }
    }

    def visitExpr(exprTree: Tree)(implicit sctx: SharedContext): Expr = {
      assert(exprTree.kind == TreeKind.Expr.Expr)
      val tree = unfold(exprTree)
      tree.kind match {
        case TreeKind.Ident =>
          val ident = tokenToIdent(tree)
          Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc)
        case TreeKind.QName => visitQnameExpr(tree)
        case TreeKind.Expr.Paren => visitParenExpr(tree)
        case TreeKind.Expr.Block => visitBlockExpr(tree)
        case TreeKind.Expr.DebugInterpolator => visitDebugInterpolator(tree)
        case TreeKind.Expr.StringInterpolation => visitStringInterpolationExpr(tree)
        case TreeKind.Expr.OpenVariant => visitOpenVariantExpr(tree)
        case TreeKind.Expr.OpenVariantAs => visitOpenVariantAsExpr(tree)
        case TreeKind.Expr.OperatorAsLambda => visitOperatorAsLambda(tree)
        case TreeKind.Expr.Hole => visitHoleExpr(tree)
        case TreeKind.Expr.HoleVariable => visitHoleWithExpExpr(tree)
        case TreeKind.Expr.Use => visitExprUseExpr(tree)
        case TreeKind.Expr.Literal => visitLiteralExpr(tree)
        case TreeKind.Expr.Apply => visitApplyExpr(tree)
        case TreeKind.Expr.Lambda => visitLambdaExpr(tree)
        case TreeKind.Expr.LambdaExtMatch => visitLambdaExtMatchExpr(tree)
        case TreeKind.Expr.LambdaMatch => visitLambdaMatchExpr(tree)
        case TreeKind.Expr.Unary => visitUnaryExpr(tree)
        case TreeKind.Expr.Binary => visitBinaryExpr(tree)
        case TreeKind.Expr.IfThenElse => visitIfThenElseExpr(tree)
        case TreeKind.Expr.Statement => visitStatementExpr(tree)
        case TreeKind.Expr.LocalDef => visitLocalDefExpr(tree)
        case TreeKind.Expr.Region => visitRegionExpr(tree)
        case TreeKind.Expr.Match => visitMatchExpr(tree)
        case TreeKind.Expr.RestrictableChoose
             | TreeKind.Expr.RestrictableChooseStar => visitRestrictableChooseExpr(tree)
        case TreeKind.Expr.ForApplicative => visitForApplicativeExpr(tree)
        case TreeKind.Expr.Foreach => visitForeachExpr(tree)
        case TreeKind.Expr.ForMonadic => visitForMonadicExpr(tree)
        case TreeKind.Expr.GetField => visitGetFieldExpr(tree)
        case TreeKind.Expr.LetMatch => visitLetMatchExpr(tree)
        case TreeKind.Expr.Tuple => visitTupleExpr(tree)
        case TreeKind.Expr.RecordSelect => visitRecordSelectExpr(tree)
        case TreeKind.Expr.RecordOperation => visitRecordOperationOrLiteralExpr(tree)
        case TreeKind.Expr.LiteralArray => visitLiteralArrayExpr(tree)
        case TreeKind.Expr.LiteralVector => visitLiteralVectorExpr(tree)
        case TreeKind.Expr.LiteralList => visitLiteralListExpr(tree)
        case TreeKind.Expr.LiteralMap => visitLiteralMapExpr(tree)
        case TreeKind.Expr.LiteralSet => visitLiteralSetExpr(tree)
        case TreeKind.Expr.Ascribe => visitAscribeExpr(tree)
        case TreeKind.Expr.CheckedTypeCast => visitCheckedTypeCastExpr(tree)
        case TreeKind.Expr.CheckedEffectCast => visitCheckedEffectCastExpr(tree)
        case TreeKind.Expr.UncheckedCast => visitUncheckedCastExpr(tree)
        case TreeKind.Expr.Unsafe => visitUnsafeExpr(tree)

        case TreeKind.Expr.Run => visitRunExpr(tree)
        case TreeKind.Expr.Handler => visitHandlerExpr(tree)
        case TreeKind.Expr.Try => visitTryExpr(tree)
        case TreeKind.Expr.Throw => visitThrow(tree)
        case TreeKind.Expr.Index => visitIndexExpr(tree)
        case TreeKind.Expr.IndexMut => visitIndexMutExpr(tree)
        case TreeKind.Expr.InvokeConstructor => visitInvokeConstructorExpr(tree)
        case TreeKind.Expr.InvokeSuperConstructor => visitInvokeSuperConstructorExpr(tree)
        case TreeKind.Expr.InvokeMethod => visitInvokeMethodExpr(tree)
        case TreeKind.Expr.InvokeSuperMethod => visitInvokeSuperMethodExpr(tree)
        case TreeKind.Expr.AmbiguousNew => visitAmbiguousNewExpr(tree)
        case TreeKind.Expr.StructGet => visitStructGetExpr(tree)
        case TreeKind.Expr.StructPut => visitStructPutExpr(tree)
        case TreeKind.Expr.Static => visitStaticExpr(tree)
        case TreeKind.Expr.Select => visitSelectExpr(tree)
        case TreeKind.Expr.Spawn => visitSpawnExpr(tree)
        case TreeKind.Expr.ParYield => visitParYieldExpr(tree)
        case TreeKind.Expr.FixpointConstraintSet => visitFixpointConstraintSetExpr(tree)
        case TreeKind.Expr.FixpointLambda => visitFixpointLambdaExpr(tree)
        case TreeKind.Expr.FixpointInject => visitFixpointInjectExpr(tree)
        case TreeKind.Expr.FixpointSolveWithProvenance => visitFixpointSolveExpr(tree, isPSolve = true)
        case TreeKind.Expr.FixpointSolveWithProject => visitFixpointSolveExpr(tree, isPSolve = false)
        case TreeKind.Expr.FixpointQuery => visitFixpointQueryExpr(tree)
        case TreeKind.Expr.FixpointQueryWithProvenance => visitFixpointQueryWithProvenanceExpr(tree)
        case TreeKind.Expr.ExtMatch => visitExtMatch(tree)
        case TreeKind.Expr.ExtTag => visitExtTag(tree)
        case TreeKind.Expr.Intrinsic => visitIntrinsic(tree, None, tree.loc)
        case TreeKind.ErrorTree(err) => Expr.Error(err)
        case k =>
          throw InternalCompilerException(s"Expected expression, got '$k'.", tree.loc)
      }
    }

    private def visitQnameExpr(tree: Tree)(implicit sctx: SharedContext): Expr.Ambiguous = {
      val qname = visitQName(tree)
      Expr.Ambiguous(qname, qname.loc)
    }

    private def visitParenExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Paren)
      Expr.Tuple(List(pickExpr(tree)), tree.loc)
    }

    private def visitBlockExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Block)
      pickExpr(tree)
    }

    private def visitDebugInterpolator(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.DebugInterpolator)
      val exp2 = pickExpr(tree)
      val file = tree.loc.source.name
      val line = tree.loc.start.lineOneIndexed
      val cst = Constant.Str(s"[$file:$line] ")
      val exp1 = WeededAst.Expr.Cst(cst, tree.loc)
      WeededAst.Expr.Binary(SemanticOp.StringOp.Concat, exp1, exp2, tree.loc)
    }

    private def visitStringInterpolationExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.StringInterpolation)
      val init = WeededAst.Expr.Cst(Constant.Str(""), tree.loc)

      // Check for empty interpolated expressions, e.g. `"${}"` or `"${x}${}"`.
      val emptyInterpolations = findEmptyInterpolations(tree)
      if (emptyInterpolations.nonEmpty) {
        emptyInterpolations.foreach(sctx.errors.add)
        return Expr.Error(emptyInterpolations.head)
      }

      tree.children.foldLeft(init: WeededAst.Expr) {
        // A string part: Concat it onto the result
        case (acc, token@Token(_, _, _, _, _, _)) =>
          val loc = token.mkSourceLocation()
          val lit0 = token.text.stripPrefix("\"").stripSuffix("\"").stripPrefix("}")
          val lit = lit0.stripSuffix("${")
          if (lit == "") {
            acc
          } else {
            val (processed, errors) = Constants.visitChars(lit, loc)
            errors.foreach(sctx.errors.add)
            val cst = Expr.Cst(Constant.Str(processed), loc)
            Expr.Binary(SemanticOp.StringOp.Concat, acc, cst, tree.loc.asSynthetic)
          }
        // An expression part: Apply 'toString' to it and concat the result
        case (acc, tree: Tree) if tree.kind == TreeKind.Expr.Expr =>
          val expr = visitExpr(tree)
          val loc = tree.loc.asSynthetic
          val funcName = "ToString.toString"
          val str = Expr.Apply(Expr.Ambiguous(Name.mkQName(funcName), loc), List(expr), loc)
          Expr.Binary(SemanticOp.StringOp.Concat, acc, str, loc)
        // Skip anything else (Parser will have produced an error.)
        case (acc, _) => acc
      }
    }

    /**
      * Returns an [[EmptyInterpolatedExpression]] error for each empty interpolation (e.g. `${}`) in `tree`.
      *
      * Each interpolation is opened by a [[TokenKind.LiteralStringInterpolationL]] token (`"${` or `}${`) and must be
      * followed by an expression. If the opener is instead followed by another opener or the closing `}"` then the
      * interpolation is empty. This catches both fully-empty strings (`"${}"`) and empty interpolations mixed with
      * non-empty ones (`"${x}${}"`).
      */
    private def findEmptyInterpolations(tree: Tree): List[EmptyInterpolatedExpression] = {
      def isExpr(child: SyntaxTree.Child): Boolean = child match {
        case t: Tree => t.kind == TreeKind.Expr.Expr
        case _ => false
      }
      // Ignore comment tokens so they cannot sit between an opener and its expression.
      val children = tree.children.filterNot {
        case token@Token(_, _, _, _, _, _) => token.kind.isComment
        case _ => false
      }
      children.sliding(2).collect {
        case Array(opener@Token(_, _, _, _, _, _), next)
          if opener.kind == TokenKind.LiteralStringInterpolationL && !isExpr(next) =>
          EmptyInterpolatedExpression(opener.mkSourceLocation())
      }.toList
    }

    private def visitOpenVariantExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.OpenVariant)
      Expr.Open(pickQName(tree), tree.loc)
    }

    private def visitOpenVariantAsExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.OpenVariantAs)
      val name = pickQName(tree)
      Expr.OpenAs(name, pickExpr(tree), tree.loc)
    }

    private def visitHoleExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Hole)
      val ident = tryPickNameIdent(tree)
      val strippedIdent = ident.map(id => Name.Ident(id.name.stripPrefix("?"), id.loc))
      Expr.Hole(strippedIdent, tree.loc)
    }

    private def visitHoleWithExpExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.HoleVariable)
      val ident = pickNameIdent(tree)
      // Strip '?' suffix and update source location
      val sp1 = ident.loc.start
      val sp2 = SourcePosition.moveLeft(ident.loc.end)
      val id = Name.Ident(ident.name.stripSuffix("?"), SourceLocation(isReal = true, ident.loc.source, sp1, sp2))
      val expr = Expr.Ambiguous(Name.QName(Name.RootNS, id, id.loc), id.loc)
      Expr.HoleWithExp(expr, tree.loc)
    }

    private def visitExprUseExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Use)
      val use = visitUse(pick(TreeKind.UsesOrImports.Use, tree))
      Expr.Use(use, pickExpr(tree), tree.loc)
    }

    def visitLiteralExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      // Note: This visitor is used by both expression literals and pattern literals.
      expectAny(tree, List(TreeKind.Expr.Literal, TreeKind.Pattern.Literal))
      tree.children(0) match {
        case token@Token(_, _, _, _, _, _) => token.kind match {
          case TokenKind.DebugInterpolator =>
            val loc = tree.loc
            val file = loc.source.name
            val line = loc.start.lineOneIndexed
            val text = token.text.stripPrefix("d\"").stripSuffix("\"")
            Expr.Cst(Constant.Str(s"[$file:$line] $text"), loc)
          case TokenKind.KeywordNull => Expr.Cst(Constant.Null, token.mkSourceLocation())
          case TokenKind.KeywordTrue => Expr.Cst(Constant.Bool(true), token.mkSourceLocation())
          case TokenKind.KeywordFalse => Expr.Cst(Constant.Bool(false), token.mkSourceLocation())
          case TokenKind.LiteralString => Constants.toStringCst(token)
          case TokenKind.LiteralChar => Constants.toChar(token)
          case TokenKind.LiteralInt => Constants.toInt32(token)
          case TokenKind.LiteralInt8 => Constants.toInt8(token)
          case TokenKind.LiteralInt16 => Constants.toInt16(token)
          case TokenKind.LiteralInt32 => Constants.toInt32(token)
          case TokenKind.LiteralInt64 => Constants.toInt64(token)
          case TokenKind.LiteralBigInt => Constants.toBigInt(token)
          case TokenKind.LiteralFloat => Constants.toFloat64(token)
          case TokenKind.LiteralFloat32 => Constants.toFloat32(token)
          case TokenKind.LiteralFloat64 => Constants.toFloat64(token)
          case TokenKind.LiteralBigDecimal => Constants.toBigDecimal(token)
          case TokenKind.LiteralRegex => Constants.toRegex(token)
          case TokenKind.NameLowercase
               | TokenKind.NameUppercase
               | TokenKind.NameMath =>
            val ident = pickNameIdent(tree)
            Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc)
          case _ =>
            val error = UnexpectedToken(expected = NamedTokenSet.Literal, actual = None, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
            sctx.errors.add(error)
            Expr.Error(error)
        }
        case _ => throw InternalCompilerException(s"Literal had tree child", tree.loc)
      }
    }

    private def visitApplyExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Apply)
      val exprTree = pick(TreeKind.Expr.Expr, tree)
      val args = pickArguments(tree, synctx = SyntacticContext.Expr.OtherExpr)
      tryPick(TreeKind.Expr.Intrinsic, exprTree) match {
        case Some(intrinsic) => visitIntrinsic(intrinsic, Some(args), tree.loc)
        case None => Expr.Apply(visitExpr(exprTree), args, tree.loc)
      }
    }

    private def pickArguments(tree: Tree, synctx: SyntacticContext)(implicit sctx: SharedContext): List[Expr] = {
      visitArguments(pick(TreeKind.ArgumentList, tree, synctx = synctx))
    }

    /**
      * This method is the same as pickArguments but considers Unit as no-argument. It calls visitMethodArguments instead.
      */
    private def pickRawArguments(tree: Tree, synctx: SyntacticContext)(implicit sctx: SharedContext): List[Expr] = {
      visitMethodArguments(pick(TreeKind.ArgumentList, tree, synctx = synctx))
    }

    private def tryPickArguments(tree: Tree)(implicit sctx: SharedContext): Option[List[Expr]] = {
      tryPick(TreeKind.ArgumentList, tree).map(visitArguments)
    }

    private def visitArguments(tree: Tree)(implicit sctx: SharedContext): List[Expr] = {
      val args = pickAllMulti(tree, TreeKind.Argument, TreeKind.ArgumentNamed).map { t =>
        t.kind match {
          case TreeKind.Argument => pickExpr(t)
          case TreeKind.ArgumentNamed => visitArgumentNamed(t)
          case k => throw InternalCompilerException(s"unexpected tree kind '$k'", t.loc)
        }
      }
      args match {
        // Add synthetic unit arguments if there are none
        case Nil => List(Expr.Cst(Constant.Unit, tree.loc))
        case args => args
      }
    }

    /**
      * This method is the same as visitArguments but for InvokeMethod. It does not consider named arguments
      * as they are not allowed and it doesn't add unit arguments for empty arguments.
      */
    private def visitMethodArguments(tree: Tree)(implicit sctx: SharedContext): List[Expr] = {
      pickAll(TreeKind.Argument, tree).map(pickExpr)
    }

    private def visitArgumentNamed(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.ArgumentNamed)
      pickAll(TreeKind.Expr.Expr, tree).map(visitExpr) match {
        case e1 :: e2 :: Nil =>
          // First expression must be a name
          e1 match {
            case Expr.Ambiguous(qname, _) =>
              Expr.RecordExtend(Name.mkLabel(qname.ident), e2, Expr.Cst(Constant.RecordEmpty, tree.loc), tree.loc)
            case _ =>
              val error = Malformed(NamedTokenSet.Name, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
        case _ =>
          val error = Malformed(NamedTokenSet.Name, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
      }
    }

    private def visitIndexExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Index)
      pickAll(TreeKind.Expr.Expr, tree).map(visitExpr) match {
        case exp1 :: exp2 :: Nil =>
          Expr.Apply(Expr.Ambiguous(Name.mkQName("Indexable.get", exp1.loc), exp1.loc), List(exp1, exp2), tree.loc)
        case other => throw InternalCompilerException(s"Expr.Index tree with ${other.length} operands", tree.loc)
      }
    }

    private def visitIndexMutExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.IndexMut)
      pickAll(TreeKind.Expr.Expr, tree).map(visitExpr) match {
        case exp1 :: exp2 :: exp3 :: Nil =>
          Expr.Apply(Expr.Ambiguous(Name.mkQName("IndexableMut.put", exp1.loc), exp1.loc), List(exp1, exp2, exp3), tree.loc)
        case other => throw InternalCompilerException(s"Expr.IndexMut tree with ${other.length} operands", tree.loc)
      }
    }

    private def visitLambdaExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Lambda)
      val fparams = Decls.pickFormalParameters(tree, presence = Presence.Optional)
      val expr = pickExpr(tree)
      val l = tree.loc.asSynthetic
      fparams.foldRight(expr) {
        case (fparam, acc) => WeededAst.Expr.Lambda(fparam, acc, l)
      }
    }

    private def visitOperatorAsLambda(tree: Tree)(implicit sctx: SharedContext): Expr = {
      val loc = tree.loc
      val op = pick(TreeKind.Operator, tree)
      op.children.head match {
        // User-defined operators - use the operator text directly
        case t@Token(TokenKind.GenericOperator, _, _, _, _, _) =>
          Expr.Ambiguous(Name.mkQName(t.text, loc), loc)

        // Built-in operators - lookup
        case Token(kind, _, _, _, _, _) =>
          tokenOperatorToName(kind) match {
            case Some(funcName) =>
              Expr.Ambiguous(Name.mkQName(funcName, loc), loc)
            case None =>
              throw InternalCompilerException(s"Unsupported operator: $kind", loc)
          }
      }
    }

    private def visitLambdaExtMatchExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LambdaExtMatch)
      Expr.LambdaExtMatch(Patterns.pickExtPattern(tree), pickExpr(tree), tree.loc)
    }

    private def visitLambdaMatchExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LambdaMatch)
      val pat = Patterns.pickPattern(tree)
      Expr.LambdaMatch(Patterns.restrictToNonConstant(pat), pickExpr(tree), tree.loc)
    }

    private def visitUnaryExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Unary)
      val opTree = pick(TreeKind.Operator, tree)
      val exprTree = pick(TreeKind.Expr.Expr, tree)
      opTree.children.headOption match {
        case Some(opToken@Token(_, _, _, _, _, _)) =>
          val literalToken = tryPickNumberLiteralToken(exprTree)
          literalToken match {
            // fold unary minus into a constant
            case Some(lit) if opToken.text == "-" =>
              // Construct a synthetic literal tree with the unary minus and visit that like any other literal expression
              val syntheticToken = Token(lit.kind, lit.src, opToken.startIndex, lit.endIndex, lit.start, lit.end)
              val syntheticLiteral = Tree(TreeKind.Expr.Literal, Array(syntheticToken), exprTree.loc.asSynthetic)
              visitLiteralExpr(syntheticLiteral)
            case _ =>
              val expr = visitExpr(exprTree)
              opToken.text match {
                case "discard" => Expr.Discard(expr, tree.loc)
                case "force" => Expr.Force(expr, tree.loc)
                case "lazy" => Expr.Lazy(expr, tree.loc)
                case "not" => Expr.Unary(SemanticOp.BoolOp.Not, expr, tree.loc)
                case "-" => Expr.Apply(Expr.Ambiguous(Name.mkQName("Neg.neg", tree.loc), opTree.loc), List(expr), tree.loc)
                case "+" =>
                  val error = IllegalUnaryPlus(opTree.loc)
                  sctx.errors.add(error)
                  expr
                case op => Expr.Apply(Expr.Ambiguous(Name.mkQName(op, tree.loc), opTree.loc), List(expr), tree.loc)
              }
          }
        case Some(_) => throw InternalCompilerException(s"Expected unary operator but found tree", tree.loc)
        case None => throw InternalCompilerException(s"Parser produced tree of kind 'Op' without child", tree.loc)
      }
    }

    private def tryPickNumberLiteralToken(tree: Tree): Option[Token] = {
      val NumberLiteralKinds = List(TokenKind.LiteralInt, TokenKind.LiteralInt8, TokenKind.LiteralInt16, TokenKind.LiteralInt32, TokenKind.LiteralInt64, TokenKind.LiteralBigInt, TokenKind.LiteralFloat, TokenKind.LiteralFloat32, TokenKind.LiteralFloat64, TokenKind.LiteralBigDecimal)
      val maybeTree = tryPick(TreeKind.Expr.Literal, tree)
      maybeTree.flatMap(_.children(0) match {
        case t@Token(_, _, _, _, _, _) if NumberLiteralKinds.contains(t.kind) => Some(t)
        case _ => None
      })
    }

    private def visitBinaryExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Binary)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      val op = pick(TreeKind.Operator, tree)
      exprs.map(visitExpr) match {
        case e1 :: e2 :: Nil =>
          def mkApply(name: String): Expr.Apply = Expr.Apply(
            Expr.Ambiguous(Name.mkQName(name, op.loc), op.loc), List(e1, e2),
            tree.loc
          )

          tryPickQName(op) match {
            case Some(name) =>
              // Infix Operator.
              val opExpr = Expr.Ambiguous(name, op.loc)
              Expr.Infix(e1, opExpr, e2, tree.loc)
            case None =>
              // Single Token Operator (or synthetic ErrorOperator).
              op.children.head match {
                case t: SyntaxTree.Tree if t.kind == TreeKind.OperatorError =>
                  // Synthetic OperatorError inserted by the parser to recover from a missing binary operator.
                  // Use the source location between the two expressions to indicate where the operator is missing.
                  val betweenLoc = SourceLocation(isReal = true, e1.loc.source, e1.loc.end, e2.loc.start)
                  val error = ParseError.MissingBinaryOperator(SyntacticContext.Expr.OtherExpr, betweenLoc)
                  sctx.errors.add(error)
                  Expr.LetMatch(Pattern.Wild(tree.loc.asSynthetic), None, e1, e2, tree.loc)
                // Standard operators.
                case Token(kind, _, _, _, _, _) if tokenOperatorToName(kind).isDefined =>
                  mkApply(tokenOperatorToName(kind).get)

                // Special cases that create different AST nodes
                case Token(TokenKind.KeywordAnd, _, _, _, _, _) => Expr.Binary(SemanticOp.BoolOp.And, e1, e2, tree.loc)
                case Token(TokenKind.KeywordOr, _, _, _, _, _) => Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, tree.loc)
                case Token(TokenKind.ColonColon, _, _, _, _, _) => Expr.FCons(e1, e2, tree.loc)
                case Token(TokenKind.AngledPlus, _, _, _, _, _) => Expr.FixpointMerge(e1, e2, tree.loc)
                case Token(TokenKind.KeywordInstanceOf, _, _, _, _, _) =>
                  tryPickQName(exprs(1)) match {
                    case Some(qname) =>
                      if (qname.isUnqualified) Expr.InstanceOf(e1, qname.ident, tree.loc)
                      else {
                        val error = IllegalQualifiedName(exprs(1).loc)
                        sctx.errors.add(error)
                        Expr.Error(error)
                      }
                    case None =>
                      val error = UnexpectedToken(
                        NamedTokenSet.FromTreeKinds(Set(TreeKind.QName)),
                        None,
                        SyntacticContext.Expr.OtherExpr,
                        hint = Some("Use a single unqualified Java type like 'Object' instead of 'java.lang.object'."),
                        loc = exprs(1).loc
                      )
                      sctx.errors.add(error)
                      Expr.Error(error)
                  }
                case token@Token(TokenKind.GenericOperator, _, _, _, _, _) =>
                  val ident = Name.Ident(token.text, op.loc)
                  Expr.Apply(Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), op.loc), List(e1, e2), tree.loc)
                case _ =>
                  throw InternalCompilerException(s"Expr.Binary operator not recognized", tree.loc)
              }
          }
        case operands => throw InternalCompilerException(s"Expr.Binary tree with ${operands.length} operands", tree.loc)
      }
    }

    /* Optionally returns the function name of the given operator. */
    private def tokenOperatorToName(kind: TokenKind): Option[String] = kind match {
      // Arithmetic operators
      case TokenKind.Plus => Some("Add.add")
      case TokenKind.Minus => Some("Sub.sub")
      case TokenKind.Star => Some("Mul.mul")
      case TokenKind.Slash => Some("Div.div")

      // Comparison operators
      case TokenKind.AngleL => Some("Order.less")
      case TokenKind.AngleLEqual => Some("Order.lessEqual")
      case TokenKind.AngleR => Some("Order.greater")
      case TokenKind.AngledEqual => Some("Order.compare")
      case TokenKind.AngleREqual => Some("Order.greaterEqual")
      case TokenKind.EqualEqual => Some("Eq.eq")
      case TokenKind.BangEqual => Some("Eq.neq")

      // List operators
      case TokenKind.ColonColonColon => Some("List.append")

      // Unsupported operators
      case _ => None
    }

    private def visitIfThenElseExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.IfThenElse)
      pickAll(TreeKind.Expr.Expr, tree).map(visitExpr) match {
        case condition :: tthen :: eelse :: Nil =>
          Expr.IfThenElse(condition, tthen, Some(eelse), tree.loc)
        case condition :: tthen :: Nil =>
          Expr.IfThenElse(condition, tthen, None, tree.loc)
        case exprs =>
          throw InternalCompilerException(s"Parser error. Expected 2 expressions in statement but found '${exprs.length}'.", tree.loc)
      }
    }

    /**
      * Flattens a right-nested Statement tree into a list of head expression trees and a single tail tree.
      * Uses `unfold` to peek through Expr.Expr wrapper nodes.
      *
      * Example: Statement(f(), Expr(Statement(g(), Expr(h()))))
      *       => (List(f(), g()), h())
      */
    private def flattenStmTree(tree: Tree): (List[Tree], Tree) = {
      val heads = mutable.ArrayBuffer.empty[Tree]
      var current = tree
      var tail: Tree = null
      var done = false
      while (!done) {
        pickAll(TreeKind.Expr.Expr, current) match {
          case first :: second :: Nil =>
            heads.addOne(first)
            val inner = unfold(second)
            if (inner.kind == TreeKind.Expr.Statement) {
              current = inner
            } else {
              tail = second
              done = true
            }
          case exprs =>
            throw InternalCompilerException(s"Parser error. Expected 2 expressions in statement but found '${exprs.length}'.", current.loc)
        }
      }
      (heads.toList, tail)
    }

    private def visitStatementExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Statement)
      val (headTrees, tailTree) = flattenStmTree(tree)
      Expr.Stm(headTrees.map(visitExpr), visitExpr(tailTree), tree.loc)
    }

    private def visitLocalDefExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LocalDef)
      val ann = Decls.pickAnnotations(tree) match {
        case Annotations(as) =>
          // Only @Terminates and @Tailrec are allowed on local defs
          for (a <- as) a match {
            case _: Annotation.TailRecursive => () // allowed
            case _: Annotation.Terminates    => () // allowed
            case _                           => sctx.errors.add(IllegalAnnotation(a.toString, a.loc))
          }
          Annotations(as.filter(a => a.isInstanceOf[Annotation.TailRecursive] || a.isInstanceOf[Annotation.Terminates]))
      }

      // Extract (defBody, restExp) from the Stm wrapping the local def.
      val (exp1, exp2) = pickExpr(tree) match {
        case Expr.Stm(defBody :: rest, exp, loc) =>
          val continuation = if (rest.isEmpty) exp else Expr.Stm(rest, exp, loc)
          (defBody, continuation)
        case e =>
          // Fall back on Expr.Error. Parser has reported an error here.
          val error = Malformed(NamedTokenSet.FromKinds(Set(TokenKind.KeywordDef)), SyntacticContext.Expr.OtherExpr, hint = Some("Internal definitions must be followed by an expression"), loc = e.loc)
          (e, Expr.Error(error))
      }

      val ident = pickNameIdent(tree)
      val declaredEff = Types.tryPickEffect(tree)
      val fparams = Decls.pickFormalParameters(tree, Presence.Optional)
      val declaredTpe = Types.tryPickType(tree)
      Expr.LocalDef(ann, ident, fparams, declaredTpe, declaredEff, exp1, exp2, tree.loc)
    }

    private def visitRegionExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Region)
      val block = visitBlockExpr(pick(TreeKind.Expr.Block, tree))
      val ident = pickNameIdent(tree)
      Expr.Region(ident, block, tree.loc)
    }

    private def visitMatchExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Match)
      val rules0 = pickAll(TreeKind.Expr.MatchRuleFragment, tree)
      val expr = pickExpr(tree)
      rules0.map(visitMatchRule) match {
        // Case: no valid match rule found in match expr
        case Nil =>
          val error = NeedAtleastOne(NamedTokenSet.MatchRule, SyntacticContext.Expr.OtherExpr, loc = expr.loc)
          sctx.errors.add(error)
          // Fall back on Expr.Error.
          Expr.Error(error)
        case rules => Expr.Match(expr, rules, tree.loc)
      }
    }

    private def visitMatchRule(tree: Tree)(implicit sctx: SharedContext): MatchRule = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      val pat = Patterns.pickPattern(tree)
      exprs.map(visitExpr) match {
        // case pattern => expr
        case expr :: Nil => MatchRule(pat, None, expr, tree.loc)
        // case pattern if expr => expr
        case expr1 :: expr2 :: Nil => MatchRule(pat, Some(expr1), expr2, tree.loc)
        // Fall back on Expr.Error. Parser has reported an error here.
        case _ =>
          val error = Malformed(NamedTokenSet.MatchRule, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          MatchRule(Pattern.Error(tree.loc), None, Expr.Error(error), tree.loc)
      }
    }

    private def visitRestrictableChooseExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expectAny(tree, List(TreeKind.Expr.RestrictableChoose, TreeKind.Expr.RestrictableChooseStar))
      val isStar = tree.kind == TreeKind.Expr.RestrictableChooseStar
      val rules0 = pickAll(TreeKind.Expr.MatchRuleFragment, tree)
      val expr = pickExpr(tree)
      rules0.map(t => visitRestrictableChooseRule(isStar, t)) match {
        case Nil =>
          val error = NeedAtleastOne(NamedTokenSet.MatchRule, SyntacticContext.Expr.OtherExpr, None, tree.loc)
          sctx.errors.add(error)
          val rules = RestrictableChooseRule(WeededAst.RestrictableChoosePattern.Error(tree.loc.asSynthetic), Expr.Error(error)) :: Nil
          Expr.RestrictableChoose(isStar, expr, rules, tree.loc)
        case rules => Expr.RestrictableChoose(isStar, expr, rules, tree.loc)
      }
    }

    private def visitRestrictableChooseRule(isStar: Boolean, tree: Tree)(implicit sctx: SharedContext): RestrictableChooseRule = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      val pat = pickRestrictableChoosePattern(isStar, tree)
      RestrictableChooseRule(pat, pickExpr(tree))
    }

    private def pickRestrictableChoosePattern(isStar: Boolean, tree: Tree)(implicit sctx: SharedContext): RestrictableChoosePattern = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      Patterns.pickPattern(tree) match {
        case Pattern.Tag(qname, pats, loc0) =>
          val inner = pats.map {
            case Pattern.Wild(loc) => WeededAst.RestrictableChoosePattern.Wild(loc)
            case Pattern.Var(ident, loc) => WeededAst.RestrictableChoosePattern.Var(ident, loc)
            case Pattern.Cst(Constant.Unit, loc) => WeededAst.RestrictableChoosePattern.Wild(loc)
            case _ =>
              val error = UnsupportedRestrictedChoicePattern(isStar, loc0)
              sctx.errors.add(error)
              WeededAst.RestrictableChoosePattern.Error(loc0.asSynthetic)
          }
          RestrictableChoosePattern.Tag(qname, inner, loc0)
        case other =>
          val error = UnsupportedRestrictedChoicePattern(isStar, other.loc)
          sctx.errors.add(error)
          RestrictableChoosePattern.Error(other.loc)
      }
    }

    private def visitForApplicativeExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.ForApplicative)
      val fragments = pickForFragments(tree)
      val expr = pickExpr(tree)
      fragments match {
        case Nil =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case fragments =>
          // Check that there are only generators
          val (generators, nonGeneratorFragments) = fragments.partition {
            case _: ForFragment.Generator => true
            case _ => false
          }
          if (nonGeneratorFragments.nonEmpty) {
            val errors = nonGeneratorFragments.map(f => IllegalForAFragment(f.loc))
            errors.foreach(sctx.errors.add)
            val error = IllegalForAFragment(nonGeneratorFragments.head.loc)
            Expr.Error(error)
          } else {
            Expr.ApplicativeFor(generators.asInstanceOf[List[ForFragment.Generator]], expr, tree.loc)
          }
      }
    }

    private def visitForeachExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Foreach)
      val fragments = pickForFragments(tree)
      val expr = pickExpr(tree)
      fragments match {
        case Nil =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case fragments =>
          // It's okay to do head rather than headOption here because we check for Nil above.
          fragments.head match {
            // Check that fragments start with a generator.
            case _: ForFragment.Generator => Expr.ForEach(fragments, expr, tree.loc)
            case f =>
              val error = IllegalForFragment(f.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }

    private def visitForMonadicExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.ForMonadic)
      val fragments = pickForFragments(tree)
      val expr = pickExpr(tree)
      fragments match {
        case Nil =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case fragments =>
          // It's okay to do head rather than headOption here because we check for Nil above.
          fragments.head match {
            // Check that fragments start with a generator.
            case _: ForFragment.Generator => Expr.MonadicFor(fragments, expr, tree.loc)
            case f =>
              val error = IllegalForFragment(f.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }


    private def pickForFragments(tree: Tree)(implicit sctx: SharedContext): List[ForFragment] = {
      pickAllMulti(tree, TreeKind.Expr.ForFragmentGuard, TreeKind.Expr.ForFragmentGenerator, TreeKind.Expr.ForFragmentLet).map { t =>
        t.kind match {
          case TreeKind.Expr.ForFragmentGuard => visitForFragmentGuard(t)
          case TreeKind.Expr.ForFragmentGenerator => visitForFragmentGenerator(t)
          case TreeKind.Expr.ForFragmentLet => visitForFragmentLet(t)
          case k => throw InternalCompilerException(s"unexpected tree kind '$k'", t.loc)
        }
      }
    }

    private def visitForFragmentGuard(tree: Tree)(implicit sctx: SharedContext): ForFragment.Guard = {
      expect(tree, TreeKind.Expr.ForFragmentGuard)
      ForFragment.Guard(pickExpr(tree), tree.loc)
    }

    private def visitForFragmentGenerator(tree: Tree)(implicit sctx: SharedContext): ForFragment.Generator = {
      expect(tree, TreeKind.Expr.ForFragmentGenerator)
      val pat = Patterns.pickPattern(tree)
      ForFragment.Generator(Patterns.restrictToNonConstant(pat), pickExpr(tree), tree.loc)
    }

    private def visitForFragmentLet(tree: Tree)(implicit sctx: SharedContext): ForFragment.Let = {
      expect(tree, TreeKind.Expr.ForFragmentLet)
      val pat = Patterns.pickPattern(tree)
      ForFragment.Let(Patterns.restrictToNonConstant(pat), pickExpr(tree), tree.loc)
    }

    private def visitLetMatchExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LetMatch)
      val tpe = Types.tryPickType(tree)
      val pattern = Patterns.pickPattern(tree)
      // Extract (boundValue, restExp) from the Stm wrapping the let-match.
      val (boundValue, continuation) = pickExpr(tree) match {
        case Expr.Stm(bv :: rest, exp, loc) =>
          val cont = if (rest.isEmpty) exp else Expr.Stm(rest, exp, loc)
          (bv, cont)
        // Fall back on Expr.Error. Parser has reported an error here.
        case e =>
          // The location of the error is the end of the expression, zero-width.
          val loc = e.loc.copy(start = e.loc.end).asSynthetic
          val error = Malformed(NamedTokenSet.FromKinds(Set(TokenKind.KeywordLet)), SyntacticContext.Expr.OtherExpr, hint = Some("let-bindings must be followed by an expression"), loc)
          (e, Expr.Error(error))
      }
      Expr.LetMatch(Patterns.restrictToNonConstant(pattern), tpe, boundValue, continuation, tree.loc)
    }

    private def visitExtMatch(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.ExtMatch)
      val rules0 = pickAll(TreeKind.Expr.ExtMatchRuleFragment, tree)
      val expr = pickExpr(tree)
      rules0.map(visitExtMatchRule) match {
        // Case: no valid match rule found in ematch expr
        case Nil =>
          val error = NeedAtleastOne(NamedTokenSet.ExtMatchRule, SyntacticContext.Expr.OtherExpr, loc = expr.loc)
          sctx.errors.add(error)
          val defaultRule = ExtMatchRule(ExtPattern.Error(expr.loc), Expr.Error(error), tree.loc)
          Expr.ExtMatch(expr, defaultRule :: Nil, tree.loc)
        case rules =>
          Expr.ExtMatch(expr, rules.flatten, tree.loc)
      }
    }

    private def visitExtMatchRule(tree: Tree)(implicit sctx: SharedContext): Option[ExtMatchRule] = {
      expect(tree, TreeKind.Expr.ExtMatchRuleFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      val pat = Patterns.pickExtPattern(tree)
      exprs.map(visitExpr) match {
        case expr :: Nil =>
          // case pat => expr
          Some(ExtMatchRule(pat, expr, tree.loc))

        case _ =>
          // Fall back on None. Parser has reported an error here.
          None
      }
    }

    private def visitExtTag(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.ExtTag)
      val ident = pickNameIdent(tree)
      tryPickArguments(tree) match {
        case None =>
          // Nullary constructor
          Expr.ExtTag(Name.mkLabel(ident), List.empty, tree.loc)

        case Some(exps) =>
          Expr.ExtTag(Name.mkLabel(ident), exps, tree.loc)
      }
    }

    private def visitTupleExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Tuple)
      val args = pickAllMulti(tree, TreeKind.Argument, TreeKind.ArgumentNamed).map { t =>
        t.kind match {
          case TreeKind.Argument => pickExpr(t)
          case TreeKind.ArgumentNamed => visitArgumentNamed(t)
          case k => throw InternalCompilerException(s"unexpected tree kind '$k'", t.loc)
        }
      }
      Expr.Tuple(args, tree.loc)
    }

    private def visitLiteralRecordExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.RecordOperation)
      // `{ +x = expr }` is not allowed.
      pickAll(TreeKind.Expr.RecordOpExtend, tree).foreach { t =>
        val error = IllegalRecordOperation(t.loc)
        sctx.errors.add(error)
      }
      // `{ -x }` is not allowed.
      pickAll(TreeKind.Expr.RecordOpRestrict, tree).foreach { t =>
        val error = IllegalRecordOperation(t.loc)
        sctx.errors.add(error)
      }
      val fields = pickAll(TreeKind.Expr.RecordOpUpdate, tree).map(visitLiteralRecordField)
      fields.foldRight(Expr.Cst(Constant.RecordEmpty, tree.loc.asSynthetic): Expr) {
        case ((label, expr, loc), acc) =>
          val SourceLocation(isReal, src, sp1, _) = loc
          val extendLoc = SourceLocation(isReal, src, sp1, tree.loc.end)
          Expr.RecordExtend(label, expr, acc, extendLoc)
      }
    }

    private def visitLiteralRecordField(tree: Tree)(implicit sctx: SharedContext): (Name.Label, Expr, SourceLocation) = {
      val ident = pickNameIdent(tree)
      (Name.mkLabel(ident), pickExpr(tree), tree.loc)
    }

    private def visitRecordSelectExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.RecordSelect)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      idents.foldLeft(pickExpr(tree)) {
        case (acc, ident) =>
          val loc = SourceLocation(ident.loc.isReal, tree.loc.source, tree.loc.start, ident.loc.end)
          Expr.RecordSelect(acc, Name.mkLabel(ident), loc)
      }
    }

    private def visitRecordOperationOrLiteralExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      if (hasToken(TokenKind.Bar, tree)) {
        // { +x = expr | expr }
        visitRecordOperationExpr(tree)
      } else {
        // { x = expr }
        visitLiteralRecordExpr(tree)
      }
    }

    private def visitRecordOperationExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.RecordOperation)
      val ops = pickAllMulti(tree, TreeKind.Expr.RecordOpUpdate, TreeKind.Expr.RecordOpExtend, TreeKind.Expr.RecordOpRestrict)
      if (ops.isEmpty) {
        val error = NeedAtleastOne(NamedTokenSet.FromKinds(Set(TokenKind.Plus, TokenKind.Minus, TokenKind.NameLowercase)), SyntacticContext.Expr.OtherExpr, hint = Some("Record operations must contain at least one operation"), tree.loc)
        sctx.errors.add(error)
      }
      ops.foldRight(pickExpr(tree))((op, acc) => {
        // The base record `acc` appears to the right of `op` (e.g. `{ op | acc }`), so the location
        // of each constructed node must span both the operation and the accumulated base record.
        val loc = op.loc.spanWith(acc.loc)
        op.kind match {
          case TreeKind.Expr.RecordOpExtend =>
            val id = pickNameIdent(op)
            Expr.RecordExtend(Name.mkLabel(id), pickExpr(op), acc, loc)
          case TreeKind.Expr.RecordOpRestrict =>
            val id = pickNameIdent(op)
            Expr.RecordRestrict(Name.mkLabel(id), acc, loc)
          case TreeKind.Expr.RecordOpUpdate =>
            val id = pickNameIdent(op)
            // An update is a restrict followed by an extension
            val restricted = Expr.RecordRestrict(Name.mkLabel(id), acc, loc)
            Expr.RecordExtend(Name.mkLabel(id), pickExpr(op), restricted, loc)
          case k => throw InternalCompilerException(s"'$k' is not a record operation", op.loc)
        }
      })
    }

    private def visitLiteralArrayExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LiteralArray)
      val exprs0 = pickAll(TreeKind.Expr.Expr, tree)
      val regionName = tryPick(TreeKind.Expr.RegionName, tree)
      val exprs = exprs0.map(visitExpr)
      regionName.map(visitRegionName) match {
        case Some(region) => Expr.ArrayLit(exprs, region, tree.loc)
        case None =>
          val error = MissingRegion(TokenKind.ArrayHash, SyntacticContext.Expr.OtherExpr, tree.loc)
          sctx.errors.add(error)
          Expr.ArrayLit(exprs, Expr.Error(error), tree.loc)
      }
    }

    private def visitRegionName(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.RegionName)
      pickExpr(tree)
    }

    private def visitLiteralVectorExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LiteralVector)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      Expr.VectorLit(exprs.map(visitExpr), tree.loc)
    }

    private def visitLiteralListExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LiteralList)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      Expr.ListLit(exprs.map(visitExpr), tree.loc)
    }

    private def visitLiteralMapExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LiteralMap)
      val pairs = pickAll(TreeKind.Expr.LiteralMapKeyValueFragment, tree)
      Expr.MapLit(pairs.map(visitKeyValuePair), tree.loc)
    }

    private def visitKeyValuePair(tree: Tree)(implicit sctx: SharedContext): (Expr, Expr) = {
      expect(tree, TreeKind.Expr.LiteralMapKeyValueFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      exprs.map(visitExpr) match {
        // case: k => v
        case k :: v :: Nil => (k, v)
        // case: k =>
        case k :: Nil =>
          val error = Malformed(NamedTokenSet.KeyValuePair, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          (k, Expr.Error(error))
        case xs => throw InternalCompilerException(s"Malformed KeyValue pair, found ${xs.length} expressions", tree.loc)
      }
    }

    private def visitLiteralSetExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.LiteralSet)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      Expr.SetLit(exprs.map(visitExpr), tree.loc)
    }

    private def visitAscribeExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Ascribe)
      val eff = Types.tryPickEffect(tree)
      val tpe = Types.tryPickTypeNoWild(tree)
      Expr.Ascribe(pickExpr(tree), tpe, eff, tree.loc)
    }

    private def visitCheckedTypeCastExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.CheckedTypeCast)
      Expr.CheckedCast(CheckedCastType.TypeCast, pickExpr(tree), tree.loc)
    }

    private def visitCheckedEffectCastExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.CheckedEffectCast)
      Expr.CheckedCast(CheckedCastType.EffectCast, pickExpr(tree), tree.loc)
    }

    private def visitUncheckedCastExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.UncheckedCast)
      val eff = Types.tryPickEffect(tree)
      val tpe = Types.tryPickTypeNoWild(tree)
      Expr.UncheckedCast(pickExpr(tree), tpe, eff, tree.loc)
    }

    private def visitUnsafeExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Unsafe)
      val asEff = tryPick(TreeKind.Expr.UnsafeAsEffFragment, tree).map(Types.pickType)
      val eff = Types.pickType(tree)
      Expr.Unsafe(pickExpr(tree), eff, asEff, tree.loc)
    }


    private def visitRunExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Run)
      val maybeWith = pickAll(TreeKind.Expr.RunWithBodyExpr, tree)
      val expr = pickExpr(tree)
      maybeWith.map(visitRunWithBody) match {
        // Bad case: run expr
        case Nil =>
          // Fall back on Expr.Error
          val error = UnexpectedToken(
            expected = NamedTokenSet.FromKinds(Set(TokenKind.KeywordCatch, TokenKind.KeywordWith)),
            actual = None,
            SyntacticContext.Expr.OtherExpr,
            loc = tree.loc)
          sctx.errors.add(error)
          Expr.RunWith(expr, List(Expr.Error(error)), tree.loc)
        // Case: run expr [with expr]...
        case exprs => Expr.RunWith(expr, exprs, tree.loc)
      }
    }

    private def visitHandlerExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Handler)
      val rules = pickAll(TreeKind.Expr.RunWithRuleFragment, tree)
      val eff = pickQName(tree) // This qname is an effect
      Expr.Handler(eff, rules.map(visitRunWithRule), tree.loc)
    }

    private def visitTryExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Try)
      val maybeCatch = pickAll(TreeKind.Expr.TryCatchBodyFragment, tree)
      val expr = pickExpr(tree)
      maybeCatch.map(visitTryCatchBody) match {
        // Bad case: try expr
        case Nil | Nil :: Nil =>
          // Fall back on Expr.Error
          val error = NeedAtleastOne(NamedTokenSet.CatchRule, SyntacticContext.Expr.OtherExpr, None, tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        // Case: try expr catch { rules... }
        case catches => Expr.TryCatch(expr, catches.flatten, tree.loc)
      }
    }

    private def visitTryCatchBody(tree: Tree)(implicit sctx: SharedContext): List[CatchRule] = {
      expect(tree, TreeKind.Expr.TryCatchBodyFragment)
      val rules = pickAll(TreeKind.Expr.TryCatchRuleFragment, tree)
      rules.map(visitTryCatchRule)
    }

    private def visitTryCatchRule(tree: Tree)(implicit sctx: SharedContext): CatchRule = {
      expect(tree, TreeKind.Expr.TryCatchRuleFragment)
      val qname = pickQName(tree)
      val ident = pickNameIdent(tree)
      pickExpr(tree) match {
        case expr if qname.isUnqualified => CatchRule(ident, qname.ident, expr, tree.loc)
        case expr =>
          val error = IllegalQualifiedName(qname.loc)
          sctx.errors.add(error)
          CatchRule(ident, qname.ident, expr, tree.loc)
      }
    }

    private def visitRunWithBody(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.RunWithBodyExpr)
      pickExpr(tree)
    }

    private def visitRunWithRule(tree: Tree)(implicit sctx: SharedContext): HandlerRule = {
      expect(tree, TreeKind.Expr.RunWithRuleFragment)
      val ident = pickNameIdent(tree)
      val fparams0 = Decls.pickFormalParameters(tree, Presence.Forbidden)
      val expr = pickExpr(tree)
      // `def f()` becomes `def f(_unit: Unit)` (via Decls.pickFormalParameters).
      // `def f(x)` becomes `def f(_unit: Unit, x)`.
      // `def f(x, y, ..)` is unchanged.
      fparams0 match {
        case fparam :: Nil =>
          // Since a continuation argument must always be there, the underlying function needs a
          // unit param. For example `def f(k)` becomes `def f(_unit: Unit, k)`.

          // The new param has the zero-width location of the actual argument.
          val loc = SourceLocation.zeroPoint(isReal = false, fparam.loc.source, fparam.loc.start)
          val unitParam = Decls.unitFormalParameter(loc)
          HandlerRule(ident, List(unitParam, fparam), expr, tree.loc)
        case fparams =>
          HandlerRule(ident, fparams, expr, tree.loc)
      }
    }

    private def visitThrow(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Throw)
      Expr.Throw(pickExpr(tree), tree.loc)
    }

    private def visitInvokeConstructorExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.InvokeConstructor)
      val exps = tryPick(TreeKind.ArgumentList, tree) match {
        case None =>
          val error = WeederError.MissingArgumentList(tree.loc)
          sctx.errors.add(error)
          List.empty
        case Some(argumentList) =>
          visitMethodArguments(argumentList)
      }
      Types.pickType(tree) match {
        case WeededAst.Type.Ambiguous(qname, _) if qname.isUnqualified =>
          Expr.InvokeConstructor(qname.ident, exps, tree.loc)
        case _ =>
          val error = IllegalQualifiedName(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
      }
    }

    private def visitInvokeSuperConstructorExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.InvokeSuperConstructor)
      val exps = tryPick(TreeKind.ArgumentList, tree) match {
        case None =>
          val error = WeederError.MissingArgumentList(tree.loc)
          sctx.errors.add(error)
          List.empty
        case Some(argumentList) =>
          visitMethodArguments(argumentList)
      }
      Expr.InvokeSuperConstructor(exps, tree.loc)
    }

    private def visitInvokeMethodExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.InvokeMethod)
      val baseExp = pickExpr(tree)
      val method = pickNameIdent(tree)
      val argsExps = pickRawArguments(tree, synctx = SyntacticContext.Expr.OtherExpr)
      Expr.InvokeMethod(baseExp, method, argsExps, tree.loc)
    }

    private def visitInvokeSuperMethodExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.InvokeSuperMethod)
      val method = pickNameIdent(tree)
      val argsExps = pickRawArguments(tree, synctx = SyntacticContext.Expr.OtherExpr)
      Expr.InvokeSuperMethod(method, argsExps, tree.loc)
    }

    private def visitGetFieldExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.GetField)
      val baseExp = pickExpr(tree)
      val method = pickNameIdent(tree)
      Expr.GetField(baseExp, method, tree.loc)
    }

    private def visitAmbiguousNewExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.AmbiguousNew)
      val fieldTrees = pickAll(TreeKind.Expr.LiteralStructFieldFragment, tree)
      val constructorTrees = pickAll(TreeKind.Expr.JvmConstructor, tree)
      val methodTrees = pickAll(TreeKind.Expr.JvmMethod, tree)
      val regionTree = tryPick(TreeKind.Expr.Expr, tree)
      val tpe = Types.pickType(tree)
      val region = regionTree.map(Exprs.visitExpr)
      val fields = fieldTrees.map(visitNewStructField)
      val constructors = constructorTrees.map(visitJvmConstructor)
      val methods = methodTrees.map(visitJvmMethod)
      Expr.AmbiguousNew(tpe, region, fields, constructors, methods, tree.loc)
    }

    private def visitStructGetExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.StructGet)
      val ident = pickNameIdent(tree)
      Expr.StructGet(pickExpr(tree), Name.mkLabel(ident), tree.loc)
    }

    private def visitStructPutExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.StructPut)
      val struct = pickExpr(tree)
      val ident = pickNameIdent(tree)
      val rhs = pickExpr(pick(TreeKind.Expr.StructPutRHS, tree))
      Expr.StructPut(struct, Name.mkLabel(ident), rhs, tree.loc)
    }

    private def visitJvmMethod(tree: Tree)(implicit sctx: SharedContext): JvmMethod = {
      expect(tree, TreeKind.Expr.JvmMethod)
      val jvmAnns = pickJvmAnnotations(tree)
      val ident = pickNameIdent(tree)
      val tpe = Types.pickType(tree)
      val eff = Types.tryPickEffect(tree)
      val fparams = Decls.pickFormalParameters(tree)
      JvmMethod(jvmAnns, ident, fparams, pickExpr(tree), tpe, eff, tree.loc)
    }

    /**
      * Extracts JVM annotations from a JvmMethod tree node.
      * All annotations are extracted as JvmAnnotation objects;
      * filtering of Flix annotations is done later in the Resolver.
      */
    private def pickJvmAnnotations(tree: Tree)(implicit sctx: SharedContext): List[WeededAst.JvmAnnotation] = {
      val optAnn = tryPick(TreeKind.AnnotationList, tree)
      optAnn.map { annTree =>
        val tokens = pickAllTokens(annTree)
        tokens.toList.map { token =>
          val loc = token.mkSourceLocation()
          val name = token.text.stripPrefix("@")
          WeededAst.JvmAnnotation(Name.Ident(name, loc), loc)
        }
      }.getOrElse(Nil)
    }

    private def visitJvmConstructor(tree: Tree)(implicit sctx: SharedContext): JvmConstructor = {
      expect(tree, TreeKind.Expr.JvmConstructor)
      val tpe = Types.pickType(tree)
      val eff = Types.tryPickEffect(tree)
      JvmConstructor(pickExpr(tree), tpe, eff, tree.loc)
    }

    private def visitNewStructField(tree: Tree)(implicit sctx: SharedContext): (Name.Label, Expr) = {
      expect(tree, TreeKind.Expr.LiteralStructFieldFragment)
      val ident = pickNameIdent(tree)
      (Name.mkLabel(ident), pickExpr(tree))
    }

    private def visitStaticExpr(tree: Tree): Expr = {
      expect(tree, TreeKind.Expr.Static)
      Expr.Static(tree.loc)
    }

    private def visitSelectExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Select)
      val rules = pickAll(TreeKind.Expr.SelectRuleFragment, tree).map(visitSelectRule)
      val maybeDefault = tryPick(TreeKind.Expr.SelectRuleDefaultFragment, tree).map(pickExpr)
      Result.sequence(rules) match {
        case Result.Ok(rs) => Expr.SelectChannel(rs, maybeDefault, tree.loc)
        case Result.Err(error) => Expr.Error(error)
      }
    }

    private def visitSelectRule(tree: Tree)(implicit sctx: SharedContext): Result[SelectChannelRule, UnexpectedSelectChannelRuleFunction] = {
      expect(tree, TreeKind.Expr.SelectRuleFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree).map(visitExpr)
      val qname = pickQName(tree)
      val ident = pickNameIdent(tree)
      exprs match {
        case channel :: body :: Nil => // Shape is correct
          val isRecvFunction = qname.toString == "Channel.recv" || qname.toString == "recv"
          if (isRecvFunction) {
            Result.Ok(SelectChannelRule(ident, channel, body, tree.loc))
          } else {
            val error = UnexpectedSelectChannelRuleFunction(qname)
            sctx.errors.add(error)
            Result.Err(error)
          }
        case _ => // Unreachable
          throw InternalCompilerException("unexpected invalid select channel rule", tree.loc)
      }
    }

    private def visitSpawnExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Spawn)
      val regionName = tryPick(TreeKind.Expr.RegionName, tree)
      val expr1 = pickExpr(tree)
      regionName.map(visitRegionName) match {
        case Some(expr2) =>
          Expr.Spawn(expr1, expr2, tree.loc)
        case None =>
          val error = MissingRegion(TokenKind.KeywordSpawn, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          Expr.Spawn(expr1, Expr.Error(error), tree.loc)
      }
    }

    private def visitParYieldExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.ParYield)
      val fragments = pickAll(TreeKind.Expr.ParYieldFragment, tree)
      if (fragments.isEmpty) {
        val error = NeedAtleastOne(NamedTokenSet.Pattern, SyntacticContext.Expr.OtherExpr, Some("Valid par-yield syntax looks like `par (x <- e) yield x` "), loc = tree.loc)
        sctx.errors.add(error)
        Expr.Error(error)
      } else {
        Expr.ParYield(fragments.map(visitParYieldFragment), pickExpr(tree), tree.loc)
      }
    }

    private def visitParYieldFragment(tree: Tree)(implicit sctx: SharedContext): ParYieldFragment = {
      expect(tree, TreeKind.Expr.ParYieldFragment)
      val pat = Patterns.pickPattern(tree)
      ParYieldFragment(Patterns.restrictToNonConstant(pat), pickExpr(tree), tree.loc)
    }

    private def visitFixpointConstraintSetExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.FixpointConstraintSet)
      val constraints = pickAll(TreeKind.Expr.FixpointConstraint, tree)
      Expr.FixpointConstraintSet(constraints.map(visitFixpointConstraint), tree.loc)
    }

    private def visitFixpointConstraint(tree: Tree)(implicit sctx: SharedContext): Constraint = {
      expect(tree, TreeKind.Expr.FixpointConstraint)
      val bodyItems = pickAll(TreeKind.Predicate.Body, tree)
      val head = Predicates.pickHead(tree)
      Constraint(head, bodyItems.map(Predicates.visitBody), tree.loc)
    }

    private def visitFixpointLambdaExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.FixpointLambda)
      val paramTrees = pickAllMulti(pick(TreeKind.Predicate.ParamList, tree), TreeKind.Predicate.ParamUntyped, TreeKind.Predicate.Param)
      val params = paramTrees.map(Predicates.visitParam)
      Expr.FixpointLambda(params, pickExpr(tree), tree.loc)
    }

    private def visitFixpointInjectExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.FixpointInject)
      val expTrees = pickAll(TreeKind.Expr.Expr, tree)
      val predAndArityTrees = pickAll(TreeKind.PredicateAndArity, tree)
      val exprs = expTrees.map(visitExpr)
      val predsAndArities = predAndArityTrees.map(visitPredicateAndArity)
      if (exprs.length != predsAndArities.length) {
        // Check for mismatched arity
        val error = MismatchedArity(exprs.length, predsAndArities.length, tree.loc)
        sctx.errors.add(error)
        WeededAst.Expr.Error(error)
      } else {
        Expr.FixpointInjectInto(exprs, predsAndArities, tree.loc)
      }
    }

    private def visitFixpointSolveExpr(tree: Tree, isPSolve: Boolean)(implicit sctx: SharedContext): Expr = {
      val expectedTree = if (isPSolve) TreeKind.Expr.FixpointSolveWithProvenance else TreeKind.Expr.FixpointSolveWithProject
      val solveMode = if (isPSolve) SolveMode.WithProvenance else SolveMode.Default
      expect(tree, expectedTree)
      val expressions = pickAll(TreeKind.Expr.Expr, tree)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      val exprs = expressions.map(visitExpr)
      val optPreds = if (idents.isEmpty) None else Some(idents.map(Name.mkPred))
      Expr.FixpointSolveWithProject(exprs, optPreds, solveMode, tree.loc)
    }

    private def visitFixpointQueryExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.FixpointQuery)
      val expressions = pickAll(TreeKind.Expr.Expr, tree).map(visitExpr)
      val selects = pickAll(TreeKind.Expr.Expr, pick(TreeKind.Expr.FixpointSelect, tree)).map(visitExpr)
      val froms = pickAll(TreeKind.Predicate.Atom, pick(TreeKind.Expr.FixpointFromFragment, tree)).map(Predicates.visitAtom)
      val where = tryPick(TreeKind.Expr.FixpointWhere, tree).map(pickExpr)
      val whereList = where.map(w => List(w)).getOrElse(List.empty)
      Expr.FixpointQueryWithSelect(expressions, selects, froms, whereList, tree.loc)
    }

    private def visitFixpointQueryWithProvenanceExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.FixpointQueryWithProvenance)
      val expressions = pickAll(TreeKind.Expr.Expr, tree).map(visitExpr)
      val select = Predicates.pickHead(pick(TreeKind.Expr.FixpointSelect, tree))
      val withh = pickAll(TreeKind.Ident, pick(TreeKind.Expr.FixpointWith, tree)).map(tokenToIdent)
      if (select.den == Denotation.Latticenal) {
        val error = IllegalLatticeProvenance(select.loc)
        sctx.errors.add(error)
      }
      Expr.FixpointQueryWithProvenance(expressions, select, withh.map(Name.mkPred), tree.loc)
    }

    /**
      * Returns the intrinsic expression corresponding to the given intrinsic.
      *
      *   - `args = None` is an unapplied intrinsic (e.g. `%%LINE%%`)
      *   - `args = Some(Nil)` is an applied intrinsic with zero arguments (e.g. `%%EXAMPLE%%()`)
      *   - `args = Some(Cons(.., ..))` is an applied intrinsic with some arguments (e.g. `%%VECTOR_LENGTH%%(v)`)
      */
    private def visitIntrinsic(tree: Tree, args: Option[List[Expr]], loc: SourceLocation)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Intrinsic)
      val intrinsic = text(tree).head.stripPrefix("%%").stripSuffix("%%")
      val res = (intrinsic, args) match {
        case ("ARRAY_LENGTH", Some(e1 :: Nil)) => Expr.ArrayLength(e1, loc)
        case ("ARRAY_LOAD", Some(e1 :: e2 :: Nil)) => Expr.ArrayLoad(e1, e2, loc)
        case ("ARRAY_NEW", Some(e1 :: e2 :: e3 :: Nil)) => Expr.ArrayNew(e1, e2, e3, loc)
        case ("ARRAY_STORE", Some(e1 :: e2 :: e3 :: Nil)) => Expr.ArrayStore(e1, e2, e3, loc)
        case ("BOOL_AND", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.BoolOp.And, e1, e2, loc)
        case ("BOOL_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.BoolOp.Eq, e1, e2, loc)
        case ("BOOL_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.BoolOp.Neq, e1, e2, loc)
        case ("BOOL_NOT", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.BoolOp.Not, e1, loc)
        case ("BOOL_OR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, loc)
        case ("CHANNEL_GET", Some(e1 :: Nil)) => Expr.GetChannel(e1, loc)
        case ("CHANNEL_NEW", Some(e :: Nil)) => Expr.NewChannel(e, loc)
        case ("CHANNEL_PUT", Some(e1 :: e2 :: Nil)) => Expr.PutChannel(e1, e2, loc)
        case ("CHAR_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Eq, e1, e2, loc)
        case ("CHAR_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Ge, e1, e2, loc)
        case ("CHAR_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Gt, e1, e2, loc)
        case ("CHAR_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Le, e1, e2, loc)
        case ("CHAR_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Lt, e1, e2, loc)
        case ("CHAR_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.CharOp.Neq, e1, e2, loc)
        case ("FILE", None) => Expr.Cst(Constant.Str(s"${loc.source.name}"), loc)
        case ("FLOAT32_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Add, e1, e2, loc)
        case ("FLOAT32_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Div, e1, e2, loc)
        case ("FLOAT32_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Eq, e1, e2, loc)
        case ("FLOAT32_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Exp, e1, e2, loc)
        case ("FLOAT32_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Ge, e1, e2, loc)
        case ("FLOAT32_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Gt, e1, e2, loc)
        case ("FLOAT32_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Le, e1, e2, loc)
        case ("FLOAT32_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Lt, e1, e2, loc)
        case ("FLOAT32_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Mul, e1, e2, loc)
        case ("FLOAT32_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Float32Op.Neg, e1, loc)
        case ("FLOAT32_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Neq, e1, e2, loc)
        case ("FLOAT32_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float32Op.Sub, e1, e2, loc)
        case ("FLOAT64_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Add, e1, e2, loc)
        case ("FLOAT64_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Div, e1, e2, loc)
        case ("FLOAT64_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Eq, e1, e2, loc)
        case ("FLOAT64_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Exp, e1, e2, loc)
        case ("FLOAT64_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Ge, e1, e2, loc)
        case ("FLOAT64_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Gt, e1, e2, loc)
        case ("FLOAT64_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Le, e1, e2, loc)
        case ("FLOAT64_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Lt, e1, e2, loc)
        case ("FLOAT64_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Mul, e1, e2, loc)
        case ("FLOAT64_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Float64Op.Neg, e1, loc)
        case ("FLOAT64_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Neq, e1, e2, loc)
        case ("FLOAT64_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Float64Op.Sub, e1, e2, loc)
        case ("INT16_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Add, e1, e2, loc)
        case ("INT16_AND", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.And, e1, e2, loc)
        case ("INT16_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Div, e1, e2, loc)
        case ("INT16_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Eq, e1, e2, loc)
        case ("INT16_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Exp, e1, e2, loc)
        case ("INT16_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Ge, e1, e2, loc)
        case ("INT16_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Gt, e1, e2, loc)
        case ("INT16_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Le, e1, e2, loc)
        case ("INT16_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Lt, e1, e2, loc)
        case ("INT16_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Mul, e1, e2, loc)
        case ("INT16_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int16Op.Neg, e1, loc)
        case ("INT16_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Neq, e1, e2, loc)
        case ("INT16_NOT", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int16Op.Not, e1, loc)
        case ("INT16_OR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Or, e1, e2, loc)
        case ("INT16_REM", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Rem, e1, e2, loc)
        case ("INT16_SHL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Shl, e1, e2, loc)
        case ("INT16_SHR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Shr, e1, e2, loc)
        case ("INT16_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Sub, e1, e2, loc)
        case ("INT16_XOR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int16Op.Xor, e1, e2, loc)
        case ("INT32_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Add, e1, e2, loc)
        case ("INT32_AND", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.And, e1, e2, loc)
        case ("INT32_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Div, e1, e2, loc)
        case ("INT32_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Eq, e1, e2, loc)
        case ("INT32_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Exp, e1, e2, loc)
        case ("INT32_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Ge, e1, e2, loc)
        case ("INT32_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Gt, e1, e2, loc)
        case ("INT32_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Le, e1, e2, loc)
        case ("INT32_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Lt, e1, e2, loc)
        case ("INT32_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Mul, e1, e2, loc)
        case ("INT32_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int32Op.Neg, e1, loc)
        case ("INT32_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Neq, e1, e2, loc)
        case ("INT32_NOT", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int32Op.Not, e1, loc)
        case ("INT32_OR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Or, e1, e2, loc)
        case ("INT32_REM", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Rem, e1, e2, loc)
        case ("INT32_SHL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Shl, e1, e2, loc)
        case ("INT32_SHR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Shr, e1, e2, loc)
        case ("INT32_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Sub, e1, e2, loc)
        case ("INT32_XOR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int32Op.Xor, e1, e2, loc)
        case ("INT64_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Add, e1, e2, loc)
        case ("INT64_AND", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.And, e1, e2, loc)
        case ("INT64_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Div, e1, e2, loc)
        case ("INT64_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Eq, e1, e2, loc)
        case ("INT64_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Exp, e1, e2, loc)
        case ("INT64_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Ge, e1, e2, loc)
        case ("INT64_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Gt, e1, e2, loc)
        case ("INT64_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Le, e1, e2, loc)
        case ("INT64_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Lt, e1, e2, loc)
        case ("INT64_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Mul, e1, e2, loc)
        case ("INT64_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int64Op.Neg, e1, loc)
        case ("INT64_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Neq, e1, e2, loc)
        case ("INT64_NOT", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int64Op.Not, e1, loc)
        case ("INT64_OR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Or, e1, e2, loc)
        case ("INT64_REM", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Rem, e1, e2, loc)
        case ("INT64_SHL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Shl, e1, e2, loc)
        case ("INT64_SHR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Shr, e1, e2, loc)
        case ("INT64_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Sub, e1, e2, loc)
        case ("INT64_XOR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int64Op.Xor, e1, e2, loc)
        case ("INT8_ADD", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Add, e1, e2, loc)
        case ("INT8_AND", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.And, e1, e2, loc)
        case ("INT8_DIV", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Div, e1, e2, loc)
        case ("INT8_EQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Eq, e1, e2, loc)
        case ("INT8_EXP", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Exp, e1, e2, loc)
        case ("INT8_GE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Ge, e1, e2, loc)
        case ("INT8_GT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Gt, e1, e2, loc)
        case ("INT8_LE", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Le, e1, e2, loc)
        case ("INT8_LT", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Lt, e1, e2, loc)
        case ("INT8_MUL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Mul, e1, e2, loc)
        case ("INT8_NEG", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int8Op.Neg, e1, loc)
        case ("INT8_NEQ", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Neq, e1, e2, loc)
        case ("INT8_NOT", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.Int8Op.Not, e1, loc)
        case ("INT8_OR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Or, e1, e2, loc)
        case ("INT8_REM", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Rem, e1, e2, loc)
        case ("INT8_SHL", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Shl, e1, e2, loc)
        case ("INT8_SHR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Shr, e1, e2, loc)
        case ("INT8_SUB", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Sub, e1, e2, loc)
        case ("INT8_XOR", Some(e1 :: e2 :: Nil)) => Expr.Binary(SemanticOp.Int8Op.Xor, e1, e2, loc)
        case ("LINE", None) => Expr.Cst(Constant.Str(s"${loc.startLine}"), loc)
        case ("REFLECT_EFF", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.ReflectOp.ReflectEff, e1, loc)
        case ("REFLECT_VALUE", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.ReflectOp.ReflectValue, e1, loc)
        case ("REFLECT_TYPE", Some(e1 :: Nil)) => Expr.Unary(SemanticOp.ReflectOp.ReflectType, e1, loc)
        case ("VECTOR_GET", Some(e1 :: e2 :: Nil)) => Expr.VectorLoad(e1, e2, loc)
        case ("VECTOR_LENGTH", Some(e1 :: Nil)) => Expr.VectorLength(e1, loc)
        case _ =>
          val error = UndefinedIntrinsic(loc)
          sctx.errors.add(error)
          Expr.Error(error)
      }
      res
    }
  }

  private object Patterns {
    def pickPattern(tree: Tree)(implicit sctx: SharedContext): Pattern = {
      visitPattern(pick(TreeKind.Pattern.Pattern, tree))
    }

    def pickExtPattern(tree: Tree)(implicit sctx: SharedContext): ExtPattern = {
      visitExtPattern(pick(TreeKind.Pattern.Pattern, tree))
    }

    def visitPattern(tree: Tree, seen: collection.mutable.Map[String, Name.Ident] = collection.mutable.Map.empty)(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Pattern)
      tree.children.headOption match {
        case Some(tree: Tree) => tree.kind match {
          case TreeKind.Pattern.Variable => visitVariablePat(tree, seen)
          case TreeKind.Pattern.Literal => visitLiteralPat(tree)
          case TreeKind.Pattern.Tag => visitTagPat(tree, seen)
          case TreeKind.Pattern.Tuple => visitTuplePat(tree, seen)
          case TreeKind.Pattern.Record => visitRecordPat(tree, seen)
          case TreeKind.Pattern.Unary => visitUnaryPat(tree)
          case TreeKind.Pattern.FCons => visitFConsPat(tree, seen)
          // Avoid double reporting errors by returning a success here
          case TreeKind.ErrorTree(_) => Pattern.Error(tree.loc)
          case _ =>
            val error = UnexpectedToken(NamedTokenSet.Pattern, actual = None, SyntacticContext.Unknown, loc = tree.loc)
            sctx.errors.add(error)
            Pattern.Error(tree.loc)
        }
        case _ => throw InternalCompilerException(s"Expected Pattern.Pattern to have tree child", tree.loc)
      }
    }

    /**
      * Attempts to verify that there are no constants in `pat`.
      *
      * Constants are reported as errors to `sctx` and replaced by `Pattern.Error`.
      */
    def restrictToNonConstant(pat: Pattern)(implicit sctx: SharedContext): Pattern = pat match {
      case Pattern.Cst(_, loc) =>
        sctx.errors.add(IllegalConstantPattern(loc))
        Pattern.Error(loc)
      case Pattern.Tag(qname, pats, loc) =>
        if (pats.isEmpty) {
          // Disallow `A.A`
          sctx.errors.add(IllegalConstantPattern(loc))
          Pattern.Error(loc)
        } else {
          Pattern.Tag(qname, pats.map(restrictToNonConstant), loc)
        }
      case Pattern.Tuple(pats, loc) =>
        Pattern.Tuple(pats.map(restrictToNonConstant), loc)
      case Pattern.Record(pats, rowPat, loc) =>
        val newPats = pats.map {
          innerPat => innerPat.copy(pat = innerPat.pat.map(restrictToNonConstant))
        }
        Pattern.Record(newPats, rowPat, loc)
      case _ => pat
    }

    private def visitExtPattern(tree: Tree, seen: collection.mutable.Map[String, Name.Ident] = collection.mutable.Map.empty)(implicit sctx: SharedContext): ExtPattern = {
      expect(tree, TreeKind.Pattern.Pattern)
      tree.children.headOption match {
        case Some(subtree: Tree) => subtree.kind match {
          case TreeKind.Pattern.Tag => visitExtTagPattern(subtree, seen)
          case TreeKind.Pattern.Variable => visitExtTagDefaultPattern(subtree)
          // Avoid double reporting errors by returning a success here
          case TreeKind.ErrorTree(_) => ExtPattern.Error(subtree.loc)
          case _ =>
            val error = IllegalExtPattern(subtree.loc)
            sctx.errors.add(error)
            ExtPattern.Error(subtree.loc)
        }
        case _ => throw InternalCompilerException(s"Expected Pattern.Pattern to have tree child", tree.loc)
      }
    }

    private def visitExtTagPattern(tree: SyntaxTree.Tree, seen: mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): ExtPattern = {
      expect(tree, TreeKind.Pattern.Tag)
      val maybePat = tryPick(TreeKind.Pattern.TagBody, tree)
      val qname = pickQName(tree)
      if (!qname.isUnqualified) {
        val error = IllegalQualifiedExtPattern(qname)
        sctx.errors.add(error)
      }
      maybePat.map(visitExtTagTermsPat(_, seen)) match {
        case None => ExtPattern.Tag(Name.mkLabel(qname.ident), List.empty, tree.loc)
        case Some(elms) => ExtPattern.Tag(Name.mkLabel(qname.ident), elms, tree.loc)
      }
    }

    private def visitExtTagDefaultPattern(tree: SyntaxTree.Tree)(implicit sctx: SharedContext): ExtPattern = {
      expect(tree, TreeKind.Pattern.Variable)
      pickNameIdent(tree) match {
        case ident if ident.name == "_" =>
          ExtPattern.Default(tree.loc)

        case ident =>
          val error = IllegalExtPattern(ident.loc)
          sctx.errors.add(error)
          ExtPattern.Error(tree.loc)
      }
    }

    private def visitVariablePat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Variable)
      val ident = pickNameIdent(tree)
      if (ident.name == "_")
        Pattern.Wild(tree.loc)
      else {
        seen.get(ident.name) match {
          case Some(other) =>
            val error = NonLinearPattern(ident.name, other.loc, tree.loc)
            sctx.errors.add(error)
            Pattern.Var(ident, tree.loc)
          case None =>
            seen += (ident.name -> ident)
            Pattern.Var(ident, tree.loc)
        }
      }
    }

    private def visitLiteralPat(tree: Tree)(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Literal)
      Exprs.visitLiteralExpr(tree) match {
        case Expr.Cst(cst, _) => cst match {
          case Constant.Null =>
            val error = IllegalNullPattern(tree.loc)
            sctx.errors.add(error)
            WeededAst.Pattern.Error(tree.loc)
          case Constant.Regex(_) =>
            val error = IllegalRegexPattern(tree.loc)
            sctx.errors.add(error)
            WeededAst.Pattern.Error(tree.loc)
          case Constant.BigDecimal(_) =>
            val error = IllegalBigDecimalPattern(tree.loc)
            sctx.errors.add(error)
            WeededAst.Pattern.Error(tree.loc)
          case c =>
            Pattern.Cst(c, tree.loc)
        }
        // Avoid double reporting errors
        case Expr.Error(_) => Pattern.Error(tree.loc)
        case e => throw InternalCompilerException(s"Malformed Pattern.Literal. Expected literal but found $e", e.loc)
      }
    }

    private def visitTagPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Tag)
      val maybePat = tryPick(TreeKind.Pattern.TagBody, tree)
      val qname = pickQName(tree)
      maybePat.map(visitTagTermsPat(_, seen)) match {
        case None => Pattern.Tag(qname, Nil, tree.loc)
        case Some(elms) => Pattern.Tag(qname, elms.toList, tree.loc)
      }
    }

    /** Extracts a tag body pattern as a list, expanding `()` to be `List(Unit)`. */
    private def visitTagTermsPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Nel[Pattern] = {
      expect(tree, TreeKind.Pattern.TagBody)
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      patterns.map(visitPattern(_, seen)) match {
        case Nil => Nel(Pattern.Cst(Constant.Unit, tree.loc), Nil)
        case x :: xs => Nel(x, xs)
      }
    }

    private def visitExtTagTermsPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): List[ExtTagPattern] = {
      expect(tree, TreeKind.Pattern.TagBody)
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      patterns.map(visitPattern(_, seen)) match {
        case Nil => List(ExtTagPattern.Unit(tree.loc))
        case xs => xs.map(restrictToVarOrWild)
      }
    }

    private def restrictToVarOrWild(pat: Pattern)(implicit sctx: SharedContext): ExtTagPattern = pat match {
      case Pattern.Wild(loc) => ExtTagPattern.Wild(loc)
      case Pattern.Var(ident, loc) => ExtTagPattern.Var(ident, loc)
      case _ =>
        val error = WeederError.IllegalExtPattern(pat.loc)
        sctx.errors.add(error)
        ExtTagPattern.Error(pat.loc)
    }

    private def visitTuplePat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Tuple)
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      patterns.map(visitPattern(_, seen)) match {
        case Nil => Pattern.Cst(Constant.Unit, tree.loc)
        case x :: Nil => x
        case x :: xs => Pattern.Tuple(Nel(x, xs), tree.loc)
      }
    }

    private def visitRecordPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Record)
      val fields = pickAll(TreeKind.Pattern.RecordFieldFragment, tree)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree)
      (fields.map(visitRecordField(_, seen)), maybePattern.map(visitPattern(_, seen))) match {

        // Pattern { ... }
        case (fs, None) =>
          Pattern.Record(fs, Pattern.Cst(Constant.RecordEmpty, tree.loc.asSynthetic), tree.loc)

        // Pattern { x, ... | r }
        case (x :: xs, Some(Pattern.Var(v, l))) =>
          Pattern.Record(x :: xs, Pattern.Var(v, l), tree.loc)

        // Pattern { x, ... | _ }
        case (x :: xs, Some(Pattern.Wild(l))) =>
          Pattern.Record(x :: xs, Pattern.Wild(l), tree.loc)

        // Illegal pattern: { | r }
        case (Nil, Some(r)) =>
          val error = EmptyRecordExtensionPattern(r.loc)
          sctx.errors.add(error)
          Pattern.Error(r.loc)

        // Illegal pattern: { x, ... | (1, 2, 3) }
        case (_, Some(r)) =>
          val error = IllegalRecordExtensionPattern(r.loc)
          sctx.errors.add(error)
          Pattern.Error(r.loc)
      }
    }

    private def visitRecordField(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern.Record.RecordLabelPattern = {
      expect(tree, TreeKind.Pattern.RecordFieldFragment)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree)
      val ident = pickNameIdent(tree)
      maybePattern.map(visitPattern(_, seen)) match {
        case None =>
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc)
            case Some(other) =>
              val error = NonLinearPattern(ident.name, other.loc, ident.loc)
              sctx.errors.add(error)
              Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc)
          }
        case pat =>
          Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), pat, tree.loc)
      }
    }

    private def visitUnaryPat(tree: Tree)(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.Unary)
      val NumberLiteralKinds = List(TokenKind.LiteralInt, TokenKind.LiteralInt8, TokenKind.LiteralInt16, TokenKind.LiteralInt32, TokenKind.LiteralInt64, TokenKind.LiteralBigInt, TokenKind.LiteralFloat, TokenKind.LiteralFloat32, TokenKind.LiteralFloat64, TokenKind.LiteralBigDecimal)
      val literalToken = ArrayOps.getOption(tree.children, 1) match {
        case Some(t@Token(_, _, _, _, _, _)) if NumberLiteralKinds.contains(t.kind) => Some(t)
        case _ => None
      }
      pick(TreeKind.Operator, tree).children(0) match {
        case opToken@Token(_, _, _, _, _, _) =>
          literalToken match {
            // fold unary minus into a constant, and visit it like any other constant
            case Some(lit) if opToken.text == "-" =>
              // Construct a synthetic literal tree with the unary minus and visit that like any other literal expression
              val syntheticToken = Token(lit.kind, lit.src, opToken.startIndex, lit.endIndex, lit.start, lit.end)
              val syntheticLiteral = Tree(TreeKind.Pattern.Literal, Array(syntheticToken), tree.loc.asSynthetic)
              visitLiteralPat(syntheticLiteral)
            case _ =>
              sctx.errors.add(WeederError.MalformedInt(tree.loc))
              Pattern.Error(tree.loc)
          }
        case _ => throw InternalCompilerException(s"Expected unary operator but found tree", tree.loc)
      }
    }

    private def visitFConsPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Pattern = {
      expect(tree, TreeKind.Pattern.FCons)
      // FCons are rewritten into tag patterns
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      patterns.map(visitPattern(_, seen)) match {
        case pat1 :: pat2 :: Nil =>
          val qname = Name.mkQName("List.Cons", tree.loc)
          Pattern.Tag(qname, List(pat1, pat2), tree.loc)
        case pats => throw InternalCompilerException(s"Pattern.FCons expected 2 but found '${pats.length}' sub-patterns", tree.loc)
      }
    }
  }

  private object Constants {
    private def tryParseFloat(token: Token, after: (String, SourceLocation) => Expr)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      try {
        after(token.text.filterNot(_ == '_'), loc)
      } catch {
        case _: NumberFormatException =>
          val error = MalformedFloat(loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    private def tryParseInt(token: Token, suffix: String, after: (Int, String, SourceLocation) => Expr)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      try {
        val radix = if (token.text.startsWith("0x")) 16 else 10
        val digits = token.text.stripPrefix("0x").stripSuffix(suffix).filterNot(_ == '_')
        after(radix, digits, loc)
      } catch {
        case _: NumberFormatException =>
          val error = MalformedInt(loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    /**
      * Attempts to parse the given tree to a float32.
      */
    def toFloat32(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token, (text, loc) => {
        val value = java.lang.Float.parseFloat(text.stripSuffix("f32"))
        if (java.lang.Float.isInfinite(value)) {
          throw new NumberFormatException()
        }
        Expr.Cst(Constant.Float32(value), loc)
      })

    /**
      * Attempts to parse the given tree to a float64.
      */
    def toFloat64(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token, (text, loc) => {
        val value = java.lang.Double.parseDouble(text.stripSuffix("f64"))
        if (java.lang.Double.isInfinite(value)) {
          throw new NumberFormatException()
        }
        Expr.Cst(Constant.Float64(value), loc)
      })

    /**
      * Attempts to parse the given tree to a big decimal.
      */
    def toBigDecimal(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token, (text, loc) => {
        val bigDecimal = new java.math.BigDecimal(text.stripSuffix("ff"))
        Expr.Cst(Constant.BigDecimal(bigDecimal), loc)
      })

    /**
      * Attempts to parse the given tree to a int8.
      */
    def toInt8(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "i8", (radix, digits, loc) =>
        Expr.Cst(Constant.Int8(JByte.parseByte(digits, radix)), loc)
      )

    /**
      * Attempts to parse the given tree to a int16.
      */
    def toInt16(token: Token)(implicit sctx: SharedContext): Expr = {
      tryParseInt(token, "i16", (radix, digits, loc) =>
        Expr.Cst(Constant.Int16(JShort.parseShort(digits, radix)), loc)
      )
    }

    /**
      * Attempts to parse the given tree to a int32.
      */
    def toInt32(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "i32", (radix, digits, loc) =>
        Expr.Cst(Constant.Int32(JInt.parseInt(digits, radix)), loc)
      )

    /**
      * Attempts to parse the given tree to a int64.
      */
    def toInt64(token: Token)(implicit sctx: SharedContext): Expr = {
      tryParseInt(token, "i64", (radix, digits, loc) =>
        Expr.Cst(Constant.Int64(JLong.parseLong(digits, radix)), loc)
      )
    }

    /**
      * Attempts to parse the given tree to a int64.
      */
    def toBigInt(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "ii", (radix, digits, loc) =>
        Expr.Cst(Constant.BigInt(new java.math.BigInteger(digits, radix)), loc)
      )

    /**
      * Attempts to compile the given regular expression into a Pattern.
      */
    def toRegex(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("regex\"").stripSuffix("\"")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      try {
        val pattern = JPattern.compile(processed)
        Expr.Cst(Constant.Regex(pattern), loc)
      } catch {
        case ex: PatternSyntaxException =>
          val error = MalformedRegex(token.text, ex.getMessage, loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    def visitChars(str: String, loc: SourceLocation): (String, List[CompilationMessage]) = {
      @tailrec
      def visit(chars: List[Char], acc: List[Char], accErr: List[CompilationMessage]): (String, List[CompilationMessage]) = {
        chars match {
          // Case 1: End of the sequence
          case Nil => (acc.reverse.mkString, accErr)
          // Case 2: Escaped character literal
          case esc :: c0 :: rest if esc == '\\' =>
            c0 match {
              case 'n' => visit(rest, '\n' :: acc, accErr)
              case 'r' => visit(rest, '\r' :: acc, accErr)
              case '\\' => visit(rest, '\\' :: acc, accErr)
              case '\"' => visit(rest, '\"' :: acc, accErr)
              case '\'' => visit(rest, '\'' :: acc, accErr)
              case 't' => visit(rest, '\t' :: acc, accErr)
              // Special flix escapes for string interpolations
              case '$' => visit(rest, '$' :: acc, accErr)
              case '%' => visit(rest, '%' :: acc, accErr)
              // Case unicode escape "\u1234".
              case 'u' => rest match {
                case d1 :: d2 :: d3 :: d4 :: rest2 =>
                  // Doing manual flatMap here to keep recursive call in tail-position
                  visitHex(d1, d2, d3, d4) match {
                    case Result.Ok(c) => visit(rest2, c :: acc, accErr)
                    case Result.Err(error) => visit(rest2, d1 :: d2 :: d3 :: d4 :: acc, error :: accErr)
                  }
                // less than 4 chars were left in the string
                case rest2 =>
                  val malformedCode = rest2.takeWhile(_ != '\\').mkString("")
                  val err = MalformedUnicodeEscapeSequence(malformedCode, loc)
                  visit(rest2, malformedCode.toList ++ acc, err :: accErr)
              }
              case c => visit(rest, c :: acc, IllegalEscapeSequence(c, loc) :: accErr)
            }
          // Case 2: Simple character literal
          case c :: rest => visit(rest, c :: acc, accErr)
        }
      }

      def visitHex(d1: Char, d2: Char, d3: Char, d4: Char): Result[Char, CompilationMessage] = {
        try {
          Result.Ok(Integer.parseInt(s"$d1$d2$d3$d4", 16).toChar)
        } catch {
          // Case: the four characters following "\u" does not make up a number
          case _: NumberFormatException => Result.Err(MalformedUnicodeEscapeSequence(s"$d1$d2$d3$d4", loc))
        }
      }

      visit(str.toList, Nil, Nil)
    }

    def toChar(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("\'").stripSuffix("\'")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      if (processed.length != 1) {
        val error = MalformedChar(processed, loc)
        sctx.errors.add(error)
        Expr.Error(error)
      } else {
        Expr.Cst(Constant.Char(processed.head), loc)
      }
    }

    def toStringCst(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("\"").stripSuffix("\"")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      Expr.Cst(Constant.Str(processed), loc)
    }
  }

  private object Predicates {
    def pickHead(tree: Tree)(implicit sctx: SharedContext): Predicate.Head.Atom = {
      val headTree = pick(TreeKind.Predicate.Head, tree)
      val maybeTermList = tryPick(TreeKind.Predicate.TermList, headTree)
      val ident = pickNameIdent(headTree)
      maybeTermList.map(visitTermList) match {
        case None =>
          Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, Nil, headTree.loc)
        case Some((exprs, None)) =>
          Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, exprs, headTree.loc)
        case Some((exprs, Some(term))) =>
          Predicate.Head.Atom(Name.mkPred(ident), Denotation.Latticenal, exprs ::: term :: Nil, headTree.loc)
      }
    }

    private def visitTermList(tree: Tree)(implicit sctx: SharedContext): (List[Expr], Option[Expr]) = {
      val rawList = pickAll(TreeKind.Expr.Expr, tree).map(Exprs.visitExpr)
      val maybeLatTerm = tryPickLatticeTermExpr(tree)
      rawList match {
        // A() => A(())
        case Nil => (List(Expr.Cst(Constant.Unit, tree.loc)), maybeLatTerm)
        case nonEmpty => (nonEmpty, maybeLatTerm)
      }
    }

    def visitBody(parentTree: Tree)(implicit sctx: SharedContext): Predicate.Body = {
      assert(parentTree.kind == TreeKind.Predicate.Body)
      val tree = unfold(parentTree)
      tree.kind match {
        case TreeKind.Predicate.Atom => visitAtom(tree)
        case TreeKind.Predicate.Guard => visitGuard(tree)
        case TreeKind.Predicate.Functional => visitFunctional(tree)
        case kind => throw InternalCompilerException(s"expected predicate body but found '$kind'", tree.loc)
      }
    }

    def visitAtom(tree: Tree)(implicit sctx: SharedContext): Predicate.Body.Atom = {
      expect(tree, TreeKind.Predicate.Atom)
      val fixity = if (hasToken(TokenKind.KeywordFix, tree)) Fixity.Fixed else Fixity.Loose
      val polarity = if (hasToken(TokenKind.KeywordNot, tree)) Polarity.Negative else Polarity.Positive
      val maybePatList = tryPick(TreeKind.Predicate.PatternList, tree)
      val ident = pickNameIdent(tree)
      maybePatList.map(visitPatternList) match {
        case None =>
          Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, Nil, tree.loc)
        case Some((pats, None)) =>
          // Check for `[[IllegalFixedAtom]]`.
          val isNegativePolarity = polarity == Polarity.Negative
          val isFixedFixity = fixity == Fixity.Fixed
          val isIllegalFixedAtom = isNegativePolarity && isFixedFixity
          if (isIllegalFixedAtom) {
            val error = IllegalFixedAtom(tree.loc)
            sctx.errors.add(error)
          }
          Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, pats, tree.loc)
        case Some((pats, Some(term))) =>
          Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, fixity, pats ::: term :: Nil, tree.loc)
      }
    }

    private def visitPatternList(tree: Tree)(implicit sctx: SharedContext): (List[Pattern], Option[Pattern]) = {
      val rawList = pickAll(TreeKind.Pattern.Pattern, tree).map(t => Patterns.visitPattern(t))
      val maybeLatTerm = tryPickLatticeTermPattern(tree)
      rawList match {
        // A() => A(())
        case Nil => (List(Pattern.Cst(Constant.Unit, tree.loc)), maybeLatTerm)
        case nonEmpty => (nonEmpty, maybeLatTerm)
      }
    }

    private def visitGuard(tree: Tree)(implicit sctx: SharedContext): Predicate.Body.Guard = {
      expect(tree, TreeKind.Predicate.Guard)
      Predicate.Body.Guard(Exprs.pickExpr(tree), tree.loc)
    }

    private def visitFunctional(tree: Tree)(implicit sctx: SharedContext): Predicate.Body.Functional = {
      expect(tree, TreeKind.Predicate.Functional)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      Predicate.Body.Functional(idents, Exprs.pickExpr(tree), tree.loc)
    }

    def visitParam(tree: Tree)(implicit sctx: SharedContext): PredicateParam = {
      expectAny(tree, List(TreeKind.Predicate.Param, TreeKind.Predicate.ParamUntyped))
      val ident = pickNameIdent(tree)
      val tps = pickAll(TreeKind.Type.Type, tree).map(Types.visitType)
      val maybeLatTerm = tryPickLatticeTermType(tree)
      (tps, maybeLatTerm) match {
        case (Nil, _) => PredicateParam.PredicateParamUntyped(Name.mkPred(ident), tree.loc)
        case (types, None) => PredicateParam.PredicateParamWithType(Name.mkPred(ident), Denotation.Relational, types, tree.loc)
        case (types, Some(latTerm)) => PredicateParam.PredicateParamWithType(Name.mkPred(ident), Denotation.Latticenal, types :+ latTerm, tree.loc)
      }
    }

    private def tryPickLatticeTermExpr(tree: Tree)(implicit sctx: SharedContext): Option[Expr] = {
      tryPick(TreeKind.Predicate.LatticeTerm, tree).map(Exprs.pickExpr)
    }

    private def tryPickLatticeTermType(tree: Tree)(implicit sctx: SharedContext): Option[Type] = {
      tryPick(TreeKind.Predicate.LatticeTerm, tree).map(Types.pickType)
    }

    private def tryPickLatticeTermPattern(tree: Tree)(implicit sctx: SharedContext): Option[Pattern] = {
      tryPick(TreeKind.Predicate.LatticeTerm, tree).map(Patterns.pickPattern)
    }

  }

  private object Types {
    def pickType(tree: Tree)(implicit sctx: SharedContext): Type = {
      // Fall back on Type.Error if there is no type. The parser will have reported an error.
      tryPick(TreeKind.Type.Type, tree).map(visitType).getOrElse(Type.Error(tree.loc))
    }

    def tryPickTypeNoWild(tree: Tree)(implicit sctx: SharedContext): Option[Type] = {
      tryPickType(tree) match {
        case Some(Type.Var(ident, _)) if ident.isWild => None
        case t => t
      }
    }

    def tryPickType(tree: Tree)(implicit sctx: SharedContext): Option[Type] = {
      tryPick(TreeKind.Type.Type, tree).map(visitType)
    }

    def tryPickEffect(tree: Tree)(implicit sctx: SharedContext): Option[Type] = {
      val maybeEffect = tryPick(TreeKind.Type.Effect, tree)
      maybeEffect.map(pickType)
    }

    def visitType(tree: Tree)(implicit sctx: SharedContext): WeededAst.Type = {
      expectAny(tree, List(TreeKind.Type.Type, TreeKind.Type.Effect))
      // Visit first child and match its kind to know what to to
      val inner = unfold(tree)
      inner.kind match {
        case TreeKind.QName => visitNameType(inner)
        case TreeKind.Ident => visitIdentType(inner)
        case TreeKind.Type.Tuple => visitTupleType(inner)
        case TreeKind.Type.Record => visitRecordType(inner)
        case TreeKind.Type.RecordRow => visitRecordRowType(inner)
        case TreeKind.Type.Schema => visitSchemaType(inner)
        case TreeKind.Type.Extensible => visitExtensibleType(inner)
        case TreeKind.Type.SchemaRow => visitSchemaRowType(inner)
        case TreeKind.Type.Apply => visitApplyType(inner)
        case TreeKind.Type.Constant => visitConstantType(inner)
        case TreeKind.Type.Unary => visitUnaryType(inner)
        case TreeKind.Type.Binary => visitBinaryType(inner)
        case TreeKind.Type.CaseSet => visitCaseSetType(inner)
        case TreeKind.Type.EffectSet => visitEffectType(inner)
        case TreeKind.Type.Ascribe => visitAscribeType(inner)
        case TreeKind.Type.Variable => visitVariableType(inner)
        case TreeKind.ErrorTree(_) => Type.Error(tree.loc)
        case kind => throw InternalCompilerException(s"Parser passed unknown type '$kind'", tree.loc)
      }
    }

    /**
      * This is a customized version of [[visitType]] to avoid parsing `case Case((a, b))` as
      * `case Case(a, b)`.
      *
      *   - `Tuple() --> Nil`
      *   - `Tuple(t) --> List(visitType(t))`
      *   - `t --> List(visitType(t))`
      */
    def visitCaseType(tree: Tree)(implicit sctx: SharedContext): List[Type] = {
      expect(tree, TreeKind.CaseBody)
      val innerTypes = pickAll(TreeKind.Type.Type, tree)
      innerTypes.map(visitType) match {
        case Nil =>
          sctx.errors.add(NeedAtleastOne(NamedTokenSet.Type, SyntacticContext.Decl.Enum, loc = tree.loc))
          List(Type.Error(tree.loc))
        case types => types
      }
    }

    private def visitNameType(tree: Tree)(implicit sctx: SharedContext): Type.Ambiguous = {
      val qname = visitQName(tree)
      Type.Ambiguous(qname, qname.loc)
    }

    private def visitIdentType(tree: Tree)(implicit sctx: SharedContext): Type = {
      val ident = tokenToIdent(tree)
      if (ident.isWild)
        Type.Var(ident, tree.loc)
      else
        Type.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc)
    }

    private def visitTupleType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Tuple)
      pickAll(TreeKind.Type.Type, tree).map(visitType) match {
        case tpe :: Nil => tpe // flatten singleton tuple types
        case tpe :: types => Type.Tuple(Nel(tpe, types), tree.loc)
        case Nil =>
          val error = WeederError.IllegalEmptyTupleType(tree.loc)
          sctx.errors.add(error)
          Type.Error(tree.loc)
      }
    }

    private def visitRecordType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Record)
      Type.Record(visitRecordRowType(tree), tree.loc)
    }

    private def visitRecordRowType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expectAny(tree, List(TreeKind.Type.Record, TreeKind.Type.RecordRow))
      val maybeType = tryPick(TreeKind.Type.Type, tree).map(visitType)
      val fields = pickAll(TreeKind.Type.RecordFieldFragment, tree).map(visitRecordField)
      val tail = maybeType.getOrElse(Type.RecordRowEmpty(tree.loc))
      fields.foldRight(tail) { case ((label, tpe), acc) => Type.RecordRowExtend(label, tpe, acc, tree.loc) }
    }

    private def visitRecordField(tree: Tree)(implicit sctx: SharedContext): (Name.Label, Type) = {
      expect(tree, TreeKind.Type.RecordFieldFragment)
      val ident = pickNameIdent(tree)
      (Name.mkLabel(ident), pickType(tree))
    }

    private def visitSchemaType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Schema)
      Type.Schema(visitSchemaRowType(tree), tree.loc)
    }

    private def visitExtensibleType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Extensible)
      Type.Extensible(visitSchemaRowType(tree), tree.loc)
    }

    private def visitSchemaRowType(parentTree: Tree)(implicit sctx: SharedContext): Type = {
      val rest: Type = tryPick(TreeKind.Ident, parentTree).map(tokenToIdent) match {
        case None => WeededAst.Type.SchemaRowEmpty(parentTree.loc)
        case Some(name) => WeededAst.Type.Var(name, name.loc)
      }
      pickAllTrees(parentTree).foldRight(rest) {
        case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithAlias =>
          val qname = pickQName(parentTree)
          val targs = Types.pickArguments(tree)
          Type.SchemaRowExtendByAlias(qname, targs, acc, tree.loc)

        case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithTypes =>
          val maybeLatTerm = tryPick(TreeKind.Predicate.LatticeTerm, tree)
          val qname = pickQName(tree)
          val tps = pickAll(TreeKind.Type.Type, tree).map(Types.visitType)
          maybeLatTerm.map(Types.pickType) match {
            case None => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Relational, tps, acc, tree.loc)
            case Some(t) => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Latticenal, tps :+ t, acc, tree.loc)
          }

        case (_, acc) => acc
      }
    }

    private def visitApplyType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Apply)
      val argsTree = pick(TreeKind.Type.ArgumentList, tree)
      val tpe = pickType(tree)
      // Curry type arguments
      val arguments = pickAll(TreeKind.Type.Argument, argsTree).map(pickType)
      arguments.foldLeft(tpe) { case (acc, t2) => Type.Apply(acc, t2, tree.loc) }
    }

    private def visitConstantType(tree: Tree): Type = {
      expect(tree, TreeKind.Type.Constant)
      text(tree).head match {
        case "false" => Type.False(tree.loc)
        case "true" => Type.True(tree.loc)
        // TODO EFF-MIGRATION create dedicated Impure type
        case "Univ" => Type.Complement(Type.Pure(tree.loc), tree.loc)
        case other => throw InternalCompilerException(s"'$other' used as Type.Constant ${tree.loc}", tree.loc)
      }
    }

    private def visitUnaryType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Unary)
      val types = pickAll(TreeKind.Type.Type, tree).map(visitType)
      val op = pick(TreeKind.Operator, tree)
      types match {
        case t :: Nil =>
          text(op).head match {
            case "~" => Type.Complement(t, tree.loc)
            case "rvnot" => Type.CaseComplement(t, tree.loc)
            case "not" => Type.Not(t, tree.loc)
            // UNRECOGNIZED
            case kind => throw InternalCompilerException(s"Parser passed unknown type operator '$kind'", tree.loc)
          }
        case operands => throw InternalCompilerException(s"Type.Unary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitBinaryType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Binary)
      val types = pickAll(TreeKind.Type.Type, tree).map(visitType)
      val op = pick(TreeKind.Operator, tree)
      types match {
        case t1 :: t2 :: Nil =>
          text(op).head match {
            // ARROW FUNCTIONS
            case "->" =>
              val eff = tryPickEffect(tree)
              val params = t1 match {
                // Normally singleton tuples `((a, b))` are treated as `(a, b)`. That's fine unless we are doing an arrow type!
                // In this case we need t1 "unflattened" so we redo the visit.
                case Type.Tuple(_, _) =>
                  val t1Tree = pick(TreeKind.Type.Tuple, pick(TreeKind.Type.Type, tree))
                  pickAll(TreeKind.Type.Type, t1Tree).map(visitType)
                case t => List(t)
              }
              mkCurriedArrow(params, eff, t2, tree.loc)
            // REGULAR TYPE OPERATORS
            case "+" => Type.Union(t1, t2, tree.loc)
            case "-" => Type.Difference(t1, t2, tree.loc)
            case "&" => Type.Intersection(t1, t2, tree.loc)
            case "and" => Type.And(t1, t2, tree.loc)
            case "or" => Type.Or(t1, t2, tree.loc)
            case "rvadd" => Type.CaseUnion(t1, t2, tree.loc)
            case "rvand" => Type.CaseIntersection(t1, t2, tree.loc)
            case "rvsub" => Type.CaseIntersection(t1, Type.CaseComplement(t2, tree.loc.asSynthetic), tree.loc)
            case "xor" => Type.Or(
              Type.And(t1, Type.Not(t2, tree.loc), tree.loc),
              Type.And(Type.Not(t1, tree.loc), t2, tree.loc),
              tree.loc
            )
            // UNRECOGNIZED
            case kind =>
              sctx.errors.add(WeederError.UnexpectedBinaryTypeOperator(kind, tree.loc))
              Type.Error(tree.loc)
          }

        case operands => throw InternalCompilerException(s"Type.Binary tree with ${operands.length} operands: $operands", tree.loc)
      }
    }

    /**
      * Returns the curried arrow type formed from the parameter types `params`, effect `eff`, and result
      * type `tresult`.
      *
      * A multi-parameter arrow `(a, b) -> c` is curried into nested single-parameter arrows `a -> (b -> c)`.
      * The outermost arrow is given the real location `loc` since it corresponds to the source arrow type,
      * whereas the inner arrows introduced by currying are synthetic since they do not appear directly in the
      * source. The innermost arrow carries the effect `eff`.
      */
    private def mkCurriedArrow(params: List[Type], eff: Option[Type], tresult: Type, loc: SourceLocation): Type = {
      val synthLoc = loc.asSynthetic
      params match {
        case Nil =>
          // A parameterless arrow cannot be written in source.
          tresult
        case onlyParam :: Nil =>
          Type.Arrow(List(onlyParam), eff, tresult, loc)
        case firstParam :: restParams =>
          val innermost = Type.Arrow(List(restParams.last), eff, tresult, synthLoc)
          val inner = restParams.init.foldRight(innermost: Type)((param, acc) => Type.Arrow(List(param), None, acc, synthLoc))
          Type.Arrow(List(firstParam), None, inner, loc)
      }
    }

    private def visitCaseSetType(tree: Tree)(implicit sctx: SharedContext): Type.CaseSet = {
      expect(tree, TreeKind.Type.CaseSet)
      val cases = pickAll(TreeKind.QName, tree).map(visitQName)
      Type.CaseSet(cases, tree.loc)
    }

    private def visitEffectType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.EffectSet)
      pickAll(TreeKind.Type.Type, tree).map(visitType) match {
        // Default to Pure
        case Nil => Type.Pure(tree.loc)
        // Otherwise reduce effects into a union type
        case effects => effects.reduceLeft({
          case (acc, tpe) => Type.Union(acc, tpe, tree.loc)
        }: (Type, Type) => Type)
      }
    }

    private def visitAscribeType(tree: Tree)(implicit sctx: SharedContext): Type = {
      expect(tree, TreeKind.Type.Ascribe)
      val kind = pickKind(tree)
      Type.Ascribe(pickType(tree), kind, tree.loc)
    }

    private def visitVariableType(tree: Tree)(implicit sctx: SharedContext): Type.Var = {
      expect(tree, TreeKind.Type.Variable)
      val ident = tokenToIdent(tree)
      Type.Var(ident, tree.loc)
    }

    def pickArguments(tree: Tree)(implicit sctx: SharedContext): List[Type] = {
      tryPick(TreeKind.Type.ArgumentList, tree)
        .map(argTree => pickAll(TreeKind.Type.Argument, argTree).map(pickType))
        .getOrElse(List.empty)
    }

    def pickDerivations(tree: Tree)(implicit sctx: SharedContext): Derivations = {
      val maybeDerivations = tryPick(TreeKind.DerivationList, tree)
      val loc = maybeDerivations.map(_.loc).getOrElse(SourceLocation.Unknown)
      val derivations = maybeDerivations.toList.flatMap {
        tree => pickAll(TreeKind.QName, tree).map(visitQName)
      }
      Derivations(derivations, loc)
    }

    def pickParameters(tree: Tree)(implicit sctx: SharedContext): List[TypeParam] = {
      tryPick(TreeKind.TypeParameterList, tree) match {
        case None => Nil
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree)
          val tparams = parameters.map(visitParameter)
          val kinded = tparams.collect { case t: TypeParam.Kinded => t }
          val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
          (kinded, unkinded) match {
            // Only unkinded type parameters
            case (Nil, _ :: _) => tparams
            // Only kinded type parameters
            case (_ :: _, Nil) => tparams
            // Some kinded and some unkinded type parameters. Give an error and keep going.
            case (_ :: _, _ :: _) =>
              val error = MismatchedKindAnnotations(tparamsTree.loc)
              sctx.errors.add(error)
              tparams
            // No type parameters. Issue an error and return an empty list.
            case (Nil, Nil) =>
              val error = NeedAtleastOne(NamedTokenSet.Parameter, SyntacticContext.Decl.Type, None, tparamsTree.loc)
              sctx.errors.add(error)
              Nil
          }
      }
    }

    def pickKindedParameters(tree: Tree)(implicit sctx: SharedContext): List[TypeParam] = {
      tryPick(TreeKind.TypeParameterList, tree) match {
        case None => Nil
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree)
          val tparams = parameters.map(visitParameter)
          val kinded = tparams.collect { case t: TypeParam.Kinded => t }
          val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
          (kinded, unkinded) match {
            // Only kinded type parameters
            case (_ :: _, Nil) => tparams
            // Some kinded and some unkinded type parameters. We recover by kinding the unkinded ones as Ambiguous.
            case (_, _ :: _) =>
              unkinded.foreach(t => sctx.errors.add(MissingKindAscription(t.ident.loc)))
              tparams
            // Empty list. Syntax error, but recover with an empty list.
            case (Nil, Nil) =>
              sctx.errors.add(EmptyTypeParamList(tparamsTree.loc))
              tparams
          }
      }
    }

    def pickSingleParameter(tree: Tree)(implicit sctx: SharedContext): TypeParam = {
      val tparams = pick(TreeKind.TypeParameterList, tree)
      visitParameter(pick(TreeKind.Parameter, tparams))
    }

    def visitParameter(tree: Tree)(implicit sctx: SharedContext): TypeParam = {
      expect(tree, TreeKind.Parameter)
      val ident = pickNameIdent(tree)
      tryPickKind(tree)
        .map(kind => TypeParam.Kinded(ident, kind))
        .getOrElse(TypeParam.Unkinded(ident))
    }

    def pickConstraints(tree: Tree)(implicit sctx: SharedContext): List[TraitConstraint] = {
      val maybeWithClause = tryPick(TreeKind.Type.ConstraintList, tree)
      maybeWithClause.map(
        withClauseTree => pickAll(TreeKind.Type.Constraint, withClauseTree).map(visitTraitConstraint)
      ).getOrElse(List.empty)
    }

    private def visitTraitConstraint(tree: Tree)(implicit sctx: SharedContext): TraitConstraint = {
      def replaceIllegalTypesWithErrors(tpe: Type): (Type, List[SourceLocation]) = {
        val errorLocations = mutable.ArrayBuffer.empty[SourceLocation]

        def replace(tpe0: Type): Type = tpe0 match {
          case Type.Var(ident, loc) => Type.Var(ident, loc)
          case Type.Apply(t1, t2, loc) => Type.Apply(replace(t1), replace(t2), loc)
          case t =>
            errorLocations += t.loc
            Type.Error(t.loc)
        }

        (replace(tpe), errorLocations.toList)
      }

      expect(tree, TreeKind.Type.Constraint)
      val qname = pickQName(tree)
      val tpe = Types.pickType(tree)
      // Check for illegal type constraint parameter
      val (tpe1, errors) = replaceIllegalTypesWithErrors(tpe)
      errors.headOption.map(loc => sctx.errors.add(IllegalTraitConstraintParameter(loc)))
      TraitConstraint(qname, tpe1, tree.loc)
    }

    private def visitKind(tree: Tree)(implicit sctx: SharedContext): Kind = {
      expect(tree, TreeKind.Kind)
      val ident = pickNameIdent(tree)
      val kind = Kind.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), ident.loc)
      tryPick(TreeKind.Kind, tree)
      tryPickKind(tree)
        .map(Kind.Arrow(kind, _, tree.loc))
        .getOrElse(kind)
    }

    private def pickKind(tree: Tree)(implicit sctx: SharedContext): Kind = {
      visitKind(pick(TreeKind.Kind, tree))
    }

    def tryPickKind(tree: Tree)(implicit sctx: SharedContext): Option[Kind] = {
      // Cast a missing kind to None because 'tryPick' means that it's okay not to find a kind here.
      tryPick(TreeKind.Kind, tree).map(visitKind)
    }
  }

  private def pickQName(tree: Tree)(implicit sctx: SharedContext): Name.QName = {
    visitQName(pick(TreeKind.QName, tree))
  }

  private def tryPickQName(tree: Tree)(implicit sctx: SharedContext): Option[Name.QName] = {
    tryPick(TreeKind.QName, tree).map(visitQName)
  }

  private def visitQName(tree: Tree)(implicit sctx: SharedContext): Name.QName = {
    expect(tree, TreeKind.QName)
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    val trailingDot = tryPick(TreeKind.TrailingDot, tree).nonEmpty
    assert(idents.nonEmpty) // We require at least one element to construct a qname
    val first = idents.head
    val last = idents.last
    val loc = SourceLocation(isReal = true, first.loc.source, first.loc.start, last.loc.end)

    // If there is a trailing dot, we use all the idents as namespace and use "" as the ident
    // The resulting QName will be something like QName(["A", "B"], "")
    if (trailingDot) {
      val nname = Name.NName(idents, loc)
      val positionAfterDot = SourcePosition.moveRight(last.loc.end)
      val emptyIdentLoc = SourceLocation(isReal = true, last.loc.source, positionAfterDot, positionAfterDot)
      val emptyIdent = Name.Ident("", emptyIdentLoc)
      val qnameLoc = SourceLocation(isReal = true, first.loc.source, first.loc.start, positionAfterDot)
      Name.QName(nname, emptyIdent, qnameLoc)
    } else {
      // Otherwise we use all but the last ident as namespace and the last ident as the ident
      val nname = Name.NName(idents.dropRight(1), loc)
      Name.QName(nname, last, loc)
    }
  }

  private def pickNameIdent(tree: Tree)(implicit sctx: SharedContext): Name.Ident = {
    tokenToIdent(pick(TreeKind.Ident, tree))
  }

  private def tryPickNameIdent(tree: Tree)(implicit sctx: SharedContext): Option[Name.Ident] = {
    tryPick(TreeKind.Ident, tree).map(tokenToIdent)
  }

  private def pickJavaName(tree: Tree): Name.JavaName = {
    val qname = pick(TreeKind.QName, tree)
    Name.JavaName(pickAll(TreeKind.Ident, qname).flatMap(text), qname.loc)
  }

  private def visitPredicateAndArity(tree: Tree)(implicit sctx: SharedContext): PredicateAndArity = {
    val arityToken = pickToken(TokenKind.LiteralInt, tree)
    val ident = pickNameIdent(tree)
    val arity = tryParsePredicateArity(arityToken)
    PredicateAndArity(Name.mkPred(ident), arity)
  }

  private def tryParsePredicateArity(token: Token)(implicit sctx: SharedContext): Int = {
    token.text.toIntOption match {
      case Some(i) if i >= 1 => i
      case _ =>
        // Soft failure: report the error and fall back on an arity of 0.
        sctx.errors.add(WeederError.IllegalPredicateArity(token.mkSourceLocation()))
        0
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  /// HELPERS ////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

  /**
    * Checks that tree has expected kind by wrapping assert.
    * This provides an isolated spot for deciding what to do with unexpected kinds.
    */
  private def expect(tree: Tree, kind: TreeKind): Unit = assert(tree.kind == kind)


  /**
    * Checks that tree has one of expected kinds by wrapping assert.
    * This provides an isolated spot for deciding what to do with unexpected kinds.
    */
  private def expectAny(tree: Tree, kinds: List[TreeKind]): Unit = assert(kinds.contains(tree.kind))

  /**
    * Picks first child from a tree and if it is a [[Token]] turns it into a Name.Ident.
    * Only use this if the structure of tree is well-known.
    * IE. Calling on a tree of kind [[TreeKind.Ident]] is fine, but if the kind is not known avoid using [[tokenToIdent]].
    */
  private def tokenToIdent(tree: Tree)(implicit sctx: SharedContext): Name.Ident = {
    tree.children.headOption match {
      case Some(token@Token(_, src, _, _, sp1, sp2)) =>
        Name.Ident(token.text, SourceLocation(isReal = true, src, sp1, sp2))
      // If child is an ErrorTree, that means the parse already reported and error.
      // We can avoid double reporting by returning a success here.
      // Doing it this way is most resilient, but phases down the line might have trouble with this sort of thing.
      case Some(t: Tree) if t.kind.isInstanceOf[TreeKind.ErrorTree] =>
        val name = text(tree).mkString("")
        Name.Ident(name, tree.loc)
      case Some(t: Tree) if t.kind == TreeKind.CommentList =>
        // We hit a misplaced comment.
        val name = text(tree).mkString("")
        val error = MisplacedComments(SyntacticContext.Unknown, t.loc)
        sctx.errors.add(error)
        Name.Ident(name, tree.loc)
      case _ => throw InternalCompilerException(s"Parse failure: expected first child of '${tree.kind}' to be Child.Token", tree.loc)
    }
  }

  /**
    * When kinds are elided they default to the kind `Type`.
    */
  private def defaultKind(ident: Name.Ident): Kind = Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)

  /**
    * Plucks the first inner tree in children.
    * This is intended to be used to unfold the inner tree on special marker [[TreeKind]]s,
    * such as [[TreeKind.Type.Type]] or [[TreeKind.Expr.Expr]].
    * The parser guarantees that these tree kinds have at least a single child.
    */
  private def unfold(tree: Tree): Tree = {
    assert(tree.kind match {
      case TreeKind.Type.Type | TreeKind.Type.Effect | TreeKind.Expr.Expr | TreeKind.Predicate.Body => true
      case _ => false
    })

    // Find the first sub-tree that isn't a comment
    tree.children.find {
      case tree: Tree if tree.kind != TreeKind.CommentList => true
      case _ => false
    }.map {
      case tree: Tree => tree
      case _ => throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    }.getOrElse(
      throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    )
  }

  /**
    * Tries to find a token child of a specific [[TokenKind]].
    */
  private def hasToken(kind: TokenKind, tree: Tree): Boolean = {
    tree.children.exists {
      case Token(k, _, _, _, _, _) => k == kind
      case _ => false
    }
  }

  /**
    * Collects all immediate child trees from a tree.
    */
  private def pickAllTrees(tree: Tree): List[Tree] = {
    tree.children.collect {
      case t: Tree => t
    }.toList
  }

  /**
    * Collects all immediate child tokens from a tree.
    */
  private def pickAllTokens(tree: Tree): Array[Token] = {
    tree.children.collect { case token@Token(_, _, _, _, _, _) => token }
  }

  /**
    * Collects the text in immediate token children
    */
  private def text(tree: Tree): List[String] = {
    tree.children.foldLeft[List[String]](List.empty)((acc, c) => c match {
      case token@Token(_, _, _, _, _, _) => acc :+ token.text
      case _ => acc
    })
  }

  /**
    * Picks out the first sub-tree of a specific [[TreeKind]].
    */
  private def pick(kind: TreeKind, tree: Tree, synctx: SyntacticContext = SyntacticContext.Unknown): Tree = {
    tryPick(kind, tree) match {
      case Some(t) => t
      case None =>
        val error = NeedAtleastOne(NamedTokenSet.FromTreeKinds(Set(kind)), synctx, loc = tree.loc)
        throw PickException(error)
    }
  }

  /**
    * Picks out the first token of a specific [[TokenKind]].
    */
  private def pickToken(kind: TokenKind, tree: Tree, synctx: SyntacticContext = SyntacticContext.Unknown): Token = {
    tree.children.collectFirst {
      case token: Token if token.kind == kind => token
    } match {
      case Some(t) => t
      case _ =>
        val error = NeedAtleastOne(NamedTokenSet.FromKinds(Set(kind)), synctx, loc = tree.loc)
        throw PickException(error)
    }
  }

  /**
    * Tries to pick out the first sub-tree of a specific [[TreeKind]].
    */
  private def tryPick(kind: TreeKind, tree: Tree): Option[Tree] = {
    tree.children.find {
      case tree: Tree if tree.kind == kind => true
      case _ => false
    } match {
      case Some(tree: Tree) => Some(tree)
      case _ => None
    }
  }

  /**
    * Picks out all the sub-trees of a specific [[TreeKind]].
    */
  private def pickAll(kind: TreeKind, tree: Tree): List[Tree] = {
    tree.children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case tree: Tree if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }

  /**
    * Picks out all the sub-trees matching any of the given [[TreeKind]]s, preserving source order.
    */
  private def pickAllMulti(tree: Tree, kinds: TreeKind*): List[Tree] = {
    val kindSet = kinds.toSet
    tree.children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case tree: Tree if kindSet.contains(tree.kind) => acc.appended(tree)
      case _ => acc
    })
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
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {
    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[WeederError]]s or [[ParseError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[CompilationMessage])

  /**
    * An exception thrown by [[pick]] and [[pickToken]] when a required sub-tree or token is missing.
    *
    * This bypasses the [[Validation]] machinery: the exception is thrown deep inside weeding and
    * caught in [[weed]], where it is converted into a [[Validation.Failure]]. It exists so that the
    * remaining hard failures no longer rely on [[Validation]], which is slated for removal.
    */
  private case class PickException(error: NeedAtleastOne) extends RuntimeException

}
