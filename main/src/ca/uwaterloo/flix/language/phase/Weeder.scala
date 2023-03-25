/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Fixity}
import ca.uwaterloo.flix.language.ast.ParsedAst.{Effect, EffectSet, TypeParams}
import ca.uwaterloo.flix.language.ast.WeededAst.Pattern
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal, BigInteger}
import scala.annotation.tailrec
import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder {

  /**
    * Words that the Flix compiler reserves for special expressions.
    * Users must not define fields or variables with these names.
    */
  private val ReservedWords = Set(
    "!=", "&&&", "*", "**", "+", "-", "..", "/", ":", "::", ":::", ":=", "<", "<+>", "<-", "<<<", "<=",
    "<=>", "==", "=>", ">", ">=", ">>>", "???", "@", "Absent", "Bool", "Impure", "Nil", "Predicate", "Present", "Pure",
    "Read", "RecordRow", "Region", "SchemaRow", "Type", "Write", "^^^", "alias", "case", "catch", "chan",
    "class", "def", "deref", "else", "enum", "false", "fix", "force",
    "if", "import", "inline", "instance", "into", "lat", "law", "lawful", "lazy", "let", "let*", "match",
    "null", "opaque", "override", "pub", "ref", "region",
    "rel", "sealed", "set", "spawn", "Static", "true",
    "type", "use", "where", "with", "|||", "~~~", "discard", "object"
  )


  // NB: The following words should be reserved, but are currently allowed because of their presence in the standard library:
  // as, and, choose, choose*, forall, from, get, new, not, or, project, query, select, solve, try


  /**
    * Weeds the whole program.
    */
  def run(root: ParsedAst.Root, oldRoot: WeededAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[WeededAst.Root, WeederError] =
    flix.phase("Weeder") {
      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(root.units, oldRoot.units)

      val results = ParOps.parMap(stale)(kv => visitCompilationUnit(kv._1, kv._2))
      Validation.sequence(results) map {
        case rs =>
          val m = rs.foldLeft(fresh) {
            case (acc, (k, v)) => acc + (k -> v)
          }
          WeededAst.Root(m, root.entryPoint, root.names)
      }
    }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def visitCompilationUnit(src: Ast.Source, unit: ParsedAst.CompilationUnit)(implicit flix: Flix): Validation[(Ast.Source, WeededAst.CompilationUnit), WeederError] = {
    val usesAndImportsVal = traverse(unit.usesOrImports)(visitUseOrImport)
    val declarationsVal = traverse(unit.decls)(visitDecl)
    val loc = mkSL(unit.sp1, unit.sp2)

    mapN(usesAndImportsVal, declarationsVal) {
      case (usesAndImports, decls) =>
        src -> WeededAst.CompilationUnit(usesAndImports.flatten, decls.flatten, loc)
    }
  }

  /**
    * Compiles the given parsed declaration `past` to a list of weeded declarations.
    */
  private def visitDecl(decl: ParsedAst.Declaration)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = decl match {
    case ParsedAst.Declaration.Namespace(sp1, names, usesOrImports, decls, sp2) =>
      val usesAndImportsVal = traverse(usesOrImports)(visitUseOrImport)

      val declarationsVal = traverse(decls)(visitDecl)

      mapN(usesAndImportsVal, declarationsVal) {
        case (us, ds) =>
          // TODO can improve SL by starting from ident
          val loc = mkSL(sp1, sp2)

          val base = WeededAst.Declaration.Namespace(names.idents.last, us.flatten, ds.flatten, mkSL(sp1, sp2))
          val ns = names.idents.init.foldRight(base: WeededAst.Declaration) {
            case (ident, acc) => WeededAst.Declaration.Namespace(ident, Nil, List(acc), loc)
          }
          List(ns)
      }

    case d: ParsedAst.Declaration.Def => visitTopDef(d)

    case d: ParsedAst.Declaration.Law => visitLaw(d)

    case d: ParsedAst.Declaration.Enum => visitEnum(d)

    case d: ParsedAst.Declaration.RestrictableEnum => visitRestrictableEnum(d)

    case d: ParsedAst.Declaration.TypeAlias => visitTypeAlias(d)

    case d: ParsedAst.Declaration.Relation => visitRelation(d)

    case d: ParsedAst.Declaration.Lattice => visitLattice(d)

    case d: ParsedAst.Declaration.Class => visitClass(d)

    case d: ParsedAst.Declaration.Instance => visitInstance(d)

    case d: ParsedAst.Declaration.Effect => visitEffect(d)
  }

  /**
    * Performs weeding on the given class declaration `c0`.
    */
  private def visitClass(c0: ParsedAst.Declaration.Class)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Class], WeederError] = c0 match {
    case ParsedAst.Declaration.Class(doc0, ann0, mods0, sp1, ident, tparam0, superClasses0, lawsAndSigs, sp2) =>
      val loc = mkSL(sp1, sp2)
      val doc = visitDoc(doc0)
      val laws0 = lawsAndSigs.collect { case law: ParsedAst.Declaration.Law => law }
      val sigs0 = lawsAndSigs.collect { case sig: ParsedAst.Declaration.Sig => sig }

      val annVal = visitAnnotations(ann0)
      val modsVal = visitModifiers(mods0, legalModifiers = Set(Ast.Modifier.Lawful, Ast.Modifier.Public, Ast.Modifier.Sealed))
      val sigsVal = traverse(sigs0)(visitSig)
      val lawsVal = traverse(laws0)(visitLaw)
      val superClassesVal = traverse(superClasses0)(visitTypeConstraint)

      val tparam = visitTypeParam(tparam0)

      mapN(annVal, modsVal, sigsVal, lawsVal, superClassesVal) {
        case (ann, mods, sigs, laws, superClasses) =>
          List(WeededAst.Declaration.Class(doc, ann, mods, ident, tparam, superClasses, sigs.flatten, laws.flatten, loc))
      }
  }

  /**
    * Performs weeding on the given sig declaration `s0`.
    */
  private def visitSig(s0: ParsedAst.Declaration.Sig)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Sig], WeederError] = s0 match {
    case ParsedAst.Declaration.Sig(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, purOrEff, tconstrs0, exp0, sp2) =>
      val doc = visitDoc(doc0)

      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val pubVal = requirePublic(mods, ident)
      val identVal = visitName(ident)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val purAndEff = visitPurityAndEffect(purOrEff)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      val expVal = traverseOpt(exp0)(visitExp(_, SyntacticEnv.Top))

      mapN(annVal, modVal, pubVal, identVal, tparamsVal, formalsVal, tconstrsVal, expVal) {
        case (as, mod, _, _, tparams, fparams, tconstrs, exp) =>
          val tpe = visitType(tpe0)
          List(WeededAst.Declaration.Sig(doc, as, mod, ident, tparams, fparams, exp, tpe, purAndEff, tconstrs, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given instance declaration `i0`.
    */
  private def visitInstance(i0: ParsedAst.Declaration.Instance)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Instance], WeederError] = i0 match {
    case ParsedAst.Declaration.Instance(doc0, ann0, mods0, sp1, clazz, tpe0, tconstrs0, defs0, sp2) =>
      val doc = visitDoc(doc0)
      val tpe = visitType(tpe0)

      val annVal = visitAnnotations(ann0)
      val modsVal = visitModifiers(mods0, legalModifiers = Set.empty)
      val defsVal = traverse(defs0)(visitInstanceDef)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)

      mapN(annVal, modsVal, defsVal, tconstrsVal) {
        case (ann, mods, defs, tconstrs) =>
          List(WeededAst.Declaration.Instance(doc, ann, mods, clazz, tpe, tconstrs, defs.flatten, mkSL(sp1, sp2)))
      }

  }

  /**
    * Performs weeding on the given top-level def declaration `d0`.
    */
  private def visitTopDef(d0: ParsedAst.Declaration.Def)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = {
    visitDef(d0, Set(Ast.Modifier.Public), requiresPublic = false)
  }

  /**
    * Performs weeding on the given instance def declaration `d0`.
    */
  private def visitInstanceDef(d0: ParsedAst.Declaration.Def)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = {
    visitDef(d0, Set(Ast.Modifier.Public, Ast.Modifier.Override), requiresPublic = true)
  }

  /**
    * Performs weeding on the given def declaration `d0`.
    */
  private def visitDef(d0: ParsedAst.Declaration.Def, legalModifiers: Set[Ast.Modifier], requiresPublic: Boolean)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Def(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, purOrEff, tconstrs0, exp0, sp2) =>
      flix.subtask(ident.name, sample = true)

      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers)
      val pubVal = if (requiresPublic) requirePublic(mods, ident) else ().toSuccess // conditionally require a public modifier
      val identVal = visitName(ident)
      val expVal = visitExp(exp0, SyntacticEnv.Top)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val purAndEff = visitPurityAndEffect(purOrEff)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)

      mapN(annVal, modVal, pubVal, identVal, tparamsVal, formalsVal, expVal, tconstrsVal) {
        case (as, mod, _, _, tparams, fparams, exp, tconstrs) =>
          val tpe = visitType(tpe0)
          List(WeededAst.Declaration.Def(doc, as, mod, ident, tparams, fparams, exp, tpe, purAndEff, tconstrs, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given law declaration `d0`.
    */
  private def visitLaw(d0: ParsedAst.Declaration.Law)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Law(doc0, ann0, mod0, sp1, ident, tparams0, fparams0, tconstrs0, exp0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mod0, legalModifiers = Set.empty)
      val identVal = visitName(ident)
      val expVal = visitExp(exp0, SyntacticEnv.Top)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val tconstrsVal = Validation.traverse(tconstrs0)(visitTypeConstraint)

      mapN(annVal, modVal, identVal, tparamsVal, formalsVal, expVal, tconstrsVal) {
        case (ann, mod, _, tparams, fs, exp, tconstrs) =>
          val ts = fs.map(_.tpe.get)
          val purAndEff = WeededAst.PurityAndEffect(None, None)
          val tpe = WeededAst.Type.Ambiguous(Name.mkQName("Bool"), ident.loc)
          List(WeededAst.Declaration.Def(doc, ann, mod, ident, tparams, fs, exp, tpe, purAndEff, tconstrs, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given effect declaration.
    */
  private def visitEffect(d0: ParsedAst.Declaration.Effect)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Effect], WeederError] = d0 match {
    case ParsedAst.Declaration.Effect(doc0, ann0, mod0, sp1, ident, tparams0, ops0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val identVal = visitName(ident)
      val tparamsVal = requireNoTypeParams(tparams0)
      val opsVal = traverse(ops0)(visitOp)
      mapN(annVal, modVal, identVal, tparamsVal, opsVal) {
        case (ann, mod, _, _, ops) =>
          List(WeededAst.Declaration.Effect(doc, ann, mod, ident, ops, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given effect operation.
    */
  private def visitOp(d0: ParsedAst.Declaration.Op)(implicit flix: Flix): Validation[WeededAst.Declaration.Op, WeederError] = d0 match {
    case ParsedAst.Declaration.Op(doc0, ann0, mod0, sp1, ident, tparams0, fparamsOpt0, tpe0, purAndEff0, tconstrs0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val pubVal = requirePublic(mod0, ident)
      val identVal = visitName(ident)
      val tparamsVal = requireNoTypeParams(tparams0)
      val fparamsVal = visitFormalParams(fparamsOpt0, Presence.Required)
      val tpe = visitType(tpe0)
      val unitVal = requireUnit(tpe0, ident.loc)
      val purAndEffVal = requireNoEffect(purAndEff0, ident.loc)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      mapN(annVal, modVal, pubVal, identVal, tparamsVal, fparamsVal, unitVal, purAndEffVal, tconstrsVal) {
        case (ann, mod, _, _, _, fparams, _, _, tconstrs) =>
          val ts = fparams.map(_.tpe.get)
          val purAndEff = WeededAst.PurityAndEffect(None, None)
          WeededAst.Declaration.Op(doc, ann, mod, ident, fparams, tpe, tconstrs, mkSL(sp1, sp2));
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitEnum(d0: ParsedAst.Declaration.Enum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Enum], WeederError] = d0 match {
    case ParsedAst.Declaration.Enum(doc0, ann0, mods, sp1, ident, tparams0, tpe0, derives, cases0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public, Ast.Modifier.Opaque))
      val tparamsVal = visitTypeParams(tparams0)

      val casesVal = (tpe0, cases0) match {
        // Case 1: empty enum
        case (None, None) => Map.empty.toSuccess
        // Case 2: singleton enum
        case (Some(t0), None) => Map(ident -> WeededAst.Case(ident, visitType(t0), mkSL(sp1, sp2))).toSuccess
        // Case 3: multiton enum
        case (None, Some(cs0)) =>
          /*
           * Check for `DuplicateTag`.
           */
          Validation.fold[ParsedAst.Case, Map[Name.Ident, WeededAst.Case], WeederError](cs0, Map.empty) {
            case (macc, caze: ParsedAst.Case) =>
              val tagName = caze.ident
              macc.get(tagName) match {
                case None => (macc + (tagName -> visitCase(caze))).toSuccess
                case Some(otherTag) =>
                  val enumName = ident.name
                  val loc1 = otherTag.ident.loc
                  val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                  Failure(LazyList(
                    // NB: We report an error at both source locations.
                    DuplicateTag(enumName, tagName, loc1, loc2),
                    DuplicateTag(enumName, tagName, loc2, loc1)
                  ))
              }
          }
        // Case 4: both singleton and multiton syntax used: Error.
        case (Some(_), Some(_)) =>
          WeederError.IllegalEnum(ident.loc).toFailure
      }

      mapN(annVal, modVal, tparamsVal, casesVal) {
        case (ann, mod, tparams, cases) =>
          List(WeededAst.Declaration.Enum(doc, ann, mod, ident, tparams, derives.toList, cases.values.toList, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitRestrictableEnum(d0: ParsedAst.Declaration.RestrictableEnum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.RestrictableEnum], WeederError] = d0 match {
    case ParsedAst.Declaration.RestrictableEnum(doc0, ann0, mods, sp1, ident, index0, tparams0, tpe0, derives, cases0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public, Ast.Modifier.Opaque))
      val index = visitTypeParam(index0)
      val tparamsVal = visitTypeParams(tparams0)

      val casesVal = (tpe0, cases0) match {
        // Case 1: empty enum
        case (None, None) => Map.empty.toSuccess
        // Case 2: singleton enum
        case (Some(t0), None) => Map(ident -> WeededAst.RestrictableCase(ident, visitType(t0), mkSL(sp1, sp2))).toSuccess
        // Case 3: multiton enum
        case (None, Some(cs0)) =>
          /*
           * Check for `DuplicateTag`.
           */
          Validation.fold[ParsedAst.RestrictableCase, Map[Name.Ident, WeededAst.RestrictableCase], WeederError](cs0, Map.empty) {
            case (macc, caze: ParsedAst.RestrictableCase) =>
              val tagName = caze.ident
              macc.get(tagName) match {
                case None => (macc + (tagName -> visitRestrictableCase(caze))).toSuccess
                case Some(otherTag) =>
                  val enumName = ident.name
                  val loc1 = otherTag.ident.loc
                  val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                  Failure(LazyList(
                    // NB: We report an error at both source locations.
                    DuplicateTag(enumName, tagName, loc1, loc2),
                    DuplicateTag(enumName, tagName, loc2, loc1)
                  ))
              }
          }
        // Case 4: both singleton and multiton syntax used: Error.
        case (Some(_), Some(_)) =>
          WeederError.IllegalEnum(ident.loc).toFailure
      }

      mapN(annVal, modVal, tparamsVal, casesVal) {
        case (ann, mod, tparams, cases) =>
          List(WeededAst.Declaration.RestrictableEnum(doc, ann, mod, ident, index, tparams, derives.toList, cases.values.toList, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given enum case `c0`.
    */
  private def visitCase(c0: ParsedAst.Case)(implicit flix: Flix): WeededAst.Case = c0 match {
    case ParsedAst.Case(sp1, ident, tpe0, sp2) =>
      val tpe = tpe0.map(visitType).getOrElse(WeededAst.Type.Unit(ident.loc))
      WeededAst.Case(ident, tpe, mkSL(sp1, sp2))
  }

  /**
    * Performs weeding on the given enum case `c0`.
    */
  private def visitRestrictableCase(c0: ParsedAst.RestrictableCase)(implicit flix: Flix): WeededAst.RestrictableCase = c0 match {
    case ParsedAst.RestrictableCase(sp1, ident, tpe0, sp2) =>
      val tpe = tpe0.map(visitType).getOrElse(WeededAst.Type.Unit(ident.loc))
      WeededAst.RestrictableCase(ident, tpe, mkSL(sp1, sp2))
  }

  /**
    * Performs weeding on the given type alias declaration `d0`.
    */
  private def visitTypeAlias(d0: ParsedAst.Declaration.TypeAlias)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = d0 match {
    case ParsedAst.Declaration.TypeAlias(doc0, mod0, sp1, ident, tparams0, tpe0, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val tpe = visitType(tpe0)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, mkSL(sp1, sp2)))
      }
  }

  /**
    * Rewrites the given relation declaration `r0` to a type alias.
    */
  private def visitRelation(r0: ParsedAst.Declaration.Relation)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = r0 match {
    case ParsedAst.Declaration.Relation(doc0, mod0, sp1, ident, tparams0, attr, sp2) =>
      val doc = visitDoc(doc0)
      val loc = mkSL(sp1, sp2)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      //
      // Rewrite the relation declaration to a type alias.
      //
      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val termTypes = attr.map(a => visitType(a.tpe))
          val tpe = WeededAst.Type.Relation(termTypes.toList, ident.loc)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc))
      }
  }

  /**
    * Performs weeding on the given lattice `r0`.
    */
  private def visitLattice(l0: ParsedAst.Declaration.Lattice)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = l0 match {
    case ParsedAst.Declaration.Lattice(doc0, mod0, sp1, ident, tparams0, attr, sp2) =>
      val doc = visitDoc(doc0)
      val loc = mkSL(sp1, sp2)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      //
      // Rewrite the lattice declaration to a type alias.
      //
      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val termTypes = attr.map(a => visitType(a.tpe))
          val tpe = WeededAst.Type.Lattice(termTypes.toList, ident.loc)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc))
      }
  }

  /**
    * Performs weeding on the given use or import `u0`.
    */
  private def visitUseOrImport(u0: ParsedAst.UseOrImport): Validation[List[WeededAst.UseOrImport], WeederError] = u0 match {
    case ParsedAst.Use.UseOne(sp1, qname, sp2) =>
      List(WeededAst.UseOrImport.Use(qname, qname.ident, mkSL(sp1, sp2))).toSuccess

    case ParsedAst.Use.UseMany(_, nname, names, _) =>
      traverse(names) {
        case ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2) =>
          mapN(checkAliasCase(ident.name, aliasOpt)) {
            case () =>
              val alias = aliasOpt.getOrElse(ident)
              WeededAst.UseOrImport.Use(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)): WeededAst.UseOrImport
          }
      }

    case ParsedAst.Imports.ImportOne(sp1, name, sp2) =>
      val loc = mkSL(sp1, sp2)
      val alias = name.fqn.last
      if (raw"[A-Z][A-Za-z0-9_!]*".r matches alias) {
        List(WeededAst.UseOrImport.Import(name, Name.Ident(sp1, alias, sp2), loc)).toSuccess
      } else {
        WeederError.IllegalJavaClass(alias, loc).toFailure
      }

    case ParsedAst.Imports.ImportMany(sp1, pkg, ids, sp2) =>
      val loc = mkSL(sp1, sp2)
      val is = ids.map {
        case ParsedAst.Imports.NameAndAlias(_, name, alias, _) =>
          val fqn = Name.JavaName(pkg.sp1, pkg.fqn :+ name, pkg.sp2)
          val ident = alias.getOrElse(Name.Ident(sp1, name, sp2))
          WeededAst.UseOrImport.Import(fqn, ident, loc)
      }
      is.toList.toSuccess
  }

  /**
    * Checks that the name's casing matches the alias's casing. (I.e., both uppercase or both lowercase.)
    */
  private def checkAliasCase(name: String, aliasOpt: Option[Name.Ident]): Validation[Unit, WeederError] = {
    aliasOpt match {
      case Some(alias) if (alias.name.charAt(0).isUpper != name(0).isUpper) =>
        WeederError.IllegalUseAlias(name, alias.name, alias.loc).toFailure
      case _ => ().toSuccess
    }
  }

  /**
    * Weeds the given expression.
    */
  private def visitExp(exp0: ParsedAst.Expression, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = exp0 match {
    case ParsedAst.Expression.QName(sp1, qname, sp2) =>
      val parts = qname.namespace.idents :+ qname.ident
      val prefix = parts.takeWhile(_.isUpper)
      val suffix = parts.dropWhile(_.isUpper)
      suffix match {
        // Case 1: upper qualified name
        case Nil =>
          // NB: We only use the source location of the identifier itself.
          WeededAst.Expression.Ambiguous(qname, qname.ident.loc).toSuccess

        // Case 1: basic qualified name
        case ident :: Nil =>
          // NB: We only use the source location of the identifier itself.
          WeededAst.Expression.Ambiguous(qname, ident.loc).toSuccess

        // Case 2: actually a record access
        case ident :: fields =>
          // NB: We only use the source location of the identifier itself.
          val base = WeededAst.Expression.Ambiguous(Name.mkQName(prefix.map(_.toString), ident.name, ident.sp1, ident.sp2), ident.loc)
          fields.foldLeft(base: WeededAst.Expression) {
            case (acc, field) => WeededAst.Expression.RecordSelect(acc, Name.mkField(field), field.loc) // TODO NS-REFACTOR should use better location
          }.toSuccess
      }

    case ParsedAst.Expression.Open(sp1, qname, sp2) =>
      // TODO RESTR-VARS make sure it's capital
      WeededAst.Expression.Open(qname, mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Expression.OpenAs(sp1, qname, exp, sp2) =>
      // TODO RESTR-VARS make sure it's capital
      val eVal = visitExp(exp, senv)
      mapN(eVal) {
        case e => WeededAst.Expression.OpenAs(qname, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Hole(sp1, name, sp2) =>
      val loc = mkSL(sp1, sp2)
      WeededAst.Expression.Hole(name, loc).toSuccess

    case ParsedAst.Expression.HolyName(ident, sp2) =>
      val loc = mkSL(ident.sp1, sp2)
      val exp = WeededAst.Expression.Ambiguous(Name.mkQName(ident), ident.loc)
      WeededAst.Expression.HoleWithExp(exp, loc).toSuccess

    case ParsedAst.Expression.Use(sp1, use, exp, sp2) =>
      mapN(visitUseOrImport(use), visitExp(exp, senv)) {
        case (us, e) => WeededAst.Expression.Use(us, e, mkSL(sp1, sp2))
      }.recoverOne {
        case err: WeederError.IllegalJavaClass => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Lit(sp1, lit, sp2) =>
      mapN(weedLiteral(lit)) {
        case l => WeededAst.Expression.Cst(l, mkSL(sp1, sp2))
      }.recoverOne {
        case err: WeederError.IllegalLiteral => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Intrinsic(sp1, op, exps, sp2) =>
      val loc = mkSL(sp1, sp2)
      flatMapN(traverse(exps)(visitArgument(_, senv))) {
        case es => (op.name, es) match {
          case ("BOOL_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BoolOp.Not, e1, loc).toSuccess
          case ("BOOL_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.And, e1, e2, loc).toSuccess
          case ("BOOL_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Or, e1, e2, loc).toSuccess
          case ("BOOL_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Eq, e1, e2, loc).toSuccess
          case ("BOOL_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Neq, e1, e2, loc).toSuccess

          case ("CHAR_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Eq, e1, e2, loc).toSuccess
          case ("CHAR_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Neq, e1, e2, loc).toSuccess
          case ("CHAR_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Lt, e1, e2, loc).toSuccess
          case ("CHAR_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Le, e1, e2, loc).toSuccess
          case ("CHAR_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Gt, e1, e2, loc).toSuccess
          case ("CHAR_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Ge, e1, e2, loc).toSuccess

          case ("FLOAT32_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Float32Op.Neg, e1, loc).toSuccess
          case ("FLOAT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Add, e1, e2, loc).toSuccess
          case ("FLOAT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Sub, e1, e2, loc).toSuccess
          case ("FLOAT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Mul, e1, e2, loc).toSuccess
          case ("FLOAT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Div, e1, e2, loc).toSuccess
          case ("FLOAT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Exp, e1, e2, loc).toSuccess
          case ("FLOAT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Eq, e1, e2, loc).toSuccess
          case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Neq, e1, e2, loc).toSuccess
          case ("FLOAT32_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Lt, e1, e2, loc).toSuccess
          case ("FLOAT32_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Le, e1, e2, loc).toSuccess
          case ("FLOAT32_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Gt, e1, e2, loc).toSuccess
          case ("FLOAT32_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Ge, e1, e2, loc).toSuccess

          case ("FLOAT64_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Float64Op.Neg, e1, loc).toSuccess
          case ("FLOAT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Add, e1, e2, loc).toSuccess
          case ("FLOAT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Sub, e1, e2, loc).toSuccess
          case ("FLOAT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Mul, e1, e2, loc).toSuccess
          case ("FLOAT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Div, e1, e2, loc).toSuccess
          case ("FLOAT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Exp, e1, e2, loc).toSuccess
          case ("FLOAT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Eq, e1, e2, loc).toSuccess
          case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Neq, e1, e2, loc).toSuccess
          case ("FLOAT64_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Lt, e1, e2, loc).toSuccess
          case ("FLOAT64_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Le, e1, e2, loc).toSuccess
          case ("FLOAT64_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Gt, e1, e2, loc).toSuccess
          case ("FLOAT64_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Ge, e1, e2, loc).toSuccess

          case ("BIGDECIMAL_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BigDecimalOp.Neg, e1, loc).toSuccess
          case ("BIGDECIMAL_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Add, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Sub, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Mul, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Div, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Exp, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Eq, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Neq, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Lt, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Le, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Gt, e1, e2, loc).toSuccess
          case ("BIGDECIMAL_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigDecimalOp.Ge, e1, e2, loc).toSuccess

          case ("INT8_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int8Op.Neg, e1, loc).toSuccess
          case ("INT8_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int8Op.Not, e1, loc).toSuccess
          case ("INT8_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Add, e1, e2, loc).toSuccess
          case ("INT8_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Sub, e1, e2, loc).toSuccess
          case ("INT8_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Mul, e1, e2, loc).toSuccess
          case ("INT8_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Div, e1, e2, loc).toSuccess
          case ("INT8_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Rem, e1, e2, loc).toSuccess
          case ("INT8_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Exp, e1, e2, loc).toSuccess
          case ("INT8_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.And, e1, e2, loc).toSuccess
          case ("INT8_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Or, e1, e2, loc).toSuccess
          case ("INT8_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Xor, e1, e2, loc).toSuccess
          case ("INT8_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Shl, e1, e2, loc).toSuccess
          case ("INT8_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Shr, e1, e2, loc).toSuccess
          case ("INT8_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Eq, e1, e2, loc).toSuccess
          case ("INT8_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Neq, e1, e2, loc).toSuccess
          case ("INT8_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Lt, e1, e2, loc).toSuccess
          case ("INT8_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Le, e1, e2, loc).toSuccess
          case ("INT8_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Gt, e1, e2, loc).toSuccess
          case ("INT8_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Ge, e1, e2, loc).toSuccess

          case ("INT16_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int16Op.Neg, e1, loc).toSuccess
          case ("INT16_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int16Op.Not, e1, loc).toSuccess
          case ("INT16_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Add, e1, e2, loc).toSuccess
          case ("INT16_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Sub, e1, e2, loc).toSuccess
          case ("INT16_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Mul, e1, e2, loc).toSuccess
          case ("INT16_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Div, e1, e2, loc).toSuccess
          case ("INT16_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Rem, e1, e2, loc).toSuccess
          case ("INT16_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Exp, e1, e2, loc).toSuccess
          case ("INT16_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.And, e1, e2, loc).toSuccess
          case ("INT16_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Or, e1, e2, loc).toSuccess
          case ("INT16_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Xor, e1, e2, loc).toSuccess
          case ("INT16_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Shl, e1, e2, loc).toSuccess
          case ("INT16_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Shr, e1, e2, loc).toSuccess
          case ("INT16_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Eq, e1, e2, loc).toSuccess
          case ("INT16_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Neq, e1, e2, loc).toSuccess
          case ("INT16_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Lt, e1, e2, loc).toSuccess
          case ("INT16_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Le, e1, e2, loc).toSuccess
          case ("INT16_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Gt, e1, e2, loc).toSuccess
          case ("INT16_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Ge, e1, e2, loc).toSuccess

          case ("INT32_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int32Op.Neg, e1, loc).toSuccess
          case ("INT32_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int32Op.Not, e1, loc).toSuccess
          case ("INT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Add, e1, e2, loc).toSuccess
          case ("INT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Sub, e1, e2, loc).toSuccess
          case ("INT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Mul, e1, e2, loc).toSuccess
          case ("INT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Div, e1, e2, loc).toSuccess
          case ("INT32_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Rem, e1, e2, loc).toSuccess
          case ("INT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Exp, e1, e2, loc).toSuccess
          case ("INT32_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.And, e1, e2, loc).toSuccess
          case ("INT32_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Or, e1, e2, loc).toSuccess
          case ("INT32_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Xor, e1, e2, loc).toSuccess
          case ("INT32_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Shl, e1, e2, loc).toSuccess
          case ("INT32_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Shr, e1, e2, loc).toSuccess
          case ("INT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Eq, e1, e2, loc).toSuccess
          case ("INT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Neq, e1, e2, loc).toSuccess
          case ("INT32_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Lt, e1, e2, loc).toSuccess
          case ("INT32_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Le, e1, e2, loc).toSuccess
          case ("INT32_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Gt, e1, e2, loc).toSuccess
          case ("INT32_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Ge, e1, e2, loc).toSuccess

          case ("INT64_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int64Op.Neg, e1, loc).toSuccess
          case ("INT64_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int64Op.Not, e1, loc).toSuccess
          case ("INT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Add, e1, e2, loc).toSuccess
          case ("INT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Sub, e1, e2, loc).toSuccess
          case ("INT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Mul, e1, e2, loc).toSuccess
          case ("INT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Div, e1, e2, loc).toSuccess
          case ("INT64_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Rem, e1, e2, loc).toSuccess
          case ("INT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Exp, e1, e2, loc).toSuccess
          case ("INT64_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.And, e1, e2, loc).toSuccess
          case ("INT64_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Or, e1, e2, loc).toSuccess
          case ("INT64_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Xor, e1, e2, loc).toSuccess
          case ("INT64_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Shl, e1, e2, loc).toSuccess
          case ("INT64_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Shr, e1, e2, loc).toSuccess
          case ("INT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Eq, e1, e2, loc).toSuccess
          case ("INT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Neq, e1, e2, loc).toSuccess
          case ("INT64_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Lt, e1, e2, loc).toSuccess
          case ("INT64_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Le, e1, e2, loc).toSuccess
          case ("INT64_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Gt, e1, e2, loc).toSuccess
          case ("INT64_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Ge, e1, e2, loc).toSuccess

          case ("BIGINT_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BigIntOp.Neg, e1, loc).toSuccess
          case ("BIGINT_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BigIntOp.Not, e1, loc).toSuccess
          case ("BIGINT_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Add, e1, e2, loc).toSuccess
          case ("BIGINT_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Sub, e1, e2, loc).toSuccess
          case ("BIGINT_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Mul, e1, e2, loc).toSuccess
          case ("BIGINT_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Div, e1, e2, loc).toSuccess
          case ("BIGINT_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Rem, e1, e2, loc).toSuccess
          case ("BIGINT_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Exp, e1, e2, loc).toSuccess
          case ("BIGINT_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.And, e1, e2, loc).toSuccess
          case ("BIGINT_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Or, e1, e2, loc).toSuccess
          case ("BIGINT_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Xor, e1, e2, loc).toSuccess
          case ("BIGINT_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Shl, e1, e2, loc).toSuccess
          case ("BIGINT_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Shr, e1, e2, loc).toSuccess
          case ("BIGINT_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Eq, e1, e2, loc).toSuccess
          case ("BIGINT_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Neq, e1, e2, loc).toSuccess
          case ("BIGINT_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Lt, e1, e2, loc).toSuccess
          case ("BIGINT_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Le, e1, e2, loc).toSuccess
          case ("BIGINT_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Gt, e1, e2, loc).toSuccess
          case ("BIGINT_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Ge, e1, e2, loc).toSuccess

          case ("STRING_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.StringOp.Eq, e1, e2, loc).toSuccess
          case ("STRING_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.StringOp.Neq, e1, e2, loc).toSuccess

          case ("CHANNEL_GET", e1 :: Nil) => WeededAst.Expression.GetChannel(e1, loc).toSuccess
          case ("CHANNEL_PUT", e1 :: e2 :: Nil) => WeededAst.Expression.PutChannel(e1, e2, loc).toSuccess
          case ("CHANNEL_NEW", e1 :: e2 :: Nil) => WeededAst.Expression.NewChannel(e1, e2, loc).toSuccess

          case ("ARRAY_NEW", e1 :: e2 :: e3 :: Nil) => WeededAst.Expression.ArrayNew(e1, e2, e3, loc).toSuccess
          case ("ARRAY_LENGTH", e1 :: Nil) => WeededAst.Expression.ArrayLength(e1, loc).toSuccess
          case ("ARRAY_LOAD", e1 :: e2 :: Nil) => WeededAst.Expression.ArrayLoad(e1, e2, loc).toSuccess
          case ("ARRAY_STORE", e1 :: e2 :: e3 :: Nil) => WeededAst.Expression.ArrayStore(e1, e2, e3, loc).toSuccess

          case ("VECTOR_GET", e1 :: e2 :: Nil) => WeededAst.Expression.VectorLoad(e1, e2, loc).toSuccess
          case ("VECTOR_LENGTH", e1 :: Nil) => WeededAst.Expression.VectorLength(e1, loc).toSuccess

          case ("SCOPE_EXIT", e1 :: e2 :: Nil) => WeededAst.Expression.ScopeExit(e1, e2, loc).toSuccess

          case _ =>
            val err = WeederError.IllegalIntrinsic(loc)
            WeededAst.Expression.Error(err).toSoftFailure(err)
        }
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Apply(lambda, args, sp2) =>
      val sp1 = leftMostSourcePosition(lambda)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(lambda, senv), traverse(args)(e => visitArgument(e, senv))) {
        case (e, as) =>
          val es = getArguments(as, loc)
          WeededAst.Expression.Apply(e, es, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
      /*
       * Rewrites infix expressions to apply expressions.
       */
      mapN(visitExp(exp1, senv), visitExp(name, senv), visitExp(exp2, senv)) {
        case (e1, lambda, e2) =>
          val loc = mkSL(leftMostSourcePosition(exp1), sp2)
          WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
      }

    case ParsedAst.Expression.Lambda(sp1, fparams0, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      /*
       * Check for `DuplicateFormal`.
       */
      val fparamsVal = visitFormalParams(fparams0, Presence.Optional)
      val expVal = visitExp(exp, senv)
      mapN(fparamsVal, expVal) {
        case (fparams, e) => mkCurried(fparams, e, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.LambdaMatch(sp1, pat, exp, sp2) =>
      /*
       * Rewrites lambda pattern match expressions into a lambda expression with a nested pattern match.
       */
      mapN(visitPattern(pat), visitExp(exp, senv)) {
        case (p, e) =>
          mkLambdaMatch(sp1, p, e, sp2)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Unary(sp1, op, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      visitExp(exp, senv) map {
        case e => visitUnaryOperator(op) match {
          case OperatorResult.BuiltIn(name) => WeededAst.Expression.Apply(WeededAst.Expression.Ambiguous(name, name.loc), List(e), loc)
          case OperatorResult.Operator(o) => WeededAst.Expression.Unary(o, e, loc)
          case OperatorResult.Unrecognized(ident) => WeededAst.Expression.Apply(WeededAst.Expression.Ambiguous(Name.mkQName(ident), ident.loc), List(e), loc)
        }
      }

    case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) => visitBinaryOperator(op) match {
          case OperatorResult.BuiltIn(name) => WeededAst.Expression.Apply(WeededAst.Expression.Ambiguous(name, name.loc), List(e1, e2), loc)
          case OperatorResult.Operator(o) => WeededAst.Expression.Binary(o, e1, e2, loc)
          case OperatorResult.Unrecognized(ident) => WeededAst.Expression.Apply(WeededAst.Expression.Ambiguous(Name.mkQName(ident), ident.loc), List(e1, e2), loc)
        }
      }

    case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
      mapN(visitExp(exp1, senv), visitExp(exp2, senv), visitExp(exp3, senv)) {
        case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Stm(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) => WeededAst.Expression.Stm(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Discard(sp1, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      visitExp(exp, senv) map {
        case e => WeededAst.Expression.Discard(e, loc)
      }

    case ParsedAst.Expression.ApplicativeFor(sp1, frags, exp, sp2) =>
      //
      // Rewrites a ForA loop into a series of Applicative.ap calls:
      //
      //     forA (
      //         x <- xs;
      //         y <- ys
      //     ) yield exp
      //
      // desugars to
      //
      //     Applicative.ap(Functor.map(x -> y -> exp, xs), ys)
      //
      val loc = mkSL(sp1, sp2).asSynthetic

      if (frags.nonEmpty) {
        val fqnAp = "Applicative.ap"
        val fqnMap = "Functor.map"
        val yieldExp = visitExp(exp, senv)

        // Make lambda for Functor.map(lambda, ...). This lambda uses all patterns from the for-fragments.
        val lambda = foldRight(frags)(yieldExp) {
          case (ParsedAst.ForFragment.Generator(sp11, pat, _, sp12), acc) =>
            mapN(visitPattern(pat)) {
              case p => mkLambdaMatch(sp11, p, acc, sp12)
            }
        }

        // Apply first fragment to Functor.map
        val xs = visitExp(frags.head.exp, senv)
        val baseExp = mapN(lambda, xs) {
          case (l, x) => mkApplyFqn(fqnMap, List(l, x), loc)
        }

        // Apply rest of fragments to Applicative.ap
        frags.tail.foldLeft(baseExp) {
          case (acc, ParsedAst.ForFragment.Generator(sp11, _, fexp, sp12)) =>
            mapN(acc, visitExp(fexp, senv)) {
              case (a, e) => mkApplyFqn(fqnAp, List(a, e), mkSL(sp11, sp12).asSynthetic)
            }
        }
      } else {
        val err = WeederError.IllegalEmptyForFragment(loc)
        WeededAst.Expression.Error(err).toSoftFailure(err)
      }

    case ParsedAst.Expression.ForEach(sp1, frags, exp, sp2) =>
      //
      // Rewrites a foreach loop to Iterator.forEach call.
      //
      val loc = mkSL(sp1, sp2)

      if (frags.nonEmpty) {
        val fqnForEach = "Iterator.forEach"
        val fqnIterator = "Iterable.iterator"
        val regIdent = Name.Ident(sp1, "reg" + Flix.Delimiter + flix.genSym.freshId(), sp2)
        val regVar = WeededAst.Expression.Ambiguous(Name.mkQName(regIdent), loc)

        val foreachExp = foldRight(frags)(visitExp(exp, senv)) {
          case (ParsedAst.ForFragment.Generator(sp11, pat, exp1, sp12), exp0) =>
            mapN(visitPattern(pat), visitExp(exp1, senv)) {
              case (p, e1) =>
                val loc = mkSL(sp11, sp12).asSynthetic
                val lambda = mkLambdaMatch(sp11, p, exp0, sp12)
                val iterable = mkApplyFqn(fqnIterator, List(regVar, e1), e1.loc)
                val fparams = List(lambda, iterable)
                mkApplyFqn(fqnForEach, fparams, loc)
            }

          case (ParsedAst.ForFragment.Guard(sp11, exp1, sp12), exp0) =>
            mapN(visitExp(exp1, senv)) { e1 =>
              val loc = mkSL(sp11, sp12).asSynthetic
              WeededAst.Expression.IfThenElse(e1, exp0, WeededAst.Expression.Cst(Ast.Constant.Unit, loc), loc)
            }
        }

        mapN(foreachExp)(WeededAst.Expression.Scope(regIdent, _, loc))
          .recoverOne {
            case err: WeederError => WeededAst.Expression.Error(err)
          }
      } else {
        val err = WeederError.IllegalEmptyForFragment(loc)
        WeededAst.Expression.Error(err).toSoftFailure(err)
      }

    case ParsedAst.Expression.MonadicFor(sp1, frags, exp, sp2) =>
      //
      // Rewrites a for-loop to Monad.flatMap.
      //
      val loc = mkSL(sp1, sp2).asSynthetic

      if (frags.nonEmpty) {
        val fqnFlatMap = "Monad.flatMap"
        val fqnPoint = "Applicative.point"
        val fqnZero = "MonadZero.empty"
        val yieldExp = mapN(visitExp(exp, senv)) {
          case e => mkApplyFqn(fqnPoint, List(e), loc)
        }

        foldRight(frags)(yieldExp) {
          case (ParsedAst.ForFragment.Generator(sp11, pat, exp1, sp12), exp0) =>
            mapN(visitPattern(pat), visitExp(exp1, senv)) {
              case (p, e1) =>
                val loc = mkSL(sp11, sp12).asSynthetic
                val lambda = mkLambdaMatch(sp11, p, exp0, sp12)
                val fparams = List(lambda, e1)
                mkApplyFqn(fqnFlatMap, fparams, loc)
            }

          case (ParsedAst.ForFragment.Guard(sp11, exp1, sp12), exp0) =>
            mapN(visitExp(exp1, senv)) {
              case e1 =>
                val loc = mkSL(sp11, sp12).asSynthetic
                val zero = mkApplyFqn(fqnZero, List(WeededAst.Expression.Cst(Ast.Constant.Unit, loc)), loc)
                WeededAst.Expression.IfThenElse(e1, exp0, zero, loc)
            }
        }.recoverOne {
          case err: WeederError => WeededAst.Expression.Error(err)
        }
      } else {
        val err = WeederError.IllegalEmptyForFragment(loc)
        WeededAst.Expression.Error(err).toSoftFailure(err)
      }

    case ParsedAst.Expression.ForEachYield(sp1, frags, exp, sp2) => {
      //
      // Rewrites a foreach-yield loop into a series of iterators
      // wrapped in a Collectable.collect call:
      //
      //     foreach (x <- xs) yield x
      //
      // desugars to
      //
      //     region rc {
      //         Collectable.collect(
      //             Iterator.flatMap(
      //                 match x -> Iterator.singleton(rc, x),
      //                 Iterable.iterator(rc, xs)
      //             )
      //         )
      //     }
      //
      val baseLoc = mkSL(sp1, sp2).asSynthetic

      // Declare functions
      val fqnEmpty = "Iterator.empty"
      val fqnSingleton = "Iterator.singleton"
      val fqnFlatMap = "Iterator.flatMap"
      val fqnIterator = "Iterable.iterator"
      val fqnCollect = "Collectable.collect"

      // Make region variable
      val regionSym = "forEachYieldIteratorRegion" + Flix.Delimiter + flix.genSym.freshId()
      val regionIdent = Name.Ident(sp1, regionSym, sp2).asSynthetic
      val regionVar = WeededAst.Expression.Ambiguous(Name.mkQName(regionIdent), baseLoc)

      // Check that first fragment is an iterable collection
      // Also implies that there exists at least 1 iterable collection.
      frags.headOption match {
        case Some(ParsedAst.ForFragment.Generator(_, _, _, _)) =>
          // Desugar yield-exp
          //    ... yield x
          // Becomes
          //     Iterator.singleton(rc, x)
          val yieldExp = mapN(visitExp(exp, senv)) {
            case e =>
              mkApplyFqn(fqnSingleton, List(regionVar, e), baseLoc)
          }

          // Desugar loop
          val loop = foldRight(frags)(yieldExp) {
            case (ParsedAst.ForFragment.Generator(sp11, pat1, exp1, sp12), acc) =>
              // Case 1: a generator fragment i.e. `pat <- exp`
              // This should be desugared into
              //     Iterator.flatMap(match pat -> accExp, Iterator.iterator(exp))
              mapN(visitPattern(pat1), visitExp(exp1, senv)) {
                case (p, e1) =>
                  val loc = mkSL(sp11, sp12).asSynthetic

                  // 1. Create iterator from exp1
                  val iter = mkApplyFqn(fqnIterator, List(regionVar, e1), loc)

                  // 2. Create match-lambda with pat1 as params and acc as body
                  val lambda = mkLambdaMatch(sp11, p, acc, sp12)

                  // 3. Wrap in flatmap call
                  val fparams = List(lambda, iter)
                  mkApplyFqn(fqnFlatMap, fparams, loc)
              }

            case (ParsedAst.ForFragment.Guard(sp11, exp1, sp12), acc) =>
              // Case 2: a guard fragment i.e. `if exp`
              // This should be desugared into
              //     if (exp) accExp else Iterator.empty(rc)
              mapN(visitExp(exp1, senv)) {
                case e1 =>
                  val loc = mkSL(sp11, sp12).asSynthetic

                  // 1. Create empty iterator
                  val empty = mkApplyFqn(fqnEmpty, List(regionVar), loc)

                  // 2. Wrap acc in if-then-else exp: if (exp1) acc else Iterator.empty(empty)
                  WeededAst.Expression.IfThenElse(e1, acc, empty, loc)
              }
          }

          // Wrap in Collectable.collect function.
          // The nested calls to Iterator.flatMap are wrapped in
          // this function.
          val resultExp = mapN(loop) {
            case l => mkApplyFqn(fqnCollect, List(l), baseLoc)
          }

          // Wrap in region
          mapN(resultExp) {
            case e => WeededAst.Expression.Scope(regionIdent, e, baseLoc)
          }

        case Some(ParsedAst.ForFragment.Guard(_, _, _)) =>
          val err = WeederError.IllegalForFragment(baseLoc)
          WeededAst.Expression.Error(err).toSoftFailure(err)

        case None =>
          val err = WeederError.IllegalEmptyForFragment(baseLoc)
          WeededAst.Expression.Error(err).toSoftFailure(err)
      }
    }

    case ParsedAst.Expression.LetMatch(sp1, mod0, pat, tpe, exp1, exp2, sp2) =>
      //
      // Rewrites a let-match to a regular let-binding or a full-blown pattern match.
      //
      val loc = mkSL(sp1, sp2)

      val patVal = visitPattern(pat)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      val modVal = visitModifiers(mod0, legalModifiers = Set.empty)

      mapN(patVal, exp1Val, exp2Val, modVal) {
        case (WeededAst.Pattern.Var(ident, _), value, body, mod) =>
          // No pattern match.
          WeededAst.Expression.Let(ident, mod, withAscription(value, tpe), body, loc)
        case (pat, value, body, _) =>
          // Full-blown pattern match.
          val rule = WeededAst.MatchRule(pat, None, body)
          WeededAst.Expression.Match(withAscription(value, tpe), List(rule), loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.LetMatchStar(sp1, pat, tpe, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)

      //
      // Rewrites a monadic let-match to a regular let-binding or a full-blown pattern match inside a flatMap.
      //
      // let* x = exp1; exp2     ==>   flatMap(x -> exp2)(exp1)
      //
      val ident = Name.Ident(sp1, "flatMap", sp2)
      val flatMap = WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)

      mapN(visitPattern(pat), visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (WeededAst.Pattern.Var(ident, loc), value, body) =>
          // No pattern match.
          val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, body, loc)
          val inner = WeededAst.Expression.Apply(flatMap, List(lambda), loc)
          WeededAst.Expression.Apply(inner, List(value), loc)
        case (pat, value, body) =>
          // Full-blown pattern match.
          val lambdaIdent = Name.Ident(sp1, "pat" + Flix.Delimiter + flix.genSym.freshId(), sp2)
          val lambdaVar = WeededAst.Expression.Ambiguous(Name.mkQName(lambdaIdent), loc)

          val rule = WeededAst.MatchRule(pat, None, body)
          val lambdaBody = WeededAst.Expression.Match(withAscription(lambdaVar, tpe), List(rule), loc)

          val fparam = WeededAst.FormalParam(lambdaIdent, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
          val inner = WeededAst.Expression.Apply(flatMap, List(lambda), loc)
          WeededAst.Expression.Apply(inner, List(value), loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.LetRecDef(sp1, ident, fparams, _, exp1, exp2, sp2) =>
      val mod = Ast.Modifiers.Empty
      val loc = mkSL(sp1, sp2)

      mapN(visitFormalParams(fparams, Presence.Optional), visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (fp, e1, e2) =>
          val lambda = mkCurried(fp, e1, e1.loc)
          WeededAst.Expression.LetRec(ident, mod, lambda, e2, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.LetImport(sp1, impl, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)

      //
      // Visit the inner expression exp2.
      //
      impl match {
        case ParsedAst.JvmOp.Constructor(fqn, sig, tpe0, purAndEff0, ident) =>
          //
          // Introduce a let-bound lambda: (args...) -> InvokeConstructor(args) as tpe & pur
          //
          mapN(visitExp(exp2, senv)) {
            case e2 =>
              // Compute the class name.
              val className = fqn.toString

              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              //
              // Case 1: No arguments.
              //
              if (sig.isEmpty) {
                val fparam = WeededAst.FormalParam(Name.Ident(sp1, "_", sp2), Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
                val call = WeededAst.Expression.InvokeConstructor(className, Nil, Nil, loc)
                val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
                val e1 = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
                return WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc).toSuccess
              }

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val id = Name.Ident(sp1, "a" + index, sp2)
                  WeededAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = ts.zipWithIndex.map {
                case (_, index) =>
                  val ident = Name.Ident(sp1, "a" + index, sp2)
                  WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val call = WeededAst.Expression.InvokeConstructor(className, as, ts, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.Method(fqn, sig, tpe0, purAndEff0, identOpt) =>
          //
          // Introduce a let-bound lambda: (obj, args...) -> InvokeMethod(obj, args) as tpe & pur
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {
            case ((className, methodName), e2) =>
              // Compute the name of the let-bound variable.
              val ident = identOpt.getOrElse(Name.Ident(fqn.sp1, methodName, fqn.sp2))

              val receiverType = WeededAst.Type.Native(className, loc)

              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter for the object argument.
              val objId = Name.Ident(sp1, "obj" + Flix.Delimiter, sp2)
              val objParam = WeededAst.FormalParam(objId, Ast.Modifiers.Empty, Some(receiverType), loc)
              val objExp = WeededAst.Expression.Ambiguous(Name.mkQName(objId), loc)

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = objParam :: ts.zipWithIndex.map {
                case (tpe, index) =>
                  val ident = Name.Ident(sp1, "a" + index + Flix.Delimiter, sp2)
                  WeededAst.FormalParam(ident, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = objExp :: ts.zipWithIndex.map {
                case (_, index) =>
                  val ident = Name.Ident(sp1, "a" + index + Flix.Delimiter, sp2)
                  WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val call = WeededAst.Expression.InvokeMethod(className, methodName, as.head, as.tail, ts, tpe, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.StaticMethod(fqn, sig, tpe0, purAndEff0, identOpt) =>
          //
          // Introduce a let-bound lambda: (args...) -> InvokeStaticMethod(args) as tpe & pur
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {
            case ((className, methodName), e2) =>

              // Compute the name of the let-bound variable.
              val ident = identOpt.getOrElse(Name.Ident(fqn.sp1, methodName, fqn.sp2))

              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              //
              // Case 1: No arguments.
              //
              if (sig.isEmpty) {
                val fparam = WeededAst.FormalParam(Name.Ident(sp1, "_", sp2), Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
                val call = WeededAst.Expression.InvokeStaticMethod(className, methodName, Nil, Nil, tpe, loc)
                val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
                val e1 = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
                return WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc).toSuccess
              }

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val id = Name.Ident(sp1, "a" + index + Flix.Delimiter, sp2)
                  WeededAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = ts.zipWithIndex.map {
                case (_, index) =>
                  val ident = Name.Ident(sp1, "a" + index + Flix.Delimiter, sp2)
                  WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val call = WeededAst.Expression.InvokeStaticMethod(className, methodName, as, ts, tpe, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.GetField(fqn, tpe0, purAndEff0, ident) =>
          //
          // Introduce a let-bound lambda: o -> GetField(o) as tpe & pur
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {

            case ((className, fieldName), e2) =>
              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              val objectId = Name.Ident(sp1, "o" + Flix.Delimiter, sp2)
              val objectExp = WeededAst.Expression.Ambiguous(Name.mkQName(objectId), loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val call = WeededAst.Expression.GetField(className, fieldName, objectExp, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = WeededAst.Expression.Lambda(objectParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutField(fqn, tpe0, purAndEff0, ident) =>
          //
          // Introduce a let-bound lambda: (o, v) -> PutField(o, v) as tpe & pur
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {
            case ((className, fieldName), e2) =>
              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              val objectId = Name.Ident(sp1, "o" + Flix.Delimiter, sp2)
              val valueId = Name.Ident(sp1, "v" + Flix.Delimiter, sp2)
              val objectExp = WeededAst.Expression.Ambiguous(Name.mkQName(objectId), loc)
              val valueExp = WeededAst.Expression.Ambiguous(Name.mkQName(valueId), loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val call = WeededAst.Expression.PutField(className, fieldName, objectExp, valueExp, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = mkCurried(objectParam :: valueParam :: Nil, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.GetStaticField(fqn, tpe0, purAndEff0, ident) =>
          //
          // Introduce a let-bound lambda: _: Unit -> GetStaticField.
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {
            case ((className, fieldName), e2) =>
              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              val unitId = Name.Ident(sp1, "_", sp2)
              val unitParam = WeededAst.FormalParam(unitId, Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
              val call = WeededAst.Expression.GetStaticField(className, fieldName, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = WeededAst.Expression.Lambda(unitParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutStaticField(fqn, tpe0, purAndEff0, ident) =>
          //
          // Introduce a let-bound lambda: x -> PutStaticField(x).
          //
          mapN(parseClassAndMember(fqn), visitExp(exp2, senv)) {
            case ((className, fieldName), e2) =>
              val tpe = visitType(tpe0)
              val purAndEff = visitPurityAndEffect(purAndEff0)

              val valueId = Name.Ident(sp1, "v" + Flix.Delimiter, sp2)
              val valueExp = WeededAst.Expression.Ambiguous(Name.mkQName(valueId), loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val call = WeededAst.Expression.PutStaticField(className, fieldName, valueExp, loc)
              val lambdaBody = WeededAst.Expression.UncheckedCast(call, Some(tpe), purAndEff, loc)
              val e1 = WeededAst.Expression.Lambda(valueParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }
      }

    case ParsedAst.Expression.NewObject(sp1, tpe, methods, sp2) =>
      mapN(traverse(methods)(visitJvmMethod(_, senv))) {
        case ms =>
          val t = visitType(tpe)
          WeededAst.Expression.NewObject(t, ms, mkSL(sp1, sp2))
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Static(sp1, sp2) =>
      val loc = mkSL(sp1, sp2)
      val tpe = Type.mkRegion(Type.False, loc)
      WeededAst.Expression.Region(tpe, loc).toSuccess

    case ParsedAst.Expression.Scope(sp1, ident, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.Scope(ident, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
      val loc = mkSL(sp1, sp2)
      val rulesVal = traverse(rules) {
        case ParsedAst.MatchRule(pat, guard, body) =>
          mapN(visitPattern(pat), traverseOpt(guard)(visitExp(_, senv)), visitExp(body, senv)) {
            case (p, g, b) => WeededAst.MatchRule(p, g, b)
          }
      }
      mapN(visitExp(exp, senv), rulesVal) {
        case (e, rs) => WeededAst.Expression.Match(e, rs, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.TypeMatch(sp1, exp, rules, sp2) =>
      val loc = mkSL(sp1, sp2)
      val rulesVal = traverse(rules) {
        case ParsedAst.MatchTypeRule(ident, tpe, body) =>
          val t = visitType(tpe)
          mapN(visitExp(body, senv)) {
            // Pattern match without guard.
            case e => WeededAst.MatchTypeRule(ident, t, e)
          }
      }
      mapN(visitExp(exp, senv), rulesVal) {
        case (e, rs) => WeededAst.Expression.TypeMatch(e, rs, loc)
      }

    case ParsedAst.Expression.RelationalChoose(sp1, star, exps, rules, sp2) =>
      //
      // Check for mismatched arity of `exps` and `rules`.
      //
      val expectedArity = exps.length
      for (ParsedAst.RelationalChoiceRule(sp1, pat, _, sp2) <- rules) {
        val actualArity = pat.length
        if (actualArity != expectedArity) {
          val err = WeederError.MismatchedArity(expectedArity, actualArity, mkSL(sp1, sp2))
          return WeededAst.Expression.Error(err).toSoftFailure(err)
        }
      }

      val expsVal = traverse(exps)(visitExp(_, senv))
      val rulesVal = traverse(rules) {
        case ParsedAst.RelationalChoiceRule(_, pat, exp, _) =>
          val p = pat.map {
            case ParsedAst.RelationalChoicePattern.Wild(sp1, sp2) => WeededAst.RelationalChoicePattern.Wild(mkSL(sp1, sp2))
            case ParsedAst.RelationalChoicePattern.Absent(sp1, sp2) => WeededAst.RelationalChoicePattern.Absent(mkSL(sp1, sp2))
            case ParsedAst.RelationalChoicePattern.Present(sp1, ident, sp2) => WeededAst.RelationalChoicePattern.Present(ident, mkSL(sp1, sp2))
          }
          mapN(visitExp(exp, senv)) {
            case e => WeededAst.RelationalChoiceRule(p.toList, e)
          }
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => WeededAst.Expression.RelationalChoose(star, es, rs, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RestrictableChoose(sp1, star, exp, rules, sp2) =>
      val expVal = visitExp(exp, senv)
      val rulesVal = traverse(rules) {
        case ParsedAst.MatchRule(pat, guard, body) =>
          flatMapN(visitPattern(pat), traverseOpt(guard)(visitExp(_, senv)), visitExp(body, senv)) {
            case (p, g, b) => createRestrictableChoiceRule(star, p, g, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => WeededAst.Expression.RestrictableChoose(star, e, rs, mkSL(sp1, sp2))
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
      /*
       * Rewrites empty tuples to Unit and eliminate single-element tuples.
       */
      traverse(elms)(visitArgument(_, senv)) map {
        case Nil =>
          val loc = mkSL(sp1, sp2)
          WeededAst.Expression.Cst(Ast.Constant.Unit, loc)
        case x :: Nil => x
        case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
      } recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.RecordLit(sp1, fields, sp2) =>
      val fieldsVal = traverse(fields) {
        case ParsedAst.RecordField(_, ident, exp, _) =>
          val expVal = visitExp(exp, senv)

          mapN(expVal) {
            case e => ident -> e
          }
      }

      mapN(fieldsVal) {
        case fs =>
          // Rewrite into a sequence of nested record extensions.
          val zero = WeededAst.Expression.RecordEmpty(mkSL(sp1, sp2))
          fs.foldRight(zero: WeededAst.Expression) {
            case ((ident, e), acc) => WeededAst.Expression.RecordExtend(Name.mkField(ident), e, acc, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.RecordSelect(exp, ident, sp2) =>
      val sp1 = leftMostSourcePosition(exp)
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.RecordSelect(e, Name.mkField(ident), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordSelectLambda(sp1, _, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ident = Name.Ident(sp1, "_rec", sp2)
      val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc)
      val varExp = WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)
      val lambdaBody = WeededAst.Expression.RecordSelect(varExp, Name.mkField(ident), loc)
      WeededAst.Expression.Lambda(fparam, lambdaBody, loc).toSuccess

    case ParsedAst.Expression.RecordOperation(_, ops, rest, _) =>
      // We translate the sequence of record operations into a nested tree using a fold right.
      foldRight(ops)(visitExp(rest, senv)) {
        case (ParsedAst.RecordOp.Extend(sp1, ident, exp, sp2), acc) =>
          mapN(visitExp(exp, senv)) {
            case e =>
              WeededAst.Expression.RecordExtend(Name.mkField(ident), e, acc, mkSL(sp1, sp2))
          }

        case (ParsedAst.RecordOp.Restrict(sp1, ident, sp2), acc) =>
          WeededAst.Expression.RecordRestrict(Name.mkField(ident), acc, mkSL(sp1, sp2)).toSuccess

        case (ParsedAst.RecordOp.Update(sp1, ident, exp, sp2), acc) =>
          mapN(visitExp(exp, senv)) {
            case e =>
              // An update is a restrict followed by an extension.
              val inner = WeededAst.Expression.RecordRestrict(Name.mkField(ident), acc, mkSL(sp1, sp2))
              WeededAst.Expression.RecordExtend(Name.mkField(ident), e, inner, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.ArrayLit(sp1, exps, exp, sp2) =>
      mapN(traverse(exps)(visitExp(_, senv)), visitExp(exp, senv)) {
        case (es, e) => WeededAst.Expression.ArrayLit(es, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.VectorLit(sp1, exps, sp2) =>
      mapN(traverse(exps)(visitExp(_, senv))) {
        case es => WeededAst.Expression.VectorLit(es, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FCons(exp1, sp1, sp2, exp2) =>
      /*
       * Rewrites a `FCons` expression into a tag expression.
       */
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) =>
          val loc = mkSL(sp1, sp2)
          mkApplyFqn("List.Cons", List(e1, e2), loc)
      }

    case ParsedAst.Expression.FAppend(exp1, sp1, sp2, exp2) =>
      /*
       * Rewrites a `FAppend` expression into a call to `List/append`.
       */
      val loc = mkSL(sp1, sp2).asSynthetic

      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) =>
          // NB: We painstakingly construct the qualified name
          // to ensure that source locations are available.
          mkApplyFqn("List.append", List(e1, e2), loc)
      }

    case ParsedAst.Expression.ListLit(sp1, sp2, exps) =>
      /*
       * Rewrites a `FList` expression into `List.Nil` with `List.Cons`.
       */
      val loc = mkSL(sp1, sp2).asSynthetic

      traverse(exps)(e => visitExp(e, senv)) map {
        case es =>
          val nil: WeededAst.Expression = WeededAst.Expression.Ambiguous(Name.mkQName("List.Nil"), loc)
          es.foldRight(nil) {
            case (elm, acc) =>
              mkApplyFqn("List.Cons", List(elm, acc), loc)
          }
      }

    case ParsedAst.Expression.SetLit(sp1, sp2, exps) =>
      /*
       * Rewrites a `FSet` expression into `Set/empty` and a `Set/insert` calls.
       */
      val loc = mkSL(sp1, sp2).asSynthetic

      traverse(exps)(e => visitExp(e, senv)) map {
        case es =>
          val empty = mkApplyFqn("Set.empty", List(WeededAst.Expression.Cst(Ast.Constant.Unit, loc)), loc)
          es.foldLeft(empty) {
            case (acc, elm) => mkApplyFqn("Set.insert", List(elm, acc), loc)
          }
      }

    case ParsedAst.Expression.MapLit(sp1, sp2, exps) =>
      /*
       * Rewrites a `FMap` expression into `Map/empty` and a `Map/insert` calls.
       */
      val loc = mkSL(sp1, sp2).asSynthetic

      val elmsVal = traverse(exps) {
        case (key, value) => mapN(visitExp(key, senv), visitExp(value, senv)) {
          case (k, v) => (k, v)
        }
      }

      elmsVal map {
        case es =>
          val empty = mkApplyFqn("Map.empty", List(WeededAst.Expression.Cst(Ast.Constant.Unit, loc)), loc)
          es.foldLeft(empty) {
            case (acc, (k, v)) => mkApplyFqn("Map.insert", List(k, v, acc), loc)
          }
      }

    case ParsedAst.Expression.Interpolation(sp1, parts, sp2) =>

      /**
        * Returns an expression that concatenates the result of the expression `e1` with the expression `e2`.
        */
      def mkConcat(e1: WeededAst.Expression, e2: WeededAst.Expression, loc: SourceLocation): WeededAst.Expression = {
        val sop = SemanticOperator.StringOp.Concat
        val l = loc.asSynthetic
        WeededAst.Expression.Binary(sop, e1, e2, l)
      }

      /**
        * Returns an expression that applies `toString` to the result of the given expression `e`.
        */
      def mkApplyToString(e: WeededAst.Expression, sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
        val fqn = "ToString.toString"
        val loc = mkSL(sp1, sp2).asSynthetic
        mkApplyFqn(fqn, List(e), loc)
      }

      /**
        * Returns an expression that applies `debugString` to the result of the given expression `e`.
        */
      def mkApplyDebugString(e: WeededAst.Expression, sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
        val fqn = "Debug.stringify"
        val loc = mkSL(sp1, sp2).asSynthetic
        mkApplyFqn(fqn, List(e), loc)
      }

      val loc = mkSL(sp1, sp2)

      parts match {
        case Seq(ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
          // Special case: We have a constant string. Check the contents and return it.
          weedCharSequence(chars) map {
            string => WeededAst.Expression.Cst(Ast.Constant.Str(string), mkSL(innerSp1, innerSp2))
          }

        case _ =>
          // General Case: Fold the interpolator parts together.
          val init = WeededAst.Expression.Cst(Ast.Constant.Str(""), loc)
          Validation.fold(parts, init: WeededAst.Expression) {
            // Case 1: string part
            case (acc, ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
              weedCharSequence(chars) map {
                string =>
                  val e2 = WeededAst.Expression.Cst(Ast.Constant.Str(string), mkSL(innerSp1, innerSp2))
                  mkConcat(acc, e2, loc)
              }
            // Case 2: interpolated expression
            case (acc, ParsedAst.InterpolationPart.ExpPart(innerSp1, Some(exp), innerSp2)) =>
              mapN(visitExp(exp, senv)) {
                e =>
                  val e2 = mkApplyToString(e, innerSp1, innerSp2)
                  mkConcat(acc, e2, mkSL(innerSp1, innerSp2))
              }
            // Case 3: interpolated debug
            case (acc, ParsedAst.InterpolationPart.DebugPart(innerSp1, Some(exp), innerSp2)) =>
              mapN(visitExp(exp, senv)) {
                e =>
                  val e2 = mkApplyDebugString(e, innerSp1, innerSp2)
                  mkConcat(acc, e2, mkSL(innerSp1, innerSp2))
              }
            // Case 4: empty interpolated expression
            case (_, ParsedAst.InterpolationPart.ExpPart(innerSp1, None, innerSp2)) =>
              val err = WeederError.EmptyInterpolatedExpression(mkSL(innerSp1, innerSp2))
              WeededAst.Expression.Error(err).toSoftFailure(err)
            // Case 5: empty interpolated debug
            case (_, ParsedAst.InterpolationPart.DebugPart(innerSp1, None, innerSp2)) =>
              val err = WeederError.EmptyInterpolatedExpression(mkSL(innerSp1, innerSp2))
              WeededAst.Expression.Error(err).toSoftFailure(err)
          }
      }

    case ParsedAst.Expression.Ref(sp1, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expression.Ref(e1, e2, loc)
      }

    case ParsedAst.Expression.Deref(sp1, exp, sp2) =>
      for {
        e <- visitExp(exp, senv)
      } yield WeededAst.Expression.Deref(e, mkSL(sp1, sp2))

    case ParsedAst.Expression.Assign(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expression.Assign(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Ascribe(exp, expectedType, expectedEff, sp2) =>
      val t = expectedType.map(visitType)
      val f = visitPurityAndEffect(expectedEff)
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.Ascribe(e, t, f, mkSL(leftMostSourcePosition(exp), sp2))
      }

    case ParsedAst.Expression.CheckedTypeCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.CheckedCast(Ast.CheckedCastType.TypeCast, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.CheckedEffectCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.CheckedCast(Ast.CheckedCastType.EffectCast, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.UncheckedCast(sp1, exp, declaredType, declaredEff, sp2) =>
      val t = declaredType.map(visitType)
      val f = visitPurityAndEffect(declaredEff)
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.UncheckedCast(e, t, f, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.UncheckedMaskingCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.UncheckedMaskingCast(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Without(exp, effs, sp2) =>
      val loc = mkSL(leftMostSourcePosition(exp), sp2)
      // NB: We only give the innermost expression a real location
      mapN(visitExp(exp, senv)) {
        e =>
          val base = WeededAst.Expression.Without(e, effs.head, loc)
          effs.tail.foldLeft(base) {
            case (acc, eff) => WeededAst.Expression.Without(acc, eff, loc.asSynthetic)
          }
      }

    case ParsedAst.Expression.Do(sp1, op, args0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val argsVal = traverse(args0)(visitArgument(_, senv)).map(getArguments(_, loc))
      mapN(argsVal) {
        args => WeededAst.Expression.Do(op, args, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Resume(sp1, arg0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val argVal = visitArgument(arg0, senv)
      flatMapN(argVal) {
        case arg =>
          // ensure we are in a handler
          senv match {
            // Case 1: In a handler. All is well.
            case SyntacticEnv.Handler =>
              WeededAst.Expression.Resume(arg, loc).toSuccess
            // Case 2: Not in a handler. Error.
            case SyntacticEnv.Top =>
              val err = WeederError.IllegalResume(loc)
              WeededAst.Expression.Error(err).toSoftFailure(err)
          }
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Try(sp1, exp, ParsedAst.CatchOrHandler.Catch(rules), sp2) =>
      val expVal = visitExp(exp, senv)
      val rulesVal = traverse(rules) {
        case ParsedAst.CatchRule(ident, fqn, body) =>
          visitExp(body, senv) map {
            case b => WeededAst.CatchRule(ident, fqn.toString, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => WeededAst.Expression.TryCatch(e, rs, mkSL(sp1, sp2))
      }

    // not handling these rules yet
    case ParsedAst.Expression.Try(sp1, exp0, ParsedAst.CatchOrHandler.Handler(eff, rules0), sp2) =>
      val expVal = visitExp(exp0, senv)
      val rulesVal = traverse(rules0.getOrElse(Seq.empty)) {
        case ParsedAst.HandlerRule(op, fparams0, body0) =>
          val fparamsVal = visitFormalParams(fparams0, Presence.Forbidden)
          val bodyVal = visitExp(body0, SyntacticEnv.Handler)
          mapN(fparamsVal, bodyVal) {
            case (fparams, body) => WeededAst.HandlerRule(op, fparams, body)
          }
      }
      val loc = mkSL(sp1, sp2)
      mapN(expVal, rulesVal) {
        case (exp, rules) => WeededAst.Expression.TryWith(exp, eff, rules, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.SelectChannel(sp1, rules, exp, sp2) =>
      val rulesVal = traverse(rules) {
        case ParsedAst.SelectChannelRule(ident, chan, body) => mapN(visitExp(chan, senv), visitExp(body, senv)) {
          case (c, b) => WeededAst.SelectChannelRule(ident, c, b)
        }
      }

      val defaultVal = exp match {
        case Some(exp) => visitExp(exp, senv) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => WeededAst.Expression.SelectChannel(rs, d, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Spawn(sp1, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expression.Spawn(e1, e2, loc)
      }

    case ParsedAst.Expression.Par(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv))(WeededAst.Expression.Par(_, mkSL(sp1, sp2)))

    case ParsedAst.Expression.ParYield(sp1, frags, exp, sp2) =>
      val fragVals = traverse(frags) {
        case ParsedAst.ParYieldFragment(fsp1, pat, e, fsp2) => mapN(visitPattern(pat), visitExp(e, senv)) {
          case (p, e1) => WeededAst.ParYieldFragment(p, e1, mkSL(fsp1, fsp2))
        }
      }

      mapN(fragVals, visitExp(exp, senv)) {
        case (fs, e) => WeededAst.Expression.ParYield(fs, e, mkSL(sp1, sp2))
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Lazy(sp1, exp, sp2) =>
      visitExp(exp, senv) map {
        case e => WeededAst.Expression.Lazy(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Force(sp1, exp, sp2) =>
      visitExp(exp, senv) map {
        case e => WeededAst.Expression.Force(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointConstraint(sp1, con, sp2) =>
      val loc = mkSL(sp1, sp2)

      mapN(visitConstraint(con, senv)) {
        case c => WeededAst.Expression.FixpointConstraintSet(c :: Nil, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.FixpointConstraintSet(sp1, cs0, sp2) =>
      val loc = mkSL(sp1, sp2)

      traverse(cs0)(visitConstraint(_, senv)) map {
        case cs => WeededAst.Expression.FixpointConstraintSet(cs, loc)
      } recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.FixpointLambda(sp1, pparams, exp, sp2) =>
      val ps = pparams.map(visitPredicateParam)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expression.FixpointLambda(ps.toList, e, loc)
      }

    case ParsedAst.Expression.FixpointCompose(exp1, exp2, sp2) =>
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) =>
          val sp1 = leftMostSourcePosition(exp1)
          WeededAst.Expression.FixpointMerge(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointInjectInto(sp1, exps, idents, sp2) =>
      val loc = mkSL(sp1, sp2)

      ///
      /// Check for [[MismatchedArity]].
      ///
      if (exps.length != idents.length) {
        val err = WeederError.MismatchedArity(exps.length, idents.length, loc)
        return WeededAst.Expression.Error(err).toSoftFailure(err)
      }

      mapN(traverse(exps)(visitExp(_, senv))) {
        case es =>
          val init = WeededAst.Expression.FixpointConstraintSet(Nil, loc)
          es.zip(idents.toList).foldRight(init: WeededAst.Expression) {
            case ((exp, ident), acc) =>
              val pred = Name.mkPred(ident)
              val innerExp = WeededAst.Expression.FixpointInject(exp, pred, loc)
              WeededAst.Expression.FixpointMerge(innerExp, acc, loc)
          }
      }

    case ParsedAst.Expression.FixpointSolveWithProject(sp1, exps, optIdents, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      mapN(traverse(exps)(visitExp(_, senv))) {
        case es =>
          //
          // Performs the following rewrite:
          //
          // solve e1, e2, e3 project P1, P2, P3
          //
          // =>
          //
          // let tmp% = solve (merge e1, 2, e3);
          // merge (project P1 tmp%, project P2 tmp%, project P3 tmp%)

          // Introduce a tmp% variable that holds the minimal model of the merge of the exps.
          val freshVar = flix.genSym.freshId()
          val localVar = Name.Ident(SourcePosition.Unknown, s"tmp" + Flix.Delimiter + freshVar, SourcePosition.Unknown)

          // Merge all the exps into one Datalog program value.
          val mergeExp = es.reduceRight[WeededAst.Expression] {
            case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
          }
          val modelExp = WeededAst.Expression.FixpointSolve(mergeExp, loc)

          // Any projections?
          val bodyExp = optIdents match {
            case None =>
              // Case 1: No projections: Simply return the minimal model.
              WeededAst.Expression.Ambiguous(Name.mkQName(localVar), loc)
            case Some(idents) =>
              // Case 2: A non-empty sequence of predicate symbols to project.

              // Construct a list of each projection.
              val projectExps = idents.map {
                case ident =>
                  val varExp = WeededAst.Expression.Ambiguous(Name.mkQName(localVar), loc)
                  WeededAst.Expression.FixpointFilter(Name.Pred(ident.name, loc), varExp, loc)
              }

              // Merge all of the projections into one result.
              projectExps.reduceRight[WeededAst.Expression] {
                case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
              }
          }

          // Bind the tmp% variable to the minimal model and combine it with the body expression.
          WeededAst.Expression.Let(localVar, Ast.Modifiers.Empty, modelExp, bodyExp, loc.asReal)
      }

    case ParsedAst.Expression.FixpointQueryWithSelect(sp1, exps0, selects0, from0, whereExp0, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic

      mapN(traverse(exps0)(visitExp(_, senv)), traverse(selects0)(visitExp(_, senv)), traverse(from0)(visitPredicateBody(_, senv)), traverse(whereExp0)(visitExp(_, senv))) {
        case (exps, selects, from, where) =>
          //
          // Performs the following rewrite:
          //
          // query e1, e2, e3 select (x, y, z) from A(x, y), B(z) where x > 0
          //
          // =>
          //
          // project out %Result from (solve (merge (merge e1, e2, e3) #{ #Result(x, y, z) :- A(x, y), B(y) if x > 0 } )
          //
          // OBS: The last merge and solve is done in the typer because of trouble when
          // `(merge e1, e2, e3)` is a closed row.

          // The fresh predicate name where to store the result of the query.
          val pred = Name.Pred(Flix.Delimiter + "Result", loc)

          // The head of the pseudo-rule.
          val den = Denotation.Relational
          val head = WeededAst.Predicate.Head.Atom(pred, den, selects, loc)

          // The body of the pseudo-rule.
          val guard = where.map(g => WeededAst.Predicate.Body.Guard(g, loc))

          // Automatically fix all lattices atoms.
          val body = guard ::: from.map {
            case WeededAst.Predicate.Body.Atom(pred, Denotation.Latticenal, polarity, _, terms, loc) =>
              WeededAst.Predicate.Body.Atom(pred, Denotation.Latticenal, polarity, Fixity.Fixed, terms, loc)
            case pred => pred
          }

          // Construct the pseudo-query.
          val pseudoConstraint = WeededAst.Constraint(head, body, loc)

          // Construct a constraint set that contains the single pseudo constraint.
          val queryExp = WeededAst.Expression.FixpointConstraintSet(List(pseudoConstraint), loc)

          // Construct the merge of all the expressions.
          val dbExp = exps.reduceRight[WeededAst.Expression] {
            case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
          }

          // Extract the tuples of the result predicate.
          WeededAst.Expression.FixpointProject(pred, queryExp, dbExp, loc)
      }.recoverOne {
        case err: WeederError => WeededAst.Expression.Error(err)
      }

    case ParsedAst.Expression.Debug(sp1, kind, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e =>
          val loc = mkSL(sp1, sp2)
          val prefix = kind match {
            case ParsedAst.DebugKind.Debug => ""
            case ParsedAst.DebugKind.DebugWithLoc => s"[${loc.formatWithLine}] "
            case ParsedAst.DebugKind.DebugWithLocAndSrc =>
              val locPart = s"[${loc.formatWithLine}]"
              val srcPart = e.loc.text.map(s => s" $s = ").getOrElse("")
              locPart + srcPart
          }
          val e1 = WeededAst.Expression.Cst(Ast.Constant.Str(prefix), loc)
          val call = mkApplyFqn("Debug.debugWithPrefix", List(e1, e), loc)
          WeededAst.Expression.UncheckedMaskingCast(call, loc)
      }

  }

  /**
    * Returns a restrictable choice rule from an already visited pattern, guard, and body.
    * It is checked that
    *
    * - The guard is absent
    *
    * - The patterns are only tags with possible terms of variables and wildcards
    */
  private def createRestrictableChoiceRule(star: Boolean, p0: WeededAst.Pattern, g0: Option[WeededAst.Expression], b0: WeededAst.Expression): Validation[WeededAst.RestrictableChoiceRule, WeederError] = {
    // Check that guard is not present
    val gVal = g0 match {
      case Some(g) => WeederError.RestrictableChoiceGuard(star, g.loc).toFailure
      case None => ().toSuccess
    }
    // Check that patterns are only tags of variables (or wildcards as variables, or unit)
    val pVal = p0 match {
      case Pattern.Tag(qname, pat, loc) =>
        val innerVal = pat match {
          case Pattern.Tuple(elms, _) =>
            traverse(elms) {
              case Pattern.Wild(loc) => WeededAst.RestrictableChoicePattern.Wild(loc).toSuccess
              case Pattern.Var(ident, loc) => WeededAst.RestrictableChoicePattern.Var(ident, loc).toSuccess
              case Pattern.Cst(Ast.Constant.Unit, loc) => WeededAst.RestrictableChoicePattern.Wild(loc).toSuccess
              case other => WeederError.UnsupportedRestrictedChoicePattern(star, other.loc).toFailure
            }
          case Pattern.Wild(loc) => List(WeededAst.RestrictableChoicePattern.Wild(loc)).toSuccess
          case Pattern.Var(ident, loc) => List(WeededAst.RestrictableChoicePattern.Var(ident, loc)).toSuccess
          case Pattern.Cst(Ast.Constant.Unit, loc) => List(WeededAst.RestrictableChoicePattern.Wild(loc)).toSuccess
          case other => WeederError.UnsupportedRestrictedChoicePattern(star, other.loc).toFailure
        }
        mapN(innerVal) {
          case inner => WeededAst.RestrictableChoicePattern.Tag(qname, inner, loc)
        }
      case other => WeederError.UnsupportedRestrictedChoicePattern(star, p0.loc).toFailure
    }
    mapN(gVal, pVal) {
      case (_, p) => WeededAst.RestrictableChoiceRule(p, b0)
    }
  }

  /**
    * Returns a match lambda, i.e. a lambda with a pattern match on its arguments.
    *
    * This is also known as `ParsedAst.Expression.LambdaMatch`
    *
    * @param sp1 the position of the first character in the expression
    * @param p   the pattern of the parameter
    * @param e   the body of the lambda
    * @param sp2 the position of the last character in the expression
    * @return A lambda that matches on its parameter i.e. a `WeededAst.Expression.Lambda` that has a pattern match in its body.
    */
  private def mkLambdaMatch(sp1: SourcePosition, p: WeededAst.Pattern, e: WeededAst.Expression, sp2: SourcePosition)(implicit flix: Flix): WeededAst.Expression.Lambda = {
    val loc = mkSL(sp1, sp2).asSynthetic

    // The name of the lambda parameter.
    val ident = Name.Ident(sp1, "pat" + Flix.Delimiter + flix.genSym.freshId(), sp2).asSynthetic

    // Construct the body of the lambda expression.
    val varOrRef = WeededAst.Expression.Ambiguous(Name.mkQName(ident), loc)
    val rule = WeededAst.MatchRule(p, None, e)

    val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc)
    val body = WeededAst.Expression.Match(varOrRef, List(rule), loc)
    WeededAst.Expression.Lambda(fparam, body, loc)
  }

  /**
    * Performs weeding on the given argument.
    *
    * Named arguments are transformed into records.
    * `f(arg = x)` becomes `f({arg = x})`
    */
  private def visitArgument(arg: ParsedAst.Argument, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = arg match {
    // Case 1: Named parameter. Turn it into a record.
    case ParsedAst.Argument.Named(name, exp0, sp2) =>
      visitExp(exp0, senv) map {
        exp =>
          val loc = mkSL(name.sp1, sp2)
          WeededAst.Expression.RecordExtend(Name.mkField(name), exp, WeededAst.Expression.RecordEmpty(loc), loc)
      }
    // Case 2: Unnamed parameter. Just return it.
    case ParsedAst.Argument.Unnamed(exp) => visitExp(exp, senv)
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
    case class Operator(op: SemanticOperator) extends OperatorResult

    /**
      * The operator is unrecognized: it must have been defined elsewhere.
      */
    case class Unrecognized(ident: Name.Ident) extends OperatorResult
  }

  /**
    * Performs weeding on the given unary operator.
    */
  private def visitUnaryOperator(o: ParsedAst.Operator)(implicit flix: Flix): OperatorResult = o match {
    case ParsedAst.Operator(sp1, op, sp2) =>
      op match {
        case "not" => OperatorResult.Operator(SemanticOperator.BoolOp.Not)
        case "-" => OperatorResult.BuiltIn(Name.mkQName("Neg.neg", sp1, sp2))
        case "~~~" => OperatorResult.BuiltIn(Name.mkQName("BitwiseNot.not", sp1, sp2))
        case _ => OperatorResult.Unrecognized(Name.Ident(sp1, op, sp2))
      }
  }

  /**
    * Performs weeding on the given binary operator.
    */
  private def visitBinaryOperator(o: ParsedAst.Operator)(implicit flix: Flix): OperatorResult = o match {
    case ParsedAst.Operator(sp1, op, sp2) =>
      op match {
        case "+" => OperatorResult.BuiltIn(Name.mkQName("Add.add", sp1, sp2))
        case "-" => OperatorResult.BuiltIn(Name.mkQName("Sub.sub", sp1, sp2))
        case "*" => OperatorResult.BuiltIn(Name.mkQName("Mul.mul", sp1, sp2))
        case "/" => OperatorResult.BuiltIn(Name.mkQName("Div.div", sp1, sp2))
        case "**" => OperatorResult.BuiltIn(Name.mkQName("Exp.exp", sp1, sp2))
        case "<" => OperatorResult.BuiltIn(Name.mkQName("Order.less", sp1, sp2))
        case "<=" => OperatorResult.BuiltIn(Name.mkQName("Order.lessEqual", sp1, sp2))
        case ">" => OperatorResult.BuiltIn(Name.mkQName("Order.greater", sp1, sp2))
        case ">=" => OperatorResult.BuiltIn(Name.mkQName("Order.greaterEqual", sp1, sp2))
        case "==" => OperatorResult.BuiltIn(Name.mkQName("Eq.eq", sp1, sp2))
        case "!=" => OperatorResult.BuiltIn(Name.mkQName("Eq.neq", sp1, sp2))
        case "<=>" => OperatorResult.BuiltIn(Name.mkQName("Order.compare", sp1, sp2))
        case "and" => OperatorResult.Operator(SemanticOperator.BoolOp.And)
        case "or" => OperatorResult.Operator(SemanticOperator.BoolOp.Or)
        case "&&&" => OperatorResult.BuiltIn(Name.mkQName("BitwiseAnd.and", sp1, sp2))
        case "|||" => OperatorResult.BuiltIn(Name.mkQName("BitwiseOr.or", sp1, sp2))
        case "^^^" => OperatorResult.BuiltIn(Name.mkQName("BitwiseXor.xor", sp1, sp2))
        case "<<<" => OperatorResult.BuiltIn(Name.mkQName("BitwiseShl.shl", sp1, sp2))
        case ">>>" => OperatorResult.BuiltIn(Name.mkQName("BitwiseShr.shr", sp1, sp2))
        case _ => OperatorResult.Unrecognized(Name.Ident(sp1, op, sp2))
      }
  }

  /**
    * Translates the hex code into the corresponding character.
    * Returns an error if the code is not hexadecimal.
    */
  private def translateHexCode(code: String, loc: SourceLocation): Validation[Char, WeederError.MalformedUnicodeEscapeSequence] = {
    try {
      Integer.parseInt(code, 16).toChar.toSuccess
    } catch {
      case _: NumberFormatException => WeederError.MalformedUnicodeEscapeSequence(code, loc).toFailure
    }
  }

  /**
    * Performs weeding on the given sequence of CharCodes.
    */
  private def weedCharSequence(chars0: Seq[ParsedAst.CharCode]): Validation[String, WeederError.IllegalLiteral] = {

    @tailrec
    def visit(chars: List[ParsedAst.CharCode], acc: List[Char]): Validation[String, WeederError.IllegalLiteral] = {
      chars match {
        // Case 1: End of the sequence
        case Nil => acc.reverse.mkString.toSuccess
        // Case 2: Simple character literal
        case ParsedAst.CharCode.Literal(_, char, _) :: rest => visit(rest, char.head :: acc)
        // Case 3: Escape sequence
        case (esc@ParsedAst.CharCode.Escape(sp1, char, sp2)) :: rest => char match {
          // Cases 3.1: Standard escapes
          case "n" => visit(rest, '\n' :: acc)
          case "r" => visit(rest, '\r' :: acc)
          case "\\" => visit(rest, '\\' :: acc)
          case "\"" => visit(rest, '\"' :: acc)
          case "\'" => visit(rest, '\'' :: acc)
          case "t" => visit(rest, '\t' :: acc)

          // Case 3.2: Interpolation escape
          case "$" => rest match {
            case ParsedAst.CharCode.Literal(_, "{", _) :: rest2 => visit(rest2, '{' :: '$' :: acc)
            case _ => WeederError.InvalidEscapeSequence('$', mkSL(sp1, sp2)).toFailure
          }

          // Case 3.3 Debug escape
          case "%" => rest match {
            case ParsedAst.CharCode.Literal(_, "{", _) :: rest2 => visit(rest2, '{' :: '%' :: acc)
            case _ => WeederError.InvalidEscapeSequence('%', mkSL(sp1, sp2)).toFailure
          }

          // Case 3.3: Unicode escape
          case "u" => rest match {
            // Case 3.3.1: `\\u` followed by 4 or more literals
            case ParsedAst.CharCode.Literal(sp1, d0, _) ::
              ParsedAst.CharCode.Literal(_, d1, _) ::
              ParsedAst.CharCode.Literal(_, d2, _) ::
              ParsedAst.CharCode.Literal(_, d3, sp2) ::
              rest2 =>
              val code = List(d0, d1, d2, d3).mkString
              // Doing a manual flatMap to keep the function tail-recursive
              translateHexCode(code, mkSL(sp1, sp2)) match {
                case Validation.Success(char) => visit(rest2, char :: acc)
                case failure => Validation.Failure(failure.errors)
              }
            // Case 3.3.2: `\\u` followed by less than 4 literals
            case rest2 =>
              val code = rest2.takeWhile(_.isInstanceOf[ParsedAst.CharCode.Literal])
              val sp2 = code.lastOption.getOrElse(esc).sp2
              WeederError.MalformedUnicodeEscapeSequence(code.mkString, mkSL(esc.sp1, sp2)).toFailure
          }

          // Case 3.4: Invalid escape character
          case _ => WeederError.InvalidEscapeSequence(char.head, mkSL(sp1, sp2)).toFailure
        }
      }
    }

    visit(chars0.toList, Nil)
  }

  /**
    * Performs weeding on the given literal.
    */
  private def weedLiteral(lit0: ParsedAst.Literal)(implicit flix: Flix): Validation[Ast.Constant, WeederError.IllegalLiteral] = lit0 match {
    case ParsedAst.Literal.Unit(_, _) =>
      Ast.Constant.Unit.toSuccess

    case ParsedAst.Literal.Null(_, _) =>
      Ast.Constant.Null.toSuccess

    case ParsedAst.Literal.True(_, _) =>
      Ast.Constant.Bool(true).toSuccess

    case ParsedAst.Literal.False(_, _) =>
      Ast.Constant.Bool(false).toSuccess

    case ParsedAst.Literal.Char(sp1, chars, sp2) =>
      flatMapN(weedCharSequence(chars)) {
        case string if string.lengthIs == 1 => Ast.Constant.Char(string.head).toSuccess
        case string => WeederError.NonSingleCharacter(string, mkSL(sp1, sp2)).toFailure
      }

    case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
      toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Float32(lit)
      }

    case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
      toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Float64(lit)
      }

    case ParsedAst.Literal.BigDecimal(sp1, sign, before, after, power, sp2) =>
      toBigDecimal(sign, before, after, power, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.BigDecimal(lit)
      }

    case ParsedAst.Literal.Int8(sp1, sign, radix, digits, sp2) =>
      toInt8(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Int8(lit)
      }

    case ParsedAst.Literal.Int16(sp1, sign, radix, digits, sp2) =>
      toInt16(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Int16(lit)
      }

    case ParsedAst.Literal.Int32(sp1, sign, radix, digits, sp2) =>
      toInt32(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Int32(lit)
      }

    case ParsedAst.Literal.Int64(sp1, sign, radix, digits, sp2) =>
      toInt64(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.Int64(lit)
      }

    case ParsedAst.Literal.BigInt(sp1, sign, radix, digits, sp2) =>
      toBigInt(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => Ast.Constant.BigInt(lit)
      }

    case ParsedAst.Literal.Str(_, chars, _) =>
      weedCharSequence(chars) map {
        string => Ast.Constant.Str(string)
      }
  }

  /**
    * Compiles a parsed pattern into a weeded pattern.
    */
  private def visitPattern(pattern: ParsedAst.Pattern)(implicit flix: Flix): Validation[WeededAst.Pattern, WeederError] = {
    /*
     *  Check for non-linear pattern, i.e. if a variable occurs multiple times.
     */
    val seen = mutable.Map.empty[String, Name.Ident]

    /*
     * Local visitor.
     */
    def visit(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = pattern match {
      case ParsedAst.Pattern.Var(sp1, ident, sp2) =>
        // Check if the identifier is a wildcard.
        if (ident.name == "_") {
          WeededAst.Pattern.Wild(mkSL(sp1, sp2)).toSuccess
        } else {
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              WeededAst.Pattern.Var(ident, mkSL(sp1, sp2)).toSuccess
            case Some(otherIdent) =>
              NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
          }
        }

      case ParsedAst.Pattern.Lit(sp1, lit, sp2) =>
        flatMapN(weedLiteral(lit): Validation[Ast.Constant, WeederError]) {
          case Ast.Constant.Null => WeederError.IllegalNullPattern(mkSL(sp1, sp2)).toFailure
          case l => WeededAst.Pattern.Cst(l, mkSL(sp1, sp2)).toSuccess
        }

      case ParsedAst.Pattern.Tag(sp1, qname, o, sp2) =>
        /*
         * Introduce implicit unit, if needed.
         */
        o match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val lit = WeededAst.Pattern.Cst(Ast.Constant.Unit, loc.asSynthetic)
            WeededAst.Pattern.Tag(qname, lit, loc).toSuccess
          case Some(pat) => visit(pat) map {
            case p => WeededAst.Pattern.Tag(qname, p, mkSL(sp1, sp2))
          }
        }

      case ParsedAst.Pattern.Tuple(sp1, pats, sp2) =>
        val loc = mkSL(sp1, sp2)

        /*
         * Rewrites empty tuples to Unit and eliminate single-element tuples.
         */
        traverse(pats)(visit) map {
          case Nil => WeededAst.Pattern.Cst(Ast.Constant.Unit, loc)
          case x :: Nil => x
          case xs => WeededAst.Pattern.Tuple(xs, loc)
        }

      case ParsedAst.Pattern.FCons(pat1, sp1, sp2, pat2) =>
        /*
         * Rewrites a `FCons` pattern into a tag pattern.
         */
        mapN(visitPattern(pat1), visitPattern(pat2)) {
          case (hd, tl) =>
            val loc = mkSL(sp1, sp2)
            val qname = Name.mkQName("List.Cons", sp1, sp2)
            val pat = WeededAst.Pattern.Tuple(List(hd, tl), loc)
            WeededAst.Pattern.Tag(qname, pat, loc)
        }

    }

    visit(pattern)
  }

  /**
    * Performs weeding on the given constraint `c0`.
    */
  private def visitConstraint(c0: ParsedAst.Constraint, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Constraint, WeederError] = c0 match {
    case ParsedAst.Constraint(sp1, head0, body0, sp2) =>
      val headVal = visitHeadPredicate(head0, senv)
      val bodyVal = traverse(body0)(visitPredicateBody(_, senv))

      mapN(headVal, bodyVal) {
        case (h, bs) => WeededAst.Constraint(h, bs, mkSL(sp1, sp2))
      }
  }

  /**
    * Weeds the given head predicate.
    */
  private def visitHeadPredicate(past: ParsedAst.Predicate.Head, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Predicate.Head, WeederError] = past match {
    case ParsedAst.Predicate.Head.Atom(sp1, ident, terms, None, sp2) =>
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp(_, senv))) {
        case ts => WeededAst.Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, ts, loc)
      }

    case ParsedAst.Predicate.Head.Atom(sp1, ident, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp(_, senv)), visitExp(term, senv)) {
        case (ts, t) => WeededAst.Predicate.Head.Atom(Name.Pred(ident.name, mkSL(ident.sp1, ident.sp2)), Denotation.Latticenal, ts ::: t :: Nil, loc)
      }
  }

  /**
    * Weeds the given body predicate.
    */
  private def visitPredicateBody(b: ParsedAst.Predicate.Body, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Predicate.Body, WeederError] = b match {
    case ParsedAst.Predicate.Body.Atom(sp1, polarity, fixity, ident, terms, None, sp2) =>
      //
      // Check for `[[IllegalFixedAtom]]`.
      //
      if (polarity == Ast.Polarity.Negative && fixity == Ast.Fixity.Fixed) {
        return WeederError.IllegalFixedAtom(mkSL(sp1, sp2)).toFailure
      }

      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern)) {
        case ts =>
          WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, ts, loc)
      }

    case ParsedAst.Predicate.Body.Atom(sp1, polarity, fixity, ident, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern), visitPattern(term)) {
        case (ts, t) =>
          WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, fixity, ts ::: t :: Nil, loc)
      }

    case ParsedAst.Predicate.Body.Guard(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Predicate.Body.Guard(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Predicate.Body.Loop(sp1, idents, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Predicate.Body.Loop(idents.toList, e, mkSL(sp1, sp2))
      }

  }

  /**
    * Weeds the given sequence of parsed annotation `xs`.
    */
  private def visitAnnotations(xs: Seq[ParsedAst.Annotation])(implicit flix: Flix): Validation[Ast.Annotations, WeederError] = {
    // collect seen annotations.
    val seen = mutable.Map.empty[String, ParsedAst.Annotation]

    // loop through each annotation.
    val result = xs.toList.collect {
      case x: ParsedAst.Annotation => seen.get(x.ident.name) match {
        case None =>
          seen += (x.ident.name -> x)
          visitAnnotation(x.ident)
        case Some(otherAnn) =>
          val name = x.ident.name
          val loc1 = mkSL(otherAnn.sp1, otherAnn.sp2)
          val loc2 = mkSL(x.sp1, x.sp2)
          Failure(LazyList(
            // NB: We report an error at both source locations.
            DuplicateAnnotation(name, loc1, loc2),
            DuplicateAnnotation(name, loc2, loc1),
          ))
      }
    }

    sequence(result) map {
      case anns => Ast.Annotations(anns)
    }
  }

  /**
    * Performs weeding on the given annotation.
    */
  private def visitAnnotation(ident: Name.Ident)(implicit flix: Flix): Validation[Ast.Annotation, WeederError] = ident.name match {
    case "benchmark" => Ast.Annotation.Benchmark(ident.loc).toSuccess
    case "test" => Ast.Annotation.Test(ident.loc).toSuccess
    case "Test" => Ast.Annotation.Test(ident.loc).toSuccess
    case "Deprecated" => Ast.Annotation.Deprecated(ident.loc).toSuccess
    case "Experimental" => Ast.Annotation.Experimental(ident.loc).toSuccess
    case "Internal" => Ast.Annotation.Internal(ident.loc).toSuccess
    case "Parallel" => Ast.Annotation.Parallel(ident.loc).toSuccess
    case "ParallelWhenPure" => Ast.Annotation.ParallelWhenPure(ident.loc).toSuccess
    case "Lazy" => Ast.Annotation.Lazy(ident.loc).toSuccess
    case "LazyWhenPure" => Ast.Annotation.LazyWhenPure(ident.loc).toSuccess
    case "MustUse" => Ast.Annotation.MustUse(ident.loc).toSuccess
    case "Skip" => Ast.Annotation.Skip(ident.loc).toSuccess
    case "Unsafe" => Ast.Annotation.Unsafe(ident.loc).toSuccess
    case name => WeederError.UndefinedAnnotation(name, ident.loc).toFailure
  }

  /**
    * Weeds the given sequence of parsed modifiers `xs`.
    */
  private def visitModifiers(xs: Seq[ParsedAst.Modifier], legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifiers, WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Modifier]
    val modifiersVal = traverse(xs) {
      modifier =>
        seen.get(modifier.name) match {
          case None =>
            seen += (modifier.name -> modifier)
            visitModifier(modifier, legalModifiers)
          case Some(other) =>
            val name = modifier.name
            val loc1 = mkSL(other.sp1, other.sp2)
            val loc2 = mkSL(modifier.sp1, modifier.sp2)
            Failure(LazyList(
              // NB: We report an error at both source locations.
              WeederError.DuplicateModifier(name, loc1, loc2),
              WeederError.DuplicateModifier(name, loc2, loc1)
            ))
        }
    }

    modifiersVal.map(ms => Ast.Modifiers(ms))
  }

  /**
    * Weeds the given parsed modifier `m`.
    */
  private def visitModifier(m: ParsedAst.Modifier, legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifier, WeederError] = {
    val modifier = m.name match {
      case "lawful" => Ast.Modifier.Lawful
      case "opaque" => Ast.Modifier.Opaque
      case "override" => Ast.Modifier.Override
      case "pub" => Ast.Modifier.Public
      case "sealed" => Ast.Modifier.Sealed
      case s => throw InternalCompilerException(s"Unknown modifier '$s'.", mkSL(m.sp1, m.sp2))
    }

    //
    // Check for `IllegalModifier`.
    //
    if (legalModifiers contains modifier)
      modifier.toSuccess
    else
      IllegalModifier(mkSL(m.sp1, m.sp2)).toFailure
  }

  /**
    * Returns an error if `public` is not among the modifiers in `mods`.
    */
  private def requirePublic(mods: Seq[ParsedAst.Modifier], ident: Name.Ident): Validation[Unit, WeederError] = {
    if (mods.exists(_.name == "pub")) {
      ().toSuccess
    } else {
      WeederError.IllegalPrivateDeclaration(ident, ident.loc).toFailure
    }
  }

  /**
    * Returns an error if type parameters are present.
    */
  private def requireNoTypeParams(tparams0: ParsedAst.TypeParams): Validation[Unit, WeederError] = tparams0 match {
    case TypeParams.Elided => ().toSuccess
    case TypeParams.Explicit(tparams) =>
      // safe to take head and tail since parsing ensures nonempty type parameters if explicit
      val sp1 = tparams.head.sp1
      val sp2 = tparams.last.sp2
      WeederError.IllegalEffectTypeParams(mkSL(sp1, sp2)).toFailure
  }

  /**
    * Returns an error if an effect is present.
    */
  private def requireNoEffect(purAndEff: ParsedAst.PurityAndEffect, loc: SourceLocation): Validation[Unit, WeederError] = purAndEff match {
    case ParsedAst.PurityAndEffect(None, None) => ().toSuccess
    case ParsedAst.PurityAndEffect(_, _) => WeederError.IllegalOperationEffect(loc).toFailure
  }

  /**
    * Returns an error if the type is not Unit.
    */
  private def requireUnit(tpe: ParsedAst.Type, loc: SourceLocation): Validation[Unit, WeederError] = tpe match {
    case ParsedAst.Type.Ambiguous(_, name, _) if name.isUnqualified && name.ident.name == "Unit" => ().toSuccess
    case _ => WeederError.NonUnitOperationType(loc).toFailure
  }

  /**
    * Weeds the given parsed type `tpe`.
    */
  private def visitType(tpe: ParsedAst.Type): WeededAst.Type = tpe match {
    case ParsedAst.Type.Var(sp1, ident, sp2) => visitEffectIdent(ident)

    case ParsedAst.Type.Ambiguous(sp1, qname, sp2) => WeededAst.Type.Ambiguous(qname, mkSL(sp1, sp2))

    case ParsedAst.Type.Tuple(sp1, elms, sp2) => WeededAst.Type.Tuple(elms.toList.map(visitType), mkSL(sp1, sp2))

    case ParsedAst.Type.Record(sp1, fields, restOpt, sp2) =>
      val row = buildRecordRow(fields, restOpt, mkSL(sp1, sp2))
      WeededAst.Type.Record(row, mkSL(sp1, sp2))

    case ParsedAst.Type.RecordRow(sp1, fields, restOpt, sp2) =>
      buildRecordRow(fields, restOpt, mkSL(sp1, sp2))

    case ParsedAst.Type.Schema(sp1, predicates, restOpt, sp2) =>
      val row = buildSchemaRow(predicates, restOpt, mkSL(sp1, sp2))
      WeededAst.Type.Schema(row, mkSL(sp1, sp2))

    case ParsedAst.Type.SchemaRow(sp1, predicates, restOpt, sp2) =>
      buildSchemaRow(predicates, restOpt, mkSL(sp1, sp2))

    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, tpe2, purAndEff0, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      val purAndEff = visitPurityAndEffect(purAndEff0)
      mkArrow(t1, purAndEff, t2, loc)

    case ParsedAst.Type.PolymorphicArrow(sp1, tparams, tresult, purAndEff0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ts = tparams.map(visitType)
      val tr = visitType(tresult)
      val purAndEff = visitPurityAndEffect(purAndEff0)
      mkCurriedArrow(ts, purAndEff, tr, loc)

    case ParsedAst.Type.Native(sp1, fqn, sp2) =>
      WeededAst.Type.Native(fqn.toString, mkSL(sp1, sp2))

    case ParsedAst.Type.Apply(t1, args, sp2) =>
      // Curry the type arguments.
      val sp1 = leftMostSourcePosition(t1)
      args.foldLeft(visitType(t1)) {
        case (acc, t2) => WeededAst.Type.Apply(acc, visitType(t2), mkSL(sp1, sp2))
      }

    case ParsedAst.Type.True(sp1, sp2) =>
      WeededAst.Type.True(mkSL(sp1, sp2))

    case ParsedAst.Type.False(sp1, sp2) =>
      WeededAst.Type.False(mkSL(sp1, sp2))

    case ParsedAst.Type.Not(sp1, tpe, sp2) =>
      val t = visitType(tpe)
      WeededAst.Type.Not(t, mkSL(sp1, sp2))

    case ParsedAst.Type.And(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.And(t1, t2, mkSL(sp1, sp2))

    case ParsedAst.Type.Or(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.Or(t1, t2, mkSL(sp1, sp2))

    case ParsedAst.Type.Effect(sp1, eff0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val effs = visitEffectSet(eff0)
      // NB: safe to reduce since effs is never empty
      val effOpt = effs.reduceLeftOption({
        case (acc, eff) => WeededAst.Type.Union(acc, eff, loc)
      }: (WeededAst.Type, WeededAst.Type) => WeededAst.Type)
      effOpt.getOrElse(WeededAst.Type.Empty(loc))

    case ParsedAst.Type.CaseSet(sp1, cases, sp2) =>
      val loc = mkSL(sp1, sp2)
      WeededAst.Type.CaseSet(cases.toList, loc)

    case ParsedAst.Type.CaseUnion(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.CaseUnion(t1, t2, loc)

    case ParsedAst.Type.CaseIntersection(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.CaseIntersection(t1, t2, loc)

    case ParsedAst.Type.CaseDifference(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.CaseIntersection(t1, WeededAst.Type.CaseComplement(t2, loc), loc)

    case ParsedAst.Type.CaseComplement(sp1, tpe, sp2) =>
      val loc = mkSL(sp1, sp2)
      val t = visitType(tpe)
      WeededAst.Type.CaseComplement(t, loc)

    case ParsedAst.Type.Ascribe(tpe, kind, sp2) =>
      val sp1 = leftMostSourcePosition(tpe)
      val t = visitType(tpe)
      val k = visitKind(kind)
      WeededAst.Type.Ascribe(t, k, mkSL(sp1, sp2))
  }

  /**
    * Builds a record row from the given fields and optional rest variable.
    */
  private def buildRecordRow(fields: Seq[ParsedAst.RecordFieldType], restOpt: Option[Name.Ident], loc: SourceLocation): WeededAst.Type = {
    // If rest is absent, then it is the empty record row
    val rest = restOpt match {
      case None => WeededAst.Type.RecordRowEmpty(loc)
      case Some(name) => WeededAst.Type.Var(name, name.loc)
    }

    fields.foldRight(rest) {
      case (ParsedAst.RecordFieldType(ssp1, ident, t, ssp2), acc) =>
        WeededAst.Type.RecordRowExtend(Name.mkField(ident), visitType(t), acc, mkSL(ssp1, ssp2))
    }
  }

  /**
    * Builds a schema row from the given predicates and optional rest identifier.
    */
  private def buildSchemaRow(predicates: Seq[ParsedAst.PredicateType], restOpt: Option[Name.Ident], loc: SourceLocation): WeededAst.Type = {
    // If rest is absent, then it is the empty schema row
    val rest = restOpt match {
      case None => WeededAst.Type.SchemaRowEmpty(loc)
      case Some(name) => WeededAst.Type.Var(name, name.loc)
    }

    predicates.foldRight(rest) {
      case (ParsedAst.PredicateType.PredicateWithAlias(ssp1, qname, targs, ssp2), acc) =>
        val ts = targs match {
          case None => Nil
          case Some(xs) => xs.map(visitType).toList
        }
        WeededAst.Type.SchemaRowExtendByAlias(qname, ts, acc, mkSL(ssp1, ssp2))

      case (ParsedAst.PredicateType.RelPredicateWithTypes(ssp1, name, ts, ssp2), acc) =>
        WeededAst.Type.SchemaRowExtendByTypes(name, Ast.Denotation.Relational, ts.toList.map(visitType), acc, mkSL(ssp1, ssp2))

      case (ParsedAst.PredicateType.LatPredicateWithTypes(ssp1, name, ts, tpe, ssp2), acc) =>
        WeededAst.Type.SchemaRowExtendByTypes(name, Ast.Denotation.Latticenal, ts.toList.map(visitType) ::: visitType(tpe) :: Nil, acc, mkSL(ssp1, ssp2))
    }
  }

  /**
    * Returns an arrow type from `tpe1` to `tpe2` with effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->{eff} tpe2`
    */
  private def mkArrow(tpe1: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect, tpe2: WeededAst.Type, loc: SourceLocation): WeededAst.Type =
    WeededAst.Type.Arrow(List(tpe1), purAndEff, tpe2, loc.asSynthetic)

  /**
    * Returns a sequence of arrow types type from `tparams` to `tresult` where every arrow is pure except the last which has effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->> tpe2 ->> ... ->{eff} tresult`.
    */
  private def mkCurriedArrow(tparams: Seq[WeededAst.Type], purAndEff: WeededAst.PurityAndEffect, tresult: WeededAst.Type, loc: SourceLocation): WeededAst.Type = {
    val l = loc.asSynthetic
    val base = mkArrow(tparams.last, purAndEff, tresult, l)
    tparams.init.foldRight(base)(mkArrow(_, WeededAst.PurityAndEffect(None, None), _, l))
  }

  /**
    * Constructs the type equivalent to the type (tpe1 - tpe2)
    */
  private def mkDifference(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation): WeededAst.Type = {
    // (ef1 - ef2) is sugar for (ef1 && (~ef2))
    WeededAst.Type.Intersection(
      tpe1,
      WeededAst.Type.Complement(tpe2, loc),
      loc
    )
  }

  /**
    * Weeds the given parsed optional purity and effect `purAndEff`.
    */
  private def visitPurityAndEffect(purAndEff: ParsedAst.PurityAndEffect): WeededAst.PurityAndEffect = purAndEff match {
    case ParsedAst.PurityAndEffect(pur0, eff0) =>
      val pur = pur0.map(visitType)
      val eff = eff0.map(visitEffectSet)
      WeededAst.PurityAndEffect(pur, eff)
  }

  /**
    * Weeds the given effect set.
    */
  private def visitEffectSet(eff0: ParsedAst.EffectSet): List[WeededAst.Type] = eff0 match {
    case EffectSet.Singleton(_, eff, _) => List(visitSingleEffect(eff))
    case EffectSet.Pure(sp1, sp2) => Nil
    case EffectSet.Set(sp1, effs0, sp2) => effs0.map(visitSingleEffect).toList
  }

  /**
    * Weeds the given single effect.
    */
  private def visitSingleEffect(eff0: ParsedAst.Effect): WeededAst.Type = {
    val leftSp = leftMostSourcePosition(eff0)
    val loc = mkSL(leftSp, rightMostSourcePosition(eff0))

    eff0 match {
      case ParsedAst.Effect.Var(sp1, ident, sp2) => visitEffectIdent(ident)

      case ParsedAst.Effect.Read(sp1, idents, sp2) =>
        idents.map(ident => WeededAst.Type.Read(WeededAst.Type.Var(ident, ident.loc), ident.loc): WeededAst.Type)
          .reduceOption(WeededAst.Type.And(_, _, loc))
          .getOrElse(WeededAst.Type.True(loc))

      case ParsedAst.Effect.Write(sp1, idents, sp2) =>
        idents.map(ident => WeededAst.Type.Write(WeededAst.Type.Var(ident, ident.loc), ident.loc): WeededAst.Type)
          .reduceOption(WeededAst.Type.And(_, _, loc))
          .getOrElse(WeededAst.Type.True(loc))

      case ParsedAst.Effect.Impure(sp1, sp2) => WeededAst.Type.False(mkSL(sp1, sp2))

      case ParsedAst.Effect.Eff(sp1, name, sp2) => WeededAst.Type.Ambiguous(name, loc)

      case ParsedAst.Effect.Complement(sp1, eff, sp2) =>
        val innerEff = visitSingleEffect(eff)
        WeededAst.Type.Complement(innerEff, loc)

      case ParsedAst.Effect.Union(eff1, effs) =>
        val innerEff1 = visitSingleEffect(eff1)
        effs.foldLeft(innerEff1) {
          case (acc, innerEff0) =>
            val innerEff = visitSingleEffect(innerEff0)
            val innerLoc = mkSL(leftSp, rightMostSourcePosition(innerEff0))
            WeededAst.Type.Union(acc, innerEff, innerLoc)
        }

      case ParsedAst.Effect.Intersection(eff1, effs) =>
        val innerEff1 = visitSingleEffect(eff1)
        effs.foldLeft(innerEff1) {
          case (acc, innerEff0) =>
            val innerEff = visitSingleEffect(innerEff0)
            val innerLoc = mkSL(leftSp, rightMostSourcePosition(innerEff0))
            WeededAst.Type.Intersection(acc, innerEff, innerLoc)
        }

      case ParsedAst.Effect.Difference(eff1, effs) =>
        val innerEff1 = visitSingleEffect(eff1)
        effs.foldLeft(innerEff1) {
          case (acc, innerEff0) =>
            val innerEff = visitSingleEffect(innerEff0)
            val innerLoc = mkSL(leftSp, rightMostSourcePosition(innerEff0))
            mkDifference(acc, innerEff, innerLoc)
        }
    }
  }

  /**
    * Weeds the given list of formal parameter `fparams`.
    *
    * Checks for [[MissingFormalParamAscription]] and [[DuplicateFormalParam]].
    */
  private def visitFormalParams(fparams: Seq[ParsedAst.FormalParam], typePresence: Presence): Validation[List[WeededAst.FormalParam], WeederError] = {
    //
    // Special Case: Check if no formal parameters are present. If so, introduce a unit parameter.
    //
    if (fparams.isEmpty) {
      val sp1 = SourcePosition.Unknown
      val sp2 = SourcePosition.Unknown
      val loc = mkSL(sp1, sp2)
      val ident = Name.Ident(sp1, "_unit", sp2)
      val tpe = Some(WeededAst.Type.Unit(loc))
      return List(WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe, loc)).toSuccess
    }

    val seen = mutable.Map.empty[String, ParsedAst.FormalParam]

    traverse(fparams) {
      case param@ParsedAst.FormalParam(sp1, mods, ident, typeOpt, sp2) => seen.get(ident.name) match {
        case None =>
          if (!ident.name.startsWith("_")) {
            // Wildcards cannot be duplicate.
            seen += (ident.name -> param)
          }

          flatMapN(visitModifiers(mods, legalModifiers = Set.empty)) {
            case mod =>
              (typeOpt, typePresence) match {
                // Case 1: Required but missing. Error.
                case (None, Presence.Required) => MissingFormalParamAscription(ident.name, mkSL(sp1, sp2)).toFailure
                // Case 2: Forbidden but present. Error.
                case (Some(tpe), Presence.Forbidden) => IllegalFormalParamAscription(mkSL(sp1, sp2)).toFailure
                // Case 3: No violation. Good to go.
                case _ => WeededAst.FormalParam(ident, mod, typeOpt.map(visitType), mkSL(sp1, sp2)).toSuccess
              }
          }
        case Some(otherParam) =>
          val name = ident.name
          val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
          val loc2 = mkSL(param.sp1, param.sp2)
          Failure(LazyList(
            // NB: We report an error at both source locations.
            DuplicateFormalParam(name, loc1, loc2),
            DuplicateFormalParam(name, loc2, loc1)
          ))
      }
    }
  }

  /**
    * Weeds the given predicate param `pparam`.
    */
  private def visitPredicateParam(pparam: ParsedAst.PredicateParam): WeededAst.PredicateParam = pparam match {
    case ParsedAst.PredicateParam.UntypedPredicateParam(sp1, ident, sp2) =>
      val pred = Name.mkPred(ident)
      WeededAst.PredicateParam.PredicateParamUntyped(pred, mkSL(sp1, sp2))

    case ParsedAst.PredicateParam.RelPredicateParam(sp1, ident, tpes, sp2) =>
      val pred = Name.mkPred(ident)
      val den = Ast.Denotation.Relational
      val ts = tpes.map(visitType).toList
      WeededAst.PredicateParam.PredicateParamWithType(pred, den, ts, mkSL(sp1, sp2))

    case ParsedAst.PredicateParam.LatPredicateParam(sp1, ident, tpes, tpe, sp2) =>
      val pred = Name.mkPred(ident)
      val den = Ast.Denotation.Latticenal
      val ts = tpes.map(visitType).toList
      val t = visitType(tpe)
      WeededAst.PredicateParam.PredicateParamWithType(pred, den, ts ::: t :: Nil, mkSL(sp1, sp2))
  }

  /**
    * Weeds the given documentation.
    */
  private def visitDoc(doc0: ParsedAst.Doc): Ast.Doc = {
    val trimmedLines = doc0.lines.map(_.trim)
    val trimmedBeginning = trimmedLines.dropWhile(_ == "")
    val trimmedEnd = trimmedBeginning.reverse.dropWhile(_ == "").reverse
    Ast.Doc(trimmedEnd.toList, mkSL(doc0.sp1, doc0.sp2))
  }

  /**
    * Weeds the given type parameters `tparams0`.
    */
  private def visitTypeParams(tparams0: ParsedAst.TypeParams): Validation[WeededAst.TypeParams, WeederError] = tparams0 match {
    case ParsedAst.TypeParams.Elided => WeededAst.TypeParams.Elided.toSuccess
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        // Case 1: only unkinded type parameters
        case (Nil, _ :: _) => WeededAst.TypeParams.Unkinded(unkindedTypeParams).toSuccess
        // Case 2: only kinded type parameters
        case (_ :: _, Nil) => WeededAst.TypeParams.Kinded(kindedTypeParams).toSuccess
        // Case 3: some unkinded and some kinded
        case (_ :: _, _ :: _) =>
          val loc = mkSL(tparams.head.sp1, tparams.last.sp2)
          WeederError.InconsistentTypeParameters(loc).toFailure
        // Case 4: no type parameters: should be prevented by parser
        case (Nil, Nil) => throw InternalCompilerException("Unexpected empty type parameters.", SourceLocation.Unknown)
      }
  }

  /**
    * Weeds the type params, requiring that they be explicitly kinded.
    */
  private def visitKindedTypeParams(tparams0: ParsedAst.TypeParams): Validation[WeededAst.KindedTypeParams, WeederError] = tparams0 match {
    case ParsedAst.TypeParams.Elided => WeededAst.TypeParams.Elided.toSuccess
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        // Case 1: some unkinded type params
        case (_, _ :: _) =>
          val loc = mkSL(tparams.head.sp1, tparams.last.sp2)
          WeederError.UnkindedTypeParameters(loc).toFailure
        // Case 2: only kinded type parameters
        case (_ :: _, Nil) => WeededAst.TypeParams.Kinded(kindedTypeParams).toSuccess
        // Case 3: no type parameters: should be prevented by parser
        case (Nil, Nil) => throw InternalCompilerException("Unexpected empty type parameters.", SourceLocation.Unknown)
      }
  }

  /**
    * Weeds the given type param `tparam`.
    */
  private def visitTypeParam(tparam0: ParsedAst.TypeParam): WeededAst.TypeParam = tparam0 match {
    case ParsedAst.TypeParam(_, ident, kind0, _) =>
      kind0.map(visitKind) match {
        case None => WeededAst.TypeParam.Unkinded(ident)
        case Some(kind) => WeededAst.TypeParam.Kinded(ident, kind)
      }
  }

  /**
    * Weeds the given kind `kind`.
    */
  private def visitKind(kind: ParsedAst.Kind): WeededAst.Kind = kind match {
    case ParsedAst.Kind.QName(sp1, qname, sp2) => WeededAst.Kind.Ambiguous(qname, mkSL(sp1, sp2))
    case ParsedAst.Kind.Arrow(k1, k2, sp2) =>
      val sp1 = leftMostSourcePosition(k1)
      WeededAst.Kind.Arrow(visitKind(k1), visitKind(k2), mkSL(sp1, sp2))
  }

  /**
    * Weeds the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: ParsedAst.TypeConstraint): Validation[WeededAst.TypeConstraint, WeederError] = tconstr match {
    case ParsedAst.TypeConstraint(sp1, clazz, tparam0, sp2) =>
      val tpe = visitType(tparam0)
      if (isAllVars(tpe)) {
        WeededAst.TypeConstraint(clazz, tpe, mkSL(sp1, sp2)).toSuccess
      } else {
        WeederError.IllegalTypeConstraintParameter(mkSL(sp1, sp2)).toFailure
      }
  }

  /**
    * Performs weeding on the given name `ident`.
    */
  private def visitName(ident: Name.Ident): Validation[Unit, WeederError] = {
    if (ReservedWords.contains(ident.name)) {
      WeederError.ReservedName(ident, ident.loc).toFailure
    } else {
      ().toSuccess
    }
  }

  /**
    * Performs weeding on the given effect `ident`.
    * Checks whether it is actually the keyword `Static`.
    */
  private def visitEffectIdent(ident: Name.Ident): WeededAst.Type = {
    if (ident.name == "Static")
      WeededAst.Type.False(ident.loc)
    else
      WeededAst.Type.Var(ident, ident.loc)
  }

  /**
    * Performs weeding on the given JvmMethod
    */
  private def visitJvmMethod(method: ParsedAst.JvmMethod, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.JvmMethod, WeederError] = method match {
    case ParsedAst.JvmMethod(sp1, ident, fparams0, tpe, purAndEff, exp0, sp2) =>
      val tpeVal = visitType(tpe)
      val purAndEffVal = visitPurityAndEffect(purAndEff)
      mapN(visitFormalParams(fparams0, Presence.Required), visitExp(exp0, senv)) {
        case (fparams, exp) => WeededAst.JvmMethod(ident, fparams, exp, tpeVal, purAndEffVal, mkSL(sp1, sp2))
      }
  }

  /**
    * Returns true iff the type is composed only of type variables possibly applied to other type variables.
    */
  private def isAllVars(tpe: WeededAst.Type): Boolean = tpe match {
    case _: WeededAst.Type.Var => true
    case WeededAst.Type.Apply(tpe1, tpe2, _) => isAllVars(tpe1) && isAllVars(tpe2)
    case _ => false
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  private def mkApplyFqn(fqn: String, args: List[WeededAst.Expression], loc: SourceLocation): WeededAst.Expression = {
    val l = loc.asSynthetic
    val lambda = WeededAst.Expression.Ambiguous(Name.mkQName(fqn), l)
    WeededAst.Expression.Apply(lambda, args, l)
  }

  /**
    * Returns a curried version of the given expression `e` for each formal parameter in `fparams0`.
    */
  private def mkCurried(fparams0: List[WeededAst.FormalParam], e: WeededAst.Expression, loc: SourceLocation): WeededAst.Expression = {
    val l = loc.asSynthetic
    fparams0.foldRight(e) {
      case (fparam, acc) => WeededAst.Expression.Lambda(fparam, acc, l)
    }
  }

  /**
    * Returns the list of expressions `args0` unless the list is empty.
    *
    * If so, returns a list with a single unit expression.
    */
  private def getArguments(args0: List[WeededAst.Expression], loc: SourceLocation): List[WeededAst.Expression] = {
    val l = loc.asSynthetic
    args0 match {
      case Nil => List(WeededAst.Expression.Cst(Ast.Constant.Unit, l))
      case as => as
    }
  }

  /**
    * Returns the given expression `exp0` optionally wrapped in a type ascription if `tpe0` is `Some`.
    */
  private def withAscription(exp0: WeededAst.Expression, tpe0: Option[ParsedAst.Type])(implicit flix: Flix): WeededAst.Expression = {
    val l = exp0.loc.asSynthetic
    tpe0 match {
      case None => exp0
      case Some(t) => WeededAst.Expression.Ascribe(exp0, Some(visitType(t)), WeededAst.PurityAndEffect(None, None), l)
    }
  }

  /**
    * Removes underscores from the given string of digits.
    */
  private def stripUnderscores(digits: String): String = {
    digits.filterNot(_ == '_')
  }

  /**
    * Attempts to parse the given float32 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat32(sign: String, before: String, after: String, loc: SourceLocation): Validation[Float, WeederError.IllegalFloat] = try {
    val s = s"$sign$before.$after"
    stripUnderscores(s).toFloat.toSuccess
  } catch {
    case _: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat64(sign: String, before: String, after: String, loc: SourceLocation): Validation[Double, WeederError.IllegalFloat] = try {
    val s = s"$sign$before.$after"
    stripUnderscores(s).toDouble.toSuccess
  } catch {
    case _: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given big decimal with `sign` digits `before` and `after` the comma.
    */
  private def toBigDecimal(sign: String, before: String, after: Option[String], power: Option[String], loc: SourceLocation): Validation[BigDecimal, WeederError.IllegalFloat] = try {
    val frac = after match {
      case Some(digits) => "." + digits
      case None => ""
    }
    val pow = power match {
      case Some(digits) => "e" + digits
      case None => ""
    }
    val s = s"$sign$before$frac$pow"
    new BigDecimal(stripUnderscores(s)).toSuccess
  } catch {
    case _: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  private def toInt8(sign: String, radix: Int, digits: String, loc: SourceLocation): Validation[Byte, WeederError.IllegalInt] = try {
    val s = sign + digits
    JByte.parseByte(stripUnderscores(s), radix).toSuccess
  } catch {
    case _: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  private def toInt16(sign: String, radix: Int, digits: String, loc: SourceLocation): Validation[Short, WeederError.IllegalInt] = try {
    val s = sign + digits
    JShort.parseShort(stripUnderscores(s), radix).toSuccess
  } catch {
    case _: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  private def toInt32(sign: String, radix: Int, digits: String, loc: SourceLocation): Validation[Int, WeederError.IllegalInt] = try {
    val s = sign + digits
    JInt.parseInt(stripUnderscores(s), radix).toSuccess
  } catch {
    case _: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  private def toInt64(sign: String, radix: Int, digits: String, loc: SourceLocation): Validation[Long, WeederError.IllegalInt] = try {
    val s = sign + digits
    JLong.parseLong(stripUnderscores(s), radix).toSuccess
  } catch {
    case _: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  private def toBigInt(sign: String, radix: Int, digits: String, loc: SourceLocation): Validation[BigInteger, WeederError.IllegalInt] = try {
    val s = sign + digits
    new BigInteger(stripUnderscores(s), radix).toSuccess
  } catch {
    case _: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Alias for SourceLocation.mk
    */
  private def mkSL(sp1: SourcePosition, sp2: SourcePosition): SourceLocation = SourceLocation.mk(sp1, sp2)

  /**
    * Returns the left most source position in the sub-tree of the expression `e`.
    */
  @tailrec
  private def leftMostSourcePosition(e: ParsedAst.Expression): SourcePosition = e match {
    case ParsedAst.Expression.QName(sp1, _, _) => sp1
    case ParsedAst.Expression.Open(sp1, _, _) => sp1
    case ParsedAst.Expression.OpenAs(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Hole(sp1, _, _) => sp1
    case ParsedAst.Expression.HolyName(ident, _) => ident.sp1
    case ParsedAst.Expression.Use(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Lit(sp1, _, _) => sp1
    case ParsedAst.Expression.Intrinsic(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Apply(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Infix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LambdaMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Stm(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Discard(sp1, _, _) => sp1
    case ParsedAst.Expression.ApplicativeFor(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ForEach(sp1, _, _, _) => sp1
    case ParsedAst.Expression.MonadicFor(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ForEachYield(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatchStar(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetRecDef(sp1, _, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetImport(sp1, _, _, _) => sp1
    case ParsedAst.Expression.NewObject(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Static(sp1, _) => sp1
    case ParsedAst.Expression.Scope(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.RelationalChoose(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.RestrictableChoose(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.TypeMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordLit(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordSelect(base, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.RecordSelectLambda(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordOperation(sp1, _, _, _) => sp1
    case ParsedAst.Expression.VectorLit(sp1, _, _) => sp1
    case ParsedAst.Expression.FCons(hd, _, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FAppend(fst, _, _, _) => leftMostSourcePosition(fst)
    case ParsedAst.Expression.ArrayLit(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ListLit(sp1, _, _) => sp1
    case ParsedAst.Expression.SetLit(sp1, _, _) => sp1
    case ParsedAst.Expression.MapLit(sp1, _, _) => sp1
    case ParsedAst.Expression.Interpolation(sp1, _, _) => sp1
    case ParsedAst.Expression.Ref(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Deref(sp1, _, _) => sp1
    case ParsedAst.Expression.Assign(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Ascribe(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.UncheckedCast(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.UncheckedMaskingCast(sp1, _, _) => sp1
    case ParsedAst.Expression.CheckedTypeCast(sp1, _, _) => sp1
    case ParsedAst.Expression.CheckedEffectCast(sp1, _, _) => sp1
    case ParsedAst.Expression.Without(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Do(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Resume(sp1, _, _) => sp1
    case ParsedAst.Expression.Try(sp1, _, _, _) => sp1
    case ParsedAst.Expression.SelectChannel(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Spawn(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Par(sp1, _, _) => sp1
    case ParsedAst.Expression.ParYield(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Lazy(sp1, _, _) => sp1
    case ParsedAst.Expression.Force(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraint(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraintSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointLambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointCompose(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.FixpointInjectInto(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointSolveWithProject(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointQueryWithSelect(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.Debug(sp1, _, _, _) => sp1
  }

  /**
    * Returns the left most source position in the sub-tree of the type `tpe`.
    */
  @tailrec
  private def leftMostSourcePosition(tpe: ParsedAst.Type): SourcePosition = tpe match {
    case ParsedAst.Type.Var(sp1, _, _) => sp1
    case ParsedAst.Type.Ambiguous(sp1, _, _) => sp1
    case ParsedAst.Type.Tuple(sp1, _, _) => sp1
    case ParsedAst.Type.Record(sp1, _, _, _) => sp1
    case ParsedAst.Type.RecordRow(sp1, _, _, _) => sp1
    case ParsedAst.Type.Schema(sp1, _, _, _) => sp1
    case ParsedAst.Type.SchemaRow(sp1, _, _, _) => sp1
    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, _, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.PolymorphicArrow(sp1, _, _, _, _) => sp1
    case ParsedAst.Type.Native(sp1, _, _) => sp1
    case ParsedAst.Type.Apply(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.True(sp1, _) => sp1
    case ParsedAst.Type.False(sp1, _) => sp1
    case ParsedAst.Type.Not(sp1, _, _) => sp1
    case ParsedAst.Type.And(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Or(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Effect(sp1, _, _) => sp1
    case ParsedAst.Type.CaseComplement(sp1, _, _) => sp1
    case ParsedAst.Type.CaseDifference(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.CaseIntersection(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.CaseSet(sp1, _, _) => sp1
    case ParsedAst.Type.CaseUnion(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Ascribe(tpe, _, _) => leftMostSourcePosition(tpe)
  }

  @tailrec
  private def leftMostSourcePosition(eff: ParsedAst.Effect): SourcePosition = eff match {
    case Effect.Var(sp1, _, _) => sp1
    case Effect.Read(sp1, _, _) => sp1
    case Effect.Write(sp1, _, _) => sp1
    case Effect.Impure(sp1, _) => sp1
    case Effect.Eff(sp1, _, _) => sp1
    case Effect.Complement(sp1, _, _) => sp1
    case Effect.Union(eff1, _) => leftMostSourcePosition(eff1)
    case Effect.Intersection(eff1, _) => leftMostSourcePosition(eff1)
    case Effect.Difference(eff1, _) => leftMostSourcePosition(eff1)
  }

  @tailrec
  private def rightMostSourcePosition(eff: ParsedAst.Effect): SourcePosition = eff match {
    case Effect.Var(_, _, sp2) => sp2
    case Effect.Read(_, _, sp2) => sp2
    case Effect.Write(_, _, sp2) => sp2
    case Effect.Impure(_, sp2) => sp2
    case Effect.Eff(_, _, sp2) => sp2
    case Effect.Complement(_, _, sp2) => sp2
    case Effect.Union(eff1, effs) => rightMostSourcePosition(effs.lastOption.getOrElse(eff1))
    case Effect.Intersection(eff1, effs) => rightMostSourcePosition(effs.lastOption.getOrElse(eff1))
    case Effect.Difference(eff1, effs) => rightMostSourcePosition(effs.lastOption.getOrElse(eff1))
  }

  /**
    * Returns the left most source position in the sub-tree of the kind `kind`.
    */
  @tailrec
  private def leftMostSourcePosition(kind: ParsedAst.Kind): SourcePosition = kind match {
    case ParsedAst.Kind.QName(sp1, _, _) => sp1
    case ParsedAst.Kind.Arrow(k1, _, _) => leftMostSourcePosition(k1)
  }

  /**
    * Returns the class and member name constructed from the given fully-qualified name `fqn`.
    */
  private def parseClassAndMember(fqn: Name.JavaName): Validation[(String, String), WeederError] = fqn match {
    case Name.JavaName(sp1, components, sp2) =>
      // Ensure that the fqn has at least two components.
      if (components.length == 1) {
        return WeederError.IllegalJvmFieldOrMethodName(mkSL(sp1, sp2)).toFailure
      }

      // Compute the class and member name.
      val className = components.dropRight(1).mkString(".")
      val memberName = components.last

      (className, memberName).toSuccess
  }

  /**
    * The syntactic environment of an expression.
    *
    * Used to indicate which expressions are allowed at the given point in the syntax tree.
    */
  private sealed trait SyntacticEnv

  private object SyntacticEnv {
    /**
      * Indicates a top-level expression, not inside a handler.
      */
    case object Top extends SyntacticEnv

    /**
      * Indicates an expression inside a handler.
      */
    case object Handler extends SyntacticEnv
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
