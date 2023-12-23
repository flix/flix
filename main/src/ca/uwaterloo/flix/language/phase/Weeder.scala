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
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Denotation}
import ca.uwaterloo.flix.language.ast.ParsedAst.TypeParams
import ca.uwaterloo.flix.language.ast.WeededAst.Pattern
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.language.errors._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal, BigInteger}
import java.util.regex.{PatternSyntaxException, Pattern => JPattern}
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
    "!=", "*", "**", "+", "-", "..", "/", ":", "::", ":::", ":=", "<", "<+>", "<-", "<=",
    "<=>", "==", "=>", ">", ">=", "???", "@", "Absent", "Bool", "Impure", "Nil", "Predicate", "Present", "Pure",
    "RecordRow", "Region", "SchemaRow", "Type", "alias", "case", "catch", "chan",
    "class", "def", "deref", "else", "enum", "false", "fix", "force",
    "if", "import", "inline", "instance", "instanceof", "into", "law", "lawful", "lazy", "let", "let*", "match",
    "null", "override", "pub", "ref", "region",
    "sealed", "set", "spawn", "Static", "trait", "true",
    "type", "use", "where", "with", "discard", "object"
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

      ParOps.parTraverseValues(stale)(visitCompilationUnit).map {
        result =>
          val m = fresh ++ result
          WeededAst.Root(m, root.entryPoint, root.names)
      }
    }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def visitCompilationUnit(unit: ParsedAst.CompilationUnit)(implicit flix: Flix): Validation[WeededAst.CompilationUnit, WeederError] = {
    val usesAndImportsVal = traverse(unit.usesOrImports)(visitUseOrImport)
    val declarationsVal = traverse(unit.decls)(visitDecl)
    val loc = mkSL(unit.sp1, unit.sp2)

    mapN(usesAndImportsVal, declarationsVal) {
      case (usesAndImports, decls) =>
        WeededAst.CompilationUnit(usesAndImports.flatten, decls.flatten, loc)
    }
  }

  /**
    * Is successful if all parts in `names` begin with uppercase letters.
    * Returns a SoftFailure otherwise.
    */
  private def visitModuleName(names: Name.NName): Validation[Unit, IllegalModuleName] = {
    names.idents.foldLeft(Validation.success(()): Validation[Unit, IllegalModuleName]) {
      case (acc, i) => flatMapN(acc) {
        _ =>
          val s = i.name
          val first = s.substring(0, 1)
          if (first.toUpperCase != first)
            Validation.toSoftFailure((), IllegalModuleName(s, i.loc))
          else
            Validation.success(())
      }
    }
  }

  /**
    * Compiles the given parsed declaration `past` to a list of weeded declarations.
    */
  private def visitDecl(decl: ParsedAst.Declaration)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = decl match {
    case ParsedAst.Declaration.Namespace(sp1, names, usesOrImports, decls, sp2) =>
      flatMapN(visitModuleName(names): Validation[Unit, WeederError]) {
        case _ =>
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
      }

    case d: ParsedAst.Declaration.Def => visitTopDef(d)

    case d: ParsedAst.Declaration.Law => visitLaw(d)

    case d: ParsedAst.Declaration.Enum => visitEnum(d)

    case d: ParsedAst.Declaration.RestrictableEnum => visitRestrictableEnum(d)

    case d: ParsedAst.Declaration.TypeAlias => visitTypeAlias(d)

    case d: ParsedAst.Declaration.Class => visitClass(d)

    case d: ParsedAst.Declaration.Instance => visitInstance(d)

    case d: ParsedAst.Declaration.Effect => visitEffect(d)
  }

  /**
    * Performs weeding on the given class declaration `c0`.
    */
  private def visitClass(c0: ParsedAst.Declaration.Class)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Class], WeederError] = c0 match {
    case ParsedAst.Declaration.Class(doc0, ann0, mods0, sp1, ident, tparam0, superClasses0, assocs0, lawsAndSigs, sp2) =>
      val loc = mkSL(sp1, sp2)
      val doc = visitDoc(doc0)
      val laws0 = lawsAndSigs.collect { case law: ParsedAst.Declaration.Law => law }
      val sigs0 = lawsAndSigs.collect { case sig: ParsedAst.Declaration.Sig => sig }

      val annVal = visitAnnotations(ann0)
      val modsVal = visitModifiers(mods0, legalModifiers = Set(Ast.Modifier.Lawful, Ast.Modifier.Public, Ast.Modifier.Sealed))
      val tparam = visitTypeParam(tparam0)
      val superClassesVal = traverse(superClasses0)(visitTypeConstraint)
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, tparam))
      val sigsVal = traverse(sigs0)(visitSig)
      val lawsVal = traverse(laws0)(visitLaw)

      mapN(annVal, modsVal, superClassesVal, assocsVal, sigsVal, lawsVal) {
        case (ann, mods, superClasses, assocs, sigs, laws) =>
          List(WeededAst.Declaration.Class(doc, ann, mods, ident, tparam, superClasses, assocs.flatten, sigs.flatten, laws.flatten, loc))
      }
  }

  /**
    * Performs weeding on the given sig declaration `s0`.
    */
  private def visitSig(s0: ParsedAst.Declaration.Sig)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Sig], WeederError] = s0 match {
    case ParsedAst.Declaration.Sig(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, eff0, tconstrs0, exp0, sp2) =>
      val doc = visitDoc(doc0)

      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val pubVal = requirePublic(mods, ident)
      val identVal = visitIdent(ident)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val tpeVal = visitType(tpe0)
      val effVal = traverseOpt(eff0)(visitType)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      val expVal = traverseOpt(exp0)(visitExp(_, SyntacticEnv.Top))

      mapN(annVal, modVal, pubVal, identVal, tparamsVal, formalsVal, tpeVal, effVal, tconstrsVal, expVal) {
        case (as, mod, _, id, tparams, fparams, tpe, eff, tconstrs, exp) =>
          List(WeededAst.Declaration.Sig(doc, as, mod, id, tparams, fparams, exp, tpe, eff, tconstrs, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given instance declaration `i0`.
    */
  private def visitInstance(i0: ParsedAst.Declaration.Instance)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Instance], WeederError] = i0 match {
    case ParsedAst.Declaration.Instance(doc0, ann0, mods0, sp1, clazz, tpe0, tconstrs0, assocs0, defs0, sp2) =>
      val doc = visitDoc(doc0)

      val annVal = visitAnnotations(ann0)
      val modsVal = visitModifiers(mods0, legalModifiers = Set.empty)
      val tpeVal = visitType(tpe0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      val defsVal = traverse(defs0)(visitInstanceDef)

      flatMapN(annVal, modsVal, tpeVal, defsVal, tconstrsVal) {
        case (ann, mods, tpe, defs, tconstrs) =>
          val assocsVal = traverse(assocs0)(visitAssocTypeDef(_, tpe))
          mapN(assocsVal) {
            case assocs => List(WeededAst.Declaration.Instance(doc, ann, mods, clazz, tpe, tconstrs, assocs.flatten, defs.flatten, mkSL(sp1, sp2)))
          }
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
    case ParsedAst.Declaration.Def(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, eff0, tconstrs0, econstrs0, exp0, sp2) =>
      flix.subtask(ident.name, sample = true)

      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers)
      val pubVal = if (requiresPublic) requirePublic(mods, ident) else Validation.success(()) // conditionally require a public modifier
      val identVal = visitIdent(ident)
      val expVal = visitExp(exp0, SyntacticEnv.Top)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val tpeVal = visitType(tpe0)
      val effVal = traverseOpt(eff0)(visitType)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint)

      mapN(annVal, modVal, pubVal, identVal, tparamsVal, formalsVal, tpeVal, effVal, expVal, tconstrsVal, econstrsVal) {
        case (as, mod, _, id, tparams, fparams, tpe, eff, exp, tconstrs, econstrs) =>
          List(WeededAst.Declaration.Def(doc, as, mod, id, tparams, fparams, exp, tpe, eff, tconstrs, econstrs, mkSL(sp1, sp2)))
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
      val identVal = visitIdent(ident)
      val expVal = visitExp(exp0, SyntacticEnv.Top)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, Presence.Required)
      val tconstrsVal = Validation.traverse(tconstrs0)(visitTypeConstraint)

      mapN(annVal, modVal, identVal, tparamsVal, formalsVal, expVal, tconstrsVal) {
        case (ann, mod, id, tparams, fs, exp, tconstrs) =>
          val eff = None
          val tpe = WeededAst.Type.Ambiguous(Name.mkQName("Bool"), ident.loc)
          val econstrs = Nil // TODO ASSOC-TYPES allow econstrs here
          List(WeededAst.Declaration.Def(doc, ann, mod, id, tparams, fs, exp, tpe, eff, tconstrs, econstrs, mkSL(sp1, sp2)))
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
      val identVal = visitIdent(ident)
      val tparamsVal = requireNoTypeParams(tparams0)
      val opsVal = traverse(ops0)(visitOp)
      mapN(annVal, modVal, identVal, tparamsVal, opsVal) {
        case (ann, mod, id, _, ops) =>
          List(WeededAst.Declaration.Effect(doc, ann, mod, id, ops, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given effect operation.
    */
  private def visitOp(d0: ParsedAst.Declaration.Op)(implicit flix: Flix): Validation[WeededAst.Declaration.Op, WeederError] = d0 match {
    case ParsedAst.Declaration.Op(doc0, ann0, mod0, sp1, ident, tparams0, fparamsOpt0, tpe0, eff0, tconstrs0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val pubVal = requirePublic(mod0, ident)
      val identVal = visitIdent(ident)
      val tparamsVal = requireNoTypeParams(tparams0)
      val fparamsVal = visitFormalParams(fparamsOpt0, Presence.Required)
      val tpeVal = visitType(tpe0)
      val effVal = requireNoEffect(eff0, ident.loc)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint)
      mapN(annVal, modVal, pubVal, identVal, tparamsVal, fparamsVal, tpeVal, effVal, tconstrsVal) {
        case (ann, mod, _, id, _, fparams, tpe, _, tconstrs) =>
          WeededAst.Declaration.Op(doc, ann, mod, id, fparams, tpe, tconstrs, mkSL(sp1, sp2));
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitEnum(d0: ParsedAst.Declaration.Enum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Enum], WeederError] = d0 match {
    case ParsedAst.Declaration.Enum(doc0, ann0, mods, sp1, ident, tparams0, tpe0, derives0, cases0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)

      val casesVal = (tpe0, cases0) match {
        // Case 1: empty enum
        case (None, None) => Validation.success(Map.empty)
        // Case 2: singleton enum
        case (Some(t0), None) =>
          val tVal = visitType(t0)
          mapN(tVal) {
            case t => Map(ident -> WeededAst.Case(ident, t, mkSL(sp1, sp2)))
          }
        // Case 3: multiton enum
        case (None, Some(cs0)) =>
          // Check for [[DuplicateTag]].
          val errors = mutable.ArrayBuffer.empty[DuplicateTag]
          val seen = mutable.Map.empty[Name.Ident, ParsedAst.Case]
          for (caze <- cs0) {
            val enumName = ident.name
            val tagName = caze.ident
            seen.get(tagName) match {
              case None =>
                seen += (tagName -> caze)
              case Some(otherTag) =>
                // NB: We report an error at both source locations.
                val loc1 = otherTag.ident.loc
                val loc2 = mkSL(tagName.sp1, tagName.sp2)
                errors += DuplicateTag(enumName, tagName, loc1, loc2)
                errors += DuplicateTag(enumName, tagName, loc2, loc1)
            }
          }

          // We construct a Map[Name.Ident, ParsedAst.Case]. The map is constructed in reverse order,
          // such that the *first* duplicate tag is retained (if there are duplicate tags).
          val cases = cs0.reverse.map(c => c.ident -> c).toMap

          // We traverse the cases in the map and add the (potential) errors.
          traverseValues(cases)(visitCase).withSoftFailures(errors)

        // Case 4: both singleton and multiton syntax used: Error.
        case (Some(_), Some(_)) =>
          Validation.toHardFailure(IllegalEnum(ident.loc))
      }

      mapN(annVal, modVal, tparamsVal, casesVal) {
        case (ann, mod, tparams, cases) =>
          List(WeededAst.Declaration.Enum(doc, ann, mod, ident, tparams, derives, cases.values.toList, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitRestrictableEnum(d0: ParsedAst.Declaration.RestrictableEnum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.RestrictableEnum], WeederError] = d0 match {
    case ParsedAst.Declaration.RestrictableEnum(doc0, ann0, mods, sp1, ident, index0, tparams0, tpe0, derives0, cases0, sp2) =>
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val index = visitTypeParam(index0)
      val tparamsVal = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)

      val casesVal = (tpe0, cases0) match {
        // Case 1: empty enum
        case (None, None) => Validation.success(Map.empty)
        // Case 2: singleton enum
        case (Some(t0), None) =>
          val tVal = visitType(t0)
          mapN(tVal) {
            case t => Map(ident -> WeededAst.RestrictableCase(ident, t, mkSL(sp1, sp2)))
          }
        // Case 3: multiton enum
        case (None, Some(cs0)) =>
          /*
           * Check for `DuplicateTag`.
           */
          Validation.fold[ParsedAst.RestrictableCase, Map[Name.Ident, WeededAst.RestrictableCase], WeederError](cs0, Map.empty) {
            case (macc, caze: ParsedAst.RestrictableCase) =>
              val tagName = caze.ident
              macc.get(tagName) match {
                case None =>
                  val cazeVal = visitRestrictableCase(caze)
                  mapN(cazeVal) {
                    case caze => (macc + (tagName -> caze))
                  }
                case Some(otherTag) =>
                  val enumName = ident.name
                  val loc1 = otherTag.ident.loc
                  val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                  HardFailure(LazyList(
                    // NB: We report an error at both source locations.
                    DuplicateTag(enumName, tagName, loc1, loc2),
                    DuplicateTag(enumName, tagName, loc2, loc1)
                  ))
              }
          }
        // Case 4: both singleton and multiton syntax used: Error.
        case (Some(_), Some(_)) =>
          Validation.toHardFailure(IllegalEnum(ident.loc))
      }

      mapN(annVal, modVal, tparamsVal, casesVal) {
        case (ann, mod, tparams, cases) =>
          List(WeededAst.Declaration.RestrictableEnum(doc, ann, mod, ident, index, tparams, derives, cases.values.toList, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given enum derivations `derives0`.
    */
  private def visitDerivations(derives0: ParsedAst.Derivations): WeededAst.Derivations = derives0 match {
    case ParsedAst.Derivations(sp1, classes, sp2) =>
      WeededAst.Derivations(classes.toList, mkSL(sp1, sp2))
  }

  /**
    * Performs weeding on the given enum case `c0`.
    */
  private def visitCase(c0: ParsedAst.Case)(implicit flix: Flix): Validation[WeededAst.Case, WeederError] = c0 match {
    case ParsedAst.Case(sp1, ident, tpe0, sp2) =>
      val tpeVal = traverseOpt(tpe0)(visitType)
      mapN(tpeVal) {
        case tpeOpt =>
          val tpe = tpeOpt.getOrElse(WeededAst.Type.Unit(ident.loc))
          WeededAst.Case(ident, tpe, mkSL(sp1, sp2))
      }
  }

  /**
    * Performs weeding on the given enum case `c0`.
    */
  private def visitRestrictableCase(c0: ParsedAst.RestrictableCase)(implicit flix: Flix): Validation[WeededAst.RestrictableCase, WeederError] = c0 match {
    case ParsedAst.RestrictableCase(sp1, ident, tpe0, sp2) =>
      val tpeVal = traverseOpt(tpe0)(visitType)
      mapN(tpeVal) {
        case tpeOpt =>
          val tpe = tpeOpt.getOrElse(WeededAst.Type.Unit(ident.loc))
          WeededAst.RestrictableCase(ident, tpe, mkSL(sp1, sp2))
      }
  }

  /**
    * Performs weeding on the given type alias declaration `d0`.
    */
  private def visitTypeAlias(d0: ParsedAst.Declaration.TypeAlias)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = d0 match {
    case ParsedAst.Declaration.TypeAlias(doc0, mod0, sp1, ident, tparams0, tpe0, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)
      val tpeVal = visitType(tpe0)

      mapN(modVal, tparamsVal, tpeVal) {
        case (mod, tparams, tpe) =>
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given associated type signature `d0`.
    */
  private def visitAssocTypeSig(d0: ParsedAst.Declaration.AssocTypeSig, clazzTparam: WeededAst.TypeParam): Validation[List[WeededAst.Declaration.AssocTypeSig], WeederError] = d0 match {
    case ParsedAst.Declaration.AssocTypeSig(doc0, mod0, sp1, ident, tparams0, kind0, sp2) =>

      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)
      val kind = kind0 match {
        case Some(k) => visitKind(k)
        case None => WeededAst.Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)
      }
      val loc = mkSL(sp1, sp2)

      flatMapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val tparamVal = tparams match {
            // Case 1: Elided. Use the class tparam.
            case WeededAst.TypeParams.Elided => Validation.success(clazzTparam)

            // Case 2: Singleton parameter.
            case WeededAst.TypeParams.Kinded(hd :: Nil) => Validation.success(hd)
            case WeededAst.TypeParams.Unkinded(hd :: Nil) => Validation.success(hd)

            // Case 3: Multiple params.
            // We recover by (arbitrarily) using the first parameter. The Parser guarantees that ts cannot be empty.
            case WeededAst.TypeParams.Kinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
            case WeededAst.TypeParams.Unkinded(ts) => Validation.toSoftFailure(ts.head, NonUnaryAssocType(ts.length, ident.loc))
          }
          mapN(tparamVal) {
            case tparam => List(WeededAst.Declaration.AssocTypeSig(doc, mod, ident, tparam, kind, loc))
          }
      }
  }

  /**
    * Performs weeding on the given associated type definition `d0`.
    */
  private def visitAssocTypeDef(d0: ParsedAst.Declaration.AssocTypeDef, instTpe: WeededAst.Type): Validation[List[WeededAst.Declaration.AssocTypeDef], WeederError] = d0 match {
    case ParsedAst.Declaration.AssocTypeDef(doc0, mod0, sp1, ident, args0, tpe0, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val argVal = args0.map(_.toList) match {
        // Case 1: Elided type. Use the type from the instance
        case None => Validation.success(instTpe)
        // Case 2: One argument. Visit it.
        case Some(hd :: Nil) => visitType(hd)
        // Case 3: Multiple arguments.
        // We recover by (arbitrarily) using the first parameter. The Parser guarantees that ts cannot be empty.
        case Some(ts) =>
          visitType(ts.head).withSoftFailure(NonUnaryAssocType(ts.length, ident.loc))
      }
      val tpeVal = visitType(tpe0)
      val loc = mkSL(sp1, sp2)

      mapN(modVal, argVal, tpeVal) {
        case (mod, arg, tpe) =>
          List(WeededAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc))
      }
  }

  /**
    * Performs weeding on the given use or import `u0`.
    */
  private def visitUseOrImport(u0: ParsedAst.UseOrImport): Validation[List[WeededAst.UseOrImport], WeederError] = u0 match {
    case ParsedAst.Use.UseOne(sp1, qname, sp2) =>
      if (qname.isQualified) {
        Validation.success(List(WeededAst.UseOrImport.Use(qname, qname.ident, mkSL(sp1, sp2))))
      } else {
        // Recover by ignoring the broken use.
        Validation.toSoftFailure(Nil, WeederError.UnqualifiedUse(qname.loc))
      }

    case ParsedAst.Use.UseMany(_, nname, names, _) =>
      // Check for [[IllegalUseAlias]].
      val errors = mutable.ArrayBuffer.empty[IllegalUse]
      for (ParsedAst.Use.NameAndAlias(_, ident, aliasOpt, _) <- names) {
        if (!isValidAlias(ident, aliasOpt)) {
          // The use of aliasOpt.get is safe because an alias can only be invalid if it exists.
          errors += IllegalUse(ident.name, aliasOpt.get.name, aliasOpt.get.loc)
        }
      }

      // Collect non-erroneous uses.
      val ns = names.collect {
        case ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2) if isValidAlias(ident, aliasOpt) =>
          val alias = aliasOpt.getOrElse(ident)
          WeededAst.UseOrImport.Use(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)): WeededAst.UseOrImport
      }.toList
      Validation.success(ns).withSoftFailures(errors)

    case ParsedAst.Imports.ImportOne(sp1, name, sp2) =>
      val loc = mkSL(sp1, sp2)
      val alias = name.fqn.last
      if (raw"[A-Z][A-Za-z0-9_!]*".r matches alias) {
        Validation.success(List(WeededAst.UseOrImport.Import(name, Name.Ident(sp1, alias, sp2), loc)))
      } else {
        // We recover by simply ignoring the broken import.
        Validation.toSoftFailure(Nil, MalformedIdentifier(alias, loc))
      }

    case ParsedAst.Imports.ImportMany(sp1, pkg, ids, sp2) =>
      val loc = mkSL(sp1, sp2)
      val is = ids.map {
        case ParsedAst.Imports.NameAndAlias(_, name, alias, _) =>
          val fqn = Name.JavaName(pkg.sp1, pkg.fqn :+ name, pkg.sp2)
          val ident = alias.getOrElse(Name.Ident(sp1, name, sp2))
          WeededAst.UseOrImport.Import(fqn, ident, loc)
      }
      Validation.success(is.toList)
  }

  /**
    * Returns `true` if `ident` and `alias` share the same case (i.e. both are upper- or lowercase).
    */
  private def isValidAlias(ident: Name.Ident, aliasOpt: Option[Name.Ident]): Boolean = aliasOpt match {
    case None => true
    case Some(alias) => ident.name(0).isUpper == alias.name.charAt(0).isUpper
  }

  /**
    * Weeds the given expression.
    */
  private def visitExp(exp0: ParsedAst.Expression, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Expr, WeederError] = exp0 match {
    case ParsedAst.Expression.QName(sp1, qname, sp2) =>
      val parts = qname.namespace.idents :+ qname.ident
      val prefix = parts.takeWhile(_.isUpper)
      val suffix = parts.dropWhile(_.isUpper)
      suffix match {
        // Case 1: upper qualified name
        case Nil =>
          // NB: We only use the source location of the identifier itself.
          Validation.success(WeededAst.Expr.Ambiguous(qname, qname.ident.loc))

        // Case 1: basic qualified name
        case ident :: Nil =>
          // NB: We only use the source location of the identifier itself.
          Validation.success(WeededAst.Expr.Ambiguous(qname, ident.loc))

        // Case 2: actually a record access
        case ident :: labels =>
          // NB: We only use the source location of the identifier itself.
          val base = WeededAst.Expr.Ambiguous(Name.mkQName(prefix.map(_.toString), ident.name, ident.sp1, ident.sp2), ident.loc)
          val lbls = labels.foldLeft(base: WeededAst.Expr) {
            case (acc, label) => WeededAst.Expr.RecordSelect(acc, Name.mkLabel(label), label.loc) // TODO NS-REFACTOR should use better location
          }
          Validation.success(lbls)
      }

    case ParsedAst.Expression.Open(sp1, qname, sp2) =>
      // TODO RESTR-VARS make sure it's capital
      Validation.success(WeededAst.Expr.Open(qname, mkSL(sp1, sp2)))

    case ParsedAst.Expression.OpenAs(sp1, qname, exp, sp2) =>
      // TODO RESTR-VARS make sure it's capital
      val eVal = visitExp(exp, senv)
      mapN(eVal) {
        case e => WeededAst.Expr.OpenAs(qname, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Hole(sp1, name, sp2) =>
      val loc = mkSL(sp1, sp2)
      Validation.success(WeededAst.Expr.Hole(name, loc))

    case ParsedAst.Expression.HolyName(ident, sp2) =>
      val loc = mkSL(ident.sp1, sp2)
      val exp = WeededAst.Expr.Ambiguous(Name.mkQName(ident), ident.loc)
      Validation.success(WeededAst.Expr.HoleWithExp(exp, loc))

    case ParsedAst.Expression.Use(sp1, use, exp, sp2) =>
      mapN(visitUseOrImport(use), visitExp(exp, senv)) {
        case (us, e) => WeededAst.Expr.Use(us, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Lit(sp1, lit, sp2) =>
      visitLiteral(lit) match {
        case Result.Ok(c) => Validation.success(WeededAst.Expr.Cst(c, mkSL(sp1, sp2)))
        case Result.Err(e) => Validation.toSoftFailure(WeededAst.Expr.Error(e), e)
      }

    case ParsedAst.Expression.Intrinsic(sp1, op, exps, sp2) =>
      val loc = mkSL(sp1, sp2)
      flatMapN(traverse(exps)(visitArgument(_, senv))) {
        case es => (op.name, es) match {
          case ("BOOL_NOT", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.BoolOp.Not, e1, loc))
          case ("BOOL_AND", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.BoolOp.And, e1, e2, loc))
          case ("BOOL_OR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, loc))
          case ("BOOL_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.BoolOp.Eq, e1, e2, loc))
          case ("BOOL_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.BoolOp.Neq, e1, e2, loc))

          case ("CHAR_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Eq, e1, e2, loc))
          case ("CHAR_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Neq, e1, e2, loc))
          case ("CHAR_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Lt, e1, e2, loc))
          case ("CHAR_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Le, e1, e2, loc))
          case ("CHAR_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Gt, e1, e2, loc))
          case ("CHAR_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.CharOp.Ge, e1, e2, loc))

          case ("FLOAT32_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Float32Op.Neg, e1, loc))
          case ("FLOAT32_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Add, e1, e2, loc))
          case ("FLOAT32_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Sub, e1, e2, loc))
          case ("FLOAT32_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Mul, e1, e2, loc))
          case ("FLOAT32_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Div, e1, e2, loc))
          case ("FLOAT32_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Exp, e1, e2, loc))
          case ("FLOAT32_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Eq, e1, e2, loc))
          case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Neq, e1, e2, loc))
          case ("FLOAT32_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Lt, e1, e2, loc))
          case ("FLOAT32_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Le, e1, e2, loc))
          case ("FLOAT32_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Gt, e1, e2, loc))
          case ("FLOAT32_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float32Op.Ge, e1, e2, loc))

          case ("FLOAT64_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Float64Op.Neg, e1, loc))
          case ("FLOAT64_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Add, e1, e2, loc))
          case ("FLOAT64_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Sub, e1, e2, loc))
          case ("FLOAT64_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Mul, e1, e2, loc))
          case ("FLOAT64_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Div, e1, e2, loc))
          case ("FLOAT64_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Exp, e1, e2, loc))
          case ("FLOAT64_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Eq, e1, e2, loc))
          case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Neq, e1, e2, loc))
          case ("FLOAT64_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Lt, e1, e2, loc))
          case ("FLOAT64_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Le, e1, e2, loc))
          case ("FLOAT64_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Gt, e1, e2, loc))
          case ("FLOAT64_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Float64Op.Ge, e1, e2, loc))

          case ("INT8_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int8Op.Neg, e1, loc))
          case ("INT8_NOT", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int8Op.Not, e1, loc))
          case ("INT8_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Add, e1, e2, loc))
          case ("INT8_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Sub, e1, e2, loc))
          case ("INT8_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Mul, e1, e2, loc))
          case ("INT8_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Div, e1, e2, loc))
          case ("INT8_REM", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Rem, e1, e2, loc))
          case ("INT8_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Exp, e1, e2, loc))
          case ("INT8_AND", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.And, e1, e2, loc))
          case ("INT8_OR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Or, e1, e2, loc))
          case ("INT8_XOR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Xor, e1, e2, loc))
          case ("INT8_SHL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Shl, e1, e2, loc))
          case ("INT8_SHR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Shr, e1, e2, loc))
          case ("INT8_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Eq, e1, e2, loc))
          case ("INT8_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Neq, e1, e2, loc))
          case ("INT8_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Lt, e1, e2, loc))
          case ("INT8_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Le, e1, e2, loc))
          case ("INT8_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Gt, e1, e2, loc))
          case ("INT8_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int8Op.Ge, e1, e2, loc))

          case ("INT16_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int16Op.Neg, e1, loc))
          case ("INT16_NOT", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int16Op.Not, e1, loc))
          case ("INT16_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Add, e1, e2, loc))
          case ("INT16_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Sub, e1, e2, loc))
          case ("INT16_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Mul, e1, e2, loc))
          case ("INT16_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Div, e1, e2, loc))
          case ("INT16_REM", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Rem, e1, e2, loc))
          case ("INT16_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Exp, e1, e2, loc))
          case ("INT16_AND", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.And, e1, e2, loc))
          case ("INT16_OR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Or, e1, e2, loc))
          case ("INT16_XOR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Xor, e1, e2, loc))
          case ("INT16_SHL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Shl, e1, e2, loc))
          case ("INT16_SHR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Shr, e1, e2, loc))
          case ("INT16_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Eq, e1, e2, loc))
          case ("INT16_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Neq, e1, e2, loc))
          case ("INT16_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Lt, e1, e2, loc))
          case ("INT16_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Le, e1, e2, loc))
          case ("INT16_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Gt, e1, e2, loc))
          case ("INT16_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int16Op.Ge, e1, e2, loc))

          case ("INT32_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int32Op.Neg, e1, loc))
          case ("INT32_NOT", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int32Op.Not, e1, loc))
          case ("INT32_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Add, e1, e2, loc))
          case ("INT32_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Sub, e1, e2, loc))
          case ("INT32_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Mul, e1, e2, loc))
          case ("INT32_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Div, e1, e2, loc))
          case ("INT32_REM", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Rem, e1, e2, loc))
          case ("INT32_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Exp, e1, e2, loc))
          case ("INT32_AND", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.And, e1, e2, loc))
          case ("INT32_OR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Or, e1, e2, loc))
          case ("INT32_XOR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Xor, e1, e2, loc))
          case ("INT32_SHL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Shl, e1, e2, loc))
          case ("INT32_SHR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Shr, e1, e2, loc))
          case ("INT32_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Eq, e1, e2, loc))
          case ("INT32_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Neq, e1, e2, loc))
          case ("INT32_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Lt, e1, e2, loc))
          case ("INT32_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Le, e1, e2, loc))
          case ("INT32_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Gt, e1, e2, loc))
          case ("INT32_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int32Op.Ge, e1, e2, loc))

          case ("INT64_NEG", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int64Op.Neg, e1, loc))
          case ("INT64_NOT", e1 :: Nil) => Validation.success(WeededAst.Expr.Unary(SemanticOp.Int64Op.Not, e1, loc))
          case ("INT64_ADD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Add, e1, e2, loc))
          case ("INT64_SUB", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Sub, e1, e2, loc))
          case ("INT64_MUL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Mul, e1, e2, loc))
          case ("INT64_DIV", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Div, e1, e2, loc))
          case ("INT64_REM", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Rem, e1, e2, loc))
          case ("INT64_EXP", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Exp, e1, e2, loc))
          case ("INT64_AND", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.And, e1, e2, loc))
          case ("INT64_OR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Or, e1, e2, loc))
          case ("INT64_XOR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Xor, e1, e2, loc))
          case ("INT64_SHL", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Shl, e1, e2, loc))
          case ("INT64_SHR", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Shr, e1, e2, loc))
          case ("INT64_EQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Eq, e1, e2, loc))
          case ("INT64_NEQ", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Neq, e1, e2, loc))
          case ("INT64_LT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Lt, e1, e2, loc))
          case ("INT64_LE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Le, e1, e2, loc))
          case ("INT64_GT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Gt, e1, e2, loc))
          case ("INT64_GE", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.Binary(SemanticOp.Int64Op.Ge, e1, e2, loc))

          case ("CHANNEL_GET", e1 :: Nil) => Validation.success(WeededAst.Expr.GetChannel(e1, loc))
          case ("CHANNEL_PUT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.PutChannel(e1, e2, loc))
          case ("CHANNEL_NEW", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.NewChannel(e1, e2, loc))

          case ("ARRAY_NEW", e1 :: e2 :: e3 :: Nil) => Validation.success(WeededAst.Expr.ArrayNew(e1, e2, e3, loc))
          case ("ARRAY_LENGTH", e1 :: Nil) => Validation.success(WeededAst.Expr.ArrayLength(e1, loc))
          case ("ARRAY_LOAD", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.ArrayLoad(e1, e2, loc))
          case ("ARRAY_STORE", e1 :: e2 :: e3 :: Nil) => Validation.success(WeededAst.Expr.ArrayStore(e1, e2, e3, loc))

          case ("VECTOR_GET", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.VectorLoad(e1, e2, loc))
          case ("VECTOR_LENGTH", e1 :: Nil) => Validation.success(WeededAst.Expr.VectorLength(e1, loc))

          case ("SCOPE_EXIT", e1 :: e2 :: Nil) => Validation.success(WeededAst.Expr.ScopeExit(e1, e2, loc))

          case _ =>
            val err = UndefinedIntrinsic(loc)
            Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
        }
      }

    case ParsedAst.Expression.Apply(lambda, args, sp2) =>
      val sp1 = leftMostSourcePosition(lambda)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(lambda, senv), traverse(args)(e => visitArgument(e, senv))) {
        case (e, as) =>
          val es = getArguments(as, loc)
          WeededAst.Expr.Apply(e, es, loc)
      }

    case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
      val loc = mkSL(leftMostSourcePosition(exp1), sp2)
      val e1 = visitExp(exp1, senv)
      val e2 = visitExp(name, senv)
      val e3 = visitExp(exp2, senv)
      mapN(e1, e2, e3) {
        case (e11, e22, e33) => WeededAst.Expr.Infix(e11, e22, e33, loc)
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
      }

    case ParsedAst.Expression.LambdaMatch(sp1, pat, exp, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val p = visitPattern(pat)
      val e = visitExp(exp, senv)
      mapN(p, e) {
        case (p1, e1) => WeededAst.Expr.LambdaMatch(p1, e1, loc)
      }

    case ParsedAst.Expression.Unary(sp1, op, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      visitExp(exp, senv).map {
        case e => visitUnaryOperator(op) match {
          case OperatorResult.BuiltIn(name) => WeededAst.Expr.Apply(WeededAst.Expr.Ambiguous(name, name.loc), List(e), loc)
          case OperatorResult.Operator(o) => WeededAst.Expr.Unary(o, e, loc)
          case OperatorResult.Unrecognized(ident) => WeededAst.Expr.Apply(WeededAst.Expr.Ambiguous(Name.mkQName(ident), ident.loc), List(e), loc)
        }
      }

    case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) => visitBinaryOperator(op) match {
          case OperatorResult.BuiltIn(name) => WeededAst.Expr.Apply(WeededAst.Expr.Ambiguous(name, name.loc), List(e1, e2), loc)
          case OperatorResult.Operator(o) => WeededAst.Expr.Binary(o, e1, e2, loc)
          case OperatorResult.Unrecognized(ident) => WeededAst.Expr.Apply(WeededAst.Expr.Ambiguous(Name.mkQName(ident), ident.loc), List(e1, e2), loc)
        }
      }

    case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
      mapN(visitExp(exp1, senv), visitExp(exp2, senv), visitExp(exp3, senv)) {
        case (e1, e2, e3) => WeededAst.Expr.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Stm(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) => WeededAst.Expr.Stm(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Discard(sp1, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      visitExp(exp, senv).map {
        case e => WeededAst.Expr.Discard(e, loc)
      }

    case ParsedAst.Expression.ApplicativeFor(sp1, frags, exp, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val (gens, bad) = frags.partition {
        case _: ParsedAst.ForFragment.Generator => true
        case _ => false
      }
      val gs = traverse(gens)(g => visitForFragmentGenerator(g.asInstanceOf[ParsedAst.ForFragment.Generator], senv))
      val errs = traverse(bad)(visitForFragment(_, senv))
      val e = visitExp(exp, senv)
      flatMapN(errs, gs, e) {
        case _ if frags.isEmpty =>
          val err = EmptyForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (es, _, _) if es.nonEmpty =>
          val err = WeederError.IllegalForAFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (_, fs, e1) => Validation.success(WeededAst.Expr.ApplicativeFor(fs, e1, loc))
      }

    case ParsedAst.Expression.ForEach(sp1, frags, exp, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val fs = traverse(frags)(visitForFragment(_, senv))
      val e = visitExp(exp, senv)
      flatMapN(fs, e) {
        case _ if frags.isEmpty =>
          val err = EmptyForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
        case (WeededAst.ForFragment.Guard(_, loc1) :: _, _) =>
          val err = IllegalForFragment(loc1)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
        case (fs1, e1) => Validation.success(WeededAst.Expr.ForEach(fs1, e1, loc))
      }

    case ParsedAst.Expression.MonadicFor(sp1, frags, exp, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val fs = traverse(frags)(visitForFragment(_, senv))
      val e = visitExp(exp, senv)
      flatMapN(fs, e) {
        case _ if frags.isEmpty =>
          val err = EmptyForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (WeededAst.ForFragment.Guard(_, _) :: _, _) =>
          val err = IllegalForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (fs1, e1) => Validation.success(WeededAst.Expr.MonadicFor(fs1, e1, loc))
      }

    case ParsedAst.Expression.ForEachYield(sp1, frags, exp, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val fs = traverse(frags)(visitForFragment(_, senv))
      val e = visitExp(exp, senv)
      flatMapN(fs, e) {
        case _ if frags.isEmpty =>
          val err = EmptyForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (WeededAst.ForFragment.Guard(_, _) :: _, _) =>
          val err = IllegalForFragment(loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)

        case (fs1, e1) => Validation.success(WeededAst.Expr.ForEachYield(fs1, e1, loc))
      }

    case ParsedAst.Expression.LetMatch(sp1, mod0, pat, tpe0, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val patVal = visitPattern(pat)
      val modVal = visitModifiers(mod0, legalModifiers = Set.empty)
      val tpeVal = traverseOpt(tpe0)(visitType)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(patVal, modVal, tpeVal, exp1Val, exp2Val) {
        case (p, m, t, e1, e2) => WeededAst.Expr.LetMatch(p, m, t, e1, e2, loc)
      }

    case ParsedAst.Expression.LetRecDef(sp1, ann, ident, fparams, tpeAndEff, exp1, exp2, sp2) =>
      val mod = Ast.Modifiers.Empty
      val loc = mkSL(sp1, sp2)
      val annVal = flatMapN(visitAnnotations(ann)) {
        case Ast.Annotations(as) =>
          // Check for [[IllegalAnnotation]]
          val errors = mutable.ArrayBuffer.empty[IllegalAnnotation]
          for (a <- as) {
            a match {
              case Ast.Annotation.TailRecursive(_) => // OK
              case otherAnn => errors += IllegalAnnotation(otherAnn.loc)
            }
          }
          Validation.toSuccessOrSoftFailure(Ast.Annotations(as), errors)
      }

      val tpeOpt = tpeAndEff.map(_._1)
      val effOpt = tpeAndEff.flatMap(_._2)

      val fpVal = visitFormalParams(fparams, Presence.Optional)
      val e1Val = visitExp(exp1, senv)
      val e2Val = visitExp(exp2, senv)
      val tpeVal = traverseOpt(tpeOpt)(visitType)
      val effVal = traverseOpt(effOpt)(visitType)

      mapN(fpVal, annVal, e1Val, e2Val, tpeVal, effVal) {
        case (fp, a1, e1, e2, tpe, eff) =>
          // skip ascription if it's empty
          val ascription = if (tpe.isDefined || eff.isDefined) {
            WeededAst.Expr.Ascribe(e1, tpe, eff, e1.loc)
          } else {
            e1
          }
          val lambda = mkCurried(fp, ascription, e1.loc)
          WeededAst.Expr.LetRec(ident, a1, mod, lambda, e2, loc)
      }

    case ParsedAst.Expression.LetImport(sp1, impl, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val implVal = visitJvmOp(impl)
      val e2Val = visitExp(exp2, senv)
      mapN(implVal, e2Val) {
        case (i, e) => WeededAst.Expr.LetImport(i, e, loc)
      }

    case ParsedAst.Expression.NewObject(sp1, tpe, methods, sp2) =>
      val tVal = visitType(tpe)
      val msVal = traverse(methods)(visitJvmMethod(_, senv))
      mapN(tVal, msVal) {
        case (t, ms) =>
          WeededAst.Expr.NewObject(t, ms, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Static(sp1, sp2) =>
      val loc = mkSL(sp1, sp2)
      Validation.success(WeededAst.Expr.Static(loc))

    case ParsedAst.Expression.Scope(sp1, ident, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expr.Scope(ident, e, mkSL(sp1, sp2))
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
        case (e, rs) => WeededAst.Expr.Match(e, rs, loc)
      }

    case ParsedAst.Expression.TypeMatch(sp1, exp, rules, sp2) =>
      val loc = mkSL(sp1, sp2)
      val rulesVal = traverse(rules) {
        case ParsedAst.TypeMatchRule(ident, tpe0, body0) =>
          val tpeVal = visitType(tpe0)
          val bodyVal = visitExp(body0, senv)
          mapN(tpeVal, bodyVal) {
            case (tpe, body) => WeededAst.TypeMatchRule(ident, tpe, body)
          }
      }
      mapN(visitExp(exp, senv), rulesVal) {
        case (e, rs) => WeededAst.Expr.TypeMatch(e, rs, loc)
      }

    case ParsedAst.Expression.RestrictableChoose(sp1, star, exp, rules, sp2) =>
      val expVal = visitExp(exp, senv)
      val rulesVal = traverse(rules) {
        case ParsedAst.MatchRule(pat, guard, body) =>
          flatMapN(visitPattern(pat), traverseOpt(guard)(visitExp(_, senv)), visitExp(body, senv)) {
            case (p, g, b) => createRestrictableChooseRule(star, p, g, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => WeededAst.Expr.RestrictableChoose(star, e, rs, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
      traverse(elms)(visitArgument(_, senv)).map {
        case args => WeededAst.Expr.Tuple(args, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordLit(sp1, labels, sp2) =>
      val labelsVal = traverse(labels) {
        case ParsedAst.RecordLabel(_, ident, exp, _) =>
          val expVal = visitExp(exp, senv)

          mapN(expVal, visitIdent(ident)) {
            case (e, id) => id -> e
          }
      }

      mapN(labelsVal) {
        case ls =>
          // Rewrite into a sequence of nested record extensions.
          val zero = WeededAst.Expr.RecordEmpty(mkSL(sp1, sp2))
          ls.foldRight(zero: WeededAst.Expr) {
            case ((ident, e), acc) => WeededAst.Expr.RecordExtend(Name.mkLabel(ident), e, acc, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.RecordSelect(exp, ident, sp2) =>
      val sp1 = leftMostSourcePosition(exp)
      mapN(visitExp(exp, senv), visitIdent(ident)) {
        case (e, id) => WeededAst.Expr.RecordSelect(e, Name.mkLabel(id), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordOperation(_, ops, rest, _) =>
      // We translate the sequence of record operations into a nested tree using a fold right.
      foldRight(ops)(visitExp(rest, senv)) {
        case (ParsedAst.RecordOp.Extend(sp1, ident, exp, sp2), acc) =>
          mapN(visitExp(exp, senv), visitIdent(ident)) {
            case (e, id) =>
              WeededAst.Expr.RecordExtend(Name.mkLabel(id), e, acc, mkSL(sp1, sp2))
          }

        case (ParsedAst.RecordOp.Restrict(sp1, ident, sp2), acc) =>
          mapN(visitIdent(ident)) {
            case id => WeededAst.Expr.RecordRestrict(Name.mkLabel(id), acc, mkSL(sp1, sp2))
          }

        case (ParsedAst.RecordOp.Update(sp1, ident, exp, sp2), acc) =>
          mapN(visitExp(exp, senv), visitIdent(ident)) {
            case (e, id) =>
              // An update is a restrict followed by an extension.
              val inner = WeededAst.Expr.RecordRestrict(Name.mkLabel(id), acc, mkSL(sp1, sp2))
              WeededAst.Expr.RecordExtend(Name.mkLabel(id), e, inner, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.ArrayLit(sp1, exps, exp, sp2) =>
      mapN(traverse(exps)(visitExp(_, senv)), visitExp(exp, senv)) {
        case (es, e) => WeededAst.Expr.ArrayLit(es, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.VectorLit(sp1, exps, sp2) =>
      mapN(traverse(exps)(visitExp(_, senv))) {
        case es => WeededAst.Expr.VectorLit(es, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FCons(exp1, sp1, sp2, exp2) =>
      val loc = mkSL(sp1, sp2)
      /*
       * Rewrites a `FCons` expression into a tag expression.
       */
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) =>
          WeededAst.Expr.FCons(e1, e2, loc)
      }

    case ParsedAst.Expression.FAppend(exp1, sp1, sp2, exp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val e1Val = visitExp(exp1, senv)
      val e2Val = visitExp(exp2, senv)
      mapN(e1Val, e2Val) {
        case (e1, e2) => WeededAst.Expr.FAppend(e1, e2, loc)
      }

    case ParsedAst.Expression.ListLit(sp1, sp2, exps) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val esVal = traverse(exps)(visitExp(_, senv))
      mapN(esVal) {
        case es => WeededAst.Expr.ListLit(es, loc)
      }

    case ParsedAst.Expression.SetLit(sp1, sp2, exps) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val esVal = traverse(exps)(visitExp(_, senv))
      mapN(esVal) {
        case es => WeededAst.Expr.SetLit(es, loc)
      }

    case ParsedAst.Expression.MapLit(sp1, sp2, exps) =>
      val loc = mkSL(sp1, sp2)
      val esVal = traverse(exps) {
        case (k, v) => mapN(visitExp(k, senv), visitExp(v, senv))(_ -> _)
      }
      mapN(esVal) {
        case es => WeededAst.Expr.MapLit(es, loc)
      }

    case ParsedAst.Expression.Interpolation(sp1, parts, sp2) =>

      /**
        * Returns an expression that concatenates the result of the expression `e1` with the expression `e2`.
        */
      def mkConcat(e1: WeededAst.Expr, e2: WeededAst.Expr, loc: SourceLocation): WeededAst.Expr = {
        val sop = SemanticOp.StringOp.Concat
        val l = loc.asSynthetic
        WeededAst.Expr.Binary(sop, e1, e2, l)
      }

      /**
        * Returns an expression that applies `toString` to the result of the given expression `e`.
        */
      def mkApplyToString(e: WeededAst.Expr, sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expr = {
        val fqn = "ToString.toString"
        val loc = mkSL(sp1, sp2).asSynthetic
        mkApplyFqn(fqn, List(e), loc)
      }

      /**
        * Returns an expression that applies `debugString` to the result of the given expression `e`.
        */
      def mkApplyDebugString(e: WeededAst.Expr, sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expr = {
        val fqn = "Debug.stringify"
        val loc = mkSL(sp1, sp2).asSynthetic
        mkApplyFqn(fqn, List(e), loc)
      }

      val loc = mkSL(sp1, sp2)

      parts match {
        case Seq(ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
          // Special case: We have a constant string. Check the contents and return it.
          visitCharSeq(chars) match {
            case Result.Ok(s) => Validation.success(WeededAst.Expr.Cst(Ast.Constant.Str(s), mkSL(innerSp1, innerSp2)))
            case Result.Err(e) => Validation.toSoftFailure(WeededAst.Expr.Error(e), e)
          }

        case _ =>
          // General Case: Fold the interpolator parts together.
          val init = WeededAst.Expr.Cst(Ast.Constant.Str(""), loc)
          Validation.fold(parts, init: WeededAst.Expr) {
            // Case 1: string part
            case (acc, ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
              visitCharSeq(chars) match {
                case Result.Ok(s) =>
                  val e2 = WeededAst.Expr.Cst(Ast.Constant.Str(s), mkSL(innerSp1, innerSp2))
                  Validation.success(mkConcat(acc, e2, loc))
                case Result.Err(e) =>
                  Validation.toSoftFailure(WeededAst.Expr.Error(e), e)
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
              val err = EmptyInterpolatedExpression(mkSL(innerSp1, innerSp2))
              Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
            // Case 5: empty interpolated debug
            case (_, ParsedAst.InterpolationPart.DebugPart(innerSp1, None, innerSp2)) =>
              val err = EmptyInterpolatedExpression(mkSL(innerSp1, innerSp2))
              Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
          }
      }

    case ParsedAst.Expression.Ref(sp1, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expr.Ref(e1, e2, loc)
      }

    case ParsedAst.Expression.Deref(sp1, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      val e = visitExp(exp, senv)
      mapN(e) {
        case e1 => WeededAst.Expr.Deref(e1, loc)
      }

    case ParsedAst.Expression.Assign(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expr.Assign(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Ascribe(exp, expectedType, expectedEff, sp2) =>
      val eVal = visitExp(exp, senv)
      val tVal = visitTypeNoWild(expectedType)
      val fVal = traverseOpt(expectedEff)(visitType)
      mapN(eVal, tVal, fVal) {
        case (e, t, f) => WeededAst.Expr.Ascribe(e, t, f, mkSL(leftMostSourcePosition(exp), sp2))
      }

    case ParsedAst.Expression.InstanceOf(exp, className, sp2) =>
      val sp1 = leftMostSourcePosition(exp)
      val loc = mkSL(sp1, sp2)
      visitExp(exp, senv).map {
        case e => WeededAst.Expr.InstanceOf(e, className.toString, loc)
      }

    case ParsedAst.Expression.CheckedTypeCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expr.CheckedCast(Ast.CheckedCastType.TypeCast, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.CheckedEffectCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expr.CheckedCast(Ast.CheckedCastType.EffectCast, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.UncheckedCast(sp1, exp, declaredType, declaredEff, sp2) =>
      val eVal = visitExp(exp, senv)
      val tVal = visitTypeNoWild(declaredType)
      val fVal = traverseOpt(declaredEff)(visitType)
      mapN(eVal, tVal, fVal) {
        case (e, t, f) =>
          WeededAst.Expr.UncheckedCast(e, t, f, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.UncheckedMaskingCast(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Expr.UncheckedMaskingCast(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Without(exp, effs, sp2) =>
      val loc = mkSL(leftMostSourcePosition(exp), sp2)
      // NB: We only give the innermost expression a real location
      mapN(visitExp(exp, senv)) {
        e =>
          val base = WeededAst.Expr.Without(e, effs.head, loc)
          effs.tail.foldLeft(base) {
            case (acc, eff) => WeededAst.Expr.Without(acc, eff, loc.asSynthetic)
          }
      }

    case ParsedAst.Expression.Do(sp1, op, args0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val argsVal = traverse(args0)(visitArgument(_, senv)).map(getArguments(_, loc))
      mapN(argsVal) {
        args => WeededAst.Expr.Do(op, args, loc)
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
              Validation.success(WeededAst.Expr.Resume(arg, loc))
            // Case 2: Not in a handler. Error.
            case SyntacticEnv.Top =>
              val err = IllegalResume(loc)
              Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
          }
      }

    case ParsedAst.Expression.Try(sp1, exp, ParsedAst.CatchOrHandler.Catch(rules), sp2) =>
      val expVal = visitExp(exp, senv)
      val rulesVal = traverse(rules) {
        case ParsedAst.CatchRule(ident, fqn, body) =>
          visitExp(body, senv).map {
            case b => WeededAst.CatchRule(ident, fqn.toString, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => WeededAst.Expr.TryCatch(e, rs, mkSL(sp1, sp2))
      }

    // not handling these rules yet
    case ParsedAst.Expression.Try(sp1, exp0, ParsedAst.CatchOrHandler.Handler(eff, rules0), sp2) =>
      val expVal = visitExp(exp0, senv)
      val rulesVal = traverse(rules0.getOrElse(Seq.empty)) {
        case ParsedAst.HandlerRule(op, fparams0, body0) =>
          // In this case, we want an extra resumption argument
          // so both an empty list and a singleton list should be padded with unit
          // [] --> [_unit]
          // [x] --> [_unit, x]
          // [x, ...] --> [x, ...]
          val fparamsValPrefix = if (fparams0.sizeIs == 1) visitFormalParams(Seq.empty, Presence.Forbidden) else Validation.success(Nil)
          val fparamsValSuffix = visitFormalParams(fparams0, Presence.Forbidden)
          val bodyVal = visitExp(body0, SyntacticEnv.Handler)
          mapN(fparamsValPrefix, fparamsValSuffix, bodyVal) {
            case (fparamsPrefix, fparamsSuffix, body) => WeededAst.HandlerRule(op, fparamsPrefix ++ fparamsSuffix, body)
          }
      }
      val loc = mkSL(sp1, sp2)
      mapN(expVal, rulesVal) {
        case (exp, rules) => WeededAst.Expr.TryWith(exp, eff, rules, loc)
      }

    case ParsedAst.Expression.SelectChannel(sp1, rules, exp, sp2) =>
      val rulesVal = traverse(rules) {
        case ParsedAst.SelectChannelRule(ident, chan, body) => mapN(visitExp(chan, senv), visitExp(body, senv)) {
          case (c, b) => WeededAst.SelectChannelRule(ident, c, b)
        }
      }

      val defaultVal = exp match {
        case Some(exp) => visitExp(exp, senv).map {
          case e => Some(e)
        }
        case None => Validation.success(None)
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => WeededAst.Expr.SelectChannel(rs, d, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Spawn(sp1, exp1, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)
      val exp1Val = visitExp(exp1, senv)
      val exp2Val = visitExp(exp2, senv)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) => WeededAst.Expr.Spawn(e1, e2, loc)
      }

    case ParsedAst.Expression.ParYield(sp1, frags, exp, sp2) =>
      val fragVals = traverse(frags) {
        case ParsedAst.ParYieldFragment(fsp1, pat, e, fsp2) => mapN(visitPattern(pat), visitExp(e, senv)) {
          case (p, e1) => WeededAst.ParYieldFragment(p, e1, mkSL(fsp1, fsp2))
        }
      }

      mapN(fragVals, visitExp(exp, senv)) {
        case (fs, e) => WeededAst.Expr.ParYield(fs, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Lazy(sp1, exp, sp2) =>
      visitExp(exp, senv).map {
        case e => WeededAst.Expr.Lazy(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Force(sp1, exp, sp2) =>
      visitExp(exp, senv).map {
        case e => WeededAst.Expr.Force(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointConstraint(sp1, con, sp2) =>
      val loc = mkSL(sp1, sp2)

      mapN(visitConstraint(con, senv)) {
        case c => WeededAst.Expr.FixpointConstraintSet(c :: Nil, loc)
      }

    case ParsedAst.Expression.FixpointConstraintSet(sp1, cs0, sp2) =>
      val loc = mkSL(sp1, sp2)

      traverse(cs0)(visitConstraint(_, senv)).map {
        case cs => WeededAst.Expr.FixpointConstraintSet(cs, loc)
      }

    case ParsedAst.Expression.FixpointLambda(sp1, pparams, exp, sp2) =>
      val psVal = traverse(pparams)(visitPredicateParam)
      val eVal = visitExp(exp, senv)
      val loc = mkSL(sp1, sp2)
      mapN(psVal, eVal) {
        case (ps, e) => WeededAst.Expr.FixpointLambda(ps, e, loc)
      }

    case ParsedAst.Expression.FixpointCompose(exp1, exp2, sp2) =>
      mapN(visitExp(exp1, senv), visitExp(exp2, senv)) {
        case (e1, e2) =>
          val sp1 = leftMostSourcePosition(exp1)
          WeededAst.Expr.FixpointMerge(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointInjectInto(sp1, exps, idents, sp2) =>
      val loc = mkSL(sp1, sp2)
      flatMapN(traverse(exps)(visitExp(_, senv))) {
        case _ if exps.length != idents.length =>
          // Check for mismatched arity
          val err = MismatchedArity(exps.length, idents.length, loc)
          Validation.toSoftFailure(WeededAst.Expr.Error(err), err)
        case es => Validation.success(WeededAst.Expr.FixpointInjectInto(es, idents.toList, loc))
      }

    case ParsedAst.Expression.FixpointSolveWithProject(sp1, exps, optIdents, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val esVal = traverse(exps)(visitExp(_, senv))
      val opts = optIdents.map(_.toList)
      mapN(esVal) {
        case es => WeededAst.Expr.FixpointSolveWithProject(es, opts, loc)
      }

    case ParsedAst.Expression.FixpointQueryWithSelect(sp1, exps0, selects0, from0, whereExp0, sp2) =>
      val loc = mkSL(sp1, sp2).asSynthetic
      val esVal = traverse(exps0)(visitExp(_, senv))
      val selectsVal = traverse(selects0)(visitExp(_, senv))
      val fromVal = traverse(from0)(visitPredicateBody(_, senv))
      val whereVal = traverse(whereExp0)(visitExp(_, senv))
      mapN(esVal, selectsVal, fromVal, whereVal) {
        case (exps, selects, from, where) =>
          WeededAst.Expr.FixpointQueryWithSelect(exps, selects, from, where, loc)
      }

    case ParsedAst.Expression.Debug(sp1, kind, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      val eVal = visitExp(exp, senv)
      val kind1 = visitDebugKind(kind)
      mapN(eVal)(WeededAst.Expr.Debug(_, kind1, loc))

  }

  /**
    * Returns a restrictable choice rule from an already visited pattern, guard, and body.
    * It is checked that
    *
    * - The guard is absent
    *
    * - The patterns are only tags with possible terms of variables and wildcards
    */
  private def createRestrictableChooseRule(star: Boolean, p0: WeededAst.Pattern, g0: Option[WeededAst.Expr], b0: WeededAst.Expr): Validation[WeededAst.RestrictableChooseRule, WeederError] = {
    // Check that guard is not present
    val gVal = g0 match {
      case Some(g) => Validation.toHardFailure(IllegalRestrictableChooseGuard(star, g.loc))
      case None => Validation.success(())
    }
    // Check that patterns are only tags of variables (or wildcards as variables, or unit)
    val pVal = p0 match {
      case Pattern.Tag(qname, pat, loc) =>
        val innerVal = pat match {
          case Pattern.Tuple(elms, _) =>
            traverse(elms) {
              case Pattern.Wild(loc) => Validation.success(WeededAst.RestrictableChoosePattern.Wild(loc))
              case Pattern.Var(ident, loc) => Validation.success(WeededAst.RestrictableChoosePattern.Var(ident, loc))
              case Pattern.Cst(Ast.Constant.Unit, loc) => Validation.success(WeededAst.RestrictableChoosePattern.Wild(loc))
              case other => Validation.toHardFailure(UnsupportedRestrictedChoicePattern(star, other.loc))
            }
          case Pattern.Wild(loc) => Validation.success(List(WeededAst.RestrictableChoosePattern.Wild(loc)))
          case Pattern.Var(ident, loc) => Validation.success(List(WeededAst.RestrictableChoosePattern.Var(ident, loc)))
          case Pattern.Cst(Ast.Constant.Unit, loc) => Validation.success(List(WeededAst.RestrictableChoosePattern.Wild(loc)))
          case other => Validation.toHardFailure(UnsupportedRestrictedChoicePattern(star, other.loc))
        }
        mapN(innerVal) {
          case inner => WeededAst.RestrictableChoosePattern.Tag(qname, inner, loc)
        }
      case other => Validation.toHardFailure(UnsupportedRestrictedChoicePattern(star, p0.loc))
    }
    mapN(gVal, pVal) {
      case (_, p) => WeededAst.RestrictableChooseRule(p, b0)
    }
  }

  /**
    * Performs weeding on the given argument.
    *
    * Named arguments are transformed into records.
    * `f(arg = x)` becomes `f({arg = x})`
    */
  private def visitArgument(arg: ParsedAst.Argument, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.Expr, WeederError] = arg match {
    // Case 1: Named parameter. Turn it into a record.
    case ParsedAst.Argument.Named(name, exp0, sp2) =>
      visitExp(exp0, senv).map {
        exp =>
          val loc = mkSL(name.sp1, sp2)
          WeededAst.Expr.RecordExtend(Name.mkLabel(name), exp, WeededAst.Expr.RecordEmpty(loc), loc)
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
    case class Operator(op: SemanticOp) extends OperatorResult

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
        case "not" => OperatorResult.Operator(SemanticOp.BoolOp.Not)
        case "-" => OperatorResult.BuiltIn(Name.mkQName("Neg.neg", sp1, sp2))
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

  /**
    * Translates the hex code into the corresponding character.
    * Returns an error if the code is not hexadecimal.
    */
  private def translateHexCode(code: String, loc: SourceLocation): Result[Char, MalformedUnicodeEscapeSequence] = {
    try {
      Result.Ok(Integer.parseInt(code, 16).toChar)
    } catch {
      case _: NumberFormatException => Result.Err(MalformedUnicodeEscapeSequence(code, loc))
    }
  }

  /**
    * Performs weeding on the given sequence of CharCodes.
    */
  private def visitCharSeq(chars0: Seq[ParsedAst.CharCode]): Result[String, WeederError with Recoverable] = {

    @tailrec
    def visit(chars: List[ParsedAst.CharCode], acc: List[Char]): Result[String, WeederError with Recoverable] = {
      chars match {
        // Case 1: End of the sequence
        case Nil => Result.Ok(acc.reverse.mkString)
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
            case _ => Result.Err(IllegalEscapeSequence('$', mkSL(sp1, sp2)))
          }

          // Case 3.3 Debug escape
          case "%" => rest match {
            case ParsedAst.CharCode.Literal(_, "{", _) :: rest2 => visit(rest2, '{' :: '%' :: acc)
            case _ => Result.Err(IllegalEscapeSequence('%', mkSL(sp1, sp2)))
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
                case Result.Ok(char) => visit(rest2, char :: acc)
                case Result.Err(e) => Result.Err(e)
              }
            // Case 3.3.2: `\\u` followed by less than 4 literals
            case rest2 =>
              val code = rest2.takeWhile(_.isInstanceOf[ParsedAst.CharCode.Literal])
              val sp2 = code.lastOption.getOrElse(esc).sp2
              Result.Err(MalformedUnicodeEscapeSequence(code.mkString, mkSL(esc.sp1, sp2)))
          }

          // Case 3.4: Invalid escape character
          case _ => Result.Err(IllegalEscapeSequence(char.head, mkSL(sp1, sp2)))
        }
      }
    }

    visit(chars0.toList, Nil)
  }

  /**
    * Performs weeding on the given literal.
    */
  private def visitLiteral(lit0: ParsedAst.Literal): Result[Ast.Constant, WeederError with Recoverable] = lit0 match {
    case ParsedAst.Literal.Unit(_, _) =>
      Result.Ok(Ast.Constant.Unit)

    case ParsedAst.Literal.Null(_, _) =>
      Result.Ok(Ast.Constant.Null)

    case ParsedAst.Literal.True(_, _) =>
      Result.Ok(Ast.Constant.Bool(true))

    case ParsedAst.Literal.False(_, _) =>
      Result.Ok(Ast.Constant.Bool(false))

    case ParsedAst.Literal.Char(sp1, chars, sp2) =>
      visitCharSeq(chars) flatMap {
        case s if s.length == 1 => Result.Ok(Ast.Constant.Char(s.head))
        case s => Result.Err(MalformedChar(s, mkSL(sp1, sp2)))
      }

    case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
      toFloat32(sign, before, after, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Float32(lit)
      }

    case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
      toFloat64(sign, before, after, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Float64(lit)
      }

    case ParsedAst.Literal.BigDecimal(sp1, sign, before, after, power, sp2) =>
      toBigDecimal(sign, before, after, power, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.BigDecimal(lit)
      }

    case ParsedAst.Literal.Int8(sp1, sign, radix, digits, sp2) =>
      toInt8(sign, radix, digits, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Int8(lit)
      }

    case ParsedAst.Literal.Int16(sp1, sign, radix, digits, sp2) =>
      toInt16(sign, radix, digits, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Int16(lit)
      }

    case ParsedAst.Literal.Int32(sp1, sign, radix, digits, sp2) =>
      toInt32(sign, radix, digits, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Int32(lit)
      }

    case ParsedAst.Literal.Int64(sp1, sign, radix, digits, sp2) =>
      toInt64(sign, radix, digits, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.Int64(lit)
      }

    case ParsedAst.Literal.BigInt(sp1, sign, radix, digits, sp2) =>
      toBigInt(sign, radix, digits, mkSL(sp1, sp2)).map {
        case lit => Ast.Constant.BigInt(lit)
      }

    case ParsedAst.Literal.Str(_, chars, _) =>
      visitCharSeq(chars).map {
        string => Ast.Constant.Str(string)
      }

    case ParsedAst.Literal.Regex(sp1, chars, sp2) =>
      visitCharSeq(chars).flatMap {
        case s => toRegexPattern(s, mkSL(sp1, sp2)).map {
          case pat => Ast.Constant.Regex(pat)
        }
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
        val loc = mkSL(sp1, sp2)

        // Check if the identifier is a wildcard.
        if (ident.name == "_") {
          Validation.success(WeededAst.Pattern.Wild(loc))
        } else {
          // Check for [[NonLinearPattern]].
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              Validation.success(WeededAst.Pattern.Var(ident, loc))
            case Some(otherIdent) =>
              Validation.toSoftFailure(WeededAst.Pattern.Var(ident, loc), NonLinearPattern(ident.name, otherIdent.loc, loc))
          }
        }

      case ParsedAst.Pattern.Lit(sp1, lit, sp2) =>
        val loc = mkSL(sp1, sp2)

        visitLiteral(lit) match {
          case Result.Ok(c) => c match {
            case Constant.Null =>
              Validation.toSoftFailure(WeededAst.Pattern.Error(loc), IllegalNullPattern(loc))
            case Constant.Regex(lit) =>
              Validation.toSoftFailure(WeededAst.Pattern.Error(loc), IllegalRegexPattern(loc))
            case c =>
              Validation.success(WeededAst.Pattern.Cst(c, loc))
          }
          case Result.Err(e) => Validation.toSoftFailure(WeededAst.Pattern.Error(loc), e)
        }

      case ParsedAst.Pattern.Tag(sp1, qname, o, sp2) =>
        /*
         * Introduce implicit unit, if needed.
         */
        o match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val lit = WeededAst.Pattern.Cst(Ast.Constant.Unit, loc.asSynthetic)
            Validation.success(WeededAst.Pattern.Tag(qname, lit, loc))
          case Some(pat) => visit(pat).map {
            case p => WeededAst.Pattern.Tag(qname, p, mkSL(sp1, sp2))
          }
        }

      case ParsedAst.Pattern.Tuple(sp1, pats, sp2) =>
        val loc = mkSL(sp1, sp2)

        /*
         * Rewrites empty tuples to Unit and eliminate single-element tuples.
         */
        traverse(pats)(visit).map {
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

      case ParsedAst.Pattern.Record(sp1, pats, rest, sp2) =>
        val loc = mkSL(sp1, sp2)
        val fsVal = traverse(pats) {
          case ParsedAst.Pattern.RecordLabelPattern(sp11, label, pat, sp22) =>
            val patLoc = mkSL(sp11, sp22)
            flatMapN(visitIdent(label), traverseOpt(pat)(visit)) {
              case (id, p) if p.isEmpty =>
                // Check for [[NonLinearPattern]].
                seen.get(id.name) match {
                  case None =>
                    // It was unseen until now, so we add it to the seen variables.
                    seen += id.name -> id
                    val l = Name.mkLabel(id)
                    val pat = WeededAst.Pattern.Record.RecordLabelPattern(l, p, patLoc)
                    Validation.success(pat)
                  case Some(otherIdent) =>
                    val l = Name.mkLabel(id)
                    val pat = WeededAst.Pattern.Record.RecordLabelPattern(l, p, patLoc)
                    Validation.toSoftFailure(pat, NonLinearPattern(id.name, otherIdent.loc, id.loc))
                }
              case (id, p) =>
                val l = Name.mkLabel(id)
                val patLoc = mkSL(sp11, sp22)
                Validation.success(WeededAst.Pattern.Record.RecordLabelPattern(l, p, patLoc))
            }
        }
        val rsVal = traverseOpt(rest)(visit)
        flatMapN(fsVal, rsVal) {
          // Pattern { ... }
          case (fs, None) =>
            Validation.success(WeededAst.Pattern.Record(fs, WeededAst.Pattern.RecordEmpty(loc.asSynthetic), loc))

          // Pattern { x, ... | r }
          case (x :: xs, Some(Pattern.Var(v, l))) =>
            Validation.success(WeededAst.Pattern.Record(x :: xs, Pattern.Var(v, l), loc))

          // Pattern { x, ... | _ }
          case (x :: xs, Some(Pattern.Wild(l))) =>
            Validation.success(WeededAst.Pattern.Record(x :: xs, Pattern.Wild(l), loc))

          // Bad Pattern { | r }
          case (Nil, Some(r)) => Validation.toSoftFailure(r, EmptyRecordExtensionPattern(r.loc))

          // Bad Pattern e.g., { x, ... | (1, 2, 3) }
          case (_, Some(r)) => Validation.toSoftFailure(WeededAst.Pattern.Error(r.loc), IllegalRecordExtensionPattern(r.loc))
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
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)

      //
      // Check for `[[IllegalFixedAtom]]`.
      //
      val errors = (polarity, fixity) match {
        case (Ast.Polarity.Negative, Ast.Fixity.Fixed) => Some(IllegalFixedAtom(loc))
        case _ => None
      }

      mapN(traverse(terms)(visitPattern)) {
        case ts => WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, ts, loc)
      }.withSoftFailures(errors)

    case ParsedAst.Predicate.Body.Atom(sp1, polarity, fixity, ident, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern), visitPattern(term)) {
        case (ts, t) =>
          WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, fixity, ts ::: t :: Nil, loc)
      }

    case ParsedAst.Predicate.Body.Functional(sp1, idents, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Predicate.Body.Functional(idents.toList, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Predicate.Body.Guard(sp1, exp, sp2) =>
      mapN(visitExp(exp, senv)) {
        case e => WeededAst.Predicate.Body.Guard(e, mkSL(sp1, sp2))
      }

  }

  /**
    * Weeds the given sequence of parsed annotation `xs`.
    */
  private def visitAnnotations(l: Seq[ParsedAst.Annotation])(implicit flix: Flix): Validation[Ast.Annotations, WeederError] = {
    //
    // Check for [[DuplicateAnnotation]].
    //
    val errors = mutable.ListBuffer.empty[WeederError.DuplicateAnnotation]
    val seen = mutable.Map.empty[String, ParsedAst.Annotation]

    for (a <- l) {
      seen.get(a.ident.name) match {
        case None =>
          seen += (a.ident.name -> a)
        case Some(otherAnn) =>
          val name = a.ident.name
          val loc1 = otherAnn.ident.loc
          val loc2 = a.ident.loc
          // NB: We report an error at both source locations.
          errors += DuplicateAnnotation(name, loc1, loc2)
          errors += DuplicateAnnotation(name, loc2, loc1)
      }
    }

    traverse(l)(visitAnnotation).withSoftFailures(errors).map {
      case as => Ast.Annotations(as)
    }
  }

  /**
    * Performs weeding on the given annotation.
    */
  private def visitAnnotation(ann: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = ann match {
    case ParsedAst.Annotation(_, ident, _) => ident.name match {
      case "benchmark" => Validation.success(Ast.Annotation.Benchmark(ident.loc))
      case "test" => Validation.success(Ast.Annotation.Test(ident.loc))
      case "Test" => Validation.success(Ast.Annotation.Test(ident.loc))
      case "Deprecated" => Validation.success(Ast.Annotation.Deprecated(ident.loc))
      case "Experimental" => Validation.success(Ast.Annotation.Experimental(ident.loc))
      case "Internal" => Validation.success(Ast.Annotation.Internal(ident.loc))
      case "Parallel" => Validation.success(Ast.Annotation.Parallel(ident.loc))
      case "ParallelWhenPure" => Validation.success(Ast.Annotation.ParallelWhenPure(ident.loc))
      case "Lazy" => Validation.success(Ast.Annotation.Lazy(ident.loc))
      case "LazyWhenPure" => Validation.success(Ast.Annotation.LazyWhenPure(ident.loc))
      case "MustUse" => Validation.success(Ast.Annotation.MustUse(ident.loc))
      case "Skip" => Validation.success(Ast.Annotation.Skip(ident.loc))
      case "Tailrec" => Validation.success(Ast.Annotation.TailRecursive(ident.loc))
      case name => Validation.toSoftFailure(Ast.Annotation.Error(name, ident.loc), UndefinedAnnotation(name, ident.loc))
    }
  }

  /**
    * Weeds the given sequence of parsed modifiers `xs`.
    */
  private def visitModifiers(l: Seq[ParsedAst.Modifier], legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifiers, WeederError] = {
    //
    // Check for [[DuplicateModifier]].
    //
    val errors = mutable.ListBuffer.empty[WeederError.DuplicateModifier]
    val seen = mutable.Map.empty[String, ParsedAst.Modifier]

    for (m <- l) {
      seen.get(m.name) match {
        case None =>
          seen += (m.name -> m)
        case Some(otherMod) =>
          val name = m.name
          val loc1 = mkSL(otherMod.sp1, otherMod.sp2)
          val loc2 = mkSL(m.sp1, m.sp2)
          // NB: We report an error at both source locations.
          errors += DuplicateModifier(name, loc1, loc2)
          errors += DuplicateModifier(name, loc2, loc1)
      }
    }

    traverse(l)(visitModifier(_, legalModifiers)).withSoftFailures(errors).map {
      case ms => Ast.Modifiers(ms)
    }
  }

  /**
    * Weeds the given parsed modifier `m`.
    */
  private def visitModifier(m: ParsedAst.Modifier, legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifier, WeederError] = {
    val modifier = m.name match {
      case "lawful" => Ast.Modifier.Lawful
      case "override" => Ast.Modifier.Override
      case "pub" => Ast.Modifier.Public
      case "sealed" => Ast.Modifier.Sealed
      case s =>
        // The Parser ensures that a modifier is one of the above.
        throw InternalCompilerException(s"Unknown modifier '$s'.", mkSL(m.sp1, m.sp2))
    }

    // Check for [[IllegalModifier]].
    if (legalModifiers.contains(modifier))
      Validation.success(modifier)
    else
      Validation.toSoftFailure(modifier, IllegalModifier(mkSL(m.sp1, m.sp2)))
  }

  /**
    * Returns an error if `public` is not among the modifiers in `mods`.
    */
  private def requirePublic(mods: Seq[ParsedAst.Modifier], ident: Name.Ident): Validation[Unit, WeederError] = {
    if (mods.exists(_.name == "pub")) {
      Validation.success(())
    } else {
      Validation.toSoftFailure((), IllegalPrivateDeclaration(ident, ident.loc))
    }
  }

  /**
    * Returns an error if type parameters are present.
    */
  private def requireNoTypeParams(tparams0: ParsedAst.TypeParams): Validation[Unit, WeederError] = tparams0 match {
    case TypeParams.Elided => Validation.success(())
    case TypeParams.Explicit(tparams) =>
      // safe to take head and tail since parsing ensures nonempty type parameters if explicit
      val sp1 = tparams.head.sp1
      val sp2 = tparams.last.sp2
      Validation.toSoftFailure((), IllegalEffectTypeParams(mkSL(sp1, sp2)))
  }

  /**
    * Returns an error if a type is present.
    */
  private def requireNoEffect(tpe: Option[ParsedAst.Type], loc: SourceLocation): Validation[Unit, WeederError] = tpe match {
    case None => Validation.success(())
    case Some(_) => Validation.toSoftFailure((), IllegalEffectfulOperation(loc))
  }

  /**
    * Weeds the given parsed type `tpe`.
    */
  private def visitType(tpe: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = tpe match {
    case ParsedAst.Type.Var(sp1, ident, sp2) =>
      Validation.success(WeededAst.Type.Var(ident, ident.loc))

    case ParsedAst.Type.Ambiguous(sp1, qname, sp2) =>
      Validation.success(WeededAst.Type.Ambiguous(qname, mkSL(sp1, sp2)))

    case ParsedAst.Type.Tuple(sp1, elms0, sp2) =>
      val elmsVal = traverse(elms0)(visitType)
      mapN(elmsVal) {
        case elms => WeededAst.Type.Tuple(elms, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Record(sp1, labels, restOpt, sp2) =>
      val rowVal = buildRecordRow(labels, restOpt, mkSL(sp1, sp2))
      mapN(rowVal) {
        case row => WeededAst.Type.Record(row, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.RecordRow(sp1, labels, restOpt, sp2) =>
      buildRecordRow(labels, restOpt, mkSL(sp1, sp2))

    case ParsedAst.Type.Schema(sp1, predicates, restOpt, sp2) =>
      val rowVal = buildSchemaRow(predicates, restOpt, mkSL(sp1, sp2))
      mapN(rowVal) {
        case row => WeededAst.Type.Schema(row, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.SchemaRow(sp1, predicates, restOpt, sp2) =>
      buildSchemaRow(predicates, restOpt, mkSL(sp1, sp2))

    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, tpe2, eff0, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      val effVal = traverseOpt(eff0)(visitType)
      mapN(t1Val, t2Val, effVal) {
        case (t1, t2, eff) => mkArrow(t1, eff, t2, loc)
      }

    case ParsedAst.Type.PolymorphicArrow(sp1, tparams, tresult, eff0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val tsVal = traverse(tparams)(visitType)
      val trVal = visitType(tresult)
      val effVal = traverseOpt(eff0)(visitType)
      mapN(tsVal, trVal, effVal) {
        case (ts, tr, eff) => mkCurriedArrow(ts, eff, tr, loc)
      }

    case ParsedAst.Type.Native(sp1, fqn, sp2) =>
      Validation.success(WeededAst.Type.Native(fqn.toString, mkSL(sp1, sp2)))

    case ParsedAst.Type.Apply(tpe1, args0, sp2) =>
      // Curry the type arguments.
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val argsVal = traverse(args0)(visitType)
      mapN(t1Val, argsVal) {
        case (t1, args) =>
          args.foldLeft(t1) {
            case (acc, t2) => WeededAst.Type.Apply(acc, t2, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Type.True(sp1, sp2) =>
      Validation.success(WeededAst.Type.True(mkSL(sp1, sp2)))

    case ParsedAst.Type.False(sp1, sp2) =>
      Validation.success(WeededAst.Type.False(mkSL(sp1, sp2)))

    case ParsedAst.Type.Not(sp1, tpe, sp2) =>
      val tVal = visitType(tpe)
      mapN(tVal) {
        case t => WeededAst.Type.Not(t, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.And(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.And(t1, t2, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Or(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.Or(t1, t2, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Xor(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      val loc = mkSL(sp1, sp2)
      mapN(t1Val, t2Val) {
        case (t1, t2) =>
          val l = WeededAst.Type.And(t1, WeededAst.Type.Not(t2, loc), loc)
          val r = WeededAst.Type.And(WeededAst.Type.Not(t1, loc), t2, loc)
          WeededAst.Type.Or(l, r, loc)
      }

    case ParsedAst.Type.Complement(sp1, tpe, sp2) =>
      val tVal = visitType(tpe)
      mapN(tVal) {
        case t => WeededAst.Type.Complement(t, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Union(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.Union(t1, t2, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Intersection(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.Intersection(t1, t2, mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Difference(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      val loc = mkSL(sp1, sp2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.Intersection(t1, WeededAst.Type.Complement(t2, loc), loc)
      }

    case ParsedAst.Type.Pure(sp1, sp2) =>
      val loc = mkSL(sp1, sp2)
      Validation.success(WeededAst.Type.Pure(loc))

    case ParsedAst.Type.Impure(sp1, sp2) =>
      val loc = mkSL(sp1, sp2)
      // TODO EFF-MIGRATION create dedicated Impure type
      Validation.success(WeededAst.Type.Complement(WeededAst.Type.Pure(loc), loc))

    case ParsedAst.Type.EffectSet(sp1, tpes0, sp2) =>
      val checkVal = traverseX(tpes0)(checkEffectSetMember)
      val tpesVal = traverse(tpes0)(visitType)
      val loc = mkSL(sp1, sp2)
      mapN(checkVal, tpesVal) {
        case ((), tpes) =>
          val effOpt = tpes.reduceLeftOption({
            case (acc, tpe) => WeededAst.Type.Union(acc, tpe, loc)
          }: (WeededAst.Type, WeededAst.Type) => WeededAst.Type)
          effOpt.getOrElse(WeededAst.Type.Pure(loc))
      }

    case ParsedAst.Type.CaseSet(sp1, cases, sp2) =>
      val loc = mkSL(sp1, sp2)
      Validation.success(WeededAst.Type.CaseSet(cases.toList, loc))

    case ParsedAst.Type.CaseUnion(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.CaseUnion(t1, t2, loc)
      }

    case ParsedAst.Type.CaseIntersection(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.CaseIntersection(t1, t2, loc)
      }

    case ParsedAst.Type.CaseDifference(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val loc = mkSL(sp1, sp2)
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => WeededAst.Type.CaseIntersection(t1, WeededAst.Type.CaseComplement(t2, loc), loc)
      }

    case ParsedAst.Type.CaseComplement(sp1, tpe, sp2) =>
      val loc = mkSL(sp1, sp2)
      val tVal = visitType(tpe)
      mapN(tVal) {
        case t => WeededAst.Type.CaseComplement(t, loc)
      }

    case ParsedAst.Type.Ascribe(tpe, kind, sp2) =>
      val sp1 = leftMostSourcePosition(tpe)
      val tVal = visitType(tpe)
      val k = visitKind(kind)
      mapN(tVal) {
        case t => WeededAst.Type.Ascribe(t, k, mkSL(sp1, sp2))
      }
  }

  /**
    * Weeds the given type. Returns None if the type is a wildcard.
    */
  private def visitTypeNoWild(tpe: ParsedAst.Type): Validation[Option[WeededAst.Type], WeederError] = tpe match {
    case ParsedAst.Type.Var(_, ident, _) if ident.isWild =>
      Validation.success(None)
    case _ => visitType(tpe).map(t => Some(t))
  }

  /**
    * Checks that the effect set member is valid: a variable or constant.
    */
  private def checkEffectSetMember(t: ParsedAst.Type): Validation[Unit, WeederError] = t match {
    case _: ParsedAst.Type.Var => Validation.success(())
    case _: ParsedAst.Type.Ambiguous => Validation.success(())
    case _: ParsedAst.Type.True => Validation.success(())
    case _: ParsedAst.Type.False => Validation.success(())
    case _: ParsedAst.Type.Pure => Validation.success(())
    case _: ParsedAst.Type.Impure => Validation.success(())
    case _ =>
      val sp1 = leftMostSourcePosition(t)
      val sp2 = t.sp2
      Validation.toSoftFailure((), IllegalEffectSetMember(mkSL(sp1, sp2)))
  }

  /**
    * Builds a record row from the given labels and optional rest variable.
    */
  private def buildRecordRow(labels0: Seq[ParsedAst.RecordLabelType], restOpt: Option[Name.Ident], loc: SourceLocation): Validation[WeededAst.Type, WeederError] = {
    // If rest is absent, then it is the empty record row
    val rest = restOpt match {
      case None => WeededAst.Type.RecordRowEmpty(loc)
      case Some(name) => WeededAst.Type.Var(name, name.loc)
    }

    val labelsVal = traverse(labels0) {
      case ParsedAst.RecordLabelType(sp1, ident, tpe, sp2) =>
        mapN(visitType(tpe)) {
          case t => (Name.mkLabel(ident), t)
        }
    }

    mapN(labelsVal) {
      case labels =>
        labels.foldRight(rest) {
          case ((label, tpe), acc) =>
            WeededAst.Type.RecordRowExtend(label, tpe, acc, loc)
        }
    }

  }

  /**
    * Builds a schema row from the given predicates and optional rest identifier.
    */
  private def buildSchemaRow(predicates: Seq[ParsedAst.PredicateType], restOpt: Option[Name.Ident], loc: SourceLocation): Validation[WeededAst.Type, WeederError] = {
    // If rest is absent, then it is the empty schema row
    val rest = restOpt match {
      case None => WeededAst.Type.SchemaRowEmpty(loc)
      case Some(name) => WeededAst.Type.Var(name, name.loc)
    }

    // TODO should be non-short-circuiting
    Validation.foldRight(predicates)(Validation.success(rest)) {
      case (ParsedAst.PredicateType.PredicateWithAlias(ssp1, qname, targs, ssp2), acc) =>
        val tsVal = traverse(targs.getOrElse(Nil))(visitType)
        mapN(tsVal) {
          case ts => WeededAst.Type.SchemaRowExtendByAlias(qname, ts, acc, mkSL(ssp1, ssp2))
        }

      case (ParsedAst.PredicateType.RelPredicateWithTypes(ssp1, name, ts0, ssp2), acc) =>
        val tsVal = traverse(ts0)(visitType)
        mapN(tsVal) {
          case ts => WeededAst.Type.SchemaRowExtendByTypes(name, Ast.Denotation.Relational, ts, acc, mkSL(ssp1, ssp2))
        }

      case (ParsedAst.PredicateType.LatPredicateWithTypes(ssp1, name, ts, tpe, ssp2), acc) =>
        val tsVal = traverse(ts :+ tpe)(visitType)
        mapN(tsVal) {
          case ts => WeededAst.Type.SchemaRowExtendByTypes(name, Ast.Denotation.Latticenal, ts, acc, mkSL(ssp1, ssp2))
        }
    }
  }

  /**
    * Returns an arrow type from `tpe1` to `tpe2` with effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->{eff} tpe2`
    */
  private def mkArrow(tpe1: WeededAst.Type, eff: Option[WeededAst.Type], tpe2: WeededAst.Type, loc: SourceLocation): WeededAst.Type =
    WeededAst.Type.Arrow(List(tpe1), eff, tpe2, loc.asSynthetic)

  /**
    * Returns a sequence of arrow types type from `tparams` to `tresult` where every arrow is pure except the last which has effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->> tpe2 ->> ... ->{eff} tresult`.
    */
  private def mkCurriedArrow(tparams: Seq[WeededAst.Type], eff: Option[WeededAst.Type], tresult: WeededAst.Type, loc: SourceLocation): WeededAst.Type = {
    val l = loc.asSynthetic
    val base = mkArrow(tparams.last, eff, tresult, l)
    tparams.init.foldRight(base)(mkArrow(_, None, _, l))
  }

  /**
    * Weeds the given list of formal parameter `fparams`.
    *
    * Checks for [[DuplicateFormalParam]], [[MissingFormalParamAscription]], and [[DuplicateFormalParam]].
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
      return Validation.success(List(WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe, loc)))
    }

    //
    // Check for [[DuplicateFormalParam]]
    //
    val seen = mutable.Map.empty[String, ParsedAst.FormalParam]
    val errors = mutable.ArrayBuffer.empty[WeederError.DuplicateFormalParam]
    for (fparam <- fparams) {
      seen.get(fparam.ident.name) match {
        case None =>
          if (!fparam.ident.name.startsWith("_")) {
            // Wildcards cannot be duplicate.
            seen += (fparam.ident.name -> fparam)
          }
        case Some(otherParam) =>
          val name = fparam.ident.name
          val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
          val loc2 = mkSL(fparam.sp1, fparam.sp2)
          // NB: We report an error at both source locations.
          errors += DuplicateFormalParam(name, loc1, loc2)
          errors += DuplicateFormalParam(name, loc2, loc1)
      }
    }

    traverse(fparams)(visitFormalParam(_, typePresence)).withSoftFailures(errors)
  }

  /**
    * Weeds the given formal parameter `fparam`.
    */
  private def visitFormalParam(fparam: ParsedAst.FormalParam, typePresence: Presence): Validation[WeededAst.FormalParam, WeederError] = fparam match {
    case ParsedAst.FormalParam(sp1, mods, ident, tpeOpt0, sp2) =>
      val tpeOptVal = traverseOpt(tpeOpt0)(visitType)

      //
      // Check for [[MissingFormalParamAscription]] and [[IllegalFormalParamAscription]]
      //
      flatMapN(visitModifiers(mods, legalModifiers = Set.empty), tpeOptVal) {
        case (mod, tpeOpt) =>
          (tpeOpt, typePresence) match {
            // Case 1: Required but missing. Error.
            case (None, Presence.Required) => Validation.toHardFailure(MissingFormalParamAscription(ident.name, mkSL(sp1, sp2)))
            // Case 2: Forbidden but present. Error.
            case (Some(_), Presence.Forbidden) => Validation.toHardFailure(IllegalFormalParamAscription(mkSL(sp1, sp2)))
            // Case 3: No violation. Good to go.
            case _ => Validation.success(WeededAst.FormalParam(ident, mod, tpeOpt, mkSL(sp1, sp2)))
          }
      }
  }

  /**
    * Weeds the given predicate param `pparam`.
    */
  private def visitPredicateParam(pparam: ParsedAst.PredicateParam): Validation[WeededAst.PredicateParam, WeederError] = pparam match {
    case ParsedAst.PredicateParam.UntypedPredicateParam(sp1, ident, sp2) =>
      val pred = Name.mkPred(ident)
      Validation.success(WeededAst.PredicateParam.PredicateParamUntyped(pred, mkSL(sp1, sp2)))

    case ParsedAst.PredicateParam.RelPredicateParam(sp1, ident, tpes, sp2) =>
      val pred = Name.mkPred(ident)
      val den = Ast.Denotation.Relational
      val tsVal = traverse(tpes)(visitType)
      mapN(tsVal) {
        case ts => WeededAst.PredicateParam.PredicateParamWithType(pred, den, ts, mkSL(sp1, sp2))
      }

    case ParsedAst.PredicateParam.LatPredicateParam(sp1, ident, tpes, tpe, sp2) =>
      val pred = Name.mkPred(ident)
      val den = Ast.Denotation.Latticenal
      val tsVal = traverse(tpes)(visitType)
      val tVal = visitType(tpe)
      mapN(tsVal, tVal) {
        case (ts, t) => WeededAst.PredicateParam.PredicateParamWithType(pred, den, ts ::: t :: Nil, mkSL(sp1, sp2))
      }
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
    case ParsedAst.TypeParams.Elided => Validation.success(WeededAst.TypeParams.Elided)
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        case (Nil, _ :: _) =>
          // Case 1: only unkinded type parameters
          Validation.success(WeededAst.TypeParams.Unkinded(unkindedTypeParams))

        case (_ :: _, Nil) =>
          // Case 2: only kinded type parameters
          Validation.success(WeededAst.TypeParams.Kinded(kindedTypeParams))

        case (_ :: _, _ :: _) =>
          // Case 3: some unkinded and some kinded

          // We recover by kinding every unkinded type parameter with Star.
          val kinded = newTparams.map {
            case WeededAst.TypeParam.Kinded(ident, kind) =>
              WeededAst.TypeParam.Kinded(ident, kind)
            case WeededAst.TypeParam.Unkinded(ident) =>
              val default = WeededAst.Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)
              WeededAst.TypeParam.Kinded(ident, default)
          }

          val r = WeededAst.TypeParams.Kinded(kindedTypeParams)
          Validation.toSoftFailure(r, MismatchedTypeParameters(mkSL(tparams.head.sp1, tparams.last.sp2)))

        case (Nil, Nil) =>
          // Case 4: no type parameters: should be prevented by parser
          throw InternalCompilerException("Unexpected empty type parameters.", SourceLocation.Unknown)
      }
  }

  /**
    * Weeds the type params, requiring that they be explicitly kinded.
    */
  private def visitKindedTypeParams(tparams0: ParsedAst.TypeParams): Validation[WeededAst.KindedTypeParams, WeederError] = tparams0 match {
    case ParsedAst.TypeParams.Elided => Validation.success(WeededAst.TypeParams.Elided)
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        case (_, _ :: _) =>
          // Case 1: We have Kinded and Unkinded type parameters.

          // We generate an error message for each unkinded type parameter.
          val errors = unkindedTypeParams.map {
            case WeededAst.TypeParam.Unkinded(ident) => MissingTypeParamKind(ident.loc)
          }

          // We then *assume* that the unkinded type parameters should have kind Star.
          val kinded = newTparams.map {
            case WeededAst.TypeParam.Kinded(ident, kind) =>
              WeededAst.TypeParam.Kinded(ident, kind)
            case WeededAst.TypeParam.Unkinded(ident) =>
              val default = WeededAst.Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)
              WeededAst.TypeParam.Kinded(ident, default)
          }

          // We can then soft fail with the missing kinds assumed to be Star.
          Validation.toSuccessOrSoftFailure(WeededAst.TypeParams.Kinded(kinded), errors)

        case (_ :: _, Nil) =>
          // Case 2: Only kinded type parameters.
          Validation.success(WeededAst.TypeParams.Kinded(kindedTypeParams))

        case (Nil, Nil) =>
          // Case 3: No type parameters: should be prevented by the Parser.
          throw InternalCompilerException("Unexpected empty type parameters.", SourceLocation.Unknown)
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
      val tpeVal = visitType(tparam0)
      val checkVal = if (isAllVars(tparam0)) {
        Validation.success(())
      } else {
        Validation.toHardFailure(IllegalTypeConstraintParameter(mkSL(sp1, sp2)))
      }
      mapN(tpeVal, checkVal) {
        case (tpe, check) => WeededAst.TypeConstraint(clazz, tpe, mkSL(sp1, sp2))
      }
  }

  /**
    * Weeds the given equality constraint `econstr`.
    */
  private def visitEqualityConstraint(econstr: ParsedAst.EqualityConstraint): Validation[WeededAst.EqualityConstraint, WeederError] = econstr match {
    case ParsedAst.EqualityConstraint(sp1, tpe1, tpe2, sp2) =>
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      val loc = mkSL(sp1, sp2)
      // TODO don't short-circuit
      flatMapN(t1Val, t2Val) {
        case (t1, t2) =>
          t1 match {
            case WeededAst.Type.Apply(WeededAst.Type.Ambiguous(qname, _), t11, _) =>
              Validation.success(WeededAst.EqualityConstraint(qname, t11, t2, loc))
            case _ => Validation.toHardFailure(IllegalEqualityConstraint(loc))
          }
      }
  }

  /**
    * Performs weeding on the given name `ident`.
    */
  private def visitIdent(ident: Name.Ident): Validation[Name.Ident, WeederError] = {
    if (ReservedWords.contains(ident.name)) {
      Validation.toSoftFailure(ident, ReservedName(ident, ident.loc))
    } else {
      Validation.success(ident)
    }
  }

  /**
    * Performs weeding on the given JvmMethod.
    */
  private def visitJvmMethod(method: ParsedAst.JvmMethod, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.JvmMethod, WeederError] = method match {
    case ParsedAst.JvmMethod(sp1, ident, fparams0, tpe, eff0, exp0, sp2) =>
      val fparamsVal = visitFormalParams(fparams0, Presence.Required)
      val tpeVal = visitType(tpe)
      val effVal = traverseOpt(eff0)(visitType)
      val expVal = visitExp(exp0, senv)
      mapN(fparamsVal, tpeVal, effVal, expVal) {
        case (fparams, tpe, eff, exp) => WeededAst.JvmMethod(ident, fparams, exp, tpe, eff, mkSL(sp1, sp2))
      }
  }

  /**
    * Performs weeding on the given [[ParsedAst.JvmOp]] `impl0`.
    */
  private def visitJvmOp(impl0: ParsedAst.JvmOp)(implicit flix: Flix): Validation[WeededAst.JvmOp, WeederError] = impl0 match {
    case ParsedAst.JvmOp.Constructor(fqn, sig, tpe, eff, ident) =>
      val sigVal = traverse(sig)(visitType)
      val tpeVal = visitType(tpe)
      val effVal = traverseOpt(eff)(visitType)
      mapN(sigVal, tpeVal, effVal) {
        case (s, t, e) => WeededAst.JvmOp.Constructor(fqn, s, t, e, ident)
      }

    case ParsedAst.JvmOp.Method(fqn, sig, tpe, eff, ident) =>
      val fqn1 = visitJavaClassMember(fqn)
      val sigVal = traverse(sig)(visitType)
      val tpeVal = visitType(tpe)
      val effVal = traverseOpt(eff)(visitType)
      mapN(sigVal, tpeVal, effVal) {
        case (s, t, e) => WeededAst.JvmOp.Method(fqn1, s, t, e, ident)
      }

    case ParsedAst.JvmOp.StaticMethod(fqn, sig, tpe, eff, ident) =>
      val fqn1 = visitJavaClassMember(fqn)
      val sigVal = traverse(sig)(visitType)
      val tpeVal = visitType(tpe)
      val effVal = traverseOpt(eff)(visitType)
      mapN(sigVal, tpeVal, effVal) {
        case (s, t, e) => WeededAst.JvmOp.StaticMethod(fqn1, s, t, e, ident)
      }

    case ParsedAst.JvmOp.GetField(fqn, tpe, eff, ident) =>
      mapN(visitField(fqn, tpe, eff)) {
        case (f, t, e) => WeededAst.JvmOp.GetField(f, t, e, ident)
      }

    case ParsedAst.JvmOp.PutField(fqn, tpe, eff, ident) =>
      mapN(visitField(fqn, tpe, eff)) {
        case (f, t, e) => WeededAst.JvmOp.PutField(f, t, e, ident)
      }

    case ParsedAst.JvmOp.GetStaticField(fqn, tpe, eff, ident) =>
      mapN(visitField(fqn, tpe, eff)) {
        case (f, t, e) => WeededAst.JvmOp.GetStaticField(f, t, e, ident)
      }

    case ParsedAst.JvmOp.PutStaticField(fqn, tpe, eff, ident) =>
      mapN(visitField(fqn, tpe, eff)) {
        case (f, t, e) => WeededAst.JvmOp.PutStaticField(f, t, e, ident)
      }
  }

  /**
    * Performs weeding on `tpe` and `eff`. Used as a helper function for java fields in [[visitJvmOp]].
    */
  private def visitField(fqn: ParsedAst.JavaClassMember, tpe: ParsedAst.Type, eff: Option[ParsedAst.Type])(implicit flix: Flix): Validation[(WeededAst.JavaClassMember, WeededAst.Type, Option[WeededAst.Type]), WeederError] = {
    val fqn1 = visitJavaClassMember(fqn)
    val tpeVal = visitType(tpe)
    val effVal = traverseOpt(eff)(visitType)
    mapN(tpeVal, effVal) {
      case (t, e) => (fqn1, t, e)
    }
  }

  /**
    * Maps `fqn` to a [[WeededAst.JavaClassMember]].
    */
  private def visitJavaClassMember(fqn: ParsedAst.JavaClassMember): WeededAst.JavaClassMember = fqn match {
    case ParsedAst.JavaClassMember(sp1, prefix, suffix, sp2) =>
      val loc = mkSL(sp1, sp2)
      WeededAst.JavaClassMember(prefix, suffix.toList, loc)
  }

  /**
    * Performs weeding on the given [[ParsedAst.ForFragment]] `frag0`.
    */
  private def visitForFragment(frag0: ParsedAst.ForFragment, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.ForFragment, WeederError] = frag0 match {
    case gen: ParsedAst.ForFragment.Generator => visitForFragmentGenerator(gen, senv)
    case guard: ParsedAst.ForFragment.Guard => visitForFragmentGuard(guard, senv)
    case let: ParsedAst.ForFragment.Let => visitForFragmentLet(let, senv)
  }

  /**
    * Performs weeding on the given [[ParsedAst.ForFragment.Generator]] `frag0`.
    */
  private def visitForFragmentGenerator(frag0: ParsedAst.ForFragment.Generator, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.ForFragment.Generator, WeederError] = frag0 match {
    case ParsedAst.ForFragment.Generator(sp1, pat, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      val p = visitPattern(pat)
      val e = visitExp(exp, senv)
      mapN(p, e) {
        case (p1, e1) => WeededAst.ForFragment.Generator(p1, e1, loc)
      }
  }

  /**
    * Performs weeding on the given [[ParsedAst.ForFragment.Guard]] `frag0`.
    */
  private def visitForFragmentGuard(frag0: ParsedAst.ForFragment.Guard, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.ForFragment.Guard, WeederError] = frag0 match {
    case ParsedAst.ForFragment.Guard(sp1, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      val e = visitExp(exp, senv)
      mapN(e) {
        case e1 => WeededAst.ForFragment.Guard(e1, loc)
      }
  }

  /**
    * Performs weeding on the given [[ParsedAst.ForFragment.Let]] `frag0`.
    */
  private def visitForFragmentLet(frag0: ParsedAst.ForFragment.Let, senv: SyntacticEnv)(implicit flix: Flix): Validation[WeededAst.ForFragment.Let, WeederError] = frag0 match {
    case ParsedAst.ForFragment.Let(sp1, pat, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      val pVal = visitPattern(pat)
      val eVal = visitExp(exp, senv)
      mapN(pVal, eVal) {
        case (p, e) => WeededAst.ForFragment.Let(p, e, loc)
      }
  }

  /**
    * Maps `kind` to a [[WeededAst.DebugKind]].
    */
  private def visitDebugKind(kind: ParsedAst.DebugKind): WeededAst.DebugKind = kind match {
    case ParsedAst.DebugKind.Debug => WeededAst.DebugKind.Debug
    case ParsedAst.DebugKind.DebugWithLoc => WeededAst.DebugKind.DebugWithLoc
    case ParsedAst.DebugKind.DebugWithLocAndSrc => WeededAst.DebugKind.DebugWithLocAndSrc
  }

  /**
    * Returns true iff the type is composed only of type variables possibly applied to other type variables.
    */
  private def isAllVars(tpe: ParsedAst.Type): Boolean = tpe match {
    case _: ParsedAst.Type.Var => true
    case ParsedAst.Type.Apply(tpe1, tpes, _) => (tpe1 +: tpes).forall(isAllVars)
    case _ => false
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  private def mkApplyFqn(fqn: String, args: List[WeededAst.Expr], loc: SourceLocation): WeededAst.Expr = {
    val l = loc.asSynthetic
    val lambda = WeededAst.Expr.Ambiguous(Name.mkQName(fqn), l)
    WeededAst.Expr.Apply(lambda, args, l)
  }

  /**
    * Returns a curried version of the given expression `e` for each formal parameter in `fparams0`.
    */
  private def mkCurried(fparams0: List[WeededAst.FormalParam], e: WeededAst.Expr, loc: SourceLocation): WeededAst.Expr = {
    val l = loc.asSynthetic
    fparams0.foldRight(e) {
      case (fparam, acc) => WeededAst.Expr.Lambda(fparam, acc, l)
    }
  }

  /**
    * Returns the list of expressions `args0` unless the list is empty.
    *
    * If so, returns a list with a single unit expression.
    */
  private def getArguments(args0: List[WeededAst.Expr], loc: SourceLocation): List[WeededAst.Expr] = {
    val l = loc.asSynthetic
    args0 match {
      case Nil => List(WeededAst.Expr.Cst(Ast.Constant.Unit, l))
      case as => as
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
  private def toFloat32(sign: String, before: String, after: String, loc: SourceLocation): Result[Float, MalformedFloat] = try {
    val s = s"$sign$before.$after"
    Result.Ok(stripUnderscores(s).toFloat)
  } catch {
    case _: NumberFormatException => Result.Err(MalformedFloat(loc))
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat64(sign: String, before: String, after: String, loc: SourceLocation): Result[Double, MalformedFloat] = try {
    val s = s"$sign$before.$after"
    Result.Ok(stripUnderscores(s).toDouble)
  } catch {
    case _: NumberFormatException => Result.Err(MalformedFloat(loc))
  }

  /**
    * Attempts to parse the given big decimal with `sign` digits `before` and `after` the comma.
    */
  private def toBigDecimal(sign: String, before: String, after: Option[String], power: Option[String], loc: SourceLocation): Result[BigDecimal, MalformedFloat] = try {
    val frac = after match {
      case Some(digits) => "." + digits
      case None => ""
    }
    val pow = power match {
      case Some(digits) => "e" + digits
      case None => ""
    }
    val s = s"$sign$before$frac$pow"
    Result.Ok(new BigDecimal(stripUnderscores(s)))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedFloat(loc))
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  private def toInt8(sign: String, radix: Int, digits: String, loc: SourceLocation): Result[Byte, MalformedInt] = try {
    val s = sign + digits
    Result.Ok(JByte.parseByte(stripUnderscores(s), radix))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedInt(loc))
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  private def toInt16(sign: String, radix: Int, digits: String, loc: SourceLocation): Result[Short, MalformedInt] = try {
    val s = sign + digits
    Result.Ok(JShort.parseShort(stripUnderscores(s), radix))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedInt(loc))
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  private def toInt32(sign: String, radix: Int, digits: String, loc: SourceLocation): Result[Int, MalformedInt] = try {
    val s = sign + digits
    Result.Ok(JInt.parseInt(stripUnderscores(s), radix))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedInt(loc))
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  private def toInt64(sign: String, radix: Int, digits: String, loc: SourceLocation): Result[Long, MalformedInt] = try {
    val s = sign + digits
    Result.Ok(JLong.parseLong(stripUnderscores(s), radix))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedInt(loc))
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  private def toBigInt(sign: String, radix: Int, digits: String, loc: SourceLocation): Result[BigInteger, MalformedInt] = try {
    val s = sign + digits
    Result.Ok(new BigInteger(stripUnderscores(s), radix))
  } catch {
    case _: NumberFormatException => Result.Err(MalformedInt(loc))
  }

  /**
    * Attempts to compile the given regular expression into a Pattern.
    */
  private def toRegexPattern(regex: String, loc: SourceLocation): Result[JPattern, MalformedRegex] = try {
    val pat = JPattern.compile(regex)
    Result.Ok(pat)
  } catch {
    case ex: PatternSyntaxException => Result.Err(MalformedRegex(regex, ex.getMessage, loc))
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
    case ParsedAst.Expression.LetRecDef(sp1, _, _, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetImport(sp1, _, _, _) => sp1
    case ParsedAst.Expression.NewObject(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Static(sp1, _) => sp1
    case ParsedAst.Expression.Scope(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.RestrictableChoose(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.TypeMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordLit(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordSelect(base, _, _) => leftMostSourcePosition(base)
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
    case ParsedAst.Expression.InstanceOf(e1, _, _) => leftMostSourcePosition(e1)
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
    case ParsedAst.Type.Xor(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Complement(sp1, _, _) => sp1
    case ParsedAst.Type.Difference(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Intersection(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Union(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Pure(sp1, _) => sp1
    case ParsedAst.Type.Impure(sp1, _) => sp1
    case ParsedAst.Type.EffectSet(sp1, _, _) => sp1
    case ParsedAst.Type.CaseComplement(sp1, _, _) => sp1
    case ParsedAst.Type.CaseDifference(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.CaseIntersection(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.CaseSet(sp1, _, _) => sp1
    case ParsedAst.Type.CaseUnion(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Ascribe(tpe, _, _) => leftMostSourcePosition(tpe)
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
