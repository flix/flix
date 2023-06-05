/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.language.ast.Kind.WildCaseSet
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.EqualityEnvironment
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unify
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, fold, mapN, traverse, traverseOpt}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.collection.immutable.SortedSet

/**
  * Attributes kinds to the types in the AST.
  *
  * For enums, classes, instances, and type aliases:
  * Either:
  *   - type parameters are not annotated and are then assumed all to be Star, or
  *   - type parameters are all annotated with their kinds.
  *
  * For defs:
  * Either:
  *   - type parameters are all annotated with their kinds, or
  *   - type parameters are not annotated and their kinds are inferred from their
  *     use in the formal parameters, return type and effect, and type constraints.
  *     This inference uses the following rules:
  *       - If the type variable is the type of a formal parameter, it is ascribed kind Star.
  *       - If the type variable is the return type of the function, it is ascribed kind Star.
  *       - If the type variable is the purity type of the function, it is ascribed kind Eff.
  *       - If the type variable is an argument to a type constraint, it is ascribed the class's parameter kind
  *       - If the type variable is an argument to a type constructor, it is ascribed the type constructor's parameter kind.
  *       - If the type variable is used as an type constructor, it is ascribed the kind Star -> Star ... -> Star -> X,
  *         where X is the kind inferred from enacting these rules in the place of the fully-applied type.
  *       - If there is an inconsistency among these kinds, an error is raised.
  *
  * In inferring types, variable type constructors are assumed to have kind * -> * -> * -> ???.
  *
  */
object Kinder {

  /**
    * The symbol for the IO effect.
    */
  private val IoSym = new Symbol.EffectSym(Nil, "IO", SourceLocation.Unknown)

  /**
    * The symbol for the NonDet effect.
    */
  private val NonDetSym = new Symbol.EffectSym(Nil, "NonDet", SourceLocation.Unknown)

  def run(root: ResolvedAst.Root, oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[KindedAst.Root, KindError] = flix.phase("Kinder") {

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    flatMapN(visitTypeAliases(root.taOrder, root)) {
      taenv =>

        // Extra type annotations are required due to limitations in Scala's type inference.
        val enumsVal = Validation.sequence(ParOps.parMap(root.enums)({
          pair: (Symbol.EnumSym, ResolvedAst.Declaration.Enum) =>
            val (sym, enum) = pair
            visitEnum(enum, taenv, root).map(sym -> _)
        }))

        // Extra type annotations are required due to limitations in Scala's type inference.
        val restrictableEnumsVal = Validation.sequence(ParOps.parMap(root.restrictableEnums)({
          pair: (Symbol.RestrictableEnumSym, ResolvedAst.Declaration.RestrictableEnum) =>
            val (sym, enum) = pair
            visitRestrictableEnum(enum, taenv, root).map(sym -> _)
        }))

        val classesVal = visitClasses(root, taenv, oldRoot, changeSet)

        val defsVal = visitDefs(root, taenv, oldRoot, changeSet)

        val instancesVal = Validation.sequence(ParOps.parMap(root.instances)({
          pair: (Symbol.ClassSym, List[ResolvedAst.Declaration.Instance]) =>
            val (sym, insts) = pair
            traverse(insts)(visitInstance(_, taenv, root)).map(sym -> _)
        }))

        val effectsVal = Validation.sequence(ParOps.parMap(root.effects)({
          pair: (Symbol.EffectSym, ResolvedAst.Declaration.Effect) =>
            val (sym, eff) = pair
            visitEffect(eff, taenv, root).map(sym -> _)
        }))

        mapN(enumsVal, restrictableEnumsVal, classesVal, defsVal, instancesVal, effectsVal) {
          case (enums, restrictableEnums, classes, defs, instances, effects) =>
            KindedAst.Root(classes, instances.toMap, defs, enums.toMap, restrictableEnums.toMap, effects.toMap, taenv, root.uses, root.entryPoint, root.sources, root.names)
        }
    }

  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum0: ResolvedAst.Declaration.Enum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, KindError] = enum0 match {
    case ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives, cases0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv))

      flatMapN(tparamsVal) {
        case tparams =>
          val targs = tparams.map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
          val tpe = Type.mkApply(Type.Cst(TypeConstructor.Enum(sym, getEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
          val casesVal = traverse(cases0) {
            case case0 => mapN(visitCase(case0, tparams, tpe, kenv, taenv, root)) {
              caze => caze.sym -> caze
            }
          }
          mapN(casesVal) {
            case cases => KindedAst.Enum(doc, ann, mod, sym, tparams, derives, cases.toMap, tpe, loc)
          }
      }
  }

  /**
    * Performs kinding on the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: ResolvedAst.Declaration.RestrictableEnum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.RestrictableEnum, KindError] = enum0 match {
    case ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives, cases0, loc) =>
      val kenvIndex = getKindEnvFromIndex(index0, sym)
      val kenvTparams = getKindEnvFromTypeParamsDefaultStar(tparams0)
      val kenv = KindEnv.disjointAppend(kenvIndex, kenvTparams)

      val indexVal = visitIndex(index0, sym, kenv)
      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv))

      flatMapN(indexVal, tparamsVal) {
        case (index, tparams) =>
          val targs = (index :: tparams).map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
          val tpe = Type.mkApply(Type.Cst(TypeConstructor.RestrictableEnum(sym, getRestrictableEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
          val casesVal = traverse(cases0) {
            case case0 => mapN(visitRestrictableCase(case0, index, tparams, tpe, kenv, taenv, root)) {
              caze => caze.sym -> caze
            }
          }
          mapN(casesVal) {
            case cases => KindedAst.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases.toMap, tpe, loc)
          }
      }

  }

  /**
    * Performs kinding on the given type alias.
    * Returns the kind of the type alias.
    */
  private def visitTypeAlias(alias: ResolvedAst.Declaration.TypeAlias, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.TypeAlias, KindError] = alias match {
    case ResolvedAst.Declaration.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, taenv, root)

      mapN(tparamsVal, tpeVal) {
        case (tparams, tpe) => KindedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type aliases.
    * The aliases must be sorted topologically.
    */
  private def visitTypeAliases(aliases: List[Symbol.TypeAliasSym], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], KindError] = {
    fold(aliases, Map.empty[Symbol.TypeAliasSym, KindedAst.TypeAlias]) {
      case (taenv, sym) =>
        val alias = root.typeAliases(sym)
        visitTypeAlias(alias, taenv, root) map {
          kind => taenv + (sym -> kind)
        }
    }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitCase(caze0: ResolvedAst.Declaration.Case, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Declaration.Case(sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          val quants = tparams.map(_.sym)
          val sc = Scheme(quants, Nil, Nil, Type.mkPureArrow(tpe, resTpe, sym.loc.asSynthetic))
          KindedAst.Case(sym, tpe, sc, loc)
      }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitRestrictableCase(caze0: ResolvedAst.Declaration.RestrictableCase, index: KindedAst.TypeParam, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.RestrictableCase, KindError] = caze0 match {
    case ResolvedAst.Declaration.RestrictableCase(sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          val quants = (index :: tparams).map(_.sym)
          val sc = Scheme(quants, Nil, Nil, Type.mkPureArrow(tpe, resTpe, sym.loc.asSynthetic))
          KindedAst.RestrictableCase(sym, tpe, sc, loc) // TODO RESTR-VARS the scheme is different for these. REVISIT
      }
  }

  /**
    * Performs kinding on the all the classes in the given root.
    */
  private def visitClasses(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, KindedAst.Class], KindError] = {
    val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)

    val results = ParOps.parMap(staleClasses.values)(visitClass(_, taenv, root))

    Validation.sequence(results) map {
      res =>
        res.foldLeft(freshClasses) {
          case (acc, defn) => acc + (defn.sym -> defn)
        }
    }
  }

  /**
    * Performs kinding on the given type class.
    */
  private def visitClass(clazz: ResolvedAst.Declaration.Class, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, KindError] = clazz match {
    case ResolvedAst.Declaration.Class(doc, ann, mod, sym, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParamDefaultStar(tparam0)

      val tparamsVal = visitTypeParam(tparam0, kenv)
      val superClassesVal = traverse(superClasses0)(visitTypeConstraint(_, kenv, taenv, root))
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, kenv))
      flatMapN(tparamsVal, superClassesVal, assocsVal) {
        case (tparam, superClasses, assocs) =>
          val sigsVal = traverse(sigs0) {
            case (sigSym, sig0) => visitSig(sig0, tparam, kenv, taenv, root).map(sig => sigSym -> sig)
          }
          val lawsVal = traverse(laws0)(visitDef(_, kenv, taenv, root))
          mapN(sigsVal, lawsVal) {
            case (sigs, laws) => KindedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs.toMap, laws, loc)
          }

      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Declaration.Instance, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Declaration.Instance(doc, ann, mod, clazz, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val kind = getClassKind(root.classes(clazz.sym))

      val kenvVal = inferType(tpe0, kind, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val tpeVal = visitType(tpe0, kind, kenv, taenv, root)
          val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, kenv, taenv, root))
          val assocsVal = traverse(assocs0)(visitAssocTypeDef(_, kenv, taenv, root))
          val defsVal = traverse(defs0)(visitDef(_, kenv, taenv, root))
          mapN(tpeVal, tconstrsVal, assocsVal, defsVal) {
            case (tpe, tconstrs, assocs, defs) => KindedAst.Instance(doc, ann, mod, clazz, tpe, tconstrs, assocs, defs, ns, loc)
          }
      }
  }

  /**
    * Performs kinding on the given effect declaration.
    */
  private def visitEffect(eff: ResolvedAst.Declaration.Effect, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Effect, KindError] = eff match {
    case ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      val opsVal = traverse(ops0)(visitOp(_, taenv, root))
      mapN(opsVal) {
        case ops => KindedAst.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs kinding on the all the definitions in the given root.
    */
  private def visitDefs(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.DefnSym, KindedAst.Def], KindError] = {
    val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)

    val results = ParOps.parMap(staleDefs.values)(visitDef(_, KindEnv.empty, taenv, root))

    Validation.sequence(results) map {
      res =>
        res.foldLeft(freshDefs) {
          case (acc, defn) => acc + (defn.sym -> defn)
        }
    }
  }

  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Declaration.Def, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Declaration.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val henv = None
          val specVal = visitSpec(spec0, Nil, kenv, taenv, root)
          val expVal = visitExp(exp0, kenv, taenv, henv, root)
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Def(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Declaration.Sig, classTparam: KindedAst.TypeParam, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Declaration.Sig(sym, spec0, exp0) =>
      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val henv = None
          val specVal = visitSpec(spec0, List(classTparam.sym), kenv, taenv, root)
          val expVal = traverseOpt(exp0)(visitExp(_, kenv, taenv, henv, root))
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given effect operation under the given kind environment.
    */
  private def visitOp(op: ResolvedAst.Declaration.Op, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Op, KindError] = op match {
    case ResolvedAst.Declaration.Op(sym, spec0) =>
      val kenvVal = inferSpec(spec0, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        case kenv =>
          val specVal = visitSpec(spec0, Nil, kenv, taenv, root)
          mapN(specVal) {
            case spec => KindedAst.Op(sym, spec)
          }
      }
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    *
    * Adds `quantifiers` to the generated scheme's quantifier list.
    */
  private def visitSpec(spec0: ResolvedAst.Spec, quantifiers: List[Symbol.KindedTypeVarSym], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, pur0, tconstrs0, econstrs0, loc) =>
      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, taenv, root))
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val purAndEffVal = visitPurityDefaultPure(pur0, kenv, taenv, root)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, kenv, taenv, root))
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint(_, kenv, taenv, root))

      mapN(tparamsVal, fparamsVal, tpeVal, purAndEffVal, tconstrsVal, econstrsVal) {
        case (tparams, fparams, tpe, pur, tconstrs, econstrs) =>
          val allQuantifiers = quantifiers ::: tparams.map(_.sym)
          val base = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), pur, tpe, tpe.loc)
          val sc = Scheme(allQuantifiers, tconstrs, econstrs.map(EqualityEnvironment.broaden), base)
          KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, pur, tconstrs, loc)
      }
  }

  /**
    * Performs kinding on the given associated type signature under the given kind environment.
    */
  private def visitAssocTypeSig(s0: ResolvedAst.Declaration.AssocTypeSig, kenv: KindEnv): Validation[KindedAst.AssocTypeSig, KindError] = s0 match {
    case ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam0, kind, loc) =>
      val tparamVal = visitTypeParam(tparam0, kenv)

      mapN(tparamVal) {
        case tparam => KindedAst.AssocTypeSig(doc, mod, sym, tparam, kind, loc)
      }
  }

  /**
    * Performs kinding on the given associated type definition under the given kind environment.
    */
  private def visitAssocTypeDef(d0: ResolvedAst.Declaration.AssocTypeDef, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.AssocTypeDef, KindError] = d0 match {
    case ResolvedAst.Declaration.AssocTypeDef(doc, mod, sym, arg0, tpe0, loc) =>
      val argVal = visitType(arg0, Kind.Wild, kenv, taenv, root) // TODO ASSOC-TYPES use expected from signature
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, taenv, root) // TODO ASSOC-TYPES use expected from signature

      mapN(argVal, tpeVal) {
        case (args, tpe) => KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExp(exp00: ResolvedAst.Expression, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv0: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {

    case ResolvedAst.Expression.Var(sym, loc) => KindedAst.Expression.Var(sym, loc).toSuccess

    case ResolvedAst.Expression.Def(sym, loc) => KindedAst.Expression.Def(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.Sig(sym, loc) => KindedAst.Expression.Sig(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.Hole(sym, loc) => KindedAst.Expression.Hole(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.HoleWithExp(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.HoleWithExp(exp, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.OpenAs(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          KindedAst.Expression.OpenAs(sym, exp, tvar, loc)
      }

    case ResolvedAst.Expression.Use(sym, alias, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Use(sym, alias, exp, loc)
      }

    case ResolvedAst.Expression.Cst(cst, loc) => KindedAst.Expression.Cst(cst, loc).toSuccess

    case ResolvedAst.Expression.Apply(exp0, exps0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expVal, expsVal) {
        case (exp, exps) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Apply(exp, exps, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.Lambda(fparam0, exp0, loc) =>
      val fparamVal = visitFormalParam(fparam0, kenv0, taenv, root)
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(fparamVal, expVal) {
        case (fparam, exp) => KindedAst.Expression.Lambda(fparam, exp, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.Unary(sop, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Unary(sop, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Binary(sop, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Binary(sop, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      val exp3Val = visitExp(exp30, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val, exp3Val) {
        case (exp1, exp2, exp3) => KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
      }

    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Stm(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.Discard(exp, loc) =>
      visitExp(exp, kenv0, taenv, henv0, root) map {
        case e => KindedAst.Expression.Discard(e, loc)
      }

    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.LetRec(sym, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.Region(tpe, loc) =>
      KindedAst.Expression.Region(tpe, loc).toSuccess

    case ResolvedAst.Expression.Scope(sym, regionVar, exp0, loc) =>
      val rv = Type.Var(regionVar.withKind(Kind.Eff), loc)
      val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      flatMapN(kenv0 + (regionVar -> Kind.Eff)) {
        case kenv =>
          val expVal = visitExp(exp0, kenv, taenv, henv0, root)
          mapN(expVal) {
            exp => KindedAst.Expression.Scope(sym, rv, exp, pvar, loc)
          }
      }

    case ResolvedAst.Expression.ScopeExit(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.ScopeExit(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitMatchRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.Match(exp, rules, loc)
      }

    case ResolvedAst.Expression.TypeMatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitMatchTypeRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TypeMatch(exp, rules, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.RelationalChoose(star, exps0, rules0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      val rulesVal = traverse(rules0)(visitRelationalChoiceRule(_, kenv0, taenv, henv0, root))
      mapN(expsVal, rulesVal) {
        case (exps, rules) => KindedAst.Expression.RelationalChoose(star, exps, rules, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RestrictableChoose(star, exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitRestrictableChoiceRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.RestrictableChoose(star, exp, rules, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Tag(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Tag(sym, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RestrictableTag(sym, exp0, isOpen, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.RestrictableTag(sym, exp, isOpen, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(elmsVal) {
        elms => KindedAst.Expression.Tuple(elms, loc)
      }

    case ResolvedAst.Expression.RecordEmpty(loc) => KindedAst.Expression.RecordEmpty(loc).toSuccess

    case ResolvedAst.Expression.RecordSelect(exp0, field, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.RecordSelect(exp, field, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, loc) =>
      val valueVal = visitExp(value0, kenv0, taenv, henv0, root)
      val restVal = visitExp(rest0, kenv0, taenv, henv0, root)
      mapN(valueVal, restVal) {
        case (value, rest) => KindedAst.Expression.RecordExtend(field, value, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RecordRestrict(field, rest0, loc) =>
      val restVal = visitExp(rest0, kenv0, taenv, henv0, root)
      mapN(restVal) {
        rest => KindedAst.Expression.RecordRestrict(field, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.ArrayLit(exps, exp, loc) =>
      val esVal = traverse(exps)(visitExp(_, kenv0, taenv, henv0, root))
      val eVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(esVal, eVal) {
        case (es, e) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.ArrayLit(es, e, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      val e3Val = visitExp(exp3, kenv0, taenv, henv0, root)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.ArrayNew(e1, e2, e3, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayLoad(base0, index0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, taenv, henv0, root)
      mapN(baseVal, indexVal) {
        case (base, index) =>
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.ArrayLoad(base, index, Type.freshVar(Kind.Star, loc.asSynthetic), pvar, loc)
      }

    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, taenv, henv0, root)
      val elmVal = visitExp(elm0, kenv0, taenv, henv0, root)
      mapN(baseVal, indexVal, elmVal) {
        case (base, index, elm) =>
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.ArrayStore(base, index, elm, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      mapN(baseVal) {
        base => KindedAst.Expression.ArrayLength(base, loc)
      }

    case ResolvedAst.Expression.VectorLit(exps, loc) =>
      val expsVal = traverse(exps)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expsVal) {
        case es =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.VectorLit(es, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.VectorLoad(exp1, exp2, loc) =>
      val exp1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) =>
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.VectorLoad(e1, e2, Type.freshVar(Kind.Star, loc.asSynthetic), pvar, loc)
      }

    case ResolvedAst.Expression.VectorLength(exp, loc) =>
      val expVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(expVal) {
        e => KindedAst.Expression.VectorLength(e, loc)
      }

    case ResolvedAst.Expression.Ref(exp1, exp2, loc) =>
      val e1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      mapN(e1Val, e2Val) {
        case (e1, e2) => KindedAst.Expression.Ref(e1, e2, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc), loc)
      }

    case ResolvedAst.Expression.Deref(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Deref(exp, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc), loc)
      }

    case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Assign(exp1, exp2, Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val expectedTypeVal = traverseOpt(expectedType0)(visitType(_, Kind.Star, kenv0, taenv, root))
      val expectedPurVal = traverseOpt(expectedEff0)(visitType(_, Kind.Eff, kenv0, taenv, root))
      mapN(expVal, expectedTypeVal, expectedPurVal) {
        case (exp, expectedType, expectedPur) =>
          KindedAst.Expression.Ascribe(exp, expectedType, expectedPur, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.InstanceOf(exp0, clazz, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.InstanceOf(exp, clazz, loc)
      }

    case ResolvedAst.Expression.CheckedCast(cast, exp, loc) =>
      mapN(visitExp(exp, kenv0, taenv, henv0, root)) {
        case e =>
          val tvar = Type.freshVar(Kind.Star, loc)
          val pvar = Type.freshVar(Kind.Eff, loc)
          KindedAst.Expression.CheckedCast(cast, e, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.UncheckedCast(exp0, declaredType0, declaredEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val declaredTypeVal = traverseOpt(declaredType0)(visitType(_, Kind.Star, kenv0, taenv, root))
      val declaredPurVal = traverseOpt(declaredEff0)(visitType(_, Kind.Eff, kenv0, taenv, root))
      mapN(expVal, declaredTypeVal, declaredPurVal) {
        case (exp, declaredType, declaredPur) =>
          KindedAst.Expression.UncheckedCast(exp, declaredType, declaredPur, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.UncheckedMaskingCast(exp, loc) =>
      val eVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(eVal) {
        case e => KindedAst.Expression.UncheckedMaskingCast(e, loc)
      }

    case ResolvedAst.Expression.Without(exp0, eff, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Without(exp, eff, loc)
      }

    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitCatchRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TryCatch(exp, rules, loc)
      }

    case ResolvedAst.Expression.TryWith(exp0, eff, rules0, loc) =>
      // create a fresh type variable for the handling block (same as resume result)
      // and for the operation result (same as resume argument)
      // and set the handled env
      val tvar = Type.freshVar(Kind.Star, loc)

      // use the old henv for the handled expression
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)

      // use the new henv for the handler
      val rulesVal = traverse(rules0)(visitHandlerRule(_, kenv0, taenv, tvar, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TryWith(exp, eff, rules, tvar, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.Do(op, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(argsVal) {
        case args => KindedAst.Expression.Do(op, args, loc)
      }

    case ResolvedAst.Expression.Resume(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      // Extract the type variable from the henv
      // Missing henv should have been caught previously
      val (argTvar, retTvar) = henv0.getOrElse(throw InternalCompilerException("Unexpected missing handler env.", loc))
      mapN(expVal) {
        case exp => KindedAst.Expression.Resume(exp, argTvar, retTvar, loc)
      }

    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expression.InvokeConstructor(constructor, args, loc)
      }

    case ResolvedAst.Expression.InvokeMethod(method, clazz, exp0, args0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expVal, argsVal) {
        case (exp, args) => KindedAst.Expression.InvokeMethod(method, clazz, exp, args, loc)
      }

    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expression.InvokeStaticMethod(method, args, loc)
      }

    case ResolvedAst.Expression.GetField(field, clazz, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.GetField(field, clazz, exp, loc)
      }

    case ResolvedAst.Expression.PutField(field, clazz, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.PutField(field, clazz, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess

    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.PutStaticField(field, exp, loc)
      }

    case ResolvedAst.Expression.NewObject(name, clazz, methods, loc) =>
      mapN(traverse(methods)(visitJvmMethod(_, kenv0, taenv, henv0, root))) {
        methods => KindedAst.Expression.NewObject(name, clazz, methods, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.NewChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.NewChannel(exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.GetChannel(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.GetChannel(exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.PutChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.PutChannel(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.SelectChannel(rules0, default0, loc) =>
      val rulesVal = traverse(rules0)(visitSelectChannelRule(_, kenv0, taenv, henv0, root))
      val defaultVal = traverseOpt(default0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(rulesVal, defaultVal) {
        case (rules, default) => KindedAst.Expression.SelectChannel(rules, default, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Spawn(exp1, exp2, loc) =>
      val e1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      mapN(e1Val, e2Val) {
        case (e1, e2) => KindedAst.Expression.Spawn(e1, e2, loc)
      }

    case ResolvedAst.Expression.ParYield(frags, exp0, loc) =>
      val fragsVal = traverse(frags) {
        case ResolvedAst.ParYieldFragment(pat, exp1, l0) =>
          val patVal = visitPattern(pat, kenv0, root)
          val expVal = visitExp(exp1, kenv0, taenv, henv0, root)
          mapN(patVal, expVal) {
            case (p, e) => KindedAst.ParYieldFragment(p, e, l0)
          }
      }

      mapN(fragsVal, visitExp(exp0, kenv0, taenv, henv0, root)) {
        case (fs, exp) => KindedAst.Expression.ParYield(fs, exp, loc)
      }

    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Lazy(exp, loc)
      }

    case ResolvedAst.Expression.Force(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Force(exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointConstraintSet(cs0, loc) =>
      val csVal = traverse(cs0)(visitConstraint(_, kenv0, taenv, henv0, root))
      mapN(csVal) {
        cs => KindedAst.Expression.FixpointConstraintSet(cs, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(visitPredicateParam(_, kenv0, taenv, root))
      val expVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(psVal, expVal) {
        case (ps, e) => KindedAst.Expression.FixpointLambda(ps, e, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expression.Error(err, tvar, pvar)
      }

    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointSolve(exp, loc)
      }

    case ResolvedAst.Expression.FixpointFilter(pred, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointFilter(pred, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointInject(exp0, pred, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointInject(exp, pred, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointProject(pred, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.FixpointProject(pred, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Error(m) =>
      val tvar = Type.freshVar(Kind.Star, m.loc)
      val pvar = Type.freshVar(Kind.Eff, m.loc)
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.SoftFailure(KindedAst.Expression.Error(m, tvar, pvar), LazyList.empty)
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      val patVal = visitPattern(pat0, kenv, root)
      val guardVal = traverseOpt(guard0)(visitExp(_, kenv, taenv, henv, root))
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(patVal, guardVal, expVal) {
        case (pat, guard, exp) => KindedAst.MatchRule(pat, guard, exp)
      }
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchTypeRule(rule0: ResolvedAst.TypeMatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.TypeMatchRule, KindError] = rule0 match {
    case ResolvedAst.TypeMatchRule(sym, tpe0, exp0) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(tpeVal, expVal) {
        case (tpe, exp) => KindedAst.TypeMatchRule(sym, tpe, exp)
      }
  }

  /**
    * Performs kinding on the given relational choice rule under the given kind environment.
    */
  private def visitRelationalChoiceRule(rule0: ResolvedAst.RelationalChoiceRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.RelationalChoiceRule, KindError] = rule0 match {
    case ResolvedAst.RelationalChoiceRule(pat0, exp0) =>
      val patVal = traverse(pat0)(visitRelationalChoicePattern)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(patVal, expVal) {
        case (pat, exp) => KindedAst.RelationalChoiceRule(pat, exp)
      }
  }

  /**
    * Performs kinding on the given relational choice rule under the given kind environment.
    */
  private def visitRestrictableChoiceRule(rule0: ResolvedAst.RestrictableChoiceRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.RestrictableChoiceRule, KindError] = rule0 match {
    case ResolvedAst.RestrictableChoiceRule(pat0, exp0) =>
      val patVal = visitRestrictableChoicePattern(pat0)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(patVal, expVal) {
        case (pat, exp) => KindedAst.RestrictableChoiceRule(pat, exp)
      }
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.CatchRule(sym, clazz, exp)
      }
  }

  /**
    * Performs kinding on the given handler rule under the given kind environment.
    */
  private def visitHandlerRule(rule0: ResolvedAst.HandlerRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], hTvar: Type.Var, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.HandlerRule, KindError] = rule0 match {
    case ResolvedAst.HandlerRule(op, fparams0, exp0) =>
      // create a new type variable for the op return type (same as resume argument type)
      val tvar = Type.freshVar(Kind.Star, exp0.loc)

      val henv = Some((tvar, hTvar))

      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, taenv, root))
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(fparamsVal, expVal) {
        case (fparams, exp) => KindedAst.HandlerRule(op, fparams, exp, tvar)
      }
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      val chanVal = visitExp(chan0, kenv, taenv, henv, root)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(chanVal, expVal) {
        case (chan, exp) => KindedAst.SelectChannelRule(sym, chan, exp)
      }
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def visitPattern(pat00: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Pattern, KindError] = pat00 match {
    case ResolvedAst.Pattern.Wild(loc) => KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
    case ResolvedAst.Pattern.Var(sym, loc) => KindedAst.Pattern.Var(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
    case ResolvedAst.Pattern.Cst(cst, loc) => KindedAst.Pattern.Cst(cst, loc).toSuccess
    case ResolvedAst.Pattern.Tag(sym, pat0, loc) =>
      val patVal = visitPattern(pat0, kenv, root)
      mapN(patVal) {
        pat => KindedAst.Pattern.Tag(sym, pat, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitPattern(_, kenv, root))
      mapN(elmsVal) {
        elms => KindedAst.Pattern.Tuple(elms, loc)
      }
  }

  /**
    * Performs kinding on the given relational choice pattern under the given kind environment.
    */
  private def visitRelationalChoicePattern(pat0: ResolvedAst.RelationalChoicePattern)(implicit flix: Flix): Validation[KindedAst.RelationalChoicePattern, KindError] = pat0 match {
    case ResolvedAst.RelationalChoicePattern.Wild(loc) => KindedAst.RelationalChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.RelationalChoicePattern.Absent(loc) => KindedAst.RelationalChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.RelationalChoicePattern.Present(sym, loc) => KindedAst.RelationalChoicePattern.Present(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoicePattern(pat00: ResolvedAst.RestrictableChoicePattern)(implicit flix: Flix): Validation[KindedAst.RestrictableChoicePattern, KindError] = pat00 match {
    case ResolvedAst.RestrictableChoicePattern.Tag(sym, pat0, loc) =>
      val patVal = traverse(pat0)(visitRestrictableChoicePatternVarOrWild)
      mapN(patVal) {
        case pat => KindedAst.RestrictableChoicePattern.Tag(sym, pat, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoicePatternVarOrWild(pat0: ResolvedAst.RestrictableChoicePattern.VarOrWild)(implicit flix: Flix): Validation[KindedAst.RestrictableChoicePattern.VarOrWild, KindError] = pat0 match {
    case ResolvedAst.RestrictableChoicePattern.Wild(loc) => KindedAst.RestrictableChoicePattern.Wild(Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
    case ResolvedAst.RestrictableChoicePattern.Var(sym, loc) => KindedAst.RestrictableChoicePattern.Var(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparams = cparams0.map(visitConstraintParam)
      val headVal = visitHeadPredicate(head0, kenv, taenv, henv, root)
      val bodyVal = traverse(body0)(visitBodyPredicate(_, kenv, taenv, henv, root))
      mapN(headVal, bodyVal) {
        case (head, body) => KindedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Performs kinding on the given constraint param under the given kind environment.
    */
  private def visitConstraintParam(cparam0: ResolvedAst.ConstraintParam)(implicit flix: Flix): KindedAst.ConstraintParam = cparam0 match {
    case ResolvedAst.ConstraintParam(sym, loc) => KindedAst.ConstraintParam(sym, loc)
  }

  /**
    * Performs kinding on the given head predicate under the given kind environment.
    */
  private def visitHeadPredicate(pred0: ResolvedAst.Predicate.Head, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred0 match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      val termsVal = traverse(terms0)(visitExp(_, kenv, taenv, henv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Head.Atom(pred, den, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred0: ResolvedAst.Predicate.Body, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms0, loc) =>
      val termsVal = traverse(terms0)(visitPattern(_, kenv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }

    case ResolvedAst.Predicate.Body.Functional(outVars, exp0, loc) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Functional(outVars, exp, loc)
      }

    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Guard(exp, loc)
      }
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVar(tvar: UnkindedType.Var, expectedKind: Kind, kenv: KindEnv): Validation[Type.Var, KindError] = tvar match {
    case UnkindedType.Var(sym0, loc) =>
      mapN(visitTypeVarSym(sym0, expectedKind, kenv, loc)) {
        sym => Type.Var(sym, loc)
      }
  }

  /**
    * Performs kinding on the given type variable symbol under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVarSym(sym: Symbol.UnkindedTypeVarSym, expectedKind: Kind, kenv: KindEnv, loc: SourceLocation): Validation[Symbol.KindedTypeVarSym, KindError] = {
    kenv.map.get(sym) match {
      // Case 1: we don't know about this kind, just ascribe it with what the context expects
      case None => sym.withKind(expectedKind).toSuccess
      // Case 2: we know about this kind, make sure it's behaving as we expect
      case Some(actualKind) =>
        unify(expectedKind, actualKind) match {
          case Some(kind) => sym.withKind(kind).toSuccess
          case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc = loc).toFailure
        }
    }
  }


  /**
    * Performs kinding on the given type under the given kind environment, with `expectedKind` expected from context.
    * This is roughly analogous to the reassembly of expressions under a type environment, except that:
    * - Kind errors may be discovered here as they may not have been found during inference (or inference may not have happened at all).
    */
  private def visitType(tpe0: UnkindedType, expectedKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => visitTypeVar(tvar, expectedKind, kenv)

    // TODO EFF-MIGRATION temporary hack to maintain behavior of IO
    case UnkindedType.Cst(TypeConstructor.Effect(sym), loc) if (sym == IoSym || sym == NonDetSym) =>
      unify(expectedKind, Kind.Eff) match {
        case Some(_) => Type.Cst(TypeConstructor.EffUniv, loc).toSuccess
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = Kind.Eff, loc = loc).toFailure
      }

    case UnkindedType.Cst(cst, loc) =>
      val kind = cst.kind
      unify(expectedKind, kind) match {
        case Some(_) => Type.Cst(cst, loc).toSuccess
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case UnkindedType.Apply(t10, t20, loc) =>
      val t2Val = visitType(t20, Kind.Wild, kenv, taenv, root)
      flatMapN(t2Val) {
        t2 =>
          val k1 = Kind.Arrow(t2.kind, expectedKind)
          val t1Val = visitType(t10, k1, kenv, taenv, root)
          mapN(t1Val) {
            t1 => mkApply(t1, t2, loc)
          }
      }

    case UnkindedType.Ascribe(t, k, loc) =>
      unify(k, expectedKind) match {
        case Some(kind) => visitType(t, kind, kenv, taenv, root)
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k, loc).toFailure
      }

    case UnkindedType.Alias(cst, args0, t0, loc) =>
      taenv(cst.sym) match {
        case KindedAst.TypeAlias(_, _, _, tparams, tpe, _) =>
          val argsVal = traverse(tparams.zip(args0)) {
            case (tparam, arg) => visitType(arg, tparam.sym.kind, kenv, taenv, root)
          }
          val tpeVal = visitType(t0, tpe.kind, kenv, taenv, root)
          flatMapN(argsVal, tpeVal) {
            case (args, t) => unify(t.kind, expectedKind) match {
              case Some(_) => Type.Alias(cst, args, t, loc).toSuccess
              case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc).toFailure
            }
          }
      }

    case UnkindedType.AssocType(cst, arg0, loc) =>
      val clazz = root.classes(cst.sym.clazz)
      // TODO ASSOC-TYPES maybe have dedicated field in root for assoc types
      clazz.assocs.find(_.sym == cst.sym).get match {
        case ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, k0, loc) =>
          // TODO ASSOC-TYPES for now assuming just one type parameter
          // check that the assoc type kind matches the expected
          unify(k0, expectedKind) match {
            case Some(kind) =>
              val innerExpectedKind = getClassKind(clazz)
              val argVal = visitType(arg0, innerExpectedKind, kenv, taenv, root)
              mapN(argVal) {
                case arg => Type.AssocType(cst, arg, kind, loc)
              }
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k0, loc).toFailure
          }
      }

    case UnkindedType.Arrow(pur0, arity, loc) =>
      val kind = Kind.mkArrow(arity)
      unify(kind, expectedKind) match {
        case Some(_) =>
          val purVal = visitPurityDefaultPure(pur0, kenv, taenv, root)
          mapN(purVal) {
            case pur => Type.mkApply(Type.Cst(TypeConstructor.Arrow(arity), loc), List(pur), loc)
          }
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case UnkindedType.Enum(sym, loc) =>
      val kind = getEnumKind(root.enums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.Enum(sym, k), loc).toSuccess
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case UnkindedType.RestrictableEnum(sym, loc) =>
      val kind = getRestrictableEnumKind(root.restrictableEnums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.RestrictableEnum(sym, k), loc).toSuccess
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case UnkindedType.CaseSet(cases, loc) =>
      // Infer the kind from the cases.
      val actualKindVal: Validation[Kind, KindError] = fold(cases, Kind.WildCaseSet: Kind) {
        case (kindAcc, sym) =>
          val symKind = Kind.CaseSet(sym.enumSym)
          unify(kindAcc, symKind) match {
            // Case 1: The kinds unify. Update the kind.
            case Some(k) => k.toSuccess
            // Case 2: The kinds do not unify. Error.
            case None => KindError.MismatchedKinds(kindAcc, symKind, loc).toFailure
          }
      }

      // Check against the expected kind.
      flatMapN(actualKindVal) {
        case actualKind =>
          unify(actualKind, expectedKind) match {
            // Case 1:  We have an explicit case kind.
            case Some(Kind.CaseSet(sym)) => Type.Cst(TypeConstructor.CaseSet(cases.to(SortedSet), sym), loc).toSuccess
            // Case 2: We have a generic case kind. Error.
            case Some(Kind.WildCaseSet) => KindError.UninferrableKind(loc).toFailure
            // Case 3: Unexpected kind. Error.
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc).toFailure

            case Some(_) => throw InternalCompilerException("unexpected non-case set kind", loc)
          }
      }


    case UnkindedType.CaseComplement(t0, loc) =>
      val tVal = visitType(t0, Kind.WildCaseSet, kenv, taenv, root)
      flatMapN(tVal) {
        t =>
          unify(t.kind, expectedKind) match {
            case Some(Kind.CaseSet(enumSym)) => Type.mkCaseComplement(t, enumSym, loc).toSuccess
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc).toFailure
            case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
          }
      }

    case UnkindedType.CaseUnion(t1, t2, loc) =>
      // Get the component types.
      val t1Val = visitType(t1, Kind.WildCaseSet, kenv, taenv, root)
      val t2Val = visitType(t2, Kind.WildCaseSet, kenv, taenv, root)

      flatMapN(t1Val, t2Val) {
        case (t1, t2) =>
          val actualKindVal: Validation[Kind, KindError] = unify(t1.kind, t2.kind) match {
            // Case 1: The kinds unify.
            case Some(k) => k.toSuccess
            // Case 2: The kinds do not unify. Error.
            case None => KindError.MismatchedKinds(t1.kind, t2.kind, loc).toFailure
          }

          flatMapN(actualKindVal) {
            case actualKind =>
              unify(actualKind, expectedKind) match {
                case Some(Kind.CaseSet(enumSym)) => Type.mkCaseUnion(t1, t2, enumSym, loc).toSuccess
                case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc).toFailure
                case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
              }
          }
      }

    case UnkindedType.CaseIntersection(t1, t2, loc) =>
      // Get the component types.
      val t1Val = visitType(t1, Kind.WildCaseSet, kenv, taenv, root)
      val t2Val = visitType(t2, Kind.WildCaseSet, kenv, taenv, root)

      flatMapN(t1Val, t2Val) {
        case (t1, t2) =>
          val actualKindVal: Validation[Kind, KindError] = unify(t1.kind, t2.kind) match {
            // Case 1: The kinds unify.
            case Some(k) => k.toSuccess
            // Case 2: The kinds do not unify. Error.
            case None => KindError.MismatchedKinds(t1.kind, t2.kind, loc).toFailure
          }

          flatMapN(actualKindVal) {
            case actualKind =>
              unify(actualKind, expectedKind) match {
                case Some(Kind.CaseSet(enumSym)) => Type.mkCaseIntersection(t1, t2, enumSym, loc).toSuccess
                case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc).toFailure
                case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
              }
          }
      }


    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe0.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe0.loc)


  }

  /**
    * Performs kinding on the given purity, assuming it to be Pure if it is absent.
    */
  private def visitPurityDefaultPure(tpe: Option[UnkindedType], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe match {
    case None => Type.mkPure(SourceLocation.Unknown).toSuccess
    case Some(t) => visitType(t, Kind.Eff, kenv, taenv, root)
  }

  /**
    * Performs kinding on the given type constraint under the given kind environment.
    */
  private def visitTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(head, tpe0, loc) =>
      val classKind = getClassKind(root.classes(head.sym))
      mapN(visitType(tpe0, classKind, kenv, taenv, root)) {
        tpe => Ast.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given equality constraint under the given kind environment.
    */
  private def visitEqualityConstraint(econstr: ResolvedAst.EqualityConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.EqualityConstraint, KindError] = econstr match {
    case ResolvedAst.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      // TODO ASSOC-TYPES check that these are the same kind
      val t1Val = visitType(tpe1, Kind.Wild, kenv, taenv, root)
      val t2Val = visitType(tpe2, Kind.Wild, kenv, taenv, root)
      mapN(t1Val, t2Val) {
        case (t1, t2) => Ast.EqualityConstraint(cst, t1, t2, loc)
      }
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv): Validation[KindedAst.TypeParam, KindError] = {
    val (name, sym0, loc) = tparam match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
    }
    val symVal = visitTypeVarSym(sym0, Kind.Wild, kenv, loc)
    mapN(symVal) {
      case sym => KindedAst.TypeParam(name, sym, loc)
    }
  }

  /**
    * Performs kinding on the given index parameter of the given enum sym under the given kind environment.
    */
  private def visitIndex(index: ResolvedAst.TypeParam, `enum`: Symbol.RestrictableEnumSym, kenv: KindEnv): Validation[KindedAst.TypeParam, KindError] = {
    val (name, sym0, loc) = index match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
    }

    val symVal = visitTypeVarSym(sym0, Kind.CaseSet(`enum`), kenv, loc)
    mapN(symVal) {
      case sym => KindedAst.TypeParam(name, sym, loc)
    }
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      val (tpeVal, src) = tpe0 match {
        case None => (sym.tvar.toSuccess, Ast.TypeSource.Inferred)
        case Some(tpe) => (visitType(tpe, Kind.Star, kenv, taenv, root), Ast.TypeSource.Ascribed)
      }
      mapN(tpeVal) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, src, loc)
      }
  }

  /**
    * Performs kinding on the given predicate param under the given kind environment.
    */
  private def visitPredicateParam(pparam0: ResolvedAst.PredicateParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.PredicateParam, KindError] = pparam0 match {
    case ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      val tpe = Type.freshVar(Kind.Predicate, loc)
      KindedAst.PredicateParam(pred, tpe, loc).toSuccess

    case ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, Kind.Star, kenv, taenv, root))) {
        case ts =>
          val tpe = den match {
            case Denotation.Relational => Type.mkRelation(ts, pred.loc.asSynthetic)
            case Denotation.Latticenal => Type.mkLattice(ts, pred.loc.asSynthetic)
          }
          KindedAst.PredicateParam(pred, tpe, loc)
      }

  }

  /**
    * Performs kinding on the given JVM method.
    */
  private def visitJvmMethod(method: ResolvedAst.JvmMethod, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix) = method match {
    case ResolvedAst.JvmMethod(_, fparams, exp, tpe0, pur0, loc) =>
      val fparamsVal = traverse(fparams)(visitFormalParam(_, kenv, taenv, root))
      val expVal = visitExp(exp, kenv, taenv, henv, root)
      val purVal = visitPurityDefaultPure(pur0, kenv, taenv, root)
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, taenv, root)
      mapN(fparamsVal, expVal, tpeVal, purVal) {
        case (f, e, tpe, pur) => KindedAst.JvmMethod(method.ident, f, e, tpe, pur, loc)
      }
  }

  /**
    * Infers a kind environment from the given spec.
    * A KindEnvironment is provided in case some subset of of kinds have been declared (and therefore should not be inferred),
    * as in the case of a class type parameter used in a sig or law.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, pur0, tconstrs, _, _) => // TODO ASSOC-TYPES use econstrs for inference?
      val fparamKenvsVal = traverse(fparams)(inferFormalParam(_, kenv, taenv, root))
      val tpeKenvVal = inferType(tpe, Kind.Star, kenv, taenv, root)
      val effKenvsVal = traverse(pur0)(inferType(_, Kind.Eff, kenv, taenv, root))
      val tconstrsKenvsVal = traverse(tconstrs)(inferTypeConstraint(_, kenv, taenv, root))

      flatMapN(fparamKenvsVal, tpeKenvVal, effKenvsVal, tconstrsKenvsVal) {
        case (fparamKenvs, tpeKenv, effKenvs, tconstrKenvs) =>
          KindEnv.merge(fparamKenvs ::: tpeKenv :: effKenvs ::: tconstrKenvs)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => tpe0 match {
      case None => KindEnv.empty.toSuccess
      case Some(tpe) => inferType(tpe, Kind.Star, kenv, taenv, root)
    }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(head, tpe, _) =>
      val kind = getClassKind(root.classes(head.sym))
      inferType(tpe, kind, kenv: KindEnv, taenv, root)
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    * The inference is roughly analogous to the inference of types for expressions.
    * The primary differences are:
    * - There are no kind variables; kinds that cannot be determined are instead marked with [[Kind.Wild]].
    * - Subkinding may allow a variable to be ascribed with two different kinds; the most specific is used in the returned environment.
    */
  private def inferType(tpe: UnkindedType, expectedKind: Kind, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: UnkindedType.Var =>
      val tyconKind = kenv0.map.get(tvar.sym) match {
        // Case 1.1: the type is not in the kenv: guess that it is Star -> Star -> ... -> ???.
        case None =>
          tpe.typeArguments.foldLeft(expectedKind) {
            case (acc, _) => Kind.Star ->: acc
          }
        // Case 1.2: the type is in the kenv: use it.
        case Some(k) => k
      }

      val args = Kind.kindArgs(tyconKind)
      fold(tpe.typeArguments.zip(args), KindEnv.singleton(tvar.sym -> tyconKind)) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Cst(cst, loc) =>
      val args = Kind.kindArgs(cst.kind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Ascribe(t, k, _) => inferType(t, k, kenv0, taenv, root)

    case UnkindedType.Alias(cst, args, _, _) =>
      val alias = taenv(cst.sym)
      val tparamKinds = alias.tparams.map(_.sym.kind)

      fold(args.zip(tparamKinds), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.AssocType(cst, arg, _) =>
      val clazz = root.classes(cst.sym.clazz)
      val kind = getClassKind(clazz)
      inferType(arg, kind, kenv0, taenv, root)

    case UnkindedType.Arrow(pur, _, _) =>
      val purKenvsVal = traverse(pur)(inferType(_, Kind.Eff, kenv0, taenv, root))
      val argKenvVal = fold(tpe.typeArguments, KindEnv.empty) {
        case (acc, targ) => flatMapN(inferType(targ, Kind.Star, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }
      flatMapN(purKenvsVal, argKenvVal) {
        case (purKenvs, argKenv) => KindEnv.merge(purKenvs :+ argKenv)
      }

    case UnkindedType.Enum(sym, _) =>
      val tyconKind = getEnumKind(root.enums(sym))
      val args = Kind.kindArgs(tyconKind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.RestrictableEnum(sym, _) =>
      val tyconKind = getRestrictableEnumKind(root.restrictableEnums(sym))
      val args = Kind.kindArgs(tyconKind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.CaseSet(_, _) => KindEnv.empty.toSuccess

    case UnkindedType.CaseComplement(t, _) =>
      // Expected kind for t is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      inferType(t, expected, kenv0, taenv, root)

    case UnkindedType.CaseUnion(t1, t2, _) =>
      // Expected kind for t1 and t2 is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      val kenv1Val = inferType(t1, expected, kenv0, taenv, root)
      val kenv2Val = inferType(t2, expected, kenv0, taenv, root)
      flatMapN(kenv1Val, kenv2Val) {
        case (kenv1, kenv2) => kenv1 ++ kenv2
      }

    case UnkindedType.CaseIntersection(t1, t2, _) =>
      // Expected kind for t1 and t2 is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      val kenv1Val = inferType(t1, expected, kenv0, taenv, root)
      val kenv2Val = inferType(t2, expected, kenv0, taenv, root)
      flatMapN(kenv1Val, kenv2Val) {
        case (kenv1, kenv2) => kenv1 ++ kenv2
      }


    case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", tpe.loc)
    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe.loc)
  }

  /**
    * Gets a kind environment from the type params, defaulting to Star kind if they are unkinded.
    */
  private def getKindEnvFromTypeParamsDefaultStar(tparams0: ResolvedAst.TypeParams)(implicit flix: Flix): KindEnv = tparams0 match {
    case tparams: ResolvedAst.TypeParams.Kinded =>
      getKindEnvFromKindedTypeParams(tparams)
    case tparams: ResolvedAst.TypeParams.Unkinded =>
      getStarKindEnvForTypeParams(tparams)
  }

  /**
    * Gets a kind environment from the type param, defaulting to Star kind if it is unkinded.
    */
  private def getKindEnvFromTypeParamDefaultStar(tparam0: ResolvedAst.TypeParam)(implicit flix: Flix): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.Star)
  }

  /**
    * Gets a kind environment from the type param, defaulting the to kind of the given enum's tags if it is unkinded.
    */
  private def getKindEnvFromIndex(index0: ResolvedAst.TypeParam, sym: Symbol.RestrictableEnumSym)(implicit flix: Flix): KindEnv = index0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.CaseSet(sym))
  }

  /**
    * Gets a kind environment from the spec.
    */
  private def getKindEnvFromSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _, _) =>
      tparams0 match {
        case tparams: ResolvedAst.TypeParams.Kinded => getKindEnvFromKindedTypeParams(tparams) ++ kenv
        case _: ResolvedAst.TypeParams.Unkinded => inferSpec(spec0, kenv, taenv, root)
      }
  }

  /**
    * Gets a kind environment from the kinded type params.
    */
  private def getKindEnvFromKindedTypeParams(tparams0: ResolvedAst.TypeParams.Kinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[Symbol.UnkindedTypeVarSym, Kind]) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) =>
          acc + (tpe -> kind)
      }
      KindEnv(map)
  }

  /**
    * Gets a kind environment from the unkinded type params, defaulting each to Star kind.
    */
  private def getStarKindEnvForTypeParams(tparams0: ResolvedAst.TypeParams.Unkinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[Symbol.UnkindedTypeVarSym, Kind]) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc + (tpe -> Kind.Star)
      }
      KindEnv(map)
  }

  /**
    * Gets the kind of the enum.
    */
  private def getEnumKind(enum0: ResolvedAst.Declaration.Enum)(implicit flix: Flix): Kind = enum0 match {
    case ResolvedAst.Declaration.Enum(_, _, _, _, tparams, _, _, _) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the restrictable enum.
    */
  private def getRestrictableEnumKind(enum0: ResolvedAst.Declaration.RestrictableEnum)(implicit flix: Flix): Kind = enum0 match {
    case ResolvedAst.Declaration.RestrictableEnum(_, _, _, sym, index, tparams, _, _, _) =>
      val kenvIndex = getKindEnvFromIndex(index, sym)
      val kenvTparams = getKindEnvFromTypeParamsDefaultStar(tparams)

      val kenv = KindEnv.disjointAppend(kenvIndex, kenvTparams)

      (index :: tparams.tparams).foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the class.
    */
  private def getClassKind(clazz: ResolvedAst.Declaration.Class): Kind = clazz.tparam match {
    case ResolvedAst.TypeParam.Kinded(_, _, kind, _) => kind
    case _: ResolvedAst.TypeParam.Unkinded => Kind.Star
  }

  /**
    * Creates the type application `t1[t2]`, while simplifying trivial boolean formulas.
    */
  private def mkApply(t1: Type, t2: Type, loc: SourceLocation): Type = t1 match {
    case Type.Apply(Type.Cst(TypeConstructor.Union, _), arg, _) => Type.mkUnion(arg, t2, loc)
    case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), arg, _) => Type.mkIntersection(arg, t2, loc)
    case Type.Cst(TypeConstructor.Complement, _) => Type.mkComplement(t2, loc)

    case t => Type.Apply(t, t2, loc)
  }

  /**
    * Adds a prime symbol to the vartext.
    */
  private def prime(text: Ast.VarText): Ast.VarText = text match {
    case Ast.VarText.Absent => Ast.VarText.Absent
    case Ast.VarText.SourceText(s) => Ast.VarText.SourceText(s + "'")
  }

  /**
    * A mapping from type variables to kinds.
    */
  private object KindEnv {
    /**
      * The empty kind environment.
      */
    val empty: KindEnv = KindEnv(Map.empty)

    /**
      * Returns a kind environment consisting of a single mapping.
      */
    def singleton(pair: (Symbol.UnkindedTypeVarSym, Kind)): KindEnv = KindEnv(Map(pair))

    /**
      * Merges all the given kind environments.
      */
    def merge(kenvs: List[KindEnv]): Validation[KindEnv, KindError] = {
      fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
    }

    /**
      * Merges the given kind environment into this kind environment.
      *
      * The environments must be disjoint.
      */
    def disjointAppend(kenv1: KindEnv, kenv2: KindEnv): KindEnv = {
      KindEnv(kenv1.map ++ kenv2.map)
    }
  }

  private case class KindEnv(map: Map[Symbol.UnkindedTypeVarSym, Kind]) {
    /**
      * Adds the given mapping to the kind environment.
      */
    def +(pair: (Symbol.UnkindedTypeVarSym, Kind)): Validation[KindEnv, KindError] = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) => unify(kind0, kind) match {
          case Some(minKind) => KindEnv(map + (tvar -> minKind)).toSuccess
          case None => KindError.MismatchedKinds(kind0, kind, tvar.loc).toFailure
        }
        case None => KindEnv(map + (tvar -> kind)).toSuccess
      }
    }

    /**
      * Merges the given kind environment into this kind environment.
      */
    def ++(other: KindEnv): Validation[KindEnv, KindError] = {
      fold(other.map, this) {
        case (acc, pair) => acc + pair
      }
    }
  }
}
