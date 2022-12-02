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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unify
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse, traverseOpt}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

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
  *       - If the type variable is the purity/effect type of the function, it is ascribed kind Beef.
  *       - If the type variable is an argument to a type constraint, it is ascribed the class's parameter kind
  *       - If the type variable is an argument to a type constructor, it is ascribed the type constructor's parameter kind.
  *       - If the type variable is used as an type constructor, it is ascribed the kind Star -> Star ... -> Star -> X,
  *         where X is the kind inferred from enacting these rules in the place of the fully-applied type.
  *       - If there is an inconsistency among these kinds, an error is raised.
  *
  * In inferring types, variable type constructors are assumed to have kind * -> * -> * -> ???.
  *
  * Purity and Effect Handling:
  * After kind inference, any type variables inferred to have kind Beef are split into two variables:
  * a Bool-kinded purity and an Effect-kinded effect. Because variables of this kind can only be found
  * where both a purity and effect are expected, we can distribute the purity and effect variables
  * appropriately into these AST fields, joining them with the appropriate operator (conjunction or union).
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
          pair: (Symbol.EnumSym, ResolvedAst.Enum) =>
            val (sym, enum) = pair
            visitEnum(enum, taenv, root).map(sym -> _)
        }))

        val classesVal = visitClasses(root, taenv, oldRoot, changeSet)

        val defsVal = visitDefs(root, taenv, oldRoot, changeSet)

        val instancesVal = Validation.sequence(ParOps.parMap(root.instances)({
          pair: (Symbol.ClassSym, List[ResolvedAst.Instance]) =>
            val (sym, insts) = pair
            traverse(insts)(visitInstance(_, taenv, root)).map(sym -> _)
        }))

        val effectsVal = Validation.sequence(ParOps.parMap(root.effects)({
          pair: (Symbol.EffectSym, ResolvedAst.Effect) =>
            val (sym, eff) = pair
            visitEffect(eff, taenv, root).map(sym -> _)
        }))

        mapN(enumsVal, classesVal, defsVal, instancesVal, effectsVal) {
          case (enums, classes, defs, instances, effects) =>
            KindedAst.Root(classes, instances.toMap, defs, enums.toMap, effects.toMap, taenv, root.uses, root.entryPoint, root.sources, root.names)
        }
    }

  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum0: ResolvedAst.Enum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, KindError] = enum0 match {
    case ResolvedAst.Enum(doc, ann0, mod, sym, tparams0, derives, cases0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv, Map.empty)).map(_.flatten)
      val annVal = traverse(ann0)(visitAnnotation(_, kenv, Map.empty, taenv, None, root))
      val tpeVal = visitType(tpe0, Kind.Star, kenv, Map.empty, taenv, root)

      flatMapN(annVal, tparamsVal, tpeVal) {
        case (ann, tparams, tpe) =>
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
    * Performs kinding on the given type alias.
    * Returns the kind of the type alias.
    */
  private def visitTypeAlias(alias: ResolvedAst.TypeAlias, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.TypeAlias, KindError] = alias match {
    case ResolvedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv, Map.empty)).map(_.flatten)
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, Map.empty, taenv, root)

      mapN(tparamsVal, tpeVal) {
        case (tparams, tpe) => KindedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type aliases.
    * The aliases must be sorted topologically.
    */
  private def visitTypeAliases(aliases: List[Symbol.TypeAliasSym], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], KindError] = {
    Validation.fold(aliases, Map.empty[Symbol.TypeAliasSym, KindedAst.TypeAlias]) {
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
  private def visitCase(caze0: ResolvedAst.Case, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Case(sym, tpe0) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, Map.empty, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          val quants = tparams.map(_.sym)
          val sc = Scheme(quants, Nil, Type.mkPureArrow(tpe, resTpe, sym.loc.asSynthetic))
          KindedAst.Case(sym, tpe, sc)
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
  private def visitClass(clazz: ResolvedAst.Class, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, KindError] = clazz match {
    case ResolvedAst.Class(doc, ann0, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParamDefaultStar(tparam0)

      val annVal = traverse(ann0)(visitAnnotation(_, kenv, Map.empty, taenv, None, root))
      val tparamsVal = visitTypeParam(tparam0, kenv, Map.empty)
      val superClassesVal = traverse(superClasses0)(visitTypeConstraint(_, kenv, Map.empty, taenv, root))
      flatMapN(annVal, tparamsVal, superClassesVal) {
        case (ann, tparams, superClasses) =>
          // tparams will always be a singleton
          val tparam = tparams.head
          val sigsVal = traverse(sigs0) {
            case (sigSym, sig0) => visitSig(sig0, tparam, kenv, taenv, root).map(sig => sigSym -> sig)
          }
          val lawsVal = traverse(laws0)(visitDef(_, kenv, taenv, root))
          mapN(sigsVal, lawsVal) {
            case (sigs, laws) => KindedAst.Class(doc, ann, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
          }

      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Instance, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Instance(doc, ann0, mod, sym, tpe0, tconstrs0, defs0, ns, loc) =>
      val kind = getClassKind(root.classes(sym.clazz))

      val kenvVal = inferType(tpe0, kind, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val annVal = traverse(ann0)(visitAnnotation(_, kenv, Map.empty, taenv, None, root))
          val tpeVal = visitType(tpe0, kind, kenv, Map.empty, taenv, root)
          val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, kenv, Map.empty, taenv, root))
          val defsVal = traverse(defs0)(visitDef(_, kenv, taenv, root))
          mapN(annVal, tpeVal, tconstrsVal, defsVal) {
            case (ann, tpe, tconstrs, defs) => KindedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, defs, ns, loc)
          }
      }
  }

  /**
    * Performs kinding on the given effect declaration.
    */
  private def visitEffect(eff: ResolvedAst.Effect, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Effect, KindError] = eff match {
    case ResolvedAst.Effect(doc, ann0, mod, sym, ops0, loc) =>
      val annVal = traverse(ann0)(visitAnnotation(_, KindEnv.empty, Map.empty, taenv, None, root))
      val opsVal = traverse(ops0)(visitOp(_, taenv, root))
      mapN(annVal, opsVal) {
        case (ann, ops) => KindedAst.Effect(doc, ann, mod, sym, ops, loc)
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
  private def visitDef(def0: ResolvedAst.Def, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv1 =>
          val (kenv, senv) = split(kenv1)
          val henv = None
          val specVal = visitSpec(spec0, Nil, kenv, senv, taenv, root)
          val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Def(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Sig, classTparam: KindedAst.TypeParam, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv1 =>
          val (kenv, senv) = split(kenv1)
          val henv = None
          val specVal = visitSpec(spec0, List(classTparam.sym), kenv, senv, taenv, root)
          val expVal = traverseOpt(exp0)(visitExp(_, kenv, senv, taenv, henv, root))
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given effect operation under the given kind environment.
    */
  private def visitOp(op: ResolvedAst.Op, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Op, KindError] = op match {
    case ResolvedAst.Op(sym, spec0) =>
      val kenvVal = inferSpec(spec0, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        case kenv1 =>
          val (kenv, senv) = split(kenv1)
          val specVal = visitSpec(spec0, Nil, kenv, senv, taenv, root)
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
  private def visitSpec(spec0: ResolvedAst.Spec, quantifiers: List[Symbol.KindedTypeVarSym], kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, tpe0, purAndEff0, tconstrs0, loc) =>
      val annVal = traverse(ann0)(visitAnnotation(_, kenv, Map.empty, taenv, None, root))
      val tparamsVal = traverse(tparams0.tparams)(visitTypeParam(_, kenv, senv)).map(_.flatten)
      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, senv, taenv, root))
      val tpeVal = visitType(tpe0, Kind.Star, kenv, senv, taenv, root)
      val purAndEffVal = visitPurityAndEffect(purAndEff0, kenv, senv, taenv, root)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, kenv, senv, taenv, root))

      mapN(annVal, tparamsVal, fparamsVal, tpeVal, purAndEffVal, tconstrsVal) {
        case (ann, tparams, fparams, tpe, (pur, eff), tconstrs) =>
          val allQuantifiers = quantifiers ::: tparams.map(_.sym)
          val base = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), pur, eff, tpe, tpe.loc)
          val sc = Scheme(allQuantifiers, tconstrs, base)
          KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, pur, eff, tconstrs, loc)
      }
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExp(exp00: ResolvedAst.Expression, kenv0: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv0: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {

    case ResolvedAst.Expression.Wild(loc) => KindedAst.Expression.Wild(Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.Var(sym, tpe0, loc) =>
      mapN(visitType(tpe0, Kind.Star, kenv0, senv, taenv, root)) {
        tpe => KindedAst.Expression.Var(sym, tpe, loc)
      }

    case ResolvedAst.Expression.Def(sym, loc) => KindedAst.Expression.Def(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.Sig(sym, loc) => KindedAst.Expression.Sig(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.Hole(sym, loc) => KindedAst.Expression.Hole(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess

    case ResolvedAst.Expression.HoleWithExp(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Effect, loc.asSynthetic)
          KindedAst.Expression.HoleWithExp(exp, tvar, pvar, evar, loc)
      }

    case ResolvedAst.Expression.Use(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Use(sym, exp, loc)
      }

    case ResolvedAst.Expression.Cst(cst, loc) => KindedAst.Expression.Cst(cst, loc).toSuccess

    case ResolvedAst.Expression.Apply(exp0, exps0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val expsVal = traverse(exps0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(expVal, expsVal) {
        case (exp, exps) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Effect, loc.asSynthetic)
          KindedAst.Expression.Apply(exp, exps, tvar, pvar, evar, loc)
      }

    case ResolvedAst.Expression.Lambda(fparam0, exp0, loc) =>
      val fparamVal = visitFormalParam(fparam0, kenv0, senv, taenv, root)
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(fparamVal, expVal) {
        case (fparam, exp) => KindedAst.Expression.Lambda(fparam, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Unary(sop, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Unary(sop, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Binary(sop, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Binary(sop, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      val exp3Val = visitExp(exp30, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val, exp3Val) {
        case (exp1, exp2, exp3) => KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
      }

    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Stm(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.Discard(exp, loc) =>
      visitExp(exp, kenv0, senv, taenv, henv0, root) map {
        case e => KindedAst.Expression.Discard(e, loc)
      }

    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.LetRec(sym, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.Region(tpe, loc) =>
      KindedAst.Expression.Region(tpe, loc).toSuccess

    case ResolvedAst.Expression.Scope(sym, regionVar, exp0, loc) =>
      val rv = Type.Var(regionVar.withKind(Kind.Bool), loc)
      val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
      flatMapN(kenv0 + (regionVar -> Kind.Bool)) {
        case kenv =>
          val expVal = visitExp(exp0, kenv, senv, taenv, henv0, root)
          mapN(expVal) {
            exp => KindedAst.Expression.Scope(sym, rv, exp, pvar, loc)
          }
      }

    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitMatchRule(_, kenv0, senv, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.Match(exp, rules, loc)
      }

    case ResolvedAst.Expression.TypeMatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitMatchTypeRule(_, kenv0, senv, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TypeMatch(exp, rules, loc)
      }

    case ResolvedAst.Expression.Choose(star, exps0, rules0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      val rulesVal = traverse(rules0)(visitChoiceRule(_, kenv0, senv, taenv, henv0, root))
      mapN(expsVal, rulesVal) {
        case (exps, rules) => KindedAst.Expression.Choose(star, exps, rules, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Tag(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Tag(sym, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(elmsVal) {
        elms => KindedAst.Expression.Tuple(elms, loc)
      }

    case ResolvedAst.Expression.RecordEmpty(loc) => KindedAst.Expression.RecordEmpty(loc).toSuccess

    case ResolvedAst.Expression.RecordSelect(exp0, field, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.RecordSelect(exp, field, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, loc) =>
      val valueVal = visitExp(value0, kenv0, senv, taenv, henv0, root)
      val restVal = visitExp(rest0, kenv0, senv, taenv, henv0, root)
      mapN(valueVal, restVal) {
        case (value, rest) => KindedAst.Expression.RecordExtend(field, value, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.RecordRestrict(field, rest0, loc) =>
      val restVal = visitExp(rest0, kenv0, senv, taenv, henv0, root)
      mapN(restVal) {
        rest => KindedAst.Expression.RecordRestrict(field, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.ArrayLit(exps, exp, loc) =>
      val esVal = traverse(exps)(visitExp(_, kenv0, senv, taenv, henv0, root))
      val eVal = visitExp(exp, kenv0, senv, taenv, henv0, root)
      mapN(esVal, eVal) {
        case (es, e) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          KindedAst.Expression.ArrayLit(es, e, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1Val = visitExp(exp1, kenv0, senv, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, senv, taenv, henv0, root)
      val e3Val = visitExp(exp3, kenv0, senv, taenv, henv0, root)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          KindedAst.Expression.ArrayNew(e1, e2, e3, tvar, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayLoad(base0, index0, loc) =>
      val baseVal = visitExp(base0, kenv0, senv, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, senv, taenv, henv0, root)
      mapN(baseVal, indexVal) {
        case (base, index) =>
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          KindedAst.Expression.ArrayLoad(base, index, Type.freshVar(Kind.Star, loc.asSynthetic), pvar, loc)
      }

    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      val baseVal = visitExp(base0, kenv0, senv, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, senv, taenv, henv0, root)
      val elmVal = visitExp(elm0, kenv0, senv, taenv, henv0, root)
      mapN(baseVal, indexVal, elmVal) {
        case (base, index, elm) =>
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          KindedAst.Expression.ArrayStore(base, index, elm, pvar, loc)
      }

    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      val baseVal = visitExp(base0, kenv0, senv, taenv, henv0, root)
      mapN(baseVal) {
        base => KindedAst.Expression.ArrayLength(base, loc)
      }

    case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
      val baseVal = visitExp(base0, kenv0, senv, taenv, henv0, root)
      val beginIndexVal = visitExp(beginIndex0, kenv0, senv, taenv, henv0, root)
      val endIndexVal = visitExp(endIndex0, kenv0, senv, taenv, henv0, root)
      mapN(baseVal, beginIndexVal, endIndexVal) {
        case (base, beginIndex, endIndex) =>
          val pvar = Type.freshVar(Kind.Bool, loc.asSynthetic)
          KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, pvar, loc)
      }

    case ResolvedAst.Expression.Ref(exp1, exp2, loc) =>
      val e1Val = visitExp(exp1, kenv0, senv, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, senv, taenv, henv0, root)
      mapN(e1Val, e2Val) {
        case (e1, e2) => KindedAst.Expression.Ref(e1, e2, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Bool, loc), loc)
      }

    case ResolvedAst.Expression.Deref(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Deref(exp, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Bool, loc), loc)
      }

    case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.Assign(exp1, exp2, Type.freshVar(Kind.Bool, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val expectedTypeVal = traverseOpt(expectedType0)(visitType(_, Kind.Star, kenv0, senv, taenv, root))
      val expectedPurAndEffVal = visitOptionalPurityAndEffect(expectedEff0, kenv0, senv, taenv, root)
      mapN(expVal, expectedTypeVal, expectedPurAndEffVal) {
        case (exp, expectedType, (expectedPur, expectedEff)) =>
          KindedAst.Expression.Ascribe(exp, expectedType, expectedPur, expectedEff, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val declaredTypeVal = traverseOpt(declaredType0)(visitType(_, Kind.Star, kenv0, senv, taenv, root))
      val declaredPurAndEffVal = visitOptionalPurityAndEffect(declaredEff0, kenv0, senv, taenv, root)
      mapN(expVal, declaredTypeVal, declaredPurAndEffVal) {
        case (exp, declaredType, (declaredPur, declaredEff)) =>
          KindedAst.Expression.Cast(exp, declaredType, declaredPur, declaredEff, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Mask(exp, loc) =>
      val eVal = visitExp(exp, kenv0, senv, taenv, henv0, root)
      mapN(eVal) {
        case e => KindedAst.Expression.Mask(e, loc)
      }

    case ResolvedAst.Expression.Upcast(exp, loc) =>
      mapN(visitExp(exp, kenv0, senv, taenv, henv0, root)) { e =>
        KindedAst.Expression.Upcast(e, Type.freshVar(Kind.Star, loc), loc)
      }

    case ResolvedAst.Expression.Supercast(exp, loc) =>
      mapN(visitExp(exp, kenv0, senv, taenv, henv0, root)) { e =>
        KindedAst.Expression.Supercast(e, Type.freshVar(Kind.Star, loc), loc)
      }

    case ResolvedAst.Expression.Without(exp0, eff, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expression.Without(exp, eff, loc)
      }

    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitCatchRule(_, kenv0, senv, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TryCatch(exp, rules, loc)
      }

    case ResolvedAst.Expression.TryWith(exp0, eff, rules0, loc) =>
      // create a fresh type variable for the handling block (same as resume result)
      // and for the operation result (same as resume argument)
      // and set the handled env
      val tvar = Type.freshVar(Kind.Star, loc)

      // use the old henv for the handled expression
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)

      // use the new henv for the handler
      val rulesVal = traverse(rules0)(visitHandlerRule(_, kenv0, senv, taenv, tvar, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expression.TryWith(exp, eff, rules, tvar, loc)
      }

    case ResolvedAst.Expression.Do(op, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(argsVal) {
        case args => KindedAst.Expression.Do(op, args, loc)
      }

    case ResolvedAst.Expression.Resume(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      // Extract the type variable from the henv
      // Missing henv should have been caught previously
      val (argTvar, retTvar) = henv0.getOrElse(throw InternalCompilerException("Unexpected missing handler env.", loc))
      mapN(expVal) {
        case exp => KindedAst.Expression.Resume(exp, argTvar, retTvar, loc)
      }

    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expression.InvokeConstructor(constructor, args, loc)
      }

    case ResolvedAst.Expression.InvokeMethod(method, clazz, exp0, args0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      val argsVal = traverse(args0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(expVal, argsVal) {
        case (exp, args) => KindedAst.Expression.InvokeMethod(method, clazz, exp, args, loc)
      }

    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expression.InvokeStaticMethod(method, args, loc)
      }

    case ResolvedAst.Expression.GetField(field, clazz, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.GetField(field, clazz, exp, loc)
      }

    case ResolvedAst.Expression.PutField(field, clazz, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.PutField(field, clazz, exp1, exp2, loc)
      }

    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess

    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.PutStaticField(field, exp, loc)
      }

    case ResolvedAst.Expression.NewObject(name, clazz, methods, loc) =>
      mapN(traverse(methods)(visitJvmMethod(_, kenv0, senv, taenv, henv0, root))) {
        methods => KindedAst.Expression.NewObject(name, clazz, methods, loc)
      }

    case ResolvedAst.Expression.NewChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.NewChannel(exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.GetChannel(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.GetChannel(exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.PutChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.PutChannel(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.SelectChannel(rules0, default0, loc) =>
      val rulesVal = traverse(rules0)(visitSelectChannelRule(_, kenv0, senv, taenv, henv0, root))
      val defaultVal = traverseOpt(default0)(visitExp(_, kenv0, senv, taenv, henv0, root))
      mapN(rulesVal, defaultVal) {
        case (rules, default) => KindedAst.Expression.SelectChannel(rules, default, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.Spawn(exp1, exp2, loc) =>
      val e1Val = visitExp(exp1, kenv0, senv, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, senv, taenv, henv0, root)
      mapN(e1Val, e2Val) {
        case (e1, e2) => KindedAst.Expression.Spawn(e1, e2, loc)
      }

    case ResolvedAst.Expression.Par(exp, loc) =>
      mapN(visitExp(exp, kenv0, senv, taenv, henv0, root))(KindedAst.Expression.Par(_, loc))

    case ResolvedAst.Expression.ParYield(frags, exp0, loc) =>
      val fragsVal = traverse(frags) {
        case ResolvedAst.ParYieldFragment(pat, exp1, l0) =>
          val patVal = visitPattern(pat, kenv0, root)
          val expVal = visitExp(exp1, kenv0, senv, taenv, henv0, root)
          mapN(patVal, expVal) {
            case (p, e) => KindedAst.ParYieldFragment(p, e, l0)
          }
      }

      mapN(fragsVal, visitExp(exp0, kenv0, senv, taenv, henv0, root)) {
        case (fs, exp) => KindedAst.Expression.ParYield(fs, exp, loc)
      }

    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Lazy(exp, loc)
      }

    case ResolvedAst.Expression.Force(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.Force(exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointConstraintSet(cs0, loc) =>
      val csVal = traverse(cs0)(visitConstraint(_, kenv0, senv, taenv, henv0, root))
      mapN(csVal) {
        cs => KindedAst.Expression.FixpointConstraintSet(cs, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(visitPredicateParam(_, kenv0, senv, taenv, root))
      val expVal = visitExp(exp, kenv0, senv, taenv, henv0, root)
      mapN(psVal, expVal) {
        case (ps, e) => KindedAst.Expression.FixpointLambda(ps, e, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
      }

    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointSolve(exp, loc)
      }

    case ResolvedAst.Expression.FixpointFilter(pred, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointFilter(pred, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointInject(exp0, pred, loc) =>
      val expVal = visitExp(exp0, kenv0, senv, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expression.FixpointInject(exp, pred, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expression.FixpointProject(pred, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, senv, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, senv, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expression.FixpointProject(pred, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      val patVal = visitPattern(pat0, kenv, root)
      val guardVal = traverseOpt(guard0)(visitExp(_, kenv, senv, taenv, henv, root))
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(patVal, guardVal, expVal) {
        case (pat, guard, exp) => KindedAst.MatchRule(pat, guard, exp)
      }
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchTypeRule(rule0: ResolvedAst.MatchTypeRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchTypeRule, KindError] = rule0 match {
    case ResolvedAst.MatchTypeRule(sym, tpe0, exp0) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, senv, taenv, root)
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(tpeVal, expVal) {
        case (tpe, exp) => KindedAst.MatchTypeRule(sym, tpe, exp)
      }
  }

  /**
    * Performs kinding on the given choice rule under the given kind environment.
    */
  private def visitChoiceRule(rule0: ResolvedAst.ChoiceRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoiceRule, KindError] = rule0 match {
    case ResolvedAst.ChoiceRule(pat0, exp0) =>
      val patVal = traverse(pat0)(visitChoicePattern)
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(patVal, expVal) {
        case (pat, exp) => KindedAst.ChoiceRule(pat, exp)
      }
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.CatchRule(sym, clazz, exp)
      }
  }

  /**
    * Performs kinding on the given handler rule under the given kind environment.
    */
  private def visitHandlerRule(rule0: ResolvedAst.HandlerRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], hTvar: Type.Var, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.HandlerRule, KindError] = rule0 match {
    case ResolvedAst.HandlerRule(op, fparams0, exp0) =>
      // create a new type variable for the op return type (same as resume argument type)
      val tvar = Type.freshVar(Kind.Star, exp0.loc)

      val henv = Some((tvar, hTvar))

      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, senv, taenv, root))
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(fparamsVal, expVal) {
        case (fparams, exp) => KindedAst.HandlerRule(op, fparams, exp, tvar)
      }
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      val chanVal = visitExp(chan0, kenv, senv, taenv, henv, root)
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
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
    case ResolvedAst.Pattern.Array(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitPattern(_, kenv, root))
      mapN(elmsVal) {
        elms => KindedAst.Pattern.Array(elms, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
    case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, loc) =>
      val elmsVal = traverse(elms0)(visitPattern(_, kenv, root))
      mapN(elmsVal) {
        elms => KindedAst.Pattern.ArrayTailSpread(elms, sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, loc) =>
      val elmsVal = traverse(elms0)(visitPattern(_, kenv, root))
      mapN(elmsVal) {
        elms => KindedAst.Pattern.ArrayHeadSpread(sym, elms, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given choice pattern under the given kind environment.
    */
  private def visitChoicePattern(pat0: ResolvedAst.ChoicePattern)(implicit flix: Flix): Validation[KindedAst.ChoicePattern, KindError] = pat0 match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.ChoicePattern.Present(sym, loc) => KindedAst.ChoicePattern.Present(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc).toSuccess
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparamsVal = traverse(cparams0)(visitConstraintParam(_, kenv, senv, taenv, root))
      val headVal = visitHeadPredicate(head0, kenv, senv, taenv, henv, root)
      val bodyVal = traverse(body0)(visitBodyPredicate(_, kenv, senv, taenv, henv, root))
      mapN(cparamsVal, headVal, bodyVal) {
        case (cparams, head, body) => KindedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Performs kinding on the given constraint param under the given kind environment.
    */
  private def visitConstraintParam(cparam0: ResolvedAst.ConstraintParam, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ConstraintParam, KindError] = cparam0 match {
    case ResolvedAst.ConstraintParam(sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, senv, taenv, root)
      mapN(tpeVal) {
        case tpe => KindedAst.ConstraintParam(sym, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given head predicate under the given kind environment.
    */
  private def visitHeadPredicate(pred0: ResolvedAst.Predicate.Head, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred0 match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      val termsVal = traverse(terms0)(visitExp(_, kenv, senv, taenv, henv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Head.Atom(pred, den, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred0: ResolvedAst.Predicate.Body, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms0, loc) =>
      val termsVal = traverse(terms0)(visitPattern(_, kenv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }

    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Guard(exp, loc)
      }

    case ResolvedAst.Predicate.Body.Loop(varSyms, exp0, loc) =>
      val expVal = visitExp(exp0, kenv, senv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Loop(varSyms, exp, loc)
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
  private def visitType(tpe0: UnkindedType, expectedKind: Kind, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => visitTypeVar(tvar, expectedKind, kenv)
    case UnkindedType.Cst(cst, loc) =>
      flatMapN(visitTypeConstructor(cst, root, loc)) {
        tycon =>
          val kind = tycon.kind
          unify(expectedKind, kind) match {
            case Some(_) => Type.Cst(tycon, loc).toSuccess
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
          }
      }
    case UnkindedType.Apply(t10, t20, loc) =>
      val t2Val = visitType(t20, Kind.Wild, kenv, senv, taenv, root)
      flatMapN(t2Val) {
        t2 =>
          val k1 = Kind.Arrow(t2.kind, expectedKind)
          val t1Val = visitType(t10, k1, kenv, senv, taenv, root)
          mapN(t1Val) {
            t1 => mkApply(t1, t2, loc)
          }
      }

    case UnkindedType.Ascribe(t, k, loc) =>
      unify(k, expectedKind) match {
        case Some(kind) => visitType(t, kind, kenv, senv, taenv, root)
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k, loc).toFailure
      }
    case UnkindedType.Alias(cst, args0, t0, loc) =>
      taenv(cst.sym) match {
        case KindedAst.TypeAlias(_, _, _, tparams, tpe, _) =>
          val argsVal = traverse(tparams.zip(args0)) {
            case (tparam, arg) => visitType(arg, tparam.sym.kind, kenv, senv, taenv, root)
          }
          val tpeVal = visitType(t0, tpe.kind, kenv, senv, taenv, root)
          flatMapN(argsVal, tpeVal) {
            case (args, t) => unify(t.kind, expectedKind) match {
              case Some(_) => Type.Alias(cst, args, t, loc).toSuccess
              case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc).toFailure
            }
          }
      }

    case UnkindedType.Arrow(purAndEff, arity, loc) =>
      val kind = Kind.mkArrow(arity)
      unify(kind, expectedKind) match {
        case Some(_) =>
          val purAndEffVal = visitPurityAndEffect(purAndEff, kenv, senv, taenv, root)
          mapN(purAndEffVal) {
            case (pur, eff) => Type.mkApply(Type.Cst(TypeConstructor.Arrow(arity), loc), List(pur, eff), loc)
          }
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case UnkindedType.ReadWrite(tpe, _) =>
      // erase the read/write wrapper
      visitType(tpe, expectedKind, kenv, senv, taenv, root)

    case UnkindedType.Enum(sym, loc) =>
      val kind = getEnumKind(root.enums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.Enum(sym, k), loc).toSuccess
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
      }

    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe0.loc)


  }

  /**
    * Performs kinding on the given type constraint under the given kind environment.
    */
  private def visitTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(head, tpe0, loc) =>
      val classKind = getClassKind(root.classes(head.sym))
      mapN(visitType(tpe0, classKind, kenv, senv, taenv, root)) {
        tpe => Ast.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type constructor under the given kind environment.
    */
  private def visitTypeConstructor(tycon: TypeConstructor, root: ResolvedAst.Root, loc: SourceLocation)(implicit flix: Flix): Validation[TypeConstructor, KindError] = tycon match {
    case _: TypeConstructor.Enum => throw InternalCompilerException("Unexpected kinded enum.", loc)
    case t => t.toSuccess
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym]): Validation[List[KindedAst.TypeParam], KindError] = {
    val (name, sym0, loc) = tparam match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
    }
    // split the symbol if necessary
    val syms = sym0 :: senv.get(sym0).toList
    traverse(syms) {
      sym => mapN(visitTypeVarSym(sym, Kind.Wild, kenv, loc))(KindedAst.TypeParam(name, _, loc))
    }
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, src, loc) =>
      mapN(visitType(tpe0, Kind.Star, kenv, senv, taenv, root)) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, src, loc)
      }
  }

  /**
    * Performs kinding on the given predicate param under the given kind environment.
    */
  private def visitPredicateParam(pparam0: ResolvedAst.PredicateParam, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.PredicateParam, KindError] = pparam0 match {
    case ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      val tpe = Type.freshVar(Kind.Predicate, loc)
      KindedAst.PredicateParam(pred, tpe, loc).toSuccess

    case ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, Kind.Star, kenv, senv, taenv, root))) {
        case ts =>
          val tpe = den match {
            case Denotation.Relational => Type.mkRelation(ts, pred.loc.asSynthetic)
            case Denotation.Latticenal => Type.mkLattice(ts, pred.loc.asSynthetic)
          }
          KindedAst.PredicateParam(pred, tpe, loc)
      }

  }

  /**
    * Performs kinding on the given annotation under the given kind environment.
    */
  private def visitAnnotation(ann: ResolvedAst.Annotation, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      mapN(traverse(exps0)(visitExp(_, kenv, senv, taenv, henv, root))) {
        exps => KindedAst.Annotation(name, exps, loc)
      }
  }

  /**
    * Performs kinding on the given purity and effect.
    * Defaults to the respective `Pure` value for each component if absent.
    */
  private def visitPurityAndEffect(purAndEff: UnkindedType.PurityAndEffect, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[(Type, Type), KindError] = purAndEff match {
    case UnkindedType.PurityAndEffect(pur0, eff0) =>
      // collect all the individual types
      val tpes0 = pur0.toList ::: eff0.getOrElse(Nil)

      // split all types in the split env
      val unkindedTpes = tpes0.flatMap {
        case tpe@UnkindedType.Var(sym, loc) if senv.contains(sym) =>
          val pur = tpe
          val eff = UnkindedType.Var(senv(sym), loc)
          List(pur, eff)

        case tpe => List(tpe)
      }

      // perform kinding on all the types
      val tpesVal = traverse(unkindedTpes)(visitType(_, Kind.Beef, kenv, Map.empty, taenv, root))

      mapN(tpesVal) {
        case tpes =>
          // sort the types by kind
          val byKind = tpes.groupBy(_.kind)
          val purs = byKind.getOrElse(Kind.Bool, Nil)
          val effs = byKind.getOrElse(Kind.Effect, Nil)

          // find the location of a necessarily impure effect if it exists
          val impureEff = effs.collectFirst {
            case Type.Cst(TypeConstructor.Effect(sym), loc) if sym == IoSym || sym == NonDetSym => loc
          }

          val pur = impureEff match {
            // Case 1: There is a necessarily impure effect. We are impure.
            case Some(loc) => Type.mkFalse(loc)
            // Case 2: There is no necessarily impure effect. Build the formula
            case None =>
              purs.reduceOption({
                case (t1, t2) => Type.mkAnd(t1, t2, t2.loc.asSynthetic)
              }: (Type, Type) => Type).getOrElse(Type.Pure)
          }

          val eff = effs.reduceLeftOption({
            case (acc, t) => Type.mkUnion(acc, t, t.loc.asSynthetic)
          }: (Type, Type) => Type).getOrElse(Type.Empty)

          (pur, eff)
      }
  }

  /**
    * Performs kinding on the given optional purity and effect.
    */
  private def visitOptionalPurityAndEffect(purAndEff0: UnkindedType.PurityAndEffect, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[(Option[Type], Option[Type]), KindError] = purAndEff0 match {
    case UnkindedType.PurityAndEffect(None, None) => (None, None).toSuccess
    case purAndEff =>
      mapN(visitPurityAndEffect(purAndEff, kenv, senv, taenv, root)) {
        case (pur, eff) => (Some(pur), Some(eff))
      }
  }

  private def visitJvmMethod(method: ResolvedAst.JvmMethod, kenv: KindEnv, senv: Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym], taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit flix: Flix) = method match {
    case ResolvedAst.JvmMethod(_, fparams, exp, tpe0, _, _) =>
      val fparamsVal = traverse(fparams)(visitFormalParam(_, kenv, senv, taenv, root))
      val expVal = visitExp(exp, kenv, senv, taenv, henv, root)
      val purAndEffVal = visitPurityAndEffect(method.purAndEff, kenv, senv, taenv, root)
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, Map.empty, taenv, root)
      mapN(fparamsVal, expVal, tpeVal, purAndEffVal) {
        case (f, e, tpe, (pur, eff)) => KindedAst.JvmMethod(method.ident, f, e, tpe, pur, eff, method.loc)
      }
  }

  /**
    * Infers a kind environment from the given spec.
    * A KindEnvironment is provided in case some subset of of kinds have been declared (and therefore should not be inferred),
    * as in the case of a class type parameter used in a sig or law.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, purAndEff, tconstrs, _) =>
      val fparamKenvsVal = traverse(fparams)(inferFormalParam(_, kenv, taenv, root))
      val tpeKenvVal = inferType(tpe, Kind.Star, kenv, taenv, root)
      val effKenvVal = inferPurityAndEffect(purAndEff, kenv, taenv, root)
      val tconstrsKenvsVal = traverse(tconstrs)(inferTypeConstraint(_, kenv, taenv, root))

      flatMapN(fparamKenvsVal, tpeKenvVal, effKenvVal, tconstrsKenvsVal) {
        case (fparamKenvs, tpeKenv, effKenv, tconstrKenvs) =>
          KindEnv.merge(fparamKenvs ::: tpeKenv :: effKenv :: tconstrKenvs)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _, _) => inferType(tpe0, Kind.Star, kenv, taenv, root)
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
      Validation.fold(tpe.typeArguments.zip(args), KindEnv.singleton(tvar.sym -> tyconKind)) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Cst(cst, loc) =>
      val tyconKind = getTyconKind(cst, root, loc)
      val args = Kind.kindArgs(tyconKind)

      Validation.fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Ascribe(t, k, _) => inferType(t, k, kenv0, taenv, root)

    case UnkindedType.Alias(cst, args, _, _) =>
      val alias = taenv(cst.sym)
      val tparamKinds = alias.tparams.map(_.sym.kind)

      Validation.fold(args.zip(tparamKinds), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Arrow(purAndEff, _, _) =>
      val purAndEffKenvVal = inferPurityAndEffect(purAndEff, kenv0, taenv, root)
      val argKenvVal = Validation.fold(tpe.typeArguments, KindEnv.empty) {
        case (acc, targ) => flatMapN(inferType(targ, Kind.Star, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }
      flatMapN(purAndEffKenvVal, argKenvVal) {
        case (purAndEffKenv, argKenv) => purAndEffKenv ++ argKenv
      }

    case UnkindedType.ReadWrite(t, _) => inferType(t, Kind.Bool, kenv0, taenv, root)

    case UnkindedType.Enum(sym, _) =>
      val tyconKind = getEnumKind(root.enums(sym))
      val args = Kind.kindArgs(tyconKind)

      Validation.fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", tpe.loc)
    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe.loc)
  }

  /**
    * Infers the given purity and effect under the kind environment.
    * If both are specified, then inference is performed assuming the purity is a Bool and the effect is an Effect.
    * Otherwise infers as [[Kind.Beef]] to be disambiguated later.
    */
  private def inferPurityAndEffect(purAndEff0: UnkindedType.PurityAndEffect, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = purAndEff0 match {
    // Case 1: Both missing. No inference to do.
    case UnkindedType.PurityAndEffect(None, None) => KindEnv.empty.toSuccess
    // Case 2: Only purity: Infer as both Bool and Eff
    case UnkindedType.PurityAndEffect(Some(pur), None) => inferType(pur, Kind.Beef, kenv, taenv, root)
    // Case 3: Only effect: Infer as both Bool and Eff
    case UnkindedType.PurityAndEffect(None, Some(effs)) =>
      val kenvsVal = traverse(effs)(inferType(_, Kind.Beef, kenv, taenv, root))
      flatMapN(kenvsVal) {
        case kenvs => KindEnv.merge(kenvs)
      }
    // Case 4: Purity and Effect. Infer Purity as Bool and Effect as Effect.
    case UnkindedType.PurityAndEffect(Some(pur), Some(effs)) =>
      val purKenvVal = inferType(pur, Kind.Bool, kenv, taenv, root)
      val effKenvsVal = traverse(effs)(inferType(_, Kind.Effect, kenv, taenv, root))
      flatMapN(purKenvVal, effKenvsVal) {
        case (purKenv, effKenvs) => KindEnv.merge(purKenv :: effKenvs)
      }
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
    * Gets a kind environment from the spec.
    */
  private def getKindEnvFromSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _) =>
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
  private def getEnumKind(enum0: ResolvedAst.Enum)(implicit flix: Flix): Kind = enum0 match {
    case ResolvedAst.Enum(_, _, _, _, tparams, _, _, _, _) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the class.
    */
  private def getClassKind(clazz: ResolvedAst.Class): Kind = clazz.tparam match {
    case ResolvedAst.TypeParam.Kinded(_, _, kind, _) => kind
    case _: ResolvedAst.TypeParam.Unkinded => Kind.Star
  }

  /**
    * Gets the kind associated with the type constructor.
    */
  private def getTyconKind(tycon: TypeConstructor, root: ResolvedAst.Root, loc: SourceLocation)(implicit flix: Flix): Kind = tycon match {
    case _: TypeConstructor.Enum => throw InternalCompilerException("Unexpected kinded enum.", loc)
    case t => t.kind
  }

  /**
    * Creates the type application `t1[t2]`, while simplifying trivial boolean formulas.
    */
  private def mkApply(t1: Type, t2: Type, loc: SourceLocation): Type = t1 match {
    case Type.Apply(Type.Cst(TypeConstructor.And, _), arg, _) => Type.mkAnd(arg, t2, loc)
    case Type.Apply(Type.Cst(TypeConstructor.Or, _), arg, _) => Type.mkOr(arg, t2, loc)
    case Type.Cst(TypeConstructor.Not, _) => Type.mkNot(t2, loc)

    case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), arg, _) => Type.mkIntersection(arg, t2, loc)
    case Type.Apply(Type.Cst(TypeConstructor.Union, _), arg, _) => Type.mkUnion(arg, t2, loc)
    case Type.Cst(TypeConstructor.Complement, _) => Type.mkComplement(t2, loc)

    case t => Type.Apply(t, t2, loc)
  }

  /**
    * Splits the given kind environment according to the [[Kind.Beef]]-kinded type parameters therein.
    * A type variable with kind [[Kind.Beef]] is split into two type variables:
    * One with the original symbol and kind [[Kind.Bool]],
    * and one with a fresh symbol and kind [[Kind.Effect]].
    *
    * Other type variables are left unchanged.
    *
    * A split environment is also provided, mapping the original symbol to the `Effect` symbol.
    *
    * For example,
    * let kenv = {
    * w -> Beef,
    * x -> Bool,
    * y -> Effect,
    * z -> Star
    * }
    * Then `split(kenv)` produces:
    * - a kind environment kenv1 = {
    * w -> Bool,
    * w' -> Effect,
    * x -> Bool,
    * y -> Effect,
    * z -> Star
    * }
    * - a split environment senv = {
    * w -> w'
    * }
    *
    */
  private def split(kenv0: KindEnv)(implicit flix: Flix): (KindEnv, Map[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym]) = {
    val (kenv, senv) = kenv0.map.foldLeft((Map.empty[Symbol.UnkindedTypeVarSym, Kind], Map.empty[Symbol.UnkindedTypeVarSym, Symbol.UnkindedTypeVarSym])) {
      case ((kenvAcc, senvAcc), (sym, Kind.Beef)) =>
        val effSym = Symbol.freshUnkindedTypeVarSym(prime(sym.text), sym.isRegion, sym.loc)

        val kenv = kenvAcc + (effSym -> Kind.Effect) + (sym -> Kind.Bool)
        val senv = senvAcc + (sym -> effSym)
        (kenv, senv)

      case ((kenvAcc, senvAcc), (sym, kind)) =>
        val kenv = kenvAcc + (sym -> kind)
        val senv = senvAcc
        (kenv, senv)
    }
    (KindEnv(kenv), senv)
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
      Validation.fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
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
      Validation.fold(other.map, this) {
        case (acc, pair) => acc + pair
      }
    }
  }
}
