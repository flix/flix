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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse}
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
  *       - If the type variable is the effect type of the function, it is ascribed kind Bool.
  *       - If the type variable is an argument to a type constraint, it is ascribed the class's parameter kind
  *       - If the type variable is an argument to a type constructor, it is ascribed the type constructor's parameter kind.
  *       - If the type variable is used as an type constructor, it is ascribed the kind Star -> Star ... -> Star -> X,
  *         where X is the kind inferred from enacting these rules in the place of the fully-applied type.
  *       - If there is an inconsistency among these kinds, an error is raised.
  *
  * In inferring types, variable type constructors are assumed to have kind * -> * -> * -> ???.
  */
object Kinder {

  def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationMessage] = flix.phase("Kinder") {

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    visitTypeAliases(root.taOrder, root) flatMap {
      taenv =>

        // Extra type annotations are required due to limitations in Scala's type inference.
        val enumsVal = Validation.sequence(ParOps.parMap(root.enums)({
          pair: (Symbol.EnumSym, ResolvedAst.Enum) =>
            val (sym, enum) = pair
            visitEnum(enum, taenv, root).map(sym -> _)
        }))

        val classesVal = Validation.sequence(ParOps.parMap(root.classes)({
          pair: (Symbol.ClassSym, ResolvedAst.Class) =>
            val (sym, clazz) = pair
            visitClass(clazz, taenv, root).map(sym -> _)
        }))

        val defsVal = Validation.sequence(ParOps.parMap(root.defs)({
          pair: (Symbol.DefnSym, ResolvedAst.Def) =>
            val (sym, defn) = pair
            visitDef(defn, KindEnv.empty, taenv, root).map(sym -> _)
        }))

        val instancesVal = Validation.sequence(ParOps.parMap(root.instances)({
          pair: (Symbol.ClassSym, List[ResolvedAst.Instance]) =>
            val (sym, insts) = pair
            traverse(insts)(visitInstance(_, taenv, root)).map(sym -> _)
        }))

        mapN(enumsVal, classesVal, defsVal, instancesVal) {
          case (enums, classes, defs, instances) =>
            KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, taenv, root.reachable, root.sources)
        }
    }

  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum: ResolvedAst.Enum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, CompilationMessage] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, derives, cases0, tpeDeprecated0, sc0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val casesVal = Validation.traverse(cases0) {
        case (tag, case0) => mapN(visitCase(case0, kenv, taenv, root)) {
          caze => (tag, caze)
        }
      }
      val tpeDeprecatedVal = visitType(tpeDeprecated0, Kind.Star, kenv, taenv, root)
      val scVal = visitScheme(sc0, kenv, taenv, root)

      mapN(tparamsVal, casesVal, tpeDeprecatedVal, scVal) {
        case (tparams, cases, tpeDeprecated, sc) => KindedAst.Enum(doc, mod, sym, tparams, derives, cases.toMap, tpeDeprecated, sc, loc)
      }
  }

  /**
    * Performs kinding on the given type alias.
    * Returns the kind of the type alias.
    */
  private def visitTypeAlias(alias: ResolvedAst.TypeAlias, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.TypeAlias, KindError] = alias match {
    case ResolvedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
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
    Validation.fold(aliases, Map.empty[Symbol.TypeAliasSym, KindedAst.TypeAlias]) {
      case (taenv, sym) =>
        val alias = root.typealiases(sym)
        visitTypeAlias(alias, taenv, root) map {
          kind => taenv + (sym -> kind)
        }
    }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitCase(caze0: ResolvedAst.Case, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated0, sc0) =>
      for {
        tpeDeprecated <- visitType(tpeDeprecated0, Kind.Star, kenv, taenv, root)
        sc <- visitScheme(sc0, kenv, taenv, root)
      } yield KindedAst.Case(enum, tag, tpeDeprecated, sc)
  }

  /**
    * Performs kinding on the given type class.
    */
  private def visitClass(clazz: ResolvedAst.Class, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, KindError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParamDefaultStar(tparam0)

      val tparamVal = visitTypeParam(tparam0, kenv)
      val superClassesVal = Validation.traverse(superClasses0)(visitTypeConstraint(_, kenv, taenv, root))
      val sigsVal = Validation.traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, kenv, taenv, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, kenv, taenv, root))

      mapN(tparamVal, superClassesVal, sigsVal, lawsVal) {
        case (tparam, superClasses, sigs, laws) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Instance, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe0, tconstrs0, defs0, ns, loc) =>
      val kind = getClassKind(root.classes(sym.clazz))
      for {
        kenv <- inferType(tpe0, kind, KindEnv.empty, taenv, root)
        tpeVal = visitType(tpe0, kind, kenv, taenv, root)
        tconstrsVal = Validation.traverse(tconstrs0)(visitTypeConstraint(_, kenv, taenv, root))
        defsVal = Validation.traverse(defs0)(visitDef(_, kenv, taenv, root))
        result <- Validation.sequenceT(tpeVal, tconstrsVal, defsVal)
        (tpe, tconstrs, defs) = result
      } yield KindedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
  }

  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Def, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)
      for {
        kenv <- getKindEnvFromSpec(spec0, kenv0, taenv, root)
        spec <- visitSpec(spec0, kenv, taenv, root)
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Def(sym, spec, exp)
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Sig, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      for {
        kenv <- getKindEnvFromSpec(spec0, kenv0, taenv, root)
        spec <- visitSpec(spec0, kenv, taenv, root)
        exp <- Validation.traverse(exp0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Sig(sym, spec, exp.headOption)
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    */
  private def visitSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, tpe0, eff0, loc) =>
      val annVal = Validation.traverse(ann0)(visitAnnotation(_, kenv, taenv, root))
      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val fparamsVal = Validation.traverse(fparams0)(visitFormalParam(_, kenv, taenv, root))
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val effVal = visitType(eff0, Kind.Bool, kenv, taenv, root)
      val scVal = visitScheme(sc0, kenv, taenv, root)
      for {
        result <- Validation.sequenceT(annVal, tparamsVal, fparamsVal, tpeVal, effVal)
        (ann, tparams, fparams, tpe, eff) = result
        sc <- scVal // ascribe the scheme separately
      } yield KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc)
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExp(exp00: ResolvedAst.Expression, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {

    case ResolvedAst.Expression.Wild(loc) => KindedAst.Expression.Wild(Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.Var(sym, tpe0, loc) =>
      mapN(visitType(tpe0, Kind.Star, kenv, taenv, root)) {
        tpe => KindedAst.Expression.Var(sym, tpe, loc)
      }

    case ResolvedAst.Expression.Def(sym, loc) => KindedAst.Expression.Def(sym, Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.Sig(sym, loc) => KindedAst.Expression.Sig(sym, Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.Hole(sym, loc) => KindedAst.Expression.Hole(sym, Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.Unit(loc) => KindedAst.Expression.Unit(loc).toSuccess

    case ResolvedAst.Expression.Null(loc) => KindedAst.Expression.Null(loc).toSuccess

    case ResolvedAst.Expression.True(loc) => KindedAst.Expression.True(loc).toSuccess

    case ResolvedAst.Expression.False(loc) => KindedAst.Expression.False(loc).toSuccess

    case ResolvedAst.Expression.Char(lit, loc) => KindedAst.Expression.Char(lit, loc).toSuccess

    case ResolvedAst.Expression.Float32(lit, loc) => KindedAst.Expression.Float32(lit, loc).toSuccess

    case ResolvedAst.Expression.Float64(lit, loc) => KindedAst.Expression.Float64(lit, loc).toSuccess

    case ResolvedAst.Expression.Int8(lit, loc) => KindedAst.Expression.Int8(lit, loc).toSuccess

    case ResolvedAst.Expression.Int16(lit, loc) => KindedAst.Expression.Int16(lit, loc).toSuccess

    case ResolvedAst.Expression.Int32(lit, loc) => KindedAst.Expression.Int32(lit, loc).toSuccess

    case ResolvedAst.Expression.Int64(lit, loc) => KindedAst.Expression.Int64(lit, loc).toSuccess

    case ResolvedAst.Expression.BigInt(lit, loc) => KindedAst.Expression.BigInt(lit, loc).toSuccess

    case ResolvedAst.Expression.Str(lit, loc) => KindedAst.Expression.Str(lit, loc).toSuccess

    case ResolvedAst.Expression.Default(loc) => KindedAst.Expression.Default(Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.Apply(exp0, exps0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        exps <- Validation.traverse(exps0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.Apply(exp, exps, Type.freshVar(Kind.Star, loc), Type.freshVar(Kind.Bool, loc), loc)

    case ResolvedAst.Expression.Lambda(fparam0, exp0, loc) =>
      for {
        fparam <- visitFormalParam(fparam0, kenv, taenv, root)
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Lambda(fparam, exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Unary(sop, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Unary(sop, exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Binary(sop, exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.Binary(sop, exp1, exp2, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
        exp3 <- visitExp(exp30, kenv, taenv, root)
      } yield KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)

    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.Stm(exp1, exp2, loc)

    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)

    case ResolvedAst.Expression.LetRec(sym, mod, exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc)

    case ResolvedAst.Expression.LetRegion(sym, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.LetRegion(sym, exp, Type.freshVar(Kind.Bool, loc), loc)

    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        rules <- traverse(rules0)(visitMatchRule(_, kenv, taenv, root))
      } yield KindedAst.Expression.Match(exp, rules, loc)

    case ResolvedAst.Expression.Choose(star, exps0, rules0, loc) =>
      for {
        exps <- Validation.traverse(exps0)(visitExp(_, kenv, taenv, root))
        rules <- Validation.traverse(rules0)(visitChoiceRule(_, kenv, taenv, root))
      } yield KindedAst.Expression.Choose(star, exps, rules, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Tag(sym, tag, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Tag(sym, tag, exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.Tuple(elms, loc)

    case ResolvedAst.Expression.RecordEmpty(loc) => KindedAst.Expression.RecordEmpty(Type.freshVar(Kind.Star, loc), loc).toSuccess

    case ResolvedAst.Expression.RecordSelect(exp0, field, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.RecordSelect(exp, field, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, loc) =>
      for {
        value <- visitExp(value0, kenv, taenv, root)
        rest <- visitExp(rest0, kenv, taenv, root)
      } yield KindedAst.Expression.RecordExtend(field, value, rest, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.RecordRestrict(field, rest0, loc) =>
      for {
        rest <- visitExp(rest0, kenv, taenv, root)
      } yield KindedAst.Expression.RecordRestrict(field, rest, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.ArrayLit(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.ArrayLit(elms, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.ArrayNew(elm0, len0, loc) =>
      for {
        elm <- visitExp(elm0, kenv, taenv, root)
        len <- visitExp(len0, kenv, taenv, root)
      } yield KindedAst.Expression.ArrayNew(elm, len, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.ArrayLoad(base0, index0, loc) =>
      for {
        base <- visitExp(base0, kenv, taenv, root)
        index <- visitExp(index0, kenv, taenv, root)
      } yield KindedAst.Expression.ArrayLoad(base, index, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      for {
        base <- visitExp(base0, kenv, taenv, root)
        index <- visitExp(index0, kenv, taenv, root)
        elm <- visitExp(elm0, kenv, taenv, root)
      } yield KindedAst.Expression.ArrayStore(base, index, elm, loc)

    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      for {
        base <- visitExp(base0, kenv, taenv, root)
      } yield KindedAst.Expression.ArrayLength(base, loc)

    case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
      for {
        base <- visitExp(base0, kenv, taenv, root)
        beginIndex <- visitExp(beginIndex0, kenv, taenv, root)
        endIndex <- visitExp(endIndex0, kenv, taenv, root)
      } yield KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)

    case ResolvedAst.Expression.Ref(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Ref(exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.RefWithRegion(exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.RefWithRegion(exp1, exp2, Type.freshVar(Kind.Star, loc), Type.freshVar(Kind.Bool, loc), loc)

    case ResolvedAst.Expression.Deref(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Deref(exp, Type.freshVar(Kind.Star, loc), Type.freshVar(Kind.Bool, loc), loc)

    case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.Assign(exp1, exp2, Type.freshVar(Kind.Bool, loc), loc)

    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        expectedType <- Validation.traverse(expectedType0)(visitType(_, Kind.Star, kenv, taenv, root))
        expectedEff <- Validation.traverse(expectedEff0)(visitType(_, Kind.Bool, kenv, taenv, root))
      } yield KindedAst.Expression.Ascribe(exp, expectedType.headOption, expectedEff.headOption, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        declaredType <- Validation.traverse(declaredType0)(visitType(_, Kind.Star, kenv, taenv, root))
        declaredEff <- Validation.traverse(declaredEff0)(visitType(_, Kind.Bool, kenv, taenv, root))
      } yield KindedAst.Expression.Cast(exp, declaredType.headOption, declaredEff.headOption, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        rules <- Validation.traverse(rules0)(visitCatchRule(_, kenv, taenv, root))
      } yield KindedAst.Expression.TryCatch(exp, rules, loc)

    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.InvokeConstructor(constructor, args, loc)

    case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        args <- Validation.traverse(args0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.InvokeMethod(method, exp, args, loc)

    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.InvokeStaticMethod(method, args, loc)

    case ResolvedAst.Expression.GetField(field, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.GetField(field, exp, loc)

    case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.PutField(field, exp1, exp2, loc)

    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess

    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.PutStaticField(field, exp, loc)

    case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
        tpe <- visitType(tpe0, Kind.Star, kenv, taenv, root)
      } yield KindedAst.Expression.NewChannel(exp, tpe, loc)

    case ResolvedAst.Expression.GetChannel(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.GetChannel(exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.PutChannel(exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.PutChannel(exp1, exp2, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.SelectChannel(rules0, default0, loc) =>
      for {
        rules <- Validation.traverse(rules0)(visitSelectChannelRule(_, kenv, taenv, root))
        default <- Validation.traverse(default0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Expression.SelectChannel(rules, default.headOption, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Spawn(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Spawn(exp, loc)

    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Lazy(exp, loc)

    case ResolvedAst.Expression.Force(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.Force(exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.FixpointConstraintSet(cs0, loc) =>
      for {
        cs <- Validation.traverse(cs0)(visitConstraint(_, kenv, taenv, root))
      } yield KindedAst.Expression.FixpointConstraintSet(cs, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.FixpointMerge(exp1, exp2, loc)

    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.FixpointSolve(exp, loc)

    case ResolvedAst.Expression.FixpointFilter(pred, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.FixpointFilter(pred, exp, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Expression.FixpointProjectIn(exp, pred, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, loc) =>
      for {
        exp1 <- visitExp(exp10, kenv, taenv, root)
        exp2 <- visitExp(exp20, kenv, taenv, root)
      } yield KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, Type.freshVar(Kind.Star, loc), loc)

    case ResolvedAst.Expression.Reify(t0, loc) =>
      for {
        t <- visitType(t0, Kind.Bool, kenv, taenv, root)
      } yield KindedAst.Expression.Reify(t, loc)

    case ResolvedAst.Expression.ReifyType(t0, k0, loc) =>
      for {
        t <- visitType(t0, k0, kenv, taenv, root)
      } yield KindedAst.Expression.ReifyType(t, k0, loc)

    case ResolvedAst.Expression.ReifyEff(sym, exp1, exp2, exp3, loc) =>
      for {
        e1 <- visitExp(exp1, kenv, taenv, root)
        e2 <- visitExp(exp2, kenv, taenv, root)
        e3 <- visitExp(exp3, kenv, taenv, root)
      } yield KindedAst.Expression.ReifyEff(sym, e1, e2, e3, loc)

  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      for {
        pat <- visitPattern(pat0, kenv, root)
        guard <- visitExp(guard0, kenv, taenv, root)
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Performs kinding on the given choice rule under the given kind environment.
    */
  private def visitChoiceRule(rule0: ResolvedAst.ChoiceRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoiceRule, KindError] = rule0 match {
    case ResolvedAst.ChoiceRule(pat0, exp0) =>
      for {
        pat <- Validation.traverse(pat0)(visitChoicePattern(_, kenv, root))
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.ChoiceRule(pat, exp)
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.CatchRule(sym, clazz, exp)
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      for {
        chan <- visitExp(chan0, kenv, taenv, root)
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.SelectChannelRule(sym, chan, exp)
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def visitPattern(pat0: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Pattern, KindError] = pat0 match {
    case ResolvedAst.Pattern.Wild(loc) => KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc).toSuccess
    case ResolvedAst.Pattern.Var(sym, loc) => KindedAst.Pattern.Var(sym, Type.freshVar(Kind.Star, loc), loc).toSuccess
    case ResolvedAst.Pattern.Unit(loc) => KindedAst.Pattern.Unit(loc).toSuccess
    case ResolvedAst.Pattern.True(loc) => KindedAst.Pattern.True(loc).toSuccess
    case ResolvedAst.Pattern.False(loc) => KindedAst.Pattern.False(loc).toSuccess
    case ResolvedAst.Pattern.Char(lit, loc) => KindedAst.Pattern.Char(lit, loc).toSuccess
    case ResolvedAst.Pattern.Float32(lit, loc) => KindedAst.Pattern.Float32(lit, loc).toSuccess
    case ResolvedAst.Pattern.Float64(lit, loc) => KindedAst.Pattern.Float64(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int8(lit, loc) => KindedAst.Pattern.Int8(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int16(lit, loc) => KindedAst.Pattern.Int16(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int32(lit, loc) => KindedAst.Pattern.Int32(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int64(lit, loc) => KindedAst.Pattern.Int64(lit, loc).toSuccess
    case ResolvedAst.Pattern.BigInt(lit, loc) => KindedAst.Pattern.BigInt(lit, loc).toSuccess
    case ResolvedAst.Pattern.Str(lit, loc) => KindedAst.Pattern.Str(lit, loc).toSuccess
    case ResolvedAst.Pattern.Tag(sym, tag, pat0, loc) =>
      for {
        pat <- visitPattern(pat0, kenv, root)
      } yield KindedAst.Pattern.Tag(sym, tag, pat, Type.freshVar(Kind.Star, loc), loc)
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.Tuple(elms, loc)
    case ResolvedAst.Pattern.Array(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.Array(elms, Type.freshVar(Kind.Star, loc), loc)
    case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayTailSpread(elms, sym, Type.freshVar(Kind.Star, loc), loc)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayHeadSpread(sym, elms, Type.freshVar(Kind.Star, loc), loc)
  }

  /**
    * Performs kinding on the given choice pattern under the given kind environment.
    */
  private def visitChoicePattern(pat0: ResolvedAst.ChoicePattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoicePattern, KindError] = pat0 match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.ChoicePattern.Present(sym, loc) => KindedAst.ChoicePattern.Present(sym, Type.freshVar(Kind.Star, loc), loc).toSuccess
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      for {
        cparams <- Validation.traverse(cparams0)(visitConstraintParam(_, kenv, root))
        head <- visitHeadPredicate(head0, kenv, taenv, root)
        body <- Validation.traverse(body0)(visitBodyPredicate(_, kenv, taenv, root))
      } yield KindedAst.Constraint(cparams, head, body, loc)
  }

  /**
    * Performs kinding on the given constraint param under the given kind environment.
    */
  private def visitConstraintParam(cparam0: ResolvedAst.ConstraintParam, kenv: KindEnv, root: ResolvedAst.Root): Validation[KindedAst.ConstraintParam, KindError] = cparam0 match {
    case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc) => KindedAst.ConstraintParam.HeadParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc) => KindedAst.ConstraintParam.RuleParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
  }

  /**
    * Performs kinding on the given head predicate under the given kind environment.
    */
  private def visitHeadPredicate(pred: ResolvedAst.Predicate.Head, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      for {
        terms <- Validation.traverse(terms0)(visitExp(_, kenv, taenv, root))
      } yield KindedAst.Predicate.Head.Atom(pred, den, terms, Type.freshVar(Kind.Predicate, loc), loc)
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred: ResolvedAst.Predicate.Body, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, loc) =>
      for {
        terms <- Validation.traverse(terms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, Type.freshVar(Kind.Predicate, loc), loc)

    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Predicate.Body.Guard(exp, loc)

    case ResolvedAst.Predicate.Body.Loop(varSyms, exp0, loc) =>
      for {
        exp <- visitExp(exp0, kenv, taenv, root)
      } yield KindedAst.Predicate.Body.Loop(varSyms, exp, loc)
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVar(tvar: Type.UnkindedVar, expectedKind: Kind, kenv: KindEnv): Validation[Type.KindedVar, KindError] = tvar match {
    case tvar@Type.UnkindedVar(id, loc, rigidity, text) =>
      kenv.map.get(tvar) match {
        // Case 1: we don't know about this kind, just ascribe it with what the context expects
        case None => tvar.ascribedWith(expectedKind).toSuccess
        // Case 2: we know about this kind, make sure it's behaving as we expect
        case Some(actualKind) =>
          unify(expectedKind, actualKind) match {
            case Some(kind) => Type.KindedVar(id, kind, loc, rigidity, text).toSuccess
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc = loc).toFailure
          }
      }
  }

  /**
    * Performs kinding on the given free type variable, with `kindMatch` expected from context.
    */
  private def visitFreeTypeVar(tvar: Type.UnkindedVar, expectedKind: Kind): Type.KindedVar = tvar match {
    case Type.UnkindedVar(id, rigidity, text, loc) =>
      Type.KindedVar(id, expectedKind, rigidity, text, loc)
  }

  /**
    * Performs kinding on the given type under the given kind environment, with `expectedKind` expected from context.
    * This is roughly analogous to the reassembly of expressions under a type environment, except that:
    * - Kind errors may be discovered here as they may not have been found during inference (or inference may not have happened at all).
    */
  private def visitType(tpe0: Type, expectedKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: Type.UnkindedVar => visitTypeVar(tvar, expectedKind, kenv)
    case Type.Cst(cst, loc) =>
      visitTypeConstructor(cst, kenv, taenv, root) flatMap {
        tycon =>
          val kind = tycon.kind
          unify(expectedKind, kind) match {
            case Some(_) => Type.Cst(tycon, loc).toSuccess
            case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc).toFailure
          }
      }
    case Type.Apply(t10, t20, loc) =>
      for {
        t2 <- visitType(t20, Kind.Wild, kenv, taenv, root)
        k1 = Kind.Arrow(t2.kind, expectedKind)
        t1 <- visitType(t10, k1, kenv, taenv, root)
      } yield Type.Apply(t1, t2, loc)
    case Type.Ascribe(t, k, loc) =>
      unify(k, expectedKind) match {
        case Some(kind) => visitType(t, kind, kenv, taenv, root)
        case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k, loc).toFailure
      }
    case Type.Alias(cst, args0, t0, loc) =>
      taenv(cst.sym) match {
        case KindedAst.TypeAlias(_, _, _, tparams, tpe, _) =>
          val argsVal = traverse(tparams.zip(args0)) {
            case (tparam, arg) => visitType(arg, tparam.tpe.kind, kenv, taenv, root)
          }
          val tpeVal = visitType(t0, tpe.kind, kenv, taenv, root)
          flatMapN(argsVal, tpeVal) {
            case (args, t) => unify(t.kind, expectedKind) match {
              case Some(_) => Type.Alias(cst, args, t, loc).toSuccess
              case None => KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc).toFailure
            }
          }
      }
    case _: Type.KindedVar => throw InternalCompilerException("Unexpected kinded type variable.")
  }

  /**
    * Performs kinding on the given type under the given kind environment.
    */
  private def visitScheme(sc: ResolvedAst.Scheme, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Scheme, KindError] = sc match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      for {
        quantifiers <- Validation.traverse(quantifiers0)(visitTypeVar(_, Kind.Wild, kenv))
        constraints <- Validation.traverse(constraints0)(visitTypeConstraint(_, kenv, taenv, root))
        base <- visitType(base0, Kind.Star, kenv, taenv, root)
      } yield Scheme(quantifiers, constraints, base)
  }

  /**
    * Performs kinding on the given type constraint under the given kind environment.
    */
  private def visitTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val classKind = getClassKind(root.classes(clazz))
      mapN(visitType(tpe0, classKind, kenv, taenv, root)) {
        tpe => Ast.TypeConstraint(clazz, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type constructor under the given kind environment.
    */
  private def visitTypeConstructor(tycon: TypeConstructor, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[TypeConstructor, KindError] = tycon match {
    case TypeConstructor.UnkindedEnum(sym) =>
      // Lookup the enum kind
      val kind = getEnumKind(root.enums(sym))
      TypeConstructor.KindedEnum(sym, kind).toSuccess
    case _: TypeConstructor.KindedEnum => throw InternalCompilerException("Unexpected kinded enum.")
    case _: TypeConstructor.UnappliedAlias => throw InternalCompilerException("Unexpected unapplied type alias.")
    case t => t.toSuccess
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv): Validation[KindedAst.TypeParam, KindError] = tparam match {
    case ResolvedAst.TypeParam.Kinded(name, tpe0, _, loc) =>
      mapN(visitTypeVar(tpe0, Kind.Wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
    case ResolvedAst.TypeParam.Unkinded(name, tpe0, loc) =>
      mapN(visitTypeVar(tpe0, Kind.Wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      mapN(visitType(tpe0, Kind.Star, kenv, taenv, root)) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given annotation under the given kind environment.
    */
  private def visitAnnotation(ann: ResolvedAst.Annotation, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      mapN(Validation.traverse(exps0)(visitExp(_, kenv, taenv, root))) {
        exps => KindedAst.Annotation(name, exps, loc)
      }
  }

  /**
    * Infers a kind environment from the given spec.
    * A KindEnvironment is provided in case some subset of of kinds have been declared (and therefore should not be inferred),
    * as in the case of a class type parameter used in a sig or law.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, _, eff, _) =>
      val fparamKenvVal = Validation.fold(fparams, KindEnv.empty) {
        case (acc, fparam) => flatMapN(inferFormalParam(fparam, kenv, taenv, root)) {
          fparamKenv => acc ++ fparamKenv
        }
      }
      val schemeKenvVal = inferScheme(sc, kenv, taenv, root)
      val effKenvVal = inferType(eff, Kind.Bool, kenv, taenv, root)

      flatMapN(fparamKenvVal, schemeKenvVal, effKenvVal) {
        case (fparamKenv, schemeKenv, effKenv) => KindEnv.merge(fparamKenv, schemeKenv, effKenv, kenv)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => inferType(tpe0, Kind.Star, kenv, taenv, root)
  }

  /**
    * Infers a kind environment from the given scheme.
    */
  private def inferScheme(sc0: ResolvedAst.Scheme, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = sc0 match {
    case ResolvedAst.Scheme(_, constraints, base) =>
      val baseKenvVal = inferType(base, Kind.Star, kenv, taenv, root)
      val tconstrsKenvsVal = Validation.traverse(constraints)(inferTconstr(_, kenv, taenv, root))

      Validation.flatMapN(baseKenvVal, tconstrsKenvsVal) {
        case (baseKenv, tconstrKenvs) => Validation.fold(tconstrKenvs, baseKenv)(_ ++ _)
      }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTconstr(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe, loc) =>
      val kind = getClassKind(root.classes(clazz))
      inferType(tpe, kind, kenv: KindEnv, taenv, root)
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    * The inference is roughly analogous to the inference of types for expressions.
    * The primary differences are:
    * - There are no kind variables; kinds that cannot be determined are instead marked with [[Kind.Wild]].
    * - Subkinding may allow a variable to be ascribed with two different kinds; the most specific is used in the returned environment.
    */
  private def inferType(tpe: Type, expectedKind: Kind, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: Type.UnkindedVar =>
      val kind = kenv0.map.get(tvar) match {
        // Case 1.1: the type is not in the kenv: guess that it is Star -> Star -> ... -> ???.
        case None =>
          tpe.typeArguments.foldLeft(expectedKind) {
            case (acc, _) => Kind.Star ->: acc
          }
        // Case 1.2: the type is in the kenv: use it.
        case Some(k) => k
      }

      Validation.fold(tpe.typeArguments, KindEnv.singleton(tvar -> kind)) {
        case (acc, targ) => flatMapN(inferType(targ, Kind.Star, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case Type.Cst(cst, loc) =>
      val tyconKind = getTyconKind(cst, taenv, root)
      val args = Kind.kindArgs(tyconKind)

      Validation.fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case Type.Ascribe(t, k, loc) => inferType(t, k, kenv0, taenv, root)

    case Type.Alias(cst, args, tpe, loc) =>
      val alias = taenv(cst.sym)
      val tparamKinds = alias.tparams.map(_.tpe.kind)

      Validation.fold(args.zip(tparamKinds), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case _: Type.KindedVar => throw InternalCompilerException("Unexpected kinded var.")
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
        case tparams: ResolvedAst.TypeParams.Kinded => getKindEnvFromKindedTypeParams(tparams).toSuccess
        case _: ResolvedAst.TypeParams.Unkinded => inferSpec(spec0, kenv, taenv, root)
      }
  }

  /**
    * Gets a kind environment from the kinded type params.
    */
  private def getKindEnvFromKindedTypeParams(tparams0: ResolvedAst.TypeParams.Kinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[Type.UnkindedVar, Kind]) {
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
      val map = tparams.foldLeft(Map.empty[Type.UnkindedVar, Kind]) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc + (tpe -> Kind.Star)
      }
      KindEnv(map)
  }

  /**
    * Gets the kind of the enum.
    */
  private def getEnumKind(enum: ResolvedAst.Enum)(implicit flix: Flix): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _, _) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.tpe) ->: acc
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
  private def getTyconKind(tycon: TypeConstructor, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Kind = tycon match {
    case TypeConstructor.UnkindedEnum(sym) => getEnumKind(root.enums(sym))
    case _: TypeConstructor.KindedEnum => throw InternalCompilerException("Unexpected kinded enum.")
    case _: TypeConstructor.UnappliedAlias => throw InternalCompilerException("Unexpected unapplied type alias.")
    case t => t.kind
  }

  /**
    * Asserts that the type variable is unkinded.
    */
  private def assertUnkindedVar(tvar: Type.Var): Type.UnkindedVar = tvar match {
    case unkinded: Type.UnkindedVar => unkinded
    case kinded: Type.KindedVar => throw InternalCompilerException("Unexpected kinded type variable.")
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
    def singleton(pair: (Type.UnkindedVar, Kind)): KindEnv = KindEnv(Map(pair))

    /**
      * Merges all the given kind environments.
      */
    def merge(kenvs: KindEnv*): Validation[KindEnv, KindError] = {
      Validation.fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
    }
  }

  /**
    * Unifies the kinds, returning the most specific kind if possible.
    */
  private def unify(k1: Kind, k2: Kind): Option[Kind] = (k1, k2) match {
    case (Kind.Wild, k) => Some(k)
    case (k, Kind.Wild) => Some(k)
    case (Kind.Arrow(k11, k12), Kind.Arrow(k21, k22)) =>
      for {
        kind1 <- unify(k11, k21)
        kind2 <- unify(k12, k22)
      } yield Kind.Arrow(kind1, kind2)
    case (kind1, kind2) if kind1 == kind2 => Some(kind1)
    case _ => None
  }

  private case class KindEnv(map: Map[Type.UnkindedVar, Kind]) {
    /**
      * Adds the given mapping to the kind environment.
      */
    def +(pair: (Type.UnkindedVar, Kind)): Validation[KindEnv, KindError] = pair match {
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
