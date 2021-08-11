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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse}

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
object Kinder extends Phase[ResolvedAst.Root, KindedAst.Root] {

  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationError] = flix.phase("Kinder") {
    val enumsVal = Validation.traverse(root.enums) {
      case (sym, enum) => visitEnum(enum, root).map((sym, _))
    }

    val classesVal = Validation.traverse(root.classes) {
      case (sym, clazz) => visitClass(clazz, root).map((sym, _))
    }

    val defsVal = Validation.traverse(root.defs) {
      case (sym, defn) => visitDef(defn, KindEnv.empty, root).map((sym, _))
    }

    val instancesVal = Validation.traverse(root.instances) {
      case (sym, insts0) => traverse(insts0)(visitInstance(_, root)).map((sym, _))
    }

    val typeAliasesVal = Validation.traverseX(root.typealiases) {
      case (_, typeAlias) => visitTypeAlias(typeAlias, root)
    }

    mapN(enumsVal, classesVal, defsVal, instancesVal, typeAliasesVal) {
      case (enums, classes, defs, instances, _) =>
        KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, root.reachable, root.sources)
    }

  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases0, tpeDeprecated0, sc0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val casesVal = Validation.traverse(cases0) {
        case (tag, case0) => mapN(visitCase(case0, kenv, root)) {
          caze => (tag, caze)
        }
      }
      val tpeDeprecatedVal = visitType(tpeDeprecated0, KindMatch.subKindOf(Kind.Star), kenv, root)
      val scVal = visitScheme(sc0, kenv, root)

      mapN(tparamsVal, casesVal, tpeDeprecatedVal, scVal) {
        case (tparams, cases, tpeDeprecated, sc) => KindedAst.Enum(doc, mod, sym, tparams, cases.toMap, tpeDeprecated, sc, loc)
      }
  }

  /**
    * Performs kinding on the given type alias.
    * Returns Unit since type aliases are not carried through to the next phase.
    */
  private def visitTypeAlias(alias: ResolvedAst.TypeAlias, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Unit, KindError] = alias match {
    case ResolvedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val tpeVal = visitType(tpe0, KindMatch.Wild, kenv, root)

      mapN(tparamsVal, tpeVal) {
        case (_, _) => ()
      }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitCase(caze0: ResolvedAst.Case, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated0, sc0) =>
      for {
        tpeDeprecated <- visitType(tpeDeprecated0, KindMatch.subKindOf(Kind.Star), kenv, root)
        sc <- visitScheme(sc0, kenv, root)
      } yield KindedAst.Case(enum, tag, tpeDeprecated, sc)
  }

  /**
    * Performs kinding on the given type class.
    */
  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, KindError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParamDefaultStar(tparam0)

      val tparamVal = visitTypeParam(tparam0, kenv)
      val superClassesVal = Validation.traverse(superClasses0)(visitTypeConstraint(_, kenv, root))
      val sigsVal = Validation.traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, kenv, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, kenv, root))

      mapN(tparamVal, superClassesVal, sigsVal, lawsVal) {
        case (tparam, superClasses, sigs, laws) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe0, tconstrs0, defs0, ns, loc) =>
      val kind = getClassKind(root.classes(sym))
      for {
        kenv <- inferType(tpe0, KindMatch.subKindOf(kind), root)
        tpeVal = visitType(tpe0, KindMatch.subKindOf(kind), kenv, root)
        tconstrsVal = Validation.traverse(tconstrs0)(visitTypeConstraint(_, kenv, root))
        defsVal = Validation.traverse(defs0)(visitDef(_, kenv, root))
        result <- Validation.sequenceT(tpeVal, tconstrsVal, defsVal)
        (tpe, tconstrs, defs) = result
      } yield KindedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
  }

  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Def, kenv0: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      for {
        specKenv <- getKindEnvFromSpec(spec0, root)
        kenv <- specKenv.refinedBy(kenv0)
        spec <- visitSpec(spec0, kenv, root)
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Def(sym, spec, exp)
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Sig, kenv0: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      for {
        specKenv <- getKindEnvFromSpec(spec0, root)
        kenv <- specKenv.refinedBy(kenv0)
        spec <- visitSpec(spec0, kenv, root)
        exp <- Validation.traverse(exp0)(visitExpression(_, kenv, root))
      } yield KindedAst.Sig(sym, spec, exp.headOption)
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    */
  private def visitSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, tpe0, eff0, loc) =>
      val annVal = Validation.traverse(ann0)(visitAnnotation(_, kenv, root))
      val tparamsVal = Validation.traverse(tparams0.tparams)(visitTypeParam(_, kenv))
      val fparamsVal = Validation.traverse(fparams0)(visitFormalParam(_, kenv, root))
      val tpeVal = visitType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)
      val effVal = visitType(eff0, KindMatch.subKindOf(Kind.Bool), kenv, root)
      val scVal = visitScheme(sc0, kenv, root)
      for {
        result <- Validation.sequenceT(annVal, tparamsVal, fparamsVal, tpeVal, effVal)
        (ann, tparams, fparams, tpe, eff) = result
        sc <- scVal // ascribe the scheme separately
      } yield KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc)
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExpression(exp00: ResolvedAst.Expression, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {

    case ResolvedAst.Expression.Wild(loc) => KindedAst.Expression.Wild(Type.freshVar(Kind.Star), loc).toSuccess

    case ResolvedAst.Expression.Var(sym, tpe0, loc) =>
      mapN(visitType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)) {
        tpe => KindedAst.Expression.Var(sym, tpe, loc)
      }

    case ResolvedAst.Expression.Def(sym, loc) => KindedAst.Expression.Def(sym, Type.freshVar(Kind.Star), loc).toSuccess

    case ResolvedAst.Expression.Sig(sym, loc) => KindedAst.Expression.Sig(sym, Type.freshVar(Kind.Star), loc).toSuccess

    case ResolvedAst.Expression.Hole(sym, loc) => KindedAst.Expression.Hole(sym, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc).toSuccess

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

    case ResolvedAst.Expression.Default(loc) => KindedAst.Expression.Default(Type.freshVar(Kind.Star), loc).toSuccess

    case ResolvedAst.Expression.Apply(exp0, exps0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        exps <- Validation.traverse(exps0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.Apply(exp, exps, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc)

    case ResolvedAst.Expression.Lambda(fparam0, exp0, loc) =>
      for {
        fparam <- visitFormalParam(fparam0, kenv, root)
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Lambda(fparam, exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Unary(sop, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Unary(sop, exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Binary(sop, exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Binary(sop, exp1, exp2, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
        exp3 <- visitExpression(exp30, kenv, root)
      } yield KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)

    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Stm(exp1, exp2, loc)

    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)

    case ResolvedAst.Expression.LetRegion(sym, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.LetRegion(sym, exp, Type.freshVar(Kind.Bool), loc)

    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        rules <- traverse(rules0)(visitMatchRule(_, kenv, root))
      } yield KindedAst.Expression.Match(exp, rules, loc)

    case ResolvedAst.Expression.Choose(star, exps0, rules0, loc) =>
      for {
        exps <- Validation.traverse(exps0)(visitExpression(_, kenv, root))
        rules <- Validation.traverse(rules0)(visitChoiceRule(_, kenv, root))
      } yield KindedAst.Expression.Choose(star, exps, rules, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Tag(sym, tag, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Tag(sym, tag, exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.Tuple(elms, loc)

    case ResolvedAst.Expression.RecordEmpty(loc) => KindedAst.Expression.RecordEmpty(Type.freshVar(Kind.Record), loc).toSuccess

    case ResolvedAst.Expression.RecordSelect(exp0, field, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.RecordSelect(exp, field, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, loc) =>
      // Ideally, if `rest` is not of record kind, we should throw a kind error.
      // But because we have subkinding, we can't do this in the Kinder.
      // Consider: { +name = 5 | id({}) }
      // This is OK, but would be seen as a kind error since id is `(a: *) -> (a: *)`, so `id({}) :: *`
      // This KindError will be caught later in the Typer
      for {
        value <- visitExpression(value0, kenv, root)
        rest <- visitExpression(rest0, kenv, root)
      } yield KindedAst.Expression.RecordExtend(field, value, rest, Type.freshVar(Kind.Record), loc)

    case ResolvedAst.Expression.RecordRestrict(field, rest0, loc) =>
      for {
        rest <- visitExpression(rest0, kenv, root)
      } yield KindedAst.Expression.RecordRestrict(field, rest, Type.freshVar(Kind.Record), loc)

    case ResolvedAst.Expression.ArrayLit(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.ArrayLit(elms, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.ArrayNew(elm0, len0, loc) =>
      for {
        elm <- visitExpression(elm0, kenv, root)
        len <- visitExpression(len0, kenv, root)
      } yield KindedAst.Expression.ArrayNew(elm, len, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.ArrayLoad(base0, index0, loc) =>
      for {
        base <- visitExpression(base0, kenv, root)
        index <- visitExpression(index0, kenv, root)
      } yield KindedAst.Expression.ArrayLoad(base, index, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      for {
        base <- visitExpression(base0, kenv, root)
        index <- visitExpression(index0, kenv, root)
        elm <- visitExpression(elm0, kenv, root)
      } yield KindedAst.Expression.ArrayStore(base, index, elm, loc)

    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      for {
        base <- visitExpression(base0, kenv, root)
      } yield KindedAst.Expression.ArrayLength(base, loc)

    case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
      for {
        base <- visitExpression(base0, kenv, root)
        beginIndex <- visitExpression(beginIndex0, kenv, root)
        endIndex <- visitExpression(endIndex0, kenv, root)
      } yield KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)

    case ResolvedAst.Expression.Ref(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Ref(exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.RefWithRegion(exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.RefWithRegion(exp1, exp2, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc)

    case ResolvedAst.Expression.Deref(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Deref(exp, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc)

    case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Assign(exp1, exp2, Type.freshVar(Kind.Bool), loc)

    case ResolvedAst.Expression.Existential(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      for {
        fparamKenv <- inferFormalParam(fparam0, root)
        kenv1 <- kenv ++ fparamKenv
        fparam <- visitFormalParam(fparam0, kenv1, root)
        exp <- visitExpression(exp0, kenv1, root)
      } yield KindedAst.Expression.Existential(fparam, exp, loc)

    case ResolvedAst.Expression.Universal(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      for {
        fparamKenv <- inferFormalParam(fparam0, root)
        kenv1 <- kenv ++ fparamKenv
        fparam <- visitFormalParam(fparam0, kenv1, root)
        exp <- visitExpression(exp0, kenv1, root)
      } yield KindedAst.Expression.Universal(fparam, exp, loc)

    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        expectedType <- Validation.traverse(expectedType0)(visitType(_, KindMatch.subKindOf(Kind.Star), kenv, root))
        expectedEff <- Validation.traverse(expectedEff0)(visitType(_, KindMatch.subKindOf(Kind.Bool), kenv, root))
      } yield KindedAst.Expression.Ascribe(exp, expectedType.headOption, expectedEff.headOption, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        declaredType <- Validation.traverse(declaredType0)(visitType(_, KindMatch.subKindOf(Kind.Star), kenv, root))
        declaredEff <- Validation.traverse(declaredEff0)(visitType(_, KindMatch.subKindOf(Kind.Bool), kenv, root))
      } yield KindedAst.Expression.Cast(exp, declaredType.headOption, declaredEff.headOption, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        rules <- Validation.traverse(rules0)(visitCatchRule(_, kenv, root))
      } yield KindedAst.Expression.TryCatch(exp, rules, loc)

    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeConstructor(constructor, args, loc)

    case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        args <- Validation.traverse(args0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeMethod(method, exp, args, loc)

    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeStaticMethod(method, args, loc)

    case ResolvedAst.Expression.GetField(field, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.GetField(field, exp, loc)

    case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.PutField(field, exp1, exp2, loc)

    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess

    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.PutStaticField(field, exp, loc)

    case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
        tpe <- visitType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)
      } yield KindedAst.Expression.NewChannel(exp, tpe, loc)

    case ResolvedAst.Expression.GetChannel(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.GetChannel(exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.PutChannel(exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.PutChannel(exp1, exp2, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.SelectChannel(rules0, default0, loc) =>
      for {
        rules <- Validation.traverse(rules0)(visitSelectChannelRule(_, kenv, root))
        default <- Validation.traverse(default0)(visitExpression(_, kenv, root))
      } yield KindedAst.Expression.SelectChannel(rules, default.headOption, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.Spawn(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Spawn(exp, loc)

    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Lazy(exp, loc)

    case ResolvedAst.Expression.Force(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Force(exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.FixpointConstraintSet(cs0, loc) =>
      for {
        cs <- Validation.traverse(cs0)(visitConstraint(_, kenv, root))
      } yield KindedAst.Expression.FixpointConstraintSet(cs, Type.freshVar(Kind.Schema), loc)

    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.FixpointMerge(exp1, exp2, loc)

    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointSolve(exp, loc)

    case ResolvedAst.Expression.FixpointFilter(pred, exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointFilter(pred, exp, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointProjectIn(exp, pred, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
      } yield KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, Type.freshVar(Kind.Star), loc)

    case ResolvedAst.Expression.MatchEff(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- visitExpression(exp10, kenv, root)
        exp2 <- visitExpression(exp20, kenv, root)
        exp3 <- visitExpression(exp30, kenv, root)
      } yield KindedAst.Expression.MatchEff(exp1, exp2, exp3, loc)

  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      for {
        pat <- visitPattern(pat0, kenv, root)
        guard <- visitExpression(guard0, kenv, root)
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Performs kinding on the given choice rule under the given kind environment.
    */
  private def visitChoiceRule(rule0: ResolvedAst.ChoiceRule, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoiceRule, KindError] = rule0 match {
    case ResolvedAst.ChoiceRule(pat0, exp0) =>
      for {
        pat <- Validation.traverse(pat0)(visitChoicePattern(_, kenv, root))
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.ChoiceRule(pat, exp)
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.CatchRule(sym, clazz, exp)
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      for {
        chan <- visitExpression(chan0, kenv, root)
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.SelectChannelRule(sym, chan, exp)
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def visitPattern(pat0: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Pattern, KindError] = pat0 match {
    case ResolvedAst.Pattern.Wild(loc) => KindedAst.Pattern.Wild(Type.freshVar(Kind.Star), loc).toSuccess
    case ResolvedAst.Pattern.Var(sym, loc) => KindedAst.Pattern.Var(sym, Type.freshVar(Kind.Star), loc).toSuccess
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
      } yield KindedAst.Pattern.Tag(sym, tag, pat, Type.freshVar(Kind.Star), loc)
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.Tuple(elms, loc)
    case ResolvedAst.Pattern.Array(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.Array(elms, Type.freshVar(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayTailSpread(elms, sym, Type.freshVar(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayHeadSpread(sym, elms, Type.freshVar(Kind.Star), loc)
  }

  /**
    * Performs kinding on the given choice pattern under the given kind environment.
    */
  private def visitChoicePattern(pat0: ResolvedAst.ChoicePattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoicePattern, KindError] = pat0 match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.ChoicePattern.Present(sym, loc) => KindedAst.ChoicePattern.Present(sym, Type.freshVar(Kind.Star), loc).toSuccess
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      for {
        cparams <- Validation.traverse(cparams0)(visitConstraintParam(_, kenv, root))
        head <- visitHeadPredicate(head0, kenv, root)
        body <- Validation.traverse(body0)(visitBodyPredicate(_, kenv, root))
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
  private def visitHeadPredicate(pred: ResolvedAst.Predicate.Head, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      for {
        terms <- Validation.traverse(terms0)(visitExpression(_, kenv, root))
      } yield KindedAst.Predicate.Head.Atom(pred, den, terms, Type.freshVar(Kind.Star), loc)
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred: ResolvedAst.Predicate.Body, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, loc) =>
      for {
        terms <- Validation.traverse(terms0)(visitPattern(_, kenv, root))
      } yield KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, Type.freshVar(Kind.Star), loc)
    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      for {
        exp <- visitExpression(exp0, kenv, root)
      } yield KindedAst.Predicate.Body.Guard(exp, loc)
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `kindMatch` expected from context.
    */
  private def visitTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch, kenv: KindEnv): Validation[Type.Var, KindError] = tvar match {
    case tvar@UnkindedType.Var(id, text, loc) =>
      kenv.map.get(tvar) match {
        // Case 1: we don't know about this kind, just ascribe it with what the context expects
        case None => tvar.ascribedWith(kindMatch.kind).toSuccess
        // Case 2: we know about this kind, make sure it's behaving as we expect
        case Some(actualKind) =>
          if (kindMatch.matches(actualKind)) {
            Type.Var(id, actualKind, text = text).toSuccess
          } else {
            val expectedKind = kindMatch.kind
            KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc).toFailure
          }
      }

  }

  /**
    * Performs kinding on the given free type variable, with `kindMatch` expected from context.
    */
  private def visitFreeTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch): Type.Var = tvar match {
    case UnkindedType.Var(id, text, loc) =>
      val kind = kindMatch.kind
      Type.Var(id, kind, text = text)
  }

  /**
    * Performs kinding on the given type under the given kind environment, with `expectedKind` expected from context.
    * This is roughly analogous to the reassembly of expressions under a type environment, except that:
    * - Kind errors may be discovered here as they may not have been found during inference (or inference may not have happened at all).
    */
  private def visitType(tpe0: UnkindedType, expectedKind: KindMatch, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => visitTypeVar(tvar, expectedKind, kenv)
    case UnkindedType.Cst(cst, loc) =>
      val tycon = visitTypeConstructor(cst, root)
      val kind = tycon.kind
      if (expectedKind.matches(kind)) {
        Type.Cst(tycon, loc).toSuccess
      } else {
        KindError.UnexpectedKind(expectedKind = expectedKind.kind, actualKind = kind, loc).toFailure
      }
    case UnkindedType.Apply(t10, t20) =>
      for {
        t2 <- visitType(t20, KindMatch.Wild, kenv, root)
        k1 = KindMatch.subKindOf(Kind.Arrow(t2.kind, expectedKind.kind))
        t1 <- visitType(t10, k1, kenv, root)
      } yield Type.Apply(t1, t2)
    case UnkindedType.Lambda(t10, t20) =>
      expectedKind match {
        case KindMatch(_, Kind.Arrow(expK1, expK2)) =>
          val t1 = visitFreeTypeVar(t10, KindMatch.subKindOf(expK1))
          for {
            newKenv <- kenv + (t10 -> t1.kind)
            t2 <- visitType(t20, KindMatch.subKindOf(expK2), newKenv, root)
          } yield Type.Lambda(t1, t2)
        case KindMatch(_, Kind.Wild) =>
          val t1 = visitFreeTypeVar(t10, KindMatch.Wild)
          for {
            newKenv <- kenv + (t10 -> t1.kind)
            t2 <- visitType(t20, KindMatch.Wild, newKenv, root)
          } yield Type.Lambda(t1, t2)
        case _ =>
          KindError.UnexpectedKind(expectedKind = expectedKind.kind, actualKind = Kind.Wild ->: Kind.Wild,  loc = t10.loc).toFailure
      }
    case UnkindedType.Ascribe(t, k, loc) =>
      if (expectedKind.matches(k)) {
        visitType(t, KindMatch.subKindOf(k), kenv, root)
      } else {
        KindError.UnexpectedKind(expectedKind = expectedKind.kind, actualKind = k, loc).toFailure
      }
  }

  /**
    * Performs kinding on the given type under the given kind environment.
    */
  private def visitScheme(sc: ResolvedAst.Scheme, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Scheme, KindError] = sc match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      for {
        quantifiers <- Validation.traverse(quantifiers0)(visitTypeVar(_, KindMatch.Wild, kenv))
        constraints <- Validation.traverse(constraints0)(visitTypeConstraint(_, kenv, root))
        base <- visitType(base0, KindMatch.subKindOf(Kind.Star), kenv, root)
      } yield Scheme(quantifiers, constraints, base)
  }

  /**
    * Performs kinding on the given type constraint under the given kind environment.
    */
  private def visitTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val classKind = getClassKind(root.classes(clazz))
      mapN(visitType(tpe0, KindMatch.subKindOf(classKind), kenv, root)) {
        tpe => Ast.TypeConstraint(clazz, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type constructor under the given kind environment.
    */
  private def visitTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root)(implicit flix: Flix): TypeConstructor = tycon match {
    case UnkindedType.Constructor.Unit => TypeConstructor.Unit
    case UnkindedType.Constructor.Null => TypeConstructor.Null
    case UnkindedType.Constructor.Bool => TypeConstructor.Bool
    case UnkindedType.Constructor.Char => TypeConstructor.Char
    case UnkindedType.Constructor.Float32 => TypeConstructor.Float32
    case UnkindedType.Constructor.Float64 => TypeConstructor.Float64
    case UnkindedType.Constructor.Int8 => TypeConstructor.Int8
    case UnkindedType.Constructor.Int16 => TypeConstructor.Int16
    case UnkindedType.Constructor.Int32 => TypeConstructor.Int32
    case UnkindedType.Constructor.Int64 => TypeConstructor.Int64
    case UnkindedType.Constructor.BigInt => TypeConstructor.BigInt
    case UnkindedType.Constructor.Str => TypeConstructor.Str
    case UnkindedType.Constructor.Arrow(arity) => TypeConstructor.Arrow(arity)
    case UnkindedType.Constructor.RecordEmpty => TypeConstructor.RecordEmpty
    case UnkindedType.Constructor.RecordExtend(field) => TypeConstructor.RecordExtend(field)
    case UnkindedType.Constructor.SchemaEmpty => TypeConstructor.SchemaEmpty
    case UnkindedType.Constructor.SchemaExtend(pred) => TypeConstructor.SchemaExtend(pred)
    case UnkindedType.Constructor.Array => TypeConstructor.Array
    case UnkindedType.Constructor.Channel => TypeConstructor.Channel
    case UnkindedType.Constructor.Lazy => TypeConstructor.Lazy
    case UnkindedType.Constructor.Tag(sym, tag) => TypeConstructor.Tag(sym, tag)
    case UnkindedType.Constructor.Enum(sym) =>
      // Lookup the enum kind
      val kind = getEnumKind(root.enums(sym))
      TypeConstructor.Enum(sym, kind)
    case UnkindedType.Constructor.Native(clazz) => TypeConstructor.Native(clazz)
    case UnkindedType.Constructor.ScopedRef => TypeConstructor.ScopedRef
    case UnkindedType.Constructor.Tuple(l) => TypeConstructor.Tuple(l)
    case UnkindedType.Constructor.Relation => TypeConstructor.Relation
    case UnkindedType.Constructor.Lattice => TypeConstructor.Lattice
    case UnkindedType.Constructor.True => TypeConstructor.True
    case UnkindedType.Constructor.False => TypeConstructor.False
    case UnkindedType.Constructor.Not => TypeConstructor.Not
    case UnkindedType.Constructor.And => TypeConstructor.And
    case UnkindedType.Constructor.Or => TypeConstructor.Or
    case UnkindedType.Constructor.Region => TypeConstructor.Region
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv): Validation[KindedAst.TypeParam, KindError] = tparam match {
    case ResolvedAst.TypeParam.Kinded(name, tpe0, _, loc) =>
      mapN(visitTypeVar(tpe0, KindMatch.Wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
    case ResolvedAst.TypeParam.Unkinded(name, tpe0, loc) =>
      mapN(visitTypeVar(tpe0, KindMatch.Wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      mapN(visitType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given annotation under the given kind environment.
    */
  private def visitAnnotation(ann: ResolvedAst.Annotation, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      mapN(Validation.traverse(exps0)(visitExpression(_, kenv, root))) {
        exps => KindedAst.Annotation(name, exps, loc)
      }
  }

  /**
    * Infers a kind environment from the given spec.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, _, eff, _) =>
      val fparamKenvVal = Validation.fold(fparams, KindEnv.empty) {
        case (acc, fparam) => flatMapN(inferFormalParam(fparam, root)) {
          fparamKenv => acc ++ fparamKenv
        }
      }
      val schemeKenvVal = inferScheme(sc, root)
      val effKenvVal = inferType(eff, KindMatch.subKindOf(Kind.Bool), root)

      flatMapN(fparamKenvVal, schemeKenvVal, effKenvVal) {
        case (fparamKenv, schemeKenv, effKenv) => KindEnv.merge(fparamKenv, schemeKenv, effKenv)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => inferType(tpe0, KindMatch.subKindOf(Kind.Star), root)
  }

  /**
    * Infers a kind environment from the given scheme.
    */
  private def inferScheme(sc0: ResolvedAst.Scheme, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = sc0 match {
    case ResolvedAst.Scheme(_, constraints, base) =>
      val baseKenvVal = inferType(base, KindMatch.subKindOf(Kind.Star), root)
      val tconstrsKenvsVal = Validation.traverse(constraints)(inferTconstr(_, root))

      Validation.flatMapN(baseKenvVal, tconstrsKenvsVal) {
        case (baseKenv, tconstrKenvs) => Validation.fold(tconstrKenvs, baseKenv)(_ ++ _)
      }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTconstr(tconstr: ResolvedAst.TypeConstraint, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe, loc) =>
      val kind = getClassKind(root.classes(clazz))
      inferType(tpe, KindMatch.subKindOf(kind), root)
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    * The inference is roughly analogous to the inference of types for expressions.
    * The primary differences are:
    * - There are no kind variables; kinds that cannot be determined are instead marked with [[Kind.Wild]].
    * - Subkinding may allow a variable to be ascribed with two different kinds; the most specific is used in the returned environment.
    */
  private def inferType(tpe: UnkindedType, expectedType: KindMatch, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: UnkindedType.Var =>
      val kind = tpe.typeArguments.foldLeft(expectedType.kind) {
        case (acc, _) => Kind.Star ->: acc
      }

      Validation.fold(tpe.typeArguments, KindEnv.singleton(tvar -> kind)) {
        case (acc, targ) => flatMapN(inferType(targ, KindMatch.subKindOf(Kind.Star), root)) {
          kenv => acc ++ kenv
        }
      }
    case UnkindedType.Cst(cst, loc) =>
      val tyconKind = getTyconKind(cst, root)
      val args = Kind.kindArgs(tyconKind)

      Validation.fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, KindMatch.subKindOf(kind), root)) {
          kenv => acc ++ kenv
        }
      }
    case UnkindedType.Lambda(t1, t2) =>
      val tyconKind = tpe.typeArguments.foldLeft(expectedType.kind) {
        case (acc, _) => Kind.Star ->: acc
      }

      tyconKind match {
        case Kind.Arrow(argKind, retKind) =>
          val argKenvVal = inferType(t1, KindMatch.superKindOf(argKind), root)
          val retKenvVal = inferType(t2, KindMatch.subKindOf(retKind), root)

          val args = Kind.kindArgs(tyconKind)
          val targsKenvVal = Validation.traverse(tpe.typeArguments.zip(args)) {
            case (targ, kind) => inferType(targ, KindMatch.subKindOf(kind), root)
          }
          flatMapN(argKenvVal, retKenvVal, targsKenvVal) {
            case (kenv1, kenv2, kenv3) => KindEnv.merge(kenv1 :: kenv2 :: kenv3: _*)
          }
        case _ => KindError.UnexpectedKind(actualKind = Kind.Wild ->: Kind.Wild, expectedKind = tyconKind, loc = t1.loc).toFailure
      }
    case UnkindedType.Ascribe(t, k, _) => inferType(t, KindMatch.subKindOf(k), root)
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
  private def getKindEnvFromSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _) =>
      tparams0 match {
        case tparams: ResolvedAst.TypeParams.Kinded => getKindEnvFromKindedTypeParams(tparams).toSuccess
        case _: ResolvedAst.TypeParams.Unkinded => inferSpec(spec0, root)
      }
  }

  /**
    * Gets a kind environment from the kinded type params.
    */
  private def getKindEnvFromKindedTypeParams(tparams0: ResolvedAst.TypeParams.Kinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
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
      val map = tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc + (tpe -> Kind.Star)
      }
      KindEnv(map)
  }

  /**
    * Gets the kind of the enum.
    */
  private def getEnumKind(enum: ResolvedAst.Enum)(implicit flix: Flix): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
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
  private def getTyconKind(tycon: UnkindedType.Constructor, root: ResolvedAst.Root)(implicit flix: Flix): Kind = tycon match {
    case UnkindedType.Constructor.Unit => Kind.Star
    case UnkindedType.Constructor.Null => Kind.Star
    case UnkindedType.Constructor.Bool => Kind.Star
    case UnkindedType.Constructor.Char => Kind.Star
    case UnkindedType.Constructor.Float32 => Kind.Star
    case UnkindedType.Constructor.Float64 => Kind.Star
    case UnkindedType.Constructor.Int8 => Kind.Star
    case UnkindedType.Constructor.Int16 => Kind.Star
    case UnkindedType.Constructor.Int32 => Kind.Star
    case UnkindedType.Constructor.Int64 => Kind.Star
    case UnkindedType.Constructor.BigInt => Kind.Star
    case UnkindedType.Constructor.Str => Kind.Star
    case UnkindedType.Constructor.Arrow(arity) => Kind.Bool ->: Kind.mkArrow(arity)
    case UnkindedType.Constructor.RecordEmpty => Kind.Record
    case UnkindedType.Constructor.RecordExtend(field) => Kind.Star ->: Kind.Record ->: Kind.Record
    case UnkindedType.Constructor.SchemaEmpty => Kind.Schema
    case UnkindedType.Constructor.SchemaExtend(pred) => Kind.Star ->: Kind.Schema ->: Kind.Schema
    case UnkindedType.Constructor.Array => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Channel => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Lazy => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Tag(sym, tag) => Kind.Star ->: Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Enum(sym) => getEnumKind(root.enums(sym))
    case UnkindedType.Constructor.Native(clazz) => Kind.Star
    case UnkindedType.Constructor.ScopedRef => Kind.Star ->: Kind.Bool ->: Kind.Star
    case UnkindedType.Constructor.Tuple(l) => Kind.mkArrow(l)
    case UnkindedType.Constructor.Relation => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Lattice => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.True => Kind.Bool
    case UnkindedType.Constructor.False => Kind.Bool
    case UnkindedType.Constructor.Not => Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.And => Kind.Bool ->: Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.Or => Kind.Bool ->: Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.Region => Kind.Bool ->: Kind.Star
  }

  /**
    * Describes a kind bound, a subkind or superkind of some kind.
    */
  private case class KindMatch(assoc: KindMatch.Association, kind: Kind) {
    /**
      * Returns true if the given kind obeys the kind boudn described by this KindMatch.
      */
    def matches(other: Kind): Boolean = assoc match {
      case KindMatch.Association.SubKind => other <:: kind
      case KindMatch.Association.SuperKind => kind <:: other
    }
  }

  private object KindMatch {

    /**
      * A KindMatch that matches every kind.
      */
    // association doesn't matter
    val Wild: KindMatch = KindMatch(Association.SubKind, Kind.Wild)

    def subKindOf(kind: Kind): KindMatch = KindMatch(Association.SubKind, kind)

    def superKindOf(kind: Kind): KindMatch = KindMatch(Association.SuperKind, kind)

    /**
      * Describes whether the KindMatch is a lower or upper bound.
      */
    sealed trait Association

    object Association {
      case object SubKind extends Association

      case object SuperKind extends Association
    }
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
    def singleton(pair: (UnkindedType.Var, Kind)): KindEnv = KindEnv(Map(pair))

    /**
      * Merges all the given kind environments.
      */
    def merge(kenvs: KindEnv*): Validation[KindEnv, KindError] = {
      Validation.fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
    }
  }

  private case class KindEnv(map: Map[UnkindedType.Var, Kind]) {
    /**
      * Adds the given mapping to the kind environment.
      */
    def +(pair: (UnkindedType.Var, Kind)): Validation[KindEnv, KindError] = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) => Kind.min(kind0, kind) match {
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

    /**
      * Helper function for [[refinedBy]]
      */
    private def refinedBy1(pair: (UnkindedType.Var, Kind)): Validation[KindEnv, KindError] = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) =>
          if (kind <:: kind0) {
            KindEnv(map + (tvar -> kind)).toSuccess
          } else {
            KindError.MismatchedKinds(kind0, kind, tvar.loc).toFailure
          }
      }
    }

    /**
      * Merges the given kind environment into this kind environment,
      * where if a type variable is present in both environments,
      * the right environment's kind must be a subkind of the left environment's kind.
      */
    def refinedBy(other: KindEnv): Validation[KindEnv, KindError] = {
      Validation.fold(other.map, this) {
        case (acc, pair) => acc.refinedBy1(pair)
      }
    }
  }
}
