package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.CheckedCastType
import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, LevelEnv, RigidityEnv, Scheme, SourceLocation, Symbol, Type, TypeConstructor}

import scala.collection.mutable.ListBuffer

object ConstraintGeneration {

  sealed class Constraint

  object Constraint {
    case class Equality(tpe1: Type, tpe2: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint

    case class Class(sym: Symbol.ClassSym, tpe: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint
  }

  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    c.constrs.append(Constraint.Equality(tpe1, tpe2, c.renv, c.lenv, loc))
  }

  def unifyAllTypesM(tpes: List[Type], kind: Kind, loc: SourceLocation)(implicit c: Context, flix: Flix): Type = {
    tpes match {
      case tpe1 :: rest =>
        rest.foreach(unifyTypeM(tpe1, _, loc))
        tpe1
      case Nil => Type.freshVar(kind, loc.asSynthetic)
    }
  }

  // TODO ASSOC-TYPES this should actually do something
  def expectTypeM(expected: Type, actual: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    unifyTypeM(expected, actual, loc)
  }

  def addTypeConstraintsM(tconstrs0: List[Ast.TypeConstraint], loc: SourceLocation)(implicit c: Context): Unit = {
    val tconstrs = tconstrs0.map {
      case Ast.TypeConstraint(head, arg, _) => Constraint.Class(head.sym, arg, c.renv, c.lenv, loc)
    }
    c.constrs.addAll(tconstrs)
  }

  def rigidityM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.renv = c.renv.markRigid(sym)
  }

  def enterScopeM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.lenv.enterScope(sym)
  }

  def exitScopeM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.lenv.exitScope(sym)
  }

  case class Context(constrs: ListBuffer[Constraint], var renv: RigidityEnv, var lenv: LevelEnv)

  def visitExp(exp0: KindedAst.Expr)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = exp0 match {
    case Expr.Var(sym, loc) => (sym.tvar, Type.Pure)
    case Expr.Def(sym, tvar, loc) =>
      val defn = root.defs(sym)
      val (tconstrs, defTpe) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
      unifyTypeM(tvar, defTpe, loc)
      addTypeConstraintsM(tconstrs, loc)
      val resTpe = defTpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Sig(sym, tvar, loc) =>
      val sig = root.classes(sym.clazz).sigs(sym)
      val (tconstrs, sigTpe) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
      unifyTypeM(tvar, sigTpe, loc)
      addTypeConstraintsM(tconstrs, loc)
      val resTpe = sigTpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Hole(sym, tpe, loc) =>
      (tpe, Type.Pure)

    case Expr.HoleWithExp(exp, tvar, evar, loc) =>
      val (tpe, eff) = visitExp(exp)
      // effect type is AT LEAST the inner expression's effect
      val atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
      unifyTypeM(atLeastEff, evar, loc)
      // result type is whatever is needed for the hole
      val resTpe = tvar
      val resEff = atLeastEff
      (resTpe, resEff)

    case Expr.OpenAs(symUse, exp, tvar, loc) => ???

    case Expr.Use(sym, alias, exp, loc) =>
      visitExp(exp)

    case Expr.Cst(cst, loc) =>
      val resTpe = TypeReconstruction.constantType(cst)
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Apply(exp, exps, tpe, eff, loc) => ???

    case Expr.Lambda(fparam, exp, loc) => {
      unifyTypeM(fparam.sym.tvar, fparam.tpe, loc)
      val (tpe, eff) = visitExp(exp)
      val resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, tpe, loc)
      val resEff = Type.Pure
      (resTpe, resEff)
    }
    case Expr.Unary(sop, exp, tpe, loc) => ???
    case Expr.Binary(sop, exp1, exp2, tpe, loc) => ???

    case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      val (tpe3, eff3) = visitExp(exp3)
      expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
      unifyTypeM(tpe2, tpe3, loc)
      val resTpe = tpe3
      val resEff = Type.mkUnion(eff1, eff2, eff3, loc)
      (resTpe, resEff)

    case Expr.Stm(exp1, exp2, loc) =>
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      val resTpe = tpe2
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.Discard(exp, loc) =>
      val (tpe, eff) = visitExp(exp)
      val resTpe = Type.Unit
      val resEff = eff
      (resTpe, resEff)

    case Expr.Let(sym, mod, exp1, exp2, loc) =>
      val (tpe1, eff1) = visitExp(exp1)
      unifyTypeM(sym.tvar, tpe1, loc)
      val (tpe2, eff2) = visitExp(exp2)
      val resTpe = tpe2
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.LetRec(sym, mod, exp1, exp2, loc) =>
      // Ensure that `exp1` is a lambda.
      val a = Type.freshVar(Kind.Star, loc)
      val b = Type.freshVar(Kind.Star, loc)
      val p = Type.freshVar(Kind.Eff, loc)
      val expectedType = Type.mkArrowWithEffect(a, p, b, loc)
      val (tpe1, eff1) = visitExp(exp1)
      unifyTypeM(expectedType, tpe1, exp1.loc)
      unifyTypeM(sym.tvar, tpe1, exp1.loc)
      val (tpe2, eff2) = visitExp(exp2)
      val resTpe = tpe2
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.Region(tpe, loc) =>
      val resTpe = tpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Scope(sym, regionVar, exp, evar, loc) =>
      rigidityM(regionVar.sym)
      enterScopeM(regionVar.sym)
      unifyTypeM(sym.tvar, Type.mkRegion(regionVar, loc), loc)
      val (tpe, eff) = visitExp(exp)
      exitScopeM(regionVar.sym)
      unifyTypeM(evar, eff, loc)
      // TODO ASSOC-TYPES noEscapeM ?
      val resTpe = tpe
      val resEff = eff
      (resTpe, resEff)

    case Expr.ScopeExit(exp1, exp2, loc) =>
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val regionTpe = Type.mkRegion(regionVar, loc)
      val evar = Type.freshVar(Kind.Eff, loc)
      val (tpe1, _) = visitExp(exp1)
      val (tpe2, _) = visitExp(exp2)
      expectTypeM(expected = Type.mkArrowWithEffect(Type.Unit, evar, Type.Unit, loc.asSynthetic), actual = tpe1, exp1.loc)
      expectTypeM(expected = regionTpe, actual = tpe2, exp2.loc)
      val resTpe = Type.Unit
      val resEff = Type.mkUnion(Type.Impure, regionVar, loc)
      (resTpe, resEff)

    case Expr.Match(exp, rules, loc) => ???
    case Expr.TypeMatch(exp, rules, loc) => ???
    case Expr.RelationalChoose(star, exps, rules, tpe, loc) => ???
    case Expr.RestrictableChoose(star, exp, rules, tpe, loc) => ???
    case Expr.Tag(sym, exp, tpe, loc) => ???
    case Expr.RestrictableTag(sym, exp, isOpen, tpe, loc) => ???

    case Expr.Tuple(elms, loc) =>
      val (elmTpes, elmEffs) = elms.map(visitExp).unzip
      val resTpe = Type.mkTuple(elmTpes, loc)
      val resEff = Type.mkUnion(elmEffs, loc)
      (resTpe, resEff)

    case Expr.RecordEmpty(loc) =>
      val resTpe = Type.mkRecord(Type.RecordRowEmpty, loc)
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.RecordSelect(exp, label, tvar, loc) =>
      //
      // r : { label = tpe | row }
      // -------------------------
      //       r.label : tpe
      //
      val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
      val expectedRowType = Type.mkRecordRowExtend(label, tvar, freshRowVar, loc)
      val expectedRecordType = Type.mkRecord(expectedRowType, loc)
      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, expectedRecordType, loc)
      val resTpe = tvar
      val resEff = eff
      (resTpe, resEff)

    case Expr.RecordExtend(label, exp1, exp2, tvar, loc) =>
      //
      //       exp1 : tpe        exp2 : {| r }
      // ---------------------------------------------
      // { label = exp1 | exp2 } : { label  :: tpe | r }
      //
      val restRow = Type.freshVar(Kind.RecordRow, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
      unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(label, tpe1, restRow, loc), loc), loc)
      val resTpe = tvar
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)

    case Expr.RecordRestrict(label, exp, tvar, loc) =>
      //
      //  exp : { label  :: t | r }
      // -------------------------
      // { -label | exp } : {| r }
      //
      val freshLabelType = Type.freshVar(Kind.Star, loc)
      val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(label, freshLabelType, freshRowVar, loc), loc), loc)
      unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
      val resTpe = tvar
      val resEff = eff
      (resTpe, resEff)

    case Expr.ArrayLit(exps, exp, tvar, evar, loc) =>
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val regionType = Type.mkRegion(regionVar, loc)
      val (tpes, effs) = exps.map(visitExp).unzip
      val (tpe, eff) = visitExp(exp)
      expectTypeM(expected = regionType, actual = tpe, exp.loc)
      val elmTpe = unifyAllTypesM(tpes, Kind.Star, loc)
      unifyTypeM(tvar, Type.mkArray(elmTpe, regionVar, loc), loc)
      unifyTypeM(evar, Type.mkUnion(Type.mkUnion(effs, loc), eff, regionVar, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val regionType = Type.mkRegion(regionVar, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      val (tpe3, eff3) = visitExp(exp3)
      expectTypeM(expected = regionType, actual = tpe1, loc)
      expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
      unifyTypeM(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
      unifyTypeM(evar, Type.mkUnion(eff1, eff2, eff3, regionVar, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      expectTypeM(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
      expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
      unifyTypeM(evar, Type.mkUnion(regionVar, eff1, eff2, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.ArrayStore(exp1, exp2, exp3, evar, loc) =>
      val elmVar = Type.freshVar(Kind.Star, loc)
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val arrayType = Type.mkArray(elmVar, regionVar, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      val (tpe3, eff3) = visitExp(exp3)
      expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
      expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
      expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
      unifyTypeM(evar, Type.mkUnion(List(regionVar, eff1, eff2, eff3), loc), loc)
      val resTpe = Type.Unit
      val resEff = evar
      (resTpe, resEff)

    case Expr.ArrayLength(exp, loc) =>
      val elmVar = Type.freshVar(Kind.Star, loc)
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val (tpe, eff) = visitExp(exp)
      expectTypeM(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
      //        unbindVar(elmVar)
      //        unbindVar(regionVar)
      // TODO ASSOC-TYPES is there an unbind equivalent?
      val resTpe = Type.Int32
      val resEff = eff
      (resTpe, resEff)

    case Expr.VectorLit(exps, tvar, evar, loc) =>
      val (tpes, effs) = exps.map(visitExp).unzip
      val tpe = unifyAllTypesM(tpes, Kind.Star, loc)
      unifyTypeM(tvar, Type.mkVector(tpe, loc), loc)
      unifyTypeM(evar, Type.mkUnion(effs, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.VectorLoad(exp1, exp2, tvar, evar, loc) =>
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      expectTypeM(expected = Type.mkVector(tvar, loc), actual = tpe1, exp1.loc)
      expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
      unifyTypeM(evar, Type.mkUnion(eff1, eff2, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.VectorLength(exp, loc) =>
      val elmVar = Type.freshVar(Kind.Star, loc)
      val (tpe, eff) = visitExp(exp)
      expectTypeM(Type.mkVector(elmVar, loc), tpe, exp.loc)
      //        _ <- unbindVar(elmVar) // TODO ASSOC-TYPES
      val resTpe = Type.Int32
      val resEff = eff
      (resTpe, resEff)

    case Expr.Ref(exp1, exp2, tvar, evar, loc) =>
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val regionType = Type.mkRegion(regionVar, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      expectTypeM(tpe2, regionType, exp2.loc)
      unifyTypeM(tvar, Type.mkRef(tpe1, regionVar, loc), loc)
      unifyTypeM(evar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.Deref(exp, tvar, evar, loc) =>
      val elmVar = Type.freshVar(Kind.Star, loc)
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val refType = Type.mkRef(elmVar, regionVar, loc)
      val (tpe, eff) = visitExp(exp)
      expectTypeM(expected = refType, actual = tpe, exp.loc)
      unifyTypeM(tvar, elmVar, loc)
      unifyTypeM(evar, Type.mkUnion(eff, regionVar, loc), loc)
      val resTpe = tvar
      val resEff = evar
      (resTpe, resEff)

    case Expr.Assign(exp1, exp2, evar, loc) =>
      val elmVar = Type.freshVar(Kind.Star, loc)
      val regionVar = Type.freshVar(Kind.Eff, loc)
      val refType = Type.mkRef(elmVar, regionVar, loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      expectTypeM(expected = refType, actual = tpe1, exp1.loc)
      expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
      unifyTypeM(evar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
      val resTpe = Type.Unit
      val resEff = evar
      (resTpe, resEff)

    case Expr.Ascribe(exp, expectedTpe, expectedEff, tvar, loc) =>
      // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
      val (actualTpe, actualEff) = visitExp(exp)
      expectTypeM(expected = expectedTpe.getOrElse(Type.freshVar(Kind.Star, loc)), actual = actualTpe, loc)
      unifyTypeM(actualTpe, tvar, loc)
      expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Eff, loc)), actual = actualEff, loc)
      val resTpe = tvar
      val resEff = actualEff
      (resTpe, resEff)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val (tpe, eff) = visitExp(exp)
      expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
      val resTpe = Type.Bool
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.CheckedCast(cast, exp, tvar, evar, loc) =>
      cast match {
        case CheckedCastType.TypeCast =>
          // Ignore the inferred type of exp.
          val (_, eff) = visitExp(exp)
          unifyTypeM(evar, eff, loc)
          val resTpe = tvar
          val resEff = evar
          (resTpe, resEff)

        case CheckedCastType.EffectCast =>
          // We simply union the purity and effect with a fresh variable.
          val (tpe, eff) = visitExp(exp)
          unifyTypeM(tvar, tpe, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff, evar, loc)
          (resTpe, resEff)
      }

    case Expr.UncheckedCast(exp, declaredTpe, declaredEff, tvar, loc) =>
      // A cast expression is unsound; the type system assumes the declared type is correct.
      val (actualTyp, actualEff) = visitExp(exp)
      unifyTypeM(tvar, declaredTpe.getOrElse(actualTyp), loc)
      val resTpe = tvar
      val resEff = declaredEff.getOrElse(actualEff)
      (resTpe, resEff)

    case Expr.UncheckedMaskingCast(exp, loc) =>
      val (tpe, eff) = visitExp(exp)
      val resTpe = tpe
      val resEff = Type.Pure
      (resTpe, resEff)

    case Expr.Without(exp, effUse, loc) =>
      val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
      //        val expected = Type.mkDifference(Type.freshVar(Kind.Bool, loc), effType, loc)
      // TODO EFF-MIGRATION use expected
      val (tpe, eff) = visitExp(exp)
      val resTpe = tpe
      val resEff = eff
      (resTpe, resEff)

    case Expr.TryCatch(exp, rules, loc) =>
      val (tpes, effs) = rules.map {
        case KindedAst.CatchRule(sym, clazz, body) =>
          visitExp(body)
      }.unzip
      val (tpe, eff) = visitExp(exp)
      val ruleTpe = unifyAllTypesM(tpes, Kind.Star, loc)
      unifyTypeM(tpe, ruleTpe, loc)
      val resTpe = tpe
      val resEff = Type.mkUnion(eff :: effs, loc)
      (resTpe, resEff)

    case Expr.TryWith(exp, eff, rules, tvar, loc) => ???
    case Expr.Do(op, args, tvar, loc) => ???
    case Expr.Resume(exp, argTvar, retTvar, loc) => ???

    case Expr.InvokeConstructor(constructor, args, loc) =>
      val classTpe = getFlixType(constructor.getDeclaringClass)
      val (_, _) = args.map(visitExp).unzip
      val resTpe = classTpe
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.InvokeMethod(method, clazz, exp, args, loc) =>
      val classTpe = getFlixType(clazz)
      val (baseTyp, _) = visitExp(exp)
      unifyTypeM(baseTyp, classTpe, loc)
      val (_, _) = args.map(visitExp).unzip
      val resTpe = getFlixType(method.getReturnType)
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.InvokeStaticMethod(method, args, loc) =>
      val returnTpe = getFlixType(method.getReturnType)
      val (_, _) = args.map(visitExp).unzip
      val resTpe = getFlixType(method.getReturnType)
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.GetField(field, clazz, exp, loc) =>
      val classType = getFlixType(clazz)
      val fieldType = getFlixType(field.getType)
      val (tpe, _) = visitExp(exp)
      expectTypeM(expected = classType, actual = tpe, exp.loc)
      val resTpe = fieldType
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.PutField(field, clazz, exp1, exp2, loc) =>
      val fieldType = getFlixType(field.getType)
      val classType = getFlixType(clazz)
      val (tpe1, _) = visitExp(exp1)
      val (tpe2, _) = visitExp(exp2)
      expectTypeM(expected = classType, actual = tpe1, exp1.loc)
      expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
      val resTpe = Type.Unit
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.GetStaticField(field, loc) =>
      val fieldType = getFlixType(field.getType)
      val resTpe = fieldType
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.PutStaticField(field, exp, loc) =>
      val (valueTyp, _) = visitExp(exp)
      expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
      val resTpe = Type.Unit
      val resEff = Type.Impure
      (resTpe, resEff)

    case Expr.NewObject(name, clazz, methods, loc) => ???
    case Expr.NewChannel(exp1, exp2, tvar, loc) => ???
    case Expr.GetChannel(exp, tvar, loc) => ???
    case Expr.PutChannel(exp1, exp2, loc) => ???
    case Expr.SelectChannel(rules, default, tvar, loc) => ???
    case Expr.Spawn(exp1, exp2, loc) => ???
    case Expr.ParYield(frags, exp, loc) => ???
    case Expr.Lazy(exp, loc) => ???
    case Expr.Force(exp, tvar, loc) => ???
    case Expr.FixpointConstraintSet(cs, tvar, loc) => ???
    case Expr.FixpointLambda(pparams, exp, tvar, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, loc) => ???
    case Expr.FixpointSolve(exp, loc) => ???
    case Expr.FixpointFilter(pred, exp, tvar, loc) => ???
    case Expr.FixpointInject(exp, pred, tvar, loc) => ???
    case Expr.FixpointProject(pred, exp1, exp2, tvar, loc) => ???
    case Expr.Error(m, tvar, eff) => ???
  }
}
