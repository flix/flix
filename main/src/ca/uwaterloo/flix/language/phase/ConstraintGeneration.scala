package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, LevelEnv, RigidityEnv, Scheme, SourceLocation, Symbol, Type}

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

    case Expr.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) => ???
    case Expr.ArrayLoad(base, index, tvar, evar, loc) => ???
    case Expr.ArrayStore(base, index, elm, evar, loc) => ???
    case Expr.ArrayLength(base, loc) => ???
    case Expr.VectorLit(exps, tvar, evar, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tvar, evar, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ref(exp1, exp2, tvar, evar, loc) => ???
    case Expr.Deref(exp, tvar, evar, loc) => ???
    case Expr.Assign(exp1, exp2, evar, loc) => ???
    case Expr.Ascribe(exp, expectedType, expectedPur, tvar, loc) => ???
    case Expr.InstanceOf(exp, clazz, loc) => ???
    case Expr.CheckedCast(cast, exp, tvar, evar, loc) => ???
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tvar, loc) => ???
    case Expr.UncheckedMaskingCast(exp, loc) => ???
    case Expr.Without(exp, eff, loc) => ???
    case Expr.TryCatch(exp, rules, loc) => ???
    case Expr.TryWith(exp, eff, rules, tvar, loc) => ???
    case Expr.Do(op, args, tvar, loc) => ???
    case Expr.Resume(exp, argTvar, retTvar, loc) => ???
    case Expr.InvokeConstructor(constructor, args, loc) => ???
    case Expr.InvokeMethod(method, clazz, exp, args, loc) => ???
    case Expr.InvokeStaticMethod(method, args, loc) => ???
    case Expr.GetField(field, clazz, exp, loc) => ???
    case Expr.PutField(field, clazz, exp1, exp2, loc) => ???
    case Expr.GetStaticField(field, loc) => ???
    case Expr.PutStaticField(field, exp, loc) => ???
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
