package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.ParOps

object TrustValidation {

  def run(root: TypedAst.Root)(implicit flix: Flix): List[TrustError] = {
    val defErrors = ParOps.parMap(root.defs.values)(visitDef).flatten
    // todo: instances, sigs, traits
    defErrors.toList
  }

  private def visitDef(defn0: TypedAst.Def): List[TrustError] = {
    visitExp(defn0.exp)(defn0.loc)
  }

  private def visitExp(expr0: TypedAst.Expr)(implicit loc0: SourceLocation): List[TrustError] = expr0 match {
    case Expr.Cst(cst, tpe, loc) =>
      List.empty

    case Expr.Var(sym, tpe, loc) =>
      List.empty

    case Expr.Hole(sym, tpe, eff, loc) =>
      List.empty

    case Expr.HoleWithExp(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.OpenAs(symUse, exp, tpe, loc) =>
      visitExp(exp)

    case Expr.Use(sym, alias, exp, loc) =>
      visitExp(exp)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.ApplySig(symUse, exps, itpe, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Region(tpe, loc) =>
      List.empty

    case Expr.Scope(bnd, regSym, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Discard(exp, eff, loc) =>
      visitExp(exp)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      visitExp(exp) // todo rules

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp) // todo rules

    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      visitExp(exp) // todo rules

    case Expr.Tag(sym, exps, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.RestrictableTag(sym, exps, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.Tuple(exps, tpe, eff, loc) =>
      exps.flatMap(visitExp)
    case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      exps.flatMap(visitExp) ::: visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ArrayLength(exp, eff, loc) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
      fields.flatMap(se => visitExp(se._2)) ::: visitExp(region)

    case Expr.StructGet(exp, sym, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLength(exp, loc) =>
      visitExp(exp)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val err = TrustError.InstanceOfUse(Expr.InstanceOf(exp, clazz, loc), loc0)
      err :: visitExp(exp)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val err = TrustError.CheckedCastUse(Expr.CheckedCast(cast, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val err = TrustError.UncheckedCastUse(Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Unsafe(exp, runEff, tpe, eff, loc) =>
      val err = TrustError.UnsafeUse(Expr.Unsafe(exp, runEff, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Without(exp, sym, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val err = TrustError.TryCatchUse(Expr.TryCatch(exp, rules, tpe, eff, loc), loc0)
      err :: visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.Throw(exp, tpe, eff, loc) =>
      val err = TrustError.ThrowUse(Expr.Throw(exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Handler(sym, rules, bodyType, bodyEff, handledEff, tpe, loc) =>
      rules.flatMap(r => visitExp(r.exp))

    case Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      exps.flatMap(visitExp)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val err = TrustError.InvokeConstructorUse(Expr.InvokeConstructor(constructor, exps, tpe, eff, loc), loc0)
      err :: exps.flatMap(visitExp)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val err = TrustError.InvokeMethodUse(Expr.InvokeMethod(method, exp, exps, tpe, eff, loc), loc0)
      err :: visitExp(exp) ::: exps.flatMap(visitExp)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val err = TrustError.InvokeStaticMethodUse(Expr.InvokeStaticMethod(method, exps, tpe, eff, loc), loc0)
      err :: exps.flatMap(visitExp)

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      val err = TrustError.GetFieldUse(Expr.GetField(field, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)


    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val err = TrustError.PutFieldUse(Expr.PutField(field, exp1, exp2, tpe, eff, loc), loc0)
      err :: visitExp(exp1) ::: visitExp(exp2)

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      val err = TrustError.GetStaticFieldUse(Expr.GetStaticField(field, tpe, eff, loc), loc0)
      List(err)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val err = TrustError.PutStaticFieldUse(Expr.PutStaticField(field, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val err = TrustError.NewObjectUse(Expr.NewObject(name, clazz, tpe, eff, methods, loc), loc0)
      err :: methods.flatMap(m => visitExp(m.exp))

    case Expr.NewChannel(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.GetChannel(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      rules.flatMap(r => visitExp(r.exp) ::: visitExp(r.chan)) ::: default.map(visitExp).getOrElse(List.empty)

    case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ParYield(frags, exp, tpe, eff, loc) =>
      frags.flatMap(f => visitExp(f.exp)) ::: visitExp(exp)

    case Expr.Lazy(exp, tpe, loc) =>
      visitExp(exp)

    case Expr.Force(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, tpe, loc) =>
      cs.flatMap(visitConstraint)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.FixpointSolve(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.Error(_, _, _) => List.empty
  }

  private def visitConstraint(constr0: TypedAst.Constraint)(implicit loc0: SourceLocation): List[TrustError] = constr0 match {
    case TypedAst.Constraint(_, head, body, _) =>
      val headErrors = head match {
        case Head.Atom(_, _, terms, _, _) => terms.flatMap(visitExp)
      }
      val bodyErrors = body.flatMap {
        case Body.Atom(_, _, _, _, _, _, _) => List.empty
        case Body.Functional(_, exp, _) => visitExp(exp)
        case Body.Guard(exp, _) => visitExp(exp)
      }
      headErrors ::: bodyErrors
  }

}
