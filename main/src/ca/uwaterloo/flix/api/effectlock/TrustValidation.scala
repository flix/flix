package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.shared.Input
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.ParOps

object TrustValidation {

  def run(root: TypedAst.Root)(implicit flix: Flix): List[SuspiciousExpr] = {
    val traitErrors = ParOps.parMap(root.traits.values)(visitTrait).flatten.toList
    val instanceErrors = ParOps.parMap(root.instances.values)(visitInstance).flatten.toList
    val signatureErrors = ParOps.parMap(root.sigs.values)(visitSig).flatten.toList
    val defErrors = ParOps.parMap(root.defs.values)(visitDef).flatten.toList
    traitErrors ::: instanceErrors ::: signatureErrors ::: defErrors
  }

  private def visitTrait(trait0: TypedAst.Trait): List[SuspiciousExpr] = trait0 match {
    case TypedAst.Trait(_, _, _, _, _, _, _, sigs, laws, loc) if isLibrary(loc) => sigs.flatMap(visitSig) ::: laws.flatMap(visitDef)
    case TypedAst.Trait(_, _, _, _, _, _, _, _, _, _) => List.empty
  }

  private def visitInstance(instance0: TypedAst.Instance): List[SuspiciousExpr] = instance0 match {
    case TypedAst.Instance(_, _, _, _, _, _, _, defs, _, loc) if isLibrary(loc) => defs.flatMap(visitDef)
    case TypedAst.Instance(_, _, _, _, _, _, _, _, _, _) => List.empty
  }

  private def visitSig(sig0: TypedAst.Sig): List[SuspiciousExpr] = sig0 match {
    case TypedAst.Sig(_, _, Some(exp), loc) if isLibrary(loc) => visitExp(exp)(loc)
    case TypedAst.Sig(_, _, _, _) => List.empty
  }

  private def visitDef(defn0: TypedAst.Def): List[SuspiciousExpr] = defn0 match {
    case TypedAst.Def(_, _, exp, loc) if isLibrary(loc) => visitExp(exp)(loc)
    case TypedAst.Def(_, _, _, _) => List.empty
  }

  private def visitExp(expr0: TypedAst.Expr)(implicit loc0: SourceLocation): List[SuspiciousExpr] = expr0 match {
    case Expr.Cst(_, _, _) =>
      List.empty

    case Expr.Var(_, _, _) =>
      List.empty

    case Expr.Hole(_, _, _, _) =>
      List.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Region(_, _) =>
      List.empty

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp) ::: r.guard.map(visitExp).getOrElse(List.empty))

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.Tag(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.Tuple(exps, _, _, _) =>
      exps.flatMap(visitExp)
    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      exps.flatMap(visitExp) ::: visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      fields.flatMap(se => visitExp(se._2)) ::: visitExp(region)

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val err = SuspiciousExpr.InstanceOfUse(Expr.InstanceOf(exp, clazz, loc), loc0)
      err :: visitExp(exp)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.CheckedCastUse(Expr.CheckedCast(cast, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val err = SuspiciousExpr.UncheckedCastUse(Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Unsafe(exp, runEff, tpe, eff, loc) =>
      val err = SuspiciousExpr.UnsafeUse(Expr.Unsafe(exp, runEff, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val err = SuspiciousExpr.TryCatchUse(Expr.TryCatch(exp, rules, tpe, eff, loc), loc0)
      err :: visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.Throw(exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.ThrowUse(Expr.Throw(exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      rules.flatMap(r => visitExp(r.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeConstructorUse(Expr.InvokeConstructor(constructor, exps, tpe, eff, loc), loc0)
      err :: exps.flatMap(visitExp)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeMethodUse(Expr.InvokeMethod(method, exp, exps, tpe, eff, loc), loc0)
      err :: visitExp(exp) ::: exps.flatMap(visitExp)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeStaticMethodUse(Expr.InvokeStaticMethod(method, exps, tpe, eff, loc), loc0)
      err :: exps.flatMap(visitExp)

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.GetFieldUse(Expr.GetField(field, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)


    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val err = SuspiciousExpr.PutFieldUse(Expr.PutField(field, exp1, exp2, tpe, eff, loc), loc0)
      err :: visitExp(exp1) ::: visitExp(exp2)

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      val err = SuspiciousExpr.GetStaticFieldUse(Expr.GetStaticField(field, tpe, eff, loc), loc0)
      List(err)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.PutStaticFieldUse(Expr.PutStaticField(field, exp, tpe, eff, loc), loc0)
      err :: visitExp(exp)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val err = SuspiciousExpr.NewObjectUse(Expr.NewObject(name, clazz, tpe, eff, methods, loc), loc0)
      err :: methods.flatMap(m => visitExp(m.exp))

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap(r => visitExp(r.exp) ::: visitExp(r.chan)) ::: default.map(visitExp).getOrElse(List.empty)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.flatMap(f => visitExp(f.exp)) ::: visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.flatMap(visitConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) => List.empty
  }

  private def visitConstraint(constr0: TypedAst.Constraint)(implicit loc0: SourceLocation): List[SuspiciousExpr] = constr0 match {
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

  private def isLibrary(loc0: SourceLocation): Boolean = loc0.sp1.source.input match {
    case Input.Text(_, _, _) => false
    case Input.TxtFile(_, _) => false
    case Input.PkgFile(_, _) => true // TODO maybe consider flipping this to false?
    case Input.FileInPackage(_, _, _, _) => true
    case Input.Unknown => false
  }

}
