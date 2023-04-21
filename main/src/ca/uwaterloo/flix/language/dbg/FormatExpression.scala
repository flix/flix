package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Formatting of expressions.
  */
object FormatExpression {

  /**
    * TypedAst.
    */
  def format(e0: TypedAst.Expression): String = e0 match {
    case TypedAst.Expression.Cst(cst, _, _) => FormatConstant.format(cst)
    case TypedAst.Expression.Wild(_, _) => "_"
    case TypedAst.Expression.Var(sym, _, _) => s"Sym($sym)"
    case TypedAst.Expression.Def(sym, _, _) => s"Def($sym)"
    case TypedAst.Expression.Sig(sym, _, _) => s"Sig($sym)"
    case TypedAst.Expression.Hole(sym, _, _) => s"Hole($sym)"
    case TypedAst.Expression.HoleWithExp(exp, _, _, _, _) => s"HoleWithExp($exp)"
    case TypedAst.Expression.OpenAs(sym, exp, _, _) => s"OpenAs($sym, $exp)"
    case TypedAst.Expression.Use(sym, alias, exp, _) => s"Use($sym, $alias, $exp)"
    case TypedAst.Expression.Lambda(fparam, exp, tpe, loc) => s"Lambda(${FormatFormalParam.format(fparam)}, $exp)"
    case TypedAst.Expression.Apply(exp1, exp2, _, _, _, _) => s"Apply($exp1, $exp2)"
    case TypedAst.Expression.Unary(sop, exp, _, _, _, _) => s"Unary($sop, $exp)"
    case TypedAst.Expression.Binary(sop, exp1, exp2, _, _, _, _) => s"Binary($sop, $exp1, $exp2)"
    case TypedAst.Expression.Let(sym, mod, exp1, exp2, _, _, _, _) => s"Let($sym, $mod, $exp1, $exp2)"
    case TypedAst.Expression.LetRec(sym, mod, exp1, exp2, _, _, _, _) => s"LetRec($sym, $mod, $exp1, $exp2)"
    case TypedAst.Expression.Region(tpe, _) => s"Region($tpe)"
    case TypedAst.Expression.Scope(sym, _, exp, _, _, _, _) => s"Scope($sym, $exp)"
    case TypedAst.Expression.ScopeExit(exp1, exp2, _, _, _, _) => s"ScopeExit($exp1, $exp2)"
    case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) => s"IfThenElse($exp1, $exp2, $exp3)"
    case TypedAst.Expression.Stm(exp1, exp2, _, _, _, _) => s"Stm($exp1, $exp2)"
    case TypedAst.Expression.Discard(exp, _, _, _) => s"Discard($exp)"
    case TypedAst.Expression.Match(exp, rules, _, _, _, _) => s"Match($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.TypeMatch(exp, rules, _, _, _, _) => s"TypeMatch($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.RelationalChoose(exps, rules, _, _, _, _) => s"RelationalChoose($exps, $rules)"
    case TypedAst.Expression.RestrictableChoose(star, exp, rules, _, _, _, _) => s"RestrictableChoose${if (star) "*" else ""}($exp, $rules)"
    case TypedAst.Expression.Tag(sym, exp, _, _, _, _) => s"Tag($sym, $exp)"
    case TypedAst.Expression.RestrictableTag(sym, exp, _, _, _, _) => s"RestrictableTag($sym, $exp)"
    case TypedAst.Expression.Tuple(elms, _, _, _, _) => s"Tuple(${elms.mkString(", ")})"
    case TypedAst.Expression.RecordEmpty(_, _) => s"RecordEmpty"
    case TypedAst.Expression.RecordSelect(exp, field, _, _, _, _) => s"RecordSelect($exp, $field)"
    case TypedAst.Expression.RecordExtend(field, value, rest, _, _, _, _) => s"RecordExtend($field, $value, $rest)"
    case TypedAst.Expression.RecordRestrict(field, rest, _, _, _, _) => s"RecordRestrict($field, $rest)"
    case TypedAst.Expression.ArrayLit(exps, exp, _, _, _, _) => s"ArrayLit(${exps.mkString(", ")}, $exp)"
    case TypedAst.Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) => s"ArrayNew($exp1, $exp2, $exp3)"
    case TypedAst.Expression.ArrayLoad(base, index, _, _, _, _) => s"ArrayLoad($base, $index)"
    case TypedAst.Expression.ArrayLength(base, _, _, _) => s"ArrayLength($base)"
    case TypedAst.Expression.ArrayStore(base, index, elm, _, _, _) => s"ArrayStore($base, $index, $elm)"
    case TypedAst.Expression.VectorLit(exps, _, _, _, _) => s"VectorLit(${exps.mkString(", ")})"
    case TypedAst.Expression.VectorLoad(exp1, exp2, _, _, _, _) => s"VectorLoad($exp1, $exp2)"
    case TypedAst.Expression.VectorLength(exp, _) => s"VectorLength($exp)"
    case TypedAst.Expression.Ref(exp1, exp2, _, _, _, _) => s"Ref($exp1, $exp2)"
    case TypedAst.Expression.Deref(exp, _, _, _, _) => s"Deref($exp)"
    case TypedAst.Expression.Assign(exp1, exp2, _, _, _, _) => s"Assign($exp1, $exp2)"
    case TypedAst.Expression.Ascribe(exp, tpe, _, _, _) => s"Ascribe($exp, $tpe)"
    case TypedAst.Expression.CheckedCast(_, exp, _, _, _, _) => s"CheckedCast($exp)"
    case TypedAst.Expression.UncheckedCast(exp, declaredType, declaredPur, declaredEff, tpe, eff, _, _) => s"UncheckedCast($exp, $declaredType, $declaredPur, $declaredEff, $tpe, $eff)"
    case TypedAst.Expression.UncheckedMaskingCast(exp, _, _, _, _) => s"UncheckedMaskingCast($exp)"
    case TypedAst.Expression.Without(exp, sym, _, _, _, _) => s"Without($exp, $sym)"
    case TypedAst.Expression.TryCatch(exp, rules, _, _, _, _) => s"TryCatch($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.TryWith(exp, sym, rules, _, _, _, _) => s"TryWith($exp, $sym, ${rules.mkString(", ")})"
    case TypedAst.Expression.Do(sym, exps, _, _, _) => s"Do($sym, ${exps.mkString(", ")})"
    case TypedAst.Expression.Resume(exp, _, _) => s"Resume($exp)"
    case TypedAst.Expression.InvokeConstructor(constructor, args, _, _, _, _) => s"InvokeConstructor($constructor, ${args.mkString(", ")})"
    case TypedAst.Expression.InvokeMethod(method, exp, args, _, _, _, _) => s"InvokeMethod($method, $exp, ${args.mkString(", ")})"
    case TypedAst.Expression.InvokeStaticMethod(method, args, _, _, _, _) => s"InvokeStaticMethod($method, ${args.mkString(", ")})"
    case TypedAst.Expression.GetField(field, exp, _, _, _, _) => s"GetField($field, $exp)"
    case TypedAst.Expression.PutField(field, exp1, exp2, _, _, _, _) => s"PutField($field, $exp1, $exp2)"
    case TypedAst.Expression.GetStaticField(field, _, _, _, _) => s"GetStaticField($field)"
    case TypedAst.Expression.PutStaticField(field, exp, _, _, _, _) => s"PutStaticField($field, $exp)"
    case TypedAst.Expression.NewObject(_, clazz, _, _, _, methods, _) => s"NewObject($clazz, ${methods.map(FormatJvmMethod.format).mkString(", ")})"
    case TypedAst.Expression.NewChannel(exp1, exp2, _, _, _, _) => s"NewChannel($exp1, $exp2)"
    case TypedAst.Expression.GetChannel(exp, _, _, _, _) => s"GetChannel($exp)"
    case TypedAst.Expression.PutChannel(exp1, exp2, _, _, _, _) => s"PutChannel($exp1, $exp2)"
    case TypedAst.Expression.SelectChannel(rules, default, _, _, _, _) => s"SelectChannel(${rules.mkString(", ")}, $default)"
    case TypedAst.Expression.Spawn(exp1, exp2, _, _, _, _) => s"Spawn($exp1, $exp2)"
    case TypedAst.Expression.Par(exp, _) => s"Par($exp)"
    case TypedAst.Expression.ParYield(frags, exp, _, _, _, _) => s"ParYield(${frags.mkString(",")}, $exp)"

    case TypedAst.Expression.Lazy(exp, _, _) => s"Lazy($exp)"
    case TypedAst.Expression.Force(exp, _, _, _, _) => s"Force($exp)"
    case TypedAst.Expression.FixpointConstraintSet(cs, _, _, _) => s"FixpointConstraintSet($cs})"
    case TypedAst.Expression.FixpointLambda(pparams, exp, _, _, _, _, _) => s"FixpointLambda(${pparams.map(_.pred).mkString(", ")}, $exp)"
    case TypedAst.Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) => s"FixpointMerge($exp1, $exp2)"
    case TypedAst.Expression.FixpointSolve(exp, stf, _, _, _, _) => s"FixpointSolve($exp, $stf)"
    case TypedAst.Expression.FixpointFilter(pred, exp, _, _, _, _) => s"FixpointFilter($pred, $exp)"
    case TypedAst.Expression.FixpointInject(exp, pred, _, _, _, _) => s"FixpointInject($exp, $pred)"
    case TypedAst.Expression.FixpointProject(pred, exp, _, _, _, _) => s"FixpointProject($pred, $exp)"
    case TypedAst.Expression.Instanceof(exp, cls, _) => s"Instanceof($exp, $cls)"
    case TypedAst.Expression.Error(m, _, _, _) => s"Error(${m.kind})"

  }

}
