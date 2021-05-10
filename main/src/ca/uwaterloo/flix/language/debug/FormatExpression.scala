package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Formatting of expressions.
  */
object FormatExpression {

  /**
    * TypedAst.
    */
  def format(e0: TypedAst.Expression): String = e0 match {
    case TypedAst.Expression.Unit(_) => "()"
    case TypedAst.Expression.Null(_, _) => "null"
    case TypedAst.Expression.True(_) => "true"
    case TypedAst.Expression.False(_) => "false"
    case TypedAst.Expression.Char(lit, _) => "'" + lit + "'"
    case TypedAst.Expression.Float32(lit, _) => s"${lit}f32"
    case TypedAst.Expression.Float64(lit, _) => s"${lit}f64"
    case TypedAst.Expression.Int8(lit, _) => s"${lit}i8"
    case TypedAst.Expression.Int16(lit, _) => s"${lit}i16"
    case TypedAst.Expression.Int32(lit, _) => s"${lit}i32"
    case TypedAst.Expression.Int64(lit, _) => s"${lit}i64"
    case TypedAst.Expression.BigInt(lit, _) => s"${lit}i64"
    case TypedAst.Expression.Str(lit, _) => "\"" + lit + "\""
    case TypedAst.Expression.Default(_, _) => s"default"
    case TypedAst.Expression.Wild(_, _) => "_"
    case TypedAst.Expression.Var(sym, _, _) => s"Sym($sym)"
    case TypedAst.Expression.Def(sym, _, _) => s"Def($sym)"
    case TypedAst.Expression.Sig(sym, _, _) => s"Sig($sym)"
    case TypedAst.Expression.Hole(sym, tpe, eff, loc) => s"Hole($sym)"
    case TypedAst.Expression.Lambda(fparam, exp, tpe, loc) => s"Lambda(${FormatFormalParam.format(fparam)}, $exp)"
    case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) => s"Apply($exp1, $exp2)"
    case TypedAst.Expression.Unary(sop, exp, tpe, eff, loc) => s"Unary($sop, $exp)"
    case TypedAst.Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => s"Binary($sop, $exp1, $exp2)"
    case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => s"Let($sym, $exp1, $exp2)"
    case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => s"IfThenElse($exp1, $exp2, $exp3)"
    case TypedAst.Expression.Stm(exp1, exp2, _, _, _) => s"Stm($exp1, $exp2)"
    case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) => s"Match($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.Choose(exps, rules, _, _, _) => s"Choose($exps, $rules)"
    case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => s"Tag($sym, $tag, $exp)"
    case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => s"Tuple(${elms.mkString(", ")})"
    case TypedAst.Expression.RecordEmpty(tpe, loc) => s"RecordEmpty"
    case TypedAst.Expression.RecordSelect(exp, field, tpe, eff, loc) => s"RecordSelect($exp, $field)"
    case TypedAst.Expression.RecordExtend(field, value, rest, tpe, eff, loc) => s"RecordExtend($field, $value, $rest)"
    case TypedAst.Expression.RecordRestrict(field, rest, tpe, eff, loc) => s"RecordRestrict($field, $rest)"
    case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => s"ArrayLit(${elms.mkString(", ")})"
    case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => s"ArrayNew($elm, $len)"
    case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => s"ArrayLoad($base, $index)"
    case TypedAst.Expression.ArrayLength(base, eff, loc) => s"ArrayLength($base)"
    case TypedAst.Expression.ArrayStore(base, index, elm, loc) => s"ArrayStore($base, $index, $elm)"
    case TypedAst.Expression.ArraySlice(base, begin, end, tpe, loc) => s"ArraySlice($base, $begin, $end)"
    case TypedAst.Expression.Ref(exp, tpe, eff, loc) => s"Ref($exp)"
    case TypedAst.Expression.Deref(exp, tpe, eff, loc) => s"Deref($exp)"
    case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) => s"Assign($exp1, $exp2)"
    case TypedAst.Expression.Existential(fparam, exp, loc) => s"Existential($fparam, $exp)"
    case TypedAst.Expression.Universal(fparam, exp, loc) => s"Universal($fparam, $exp)"
    case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => s"Ascribe($exp, $tpe)"
    case TypedAst.Expression.Cast(exp, tpe, eff, loc) => s"Cast($exp, $tpe)"
    case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) => s"TryCatch($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => s"InvokeConstructor($constructor, ${args.mkString(", ")})"
    case TypedAst.Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => s"InvokeMethod($method, $exp, ${args.mkString(", ")})"
    case TypedAst.Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => s"InvokeStaticMethod($method, ${args.mkString(", ")})"
    case TypedAst.Expression.GetField(field, exp, tpe, eff, loc) => s"GetField($field, $exp)"
    case TypedAst.Expression.PutField(field, exp1, exp2, tpe, eff, loc) => s"PutField($field, $exp1, $exp2)"
    case TypedAst.Expression.GetStaticField(field, tpe, eff, loc) => s"GetStaticField($field)"
    case TypedAst.Expression.PutStaticField(field, exp, tpe, eff, loc) => s"PutStaticField($field, $exp)"
    case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) => s"NewChannel($exp)"
    case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) => s"GetChannel($exp)"
    case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) => s"PutChannel($exp1, $exp2)"
    case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) => s"SelectChannel(${rules.mkString(", ")}, $default)"
    case TypedAst.Expression.Spawn(exp, tpe, eff, loc) => s"Spawn($exp)"
    case TypedAst.Expression.Lazy(exp, tpe, loc) => s"Lazy($exp)"
    case TypedAst.Expression.Force(exp, tpe, eff, loc) => s"Force($exp)"
    case TypedAst.Expression.FixpointConstraintSet(cs, stf, tpe, loc) => s"FixpointConstraintSet($cs})"
    case TypedAst.Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => s"FixpointMerge($exp1, $exp2)"
    case TypedAst.Expression.FixpointSolve(exp, stf, tpe, eff, loc) => s"FixpointSolve($exp, $stf)"
    case TypedAst.Expression.FixpointFilter(pred, exp, tpe, eff, loc) => s"FixpointFilter($pred, $exp)"
    case TypedAst.Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) => s"FixpointProjectIn($exp, $pred)"
    case TypedAst.Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) => s"FixpointProjectOut($pred, $exp)"
  }

}
