package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Implementations of `toString` for expressions of all the different ASTs.
  */
object FormatExpression {

  def format(e0: TypedAst.Expression): String = e0 match {
    case TypedAst.Expression.Unit(_) => "()"
    case TypedAst.Expression.True(_) => "true"
    case TypedAst.Expression.False(_) => "false"
    case TypedAst.Expression.Char(lit, _) => "'" + lit + "'"
    case TypedAst.Expression.Float32(lit, _) => s"{$lit}f32"
    case TypedAst.Expression.Float64(lit, _) => s"{$lit}f64"
    case TypedAst.Expression.Int8(lit, _) => s"{$lit}i8"
    case TypedAst.Expression.Int16(lit, _) => s"{$lit}i16"
    case TypedAst.Expression.Int32(lit, _) => s"{$lit}i32"
    case TypedAst.Expression.Int64(lit, _) => s"{$lit}i64"
    case TypedAst.Expression.BigInt(lit, _) => s"{$lit}i64"
    case TypedAst.Expression.Str(lit, _) => "\"" + lit + "\""
    case TypedAst.Expression.Wild(_, _, _) => "_"
    case TypedAst.Expression.Var(sym, _, _, _) => s"Sym($sym)"
    case TypedAst.Expression.Def(sym, _, _, _) => s"Def($sym)"
    case TypedAst.Expression.Eff(sym, _, _, _) => s"Eff($sym)"
    case TypedAst.Expression.Hole(sym, tpe, eff, loc) => s"Hole($sym)"
    case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) => s"Lambda($fparam, $exp)"
    case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) => s"Apply($exp1, $exp2)"
    case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) => s"Unary($op, $exp)"
    case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) => s"Binary($op, $exp1, $exp2)"
    case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => s"Let($sym, $exp1, $exp2)"
    case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => s"LetRec($sym, $exp1, $exp2)"
    case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => s"IfThenElse($exp1, $exp2, $exp3)"
    case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) => s"Match($exp, ${rules.mkString(", ")})"
    case TypedAst.Expression.Switch(rules, tpe, eff, loc) => s"Switch(${rules.mkString(", ")})"
    case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => s"Tag($sym, $tag, $exp)"
    case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => s"Tuple(${elms.mkString(", ")})"
    case TypedAst.Expression.RecordEmpty(tpe, eff, loc) => s"RecordEmpty"
    case TypedAst.Expression.RecordSelect(exp, label, tpe, eff, loc) => s"RecordSelect($exp, $label)"
    case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) => s"RecordExtend($label, $value, $rest)"
    case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) => s"RecordRestrict($label, $rest)"
    case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => s"ArrayLit(${elms.mkString(", ")})"
    case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => s"ArrayNew($elm, $len)"
    case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => s"ArrayLoad($base, $index)"
    case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => s"ArrayLength($base)"
    case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) => s"ArrayStore($base, $index, $elm)"
    case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => s"ArraySlice($base, $beginIndex, $endIndex)"
    case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => s"VectorLit(${elms.mkString(", ")})"
    case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.VectorLength(base, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Ref(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Deref(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Existential(fparam, exp, eff, loc) => "..." // TODO
    case TypedAst.Expression.Universal(fparam, exp, eff, loc) => "..." // TODO
    case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Cast(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.NativeField(field, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.NativeMethod(method, args, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Spawn(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Sleep(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.FixpointConstraint(c, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.UserError(tpe, eff, loc) => "..." // TODO
  }

}
