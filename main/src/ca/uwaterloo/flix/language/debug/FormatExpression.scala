package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

object FormatExpression {

  def format(e0: TypedAst.Expression): String = e0 match {
    case TypedAst.Expression.Unit(_) => "()"
    case TypedAst.Expression.True(_) => "true"
    case TypedAst.Expression.False(_) => "false"
    case TypedAst.Expression.Char(lit, _) => "'" + lit + "'"
    case TypedAst.Expression.Float32(lit, _) => "..." // TODO
    case TypedAst.Expression.Float64(lit, _) => "..." // TODO
    case TypedAst.Expression.Int8(lit, _) => "..." // TODO
    case TypedAst.Expression.Int16(lit, _) => "..." // TODO
    case TypedAst.Expression.Int32(lit, _) => "..." // TODO
    case TypedAst.Expression.Int64(lit, _) => "..." // TODO
    case TypedAst.Expression.BigInt(lit, _) => "..." // TODO
    case TypedAst.Expression.Str(lit, _) => "..." // TODO
    case TypedAst.Expression.Wild(tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Var(sym, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Def(sym, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Eff(sym, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Hole(sym, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Switch(rules, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.RecordEmpty(tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.RecordSelect(exp, label, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => "..." // TODO
    case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => "..." // TODO
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
