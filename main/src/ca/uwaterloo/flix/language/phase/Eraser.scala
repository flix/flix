package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.JType
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, FinalAst}
import ca.uwaterloo.flix.util.Validation
import ErasedAst.{Expression => ErasedExp}
import FinalAst.{Expression => FinalExp}

object Eraser {

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationError] = flix.phase("TreeShaker") {
    ???
  }

  def castToExp[T <: JType](exp: ErasedExp[_]): ErasedExp[T] = exp.asInstanceOf[ErasedExp[T]]

  def visitExp[T <: JType](exp: FinalExp): ErasedExp[T] = exp match {
//    case Expression.Unit(loc) =>
//    case Expression.Null(tpe, loc) =>
    case FinalExp.True(loc) => castToExp(ErasedExp.True(loc))
    case FinalExp.False(loc) => castToExp(ErasedExp.False(loc))
    case FinalExp.Char(lit, loc) => castToExp(ErasedExp.Char(lit, loc))
    case FinalExp.Float32(lit, loc) => castToExp(ErasedExp.Float32(lit, loc))
    case FinalExp.Float64(lit, loc) => castToExp(ErasedExp.Float64(lit, loc))
    case FinalExp.Int8(lit, loc) => castToExp(ErasedExp.Int8(lit, loc))
    case FinalExp.Int16(lit, loc) => castToExp(ErasedExp.Int16(lit, loc))
    case FinalExp.Int32(lit, loc) => castToExp(ErasedExp.Int32(lit, loc))
    case FinalExp.Int64(lit, loc) => castToExp(ErasedExp.Int64(lit, loc))
//    case Expression.BigInt(lit, _) =>
    case FinalExp.Str(lit, loc) => ErasedExp.Str(lit, loc).asInstanceOf[ErasedExp[T]]
    case FinalExp.Var(sym, tpe, loc) => ErasedExp.Var(sym, tpe, loc)
//    case Expression.Closure(sym, freeVars, fnMonoType, tpe, loc) =>
//    case Expression.ApplyClo(exp, args, tpe, loc) =>
//    case Expression.ApplyDef(sym, args, tpe, loc) =>
//    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
//    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
//    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
//    case Expression.Unary(sop, op, exp, tpe, loc) =>
//    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
    case FinalExp.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp[PrimInt32](exp1)
      val e2 = visitExp[T](exp2)
      val e3 = visitExp[T](exp3)
      ErasedExp.IfThenElse(e1, e2, e3, tpe, loc)
//    case Expression.Branch(exp, branches, tpe, loc) =>
//    case Expression.JumpTo(sym, tpe, loc) =>
//    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
//    case Expression.Is(sym, tag, exp, loc) =>
//    case Expression.Tag(sym, tag, exp, tpe, loc) =>
//    case Expression.Untag(sym, tag, exp, tpe, loc) =>
    case FinalExp.Index(base, offset, tpe, loc) =>
      val b = visitExp[JObject](base)
      val e = ErasedExp.Index(b, offset, tpe, loc)
      ErasedExp.Cast[JObject, T](e, tpe, loc)
//    case Expression.Tuple(elms, tpe, loc) =>
//    case Expression.RecordEmpty(tpe, loc) =>
//    case Expression.RecordSelect(exp, field, tpe, loc) =>
//    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
//    case Expression.RecordRestrict(field, rest, tpe, loc) =>
//    case Expression.ArrayLit(elms, tpe, loc) =>
//    case Expression.ArrayNew(elm, len, tpe, loc) =>
//    case Expression.ArrayLoad(base, index, tpe, loc) =>
//    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
//    case Expression.ArrayLength(base, tpe, loc) =>
//    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
    case FinalExp.Ref(exp, tpe, loc) =>
      val e1 = visitExp(exp)
      ErasedExp.Ref(e1, tpe, loc).asInstanceOf[ErasedExp[T]]
//    case Expression.Deref(exp, tpe, loc) =>
//    case Expression.Assign(exp1, exp2, tpe, loc) =>
//    case Expression.Existential(fparam, exp, loc) =>
//    case Expression.Universal(fparam, exp, loc) =>
//    case Expression.Cast(exp, tpe, loc) =>
//    case Expression.TryCatch(exp, rules, tpe, loc) =>
//    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
//    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
//    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
//    case Expression.GetField(field, exp, tpe, loc) =>
//    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
//    case Expression.GetStaticField(field, tpe, loc) =>
//    case Expression.PutStaticField(field, exp, tpe, loc) =>
//    case Expression.NewChannel(exp, tpe, loc) =>
//    case Expression.GetChannel(exp, tpe, loc) =>
//    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
//    case Expression.SelectChannel(rules, default, tpe, loc) =>
//    case Expression.Spawn(exp, tpe, loc) =>
//    case Expression.Lazy(exp, tpe, loc) =>
//    case Expression.Force(exp, tpe, loc) =>
//    case Expression.FixpointConstraintSet(cs, tpe, loc) =>
//    case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
//    case Expression.FixpointSolve(exp, stf, tpe, loc) =>
//    case Expression.FixpointProject(pred, exp, tpe, loc) =>
//    case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
//    case Expression.FixpointFold(pred, init, f, constraints, tpe, loc) =>
//    case Expression.HoleError(sym, tpe, loc) =>
//    case Expression.MatchError(tpe, loc) =>
    case _ => ???
  }
}
