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

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationError] = flix.phase("Eraser") {
    ???
  }

  def castExp[T <: JType](exp: ErasedExp[JType]): ErasedExp[T] = exp.asInstanceOf[ErasedExp[T]]

  def visitExp[T <: JType](exp: FinalExp): ErasedExp[T] = exp match {
    case FinalExp.Unit(loc) => castExp(ErasedExp.Unit(loc))
    case FinalExp.Null(tpe, loc) => castExp(ErasedExp.Null(tpe, loc))
    case FinalExp.True(loc) => castExp(ErasedExp.True(loc))
    case FinalExp.False(loc) => castExp(ErasedExp.False(loc))
    case FinalExp.Char(lit, loc) => castExp(ErasedExp.Char(lit, loc))
    case FinalExp.Float32(lit, loc) => castExp(ErasedExp.Float32(lit, loc))
    case FinalExp.Float64(lit, loc) => castExp(ErasedExp.Float64(lit, loc))
    case FinalExp.Int8(lit, loc) => castExp(ErasedExp.Int8(lit, loc))
    case FinalExp.Int16(lit, loc) => castExp(ErasedExp.Int16(lit, loc))
    case FinalExp.Int32(lit, loc) => castExp(ErasedExp.Int32(lit, loc))
    case FinalExp.Int64(lit, loc) => castExp(ErasedExp.Int64(lit, loc))
    case FinalExp.BigInt(lit, loc) => castExp(ErasedExp.BigInt(lit, loc))
    case FinalExp.Str(lit, loc) => castExp(ErasedExp.Str(lit, loc))
    case FinalExp.Var(sym, tpe, loc) => ErasedExp.Var(sym, tpe, loc)
    case FinalExp.Closure(sym, freeVars, fnMonoType, tpe, loc) =>
      val newFreeVars = freeVars.map({ case FinalAst.FreeVar(sym, tpe) => ErasedAst.FreeVar(sym, tpe) })
      castExp(ErasedExp.Closure(sym, newFreeVars, fnMonoType, tpe, loc))
    case FinalExp.ApplyClo(exp, args, tpe, loc) =>
      castExp(ErasedExp.ApplyClo(visitExp(exp), args.map(visitExp), tpe, loc))
    case FinalExp.ApplyDef(sym, args, tpe, loc) => castExp(ErasedExp.ApplyDef(sym, args.map(visitExp), tpe, loc))
    case FinalExp.ApplyCloTail(exp, args, tpe, loc) =>
      castExp(ErasedExp.ApplyCloTail(visitExp(exp), args.map(visitExp), tpe, loc))
    case FinalExp.ApplyDefTail(sym, args, tpe, loc) =>
      castExp(ErasedExp.ApplyDefTail(sym, args.map(visitExp), tpe, loc))
    case FinalExp.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val newFormals = formals.map({ case FinalAst.FormalParam(sym, tpe) => ErasedAst.FormalParam(sym, tpe) })
      castExp(ErasedExp.ApplySelfTail(sym, newFormals, actuals.map(visitExp), tpe, loc))
    case FinalExp.Unary(sop, op, exp, tpe, loc) => castExp(ErasedExp.Unary(sop, op, visitExp(exp), tpe, loc))
    case FinalExp.Binary(sop, op, exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp[PrimInt32](exp1)
      val e2 = visitExp[T](exp2)
      val e3 = visitExp[T](exp3)
      ErasedExp.IfThenElse(e1, e2, e3, tpe, loc)
//    case FinalExp.Branch(exp, branches, tpe, loc) =>
//    case FinalExp.JumpTo(sym, tpe, loc) =>
//    case FinalExp.Let(sym, exp1, exp2, tpe, loc) =>
//    case FinalExp.Is(sym, tag, exp, loc) =>
//    case FinalExp.Tag(sym, tag, exp, tpe, loc) =>
//    case FinalExp.Untag(sym, tag, exp, tpe, loc) =>
    case FinalExp.Index(base, offset, tpe, loc) =>
      val b = visitExp[JObject](base)
      val e = ErasedExp.Index(b, offset, tpe, loc)
      ErasedExp.Cast[JObject, T](e, tpe, loc)
//    case FinalExp.Tuple(elms, tpe, loc) =>
//    case FinalExp.RecordEmpty(tpe, loc) =>
//    case FinalExp.RecordSelect(exp, field, tpe, loc) =>
//    case FinalExp.RecordExtend(field, value, rest, tpe, loc) =>
//    case FinalExp.RecordRestrict(field, rest, tpe, loc) =>
//    case FinalExp.ArrayLit(elms, tpe, loc) =>
//    case FinalExp.ArrayNew(elm, len, tpe, loc) =>
//    case FinalExp.ArrayLoad(base, index, tpe, loc) =>
//    case FinalExp.ArrayStore(base, index, elm, tpe, loc) =>
//    case FinalExp.ArrayLength(base, tpe, loc) =>
//    case FinalExp.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
    case FinalExp.Ref(exp, tpe, loc) =>
      val e1 = visitExp(exp)
      ErasedExp.Ref(e1, tpe, loc).asInstanceOf[ErasedExp[T]]
//    case FinalExp.Deref(exp, tpe, loc) =>
//    case FinalExp.Assign(exp1, exp2, tpe, loc) =>
//    case FinalExp.Existential(fparam, exp, loc) =>
//    case FinalExp.Universal(fparam, exp, loc) =>
//    case FinalExp.Cast(exp, tpe, loc) =>
//    case FinalExp.TryCatch(exp, rules, tpe, loc) =>
//    case FinalExp.InvokeConstructor(constructor, args, tpe, loc) =>
//    case FinalExp.InvokeMethod(method, exp, args, tpe, loc) =>
//    case FinalExp.InvokeStaticMethod(method, args, tpe, loc) =>
//    case FinalExp.GetField(field, exp, tpe, loc) =>
//    case FinalExp.PutField(field, exp1, exp2, tpe, loc) =>
//    case FinalExp.GetStaticField(field, tpe, loc) =>
//    case FinalExp.PutStaticField(field, exp, tpe, loc) =>
//    case FinalExp.NewChannel(exp, tpe, loc) =>
//    case FinalExp.GetChannel(exp, tpe, loc) =>
//    case FinalExp.PutChannel(exp1, exp2, tpe, loc) =>
//    case FinalExp.SelectChannel(rules, default, tpe, loc) =>
//    case FinalExp.Spawn(exp, tpe, loc) =>
//    case FinalExp.Lazy(exp, tpe, loc) =>
//    case FinalExp.Force(exp, tpe, loc) =>
//    case FinalExp.FixpointConstraintSet(cs, tpe, loc) =>
//    case FinalExp.FixpointCompose(exp1, exp2, tpe, loc) =>
//    case FinalExp.FixpointSolve(exp, stf, tpe, loc) =>
//    case FinalExp.FixpointProject(pred, exp, tpe, loc) =>
//    case FinalExp.FixpointEntails(exp1, exp2, tpe, loc) =>
//    case FinalExp.FixpointFold(pred, init, f, constraints, tpe, loc) =>
//    case FinalExp.HoleError(sym, tpe, loc) =>
//    case FinalExp.MatchError(tpe, loc) =>
    case _ => ???
  }
}
