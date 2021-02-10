package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.JType
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, FinalAst}
import ca.uwaterloo.flix.util.Validation
import ErasedAst.{Expression => ErasedExp}
import FinalAst.{ConstraintParam, Expression => FinalExp}

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
    case FinalExp.Closure(sym, freeVars, _, tpe, loc) =>
      val newFreeVars = freeVars.map({ case FinalAst.FreeVar(sym, tpe) => ErasedAst.FreeVar(sym, tpe) })
      castExp(ErasedExp.Closure(sym, newFreeVars, tpe, loc))
    case FinalExp.ApplyClo(exp, args, tpe, loc) =>
      castExp(ErasedExp.ApplyClo(visitExp(exp), args.map(visitExp), tpe, loc))
    case FinalExp.ApplyDef(sym, args, tpe, loc) =>
      castExp(ErasedExp.ApplyDef(sym, args.map(visitExp), tpe, loc))
    case FinalExp.ApplyCloTail(exp, args, tpe, loc) =>
      castExp(ErasedExp.ApplyCloTail(visitExp(exp), args.map(visitExp), tpe, loc))
    case FinalExp.ApplyDefTail(sym, args, tpe, loc) =>
      castExp(ErasedExp.ApplyDefTail(sym, args.map(visitExp), tpe, loc))
    case FinalExp.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val newFormals = formals.map({ case FinalAst.FormalParam(sym, tpe) => ErasedAst.FormalParam(sym, tpe) })
      castExp(ErasedExp.ApplySelfTail(sym, newFormals, actuals.map(visitExp), tpe, loc))
    case FinalExp.Unary(sop, op, exp, tpe, loc) =>
      castExp(ErasedExp.Unary(sop, op, visitExp(exp), tpe, loc))
    case FinalExp.Binary(sop, op, exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp[PrimInt32](exp1)
      val e2 = visitExp[T](exp2)
      val e3 = visitExp[T](exp3)
      ErasedExp.IfThenElse(e1, e2, e3, tpe, loc)
    case FinalExp.Branch(exp, branches, tpe, loc) =>
      val newBranches = branches.map({ case (label, branchExp) => (label, visitExp(branchExp)) })
      ErasedExp.Branch(visitExp(exp), newBranches, tpe, loc)
    case FinalExp.JumpTo(sym, tpe, loc) =>
      ErasedExp.JumpTo(sym, tpe, loc)
    case FinalExp.Let(sym, exp1, exp2, tpe, loc) =>
      ErasedExp.Let(sym, visitExp(exp1), visitExp(exp2), tpe, loc)
    case FinalExp.Is(sym, tag, exp, loc) =>
      castExp(ErasedExp.Is(sym, tag, visitExp(exp), loc))
    case FinalExp.Tag(sym, tag, exp, tpe, loc) =>
      castExp(ErasedExp.Tag(sym, tag, visitExp(exp), tpe, loc))
    case FinalExp.Untag(sym, tag, exp, tpe, loc) =>
      ErasedExp.Untag(sym, tag, visitExp(exp), tpe, loc)
    case FinalExp.Index(base, offset, tpe, loc) =>
      val b = visitExp[JObject](base)
      val e = ErasedExp.Index(b, offset, tpe, loc)
      ErasedExp.Cast(e, tpe, loc)
    case FinalExp.Tuple(elms, tpe, loc) =>
      castExp(ErasedExp.Tuple(elms.map(visitExp), tpe, loc))
    case FinalExp.RecordEmpty(tpe, loc) =>
      castExp(ErasedExp.RecordEmpty(tpe, loc))
    case FinalExp.RecordSelect(exp, field, tpe, loc) =>
      ErasedExp.RecordSelect(visitExp(exp), field, tpe, loc)
    case FinalExp.RecordExtend(field, value, rest, tpe, loc) =>
      castExp(ErasedExp.RecordExtend(field, visitExp(value), visitExp(rest), tpe, loc))
    case FinalExp.RecordRestrict(field, rest, tpe, loc) =>
      castExp(ErasedExp.RecordRestrict(field, visitExp(rest), tpe, loc))
    case FinalExp.ArrayLit(elms, tpe, loc) =>
      castExp(ErasedExp.ArrayLit(elms.map(visitExp), tpe, loc))
    case FinalExp.ArrayNew(elm, len, tpe, loc) =>
      castExp(ErasedExp.ArrayNew(visitExp(elm), visitExp(len), tpe, loc))
    case FinalExp.ArrayLoad(base, index, tpe, loc) =>
      ErasedExp.ArrayLoad(visitExp(base), visitExp(index), tpe, loc)
    case FinalExp.ArrayStore(base, index, elm, tpe, loc) =>
      castExp(ErasedExp.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), tpe, loc))
    case FinalExp.ArrayLength(base, tpe, loc) =>
      castExp(ErasedExp.ArrayLength(visitExp(base), tpe, loc))
    case FinalExp.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      castExp(ErasedExp.ArraySlice(visitExp(base), visitExp(beginIndex), visitExp(endIndex), tpe, loc))
    case FinalExp.Ref(exp, tpe, loc) =>
      castExp(ErasedExp.Ref(visitExp(exp), tpe, loc))
    case FinalExp.Deref(exp, tpe, loc) =>
      ErasedExp.Deref(visitExp(exp), tpe, loc)
    case FinalExp.Assign(exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.Assign(visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.Existential(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      castExp(ErasedExp.Existential(ErasedAst.FormalParam(sym, tpe), visitExp(exp), loc))
    case FinalExp.Universal(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      castExp(ErasedExp.Universal(ErasedAst.FormalParam(sym, tpe), visitExp(exp), loc))
    case FinalExp.Cast(exp, tpe, loc) =>
      ErasedExp.Cast(visitExp(exp), tpe, loc)
    case FinalExp.TryCatch(exp, rules, tpe, loc) =>
      val newRules = rules.map({ case FinalAst.CatchRule(sym, clazz, exp) => ErasedAst.CatchRule[T](sym, clazz, visitExp(exp)) })
      ErasedExp.TryCatch(visitExp(exp), newRules, tpe, loc)
    case FinalExp.InvokeConstructor(constructor, args, tpe, loc) =>
      castExp(ErasedExp.InvokeConstructor(constructor, args.map(visitExp), tpe, loc))
    case FinalExp.InvokeMethod(method, exp, args, tpe, loc) =>
      ErasedExp.InvokeMethod(method, visitExp(exp), args.map(visitExp), tpe, loc)
    case FinalExp.InvokeStaticMethod(method, args, tpe, loc) =>
      ErasedExp.InvokeStaticMethod(method, args.map(visitExp), tpe, loc)
    case FinalExp.GetField(field, exp, tpe, loc) =>
      ErasedExp.GetField(field, visitExp(exp), tpe, loc)
    case FinalExp.PutField(field, exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.PutField(field, visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.GetStaticField(field, tpe, loc) =>
      ErasedExp.GetStaticField(field, tpe, loc)
    case FinalExp.PutStaticField(field, exp, tpe, loc) =>
      castExp(ErasedExp.PutStaticField(field, visitExp(exp), tpe, loc))
    case FinalExp.NewChannel(exp, tpe, loc) =>
      castExp(ErasedExp.NewChannel(visitExp(exp), tpe, loc))
    case FinalExp.GetChannel(exp, tpe, loc) =>
      ErasedExp.GetChannel(visitExp(exp), tpe, loc)
    case FinalExp.PutChannel(exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.PutChannel(visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.SelectChannel(rules, default, tpe, loc) =>
      val newRules = rules.map({ case FinalAst.SelectChannelRule(sym, chan, exp) => ErasedAst.SelectChannelRule[T](sym, visitExp(chan), visitExp(exp)) })
      ErasedExp.SelectChannel(newRules, default.map(visitExp[T]), tpe, loc)
    case FinalExp.Spawn(exp, tpe, loc) =>
      castExp(ErasedExp.Spawn(visitExp(exp), tpe, loc))
    case FinalExp.Lazy(exp, tpe, loc) =>
      castExp(ErasedExp.Lazy(visitExp(exp), tpe, loc))
    case FinalExp.Force(exp, tpe, loc) =>
      ErasedExp.Force(visitExp(exp), tpe, loc)
    case FinalExp.FixpointConstraintSet(cs, tpe, loc) =>
      val newCs = cs.map({
        case FinalAst.Constraint(cparams, head, body, loc) =>
          val newHead = head match {
            case FinalAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
              ErasedAst.Predicate.Head.Atom(pred, den, terms.map(visitTermHead), tpe, loc)
            case FinalAst.Predicate.Head.Union(exp, terms, tpe, loc) =>
              ErasedAst.Predicate.Head.Union(visitExp(exp), terms.map(visitTermHead), tpe, loc)
          }
          val newCparams = cparams.map(visitConstraintParam)
          val newBody = body.map(visitPredicateBody)
          ErasedAst.Constraint(newCparams, newHead, newBody, loc)
      })
      castExp(ErasedExp.FixpointConstraintSet(newCs, tpe, loc))
    case FinalExp.FixpointCompose(exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.FixpointCompose(visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.FixpointSolve(exp, stf, tpe, loc) =>
      castExp(ErasedExp.FixpointSolve(visitExp(exp), stf, tpe, loc))
    case FinalExp.FixpointProject(pred, exp, tpe, loc) =>
      castExp(ErasedExp.FixpointProject(pred, visitExp(exp), tpe, loc))
    case FinalExp.FixpointEntails(exp1, exp2, tpe, loc) =>
      castExp(ErasedExp.FixpointEntails(visitExp(exp1), visitExp(exp2), tpe, loc))
    case FinalExp.FixpointFold(pred, init, f, constraints, tpe, loc) =>
      def visitVar[TT <: JType](v: FinalExp.Var): ErasedExp.Var[TT] = visitExp(v).asInstanceOf[ErasedExp.Var[TT]]
      ErasedExp.FixpointFold(pred, visitVar(init), visitVar(f), visitVar(constraints), tpe, loc)
    case FinalExp.HoleError(sym, tpe, loc) =>
      ErasedExp.HoleError(sym, tpe, loc)
    case FinalExp.MatchError(tpe, loc) =>
      ErasedExp.MatchError(tpe, loc)
  }

  def visitTermHead(head: FinalAst.Term.Head): ErasedAst.Term.Head = head match {
    case FinalAst.Term.Head.QuantVar(sym, tpe, loc) => ErasedAst.Term.Head.QuantVar(sym, tpe, loc)
    case FinalAst.Term.Head.CapturedVar(sym, tpe, loc) => ErasedAst.Term.Head.CapturedVar(sym, tpe, loc)
    case FinalAst.Term.Head.Lit(sym, tpe, loc) => ErasedAst.Term.Head.Lit(sym, tpe, loc)
    case FinalAst.Term.Head.App(exp, args, tpe, loc) => ErasedAst.Term.Head.App(visitExp(exp), args, tpe, loc)
  }

  def visitTermBody(body: FinalAst.Term.Body): ErasedAst.Term.Body = body match {
    case FinalAst.Term.Body.Wild(tpe, loc) => ErasedAst.Term.Body.Wild(tpe, loc)
    case FinalAst.Term.Body.QuantVar(sym, tpe, loc) => ErasedAst.Term.Body.QuantVar(sym, tpe, loc)
    case FinalAst.Term.Body.CapturedVar(sym, tpe, loc) => ErasedAst.Term.Body.CapturedVar(sym, tpe, loc)
    case FinalAst.Term.Body.Lit(sym, tpe, loc) => ErasedAst.Term.Body.Lit(sym, tpe, loc)
  }

  def visitConstraintParam(param: FinalAst.ConstraintParam): ErasedAst.ConstraintParam = param match {
    case ConstraintParam.HeadParam(sym, tpe, loc) => ErasedAst.ConstraintParam.HeadParam(sym, tpe, loc)
    case ConstraintParam.RuleParam(sym, tpe, loc) => ErasedAst.ConstraintParam.RuleParam(sym, tpe, loc)
  }

  def visitPredicateBody(body: FinalAst.Predicate.Body): ErasedAst.Predicate.Body = body match {
    case FinalAst.Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      ErasedAst.Predicate.Body.Atom(pred, den, polarity, terms.map(visitTermBody), tpe, loc)
    case FinalAst.Predicate.Body.Guard(exp, terms, loc) =>
      ErasedAst.Predicate.Body.Guard(visitExp(exp), terms.map(visitTermBody), loc)
  }
}
