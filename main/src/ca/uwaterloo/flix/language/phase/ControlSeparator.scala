package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{Ast, CallByValueAst, LiftedAst, SourceLocation, Symbol}

class ControlSeparator {

  def run(root: LiftedAst.Root)(implicit flix: Flix): CallByValueAst.Root = flix.phase("ControlSeparator") {
    ???
  }

  def visitExpAsStmt(exp: LiftedAst.Expression)(implicit ctx: Context, flix: Flix): CallByValueAst.Stmt = exp match {
    case Expression.Cst(cst, tpe, loc) =>
      ret(CallByValueAst.Expr.Cst(cst, tpe, loc))
    case Expression.Var(sym, tpe, loc) =>
      ret(CallByValueAst.Expr.Var(sym, tpe, loc))
    case Expression.Closure(sym, closureArgs0, tpe, loc) =>
      val closureArgs = closureArgs0.map(visitExpAsExpr)
      ret(CallByValueAst.Expr.Closure(sym, closureArgs, tpe, loc))
    case Expression.ApplyClo(exp, args0, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val args = args0.map(visitExpAsExpr)
      CallByValueAst.Stmt.ApplyClo(e, args, tpe, purity, loc)
    case Expression.ApplyDef(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExpAsExpr)
      CallByValueAst.Stmt.ApplyDef(sym, args, tpe, purity, loc)
    case Expression.ApplyCloTail(exp, args0, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val args = args0.map(visitExpAsExpr)
      CallByValueAst.Stmt.ApplyCloTail(e, args, tpe, purity, loc)
    case Expression.ApplyDefTail(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExpAsExpr)
      CallByValueAst.Stmt.ApplyDefTail(sym, args, tpe, purity, loc)
    case Expression.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExpAsExpr)
      CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
    case Expression.Unary(sop, op, exp, tpe, purity, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case Expression.Branch(exp, branches, tpe, purity, loc) => ???
    case Expression.JumpTo(sym, tpe, purity, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, purity, loc) => ???
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => ???
    case Expression.Region(tpe, loc) => ???
    case Expression.Scope(sym, exp, tpe, purity, loc) => ???
    case Expression.ScopeExit(exp1, exp2, tpe, purity, loc) => ???
    case Expression.Is(sym, exp, purity, loc) => ???
    case Expression.Tag(sym, exp, tpe, purity, loc) => ???
    case Expression.Untag(sym, exp, tpe, purity, loc) => ???
    case Expression.Index(base, offset, tpe, purity, loc) => ???
    case Expression.Tuple(elms, tpe, purity, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, purity, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, purity, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, loc) => ???
    case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case Expression.ArrayLength(base, tpe, purity, loc) => ???
    case Expression.Ref(exp, tpe, loc) => ???
    case Expression.Deref(exp, tpe, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, loc) => ???
    case Expression.InstanceOf(exp, clazz, loc) => ???
    case Expression.Cast(exp, tpe, purity, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, purity, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) => ???
    case Expression.GetField(field, exp, tpe, purity, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) => ???
    case Expression.GetStaticField(field, tpe, purity, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, purity, loc) => ???
    case Expression.NewObject(name, clazz, tpe, purity, methods, loc) => ???
    case Expression.Spawn(exp1, exp2, tpe, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, loc) => ???
    case Expression.HoleError(sym, tpe, loc) => ???
    case Expression.MatchError(tpe, loc) => ???
  }

  sealed trait Binding

  object Binding {
    case class Val(name: Symbol.VarSym, binding: CallByValueAst.Stmt) extends Binding
  }

  /**
    * Knows how to bind.
    */
  case class Context(var l: List[Binding]) {
    def bind(stmt: CallByValueAst.Stmt)(implicit flix: Flix): CallByValueAst.Expr.Var = {
      val loc = SourceLocation.Unknown
      val sym = Symbol.freshVarSym("cbv", Ast.BoundBy.Let, loc)
      l = Binding.Val(Symbol.freshVarSym(sym), stmt) :: l
      CallByValueAst.Expr.Var(sym, stmt.tpe, stmt.loc)
    }

  }

  /**
    * Translates the lifted expression into a CBV expression.
    * This often entail let-binding.
    */
  def visitExpAsExpr(exp: LiftedAst.Expression)(implicit ctx: Context, flix: Flix): CallByValueAst.Expr = exp match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => CallByValueAst.Expr.Cst(cst, tpe, loc)
    case LiftedAst.Expression.Var(sym, tpe, loc) => CallByValueAst.Expr.Var(sym, tpe, loc)
    case LiftedAst.Expression.Closure(sym, closureArgs0, tpe, loc) =>
      val closureArgs = closureArgs0.map(visitExpAsExpr)
      CallByValueAst.Expr.Closure(sym, closureArgs, tpe, loc)
    case LiftedAst.Expression.ApplyClo(exp, args0, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val args = args0.map(visitExpAsExpr)
      val ac = CallByValueAst.Stmt.ApplyClo(e, args, tpe, purity, loc)
      ctx.bind(ac)
    case LiftedAst.Expression.ApplyDef(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExpAsExpr)
      val ad = CallByValueAst.Stmt.ApplyDef(sym, args, tpe, purity, loc)
      ctx.bind(ad)
    case LiftedAst.Expression.ApplyCloTail(exp, args0, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val args = args0.map(visitExpAsExpr)
      val act = CallByValueAst.Stmt.ApplyCloTail(e, args, tpe, purity, loc)
      ctx.bind(act)
    case LiftedAst.Expression.ApplyDefTail(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExpAsExpr)
      val adt = CallByValueAst.Stmt.ApplyDefTail(sym, args, tpe, purity, loc)
      ctx.bind(adt)
    case LiftedAst.Expression.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExpAsExpr)
      val adt = CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
      ctx.bind(adt)
    case LiftedAst.Expression.Unary(sop, op, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      val ite = CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      ctx.bind(ite)
    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) => ???
    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) => ???
    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.Region(tpe, loc) => ???
    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.Is(sym, exp, purity, loc) => ???
    case LiftedAst.Expression.Tag(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Untag(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Index(base, offset, tpe, purity, loc) => ???
    case LiftedAst.Expression.Tuple(elms, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordEmpty(tpe, loc) => ???
    case LiftedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordExtend(field, value, rest, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordRestrict(field, rest, tpe, purity, loc) => ???
    case LiftedAst.Expression.ArrayLit(elms, tpe, loc) => ???
    case LiftedAst.Expression.ArrayNew(elm, len, tpe, loc) => ???
    case LiftedAst.Expression.ArrayLoad(base, index, tpe, loc) => ???
    case LiftedAst.Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case LiftedAst.Expression.ArrayLength(base, tpe, purity, loc) => ???
    case LiftedAst.Expression.Ref(exp, tpe, loc) => ???
    case LiftedAst.Expression.Deref(exp, tpe, loc) => ???
    case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) => ???
    case LiftedAst.Expression.InstanceOf(exp, clazz, loc) => ???
    case LiftedAst.Expression.Cast(exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeConstructor(constructor, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeMethod(method, exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeStaticMethod(method, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.GetField(field, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.GetStaticField(field, tpe, purity, loc) => ???
    case LiftedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) => ???
    case LiftedAst.Expression.Spawn(exp1, exp2, tpe, loc) => ???
    case LiftedAst.Expression.Lazy(exp, tpe, loc) => ???
    case LiftedAst.Expression.Force(exp, tpe, loc) => ???
    case LiftedAst.Expression.HoleError(sym, tpe, loc) => ???
    case LiftedAst.Expression.MatchError(tpe, loc) => ???
  }

  private def visitFormalParam(p: LiftedAst.FormalParam): CallByValueAst.FormalParam = {
    val LiftedAst.FormalParam(sym, mod, tpe, loc) = p
    CallByValueAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * Wrap an expression in a return statement.
    */
  private def ret(e: CallByValueAst.Expr): CallByValueAst.Stmt.Ret = {
    CallByValueAst.Stmt.Ret(e, e.tpe, e.purity, e.loc)
  }

}
