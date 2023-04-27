package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{Ast, CallByValueAst, LiftedAst, Purity, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object ControlSeparator {

  private def todo: Nothing = throw InternalCompilerException("WIP", SourceLocation.Unknown)

  def run(root: LiftedAst.Root)(implicit flix: Flix): CallByValueAst.Root = flix.phase("ControlSeparator") {
    val LiftedAst.Root(defs0, enums0, entryPoint, sources) = root
    val defs = defs0.view.mapValues(visitDef).toMap
    val enums = enums0.view.mapValues(visitEnum).toMap
    CallByValueAst.Root(defs, enums, entryPoint, sources)
  }

  def visitDef(defn: LiftedAst.Def)(implicit flix: Flix): CallByValueAst.Def = {
    val LiftedAst.Def(ann, mod, sym, fparams0, exp, tpe, loc) = defn
    val fparams = fparams0.map(visitFormalParam)
    // important! reify bindings later
    implicit val ctx: Context = Context(Nil)
    val stmt = visitExpAsStmt(exp)
    val body = ctx.reifyBindings(stmt)
    CallByValueAst.Def(ann, mod, sym, fparams, body, tpe, loc)
  }

  def visitEnum(e: LiftedAst.Enum): CallByValueAst.Enum = {
    val LiftedAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) = e
    val cases = cases0.view.mapValues(visitEnumCase).toMap
    CallByValueAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  def visitEnumCase(c: LiftedAst.Case): CallByValueAst.Case = {
    val LiftedAst.Case(sym, tpeDeprecated, loc) = c
    CallByValueAst.Case(sym, tpeDeprecated, loc)
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
    case Expression.Unary(sop, op, exp, tpe, purity, loc) => todo
    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => todo
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case Expression.Branch(exp, branches, tpe, purity, loc) => todo
    case Expression.JumpTo(sym, tpe, purity, loc) => todo
    case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val stmt1 = visitExpAsStmt(exp1)
      val stmt2 = visitExpAsStmt(exp2)
      CallByValueAst.Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc)
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => todo
    case Expression.Region(tpe, loc) => todo
    case Expression.Scope(sym, exp, tpe, purity, loc) => todo
    case Expression.ScopeExit(exp1, exp2, tpe, purity, loc) => todo
    case Expression.Is(sym, exp, purity, loc) => todo
    case Expression.Tag(sym, exp, tpe, purity, loc) => todo
    case Expression.Untag(sym, exp, tpe, purity, loc) => todo
    case Expression.Index(base, offset, tpe, purity, loc) => todo
    case Expression.Tuple(elms, tpe, purity, loc) => todo
    case Expression.RecordEmpty(tpe, loc) => todo
    case Expression.RecordSelect(exp, field, tpe, purity, loc) => todo
    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) => todo
    case Expression.RecordRestrict(field, rest, tpe, purity, loc) => todo
    case Expression.ArrayLit(elms, tpe, loc) => todo
    case Expression.ArrayNew(elm, len, tpe, loc) => todo
    case Expression.ArrayLoad(base, index, tpe, loc) => todo
    case Expression.ArrayStore(base, index, elm, tpe, loc) => todo
    case Expression.ArrayLength(base, tpe, purity, loc) => todo
    case Expression.Ref(exp, tpe, loc) => todo
    case Expression.Deref(exp, tpe, loc) => todo
    case Expression.Assign(exp1, exp2, tpe, loc) => todo
    case Expression.InstanceOf(exp, clazz, loc) => todo
    case Expression.Cast(exp, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      ret(CallByValueAst.Expr.ApplyPure(CallByValueAst.AtomicOp.Cast, List(e), tpe, purity, loc))
    case Expression.TryCatch(exp, rules, tpe, purity, loc) => todo
    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) => todo
    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) => todo
    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) => todo
    case Expression.GetField(field, exp, tpe, purity, loc) => todo
    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) => todo
    case Expression.GetStaticField(field, tpe, purity, loc) => todo
    case Expression.PutStaticField(field, exp, tpe, purity, loc) => todo
    case Expression.NewObject(name, clazz, tpe, purity, methods, loc) => todo
    case Expression.Spawn(exp1, exp2, tpe, loc) => todo
    case Expression.Lazy(exp, tpe, loc) => todo
    case Expression.Force(exp, tpe, loc) => todo
    case Expression.HoleError(sym, tpe, loc) => todo
    case Expression.MatchError(tpe, loc) => todo
  }

  sealed trait Binding

  object Binding {
    case class Val(sym: Symbol.VarSym, binding: CallByValueAst.Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Binding
  }

  /**
    * Knows how to bind.
    */
  case class Context(var l: List[Binding]) {
    def bind(stmt: CallByValueAst.Stmt)(implicit flix: Flix): CallByValueAst.Expr.Var = {
      val loc = SourceLocation.Unknown
      val sym = Symbol.freshVarSym("cbv", Ast.BoundBy.Let, loc)
      l = Binding.Val(Symbol.freshVarSym(sym), stmt, stmt.tpe, stmt.purity, stmt.loc.asSynthetic) :: l
      CallByValueAst.Expr.Var(sym, stmt.tpe, stmt.loc)
    }

    def reifyBindings(stmt: CallByValueAst.Stmt): CallByValueAst.Stmt = {
      l.foldLeft(stmt) {
        case (acc, Binding.Val(sym, binding, tpe, purity, loc)) =>
          CallByValueAst.Stmt.LetVal(sym, binding, acc, tpe, purity, loc)
      }
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
    case LiftedAst.Expression.Unary(sop, op, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => todo
    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      val ite = CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      ctx.bind(ite)
    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) => todo
    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) => todo
    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) => todo
    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => todo
    case LiftedAst.Expression.Region(tpe, loc) => todo
    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) => todo
    case LiftedAst.Expression.Is(sym, exp, purity, loc) => todo
    case LiftedAst.Expression.Tag(sym, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.Untag(sym, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.Index(base, offset, tpe, purity, loc) => todo
    case LiftedAst.Expression.Tuple(elms, tpe, purity, loc) => todo
    case LiftedAst.Expression.RecordEmpty(tpe, loc) => todo
    case LiftedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) => todo
    case LiftedAst.Expression.RecordExtend(field, value, rest, tpe, purity, loc) => todo
    case LiftedAst.Expression.RecordRestrict(field, rest, tpe, purity, loc) => todo
    case LiftedAst.Expression.ArrayLit(elms, tpe, loc) => todo
    case LiftedAst.Expression.ArrayNew(elm, len, tpe, loc) => todo
    case LiftedAst.Expression.ArrayLoad(base, index, tpe, loc) => todo
    case LiftedAst.Expression.ArrayStore(base, index, elm, tpe, loc) => todo
    case LiftedAst.Expression.ArrayLength(base, tpe, purity, loc) => todo
    case LiftedAst.Expression.Ref(exp, tpe, loc) => todo
    case LiftedAst.Expression.Deref(exp, tpe, loc) => todo
    case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) => todo
    case LiftedAst.Expression.InstanceOf(exp, clazz, loc) => todo
    case LiftedAst.Expression.Cast(exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) => todo
    case LiftedAst.Expression.InvokeConstructor(constructor, args, tpe, purity, loc) => todo
    case LiftedAst.Expression.InvokeMethod(method, exp, args0, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val args = args0.map(visitExpAsExpr)
      CallByValueAst.Expr.ApplyPure(CallByValueAst.AtomicOp.InvokeMethod(method), e :: args, tpe, purity, loc)
    case LiftedAst.Expression.InvokeStaticMethod(method, args, tpe, purity, loc) => todo
    case LiftedAst.Expression.GetField(field, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) => todo
    case LiftedAst.Expression.GetStaticField(field, tpe, purity, loc) =>
      CallByValueAst.Expr.ApplyPure(CallByValueAst.AtomicOp.GetStaticField(field), Nil, tpe, purity, loc)
    case LiftedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) => todo
    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) => todo
    case LiftedAst.Expression.Spawn(exp1, exp2, tpe, loc) => todo
    case LiftedAst.Expression.Lazy(exp, tpe, loc) => todo
    case LiftedAst.Expression.Force(exp, tpe, loc) => todo
    case LiftedAst.Expression.HoleError(sym, tpe, loc) => todo
    case LiftedAst.Expression.MatchError(tpe, loc) => todo
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
