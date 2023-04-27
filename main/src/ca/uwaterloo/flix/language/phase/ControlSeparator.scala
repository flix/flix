package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{CallByValueAst, LiftedAst, Symbol}

class ControlSeparator {

  type TmpValue = Int
  def run(root: LiftedAst.Root)(implicit flix: Flix): CallByValueAst.Root = flix.phase("ControlSeparator") {

  }

  def visitExpAsStmt(exp: LiftedAst.Expression): CallByValueAst.Stmt = ???

  sealed trait Binding

  object Binding {
    case class Val(name: Symbol.VarSym, binding: CallByValueAst.Stmt) extends Binding
  }

  /**
    * Knows how to bind.
    */
  case class Context(var l: List[Binding]) {
    def bind(stmt: CallByValueAst.Stmt)(implicit flix: Flix): CallByValueAst.Expr.Var = {
      l = Binding.Val(Symbol.freshVarSym(???), stmt) :: l
      CallByValueAst.Expr.Var()
    }

  }

  def visitExpAsExpr(exp: LiftedAst.Expression)(implicit ctx: Context, flix: Flix): CallByValueAst.Expr = exp match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => CallByValueAst.Expr.Cst(cst, tpe, loc)
    case LiftedAst.Expression.Var(sym, tpe, loc) => CallByValueAst.Expr.Var(sym, tpe, loc)
    case LiftedAst.Expression.Closure(sym, closureArgs, tpe, loc) =>
      CallByValueAst.Expr.Closure(sym, closureArgs, tpe, loc)
    case LiftedAst.Expression.ApplyClo(exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyDef(sym, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyCloTail(exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) => ???
    case LiftedAst.Expression.Unary(sop, op, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => ???



    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>

      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)

      CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, loc)






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

}
