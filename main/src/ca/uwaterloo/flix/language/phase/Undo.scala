package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.CallByValueAst.{AtomicOp, Expr, Stmt}
import ca.uwaterloo.flix.language.ast.{CallByValueAst, LiftedAst, SourceLocation}
import ca.uwaterloo.flix.util.InternalCompilerException


object Undo {

  private def todo: Nothing = throw InternalCompilerException("WIP", SourceLocation.Unknown)

  def run(root: CallByValueAst.Root)(implicit flix: Flix): LiftedAst.Root = {
    val defs = root.defs.view.mapValues(visitDef).toMap
    val enums = root.enums.view.mapValues(visitEnum).toMap
    LiftedAst.Root(defs, enums, root.entryPoint, root.sources)
  }

  private def visitDef(d: CallByValueAst.Def): LiftedAst.Def = {
    val CallByValueAst.Def(ann, mod, sym, fparams0, exp, tpe, loc) = d
    val fparams = fparams0.map(visitFormalParam)
    val e = visitStmt(exp)
    LiftedAst.Def(ann, mod, sym, fparams, e, tpe, loc)
  }

  private def visitExp(expr: CallByValueAst.Expr): LiftedAst.Expression = expr match {
    case Expr.Cst(cst, tpe, loc) =>
      LiftedAst.Expression.Cst(cst, tpe, loc)
    case Expr.Var(sym, tpe, loc) =>
      LiftedAst.Expression.Var(sym, tpe, loc)
    case Expr.Closure(sym, closureArgs0, tpe, loc) =>
      val closureArgs = closureArgs0.map(visitExp)
      LiftedAst.Expression.Closure(sym, closureArgs, tpe, loc)
    case Expr.TryCatch(exp, rules0, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rules = rules0.map(visitCatchRule)
      LiftedAst.Expression.TryCatch(e, rules, tpe, purity, loc)
    case Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod)
      LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc)
    case Expr.ApplyPure(AtomicOp.ArrayLength, List(base0), tpe, purity, loc) =>
      val base = visitExp(base0)
      LiftedAst.Expression.ArrayLength(base, tpe, purity, loc)
    case Expr.ApplyPure(AtomicOp.Cast, List(exp), tpe, purity, loc) =>
      val e = visitExp(exp)
      LiftedAst.Expression.Cast(e, tpe, purity, loc)
    case Expr.ApplyPure(AtomicOp.GetStaticField(field), Nil, tpe, purity, loc) =>
      LiftedAst.Expression.GetStaticField(field, tpe, purity, loc)
    case Expr.ApplyPure(AtomicOp.InvokeMethod(method), exp :: args0, tpe, purity, loc) =>
      val e = visitExp(exp)
      val args = args0.map(visitExp)
      LiftedAst.Expression.InvokeMethod(method, e, args, tpe, purity, loc)
    case Expr.ApplyPure(op, exps, _, _, loc) =>
      throw InternalCompilerException(s"Mis-matched arity of ${exps.length} with op '$op'", loc)
  }

  private def visitStmt(stmt: CallByValueAst.Stmt): LiftedAst.Expression = stmt match {
    case Stmt.Ret(exp, _, _, _) =>
      visitExp(exp)
    case Stmt.IfThenElse(exp, stmt1, stmt2, tpe, purity, loc) =>
      val e1 = visitExp(exp)
      val e2 = visitStmt(stmt1)
      val e3 = visitStmt(stmt2)
      LiftedAst.Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case Stmt.Branch(stmt, branches0, tpe, purity, loc) =>
      val e = visitStmt(stmt)
      val branches = branches0.view.mapValues(visitStmt).toMap
      LiftedAst.Expression.Branch(e, branches, tpe, purity, loc)
    case Stmt.JumpTo(sym, tpe, purity, loc) =>
      LiftedAst.Expression.JumpTo(sym, tpe, purity, loc)
    case Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc) =>
      val e1 = visitStmt(stmt1)
      val e2 = visitStmt(stmt2)
      LiftedAst.Expression.Let(sym, e1, e2, tpe, purity, loc)
    case Stmt.LetRecVal(varSym, index, defSym, exp, stmt, tpe, purity, loc) =>
      val e1 = visitExp(exp)
      val e2 = visitStmt(stmt)
      LiftedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
    case Stmt.Scope(sym, stmt, tpe, purity, loc) =>
      val e = visitStmt(stmt)
      LiftedAst.Expression.Scope(sym, e, tpe, purity, loc)
    case Stmt.ApplyClo(exp, args0, tpe, purity, loc) =>
      val e = visitExp(exp)
      val args = args0.map(visitExp)
      LiftedAst.Expression.ApplyClo(e, args, tpe, purity, loc)
    case Stmt.ApplyDef(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExp)
      LiftedAst.Expression.ApplyDef(sym, args, tpe, purity, loc)
    case Stmt.ApplyCloTail(exp, args0, tpe, purity, loc) =>
      val e = visitExp(exp)
      val args = args0.map(visitExp)
      LiftedAst.Expression.ApplyCloTail(e, args, tpe, purity, loc)
    case Stmt.ApplyDefTail(sym, args0, tpe, purity, loc) =>
      val args = args0.map(visitExp)
      LiftedAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc)
    case Stmt.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExp)
      LiftedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
    //    case Stmt.Do(sym, exps0, tpe, purity, loc) => todo
    //    case Stmt.Handle(sym, stmt, tpe, purity, loc) => todo
  }

  private def visitFormalParam(p: CallByValueAst.FormalParam): LiftedAst.FormalParam = {
    val CallByValueAst.FormalParam(sym, mod, tpe, loc) = p
    LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def visitCatchRule(rule: CallByValueAst.CatchRule): LiftedAst.CatchRule = rule match {
    case CallByValueAst.CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      LiftedAst.CatchRule(sym, clazz, e)
  }

  private def visitJvmMethod(m: CallByValueAst.JvmMethod): LiftedAst.JvmMethod = {
    val CallByValueAst.JvmMethod(ident, fparams0, clo0, retTpe, purity, loc) = m
    val fparams = fparams0.map(visitFormalParam)
    val clo = visitExp(clo0)
    LiftedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc)
  }

  private def visitEnum(e: CallByValueAst.Enum): LiftedAst.Enum = {
    val CallByValueAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) = e
    val cases = cases0.view.mapValues(visitEnumCase).toMap
    LiftedAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  private def visitEnumCase(c: CallByValueAst.Case): LiftedAst.Case = {
    val CallByValueAst.Case(sym, tpeDeprecated, loc) = c
    LiftedAst.Case(sym, tpeDeprecated, loc)
  }

}
