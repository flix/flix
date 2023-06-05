package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Ast.CallType
import ca.uwaterloo.flix.language.ast.CallByValueAst.{Expr, Stmt}
import ca.uwaterloo.flix.language.ast.{CallByValueAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object CallByValueAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: CallByValueAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case CallByValueAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case CallByValueAst.Case(sym, tpe, _) =>
            DocAst.Case(sym, TypePrinter.print(tpe))
        }.toList
        DocAst.Enum(ann, mod, sym, Nil, cases)
    }.toList
    val defs = root.defs.values.map {
      case CallByValueAst.Def(ann, mod, sym, cparams, fparams, stmt, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
          TypePrinter.print(tpe),
          print(stmt)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: CallByValueAst.Expr): DocAst.Expression = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => printVarSym(sym)
    case Expr.Closure(sym, closureArgs, _, _) => DocAst.Expression.ClosureLifted(sym, closureArgs.map(print))
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case CallByValueAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
    })
    case Expr.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expression.NewObject(name, clazz, TypePrinter.print(tpe), methods.map {
      case CallByValueAst.JvmMethod(ident, fparams, clo, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), TypePrinter.print(retTpe))
    })
    case Expr.ApplyAtomic(op, exps, tpe, _, _) => OperatorPrinter.print(op, exps.map(print), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `stmt`.
    */
  def print(stmt: CallByValueAst.Stmt): DocAst.Expression = stmt match {
    case Stmt.Ret(exp, _, _, _) => DocAst.Expression.Ret(print(exp))
    case Stmt.IfThenElse(exp, stmt1, stmt2, _, _, _) => DocAst.Expression.IfThenElse(print(exp), print(stmt1), print(stmt2))
    case Stmt.Branch(stmt, branches, _, _, _) => DocAst.Expression.Branch(print(stmt), MapOps.mapValues(branches)(print))
    case Stmt.JumpTo(sym, _, _, _) => DocAst.Expression.JumpTo(sym)
    case Stmt.LetVal(sym, stmt1, stmt2, _, _, _) => DocAst.Expression.LetVal(printVarSym(sym), Some(TypePrinter.print(stmt1.tpe)), print(stmt1), print(stmt2))
    case Stmt.LetRec(varSym, _, _, exp, stmt, _, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), Some(TypePrinter.print(exp.tpe)), print(exp), print(stmt))
    case Stmt.Scope(sym, stmt, _, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(stmt))
    case Stmt.ApplyClo(exp, exps, CallType.NonTailCall, _, _, _) => DocAst.Expression.ApplyClo(print(exp), exps.map(print))
    case Stmt.ApplyClo(exp, exps, CallType.TailCall, _, _, _) => DocAst.Expression.ApplyCloTail(print(exp), exps.map(print))
    case Stmt.ApplyDef(sym, exps, CallType.NonTailCall, _, _, _) => DocAst.Expression.ApplyDef(sym, exps.map(print))
    case Stmt.ApplyDef(sym, exps, CallType.TailCall, _, _, _) => DocAst.Expression.ApplyDefTail(sym, exps.map(print))
    case Stmt.ApplySelfTail(sym, _, actuals, _, _, _) => DocAst.Expression.ApplySelfTail(sym, actuals.map(print))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: CallByValueAst.FormalParam): DocAst.Expression.Ascription = {
    val CallByValueAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)

}
