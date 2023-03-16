package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.ErasedAst
import ca.uwaterloo.flix.language.ast.ErasedAst.{Expression, IntrinsicOperator0}
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object ErasedPrinter {

  def print(e: ErasedAst.Expression): DocAst = e match {
    case Expression.Var(sym, _, _) =>
      SymbolPrinter.printWithOffset(sym)
    case Expression.Unary(sop, exp, _, _) =>
      DocAst.Unary(OperatorPrinter.print(sop), print(exp))
    case Expression.Binary(sop, _, exp1, exp2, _, _) =>
      DocAst.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
      DocAst.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, _, _) =>
      DocAst.Let(sym, None, print(exp1), print(exp2))
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => ???
    case Expression.Scope(sym, exp, tpe, loc) => ???
    case Expression.ScopeExit(exp1, exp2, tpe, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.NewObject(name, clazz, tpe, methods, loc) => ???
    case Expression.Intrinsic0(op, _, _) => op match {
      case IntrinsicOperator0.Cst(cst) => DocAst.AsIs(ConstantPrinter.print(cst))
      case IntrinsicOperator0.Region => DocAst.Region
      case IntrinsicOperator0.RecordEmpty => DocAst.RecordEmpty
      case IntrinsicOperator0.GetStaticField(field) => ???
      case IntrinsicOperator0.HoleError(sym) => DocAst.HoleError(sym)
      case IntrinsicOperator0.MatchError => DocAst.MatchError
    }
    case Expression.Intrinsic1(op, exp, tpe, loc) => ???
    case Expression.Intrinsic2(op, exp1, exp2, tpe, loc) => ???
    case Expression.Intrinsic3(op, exp1, exp2, exp3, tpe, loc) => ???
    case Expression.IntrinsicN(op, exps, tpe, loc) => ???
    case Expression.Intrinsic1N(op, exp, exps, tpe, loc) => ???
  }

}
