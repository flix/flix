package ca.uwaterloo.flix.language.phase.jvm2

import ca.uwaterloo.flix.language.ast.JvmAst
import java.lang.classfile.CodeBuilder

object GenExpr {
  // signature (needs java24)
  def gen(e: JvmAst.Expr, cb: CodeBuilder) : Unit = e match {
    case JvmAst.Expr.Cst(cst, _, _) =>
      // cst : value
      // tpe : type as ClassDesc
      // loc : source location
      GenCst.gen(cst, cb)

    case JvmAst.Expr.Var(_, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.ApplyAtomic(_, exprs, _, _, _) =>
      // op : operation
      // exprs : expressions passed into Op
      exprs.map(gen(_,cb)) // assume we generate each sub exp
      // tpe : type returned by operation
      // purity : purity of operation
      // loc : source location
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.ApplyClo(_, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.ApplyDef(_, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.ApplySelfTail(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.IfThenElse(_, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.Branch(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.JumpTo(_, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.Let(_, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.Stmt(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.Scope(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.TryCatch(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.RunWith(_, _, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.Do(_, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()

    case JvmAst.Expr.NewObject(_, _, _, _, _, _) =>
      cb.aconst_null()
      cb.athrow()
  }

  // in all cases push null on the code builder, then emit instruction athrow
  // generate classfiles that are valid, but with nothing implemented
  // start with constants and atomic expressions
}
