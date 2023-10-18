package ca.uwaterloo.flix.language.phase.mutator.numbers

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object Decrementer extends ExprMutator {
  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(lit - 1), tpe, loc))
      case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(lit - 1), tpe, loc))
      case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.subtract(java.math.BigDecimal.ONE)), tpe, loc))
      case Constant.Int8(lit) => Some(Expr.Cst(Constant.Int8((lit - 1).toByte), tpe, loc))
      case Constant.Int16(lit) => Some(Expr.Cst(Constant.Int16((lit - 1).toShort), tpe, loc))
      case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(lit - 1), tpe, loc))
      case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(lit - 1), tpe, loc))
      case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.subtract(java.math.BigInteger.ONE)), tpe, loc))
      case _ => None
    }
    case _ => None
  }
}
