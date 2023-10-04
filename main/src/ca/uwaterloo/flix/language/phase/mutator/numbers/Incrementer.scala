package ca.uwaterloo.flix.language.phase.mutator.numbers

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object Incrementer extends ExprMutator {
  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Float32(lit) => Option(Expr.Cst(Constant.Float32(lit + 1), tpe, loc))
      case Constant.Float64(lit) => Option(Expr.Cst(Constant.Float64(lit + 1), tpe, loc))
      case Constant.BigDecimal(lit) => Option(Expr.Cst(Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)), tpe, loc))
      case Constant.Int8(lit) => Option(Expr.Cst(Constant.Int8((lit + 1).toByte), tpe, loc))
      case Constant.Int16(lit) => Option(Expr.Cst(Constant.Int16((lit + 1).toShort), tpe, loc))
      case Constant.Int32(lit) => Option(Expr.Cst(Constant.Int32(lit + 1), tpe, loc))
      case Constant.Int64(lit) => Option(Expr.Cst(Constant.Int64(lit + 1), tpe, loc))
      case Constant.BigInt(lit) => Option(Expr.Cst(Constant.BigInt(lit.add(java.math.BigInteger.ONE)), tpe, loc))
      case _ => Option.empty
    }
    case _ => Option.empty
  }
}
