package ca.uwaterloo.flix.language.phase.mutator.unary

import ca.uwaterloo.flix.language.ast.SemanticOp.{BoolOp, Int16Op, Int32Op, Int64Op, Int8Op}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object UnaryMutator extends ExprMutator {

  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Unary(BoolOp.Not, exp, _, _, _) => Some(exp)
    case Expr.Unary(Int8Op.Not, exp, _, _, _) => Some(exp)
    case Expr.Unary(Int16Op.Not, exp, _, _, _) => Some(exp)
    case Expr.Unary(Int32Op.Not, exp, _, _, _) => Some(exp)
    case Expr.Unary(Int64Op.Not, exp, _, _, _) => Some(exp)
    case _ => None
  }
}
