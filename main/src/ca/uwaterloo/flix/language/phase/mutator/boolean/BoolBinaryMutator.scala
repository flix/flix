package ca.uwaterloo.flix.language.phase.mutator.boolean

import ca.uwaterloo.flix.language.ast.SemanticOp.BoolOp
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object BoolBinaryMutator extends ExprMutator {
  private val binaryBoolOpMutants: Map[BoolOp, BoolOp] = Map(
    BoolOp.And -> BoolOp.Or,
    BoolOp.Or -> BoolOp.And,
    BoolOp.Eq -> BoolOp.Neq,
    BoolOp.Neq -> BoolOp.Eq,
  )

  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Binary(sop: BoolOp, exp1, exp2, tpe, eff, loc) => sop match {
      case BoolOp.And | BoolOp.Or | BoolOp.Eq | BoolOp.Neq =>
        Some(Expr.Binary(binaryBoolOpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case _ => None
  }
}
