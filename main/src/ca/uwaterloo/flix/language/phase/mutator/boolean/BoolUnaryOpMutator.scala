package ca.uwaterloo.flix.language.phase.mutator.boolean

import ca.uwaterloo.flix.language.ast.SemanticOp.BoolOp
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object BoolUnaryOpMutator extends ExprMutator {
  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Unary(BoolOp.Not, exp, _, _, _) => Option(exp)
    case _ => Option.empty
  }
}
