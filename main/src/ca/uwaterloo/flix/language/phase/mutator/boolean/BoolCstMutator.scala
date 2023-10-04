package ca.uwaterloo.flix.language.phase.mutator.boolean

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object BoolCstMutator extends ExprMutator {
  override def mutateExpr(exp: Expr): Option[Expr] = exp match {
    case Expr.Cst(Constant.Bool(lit), tpe, loc) => Option(Expr.Cst(Constant.Bool(!lit), tpe, loc))
    case _ => Option.empty
  }
}
