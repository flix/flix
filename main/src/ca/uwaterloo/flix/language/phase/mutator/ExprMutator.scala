package ca.uwaterloo.flix.language.phase.mutator

import ca.uwaterloo.flix.language.ast.TypedAst.Expr

trait ExprMutator {
  def mutateExpr(exp: Expr): Option[Expr]
}
