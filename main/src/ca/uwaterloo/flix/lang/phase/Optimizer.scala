package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.TypedAst

object Optimizer {
  // TODO:
  // - Inline all expressions.
  // - Rewrite all lambdas to be single argument.
  // - Rewrite to a normal form (ANF)?
  // -
  // - Bit encodings?
  // - High-level optimizations?

  def optimize(tast: TypedAst.Root): TypedAst.Root = ???
}
