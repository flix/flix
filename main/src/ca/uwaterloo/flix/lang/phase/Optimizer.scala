package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.TypedAst

object Optimizer {
  // TODO:
  // - Inline all expressions.
  // - Rewrite all lambdas to be single argument.
  // - Rewrite to a normal form (ANF)?
  // - Rewrite pattern matches?
  // - Reduce the number of unary and binary operators?
  // -
  // - Bit encodings?
  // - High-level optimizations?
  // TODO: An IR could allow a match where the pattern is ONLY a tag. and then someway to index tuples.

  def optimize(tast: TypedAst.Root): TypedAst.Root = ???
}
