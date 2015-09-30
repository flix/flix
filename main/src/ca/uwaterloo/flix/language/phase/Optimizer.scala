package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.TypedAst

object Optimizer {

  // TODO: Priority Low: Optimizer:
  // - Inline every non-recursive expression.
  // - Curry all lambdas to take a single argument.
  // - Flatten all pattern match expressions.
  // - Rewrite all expressions to A Normal Formal.
  // - Introduce bit packed encodings of enumerations.
  // - Apply partial evaluation to everything.
  // - Finally, compile to JVM bytecode.

  def optimize(tast: TypedAst.Root): TypedAst.Root = throw new UnsupportedOperationException()

}
