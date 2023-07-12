package ca.uwaterloo.flix.language.phase.unification

object BoolFormulaSat extends Sat[BoolFormula, Int] {
  /**
    * Returns true if the given Boolean formula is satisfiable.
    */
  override def satisfiable(f: BoolFormula)(implicit alg: BoolAlg2[BoolFormula, Int]): Boolean = f match {
    case BoolFormula.True => true
    case BoolFormula.False => false
    case BoolFormula.Var(_) => true
    case _ => evaluateAll(f, alg.freeVars(f).toList, List.empty)
  }

  /**
    * Enumerates all assignments to `f` and checks if one of them is satisfiable.
    */
  private def evaluateAll(f: BoolFormula, l: List[Int], env: List[Int]): Boolean = l match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(f, env)
    case x :: xs =>
      // Recurse on two cases: x = false and x = true.
      evaluateAll(f, xs, env) || evaluateAll(f, xs, x :: env)
  }

  /**
    * Computes the truth value of the formula `f` assuming the variables in `trueVars`
    * are true and the rest are false.
    */
  private def evaluate(f: BoolFormula, trueVars: List[Int]): Boolean = f match {
    case BoolFormula.True => true
    case BoolFormula.False => false
    case BoolFormula.Var(x) => trueVars.contains(x)
    case BoolFormula.Not(f1) => !evaluate(f1, trueVars)
    case BoolFormula.Or(f1, f2) => evaluate(f1, trueVars) || evaluate(f2, trueVars)
    case BoolFormula.And(f1, f2) => evaluate(f1, trueVars) && evaluate(f2, trueVars)
  }
}
