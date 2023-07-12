package ca.uwaterloo.flix.language.phase.unification

object BoolFormulaTableMin extends Min[BoolFormula, Int] {
  /**
    * Minimizes the given Boolean formula.
    */
  override def minimize(f: BoolFormula)(implicit alg: BoolAlg2[BoolFormula, Int]): BoolFormula = {
    BoolFormulaTable.minimizeFormula(f)
  }
}
