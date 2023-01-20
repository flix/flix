package ca.uwaterloo.flix.language.phase.unification

import org.scalatest.FunSuite
import SetFormula._

class TestSetFormulaAlg extends FunSuite {

  implicit val universe: Set[Int] = Set(1, 2, 3, 4)

  test("Minimize.Exhaustive.01") {
    // x1 ∩ ~x1 --> {}
    val input = And(Var(1), Not(Var(1)))
    assertResult(expected = SetFormulaAlg.mkEmpty())(actual = SetFormulaAlg.simplifyByExhaustiveEvaluation(input))
  }

  test("Minimize.Exhaustive.02") {
    // (x2 ∩ ~x2) ∪ {1} --> {1}
    val input = Or(And(Var(2), Not(Var(2))), Cst(Set(1)))
    assertResult(expected = Cst(Set(1)))(actual = SetFormulaAlg.simplifyByExhaustiveEvaluation(input))
  }

  test("Minimize.Exhaustive.03") {
    // ((x1 ∩ ~{1}) ∩ (x1 ∩ ~{1, 2, 3})) ∪ {4} --> {4}
    val input = Or(And(And(Var(1), Not(Cst(Set(1)))), And(Var(1), Not(Cst(Set(1, 2, 3))))), Cst(Set(4)))
    assertResult(expected = Cst(Set(4)))(actual = SetFormulaAlg.simplifyByExhaustiveEvaluation(input))
  }

}
