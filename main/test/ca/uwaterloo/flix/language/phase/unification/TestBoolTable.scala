package ca.uwaterloo.flix.language.phase.unification

import org.scalatest.FunSuite
import BoolTable._
import BoolTable.Formula._

class TestBoolTable extends FunSuite {

  test("Minimize.True.01") {
    assertResult(expected = True)(actual = minimizeFormula(True))
  }

  test("Minimize.True.02") {
    assertResult(expected = True)(actual = minimizeFormula(Neg(False)))
  }

  test("Minimize.True.03") {
    assertResult(expected = True)(actual = minimizeFormula(Conj(True, True)))
  }

  test("Minimize.True.04") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, True)))
  }

  test("Minimize.True.05") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, False)))
  }

  test("Minimize.True.06") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(False, True)))
  }

  // TODO: Add more tests that follow the same structure, but for false.

  test("Minimize.Var1.01") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Var(0)))
  }

  test("Minimize.Var1.02") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Neg(Neg(Var(0)))))
  }

  test("Minimize.Var1.03") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Var(0), Var(0))))
  }

  test("Minimize.Var1.04") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Var(0), True)))
  }

  test("Minimize.Var1.05") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(True, Var(0))))
  }

  // TODO: Add more Minimize.Var1 tests, but with Disj.

  test("Minimize.Var1AndVar2.01") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.02") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.03") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(0), Conj(Var(1), Var(1)))))
  }

  test("Minimize.Var1AndVar2.04") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Conj(Var(1), Var(1)), Var(0))))
  }

  // TODO: Add more tests

  // TODO: Add tests with up to 3-4 variables.

}
