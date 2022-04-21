package ca.uwaterloo.flix.language.phase.unification

import org.scalatest.FunSuite
import BoolTable._
import BoolTable.Formula._

class TestBoolTable extends FunSuite {

  test("Minimize.01") {
    assertResult(expected = True)(actual = minimize(True))
  }

  test("Minimize.02") {
    assertResult(expected = False)(actual = minimize(False))
  }

  test("Minimize.03") {
    assertResult(expected = True)(actual = minimize(Conj(True, True)))
  }

  test("Minimize.04") {
    assertResult(expected = False)(actual = minimize(Conj(True, False)))
  }

  test("Minimize.05") {
    assertResult(expected = Var(0))(actual = minimize(Var(0)))
  }

}
