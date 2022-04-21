package ca.uwaterloo.flix.language.phase.unification

import org.scalatest.FunSuite
import BoolTable._
import BoolTable.Formula._

class TestBoolTable extends FunSuite {

  ignore("Minimize.01") {
    assertResult(expected = True)(actual = minimize(True))
  }

  ignore("Minimize.02") {
    assertResult(expected = True)(actual = minimize(Conj(True, True)))
  }

  test("Minimize.03") {
    assertResult(expected = False)(actual = minimize(Conj(True, False)))
  }

  ignore("Minimize.04") {
    assertResult(expected = Var(0))(actual = minimize(Var(0)))
  }

}
