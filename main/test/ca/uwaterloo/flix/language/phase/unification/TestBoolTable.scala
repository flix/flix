package ca.uwaterloo.flix.language.phase.unification

import org.scalatest.FunSuite
import BoolTable.Formula._

class TestBoolTable extends FunSuite {

  test("Minimize.01") {
    val x = True
    val y = True
    assertResult(x)(BoolTable.minimize(y))
  }

  test("Minimize.02") {
    val x = Conj(True, True)
    val y = True
    assertResult(x)(BoolTable.minimize(y))
  }

}
