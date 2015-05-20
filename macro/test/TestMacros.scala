import org.scalatest.FunSuite

import Macros._

class TestMacros extends FunSuite {

  test("demo macro") {
    val f = demo(5)
    for (i <- 0 to 100) assertResult(i + 5)(f(i))
  }
}
