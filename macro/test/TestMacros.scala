import org.scalatest.FunSuite

import impl.logic.Value
import Macros._

class TestMacros extends FunSuite {

  test("demo macro") {
    val f = demo(5)
    for (i <- 0 to 100) assertResult(i + 5)(f(i))
  }

  test("Value macro") {
    def foo(n: Int) = n * 42
    val v = Value.Int(2)
    assertResult(Value.Int(84))(m(foo _)(v))
  }

}
