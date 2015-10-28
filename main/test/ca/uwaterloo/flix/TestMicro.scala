package ca.uwaterloo.flix

import org.scalatest.FunSuite

class TestMicro extends FunSuite {

  test("Micro001") {
    val s =
      """rel R(a: Int);
        |
        |R(1).
        |assert R(1).
        |
      """.stripMargin

    Flix.solve(s)
  }

}
