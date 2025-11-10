package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTypedAstTreeShaker extends AnyFunSuite with TestUtils {

  test("Reachable.01") {
    val input =
      """
        |def main(): Unit = ()
        |""".stripMargin
    val expected = Set("main")
    expectReachable(input, expected)
  }

  test("Reachable.02") {
    val input =
      """
        |def main(): Unit =
        |    f()
        |
        |def f(): Unit = g()
        |
        |def g(): Unit = ()
        |""".stripMargin
    val expected = Set("main", "f", "g")
    expectReachable(input, expected)
  }

  private def expectReachable(input: String, expected: Set[String], options: Options = Options.TestWithLibNix): Unit = {
    val (optRoot, errors) = check(input, options)

    if (errors.nonEmpty) {
      fail(s"Expected Success, but got Failure(s): ${errors.map(e => e.getClass)}")
    }

    val root = optRoot.get
    val flix = new Flix()
    flix.setOptions(options)

    val afterShake = flix.treeshake(root)
    val actual = afterShake.defs.keys.map(_.toString).toSet
    assert(actual == expected)
  }

}
