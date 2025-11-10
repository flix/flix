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

  test("Reachable.03") {
    val input =
      """
        |def main(): Unit =
        |    A.f()
        |
        |mod A {
        |    pub def f(): Unit = ()
        |}
        |
        |def f(): Unit = g()
        |
        |def g(): Unit = ()
        |""".stripMargin
    val expected = Set("main", "A.f")
    expectReachable(input, expected)
  }

  test("Reachable.04") {
    val input =
      """
        |def main(): Unit =
        |    A.f()
        |
        |mod A {
        |    pub def f(): Unit = B.g()
        |}
        |
        |mod B {
        |    pub def g(): Unit = ()
        |}
        |""".stripMargin
    val expected = Set("main", "A.f", "B.g")
    expectReachable(input, expected)
  }

  test("Reachable.05") {
    val input =
      """
        |@Test
        |def test01(): Unit = ()
        |""".stripMargin
    val expected = Set("test01")
    expectReachable(input, expected)
  }

  test("Reachable.06") {
    val input =
      """
        |@Test
        |def test01(): Unit = main()
        |
        |def main(): Unit = A.f()
        |
        |mod A {
        |    pub def f(): Unit = B.g()
        |}
        |
        |mod B {
        |    pub def g(): Unit = ()
        |}
        |
        |""".stripMargin
    val expected = Set("test01", "main", "A.f", "B.g")
    expectReachable(input, expected)
  }

  test("Reachable.08") {
    val input =
      """
        |@Test
        |def test01(): Unit = C.f()

        |mod C {
        |    pub def f(): Unit = ()
        |}
        |
        |""".stripMargin
    val expected = Set("test01", "C.f")
    expectReachable(input, expected)
  }

  test("Reachable.09") {
    val input =
      """
        |@Test
        |def test01(): Unit = C.f()
        |
        |def main(): Unit = A.f()
        |
        |mod A {
        |    pub def f(): Unit = B.g()
        |}
        |
        |mod B {
        |    pub def g(): Unit = ()
        |}
        |
        |mod C {
        |    pub def f(): Unit = ()
        |}
        |
        |""".stripMargin
    val expected = Set("test01", "main", "A.f", "B.g", "C.f")
    expectReachable(input, expected)
  }

  /** Asserts that the program contains exactly the functions `expected` after running [[Flix.treeshake]] */
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
