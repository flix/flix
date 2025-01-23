package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class EffectLockSuite extends AnyFunSuite {

  test("EffectLock.01") {
    val input = {
      """
        |def main(): Unit =
        |    g(f)
        |
        |def f(): Unit = ()
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
        |def a(): Unit = ()
        |
      """.stripMargin
    }
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val result = flix.effectLock().get.defs.keys.map(_.text)
    val expected = Set("main", "f", "g")
    assert(result == expected)
  }
}
