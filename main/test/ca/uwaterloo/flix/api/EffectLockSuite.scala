package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.TypedAst
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
    val result = reachableDefs(checkEffectLock(input))
    val expected = Set("main", "f", "g")
    assert(result == expected)
  }

  private def checkEffectLock(input: String): Option[TypedAst.Root] = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val result = flix.effectLock()
    result
  }

  private def reachableDefs(root: Option[TypedAst.Root]): Set[String] = root match {
    case None => Set.empty
    case Some(r) => r.defs.keys.map(_.text).toSet
  }
}
