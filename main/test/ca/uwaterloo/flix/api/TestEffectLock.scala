package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.serialization
import ca.uwaterloo.flix.api.effectlock.serialization.Serialize
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEffectLock extends AnyFunSuite with TestUtils {

  test("serialization.01") {
    val input =
      """
        |pub def main(): Unit = ()
        |""".stripMargin

    val tpe = serialization.Apply(serialization.Apply(serialization.Cst(serialization.Arrow(1)), serialization.Cst(serialization.Unit)), serialization.Cst(serialization.Unit))
    val scheme = serialization.SScheme(List.empty, tpe)
    val expected = serialization.SDef(List.empty, "main", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.entryPoints.flatMap(root.defs.get).toList
    val actual = Serialize.serializeDef(defs.head)
    assert(expected == actual)
  }

}
