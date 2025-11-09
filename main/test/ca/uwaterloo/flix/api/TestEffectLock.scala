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

    val tpe = serialization.Apply(serialization.Apply(serialization.Apply(serialization.Cst(serialization.Arrow(2)), serialization.Cst(serialization.Pure)), serialization.Cst(serialization.Unit)), serialization.Cst(serialization.Unit))
    val scheme = serialization.SScheme(List.empty, tpe)
    val expected = serialization.SDef(List.empty, "main", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.entryPoints.flatMap(root.defs.get).toList
    val actual = Serialize.serializeDef(defs.head)
    assert(expected == actual)
  }

  test("serialization.02") {
    val input =
      """
        |@Test
        |pub def main(x: Int32): Int32 = x + x
        |""".stripMargin

    val tpe = serialization.Apply(serialization.Apply(serialization.Apply(serialization.Cst(serialization.Arrow(2)), serialization.Cst(serialization.Pure)), serialization.Cst(serialization.Int32)), serialization.Cst(serialization.Int32))
    val scheme = serialization.SScheme(List.empty, tpe)
    val expected = serialization.SDef(List.empty, "main", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibMin)
    val defs = root.entryPoints.flatMap(root.defs.get).toList
    val actual = Serialize.serializeDef(defs.head)
    assert(expected == actual)
  }

  test("serialization.03") {
    val input =
      """
        |@Test
        |pub def main(_: Int32): Unit = ()
        |""".stripMargin

    val tpe = serialization.Apply(serialization.Apply(serialization.Apply(serialization.Cst(serialization.Arrow(2)), serialization.Cst(serialization.Pure)), serialization.Cst(serialization.Int32)), serialization.Cst(serialization.Unit))
    val scheme = serialization.SScheme(List.empty, tpe)
    val expected = serialization.SDef(List.empty, "main", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibMin)
    val defs = root.entryPoints.flatMap(root.defs.get).toList
    val actual = Serialize.serializeDef(defs.head)
    assert(expected == actual)
  }

}
