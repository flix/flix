package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.serialization.*
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEffectLock extends AnyFunSuite with TestUtils {

  test("serialization.01") {
    val input =
      """
        |pub def fun(): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Unit))
    val scheme = SScheme(List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.02") {
    val input =
      """
        |pub def fun(x: Int32): Int32 = x + x
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Int32))
    val scheme = SScheme(List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.03") {
    val input =
      """
        |pub def toUnit(_: Int32): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Unit))
    val scheme = SScheme(List.empty, tpe)
    val expected = SDef(List.empty, "toUnit", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.04") {
    val input =
      """
        |pub def answer(): Int32 = 42
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Int32))
    val scheme = SScheme(List.empty, tpe)
    val expected = SDef(List.empty, "answer", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.05") {
    val input =
      """
        |pub def toInt32(f: a -> Int32, x: a): Int32 = f(x)
        |""".stripMargin

    val tpe = Apply(
      Apply(
        Apply(
          Apply(
            Cst(Arrow(3)), Cst(Pure)
          ),
          Apply(
            Apply(
              Apply(
                Cst(Arrow(2)), Cst(Pure)
              ), Var(VarSym(Text("a"), StarKind))
            ), Cst(Int32)
          )
        ), Var(VarSym(Text("a"), StarKind))
      ), Cst(Int32)
    )
    val scheme = SScheme(List(VarSym(Text("a"), StarKind)), tpe)
    val expected = SDef(List(), "toInt32", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

}
