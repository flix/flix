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
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
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
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
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
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
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
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
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
    val scheme = SScheme(List(VarSym(Text("a"), StarKind)), List.empty, List.empty, tpe)
    val expected = SDef(List(), "toInt32", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.06") {
    val input =
      """
        |trait ToString[a: Type] {
        |   pub def toString(x: a): String
        |}
        |
        |pub def pretty(x: a): String with ToString[a] = ToString.toString(x)
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Var(VarSym(Text("a"), StarKind))), Cst(Str))
    val scheme = SScheme(List(VarSym(Text("a"), StarKind)), List(TraitConstr(TraitSym(List.empty, "ToString"), Var(VarSym(Text("a"), StarKind)))), List.empty, tpe)
    val expected = SDef(List.empty, "pretty", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.07") {
    val input =
      """
        |trait ToString[a: Type] {
        |   pub def toString(x: a): String
        |}
        |
        |pub eff A {
        |    def thing(x: String): Unit
        |}
        |
        |pub def prettyPrint(x: t): Unit \ A with ToString[t] = A.thing(ToString.toString(x))
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Effect(EffSym(List.empty, "A"), EffKind))), Var(VarSym(Text("t"), StarKind))), Cst(Unit))
    val scheme = SScheme(List(VarSym(Text("t"), StarKind)), List(TraitConstr(TraitSym(List.empty, "ToString"), Var(VarSym(Text("t"), StarKind)))), List.empty, tpe)
    val expected = SDef(List.empty, "prettyPrint", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.08") {
    val input =
      """
        |pub eff A {
        |    def thing(x: String): Unit
        |}
        |
        |pub def prettyPrint(f: a -> b \ {ef - A}, x: a): b \ ef = f(x)
        |""".stripMargin

    val tpe = Apply(
      Apply(
        Apply(
          Apply(
            Cst(Arrow(3)), Var(VarSym(Text("ef"), EffKind))
          ),
          Apply(
            Apply(
              Apply(
                Cst(Arrow(2)),
                Apply(
                  Apply(
                    Cst(Difference), Var(VarSym(Text("ef"), EffKind))
                  ), Cst(Effect(EffSym(List(), "A"), EffKind)))
              ), Var(VarSym(Text("a"), StarKind))
            ), Var(VarSym(Text("b"), StarKind)))
        ), Var(VarSym(Text("a"), StarKind))
      ), Var(VarSym(Text("b"), StarKind))
    )
    val scheme = SScheme(List(VarSym(Text("a"), StarKind), VarSym(Text("ef"), EffKind), VarSym(Text("b"), StarKind)), List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "prettyPrint", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.09") {
    val input =
      """
        |type alias Num = Int32
        |pub def fun(x: Num): Num = x + x
        |""".stripMargin

    val alias = Alias(TypeAliasSym(List.empty, "Num"), List.empty, Cst(Int32))
    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), alias), alias)
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialization.10") {
    val input =
      """
        |pub trait T[a: Type] {
        |    type B: Type
        |    pub def f(g: a -> T.B[a]): T.B[a]
        |}
        |
        |pub def h(q: a -> T.B[a], x: a): T.B[a] = q(x)
        |""".stripMargin

    val a = VarSym(Text("a"), StarKind)
    val tb = AssocTypeSym(TraitSym(List(), "T"), "B")
    val tpe = Apply(Apply(Apply(Apply(Cst(Arrow(3)), Cst(Pure)), Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Var(a)), AssocType(tb, Var(a), StarKind))), Var(a)), AssocType(tb, Var(a), StarKind))
    val scheme = SScheme(List(a), List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "h", scheme, "<test>")

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

}
