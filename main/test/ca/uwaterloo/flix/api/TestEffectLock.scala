package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.EffectUpgrade
import ca.uwaterloo.flix.api.effectlock.serialization.*
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEffectLock extends AnyFunSuite with TestUtils {

  test("Safe.00") {
    val input =
      """
        |pub def f(x: Int32): Int32 = x
        |
        |pub def g(x: a): a = x
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.01") {
    val input =
      """
        |pub def f(): Unit = ???
        |
        |pub def g(): Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.02") {
    val input =
      """
        |pub def f(): Bool -> Unit = ???
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.03") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(): (Bool -> Unit \ E) = unchecked_cast(() as Bool -> Unit \ E)
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.04") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ E): b \ E = unchecked_cast(() as b \ E)
        |
        |pub def g(_: a -> b \ ef): b \ ef = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.05") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ E = unchecked_cast("str" as String \ E)
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.06") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.07") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String \ A = unchecked_cast("str" as String \ A)
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.08") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): b \ {ef, A, E} = unchecked_cast(() as b \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): b \ {ef, A} = unchecked_cast(() as b \ {ef, A})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.09") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.10") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.11") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  ignore("Safe.12") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  ignore("Safe.13") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  ignore("Safe.14") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ {ef1, ef2}): String \ {ef1, ef2, A, E} = unchecked_cast("str" as String \ {ef1, ef2, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  test("Safe.15") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.16") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.17") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.18") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> Unit \ E): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.19") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> b \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Safe.21") {
    val input =
      """
        |pub def f(_: a -> b): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit \ ef = checked_ecast(())
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("Unsafe.01") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(): (Bool -> Unit \ E) = unchecked_cast(() as Bool -> Unit \ E)
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.02") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ E): b \ E = unchecked_cast(() as b \ E)
        |
        |pub def g(_: a -> b \ ef): b \ ef = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.03") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ E = unchecked_cast("str" as String \ E)
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.04") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.05") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String \ A = unchecked_cast("str" as String \ A)
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.06") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A} = unchecked_cast("str" as String \ {ef, A})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.07") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: Int32 -> Unit \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.08") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.09") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.10") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.11") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> Unit \ E): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("Unsafe.12") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> b \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("serialization.01") {
    val input =
      """
        |pub def fun(): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Unit))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme, CompilerConstants.VirtualTestFile.toString)

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
    val expected = SDef(List.empty, "fun", scheme, CompilerConstants.VirtualTestFile.toString)

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
    val expected = SDef(List.empty, "toUnit", scheme, CompilerConstants.VirtualTestFile.toString)

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
    val expected = SDef(List.empty, "answer", scheme, CompilerConstants.VirtualTestFile.toString)

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
              ), Var(VarSym(0, Text("a"), StarKind))
            ), Cst(Int32)
          )
        ), Var(VarSym(0, Text("a"), StarKind))
      ), Cst(Int32)
    )
    val scheme = SScheme(List(VarSym(0, Text("a"), StarKind)), List.empty, List.empty, tpe)
    val expected = SDef(List(), "toInt32", scheme, CompilerConstants.VirtualTestFile.toString)

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

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Var(VarSym(0, Text("a"), StarKind))), Cst(Str))
    val scheme = SScheme(List(VarSym(0, Text("a"), StarKind)), List(TraitConstr(TraitSym(List.empty, "ToString"), Var(VarSym(0, Text("a"), StarKind)))), List.empty, tpe)
    val expected = SDef(List.empty, "pretty", scheme, CompilerConstants.VirtualTestFile.toString)

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

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Effect(EffSym(List.empty, "A"), EffKind))), Var(VarSym(0, Text("t"), StarKind))), Cst(Unit))
    val scheme = SScheme(List(VarSym(0, Text("t"), StarKind)), List(TraitConstr(TraitSym(List.empty, "ToString"), Var(VarSym(0, Text("t"), StarKind)))), List.empty, tpe)
    val expected = SDef(List.empty, "prettyPrint", scheme, CompilerConstants.VirtualTestFile.toString)

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
            Cst(Arrow(3)), Var(VarSym(0, Text("ef"), EffKind))
          ),
          Apply(
            Apply(
              Apply(
                Cst(Arrow(2)),
                Apply(
                  Apply(
                    Cst(Difference), Var(VarSym(0, Text("ef"), EffKind))
                  ), Cst(Effect(EffSym(List(), "A"), EffKind)))
              ), Var(VarSym(0, Text("a"), StarKind))
            ), Var(VarSym(1, Text("b"), StarKind)))
        ), Var(VarSym(0, Text("a"), StarKind))
      ), Var(VarSym(1, Text("b"), StarKind))
    )
    val scheme = SScheme(List(VarSym(0, Text("a"), StarKind), VarSym(0, Text("ef"), EffKind), VarSym(1, Text("b"), StarKind)), List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "prettyPrint", scheme, CompilerConstants.VirtualTestFile.toString)

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
    val expected = SDef(List.empty, "fun", scheme, CompilerConstants.VirtualTestFile.toString)

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

    val a = VarSym(0, Text("a"), StarKind)
    val tb = AssocTypeSym(TraitSym(List(), "T"), "B")
    val tpe = Apply(Apply(Apply(Apply(Cst(Arrow(3)), Cst(Pure)), Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Var(a)), AssocType(tb, Var(a), StarKind))), Var(a)), AssocType(tb, Var(a), StarKind))
    val scheme = SScheme(List(a), List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "h", scheme, CompilerConstants.VirtualTestFile.toString)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  private def checkIsSafe(original: String, upgrade: String, root: TypedAst.Root): Boolean = {
    implicit val flix: Flix = new Flix()
    val orig = stringToDef(original, root).get
    val upgr = stringToDef(upgrade, root).get
    EffectUpgrade.isSafe(orig.spec.declaredScheme, upgr.spec.declaredScheme)
  }

  private def stringToDef(defn: String, root: TypedAst.Root): Option[TypedAst.Def] = {
    root.defs.find {
      case (sym, _) => sym.text == defn
    }.map(_._2)
  }

}
