/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.CompilerConstants
import ca.uwaterloo.flix.api.effectlock.serialization.*
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestSerialization extends AnyFunSuite with TestUtils {

  test("serialize.def.01") {
    val input =
      """
        |pub def fun(): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Unit))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.02") {
    val input =
      """
        |pub def fun(x: Int32): Int32 = x + x
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Int32))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.03") {
    val input =
      """
        |pub def toUnit(_: Int32): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Unit))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "toUnit", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.04") {
    val input =
      """
        |pub def answer(): Int32 = 42
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Int32))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "answer", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.05") {
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
    val expected = SDef(List(), "toInt32", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.06") {
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
    val expected = SDef(List.empty, "pretty", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.07") {
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
    val expected = SDef(List.empty, "prettyPrint", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.08") {
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
    val expected = SDef(List.empty, "prettyPrint", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.09") {
    val input =
      """
        |type alias Num = Int32
        |pub def fun(x: Num): Num = x + x
        |""".stripMargin

    val erasedAlias = Cst(Int32)
    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), erasedAlias), erasedAlias)
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.def.10") {
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
    val expected = SDef(List.empty, "h", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.sig.01") {
    val input =
      """
        |trait T[a] {
        |    pub def fun(): a
        |}
        |""".stripMargin

    val a = VarSym(0, Text("a"), StarKind)
    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Var(a))
    val tconstr = TraitConstr(TraitSym(List(), "T"), Var(a))
    val scheme = SScheme(List(a), List(tconstr), List.empty, tpe)
    val expected = SSig(List("T"), "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val sigs = root.sigs.keys.flatMap(root.sigs.get)
    val actual = Serialize.serializeSig(sigs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.02") {
    val input =
      """
        |pub def fun(x: Int32): Int32 = x + x
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Int32))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.03") {
    val input =
      """
        |pub def toUnit(_: Int32): Unit = ()
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Int32)), Cst(Unit))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "toUnit", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.04") {
    val input =
      """
        |pub def answer(): Int32 = 42
        |""".stripMargin

    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), Cst(Unit)), Cst(Int32))
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "answer", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.05") {
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
    val expected = SDef(List(), "toInt32", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.06") {
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
    val expected = SDef(List.empty, "pretty", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.07") {
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
    val expected = SDef(List.empty, "prettyPrint", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.08") {
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
    val expected = SDef(List.empty, "prettyPrint", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.09") {
    val input =
      """
        |type alias Num = Int32
        |pub def fun(x: Num): Num = x + x
        |""".stripMargin

    val erasedAlias = Cst(Int32)
    val tpe = Apply(Apply(Apply(Cst(Arrow(2)), Cst(Pure)), erasedAlias), erasedAlias)
    val scheme = SScheme(List.empty, List.empty, List.empty, tpe)
    val expected = SDef(List.empty, "fun", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  ignore("serialize.sig.10") {
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
    val expected = SDef(List.empty, "h", scheme)

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val actual = Serialize.serializeDef(defs.head)
    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.01") {
    val input =
      """
        |pub def fun(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.02") {
    val input =
      """
        |pub def fun(x: Int32): Int32 = x + x
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.03") {
    val input =
      """
        |pub def toUnit(_: Int32): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.04") {
    val input =
      """
        |pub def answer(): Int32 = 42
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.05") {
    val input =
      """
        |pub def toInt32(f: a -> Int32, x: a): Int32 = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.06") {
    val input =
      """
        |trait ToString[a: Type] {
        |   pub def toString(x: a): String
        |}
        |
        |pub def pretty(x: a): String with ToString[a] = ToString.toString(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.07") {
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

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.08") {
    val input =
      """
        |pub eff A {
        |    def thing(x: String): Unit
        |}
        |
        |pub def prettyPrint(f: a -> b \ {ef - A}, x: a): b \ ef = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.09") {
    val input =
      """
        |type alias Num = Int32
        |pub def fun(x: Num): Num = x + x
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val erasedBaseType = Type.eraseAliases(defn.spec.declaredScheme.base)
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme.copy(base = erasedBaseType)))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.def.10") {
    val input =
      """
        |pub trait T[a: Type] {
        |    type B: Type
        |    pub def f(g: a -> T.B[a]): T.B[a]
        |}
        |
        |pub def h(q: a -> T.B[a], x: a): T.B[a] = q(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  test("serialize.deserialize.identity.sig.01") {
    val input =
      """
        |trait T[a] {
        |    pub def fun(): a
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val sigs = root.sigs.keys.flatMap(root.sigs.get)
    val sig = sigs.head
    val expected = (sig.sym, sig.spec.declaredScheme)
    val actual = Deserialize.deserializeSig(Serialize.serializeSig(sig))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.02") {
    val input =
      """
        |pub def fun(x: Int32): Int32 = x + x
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.03") {
    val input =
      """
        |pub def toUnit(_: Int32): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.04") {
    val input =
      """
        |pub def answer(): Int32 = 42
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.05") {
    val input =
      """
        |pub def toInt32(f: a -> Int32, x: a): Int32 = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, defn.spec.declaredScheme)
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.06") {
    val input =
      """
        |trait ToString[a: Type] {
        |   pub def toString(x: a): String
        |}
        |
        |pub def pretty(x: a): String with ToString[a] = ToString.toString(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.07") {
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

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.08") {
    val input =
      """
        |pub eff A {
        |    def thing(x: String): Unit
        |}
        |
        |pub def prettyPrint(f: a -> b \ {ef - A}, x: a): b \ ef = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.09") {
    val input =
      """
        |type alias Num = Int32
        |pub def fun(x: Num): Num = x + x
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val erasedBaseType = Type.eraseAliases(defn.spec.declaredScheme.base)
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme.copy(base = erasedBaseType)))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

  ignore("serialize.deserialize.identity.sig.10") {
    val input =
      """
        |pub trait T[a: Type] {
        |    type B: Type
        |    pub def f(g: a -> T.B[a]): T.B[a]
        |}
        |
        |pub def h(q: a -> T.B[a], x: a): T.B[a] = q(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val defs = root.defs.keys.flatMap(root.defs.get)
    val defn = defs.head
    val expected = (defn.sym, Util.alpha(defn.spec.declaredScheme))
    val actual = Deserialize.deserializeDef(Serialize.serializeDef(defn))

    assert(actual == expected)
  }

}
