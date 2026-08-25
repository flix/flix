/*
 * Copyright 2020 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.errors.TypeError.MismatchedTypes
import ca.uwaterloo.flix.util.{Options, Subeffecting}
import org.scalatest.funsuite.AnyFunSuite

class TestTyper extends AnyFunSuite with TestUtils {

  test("TestLeq01") {
    val input =
      """
        |def foo(): a = 21
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq02") {
    val input =
      """
        |def foo(): List[a] = 21 :: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq03") {
    val input =
      """
        |def foo(): Result[a, Int32] = Ok(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq04") {
    val input =
      """
        |def foo(): Result[Int32, a] = Err(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq05") {
    val input =
      """
        |def foo(): a -> a = x -> 21
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq06") {
    val input =
      """
        |def foo(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq07") {
    val input =
      """
        |def foo(): {x = Int32 | r} = {x = 21}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq08") {
    val input =
      """
        |def foo(): {x = Int32, y = Int32 | r} = {y = 42, x = 21}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestOccurs01") {
    val input = "def foo(a: #{A(Int32) | r}, b: #{B(Int32) | r}): #{A(Int32), B(Int32)} = solve (a <+> b)"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedNullaryTypes.01") {
    val input = "def foo(): #{A(Unit)| x} = #{A.}"
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestMismatchedNullaryTypes.02") {
    val input = "def foo(): #{A| x} = #{A()}"
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.01") {
    val input = "def foo(): {| x} = {a = 2} <+> {a = 2}"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.02") {
    val input = "def foo(): #{| x} = {a = 2} <+> {a = 2}"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.03") {
    val input = "def foo(): {a = Int32} = {a = 2} <+> {a = 2}"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.04") {
    val input = "def foo(): String = solve \"hello\""
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("MismatchedTypes.05") {
    val input =
      """
        |trait A[a] {
        |    type Typ
        |    pub def foo(x: a): A.Typ[a]
        |}
        |
        |enum Adapter[t, a, b](t, a -> b)
        |
        |instance A[Adapter[t, a, b]] with A[t] {
        |    type Typ = b
        |    pub def foo(adapter: Adapter[t, a, b]): b =
        |        let Adapter.Adapter(x, f) = adapter;
        |        f(A.foo(x))
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[MismatchedTypes](result)
  }

  test("MismatchedArrowAndNonArrow.01") {
    val input = "def foo(): a = solve (x -> x)"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedArrowAndNonArrow](result)
  }

  test("MismatchedArrowAndNonArrow.02") {
    val input = "def foo(): a = if (true) (x -> x) else 1"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedArrowAndNonArrow](result)
  }

  test("MismatchedArrowAndNonArrow.03") {
    // A function reference checked against a concrete non-function annotation (ExpectType).
    val input = "def foo(): Int32 = x -> x"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedArrowAndNonArrow](result)
  }

  test("MismatchedArrowAndNonArrow.04") {
    // A function reference passed where a concrete non-function argument is expected (ExpectArgument).
    val input =
      """
        |def f(x: Int32): Int32 = x
        |def foo(): Int32 = f(y -> y)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedArrowAndNonArrow](result)
  }

  test("MismatchedTypes.06") {
    val input =
      """
        |trait A[a] {
        |    type Typ
        |    pub def foo(x: a): A.Typ[a]
        |}
        |
        |enum Adapter[t, a, b](t, a -> b)
        |
        |instance A[Adapter[t, a, b]] with A[t] where A.Typ[t] ~ Int8 {
        |    type Typ = b
        |    pub def foo(adapter: Adapter[t, a, b]): b =
        |        let Adapter.Adapter(x, f) = adapter;
        |        f(A.foo(x))
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[MismatchedTypes](result)
  }

  test("TestOverApplied.01") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestOverApplied.02") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123, true)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestUnderApplied.01") {
    val input =
      """
        |def f(x: String, y: Int32): Bool = true
        |def under(): String = (f("hello"): String)
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestUnderApplied.02") {
    val input =
      """
        |def f(x: String, y: Int32, z: Bool): Bool = true
        |def under(): String = (f("hello"): String)
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.01") {
    val input = "def foo(a: _): _ = a"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.02") {
    val input = "def foo(a: Int32): _ = a"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.03") {
    val input = raw"def foo(a: Int32): Int32 \ _ = a"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.04") {
    val input = raw"def foo(g: Int32 -> Int32 \ _): Int32 \ _ = g(1)"
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.01") {
    val input =
      """
        |trait C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: a): String = C.foo(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.02") {
    val input =
      """
        |trait C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: Int32): String = C.foo(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.03") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Float64]): String = C.foo(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.04") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Int32]): String = C.foo(C.foo(x))
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.05") {
    val input =
      """
        |trait C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): Int32 = x
        |}
        |
        |def bar(x: a, y: Int32): (Int32, Int32) = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.06") {
    // missing constraint on C[b]
    val input =
      """
        |trait C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |def bar(x: a, y: b): (Int32, Int32) with C[a] = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.Location.01") {
    val input =
      """
        |trait C[a] {
        |    pub def f(x: a): Unit
        |}
        |
        |instance C[MyBox[a]] with C[a] {
        |    pub def f(x: MyBox[a]): Unit = ???
        |}
        |
        |enum MyBox[a](a)
        |
        |def foo(): Unit = {
        |  C.f(MyBox.MyBox(123)) // ERROR
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.Relation.01") {
    val input =
      """
        |pub enum E {
        |   case E1
        |}
        |
        |pub def f(): Bool = {
        |   let _x = #{
        |     R(E.E1).
        |   };
        |   true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingEq.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x == y
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstanceEq](result)
  }

  test("MissingOrder.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x <= y
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstanceOrder](result)
  }

  test("MissingToString.01") {
    val input =
      s"""
         |pub enum E {
         |   case E
         |}
         |
         |def foo(x: E): String = ToString.toString(x)
         |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstanceToString](result)
  }

  test("MissingArrowInstance.01") {
    val input =
      """
        |def main(): Unit \ IO =
        |    println(x -> x + 41i32)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstanceArrow](result)
  }

  test("Test.UnexpectedEffect.01") {
    val input =
      """
        |pub def f(): Int32 = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.02") {
    val input =
      """
        |def f(): Int32 \ {} = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.03") {
    // Regression test. See https://github.com/flix/flix/issues/4062
    val input =
      """
        |def mkArray(): Array[Int32, Static] \ IO = Array#{} @ Static
        |
        |def zero(): Int32 \ {} = %%ARRAY_LENGTH%%(mkArray())
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.04") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 = g(123)
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.05") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ {} = g(123)
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.06") {
    val input =
      """
        |eff Print {
        |    pub def print(): Unit
        |}
        |
        |eff Exc {
        |    pub def raise(): Unit
        |}
        |
        |def f(): Unit =
        |    Print.print();
        |    Exc.raise()
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.07") {
    // guards must be pure
    val input =
      """
        |eff E
        |def impureBool(): Bool \ E = checked_ecast(???)
        |
        |def foo(): Int32 \ E = {
        |    match 0 {
        |        case 0 if impureBool() => 0
        |        case _ => 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.08") {
    val input =
      """
        |import java.lang.Object
        |
        |eff IO
        |
        |def impureX(): String \ IO = checked_ecast("x")
        |
        |def f(): Object \ IO = {
        |    let x = new Object {
        |        def toString(_this: Object): String = impureX()
        |    };
        |    x
        |}
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.EffectGeneralizationError.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ ef = 123
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.EffectGeneralizationError.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef1, h: Int32 -> Int32 \ ef2): Int32 \ {ef1, ef2} = 123
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.01") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.02") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            (123, x)
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.03") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            _w -> x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.04") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            w -> {
        |                discard Ref.get(x);
        |                w
        |            }
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("RegionVarEscapes.05") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = Ref.fresh(Static, None);
        |    region rc {
        |        let x = Ref.fresh(rc, 123);
        |        Ref.put(Some(x), m);
        |        ()
        |    }
    """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("RegionVarEscapes.06") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = Ref.fresh(Static, None);
        |    region rc {
        |        let x = Ref.fresh(rc, 123);
        |        Ref.put(Some(_ -> x), m);
        |        ()
        |    }
    """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }


  test("Test.UnexpectedType.OpParam.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit \ E = E.op(123)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }


  test("Test.MismatchedEff.Apply.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit = ???
        |
        |def foo(): Unit = disjoint(_ -> E.op(), _ -> E.op())
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.MismatchedEff.Recursion.01") {
    // Regression test. See https://github.com/flix/flix/issues/10185
    val input =
      """
        |eff Something
        |def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef - Something =
        |    def loop(ll, k) = match ll {
        |        case Nil     => k(s)
        |        case x :: xs => loop(xs, ks -> k(f(x, ks)))
        |    };
        |    loop(l, x -> checked_ecast(x))
        |
        |enum List[a] {
        |    case Nil
        |    case Cons(a, List[a])
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.GeneralizationError.Eff.01") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |eff F {
        |    pub def op(): Unit
        |}
        |
        |def doBoth(f: Unit -> Unit \ {ef - E}, g: Unit -> Unit \ {ef - F}): Unit \ {ef - E - F} = g(); f()
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestParYield.01") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.02") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.03") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (a <- 1; b <- { unchecked_cast(1 as _ \ IO) }) yield (a, b);
        |     g()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.04") {
    val input =
      """
        | def f(): Int32 =
        |     par (a <- true) yield a + 1
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.01") {
    val input =
      """
        |def f[m: Eff -> Type, a: Eff](_: m[a]): m[a] = ???
        |
        |enum Box[a](a)
        |
        |def g(): Box[Int32] = f(Box.Box(123))
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def noE(f: Unit -> Unit \ ef - E): Unit = ???
        |
        |def foo(): Unit = noE(_ -> E.op())
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.03") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def mustE(f: Unit -> Unit \ {ef, E}): Unit = ???
        |
        |def foo(): Unit = mustE(x -> x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.05") {
    // Regression test.
    // See https://github.com/flix/flix/issues/3634
    val input =
      """
        |enum E[a: Type, ef: Eff](Unit)
        |def f(g: E[Int32, Pure]): Bool = ???
        |def mkE(): E[Int32, Pure] \ ef = ???
        |
        |def g(): Bool = f(mkE)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestChoose.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = choose Expr.Cst {
        |    case Expr.Var(_) => true
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChoose.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |     };
        |     let h = if (true) f else g;
        |     h(Expr.Var)
        | }
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChoose.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let h = if (true) f else g;
        |
        |     let cstOrNotOrVar = if (true) open_variant Expr.Cst else if (true) open_variant Expr.Not else open_variant Expr.Var;
        |
        |     h(cstOrNotOrVar)
        | }
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def quack(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def liquorice(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Not(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.04") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def testChooseStar4(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Var(_) => true
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.05") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Not(_) => Expr.Not()
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.06") {
    val input =
      """
        |restrictable enum E[s] {
        |    case N(E[s])
        |    case C
        |}
        |
        |def n(e: E[s rvand <E.N>]): _ = ???
        |
        |def foo(e: E[s]): E[s] = choose* e {
        |    case E.N(x) => n(x)            // must have x <: <E.N> but this doesn't hold
        |    case E.C    => E.C
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.01") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Not all cases caught
        |def isRed(c: Color[s]): Bool = choose c {
        |    case Color.Red => true
        |    case Color.Green => false
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.02") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// forgot Green intro
        |def redToGreen(c: Color[s]): Color[s rvsub <Color.Red>] = choose* c {
        |    case Color.Red => Color.Green
        |    case Color.Green => Color.Green
        |    case Color.Blue => Color.Blue
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.03") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Wrong minus
        |def isRed(c: Color[s rvsub <Color.Blue>]): Bool = choose* c {
        |    case Color.Red => true
        |    case Color.Blue => false
        |}
        |""".stripMargin
    expectError[TypeError](check(input, Options.TestWithLibNix))
  }

  test("TestLetRec.01") {
    val input =
      """
        |def f(): Int32 = {
        |    def g(): Bool = 123;
        |    g()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.01") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |def g(x: a): String with C[a] = C.f(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.02") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |def g(x: a): String with C[a] where C.T[a] ~ Int32 = C.f(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.03") {
    val input =
      """
        |pub enum Maybe[a] {
        |    case Just(a),
        |    case Nothing
        |}
        |
        |trait C[a] {
        |    type S : Type
        |    type T : Type -> Type
        |    pub def f(x: a): C.T[a][C.S[a]]
        |}
        |
        |instance C[Int32] {
        |    type S = Int32
        |    type T = Maybe
        |    pub def f(x: Int32): Maybe[Int64] = x
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.04") {
    val input =
      """
        |trait A[a] {
        |    type A : Type -> Type -> Type
        |    type B : Type -> Type
        |    type C : Type
        |    pub def f(x: a): A.A[a][A.A[a][A.B[a][A.A[a][A.B[a][A.C[a]]][A.B[a][A.C[a]]]]][A.B[a][A.C[a]]]][A.A[a][A.B[a][A.C[a]]][A.C[a]]]
        |    pub def g(x: a): A.A[a][A.B[a][A.C[a]]][A.C[a]] = A.f(x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.05") {
    // Regression test. See https://github.com/flix/flix/issues/11213
    // The associated effect of the `Vec` instance is declared as `OutInt32`, but the body
    // `Container.forEach(Runner.exec, x)` has effect `Runner.E[Container.Elm[Vec[a]]]`.
    // Reducing this nested associated type must terminate and report the mismatch.
    val input =
      """
        |trait Container[t] {
        |    type Elm: Type
        |    pub def forEach(f: Container.Elm[t] -> Unit \ ef, t: t): Unit \ ef
        |}
        |
        |enum Vec[a](a)
        |
        |instance Container[Vec[a]] {
        |    type Elm = a
        |    pub def forEach(f: a -> Unit \ ef, v: Vec[a]): Unit \ ef =
        |        let Vec.Vec(x) = v;
        |        f(x)
        |}
        |
        |eff OutInt32 {
        |    def toStream(x: Int32): Unit
        |}
        |
        |trait Runner[a] {
        |    type E: Eff
        |    pub def exec(x: a): Unit \ Runner.E[a]
        |}
        |
        |instance Runner[Int32] {
        |    type E = OutInt32
        |    pub def exec(x: Int32): Unit \ OutInt32 = OutInt32.toStream(x)
        |}
        |
        |instance Runner[Vec[a]] with Runner[a] {
        |    type E = OutInt32
        |    pub def exec(x: Vec[a]): Unit \ OutInt32 =
        |        Container.forEach(Runner.exec, x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestAssocType.06") {
    // Regression test. See https://github.com/flix/flix/issues/11213
    // The `Vec` instance is missing the `with Runner[a]` constraint, so `Runner.exec` on the
    // element type is unresolvable. Reducing the nested associated type must terminate.
    val input =
      """
        |trait Container[t] {
        |    type Elm: Type
        |    pub def forEach(f: Container.Elm[t] -> Unit \ ef, t: t): Unit \ ef
        |}
        |
        |enum Vec[a](a)
        |
        |instance Container[Vec[a]] {
        |    type Elm = a
        |    pub def forEach(f: a -> Unit \ ef, v: Vec[a]): Unit \ ef =
        |        let Vec.Vec(x) = v;
        |        f(x)
        |}
        |
        |eff OutInt32 {
        |    def toStream(x: Int32): Unit
        |}
        |
        |trait Runner[a] {
        |    type E: Eff
        |    pub def exec(x: a): Unit \ Runner.E[a]
        |}
        |
        |instance Runner[Int32] {
        |    type E = OutInt32
        |    pub def exec(x: Int32): Unit \ OutInt32 = OutInt32.toStream(x)
        |}
        |
        |instance Runner[Vec[a]] {
        |    type E = Runner.E[a]
        |    pub def exec(x: Vec[a]): Unit \ Runner.E[a] =
        |        Container.forEach(Runner.exec, x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestAssocType.07") {
    // Regression test. See https://github.com/flix/flix/issues/11213
    // The body performs an extra `OutInt32` effect on top of the nested associated effect, so
    // its effect is `Runner.E[a] + OutInt32`, which does not match the declared `Runner.E[a]`.
    val input =
      """
        |trait Container[t] {
        |    type Elm: Type
        |    pub def forEach(f: Container.Elm[t] -> Unit \ ef, t: t): Unit \ ef
        |}
        |
        |enum Vec[a](a)
        |
        |instance Container[Vec[a]] {
        |    type Elm = a
        |    pub def forEach(f: a -> Unit \ ef, v: Vec[a]): Unit \ ef =
        |        let Vec.Vec(x) = v;
        |        f(x)
        |}
        |
        |eff OutInt32 {
        |    def toStream(x: Int32): Unit
        |}
        |
        |trait Runner[a] {
        |    type E: Eff
        |    pub def exec(x: a): Unit \ Runner.E[a]
        |}
        |
        |instance Runner[Int32] {
        |    type E = OutInt32
        |    pub def exec(x: Int32): Unit \ OutInt32 = OutInt32.toStream(x)
        |}
        |
        |instance Runner[Vec[a]] with Runner[a] {
        |    type E = Runner.E[a]
        |    pub def exec(x: Vec[a]): Unit \ Runner.E[a] =
        |        Container.forEach(Runner.exec, x);
        |        OutInt32.toStream(42)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestAssocType.08") {
    // Regression test. See https://github.com/flix/flix/issues/11213
    // `Runner.exec` on a `Vec[Int32]` has effect `Runner.E[Vec[Int32]]`, which reduces through
    // the instances to `OutInt32`. The declared `OutString` must not match, and the concrete
    // reduction chain must terminate.
    val input =
      """
        |enum Vec[a](a)
        |
        |eff OutInt32 {
        |    def toStream(x: Int32): Unit
        |}
        |
        |eff OutString {
        |    def toStream(x: String): Unit
        |}
        |
        |trait Runner[a] {
        |    type E: Eff
        |    pub def exec(x: a): Unit \ Runner.E[a]
        |}
        |
        |instance Runner[Int32] {
        |    type E = OutInt32
        |    pub def exec(x: Int32): Unit \ OutInt32 = OutInt32.toStream(x)
        |}
        |
        |instance Runner[Vec[a]] with Runner[a] {
        |    type E = Runner.E[a]
        |    pub def exec(x: Vec[a]): Unit \ Runner.E[a] =
        |        let Vec.Vec(y) = x;
        |        Runner.exec(y)
        |}
        |
        |def runMismatch(x: Vec[Int32]): Unit \ OutString = Runner.exec(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.01") {
    val input =
      """
        |def f(): Bool = match { x = 1 } {
        |    case { x = false } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.02") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { x = _ } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.03") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { } => false
        |    case { x = _ | r } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.04") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { x = _ | r } => false
        |    case { x = _ } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.01") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    Gen.gen();
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.02") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen()
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    Gen.gen();
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.03") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen();
        |        AskTell.askTell(42)
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.04") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen();
        |        AskTell.askTell(42)
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    Gen.gen();
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.05") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen();
        |        AskTell.askTell(42)
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.06") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen();
        |        AskTell.askTell(42)
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    Gen.gen();
        |    AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }
  test("TestIOAndCustomEffect.07") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen()
        |    } with handler Gen {
        |        def gen(k) = k("a")
        |    };
        |    AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.08") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: String): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = run {
        |        Gen.gen()
        |    } with handler Gen {
        |        def gen(k) = AskTell.askTell(k("a"))
        |    };
        |    ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestTryCatch.01") {
    val input =
      """
        |import java.io.IOError
        |
        |enum Res { case Err(String), case Ok }
        |
        |pub def catchIO(f: Unit -> Unit \ ef): Res = {
        |    try {f(); Res.Ok} catch {
        |        case ex: IOError =>
        |            Res.Err(ex.getMessage())
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.MissingConstraint.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |def foo(): C.T[a] = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MissingTraitConstraint](result)
  }

  test("TypeError.MissingConstraint.02") {
    // missing constraint on A[t]
    val input =
      """
        |trait A[a] {
        |    type Typ
        |    pub def foo(x: a): A.Typ[a]
        |}
        |
        |enum Adapter[t, a, b](t, a -> b)
        |
        |instance A[Adapter[t, a, b]] where A.Typ[t] ~ Int8 {
        |    type Typ = b
        |    pub def foo(adapter: Adapter[t, a, b]): b = ???
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MissingTraitConstraint](result)
  }

  test("TypeError.IllegalAssocType.Enum.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |enum E[a] {
        |    case D(C.T[a])
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.IllegalAssocType](result)
  }

  test("TypeError.IllegalAssocType.Enum.02") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |enum E[a] {
        |    case D(C.T[a] -> Int32)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.IllegalAssocType](result)
  }

  test("TypeError.IllegalAssocType.Struct.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |struct S[a, r] {
        |    f: C.T[a]
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.IllegalAssocType](result)
  }

  test("TypeError.IllegalAssocType.TypeAlias.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |type alias A[a] = C.T[a]
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.IllegalAssocType](result)
  }

  test("TypeError.IllegalAssocType.RestrictableEnum.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |restrictable enum E[s][a] {
        |    case D(C.T[a])
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.IllegalAssocType](result)
  }

  test("TypeError.NewStruct.01") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = 3, b = 4, c = "hello"};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.NewStruct.02") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = (), b = "hi", c = "hello"};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.NewStruct.03") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = 3, b = "hi", c = new S @ rc {a = 4, b = 3, c = ()}};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.01") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s = new S @ rc {a = 4, b = "hi", c = "hello"};
        |            s->a + s->b;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.02") {
    val input =
      """
        |struct S[v, r] {
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s1 = new S @ rc {c = 3};
        |            let s2 = new S @ rc {c = "hello"};
        |            s1->c + s2->c;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.01") {
    val input =
      """
        |struct S[v, r] {
        |    mut a: Int32,
        |    b: String,
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s = new S @ rc {a = 4, b = "hi", c = "hello"};
        |            s->a = s->b;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.02") {
    val input =
      """
        |struct S[v, r] {
        |    mut c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s1 = new S @ rc {c = 3};
        |            let s2 = new S @ rc {c = "hello"};
        |            s1->c = s2->c;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TypeError.ConstructorUnboxing.01") {
    val input =
      """
        |import java.lang.Boolean
        |
        |def f(): Bool \ IO =
        |    let boxed = new Boolean(true);
        |    new Boolean(boxed).booleanValue()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.MethodBoxing.01") {
    val input =
      """
        |import java.lang.Boolean
        |
        |def f(): Int32 \ IO =
        |    let boxed = new Boolean(true);
        |    boxed.compareTo(false)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.MethodBoxing.02") {
    val input =
      """
        |import java.util.Objects
        |
        |def f(): Bool \ IO = Objects.isNull(true)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.MethodUnboxing.01") {
    val input =
      """
        |import java.lang.Integer
        |
        |def f(): Char \ IO =
        |    let boxed = new Integer(0);
        |    "s".charAt(boxed)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.MethodUnboxing.02") {
    val input =
      """
        |import java.lang.Boolean
        |
        |def f(): Int32 \ IO =
        |    let boxed = new Boolean(true);
        |    Boolean.compare(true, boxed)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TypeError.IllegalSpawn.01") {
    val input =
      """
        |eff Ask {
        |    pub def ask(): String
        |}
        |
        |def foo(): Unit \ Ask =
        |    region rc {
        |        spawn Ask.ask() @ rc
        |    }
        |
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Subeffecting.Def.01") {
    val input =
      """
        |def f(): Unit \ IO = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.ModDefs)))
    expectSuccess(result)
  }

  test("Subeffecting.Def.02") {
    val input =
      """
        |def f(): Unit \ IO = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.Lambdas, Subeffecting.InsDefs)))
    expectError[TypeError](result)
  }

  test("Subeffecting.Lambda.01") {
    val input =
      """
        |def mustBeIO(f: Unit -> Unit \ IO): Unit \ IO = f()
        |def f(): Unit \ IO =
        |  mustBeIO(() -> ())
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.Lambdas)))
    expectSuccess(result)
  }

  test("Subeffecting.Lambda.02") {
    val input =
      """
        |def mustBeIO(f: Unit -> Unit \ IO): Unit \ IO = f()
        |def f(): Unit \ IO =
        |  mustBeIO(() -> ())
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.InsDefs)))
    expectError[TypeError](result)
  }

  test("Subeffecting.Instance.01") {
    val input =
      """
        |trait T[t] { pub def f(x: t): Unit \ IO }
        |instance T[Char] {
        |  pub def f(_x: Char): Unit \ IO = ()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.InsDefs)))
    expectSuccess(result)
  }

  test("Subeffecting.Instance.02") {
    val input =
      """
        |trait T[t] { pub def f(x: t): Unit \ IO }
        |instance T[Char] {
        |  pub def f(_x: Char): Unit \ IO = ()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin.copy(xsubeffecting = Set(Subeffecting.ModDefs, Subeffecting.Lambdas)))
    expectError[TypeError](result)
  }

  test("ErrorType.01") {
    // There should be no type error because Abc does not resolve.
    val input =
      """
        |def foo(): Abc = "hello"
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.02") {
    // There should be no type error because Abc does not resolve.
    // Related issue: https://github.com/flix/flix/issues/10176
    val input =
      """
        |def foo(): Abc = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.03") {
    // There should be no type error because the associated type `T` is missing.
    // The Resolver reports MissingAssocTypeDef and recovers with an error type.
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |instance C[Int32] {
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = C.f(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.04") {
    // There should be no type error because the associated effect `E` is missing.
    // The Resolver reports MissingAssocTypeDef and recovers with an error type.
    val input =
      """
        |eff Out {
        |    def out(x: Int32): Unit
        |}
        |
        |trait Runner[a] {
        |    type E: Eff
        |    pub def exec(x: a): Unit \ Runner.E[a]
        |}
        |
        |instance Runner[Int32] {
        |    pub def exec(x: Int32): Unit \ Out = Out.out(x)
        |}
        |
        |def g(): Unit \ Out = Runner.exec(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.05") {
    // There should be no type error because the higher-kinded associated type `T` is missing.
    // The Resolver reports MissingAssocTypeDef and recovers with an error type.
    val input =
      """
        |enum Maybe[a] {
        |    case Just(a),
        |    case Nothing
        |}
        |
        |trait C[a] {
        |    type S: Type
        |    type T: Type -> Type
        |    pub def f(x: a): C.T[a][C.S[a]]
        |}
        |
        |instance C[Int32] {
        |    type S = Int32
        |    pub def f(x: Int32): Maybe[Int32] = Maybe.Just(x)
        |}
        |
        |def g(): Maybe[Int32] = C.f(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.06") {
    // There should be no type error because the associated type `T` is defined twice.
    // The Resolver reports DuplicateAssocTypeDef and recovers by keeping the first definition.
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |instance C[Int32] {
        |    type T = Int32
        |    type T = String
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = C.f(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.07") {
    // There should be no type error because the associated type `U` is undefined.
    // The Resolver reports UndefinedAssocType and recovers by dropping the definition;
    // the default for `T` still applies.
    val input =
      """
        |trait C[a] {
        |    type T: Type = Int32
        |    pub def f(x: a): C.T[a]
        |}
        |
        |instance C[Int32] {
        |    type U = String
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = C.f(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.08") {
    // There should be no type error because the associated type `T` is missing on `C[Int32]`
    // and is reached through the nested instance `C[MyBox[a]]`.
    // The Resolver reports MissingAssocTypeDef and recovers with an error type.
    val input =
      """
        |enum MyBox[a](a)
        |
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |instance C[Int32] {
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |instance C[MyBox[a]] with C[a] {
        |    type T = C.T[a]
        |    pub def f(x: MyBox[a]): C.T[a] = let MyBox.MyBox(y) = x; C.f(y)
        |}
        |
        |def g(): Int32 = C.f(MyBox.MyBox(42))
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.09") {
    // There should be no type error because the trait `C` is undefined.
    // The Resolver reports UndefinedTrait and recovers by dropping the instance.
    val input =
      """
        |instance C[Int32] {
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = 42
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.10") {
    // There should be no type error because the trait `C` is undefined.
    // The Resolver reports UndefinedTrait and recovers by dropping the instance,
    // including its trait constraint and associated type definition.
    val input =
      """
        |enum MyBox[a](a)
        |
        |trait D[a] {
        |    pub def h(x: a): a
        |}
        |
        |instance C[MyBox[a]] with D[a] {
        |    type T = a
        |    pub def f(x: MyBox[a]): a = let MyBox.MyBox(y) = x; D.h(y)
        |}
        |
        |def g(): Int32 = 42
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.11") {
    // There should be no type error because the super trait `B` is undefined.
    // The Resolver reports UndefinedTrait and recovers by dropping the super trait,
    // so the instance `A[Int32]` does not require an instance of `B`.
    val input =
      """
        |trait A[a] with B[a] {
        |    pub def f(x: a): a
        |}
        |
        |instance A[Int32] {
        |    pub def f(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = A.f(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.12") {
    // There should be no type error because the super trait `B` is undefined.
    // The Resolver reports UndefinedTrait and recovers by dropping the super trait;
    // the constraint `A[a]` on `g` still resolves and `A.f(x)` type checks against it.
    val input =
      """
        |trait A[a] with B[a] {
        |    pub def f(x: a): a
        |}
        |
        |def g(x: a): a with A[a] = A.f(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.13") {
    // There should be no type error because the import `java.io.Fil` is undefined.
    // The Resolver reports UndefinedJvmImport and recovers by dropping the import,
    // so `Fil` is an undefined type which resolves to an error type.
    val input =
      """
        |import java.io.Fil
        |
        |def f(): Fil = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.14") {
    // There should be no type error because the use `A.fo` is undefined.
    // The Resolver reports UndefinedUse and recovers by dropping the use,
    // so `fo` is an undefined name which resolves to an error expression.
    val input =
      """
        |mod A {
        |    pub def foo(): Int32 = 42
        |}
        |
        |mod B {
        |    use A.fo
        |    pub def g(): Int32 = fo()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.15") {
    // There should be no type error because the import `java.util.Nope` is undefined.
    // The Resolver reports UndefinedJvmImport and recovers by dropping the import,
    // so the type alias `T` and the signature of `f` mention an error type.
    val input =
      """
        |mod A {
        |    import java.util.Nope
        |    pub type alias T = Nope
        |    pub def f(x: T): T = x
        |}
        |
        |def g(): Int32 = 42
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.16") {
    // There should be no type error because the trait hierarchy A <-> B is cyclic.
    // The Resolver reports CyclicTraitHierarchy and recovers by dropping the cyclic super traits,
    // so the instances and the constrained def type check against the repaired hierarchy.
    val input =
      """
        |trait A[a] with B[a] {
        |    pub def fa(x: a): Int32
        |}
        |
        |trait B[a] with A[a] {
        |    pub def fb(x: a): Int32
        |}
        |
        |instance A[Int32] {
        |    pub def fa(x: Int32): Int32 = x
        |}
        |
        |instance B[Int32] {
        |    pub def fb(x: Int32): Int32 = x
        |}
        |
        |def g(x: a): Int32 with A[a], B[a] = A.fa(x) + B.fb(x)
        |
        |def h(): Int32 = g(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.17") {
    // There should be no type error because the trait `S` is its own super trait.
    // The Resolver reports CyclicTraitHierarchy and recovers by dropping the self loop.
    val input =
      """
        |trait S[a] with S[a] {
        |    pub def fs(x: a): Int32
        |}
        |
        |instance S[Int32] {
        |    pub def fs(x: Int32): Int32 = x
        |}
        |
        |def g(): Int32 = S.fs(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.18") {
    // There should be no type error because the trait hierarchy C -> D -> E -> C is cyclic.
    // The Resolver reports CyclicTraitHierarchy and recovers by dropping the cyclic super traits,
    // while the super trait C of F (outside the cycle) is kept: F[a] implies C[a] in `g`.
    val input =
      """
        |trait C[a] with D[a] { pub def fc(x: a): Int32 }
        |trait D[a] with E[a] { pub def fd(x: a): Int32 }
        |trait E[a] with C[a] { pub def fe(x: a): Int32 }
        |trait F[a] with C[a] { pub def ff(x: a): Int32 }
        |
        |instance C[Int32] { pub def fc(x: Int32): Int32 = x }
        |instance F[Int32] { pub def ff(x: Int32): Int32 = x }
        |
        |def g(x: a): Int32 with F[a] = F.ff(x) + C.fc(x)
        |
        |def h(): Int32 = g(42)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("ErrorType.19") {
    // There should be no type error because the type aliases `A` and `B` are cyclic.
    // The Resolver reports CyclicTypeAliases and recovers by replacing the cyclic references
    // with error types, so `A` is `(Int32, Error)` and `f` type checks against it.
    val input =
      """
        |type alias A = (Int32, B)
        |type alias B = A
        |type alias C = A
        |
        |def f(x: A): Int32 = fst(x)
        |
        |def g(x: C): Int32 = fst(x)
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    rejectError[TypeError](result)
  }

  test("ErrorType.20") {
    // There should be no type error because the type alias `L` refers to itself.
    // The Resolver reports CyclicTypeAliases and recovers by replacing the cyclic reference
    // with an error type, keeping the argument: `L[a]` is `Option[Error[a]]`.
    val input =
      """
        |type alias L[a] = Option[L[a]]
        |
        |def f(x: L[Int32]): Int32 = match x {
        |    case Some(y) => y
        |    case None => 0
        |}
        |
        |def g(): L[Bool] = Some(Some(None))
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    rejectError[TypeError](result)
  }

  test("ErrorType.21") {
    // There should be no type error because the type aliases `P`, `Q` and `R` are cyclic through
    // a function type and a record type. The Resolver reports CyclicTypeAliases and recovers by
    // replacing the cyclic references with error types.
    val input =
      """
        |type alias P = Q -> Int32
        |type alias Q = {x = R}
        |type alias R = P
        |
        |def f(x: P): Int32 = x({x = 1})
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("UndefinedLabel.01") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {x = 1};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("UndefinedLabel.02") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {y = 2};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("UndefinedLabel.03") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, x = String} = {x = 1};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("UndefinedLabel.04") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 42})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("UndefinedLabel.05") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({y = 42})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("UndefinedLabel.06") {
    val input =
      """
        |def foo(r: {x = Int32, x = String}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 42})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
  }

  test("ExtraLabel.01") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {x = 1, y = 2, z = 3};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtraLabel.02") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {w = 0, x = 1, y = 2, z = 3};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtraLabel.03") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {x = 1, y = 2, z = 3, z = "foo"};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtraLabel.04") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 1, y = 2, z = 3})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtraLabel.05") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({w = 0, x = 1, y = 2, z = 3})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtraLabel.06") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 1, y = 2, z = 3, z = "foo"})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.01") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {x = 1, z = 3};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.02") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, y = Int32} = {w = 0, z = 3};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.03") {
    val input =
      """
        |def foo(): Unit \ IO = {
        |  let r: {x = Int32, x = String} = {x = 1, z = 3, z = "foo"};
        |  println(r#x)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.04") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 1, z = 3})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.05") {
    val input =
      """
        |def foo(r: {x = Int32, y = Int32}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({w = 0, z = 3})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("UndefinedAndExtraLabel.06") {
    val input =
      """
        |def foo(r: {x = Int32, x = String}): Unit \ IO = {
        |  println(r#x)
        |}
        |
        |def bar(): Unit \ IO = {
        |  foo({x = 1, z = 3, z = "foo"})
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UndefinedLabel](result)
    expectError[TypeError.ExtraLabel](result)
  }

  test("ExtMatchError#11283") {
    val input =
      """
        |def f(): Bool = {
        |    ematch xvar A(1) {
        |        case A() => true
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.01") {
    val input =
      """
        |def f(): Unit =
        |    ematch xvar X("hello") {
        |        case A() => ()
        |    }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.02") {
    val input =
      """
        |def f(): Unit =
        |    ematch xvar X(42i32, "test", true) {
        |        case B(x, y)       => ()
        |        case A(a, b, c, d) => ()
        |        case C()           => ()
        |    }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.03") {
    val input =
      """
        |def f(): Unit =
        |    ematch xvar X(true, 'a', 3.14f64, "hello", 100i8) {
        |        case C(b, c, d)    => ()
        |        case A(x, y, z, w) => ()
        |        case B()           => ()
        |    }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.04") {
    val input =
      """
        |def f(): Unit =
        |    ematch xvar X(3.14f64, 42i16, 'x', true, "world", 999i64) {
        |        case A(s, t)                => ()
        |        case C(a, b, c, d, e)       => ()
        |        case B()                    => ()
        |        case X(p, q, r, s, t, u, v) => ()
        |    }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.05") {
    val input =
      """
        |def f(): Unit =
        |    ematch xvar X(1i32, 2i32, 3i32, 4i32) {
        |        case C(x, y, z)       => ()
        |        case A(a, b, c, d, e) => ()
        |        case B()              => ()
        |        case X(p, q)          => ()
        |    }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.06") {
    val input =
      """
        |def f(): Bool = {
        |    let scrutinee = if (true) xvar A(1) else xvar B(1);
        |    ematch scrutinee {
        |        case A(x) => x == 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.07") {
    val input =
      """
        |def f(var: #| A(Int32), B(Int32) | r |#): Bool = {
        |    ematch var {
        |        case A(x) => x == 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.08") {
    val input =
      """
        |def f(var: #| A(Int32), B(Int32) | r |#): Bool = {
        |    ematch var {
        |        case A(x) => x == 1
        |        case B(x) => x == 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TypeError.ExtMatch.09") {
    val input =
      """
        |def g(): Bool = f(xvar C(1))
        |
        |def f(var: #| A(Int32), B(Int32) |#): Bool = {
        |    ematch var {
        |        case A(x) => x == 1
        |        case B(x) => x == 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArg](result)
  }

  test("TypeError.ExtMatch.10") {
    val input =
      """
        |def f(): Bool = {
        |    let scrutinee = if (true) xvar A(false) else xvar B(true);
        |    (ematch A(x) -> x)(scrutinee)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedType](result)
  }

  test("TypeError.MismatchedPredicateArity.01") {
    val input =
      """
        |def main(): Unit \ IO =
        |    let _ = #{
        |        Foo(1).
        |        Foo(1, 2).
        |    };
        |    println("Hello World!")
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedPredicateArity](result)
  }

  test("TypeError.MismatchedPredicateArity.02") {
    val input =
      """
        |def main(): Unit \ IO =
        |    let _ = #{
        |        Foo(1).
        |        Foo(1, 2).
        |        Foo(1, 2, 3).
        |    };
        |    println("Hello World!")
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedPredicateArity](result)
  }

  test("TypeError.MismatchedPredicateArity.03") {
    val input =
      """
        |def main(): Unit \ IO =
        |    let _ = #{
        |        Foo(;1).
        |        Foo(1; 2).
        |        Foo(1, 2; 3).
        |    };
        |    println("Hello World!")
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibAll)
    expectError[TypeError.MismatchedPredicateArity](result)
  }

  test("TypeError.MismatchedPredicateDenotation.01") {
    val input =
      """
        |def main(): Unit \ IO =
        |    let _ = #{
        |        Foo(1, 2).
        |        Foo(1; 2).
        |    };
        |    println("Hello World!")
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibAll)
    expectError[TypeError.MismatchedPredicateDenotation](result)
  }

  test("TypeError.MismatchedPredicateDenotation.02") {
    val input =
      """
        |def main(): Unit \ IO =
        |    let _ = #{
        |        Foo(1; 2).
        |        Foo(1, 2).
        |    };
        |    println("Hello World!")
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibAll)
    expectError[TypeError.MismatchedPredicateDenotation](result)
  }

  test("Test.DefaultHandlerNotInModule.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |@DefaultHandler
        |pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.DefaultHandlerNotInModule](result)
  }

  test("Test.IllegalDefaultHandlerSignature.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef, u: a): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.03") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: a): a \ (ef - E) + IO =
        |            checked_ecast(f)
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.04") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): Bool \ (ef - E) + IO =
        |            run {
        |                true
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.05") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ {}): a \ IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.06") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Bool -> a \ ef, u: a): a \ (ef - E) + IO =
        |            run {
        |                f(true)
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.IllegalDefaultHandlerSignature.07") {
    val input =
      """
        |pub eff E1 {
        |   def op(): Unit
        |}
        |
        |pub eff E2 {
        |   def op(): Unit
        |}
        |
        |mod E1 {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E1) + IO + E2 =
        |            run {
        |                f()
        |            } with handler E1 {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.IllegalDefaultHandlerSignature](result)
  }

  test("Test.NonPublicDefaultHandler.01") {
    val input =
      """
        |pub eff E1 {
        |   def op(): Unit
        |}
        |
        |mod E1 {
        |    @DefaultHandler
        |    def runWithIO(f: Unit -> a \ ef): a \ (ef - E1) + IO =
        |            run {
        |                f()
        |            } with handler E1 {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.NonPublicDefaultHandler](result)
  }

  test("Test.DuplicateDefaultHandler.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |
        |    @DefaultHandler
        |    pub def runWithIO2(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour 2");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.DuplicateDefaultHandler](result)
  }

  test("TypeError.NonUnitStatement.01") {
    val input =
      """
        |def f(): String = 123; "hi"
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.NonUnitStatement](result)
  }

  test("TypeError.NonUnitStatement.Jvm.01") {
    val input =
      """
        |def f(): String \ IO = "".toString(); ""
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  test("TypeError.NonUnitStatement.Jvm.02") {
    val input =
      """
        |import java.lang.Object
        |def f(): String \ IO = Objects.toString(""); ""
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[TypeError](result)
  }

  // --- Java Generic Type Checking: Negative Tests (Bug 1 - wrong argument types) ---

  test("Test.JavaGenericCheck.Neg.01") {
    val input =
      raw"""
           |import java.util.ArrayList
           |def f(): Bool \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    l.add(123)
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.02") {
    val input =
      raw"""
           |import java.util.ArrayList
           |def f(): Bool \ IO =
           |    let l: ArrayList[Int32] = new ArrayList();
           |    l.add("hello")
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.03") {
    val input =
      raw"""
           |import java.util.ArrayList
           |def f(): Bool \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    l.add(true)
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.04") {
    val input =
      raw"""
           |import java.util.ArrayList
           |def f(): Bool \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    l.add(1.0f64)
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.05") {
    val input =
      raw"""
           |import java.util.HashMap
           |def f(): Unit \ IO =
           |    let m: HashMap[String, Int32] = new HashMap();
           |    m.put(123, 42);
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.06") {
    val input =
      raw"""
           |import java.util.HashMap
           |def f(): Unit \ IO =
           |    let m: HashMap[String, Int32] = new HashMap();
           |    m.put("k", "v");
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.07") {
    val input =
      raw"""
           |import java.util.HashSet
           |def f(): Bool \ IO =
           |    let s: HashSet[Int32] = new HashSet();
           |    s.add("hello")
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.08") {
    val input =
      raw"""
           |import java.util.ArrayList
           |def f(): Unit \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    l.add("a");
           |    l.set(0, 42);
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.09") {
    val input =
      raw"""
           |import java.util.LinkedList
           |def f(): Bool \ IO =
           |    let l: LinkedList[Float64] = new LinkedList();
           |    l.add("x")
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.10") {
    val input =
      raw"""
           |import java.util.TreeMap
           |def f(): Unit \ IO =
           |    let m: TreeMap[String, Bool] = new TreeMap();
           |    m.put(42, true);
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  // --- Java Generic Type Checking: Negative Tests (Bug 2 - wrong return type) ---

  test("Test.JavaGenericCheck.Neg.11") {
    val input =
      raw"""
           |import java.util.HashMap
           |import java.util.{Set => JSet}
           |def f(): Unit \ IO =
           |    let m: HashMap[String, Int32] = new HashMap();
           |    let _s: JSet[Float32] = m.keySet();
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.12") {
    val input =
      raw"""
           |import java.util.HashMap
           |import java.util.{Set => JSet}
           |def f(): Unit \ IO =
           |    let m: HashMap[String, Int32] = new HashMap();
           |    let _s: JSet[Int32] = m.keySet();
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.13") {
    val input =
      raw"""
           |import java.util.HashMap
           |import java.util.Collection
           |def f(): Unit \ IO =
           |    let m: HashMap[String, Int32] = new HashMap();
           |    let _v: Collection[String] = m.values();
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.14") {
    val input =
      raw"""
           |import java.util.ArrayList
           |import java.util.Iterator
           |def f(): Unit \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    let _it: Iterator[Int32] = l.iterator();
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.JavaGenericCheck.Neg.15") {
    val input =
      raw"""
           |import java.util.ArrayList
           |import java.util.{List => JList}
           |def f(): Unit \ IO =
           |    let l: ArrayList[String] = new ArrayList();
           |    l.add("a");
           |    let _sub: JList[Int32] = l.subList(0, 1);
           |    ()
         """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

}
