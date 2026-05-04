/*
 * Copyright 2026 Alexander Sommer, Samuel Skovbakke
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
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEffectProvenance extends AnyFunSuite with TestUtils {

  test("Test.ExplicitlyPureFunctionUsesIO.01") {
    val input =
      """
        |def f () : Unit \ {} = {
        |   IO.println("42")
        |}
        |eff IO {
        |   def println(x: String): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingIO.02") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 \ {} = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       println(w);
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ IO =
        |   println(area(Shape.Rectangle(2, 4)))
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureFunctionUsesIO.03") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit \ {} = println(x)
        |}
        |def foo(): Unit =
        |    Bar.bar(42)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureFunctionUsesIO.04") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit \ IO
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit \ IO = println(x)
        |}
        |def foo(): Unit \ {} =
        |    Bar.bar(42)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureFunctionUsesIO.05") {
    val input =
      """
        |def a(): Unit \ IO = println(42)
        |def b(): Unit \ {} = a()
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureFunctionUsesIO.06") {
    val input =
      """
        |def f(h: Unit -> Unit \ {}): Unit \ {}  =
        |    h(println(""))
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureFunctionUsesIO.01") {
    val input =
      """
        |def f () : Unit = {
        |    println("42")
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureFunctionUsesIO.02") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       println(w);
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ IO =
        |   println(area(Shape.Rectangle(2, 4)))
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureFunctionUsesIO.03") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit = println(x)
        |}
        |def foo(): Unit =
        |    Bar.bar(42)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureFunctionUsesIO.04") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit \ IO
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit \ IO = println(x)
        |}
        |def foo(): Unit =
        |    Bar.bar(42)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureFunctionUsesIO.05") {
    val input =
      """
        |def a(): Unit \ IO = println(42)
        |def b(): Unit = a()
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureFunctionUsesEffect.01") {
    val input =
      """
        |def foo(): Unit \ {} =
        |    Bar.buzz()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ExplicitlyPureFunctionUsesEffect.02") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 \ {} = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       Bar.buzz();
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ IO =
        |   println(area(Shape.Rectangle(2, 4)))
        |eff Bar {
        |    def buzz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ExplicitlyPureUsingEffect.03") {
    val input =
      """
        |def a(): Unit \ Bar = Bar.buzz()
        |def b(): Unit \ {} = a()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ExplicitlyPureFunctionUsesEffect.04") {
    val input =
      """
        |def f(h: Unit -> Unit \ ef): Unit \ {}  =
        |    let m = h;
        |    m()
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureFunctionUsesEffect.01") {
    val input =
      """
        |def foo(): Unit =
        |    Bar.buzz()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureFunctionUsesEffect.02") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       Bar.buzz();
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ IO =
        |   println(area(Shape.Rectangle(2, 4)))
        |eff Bar {
        |    def buzz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureFunctionUsesEffect.03") {
    val input =
      """
        |def a(): Unit \ Bar = Bar.buzz()
        |def b(): Unit = a()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.01") {
    val input =
      """
        |def foo(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2): Unit \ ef1 = f(); g()
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.02") {
    val input =
      """
        |def foo(f: Unit -> Unit \ ef1): Unit \ ef1 = f(); Bar.buzz()
        |eff Bar {
        |  def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.03") {
    val input =
      """
        |def foo(f: Unit -> Unit \ ef1): Unit \ ef1 = f(); println("42")
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.04") {
    val input =
      """
        |def foo(): Unit \ Foo = Foo.f(); Bar.buzz()
        |eff Foo {
        |   def f(): Unit
        |}
        |eff Bar {
        |   def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.05") {
    val input =
      """
        |def foo(): Unit \ IO = println("42"); Bar.buzz()
        |eff Bar {
        |  def buzz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.06") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 \ IO = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       println("42");
        |       Bar.buzz();
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ IO =
        |   println(area(Shape.Rectangle(2, 4)))
        |eff Bar {
        |    def buzz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.08") {
    val input =
      """
        |def foo(): Unit \ {Foo, IO} =
        |    Foo.f();
        |    Bar.baz();
        |    println("€")
        |
        |eff Foo {
        |    def f(): Unit
        |}
        |eff Bar {
        |    def baz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.UnusedEffectInSignature.01") {
    val input =
      """
        |def foo(): Unit \ {IO, Bar} =
        |    println("€")
        |
        |eff Bar {
        |    def baz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.UnusedEffectInSignature](result)
  }

  test("Test.UnusedEffectInSignature.02") {
    val input =
      """
        |def foo(): Unit \ {Bar, Baz} =
        |    Bar.op1()
        |
        |eff Bar {
        |    def op1(): Unit
        |}
        |
        |eff Baz {
        |    def op2(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.UnusedEffectInSignature](result)
  }

  test("Test.UnusedEffectInSignature.03") {
    val input =
      """
        |def foo(): Int32 \ Bar =
        |    42
        |
        |eff Bar {
        |    def baz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.UnusedEffectInSignature](result)
  }

  test("Test.ArgumentGivenWrongEffect.01") {
    val input =
      """
        |def hof1(f: Unit -> Unit): Unit = f()
        |def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()
        |
        |def foo(): Unit \ IO =
        |    let f = () -> println("€");
        |    hof1(f);
        |    hof2(f)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.02") {
    val input =
      """
        |def hof1(f: Unit -> Unit): Unit = f()
        |def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()
        |
        |def foo(): Unit \ Bar =
        |    let f = () -> Bar.baz();
        |    hof1(f);
        |    hof2(f)
        |eff Bar {
        |    def baz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.03") {
    val input =
      """
        |def p(_: Unit -> Unit): Unit = ()
        |
        |def f(): Unit \ {}  =
        |    let f = x -> println(x);
        |    let g = x -> f(x);
        |    p(f >> g)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.04") {
    val input =
      """
        |def hof1(f: Unit -> Unit \ Bar): Unit \ Bar = f()
        |def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()
        |
        |def foo(): Unit \ IO + Bar =
        |    let f = () -> println("€");
        |    hof1(f);
        |    hof2(f)
        |eff Bar {
        |    def baz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.05") {
    val input =
      """
        |def hof1(f: Unit -> Unit \ IO): Unit \ IO = f()
        |def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()
        |
        |def foo(): Unit \ IO + Bar =
        |    Bar.baz();
        |    let f = () -> ();
        |    hof1(f);
        |    hof2(f)
        |eff Bar {
        |    def baz(): Unit
        |}
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.06") {
    val input =
      """
        |def p(_: Unit -> Unit): Unit = ()
        |
        |def f(h: Unit -> Unit \ ef): Unit \ {}  =
        |    p(h)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectOneError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.07") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |eff Bar
        |def f(s: Shape, g: Unit -> Unit \ Bar, h: Unit -> Unit \ ef2): Unit \ Bar + ef2  = match s {
        |   case Shape.Circle(_) => g()
        |   case _ => h()
        |}
        |def foo(): Unit \ IO =
        |    let p = println;
        |    let q = x -> x;
        |    f(Shape.Circle(0), p >> q, p)
        """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }
}
