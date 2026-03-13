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

  test("Test.ExplicitlyPureUsingIO.01") {
    val input =
      """
        |def f () : Unit \ {} = {
        |    println("42")
        |}
        |eff IO
        |pub def println(x: String): Unit \ IO =
        |    System.out.println(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
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
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingIO.03") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit \ {} = println(x)
        |}
        |def foo(): Unit \ IO =
        |    Bar.bar(42)
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingIO.04") {
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
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingIO.05") {
    val input =
      """
        |def a(): Unit \ IO = println(42)
        |def b(): Unit \ {} = a()
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingIO.06") {
    val input =
      """
        |def f(h: Unit -> Unit \ {}): Unit \ {}  =
        |    h(println(""))
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ExplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureUsingIO.01") {
    val input =
      """
        |def f () : Unit = {
        |    println("42")
        |}
        |eff IO
        |pub def println(x: String): Unit \ IO =
        |    System.out.println(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureUsingIO.02") {
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
    expectError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureUsingIO.03") {
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Unit
        |}
        |instance Bar[Int32] {
        |    pub def bar(x: Int32): Unit = println(x)
        |}
        |def foo(): Unit \ IO =
        |    Bar.bar(42)
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureUsingIO.04") {
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
    expectError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ImplicitlyPureUsingIO.05") {
    val input =
      """
        |def a(): Unit \ IO = println(42)
        |def b(): Unit = a()
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ImplicitlyPureFunctionUsesIO](result)
  }

  test("Test.ExplicitlyPureUsingEffect.01") {
    val input =
      """
        |def foo(): Unit \ {} =
        |    Bar.buzz()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ExplicitlyPureUsingEffect.02") {
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
    expectError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
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
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ExplicitlyPureUsingEffect.04") {
    val input =
      """
        |def f(h: Unit -> Unit \ ef): Unit \ {}  =
        |    let m = h;
        |    m()
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ExplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureUsingEffect.01") {
    val input =
      """
        |def foo(): Unit =
        |    Bar.buzz()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureUsingEffect.02") {
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
    expectError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.ImplicitlyPureUsingEffect.03") {
    val input =
      """
        |def a(): Unit \ Bar = Bar.buzz()
        |def b(): Unit = a()
        |eff Bar {
        |    def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ImplicitlyPureFunctionUsesEffect](result)
  }

  test("Test.RigidEffUsingOtherRigidEff.01") {
    val input =
      """
        |def foo(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2): Unit \ ef1 = g()
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.RigidEffUsingCstEff.01") {
    val input =
      """
        |def foo(_: Unit -> Unit \ ef1): Unit \ ef1 = Bar.buzz()
        |eff Bar {
        |  def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.RigidEffUsingIO.01") {
    val input =
      """
        |def foo(_: Unit -> Unit \ ef1): Unit \ ef1 = println("42")
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.CstEffUsingOtherCstEff.01") {
    val input =
      """
        |def foo(): Unit \ Foo = Bar.buzz()
        |eff Foo
        |eff Bar {
        |  def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.IOUsingCstEff.01") {
    val input =
      """
        |def foo(): Unit \ IO = Bar.buzz()
        |eff Bar {
        |  def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.IOUsingCstEff.02") {
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
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.CstEffUsingIO.01") {
    val input =
      """
        |enum Shape {
        |   case Circle(Int32),
        |   case Square(Int32),
        |   case Rectangle(Int32, Int32)
        |}
        |def area(s: Shape): Int32 \ Bar = match s {
        |   case Shape.Circle(r)       => 3 * (r * r)
        |   case Shape.Square(w)       =>
        |       println(w);
        |       w * w
        |   case Shape.Rectangle(h, w) => h * w
        |}
        |def main(): Unit \ Bar =
        |   let _ = area(Shape.Rectangle(2, 4));
        |   ()
        |eff Bar {
        | def buzz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.CstEffNotUsed.01") {
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
    expectError[TypeError.UnusedEffectInSignature](result)
  }

  test("Test.EffectfulFunctionUsesOtherEffect.01") {
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
    expectError[TypeError.EffectfulFunctionUsesOtherEffect](result)
  }

  test("Test.PureArgumentGivenIO.01") {
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
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.PureArgumentGivenCstEffect.01") {
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
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.PureArgumentGivenIO.02") {
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
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.CstArgumentGivenIO.01") {
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
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.IOArgumentGivenPure.01") {
    val input =
      """
        |def hof1(f: Unit -> Unit \ IO): Unit \ IO = f()
        |def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()
        |
        |def foo(): Unit \ IO + Bar =
        |    let f = () -> ();
        |    hof1(f);
        |    hof2(f)
        |eff Bar {
        |    def baz(): Unit
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.PureArgumentGivenEffect.01") {
    val input =
      """
        |def p(_: Unit -> Unit): Unit = ()
        |
        |def f(h: Unit -> Unit \ ef): Unit \ {}  =
        |    p(h)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }

  test("Test.ArgumentGivenWrongEffect.01") {
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
        |
        |
        |def foo(): Unit \ IO =
        |    let p = println;
        |    let q = x -> x;
        |    f(Shape.Circle(0), p >> q, p)
      """.stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[TypeError.ArgumentGivenWrongEffect](result)
  }
}
