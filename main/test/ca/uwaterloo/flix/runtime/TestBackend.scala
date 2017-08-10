/*
 * Copyright 2015-2016 Ming-Ho Yee
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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{Unit => UnitClass, _}
import ca.uwaterloo.flix.util._
import org.scalatest.FunSuite

import scala.language.implicitConversions

// TODO: Class is deprecated.
class TestBackend extends FunSuite {

  private class Tester(input: String, solve: Boolean = true, dumpBytecode: Boolean = false) {

    private val interpretedFlix = createFlix(codegen = false).addStr(input)
    private val compiledFlix = createFlix(codegen = true).addStr(input)
    private var interpreted: Model = null
    private var compiled: Model = null

    // A public Flix instance to expose the interop functions.
    val flix = createFlix()

    private def createFlix(codegen: Boolean = false) = {
      val options = Options.DefaultTest.copy(evaluation = if (codegen) Evaluation.Compiled else Evaluation.Interpreted)
      new Flix().setOptions(options)
    }

    def addReachableRoot(fqn: String): Tester = {
      interpretedFlix.addReachableRoot(fqn)
      compiledFlix.addReachableRoot(fqn)
      flix.addReachableRoot(fqn)
      this
    }

    def run(): Tester = {
      compiled = compiledFlix.solve().get
      this
    }

    def recursiveGetBoxed(res : AnyRef) : AnyRef = res match {
      case r : Enum => {
        new Value.Tag(null, r.getTag, recursiveGetBoxed(r.getBoxedValue()))
      }
      case r : Tuple => {
        r.getBoxedValue().map(recursiveGetBoxed)
      }
      case r : UnitClass => Value.Unit
      case x => x
    }


    def runTest(expected: AnyRef, const: String): Unit = {
      withClue(s"interpreted value $const:") { interpreted.getConstant(const) }
      withClue(s"compiled value $const:") { recursiveGetBoxed(compiled.getConstant(const)) }
    }

    def runInterceptTest[T <: AnyRef](const:String)(implicit manifest: Manifest[T]): Unit = {
      withClue(s"interpreted value $const:") { intercept[T](interpreted.getConstant(const)) }
      withClue(s"compiled value $const:") { intercept[T](compiled.getConstant(const)) }
    }

    def checkModel(expected: AnyRef, model: String): Unit = {
      withClue(s"interpreted model $model:") { assertResult(expected)(interpreted.getRelation(model).toSet) }
      withClue(s"compiled model $model:") { assertResult(expected)(compiled.getRelation(model).map(x => x.map(recursiveGetBoxed)).toSet) }
    }

    // By default, solve the Flix program immediately.
    // But in some cases we want to defer solving, so we can add initially reachable function symbols or hooks to native functions.
    if (solve) run()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Exp                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Exp.01") {
    val input =
      """rel A(x: Unit)
        |
        |A(()).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.02") {
    val input =
      """rel A(x: Bool)
        |
        |A(true).
        |A(false).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.03") {
    val input =
      """rel A(x: Int8)
        |
        |A(1i8).
        |A(2i8).
        |A(3i8).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.04") {
    val input =
      """rel A(x: Int16)
        |
        |A(1i16).
        |A(2i16).
        |A(3i16).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.05") {
    val input =
      """rel A(x: Int32)
        |
        |A(1i32).
        |A(2i32).
        |A(3i32).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.06") {
    val input =
      """rel A(x: Int64)
        |
        |A(1i64).
        |A(2i64).
        |A(3i64).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.07") {
    val input =
      """rel A(x: Str)
        |
        |A("one").
        |A("two").
        |A("three").
      """.stripMargin
    val t = new Tester(input)
  }

  ignore("Term.Head.Exp.08") { // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Str))
        |
        |A((1, "one")).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo)
        |
        |A(Foo.Foo(1, "one")).
      """.stripMargin
    val t = new Tester(input)
  }

  ignore("Term.Head.Exp.10") {  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Int))
        |
        |A((1, 2)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.11") {
    val input =
      """rel A(x: Char)
        |
        |A('a').
        |A('b').
        |A('c').
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.12") {
    val input =
      """rel A(x: Float32)
        |
        |A(1.0f32).
        |A(2.0f32).
        |A(3.0f32).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.13") {
    val input =
      """rel A(x: Float64)
        |
        |A(1.0f64).
        |A(2.0f64).
        |A(3.0f64).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Exp.14") {
    val input =
      """rel A(x: BigInt)
        |
        |A(1ii).
        |A(2ii).
        |A(3ii).
      """.stripMargin
    val t = new Tester(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Apply                                                         //
  // These tests simply re-implement the Term.Head.Exp tests using Apply.    //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Apply.01") {
    val input =
      """rel A(x: ())
        |def f(x: Int): () = ()
        |
        |A(f(0)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.02") {
    val input =
      """rel A(x: Bool)
        |def f(x: Int): Bool = x == 0
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.03") {
    val input =
      """rel A(x: Int8)
        |def f(x: Int8): Int8 = x + 1i8
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.04") {
    val input =
      """rel A(x: Int16)
        |def f(x: Int16): Int16 = x + 1i16
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.05") {
    val input =
      """rel A(x: Int32)
        |def f(x: Int32): Int32 = x + 1i32
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.06") {
    val input =
      """rel A(x: Int64)
        |def f(x: Int64): Int64 = x + 1i64
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.07") {
    val input =
      """rel A(x: Str)
        |def f(x: Str): Str = x
        |
        |A(f("one")).
        |A(f("two")).
        |A(f("three")).
      """.stripMargin
    val t = new Tester(input)
  }

  ignore("Term.Head.Apply.08") { //  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Str))
        |def f(x: Int): (Int, Str) = (x, "one")
        |
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo)
        |def f(x: Str): Foo = Foo.Foo(1, x)
        |
        |A(f("one")).
      """.stripMargin
    val t = new Tester(input)
  }

  ignore("Term.Head.Apply.10") { //  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Int))
        |def f(x: Int, y: Int): (Int, Int) = (x, y)
        |
        |A(f(1, 2)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.11") {
    val input =
      """rel A(x: Char)
        |def f(x: Char): Char = x
        |
        |A(f('a')).
        |A(f('b')).
        |A(f('c')).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.12") {
    val input =
      """rel A(x: Float32)
        |def f(x: Float32): Float32 = x
        |
        |A(f(1.0f32)).
        |A(f(2.0f32)).
        |A(f(3.0f32)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.13") {
    val input =
      """rel A(x: Float64)
        |def f(x: Float64): Float64 = x
        |
        |A(f(1.0f64)).
        |A(f(2.0f64)).
        |A(f(3.0f64)).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Head.Apply.14") {
    val input =
      """rel A(x: BigInt)
        |def f(x: BigInt): BigInt = x + 1ii
        |
        |A(f(0ii)).
        |A(f(1ii)).
        |A(f(2ii)).
      """.stripMargin
    val t = new Tester(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Body.Wildcard                                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: See issue #65: https://github.com/magnus-madsen/flix/issues/65

  /////////////////////////////////////////////////////////////////////////////
  // Term.Body.Var                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Body.Var.01") {
    val input =
      """rel A(x: Bool, y: Bool)
        |rel B(x: Bool)
        |def f(x: Bool): Bool = x
        |
        |A(true, true).
        |A(false, true).
        |
        |B(y) :- f(x), A(x, y).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Var.02") {
    val input =
      """rel A(x: Int)
        |rel B(x: Int)
        |def f(x: Int): Bool = x % 2 == 0
        |
        |A(0).
        |A(1).
        |A(2).
        |A(3).
        |
        |B(x) :- f(x), A(x).
      """.stripMargin
    val t = new Tester(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Body.Exp                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Body.Exp.01") {
    val input =
      """rel A(x: Int)
        |def f(x: Bool): Bool = x
        |
        |A(1) :- f(true).
        |A(2) :- f(true).
        |A(3) :- f(true).
        |A(4) :- f(false).
        |A(5) :- f(false).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.02") {
    val input =
      """rel A(x: Int)
        |def f(x: Int8): Bool = x >= 0i8
        |
        |A(1) :- f(0i8).
        |A(2) :- f(0i8).
        |A(3) :- f(0i8).
        |A(4) :- f(-1i8).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.03") {
    val input =
      """rel A(x: Int)
        |def f(x: Int16): Bool = x >= 0i16
        |
        |A(1) :- f(0i16).
        |A(2) :- f(0i16).
        |A(3) :- f(0i16).
        |A(4) :- f(-200i16).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.04") {
    val input =
      """rel A(x: Int)
        |def f(x: Int32): Bool = x >= 0i32
        |
        |A(1) :- f(0i32).
        |A(2) :- f(0i32).
        |A(3) :- f(0i32).
        |A(4) :- f(-200000i32).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.05") {
    val input =
      """rel A(x: Int)
        |def f(x: Int64): Bool = x >= 0i64
        |
        |A(1) :- f(0i64).
        |A(2) :- f(0i64).
        |A(3) :- f(0i64).
        |A(4) :- f(-20000000000i64).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.06") {
    val input =
      """rel A(x: Int)
        |def f(x: Str): Bool = true
        |
        |A(1) :- f("foo").
        |A(2) :- f("bar").
        |A(3) :- f("baz").
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.09") {
    val input =
      """rel A(x: Int)
        |def f(x: Char): Bool = x >= 'b'
        |
        |A(1) :- f('b').
        |A(2) :- f('b').
        |A(3) :- f('b').
        |A(4) :- f('a').
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.10") {
    val input =
      """rel A(x: Int)
        |def f(x: Float32): Bool = x >= 0.0f32
        |
        |A(1) :- f(0.0f32).
        |A(2) :- f(0.0f32).
        |A(3) :- f(0.0f32).
        |A(4) :- f(-1.0f32).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.11") {
    val input =
      """rel A(x: Int)
        |def f(x: Float64): Bool = x >= 0.0f64
        |
        |A(1) :- f(0.0f64).
        |A(2) :- f(0.0f64).
        |A(3) :- f(0.0f64).
        |A(4) :- f(-1.0f64).
      """.stripMargin
    val t = new Tester(input)
  }

  test("Term.Body.Exp.12") {
    val input =
      """rel A(x: Int)
        |def f(x: BigInt): Bool = x >= 0ii
        |
        |A(1) :- f(0ii).
        |A(2) :- f(0ii).
        |A(3) :- f(0ii).
        |A(4) :- f(-100000000000000000000ii).
      """.stripMargin
    val t = new Tester(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Regression tests                                                        //
  /////////////////////////////////////////////////////////////////////////////

  test("Regression.01") {
    // See: https://github.com/magnus-madsen/flix/issues/149
    val input =
      """rel Load(label: Str, to: Str, from: Str)
        |rel Pt(variable: Str, target: Str)
        |rel PtH(object: Str, target: Str)
        |
        |// Note how `p` appears in both in the rule and function arg list.
        |// Calling the function overwrites the value of `p`.
        |Pt(p,b) :- Load(l,p,q), Pt(q,a), filter(b), PtH(a,b).
        |def filter(p: Str): Bool = true
        |
        |// Input facts.
        |Load("3", "d", "p").
        |Pt("b", "b").
        |Pt("p", "c").
        |PtH("c","b").
        |
        |// Expected output.
        |//Pt("d", "b").
      """.stripMargin
    val t = new Tester(input)
  }

}
