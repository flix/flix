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

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.util._
import org.scalatest.FunSuite

import scala.language.implicitConversions

// TODO: Remove TestBackend
class TestBackend extends FunSuite {

  private class Tester(input: String, solve: Boolean = true, dumpBytecode: Boolean = false) {

    private val interpretedFlix = createFlix(codegen = false).addStr(input)
    private val compiledFlix = createFlix(codegen = true).addStr(input)
    private var interpreted: CompilationResult = null
    private var compiled: CompilationResult = null

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

  test("Term.Head.Exp.08") {
    val input =
      """rel A(x: (Int, Str))
        |
        |A((1, "one")).
      """.stripMargin
    val t = new Tester(input)
  }

  // TODO
//  ignore("Term.Head.Exp.09") {
//    val input =
//      """enum Foo { case Foo(Int,Str) }
//        |rel A(x: Foo)
//        |
//        |A(Foo.Foo(1, "one")).
//      """.stripMargin
//    val t = new Tester(input)
//  }

  test("Term.Head.Exp.10") {
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

}
