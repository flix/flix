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
      interpreted = interpretedFlix.solve().get
      compiled = compiledFlix.solve().get
      this
    }

    def recursiveGetBoxed(res : AnyRef) : AnyRef = res match {
      case r : Enum => {
        new Value.Tag(r.getTag, recursiveGetBoxed(r.getBoxedValue()))
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
  // Expression.Binary (Logical)                                             //
  // BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.LogicalAnd.06") {
    val input = "def f(): Bool = true && ???"
    val t = new Tester(input)
    t.runInterceptTest[UserException]("f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.06") {
    val input = "def f(): Bool = false || ???"
    val t = new Tester(input)
    t.runInterceptTest[UserException]("f")
  }
  
  /////////////////////////////////////////////////////////////////////////////
  // Expression.Let                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Let.01") {
    val input = "def f(): Int = let x = true; 42"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "f")
  }

  test("Expression.Let.02") {
    val input = "def f(): Int8 = let x = 42i8; x"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "f")
  }

  test("Expression.Let.03") {
    val input = "def f(): Int16 = let x = 1i16; x + 2i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(3), "f")
  }

  test("Expression.Let.04") {
    val input = """def f(): Str = let x = false; if (x) "abz" else "xyz""""
    val t = new Tester(input)
    t.runTest(Value.mkStr("xyz"), "f")
  }

  test("Expression.Let.05") {
    val input = "def f(): Int = let x = 14 - 3; x + 2"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(13), "f")
  }

  test("Expression.Let.06") {
    val input =
      """def f(): Int =
        |  let x = 14 - 3;
        |  let y = 2 * 4;
        |    x + y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(19), "f")
  }

  test("Expression.Let.07") {
    val input =
      """def f(): Int =
        |  let x = 1;
        |  let y = x + 2;
        |  let z = y + 3;
        |    z
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(6), "f")
  }

  test("Expression.Let.08") {
    val input =
      """def f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337;
        |  let y = -101010;
        |  let z = 42;
        |    y
        |def g(): Int = f(-1337, 101010, -42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-101010), "g")
  }

  test("Expression.Let.09") {
    val input =
      """def f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337;
        |  let y = -101010;
        |  let z = 42;
        |    b
        |def g(): Int = f(-1337, 101010, -42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(101010), "g")
  }

  test("Expression.Let.10") {
    val input = "def f(): Int64 = let x = 0i64; x"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0), "f")
  }

  test("Expression.Let.11") {
    val input =
      """def f(): Int64 =
        |  let x = 1337i64;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "f")
  }

  test("Expression.Let.12") {
    val input =
      """def f(): Int64 =
        |  let x = 1337i64;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "f")
  }

  test("Expression.Let.13") {
    val input =
      """def f(a: Int64, b: Int64, c: Int64): Int64 =
        |  let x = 1337i64;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    y
        |def g(): Int64 = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "g")
  }

  test("Expression.Let.14") {
    val input =
      """def f(a: Int32, b: Int64, c: Int64): Int64 =
        |  let x = 1337i32;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    y
        |def g(): Int64 = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "g")
  }

  test("Expression.Let.15") {
    val input =
      """def f(a: Int64, b: Int64, c: Int64): Int64 =
        |  let x = 1337i64;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    b
        |def g(): Int64 = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(101010), "g")
  }

  test("Expression.Let.16") {
    val input =
      """def f(a: Int32, b: Int64, c: Int64): Int64 =
        |  let x = 1337i32;
        |  let y = -101010i64;
        |  let z = 42i64;
        |    b
        |def g(): Int64 = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(101010), "g")
  }

  test("Expression.Let.17") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(): ConstProp = let x = ConstProp.Val(42); x
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(42)), "f")
  }

  test("Expression.Let.18") {
    val input = "def f(): () = let x = (); x"
    val t = new Tester(input)
    t.runTest(Value.Unit, "f")
  }

  test("Expression.Let.19") {
    val input = """def f(): Str = let x = "helloworld"; x"""
    val t = new Tester(input)
    t.runTest(Value.mkStr("helloworld"), "f")
  }

  test("Expression.Let.20") {
    val input = "def f(): (Int, Int) = let x = (123, 456); x"
    val t = new Tester(input)
    t.runTest(Array(123, 456).map(Value.mkInt32), "f")
  }

  test("Expression.Let.22") {
    val input =
      """def f(): Char =
        |  let x = 'a';
        |  let y = 'b';
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkChar('b'), "f")
  }

  test("Expression.Let.23") {
    val input =
      """def f(): Float32 =
        |  let x = 1.2f32;
        |  let y = 3.4f32;
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(3.4f), "f")
  }

  test("Expression.Let.24") {
    val input =
      """def f(): Float64 =
        |  let x = 1.2f64;
        |  let y = 3.4f64;
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(3.4d), "f")
  }

  test("Expression.Let.25") {
    val input =
      """def f(x: Int): Int32 =
        |  let x = x + 1;
        |  let x = x + 2;
        |    x + 3
        |def g(): Int = f(0)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(6), "g")
  }

  test("Expression.Let.26") {
    val input =
      """def f(x: Int): Int64 =
        |  let x = x + 1;
        |  let x = 40i64;
        |  let x = x + 2i64;
        |    x
        |def g(): Int64 = f(0)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(42), "g")
  }

  test("Expression.Let.27") {
    val input =
      """def f(): BigInt =
        |  let x = 12345678901234567890ii;
        |  let y = 98765432109876543210ii;
        |    y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("98765432109876543210"), "f")
  }

  test("Expression.Let.28") {
    val input =
      """def f(): Int =
        |  let x = 42;
        |    x
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "f")
  }

  test("Expression.Let.29") {
    val input =
      """def f(): Int =
        |  let x = 42;
        |  let y = 21;
        |    x + y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42 + 21), "f")
  }

  test("Expression.Let.30") {
    val input =
      """def f(): Int =
        |  let x = 42;
        |  let y = 21;
        |  let z = 11;
        |    x + y + z
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42 + 21 + 11), "f")
  }

  test("Expression.Let.31") {
    val input =
      """def f(): Int =
        |  let x = {
        |    let a = 1;
        |    let b = 2;
        |      a + b
        |   };
        |   x
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1 + 2), "f")
  }

  test("Expression.Let.32") {
    val input =
      """def f(): Int =
        |  let x = {
        |    let a = 1;
        |    let b = 2;
        |      a + b
        |   };
        |   let y = {
        |     let c = 3;
        |     let d = 4;
        |       c + d
        |   };
        |   x + y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1 + 2 + 3 + 4), "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{CheckTag,GetTagValue}                                       //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Tag                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Tag.01") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(): ConstProp = ConstProp.Top
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Top", Value.Unit), "f")
  }

  test("Expression.Tag.02") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(): ConstProp = ConstProp.Val(42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(42)), "f")
  }

  test("Expression.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(): ConstProp = ConstProp.Bot
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Bot", Value.Unit), "f")
  }

  test("Expression.Tag.04") {
    val input =
      """enum Val { case Val(Bool) }
        |def f(): Val = Val.Val(true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.True), "f")
  }

  test("Expression.Tag.05") {
    val input =
      """enum Val { case Val(Bool) }
        |def f(x: Bool): Val = Val.Val(x)
        |def g01(): Val = f(true)
        |def g02(): Val = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.True), "g01")
    t.runTest(Value.mkTag("Val", Value.False), "g02")
  }

  test("Expression.Tag.06") {
    val input =
      """enum Val { case Val(Str) }
        |def f(): Val = Val.Val("hi")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkStr("hi")), "f")
  }

  test("Expression.Tag.07") {
    val input =
      """enum Val { case Val(Int, Str) }
        |def f(): Val = Val.Val(1, "one")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Array(Value.mkInt32(1), "one")), "f")
  }

  test("Expression.Tag.08") {
    val input =
      """enum Val { case Val(Str) }
        |def f(): Val = Val.Val(if (!(4 != 4)) "foo" else "bar")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkStr("foo")), "f")
  }

  test("Expression.Tag.09") {
    val input =
      """enum Val { case Val(Str, Int) }
        |def f(): Val = Val.Val("ABC", 20 + 22)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Array("ABC", Value.mkInt32(42))), "f")
  }

  test("Expression.Tag.10") {
    val input =
      """enum Val { case Val((Str, Int)) }
        |def f(): Val = Val.Val(("ABC", 20 + 22))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Array("ABC", Value.mkInt32(42))), "f")
  }

  test("Expression.Tag.11") {
    val input =
      """enum Val { case Val(Int8) }
        |def f(): Val = Val.Val(32i8)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt8(32)), "f")
  }

  test("Expression.Tag.12") {
    val input =
      """enum Val { case Val(Int16) }
        |def f(): Val = Val.Val(3200i16)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt16(3200)), "f")
  }

  test("Expression.Tag.13") {
    val input =
      """enum Val { case Val(Int32) }
        |def f(): Val = Val.Val(32000000i32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(32000000)), "f")
  }

  test("Expression.Tag.14") {
    val input =
      """enum Val { case Val(Int64) }
        |def f(): Val = Val.Val(320000000000i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt64(320000000000L)), "f")
  }

  test("Expression.Tag.15") {
    val input =
      """enum Val { case Val(Char) }
        |def f(): Val = Val.Val('a')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkChar('a')), "f")
  }

  test("Expression.Tag.16") {
    val input =
      """enum Val { case Val(Float32) }
        |def f(): Val = Val.Val(4.2f32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkFloat32(4.2f)), "f")
  }

  test("Expression.Tag.17") {
    val input =
      """enum Val { case Val(Float64) }
        |def f(): Val = Val.Val(4.2f64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkFloat64(4.2d)), "f")
  }

  test("Expression.Tag.18") {
    val input =
      """enum A { case AA(Int) }
        |enum B { case BB(A) }
        |def f(): B = B.BB(A.AA(42))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("BB", Value.mkTag("AA", Value.mkInt32(42))), "f")
  }

  // TODO: Requires backend support
  ignore("Expression.Tag.19") {
    val input =
      """enum Val { case Val(Set[Int]) }
        |def f(): Val = Val.Val(#{1, 2, 3})
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkSet(Set(1, 2, 3).map(Value.mkInt32))), "f")
  }

  test("Expression.Tag.20") {
    val input =
      """enum Val { case Val(BigInt) }
        |def f(): Val = Val.Val(12345678901234567890ii)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkBigInt("12345678901234567890")), "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Match,Switch}Error                                          //
  // Tested indirectly by switch expressions and pattern matching.           //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Switch expressions                                                      //
  // These don't exist in the ExecutableAst because they're desugared to     //
  // Expression.IfThenElse.                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Switch.01") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 1
        |  case !x => 0
        |}
        |def g01(): Int = f(true)
        |def g02(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g01")
    t.runTest(Value.mkInt32(0), "g02")
  }

  test("Switch.02") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 100
        |  case true => 20
        |}
        |def g01(): Int = f(true)
        |def g02(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(100), "g01")
    t.runTest(Value.mkInt32(20), "g02")
  }

  test("Switch.03") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 0
        |  case !x => 1
        |  case true => 2
        |}
        |def g01(): Int = f(true)
        |def g02(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
  }

  test("Switch.04") {
    val input =
      """def f(x: Int): Str = switch {
        |  case x < 0 => "negative"
        |  case x == 0 => "zero"
        |  case x == 1 => "one"
        |  case x == 2 => "two"
        |  case x >= 3 => "many"
        |}
        |def g01(): Str = f(-2)
        |def g02(): Str = f(-1)
        |def g03(): Str = f(0)
        |def g04(): Str = f(1)
        |def g05(): Str = f(2)
        |def g06(): Str = f(3)
        |def g07(): Str = f(4)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("negative"), "g01")
    t.runTest(Value.mkStr("negative"), "g02")
    t.runTest(Value.mkStr("zero"), "g03")
    t.runTest(Value.mkStr("one"), "g04")
    t.runTest(Value.mkStr("two"), "g05")
    t.runTest(Value.mkStr("many"), "g06")
    t.runTest(Value.mkStr("many"), "g07")
  }

  test("Switch.05") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 1
        |}
        |def g(): Int = f(true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g")
  }

  test("Switch.06") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 1
        |}
        |def g(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runInterceptTest[SwitchException]("g")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Match expressions (pattern matching)                                    //
  // These don't exist in the ExecutableAst because they're desugared into   //
  // primitives (e.g. CheckTag, GetTagValue, GetTupleIndex).                 //
  // Note that these are also good tests of the lambda implementation        //
  // (MkClosureRef, ApplyClosure, free variables).                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Match.Wildcard.01") {
    val input =
      """def f(): Int = match () with {
        |  case _ => 11
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(11), "f")
  }

  test("Match.Wildcard.02") {
    val input =
      """def f(): Int = match 42 with {
        |  case _ => 11
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(11), "f")
  }

  test("Match.Wildcard.03") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case _ => 11
        |}
        |def g01(): Int = f(-1)
        |def g02(): Int = f(0)
        |def g03(): Int = f(1)
        |def g04(): Int = f(99999)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(11), "g01")
    t.runTest(Value.mkInt32(11), "g02")
    t.runTest(Value.mkInt32(11), "g03")
    t.runTest(Value.mkInt32(11), "g04")
  }

  test("Match.Var.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case a => 1
        |}
        |def g(): Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g")
  }

  test("Match.Var.02") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case a => a
        |}
        |def g(): Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(3), "g")
  }

  test("Match.Var.03") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case a => a + 11
        |}
        |def g(): Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(14), "g")
  }

  test("Match.Literal.01") {
    val input =
      """def f(x: Unit): Bool = match x with {
        |  case () => true
        |}
        |def g(): Bool = f(())
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "g")
  }

  test("Match.Literal.02") {
    val input =
      """def f(x: Bool): Int = match x with {
        |  case true => 30
        |  case false => 81
        |}
        |def g01(): Int = f(true)
        |def g02(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(30), "g01")
    t.runTest(Value.mkInt32(81), "g02")
  }

  test("Match.Literal.03") {
    val input =
      """def f(x: Bool): Int = match x with {
        |  case true => 30
        |  case _ => 81
        |}
        |def g01(): Int = f(true)
        |def g02(): Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(30), "g01")
    t.runTest(Value.mkInt32(81), "g02")
  }

  test("Match.Literal.04") {
    val input =
      """def f(x: Int): Str = match x with {
        |  case -1 => "minus one"
        |  case 0 => "zero"
        |  case 1 => "one"
        |  case _ => "unknown"
        |}
        |def g01(): Str = f(-1)
        |def g02(): Str = f(0)
        |def g03(): Str = f(1)
        |def g04(): Str = f(2)
        |def g05(): Str = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("minus one"), "g01")
    t.runTest(Value.mkStr("zero"), "g02")
    t.runTest(Value.mkStr("one"), "g03")
    t.runTest(Value.mkStr("unknown"), "g04")
    t.runTest(Value.mkStr("unknown"), "g05")
  }

  test("Match.Literal.05") {
    val input =
      s"""def f(x: Int8): Str = match x with {
         |  case ${Byte.MinValue}i8 => "min"
         |  case -2i8 => "a"
         |  case 6i8 => "b"
         |  case ${Byte.MaxValue}i8 => "max"
         |  case _ => "unknown"
         |}
         |def g01(): Str = f(${Byte.MinValue}i8)
         |def g02(): Str = f(-2i8)
         |def g03(): Str = f(6i8)
         |def g04(): Str = f(${Byte.MaxValue}i8)
         |def g05(): Str = f(0i8)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("min"), "g01")
    t.runTest(Value.mkStr("a"), "g02")
    t.runTest(Value.mkStr("b"), "g03")
    t.runTest(Value.mkStr("max"), "g04")
    t.runTest(Value.mkStr("unknown"), "g05")
  }

  test("Match.Literal.06") {
    val input =
      s"""def f(x: Int16): Str = match x with {
         |  case ${Short.MinValue}i16 => "min"
         |  case -211i16 => "a"
         |  case 623i16 => "b"
         |  case ${Short.MaxValue}i16 => "max"
         |  case _ => "unknown"
         |}
         |def g01(): Str = f(${Short.MinValue}i16)
         |def g02(): Str = f(-211i16)
         |def g03(): Str = f(623i16)
         |def g04(): Str = f(${Short.MaxValue}i16)
         |def g05(): Str = f(0i16)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("min"), "g01")
    t.runTest(Value.mkStr("a"), "g02")
    t.runTest(Value.mkStr("b"), "g03")
    t.runTest(Value.mkStr("max"), "g04")
    t.runTest(Value.mkStr("unknown"), "g05")
  }

  test("Match.Literal.07") {
    val input =
      s"""def f(x: Int32): Str = match x with {
         |  case ${Int.MinValue}i32 => "min"
         |  case -2136541i32 => "a"
         |  case 6254523i32 => "b"
         |  case ${Int.MaxValue}i32 => "max"
         |  case _ => "unknown"
         |}
         |def g01(): Str = f(${Int.MinValue}i32)
         |def g02(): Str = f(-2136541i32)
         |def g03(): Str = f(6254523i32)
         |def g04(): Str = f(${Int.MaxValue}i32)
         |def g05(): Str = f(0i32)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("min"), "g01")
    t.runTest(Value.mkStr("a"), "g02")
    t.runTest(Value.mkStr("b"), "g03")
    t.runTest(Value.mkStr("max"), "g04")
    t.runTest(Value.mkStr("unknown"), "g05")
  }

  test("Match.Literal.08") {
    val input =
      s"""def f(x: Int64): Str = match x with {
         |  case ${Long.MinValue}i64 => "min"
         |  case -213645454545541i64 => "a"
         |  case 6287816254523i64 => "b"
         |  case ${Long.MaxValue}i64 => "max"
         |  case _ => "unknown"
         |}
         |def g01(): Str = f(${Long.MinValue}i64)
         |def g02(): Str = f(-213645454545541i64)
         |def g03(): Str = f(6287816254523i64)
         |def g04(): Str = f(${Long.MaxValue}i64)
         |def g05(): Str = f(0i64)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("min"), "g01")
    t.runTest(Value.mkStr("a"), "g02")
    t.runTest(Value.mkStr("b"), "g03")
    t.runTest(Value.mkStr("max"), "g04")
    t.runTest(Value.mkStr("unknown"), "g05")
  }

  test("Match.Literal.09") {
    val input =
      """def f(x: Str): Str = match x with {
        |  case "one" => "un"
        |  case "two" => "deux"
        |  case "three" => "trois"
        |  case _ => "???"
        |}
        |def g01(): Str = f("one")
        |def g02(): Str = f("two")
        |def g03(): Str = f("three")
        |def g04(): Str = f("four")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("un"), "g01")
    t.runTest(Value.mkStr("deux"), "g02")
    t.runTest(Value.mkStr("trois"), "g03")
    t.runTest(Value.mkStr("???"), "g04")
  }

  test("Match.Literal.10") {
    val input =
      """enum Foo { case Bar, case Baz, case Abc(Int,Str), case Xyz }
        |def f(x: Foo): Int = match x with {
        |  case Foo.Bar => 1
        |  case Foo.Baz => 2
        |  case Foo.Abc(42, "hi") => 3
        |  case _ => 0
        |}
        |def g01(): Int = f(Foo.Bar)
        |def g02(): Int = f(Foo.Baz)
        |def g03(): Int = f(Foo.Abc(42, "hi"))
        |def g04(): Int = f(Foo.Abc(42, "hi!"))
        |def g05(): Int = f(Foo.Abc(41, "hi"))
        |def g06(): Int = f(Foo.Abc(40, "a"))
        |def g07(): Int = f(Foo.Xyz)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g01")
    t.runTest(Value.mkInt32(2), "g02")
    t.runTest(Value.mkInt32(3), "g03")
    t.runTest(Value.mkInt32(0), "g04")
    t.runTest(Value.mkInt32(0), "g05")
    t.runTest(Value.mkInt32(0), "g06")
    t.runTest(Value.mkInt32(0), "g07")
  }

  test("Match.Literal.11") {
    val input =
      """def f(x: Str, y: Bool): Int = match (x, y) with {
        |  case ("hi", false) => 1
        |  case _ => 2
        |}
        |def g01(): Int = f("hi", true)
        |def g02(): Int = f("hi", false)
        |def g03(): Int = f("abc", true)
        |def g04(): Int = f("abc", false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(2), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(2), "g04")
  }

  test("Match.Literal.12") {
    val input =
      """def f(x: (Int, (Int, Int))): Int = match x with {
        |  case (4, (12, 8)) => 1
        |  case (4, (12, 0)) => 2
        |  case (1, (12, 8)) => 3
        |  case _ => 4
        |}
        |def g01(): Int = f((4, (12, 8)))
        |def g02(): Int = f((4, (12, 0)))
        |def g03(): Int = f((1, (12, 8)))
        |def g04(): Int = f((1, (12, 0)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g01")
    t.runTest(Value.mkInt32(2), "g02")
    t.runTest(Value.mkInt32(3), "g03")
    t.runTest(Value.mkInt32(4), "g04")
  }

  test("Match.Literal.13") {
    val input =
      """def f(x: Int, y: Int): Int = match x with {
        |  case 0 => y
        |  case _ =>  match y with {
        |    case 0 => x
        |    case _ => 0
        |  }
        |}
        |def g01(): Int = f(0, 0)
        |def g02(): Int = f(1, 0)
        |def g03(): Int = f(0, 2)
        |def g04(): Int = f(3, 4)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(0), "g04")
  }

  test("Match.Literal.14") {
    val input =
      s"""def f(x: BigInt): Str = match x with {
         |  case -9223372036854775809ii => "-"
         |  case 9223372036854775809ii => "+"
         |  case _ => "unknown"
         |}
         |def g01(): Str = f(-9223372036854775809ii)
         |def g02(): Str = f(9223372036854775809ii)
         |def g03(): Str = f(6287816254523ii)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("-"), "g01")
    t.runTest(Value.mkStr("+"), "g02")
    t.runTest(Value.mkStr("unknown"), "g03")
  }

  test("Match.Tag.01") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |def f(x: NameAndAge): Int =
        |  let NameAndAge.T(_, age) = x;
        |    age
        |def g01(): Int = f(NameAndAge.T("James", 42))
        |def g02(): Int = f(NameAndAge.T("John", 21))
        |def g03(): Int = f(NameAndAge.T("James", 5))
        |def g04(): Int = f(NameAndAge.T("Mary", 33))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "g01")
    t.runTest(Value.mkInt32(21), "g02")
    t.runTest(Value.mkInt32(5), "g03")
    t.runTest(Value.mkInt32(33), "g04")
  }

  test("Match.Tag.02") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |def f(x: NameAndAge): Int = match x with {
        |  case NameAndAge.T("James", age) => age
        |  case _ => -1
        |}
        |def g01(): Int = f(NameAndAge.T("James", 42))
        |def g02(): Int = f(NameAndAge.T("John", 21))
        |def g03(): Int = f(NameAndAge.T("James", 5))
        |def g04(): Int = f(NameAndAge.T("Mary", 33))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "g01")
    t.runTest(Value.mkInt32(-1), "g02")
    t.runTest(Value.mkInt32(5), "g03")
    t.runTest(Value.mkInt32(-1), "g04")
  }

  test("Match.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(x: ConstProp): Int = match x with {
        |  case ConstProp.Top => -1
        |  case ConstProp.Val(v) => v
        |  case ConstProp.Bot => -2
        |}
        |def g01(): Int = f(ConstProp.Top)
        |def g02(): Int = f(ConstProp.Val(42))
        |def g03(): Int = f(ConstProp.Val(-24))
        |def g04(): Int = f(ConstProp.Bot)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-1), "g01")
    t.runTest(Value.mkInt32(42), "g02")
    t.runTest(Value.mkInt32(-24), "g03")
    t.runTest(Value.mkInt32(-2), "g04")
  }

  test("Match.Tag.04") {
    val input =
      """enum BoolTag { case Top, case B(Bool), case Bot }
        |def f(x: BoolTag): Int = match x with {
        |  case BoolTag.Top => 0
        |  case BoolTag.B(b) => if (b) 1 else -1
        |  case BoolTag.Bot => 0
        |}
        |def g01(): Int = f(BoolTag.Top)
        |def g02(): Int = f(BoolTag.B(true))
        |def g03(): Int = f(BoolTag.B(false))
        |def g04(): Int = f(BoolTag.Bot)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(-1), "g03")
    t.runTest(Value.mkInt32(0), "g04")
  }

  test("Match.Tag.05") {
    val input =
      """enum Val { case Nip, case Val((Str, Int)) }
        |def f(x: Val): Int = match x with {
        |  case Val.Nip => 0
        |  case Val.Val(v) => match v with {
        |    case ("x", y) => -1
        |    case (_, y) => y
        |  }
        |}
        |def g01(): Int = f(Val.Nip)
        |def g02(): Int = f(Val.Val(("a", 1)))
        |def g03(): Int = f(Val.Val(("b", 2)))
        |def g04(): Int = f(Val.Val(("x", 3)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(-1), "g04")
  }

  test("Match.Tag.06") {
    val input =
      """enum Val { case Nip, case Val((Str, Int)) }
        |def f(x: Val): Int = match x with {
        |  case Val.Nip => 0
        |  case Val.Val(v) => match v with {
        |    case (x, y) => match x with {
        |      case "x" => -1
        |      case _ => match y with {
        |        case z => z
        |      }
        |    }
        |  }
        |}
        |def g01(): Int = f(Val.Nip)
        |def g02(): Int = f(Val.Val(("a", 1)))
        |def g03(): Int = f(Val.Val(("b", 2)))
        |def g04(): Int = f(Val.Val(("x", 3)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(-1), "g04")
  }

  test("Match.Tag.07") {
    val input =
      """enum A { case AA(Int), case AB(Int) }
        |enum B { case Top, case BB(A), case Bot }
        |def f(x: B): Str = match x with {
        |  case B.Top => "top"
        |  case B.BB(y) => match y with {
        |    case A.AA(a) => match a with {
        |      case 0 => "a0"
        |      case _ => "aaa"
        |    }
        |    case A.AB(b) => match b with {
        |      case 0 => "b0"
        |      case z => "bbb"
        |    }
        |  }
        |  case B.Bot => "bot"
        |}
        |def g01(): Str = f(B.Top)
        |def g02(): Str = f(B.Bot)
        |def g03(): Str = f(B.BB(A.AA(0)))
        |def g04(): Str = f(B.BB(A.AA(1)))
        |def g05(): Str = f(B.BB(A.AB(0)))
        |def g06(): Str = f(B.BB(A.AB(-1)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("top"), "g01")
    t.runTest(Value.mkStr("bot"), "g02")
    t.runTest(Value.mkStr("a0"), "g03")
    t.runTest(Value.mkStr("aaa"), "g04")
    t.runTest(Value.mkStr("b0"), "g05")
    t.runTest(Value.mkStr("bbb"), "g06")
  }

  // TODO: Requires backend support
  ignore("Match.Tag.08") {
    val input =
      """enum Val { case Nip, case Val(Set[Int]) }
        |def f(x: Val): Set[Int] = match x with {
        |  case Val.Nip => #{0}
        |  case Val.Val(s) => s
        |}
        |def g01(): Set[Int] = f(Val.Nip)
        |def g02(): Set[Int] = f(Val.Val(#{1, 2, 3}))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkInt32(0))), "g01")
    t.runTest(Value.mkSet(Set(1, 2, 3).map(Value.mkInt32)), "g02")
  }

  test("Match.Tag.09") {
    val input =
      """enum Val { case Val(Int8) }
        |def f(): Int8 = match Val.Val(32i8) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(32), "f")
  }

  test("Match.Tag.10") {
    val input =
      """enum Val { case Val(Int16) }
        |def f(): Int16 = match Val.Val(3200i16) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(3200), "f")
  }

  test("Match.Tag.11") {
    val input =
      """enum Val { case Val(Int32) }
        |def f(): Int32 = match Val.Val(32000000i32) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(32000000), "f")
  }

  test("Match.Tag.12") {
    val input =
      """enum Val { case Val(Int64) }
        |def f(): Int64 = match Val.Val(320000000000i64) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(320000000000L), "f")
  }

  test("Match.Tag.13") {
    val input =
      """enum Val { case Val(Char) }
        |def f(): Char = match Val.Val('a') with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkChar('a'), "f")
  }

  test("Match.Tag.14") {
    val input =
      """enum Val { case Val(Float32) }
        |def f(): Float32 = match Val.Val(4.2f32) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(4.2f), "f")
  }

  test("Match.Tag.15") {
    val input =
      """enum Val { case Val(Float64) }
        |def f(): Float64 = match Val.Val(4.2f64) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(4.2d), "f")
  }

  test("Match.Tag.16") {
    val input =
      """enum Val { case Val(BigInt) }
        |def f(): BigInt = match Val.Val(100000000000000000000ii) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("100000000000000000000"), "f")
  }

  test("Match.Tuple.01") {
    val input =
      """def f(x: Int, y: Int): Int =
        |  let (a, b) = (x, y);
        |    a + b
        |def g01(): Int = f(5, 6)
        |def g02(): Int = f(6, 5)
        |def g03(): Int = f(100, 23)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(11), "g01")
    t.runTest(Value.mkInt32(11), "g02")
    t.runTest(Value.mkInt32(123), "g03")
  }

  test("Match.Tuple.02") {
    val input =
      """def f(x: Int, y: Bool): Str = match (x, y) with {
        |  case (5, true) => "abc"
        |  case (5, _) => "def"
        |  case (_, true) => "ghi"
        |  case (_, _) => "jkl"
        |}
        |def g01(): Str = f(5, true)
        |def g02(): Str = f(5, false)
        |def g03(): Str = f(6, true)
        |def g04(): Str = f(0, false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("abc"), "g01")
    t.runTest(Value.mkStr("def"), "g02")
    t.runTest(Value.mkStr("ghi"), "g03")
    t.runTest(Value.mkStr("jkl"), "g04")
  }

  test("Match.Tuple.03") {
    val input =
      """def f(x: BigInt, y: Int, z: Int): Int = match (x, (y, z)) with {
        |  case (1ii, (2, 3)) => -1
        |  case (1ii, (2, _)) => -2
        |  case (1ii, (_, 3)) => -3
        |  case (1ii, _) => -4
        |  case (_, (a, b)) => a + b
        |}
        |def g01(): Int = f(1ii, 2, 3)
        |def g02(): Int = f(1ii, 2, 4)
        |def g03(): Int = f(1ii, 3, 3)
        |def g04(): Int = f(1ii, 5, 5)
        |def g05(): Int = f(2ii, 2, 3)
        |def g06(): Int = f(2ii, 10, 20)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-1), "g01")
    t.runTest(Value.mkInt32(-2), "g02")
    t.runTest(Value.mkInt32(-3), "g03")
    t.runTest(Value.mkInt32(-4), "g04")
    t.runTest(Value.mkInt32(5), "g05")
    t.runTest(Value.mkInt32(30), "g06")
  }

  test("Match.Tuple.04") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f(x: ConstProp, y: ConstProp): Int = match (x, y) with {
        |  case (ConstProp.Top, ConstProp.Top) => 1
        |  case (ConstProp.Bot, ConstProp.Bot) => 2
        |  case (ConstProp.Val(v1), ConstProp.Val(v2)) => if (v1 == v2) 3 else 4
        |  case _ => 5
        |}
        |def g01(): Int = f(ConstProp.Top, ConstProp.Top)
        |def g02(): Int = f(ConstProp.Bot, ConstProp.Bot)
        |def g03(): Int = f(ConstProp.Val(42), ConstProp.Val(42))
        |def g04(): Int = f(ConstProp.Val(42), ConstProp.Val(0))
        |def g05(): Int = f(ConstProp.Val(0), ConstProp.Val(42))
        |def g06(): Int = f(ConstProp.Top, ConstProp.Bot)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g01")
    t.runTest(Value.mkInt32(2), "g02")
    t.runTest(Value.mkInt32(3), "g03")
    t.runTest(Value.mkInt32(4), "g04")
    t.runTest(Value.mkInt32(4), "g05")
    t.runTest(Value.mkInt32(5), "g06")
  }

  test("Match.Tuple.05") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |def f(x: Int, y: NameAndAge): Int = match (x, y) with {
        |  case (1, NameAndAge.T("James", _)) => 1
        |  case (a, NameAndAge.T("James", b)) => a + b
        |  case (_, NameAndAge.T(_, 24)) => 2
        |  case _ => -1
        |}
        |def g01(): Int = f(1, NameAndAge.T("James", 20))
        |def g02(): Int = f(1, NameAndAge.T("John", 53))
        |def g03(): Int = f(2, NameAndAge.T("James", 20))
        |def g04(): Int = f(2, NameAndAge.T("John", 53))
        |def g05(): Int = f(3, NameAndAge.T("Mary", 24))
        |def g06(): Int = f(3, NameAndAge.T("Anne", 18))
        |def g07(): Int = f(4, NameAndAge.T("Charles", 64))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g01")
    t.runTest(Value.mkInt32(-1), "g02")
    t.runTest(Value.mkInt32(22), "g03")
    t.runTest(Value.mkInt32(-1), "g04")
    t.runTest(Value.mkInt32(2), "g05")
    t.runTest(Value.mkInt32(-1), "g06")
    t.runTest(Value.mkInt32(-1), "g07")
  }

  test("Match.Tuple.06") {
    val input =
      """def f(x: Int, y: Int): Int = match (x, y) with {
        |  case (x, y) => match x with {
        |    case 0 => y
        |    case _ => match y with {
        |      case 0 => x
        |      case _ => 0
        |    }
        |  }
        |}
        |def g01(): Int = f(0, 0)
        |def g02(): Int = f(1, 0)
        |def g03(): Int = f(0, 2)
        |def g04(): Int = f(3, 4)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(0), "g04")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Var                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Var.01") {
    val input =
      """rel A(x: Bool)
        |rel B(x: Bool)
        |
        |A(true).
        |
        |B(x) :- A(x).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.True)), "B")
  }

  test("Term.Head.Var.02") {
    val input =
      """rel A(x: Int)
        |rel B(x: Int)
        |
        |A(1).
        |A(2).
        |A(3).
        |
        |B(x) :- A(x).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "B")
  }

  test("Term.Head.Var.03") {
    val input =
      """rel A(x: Str)
        |rel B(x: Str)
        |
        |A("one").
        |A("two").
        |A("three").
        |
        |B(x) :- A(x).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "B")
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
    t.checkModel(Set(List(Value.Unit)), "A")
  }

  test("Term.Head.Exp.02") {
    val input =
      """rel A(x: Bool)
        |
        |A(true).
        |A(false).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(true, false).map(x => List(Value.mkBool(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt8(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt16(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt64(x))), "A")
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
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  ignore("Term.Head.Exp.08") { // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Str))
        |
        |A((1, "one")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Array(Value.mkInt32(1), Value.mkStr("one")))), "A")
  }

  test("Term.Head.Exp.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo)
        |
        |A(Foo.Foo(1, "one")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.mkTag("Foo",Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  ignore("Term.Head.Exp.10") {  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Int))
        |
        |A((1, 2)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Array(1, 2).map(Value.mkInt32))), "A")
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
    t.checkModel(Set('a', 'b', 'c').map(x => List(Value.mkChar(x))), "A")
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
    t.checkModel(Set(1.0f, 2.0f, 3.0f).map(x => List(Value.mkFloat32(x))), "A")
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
    t.checkModel(Set(1.0d, 2.0d, 3.0d).map(x => List(Value.mkFloat64(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkBigInt(x))), "A")
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
    t.checkModel(Set(List(Value.Unit)), "A")
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
    t.checkModel(Set(true, false).map(x => List(Value.mkBool(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt8(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt16(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt64(x))), "A")
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
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  ignore("Term.Head.Apply.08") { //  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Str))
        |def f(x: Int): (Int, Str) = (x, "one")
        |
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Array(Value.mkInt32(1), Value.mkStr("one")))), "A")
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
    t.checkModel(Set(List(Value.mkTag("Foo",Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  ignore("Term.Head.Apply.10") { //  // TODO: Require special equality on sets.
    val input =
      """rel A(x: (Int, Int))
        |def f(x: Int, y: Int): (Int, Int) = (x, y)
        |
        |A(f(1, 2)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Array(1, 2).map(Value.mkInt32))), "A")
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
    t.checkModel(Set('a', 'b', 'c').map(x => List(Value.mkChar(x))), "A")
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
    t.checkModel(Set(1.0f, 2.0f, 3.0f).map(x => List(Value.mkFloat32(x))), "A")
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
    t.checkModel(Set(1.0d, 2.0d, 3.0d).map(x => List(Value.mkFloat64(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkBigInt(x))), "A")
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
    t.checkModel(Set(List(Value.True)), "B")
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
    t.checkModel(Set(0, 2).map(x => List(Value.mkInt32(x))), "B")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
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
    t.checkModel(Set(("b", "b"), ("p", "c"), ("d", "b")).map { case (x,y) => List(Value.mkStr(x), Value.mkStr(y)) }, "Pt")
  }

}
