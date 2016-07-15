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

class TestBackend extends FunSuite {

  private object HookSafeHelpers {
    case class MyObject(x: Int)

    implicit def f0h(f: Function0[IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f()
    }
    implicit def f1h(f: Function1[IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0))
    }
    implicit def f2h(f: Function2[IValue,IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0), args(1))
    }
    implicit def f3h(f: Function3[IValue,IValue,IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0), args(1), args(2))
    }
    implicit def f4h(f: Function4[IValue,IValue,IValue,IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0), args(1), args(2), args(3))
    }
    implicit def f5h(f: Function5[IValue,IValue,IValue,IValue,IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0), args(1), args(2), args(3), args(4))
    }
    implicit def f6h(f: Function6[IValue,IValue,IValue,IValue,IValue,IValue,IValue]): Invokable = new Invokable {
      override def apply(args: Array[IValue]): IValue = f(args(0), args(1), args(2), args(3), args(4), args(5))
    }
  }

  private object HookUnsafeHelpers {
    type JBool = java.lang.Boolean
    type JChar = java.lang.Character
    type JFloat = java.lang.Float
    type JDouble = java.lang.Double
    type JByte = java.lang.Byte
    type JShort = java.lang.Short
    type JInt = java.lang.Integer
    type JLong = java.lang.Long
    type JBigInt = java.math.BigInteger

    case class MyObject(x: Int)

    implicit def f0h[R <: AnyRef](f: Function0[R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
      )
    }
    implicit def f1h[P0,R <: AnyRef](f: Function1[P0,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0]
      )
    }
    implicit def f2h[P0,P1,R <: AnyRef](f: Function2[P0,P1,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0],
        args(1).asInstanceOf[P1]
      )
    }
    implicit def f3h[P0,P1,P2,R <: AnyRef](f: Function3[P0,P1,P2,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0],
        args(1).asInstanceOf[P1],
        args(2).asInstanceOf[P2]
      )
    }
    implicit def f4h[P0,P1,P2,P3,R <: AnyRef](f: Function4[P0,P1,P2,P3,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0],
        args(1).asInstanceOf[P1],
        args(2).asInstanceOf[P2],
        args(3).asInstanceOf[P3]
      )
    }
    implicit def f5h[P0,P1,P2,P3,P4,R <: AnyRef](f: Function5[P0,P1,P2,P3,P4,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0],
        args(1).asInstanceOf[P1],
        args(2).asInstanceOf[P2],
        args(3).asInstanceOf[P3],
        args(4).asInstanceOf[P4]
      )
    }
    implicit def f6h[P0,P1,P2,P3,P4,P5,R <: AnyRef](f: Function6[P0,P1,P2,P3,P4,P5,R]): InvokableUnsafe = new InvokableUnsafe {
      override def apply(args: Array[AnyRef]): AnyRef = f(
        args(0).asInstanceOf[P0],
        args(1).asInstanceOf[P1],
        args(2).asInstanceOf[P2],
        args(3).asInstanceOf[P3],
        args(4).asInstanceOf[P4],
        args(5).asInstanceOf[P5]
      )
    }
  }

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

    def addHook(name: String, tpe: IType, inv: Invokable): Tester = {
      interpretedFlix.addHook(name, tpe, inv)
      compiledFlix.addHook(name, tpe, inv)
      flix.addHook(name, tpe, inv)
      this
    }

    def addHook(name: String, tpe: IType, inv: InvokableUnsafe): Tester = {
      interpretedFlix.addHookUnsafe(name, tpe, inv)
      compiledFlix.addHookUnsafe(name, tpe, inv)
      flix.addHookUnsafe(name, tpe, inv)
      this
    }

    def run(): Tester = {
      interpreted = interpretedFlix.solve().get
      compiled = compiledFlix.solve().get
      this
    }

    def runTest(expected: AnyRef, const: String): Unit = {
      withClue(s"interpreted value $const:") { assertResult(expected)(interpreted.getConstant(const)) }
      withClue(s"compiled value $const:") { assertResult(expected)(compiled.getConstant(const)) }
    }

    def runInterceptTest[T <: AnyRef](const:String)(implicit manifest: Manifest[T]): Unit = {
      withClue(s"interpreted value $const:") { intercept[T](interpreted.getConstant(const)) }
      withClue(s"compiled value $const:") { intercept[T](compiled.getConstant(const)) }
    }

    def checkModel(expected: AnyRef, model: String): Unit = {
      withClue(s"interpreted model $model:") { assertResult(expected)(interpreted.getRelation(model).toSet) }
      withClue(s"compiled model $model:") { assertResult(expected)(compiled.getRelation(model).toSet) }
    }

    // By default, solve the Flix program immediately.
    // But in some cases we want to defer solving, so we can add hooks to native functions.
    if (solve) run()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Unit,Bool,Char,Float32,Float64,Int8,Int16,Int32,Int64,Str}  //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unit") {
    val input = "def f: () = ()"
    val t = new Tester(input)
    t.runTest(Value.Unit, "f")
  }

  test("Expression.Bool.01") {
    val input = "def f: Bool = true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Bool.02") {
    val input = "def f: Bool = false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Char.01") {
    val input = "def f: Char = 'a'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('a'), "f")
  }

  test("Expression.Char.02") {
    val input = "def f: Char = '0'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('0'), "f")
  }

  test("Expression.Char.03") {
    // Minimum character value (NUL)
    val input = s"def f: Char = '${'\u0000'}'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('\u0000'), "f")
  }

  test("Expression.Char.04") {
    // Non-printable ASCII character DEL
    val input = s"def f: Char = '${'\u007f'}'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('\u007f'), "f")
  }

  test("Expression.Char.05") {
    // Maximum character value
    val input = s"def f: Char = '${'\uffff'}'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('\uffff'), "f")
  }

  test("Expression.Char.06") {
    // Chinese character for the number "ten"
    val input = s"def f: Char = '${'十'}'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('十'), "f")
  }

  test("Expression.Char.07") {
    // Zero-width space
    val input = s"def f: Char = '${'\u200b'}'"
    val t = new Tester(input)
    t.runTest(Value.mkChar('\u200b'), "f")
  }

  // TODO: More tests when we get the syntax for exponents. More tests when we have standard library (NaN, +/infinity).
  // See JLS 3.10.2:
  //   The largest positive finite literal of type float is 3.4028235e38f.
  //   The smallest positive finite non-zero literal of type float is 1.40e-45f.
  //   The largest positive finite literal of type double is 1.7976931348623157e308.
  //   The smallest positive finite non-zero literal of type double is 4.9e-324.

  test("Expression.Float.01") {
    val input = "def f: Float = 0.0"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.0), "f")
  }

  test("Expression.Float.02") {
    val input = "def f: Float = -0.0"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.0), "f")
  }

  test("Expression.Float.03") {
    val input = "def f: Float = 4.2"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(4.2), "f")
  }

  test("Expression.Float.04") {
    val input = "def f: Float = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f")
  }

  test("Expression.Float.05") {
    val input = "def f: Float = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f")
  }

  test("Expression.Float.06") {
    val input = "def f: Float = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f")
  }

  test("Expression.Float.07") {
    val input = "def f: Float = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f")
  }

  /*
   * Note that there are specific bytecode instructions for constants 0.0f, 1.0f, and 2.0f.
   */

  test("Expression.Float32.01") {
    val input = "def f: Float32 = 0.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(0.0f), "f")
  }

  test("Expression.Float32.02") {
    val input = "def f: Float32 = -0.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(-0.0f), "f")
  }

  test("Expression.Float32.03") {
    val input = "def f: Float32 = 1.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(1.0f), "f")
  }

  test("Expression.Float32.04") {
    val input = "def f: Float32 = 2.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(2.0f), "f")
  }

  test("Expression.Float32.05") {
    val input = "def f: Float32 = 4.2f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(4.2f), "f")
  }

  test("Expression.Float32.06") {
    val input = "def f: Float32 = 999999999999999999999999999999.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(999999999999999999999999999999.0f), "f")
  }

  test("Expression.Float32.07") {
    val input = "def f: Float32 = 0.0000000000000000000000000000001f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(0.0000000000000000000000000000001f), "f")
  }

  test("Expression.Float32.08") {
    val input = "def f: Float32 = -999999999999999999999999999999.0f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(-999999999999999999999999999999.0f), "f")
  }

  test("Expression.Float32.09") {
    val input = "def f: Float32 = -0.0000000000000000000000000000001f32"
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(-0.0000000000000000000000000000001f), "f")
  }

  /*
   * Note that there are specific bytecode instructions for constants 0.0d and 1.0d.
   */

  test("Expression.Float64.01") {
    val input = "def f: Float64 = 0.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.0d), "f")
  }

  test("Expression.Float64.02") {
    val input = "def f: Float64 = -0.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.0d), "f")
  }

  test("Expression.Float64.03") {
    val input = "def f: Float64 = 1.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(1.0d), "f")
  }

  test("Expression.Float64.04") {
    val input = "def f: Float64 = 2.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(2.0d), "f")
  }

  test("Expression.Float64.05") {
    val input = "def f: Float64 = 4.2f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(4.2d), "f")
  }

  test("Expression.Float64.06") {
    val input = "def f: Float64 = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f")
  }

  test("Expression.Float64.07") {
    val input = "def f: Float64 = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f")
  }

  test("Expression.Float64.08") {
    val input = "def f: Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f")
  }

  test("Expression.Float64.09") {
    val input = "def f: Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64"
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f")
  }

  /*
   * Note that there are specific bytecode instructions for the constants -1 to 5, inclusive.
   */

  test("Expression.Int.01") {
    val input = "def f: Int = 0"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f")
  }

  test("Expression.Int.02") {
    val input = "def f: Int = -1"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-1), "f")
  }

  test("Expression.Int.03") {
    val input = "def f: Int = 1"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "f")
  }

  test("Expression.Int.04") {
    val input = "def f: Int = 5"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(5), "f")
  }

  test("Expression.Int.05") {
    val input = "def f: Int = -254542"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-254542), "f")
  }

  test("Expression.Int.06") {
    val input = "def f: Int = 45649878"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(45649878), "f")
  }

  test("Expression.Int.07") {
    val input = s"def f: Int = ${Int.MaxValue}"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f")
  }

  test("Expression.Int.08") {
    val input = s"def f: Int = ${Int.MinValue}"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MinValue), "f")
  }

  /*
   * Note that there is a specific bytecode instruction (BIPUSH) for pushing bytes
   * (that aren't handled by the -1 to 5 constant instructions).
   */

  test("Expression.Int8.01") {
    val input = "def f: Int8 = -105i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(-105), "f")
  }

  test("Expression.Int8.02") {
    val input = "def f: Int8 = 121i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(121), "f")
  }

  test("Expression.Int8.03") {
    val input = "def f: Int8 = -2i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(-2), "f")
  }

  test("Expression.Int8.04") {
    val input = "def f: Int8 = 6i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(6), "f")
  }

  test("Expression.Int8.05") {
    val input = s"def f: Int8 = ${Byte.MaxValue}i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(Byte.MaxValue), "f")
  }

  test("Expression.Int8.06") {
    val input = s"def f: Int8 = ${Byte.MinValue}i8"
    val t = new Tester(input)
    t.runTest(Value.mkInt8(Byte.MinValue), "f")
  }

  /*
   * Note that there is a specific bytecode instruction (SIPUSH) for pushing shorts (that aren't handled by BIPUSH).
   */

  test("Expression.Int16.01") {
    val input = "def f: Int16 = -5320i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(-5320), "f")
  }

  test("Expression.Int16.02") {
    val input = "def f: Int16 = 4568i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(4568), "f")
  }

  test("Expression.Int16.03") {
    val input = s"def f: Int16 = ${Byte.MinValue - 1}i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Byte.MinValue - 1), "f")
  }

  test("Expression.Int16.04") {
    val input = s"def f: Int16 = ${Byte.MaxValue + 1}i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Byte.MaxValue + 1), "f")
  }

  test("Expression.Int16.05") {
    val input = s"def f: Int16 = ${Short.MaxValue}i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Short.MaxValue), "f")
  }

  test("Expression.Int16.06") {
    val input = s"def f: Int16 = ${Short.MinValue}i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Short.MinValue), "f")
  }

  /*
   * Larger int constants need to be loaded with LDC.
   */

  test("Expression.Int32.01") {
    val input = "def f: Int32 = -254542i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-254542), "f")
  }

  test("Expression.Int32.02") {
    val input = "def f: Int32 = 45649878i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(45649878), "f")
  }

  test("Expression.Int32.03") {
    val input = s"def f: Int32 = ${Short.MinValue - 1}i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Short.MinValue - 1), "f")
  }

  test("Expression.Int32.04") {
    val input = s"def f: Int32 = ${Short.MaxValue + 1}i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Short.MaxValue + 1), "f")
  }

  test("Expression.Int32.05") {
    val input = s"def f: Int32 = ${Int.MaxValue}i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f")
  }

  test("Expression.Int32.06") {
    val input = s"def f: Int32 = ${Int.MinValue}i32"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MinValue), "f")
  }

  /*
   * Note that there are specific bytecode instructions for the constants 0l and 1l.
   */

  test("Expression.Int64.01") {
    val input = "def f: Int64 = -254454121542i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-254454121542L), "f")
  }

  test("Expression.Int64.02") {
    val input = "def f: Int64 = 45641198784545i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(45641198784545L), "f")
  }

  test("Expression.Int64.03") {
    val input = s"def f: Int64 = ${Int.MinValue - 1}i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Int.MinValue - 1), "f")
  }

  test("Expression.Int64.04") {
    val input = s"def f: Int64 = ${Int.MaxValue + 1}i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Int.MaxValue + 1), "f")
  }

  test("Expression.Int64.05") {
    val input = s"def f: Int64 = ${Long.MaxValue}i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Long.MaxValue), "f")
  }

  test("Expression.Int64.06") {
    val input = s"def f: Int64 = ${Long.MinValue}i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Long.MinValue), "f")
  }

  test("Expression.Int64.07") {
    val input = "def f: Int64 = 0i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0L), "f")
  }

  test("Expression.Int64.08") {
    val input = "def f: Int64 = 1i64"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(1L), "f")
  }

  test("Expression.BigInt.01") {
    val input = "def f: BigInt = 0ii"
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(0), "f")
  }

  test("Expression.BigInt.02") {
    val input = "def f: BigInt = 1ii"
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(1), "f")
  }

  test("Expression.BigInt.03") {
    val input = "def f: BigInt = 10ii"
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(10), "f")
  }

  test("Expression.BigInt.04") {
    // One larger than Long.MaxValue
    val input = "def f: BigInt = 9223372036854775808ii"
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("9223372036854775808"), "f")
  }

  test("Expression.BigInt.05") {
    // One smaller than Long.MinValue
    val input = "def f: BigInt = -9223372036854775809ii"
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("-9223372036854775809"), "f")
  }

  test("Expression.Str.01") {
    val input = """def f: Str = """""
    val t = new Tester(input)
    t.runTest(Value.mkStr(""), "f")
  }

  test("Expression.Str.02") {
    val input = """def f: Str = "Hello World!""""
    val t = new Tester(input)
    t.runTest(Value.mkStr("Hello World!"), "f")
  }

  test("Expression.Str.03") {
    val input = """def f: Str = "asdf""""
    val t = new Tester(input)
    t.runTest(Value.mkStr("asdf"), "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // LoadExpression and StoreExpression                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: LoadExpression and StoreExpression tests.
  // {Load,Store}Expressions are generated, and not explicitly written in a Flix program

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Var                                                          //
  // Tested indirectly by Expression.{Lambda,Let}.                           //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Ref                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Ref.01") {
    val input =
      """namespace Foo.Bar {
        |  def x: Bool = false
        |  def f: Str = "foo"
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("foo"), "Foo.Bar/f")
  }

  test("Expression.Ref.02") {
    val input =
      """namespace Foo {
        |  def f: Int = 5
        |  def g: Int = f()
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(5), "Foo/g")
  }

  test("Expression.Ref.03") {
    val input =
      """namespace Foo {
        |  def x: Bool = true
        |  def y: Bool = false
        |  def f: Bool = y()
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "Foo/f")
  }

  test("Expression.Ref.04") {
    val input =
      """namespace Foo {
        |  def x: Str = "hello"
        |}
        |namespace Bar {
        |  def x: Str = Foo/x()
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("hello"), "Bar/x")
  }

  test("Expression.Ref.05") {
    val input = "def x: Int = 42"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "x")
  }

  test("Expression.Ref.06") {
    val input =
      """namespace A.B {
        |  def a: Bool = false
        |}
        |namespace A {
        |  def b: Bool = !A.B/a()
        |}
        |namespace A {
        |  namespace B {
        |    def c: Int = 0
        |
        |    namespace C {
        |      def d: Int = 42
        |    }
        |  }
        |}
        |def e: Int = -1
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "A.B/a")
    t.runTest(Value.True, "A/b")
    t.runTest(Value.mkInt32(0), "A.B/c")
    t.runTest(Value.mkInt32(42), "A.B.C/d")
    t.runTest(Value.mkInt32(-1), "e")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lambdas - Expression.{MkClosureRef,ApplyRef,ApplyClosure}               //
  // Note that closure conversion and lambda lifting means we don't actually //
  // have lambdas in the AST. A lot of functionality is tested indirectly    //
  // by pattern matching.                                                    //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: More tests when the typer handles lambda expressions.
  // Test actual lambda expressions (not just top-level definitions): passing them around, free variables, etc.
  // Note: this also applies to comparison (?) of lambdas and using lambdas in let-expressions.

  test("Expression.Lambda.01") {
    val input =
      """namespace A.B {
        |  def f: Bool = false
        |}
        |namespace A {
        |  def g: Bool = A.B/f()
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "A/g")
  }

  test("Expression.Lambda.02") {
    val input =
      """namespace A { def f(x: Int): Int = 24 }
        |def g: Int = A/f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(24), "g")
  }

  test("Expression.Lambda.03") {
    val input =
      """namespace A { def f(x: Int): Int = x }
        |namespace A { def g: Int = f(3) }
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(3), "A/g")
  }

  test("Expression.Lambda.04") {
    val input =
      """def f(x: Int64, y: Int64): Int64 = x * y - 6i64
        |def g: Int64 = f(3i64, 42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(120), "g")
  }

  test("Expression.Lambda.05") {
    val input =
      """namespace A { def f(x: Int32): Int32 = let y = B/g(x + 1i32) in y * y }
        |namespace B { def g(x: Int32): Int32 = x - 4i32 }
        |namespace C { def h: Int32 = A/f(5i32) + B/g(0i32) }
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "C/h")
  }

  test("Expression.Lambda.06") {
    val input =
      """def f(x: Int16): Int16 = g(x + 1i16)
        |def g(x: Int16): Int16 = h(x + 10i16)
        |def h(x: Int16): Int16 = x * x
        |def x: Int16 = f(3i16)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(196), "x")
  }

  test("Expression.Lambda.07") {
    val input =
      """def f(x: Int8, y: Int8): Int8 = x - y
        |def g(x: Int8): Int8 = x * 3i8
        |def h(x: Int8): Int8 = g(x - 1i8)
        |def x: Int8 = let x = 7i8 in f(g(3i8), h(h(x)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-42), "x")
  }

  test("Expression.Lambda.08") {
    val input =
      """def f(x: Bool, y: Bool): Bool = if (x) true else y
        |def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "g01")
    t.runTest(Value.True, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.True, "g04")
  }

  test("Expression.Lambda.09") {
    val input =
      """def f(x: Bool, y: Bool): Bool = if (x) y else false
        |def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "g01")
    t.runTest(Value.False, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.False, "g04")
  }

  test("Expression.Lambda.10") {
    val input =
      """def f(x: Int, y: Int, z: Int): Int = x + y + z
        |def g: Int = f(2, 42, 5)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(49), "g")
  }

  test("Expression.Lambda.11") {
    val input =
      """def f(x: (Int) -> Int, y: Int): Int = x(y)
        |def g(x: Int): Int = x + 1
        |def h: Int = f(g, 5)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(6), "h")
  }

  test("Expression.Lambda.12") {
    val input =
      """def f(x: (Int) -> Int): (Int) -> Int = x
        |def g(x: Int): Int = x + 5
        |def h: Int = (f(g))(40)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(45), "h")
  }

  test("Expression.Lambda.13") {
    val input =
      """enum Val { case Val(Int) }
        |def f(x: Int): Val = Val.Val(x)
        |def g: Val = f(111)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(111)), "g")
  }

  test("Expression.Lambda.14") {
    val input =
      """def f(a: Int, b: Int, c: Str, d: Int, e: Bool, f: ()): (Int, Int, Str, Int, Bool, ()) = (a, b, c, d, e, f)
        |def g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)), "g")
  }

  test("Expression.Lambda.15") {
    val input =
      """def f(a: Int, b: Int, c: Int): Set[Int] = #{a, b, c}
        |def g: Set[Int] = f(24, 53, 24)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))), "g")
  }

  test("Expression.Lambda.16") {
    val input =
      """def f(a: Char, b: Char): Bool = a == b
        |def g: Bool = f('a', 'b')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "g")
  }

  test("Expression.Lambda.17") {
    val input =
      """def f(a: Float32, b: Float32): Float32 = a + b
        |def g: Float32 = f(1.2f32, 2.1f32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(3.3f), "g")
  }

  test("Expression.Lambda.18") {
    val input =
      """def f(a: Float64, b: Float64): Float64 = a + b
        |def g: Float64 = f(1.2f64, 2.1f64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(3.3d), "g")
  }

  test("Expression.Lambda.19") {
    val input =
      """def f(a: BigInt, b: BigInt): BigInt = a + b
        |def g: BigInt = f(1ii, 9223372036854775808ii)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("9223372036854775809"), "g")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Safe                                     //
  // Reimplements Expression.Lambda tests but using (safe) hooks instead.    //
  // Note that some Lambda tests can't be reimplemented here and vice versa. //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: We're forced to pass a dummy parameter.
  // See: https://github.com/magnus-madsen/flix/issues/130#issuecomment-216944729
  test("Expression.Hook - Hook.Safe.01") {
    import HookSafeHelpers._
    val input = "namespace A { def g: Bool = A.B/f(0) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val x = flix.mkFalse
    def nativeF(dummy: IValue): IValue = x

    t.addHook("A.B/f", flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType), nativeF _).run()
    t.runTest(Value.False, "A/g")
  }

  test("Expression.Hook - Hook.Safe.02") {
    import HookSafeHelpers._
    val input = "def g: Int = A/f(3)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = flix.mkInt32(24)

    t.addHook("A/f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(24), "g")
  }

  test("Expression.Hook - Hook.Safe.03") {
    import HookSafeHelpers._
    val input = "namespace A { def g: Int = f(3) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = x

    t.addHook("A/f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(3), "A/g")
  }

  test("Expression.Hook - Hook.Safe.04") {
    import HookSafeHelpers._
    val input = "def g: Int64 = f(3i64, 42i64)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type, flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: IValue, y: IValue): IValue = flix.mkInt64(x.getInt64 * y.getInt64 - 6)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt64(120), "g")
  }

  test("Expression.Hook - Hook.Safe.05") {
    import HookSafeHelpers._
    val input = "namespace C { def h: Int32 = A/f(5i32) + B/g(0i32) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = {
      val y = nativeG(flix.mkInt32(x.getInt32 + 1))
      flix.mkInt32(y.getInt32 * y.getInt32)
    }
    def nativeG(x: IValue): IValue = flix.mkInt32(x.getInt32 - 4)

    t.addHook("A/f", tpe, nativeF _).addHook("B/g", tpe, nativeG _).run()
    t.runTest(Value.mkInt32(0), "C/h")
  }

  test("Expression.Hook - Hook.Safe.06") {
    import HookSafeHelpers._
    val input = "def x: Int16 = f(3i16)"
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: IValue): IValue = nativeG(flix.mkInt16(x.getInt16 + 1))
    def nativeG(x: IValue): IValue = nativeH(flix.mkInt16(x.getInt16 + 10))
    def nativeH(x: IValue): IValue = flix.mkInt16(x.getInt16 * x.getInt16)

    t.addHook("f", tpe, nativeF _).addHook("g", tpe, nativeG _).addHook("h", tpe, nativeH _).run()
    t.runTest(Value.mkInt16(196), "x")
  }

  test("Expression.Hook - Hook.Safe.07") {
    import HookSafeHelpers._
    val input = "def x: Int8 = let x = 7i8 in f(g(3i8), h(h(x)))"
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe1 = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    val tpe2 = flix.mkFunctionType(Array(flix.mkInt8Type, flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: IValue, y: IValue): IValue = flix.mkInt8(x.getInt8 - y.getInt8)
    def nativeG(x: IValue): IValue = flix.mkInt8(x.getInt8 * 3)
    def nativeH(x: IValue): IValue = nativeG(flix.mkInt8(x.getInt8 - 1))

    t.addHook("f", tpe2, nativeF _).addHook("g", tpe1, nativeG _).addHook("h", tpe1, nativeH _).run()
    t.runTest(Value.mkInt8(-42), "x")
  }

  test("Expression.Hook - Hook.Safe.08") {
    import HookSafeHelpers._
    val input =
      """def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: IValue, y: IValue): IValue = if (x.getBool) flix.mkTrue else y

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.True, "g01")
    t.runTest(Value.True, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.True, "g04")
  }

  test("Expression.Hook - Hook.Safe.09") {
    import HookSafeHelpers._
    val input =
      """def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: IValue, y: IValue): IValue = if (x.getBool) y else flix.mkFalse

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.True, "g01")
    t.runTest(Value.False, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.False, "g04")
  }

  test("Expression.Hook - Hook.Safe.10") {
    import HookSafeHelpers._
    val input = "def g: Int = f(2, 42, 5)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue, y: IValue, z: IValue): IValue = flix.mkInt32(x.getInt32 + y.getInt32 + z.getInt32)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(49), "g")
  }

  test("Expression.Hook - Hook.Safe.11") {
    import HookSafeHelpers._
    val input =
      """def f(x: (Int) -> Int, y: Int): Int = x(y)
        |def h: Int = f(g, 5)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeG(x: IValue): IValue = flix.mkInt32(x.getInt32 + 1)

    t.addHook("g", tpeG, nativeG _).run()
    t.runTest(Value.mkInt32(6), "h")
  }

  test("Expression.Hook - Hook.Safe.12") {
    import HookSafeHelpers._
    val input =
      """def f(x: (Int) -> Int): (Int) -> Int = x
        |def h: Int = (f(g))(40)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeG(x: IValue): IValue = flix.mkInt32(x.getInt32 + 5)

    t.addHook("g", tpeG, nativeG _).run()
    t.runTest(Value.mkInt32(45), "h")
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  ignore("Expression.Hook - Hook.Safe.13") {
    import HookSafeHelpers._
    val input =
      """enum Val { case Val(Int) }
        |def g: Val = f(111)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tagTpe = flix.mkTagType("Val", "Val", flix.mkInt32Type)
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkEnumType("Val", Array(tagTpe)))
    def nativeF(x: IValue): IValue = flix.mkTag("Val", x)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkTag("Val", Value.mkInt32(111)), "g")
  }

  test("Expression.Hook - Hook.Safe.14") {
    import HookSafeHelpers._
    val input = """def g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())"""
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpes = Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkStrType, flix.mkInt32Type, flix.mkBoolType, flix.mkUnitType)
    val tpe = flix.mkFunctionType(tpes, flix.mkTupleType(tpes))
    def nativeF(a: IValue, b: IValue, c: IValue, d: IValue, e: IValue, f: IValue): IValue =
      flix.mkTuple(Array(a, b, c, d, e, f))

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)), "g")
  }

  test("Expression.Hook - Hook.Safe.15") {
    import HookSafeHelpers._
    val input = "def g: Set[Int] = f(24, 53, 24)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkSetType(flix.mkInt32Type))
    def nativeF(x: IValue, y: IValue, z: IValue): IValue = flix.mkSet(Set(x, y, z))

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))), "g")
  }

  test("Expression.Hook - Hook.Safe.16") {
    import HookSafeHelpers._
    val input = "def g: Bool = f('a', 'b')"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkCharType, flix.mkCharType), flix.mkBoolType)
    def nativeF(a: IValue, b: IValue): IValue = flix.mkBool(a.getChar == b.getChar)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.False, "g")
  }

  test("Expression.Hook - Hook.Safe.17") {
    import HookSafeHelpers._
    val input = "def g: Float32 = f(1.2f32, 2.1f32)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat32Type, flix.mkFloat32Type), flix.mkFloat32Type)
    def nativeF(a: IValue, b: IValue): IValue = flix.mkFloat32(a.getFloat32 + b.getFloat32)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkFloat32(3.3f), "g")
  }

  test("Expression.Hook - Hook.Safe.18") {
    import HookSafeHelpers._
    val input = "def g: Float64 = f(1.2f64, 2.1f64)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat64Type, flix.mkFloat64Type), flix.mkFloat64Type)
    def nativeF(a: IValue, b: IValue): IValue = flix.mkFloat64(a.getFloat64 + b.getFloat64)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkFloat64(3.3d), "g")
  }

  test("Expression.Hook - Hook.Safe.19") {
    import HookSafeHelpers._
    val input = "def g: BigInt = f(1ii, 9223372036854775808ii)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkBigIntType, flix.mkBigIntType), flix.mkBigIntType)
    def nativeF(a: IValue, b: IValue): IValue = flix.mkBigInt(a.getBigInt add b.getBigInt)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkBigInt("9223372036854775809"), "g")
  }

  test("Expression.Hook - Hook.Safe.20") {
    import HookSafeHelpers._
    val input = "def h: Native = g(f(999))"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeF = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    val tpeG = flix.mkFunctionType(Array(flix.mkNativeType), flix.mkNativeType)
    def nativeF(x: IValue): IValue = flix.mkNative(MyObject(x.getInt32))
    def nativeG(o: IValue): IValue = {
      val obj = o.getUnsafeRef.asInstanceOf[MyObject]
      flix.mkNative(MyObject(obj.x + 1))
    }

    t.addHook("f", tpeF, nativeF _).addHook("g", tpeG, nativeG _).run()
    t.runTest(MyObject(1000), "h")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Unsafe                                   //
  // Reimplements Expression.Lambda tests but using (unsafe) hooks instead . //
  // Note that some Lambda tests can't be reimplemented here and vice versa. //
  // Note that native functions need to be annotated with JBool, JInt, etc.  //
  // This is necessary so that implicits are properly called.                //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: We're forced to pass a dummy parameter.
  // See: https://github.com/magnus-madsen/flix/issues/130#issuecomment-216944729
  test("Expression.Hook - Hook.Unsafe.01") {
    import HookUnsafeHelpers._
    val input = "namespace A { def g: Bool = A.B/f(0) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val x = false
    def nativeF(dummy: JInt): JBool = x

    t.addHook("A.B/f", flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType), nativeF _).run()
    t.runTest(Value.False, "A/g")
  }

  test("Expression.Hook - Hook.Unsafe.02") {
    import HookUnsafeHelpers._
    val input = "def g: Int = A/f(3)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = 24

    t.addHook("A/f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(24), "g")
  }

  test("Expression.Hook - Hook.Unsafe.03") {
    import HookUnsafeHelpers._
    val input = "namespace A { def g: Int = f(3) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = x

    t.addHook("A/f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(3), "A/g")
  }

  test("Expression.Hook - Hook.Unsafe.04") {
    import HookUnsafeHelpers._
    val input = "def g: Int64 = f(3i64, 42i64)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type, flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: JLong, y: JLong): JLong = x * y - 6

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt64(120), "g")
  }

  test("Expression.Hook - Hook.Unsafe.05") {
    import HookUnsafeHelpers._
    val input = "namespace C { def h: Int32 = A/f(5i32) + B/g(0i32) }"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = { val y = nativeG(x + 1); y * y }
    def nativeG(x: JInt): JInt = x - 4

    t.addHook("A/f", tpe, nativeF _).addHook("B/g", tpe, nativeG _).run()
    t.runTest(Value.mkInt32(0), "C/h")
  }

  test("Expression.Hook - Hook.Unsafe.06") {
    import HookUnsafeHelpers._
    val input = "def x: Int16 = f(3i16)"
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: JShort): JShort = nativeG((x + 1).toShort)
    def nativeG(x: JShort): JShort = nativeH((x + 10).toShort)
    def nativeH(x: JShort): JShort = (x * x).toShort

    t.addHook("f", tpe, nativeF _).addHook("g", tpe, nativeG _).addHook("h", tpe, nativeH _).run()
    t.runTest(Value.mkInt16(196), "x")
  }

  test("Expression.Hook - Hook.Unsafe.07") {
    import HookUnsafeHelpers._
    val input = "def x: Int8 = let x = 7i8 in f(g(3i8), h(h(x)))"
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe1 = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    val tpe2 = flix.mkFunctionType(Array(flix.mkInt8Type, flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: JByte, y: JByte): JByte = (x - y).toByte
    def nativeG(x: JByte): JByte = (x * 3).toByte
    def nativeH(x: JByte): JByte = nativeG((x - 1).toByte)

    t.addHook("f", tpe2, nativeF _).addHook("g", tpe1, nativeG _).addHook("h", tpe1, nativeH _).run()
    t.runTest(Value.mkInt8(-42), "x")
  }

  test("Expression.Hook - Hook.Unsafe.08") {
    import HookUnsafeHelpers._
    val input =
      """def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: JBool, y: JBool): JBool = if (x) true else y

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.True, "g01")
    t.runTest(Value.True, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.True, "g04")
  }

  test("Expression.Hook - Hook.Unsafe.09") {
    import HookUnsafeHelpers._
    val input =
      """def g01: Bool = f(true, true)
        |def g02: Bool = f(true, false)
        |def g03: Bool = f(false, false)
        |def g04: Bool = f(false, true)
      """.stripMargin
    val t = new Tester(input, solve = false)
    val flix = t.flix

    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: JBool, y: JBool): JBool = if (x) y else false

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.True, "g01")
    t.runTest(Value.False, "g02")
    t.runTest(Value.False, "g03")
    t.runTest(Value.False, "g04")
  }

  test("Expression.Hook - Hook.Unsafe.10") {
    import HookUnsafeHelpers._
    val input = "def g: Int = f(2, 42, 5)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt, y: JInt, z: JInt): JInt = x + y + z

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(49), "g")
  }

  test("Expression.Hook - Hook.Unsafe.11") {
    import HookUnsafeHelpers._
    val input =
      """def f(x: (Int) -> Int, y: Int): Int = x(y)
        |def h: Int = f(g, 5)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeG(x: JInt): JInt = x + 1

    t.addHook("g", tpeG, nativeG _).run()
    t.runTest(Value.mkInt32(6), "h")
  }

  test("Expression.Hook - Hook.Unsafe.12") {
    import HookUnsafeHelpers._
    val input =
      """def f(x: (Int) -> Int): (Int) -> Int = x
        |def h: Int = (f(g))(40)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeG(x: JInt): JInt = x + 5

    t.addHook("g", tpeG, nativeG _).run()
    t.runTest(Value.mkInt32(45), "h")
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  ignore("Expression.Hook - Hook.Unsafe.13") {
    import HookUnsafeHelpers._
    val input =
      """enum Val { case Val(Int) }
        |def g: Val = f(111)
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tagTpe = flix.mkTagType("Val", "Val", flix.mkInt32Type)
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkEnumType("Val", Array(tagTpe)))
    def nativeF(x: JInt): Value.Tag = Value.mkTag("Val", x)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkTag("Val", Value.mkInt32(111)), "g")
  }

  test("Expression.Hook - Hook.Unsafe.14") {
    import HookUnsafeHelpers._
    val input = """def g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())"""
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpes = Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkStrType, flix.mkInt32Type, flix.mkBoolType, flix.mkUnitType)
    val tpe = flix.mkFunctionType(tpes, flix.mkTupleType(tpes))
    def nativeF(a: JInt, b: JInt, c: String, d: JInt, e: JBool, f: Value.Unit.type): Value.Tuple =
      Value.Tuple(Array(a, b, c, d, e, f))

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)), "g")
  }

  test("Expression.Hook - Hook.Unsafe.15") {
    import HookUnsafeHelpers._
    val input = "def g: Set[Int] = f(24, 53, 24)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkSetType(flix.mkInt32Type))
    def nativeF(x: JInt, y: JInt, z: JInt): Set[JInt] = Set(x, y, z)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))), "g")
  }

  test("Expression.Hook - Hook.Unsafe.16") {
    import HookUnsafeHelpers._
    val input = "def g: Bool = f('a', 'b')"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkCharType, flix.mkCharType), flix.mkBoolType)
    def nativeF(a: JChar, b: JChar): JBool = a == b

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.False, "g")
  }

  test("Expression.Hook - Hook.Unsafe.17") {
    import HookUnsafeHelpers._
    val input = "def g: Float32 = f(1.2f32, 2.1f32)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat32Type, flix.mkFloat32Type), flix.mkFloat32Type)
    def nativeF(a: JFloat, b: JFloat): JFloat = a + b

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkFloat32(3.3f), "g")
  }

  test("Expression.Hook - Hook.Unsafe.18") {
    import HookUnsafeHelpers._
    val input = "def g: Float64 = f(1.2f64, 2.1f64)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat64Type, flix.mkFloat64Type), flix.mkFloat64Type)
    def nativeF(a: JDouble, b: JDouble): JDouble = a + b

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkFloat64(3.3d), "g")
  }

  test("Expression.Hook - Hook.Unsafe.19") {
    import HookUnsafeHelpers._
    val input = "def g: BigInt = f(1ii, 9223372036854775808ii)"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkBigIntType, flix.mkBigIntType), flix.mkBigIntType)
    def nativeF(a: JBigInt, b: JBigInt): JBigInt = a add b

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkBigInt("9223372036854775809"), "g")
  }

  test("Expression.Hook - Hook.Unsafe.20") {
    import HookUnsafeHelpers._
    val input = "def h: Native = g(f(999))"
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpeF = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    val tpeG = flix.mkFunctionType(Array(flix.mkNativeType), flix.mkNativeType)
    def nativeF(x: JInt): MyObject = MyObject(x)
    def nativeG(o: MyObject): MyObject = MyObject(o.x + 1)

    t.addHook("f", tpeF, nativeF _).addHook("g", tpeG, nativeG _).run()
    t.runTest(MyObject(1000), "h")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Unary                                                        //
  // UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unary - UnaryOperator.LogicalNot.01") {
    val input = "def f: Bool = !true"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Unary - UnaryOperator.LogicalNot.02") {
    val input = "def f: Bool = !false"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Unary - UnaryOperator.Plus.01") {
    val input =
      s"""def f01: Int = +0
         |def f02: Int = +36000
         |def f03: Int = +(-36000)
         |def f04: Int = +${Int.MaxValue}
         |def f05: Int = +${Int.MinValue}
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(36000), "f02")
    t.runTest(Value.mkInt32(-36000), "f03")
    t.runTest(Value.mkInt32(Int.MaxValue), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Plus.02") {
    val input =
      s"""def f01: Int8 = +0i8
         |def f02: Int8 = +36i8
         |def f03: Int8 = +(-36i8)
         |def f04: Int8 = +${Byte.MaxValue}i8
         |def f05: Int8 = +${Byte.MinValue}i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(0), "f01")
    t.runTest(Value.mkInt8(36), "f02")
    t.runTest(Value.mkInt8(-36), "f03")
    t.runTest(Value.mkInt8(Byte.MaxValue), "f04")
    t.runTest(Value.mkInt8(Byte.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Plus.03") {
    val input =
      s"""def f01: Int16 = +0i16
         |def f02: Int16 = +3600i16
         |def f03: Int16 = +(-3600i16)
         |def f04: Int16 = +${Short.MaxValue}i16
         |def f05: Int16 = +${Short.MinValue}i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(0), "f01")
    t.runTest(Value.mkInt16(3600), "f02")
    t.runTest(Value.mkInt16(-3600), "f03")
    t.runTest(Value.mkInt16(Short.MaxValue), "f04")
    t.runTest(Value.mkInt16(Short.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Plus.04") {
    val input =
      s"""def f01: Int32 = +0i32
         |def f02: Int32 = +36000i32
         |def f03: Int32 = +(-36000i32)
         |def f04: Int32 = +${Int.MaxValue}i32
         |def f05: Int32 = +${Int.MinValue}i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(36000), "f02")
    t.runTest(Value.mkInt32(-36000), "f03")
    t.runTest(Value.mkInt32(Int.MaxValue), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Plus.05") {
    val input =
      s"""def f01: Int64 = +0i64
         |def f02: Int64 = +3600000000i64
         |def f03: Int64 = +(-3600000000i64)
         |def f04: Int64 = +${Long.MaxValue}i64
         |def f05: Int64 = +${Long.MinValue}i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0), "f01")
    t.runTest(Value.mkInt64(3600000000L), "f02")
    t.runTest(Value.mkInt64(-3600000000L), "f03")
    t.runTest(Value.mkInt64(Long.MaxValue), "f04")
    t.runTest(Value.mkInt64(Long.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Plus.06") {
    val input =
      s"""def f01: Float = +0.0
         |def f02: Float = +(-0.0)
         |def f03: Float = +(4.2)
         |def f04: Float = +99999999999999999999999999999999999999999999999999999999999999999999999999999999.0
         |def f05: Float = +0.000000000000000000000000000000000000000000000000000000000000000000000000000000001
         |def f06: Float = +(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)
         |def f07: Float = +(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.0), "f01")
    t.runTest(Value.mkFloat64(0.0), "f02")
    t.runTest(Value.mkFloat64(4.2), "f03")
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f04")
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f05")
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f06")
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f07")
  }

  test("Expression.Unary - UnaryOperator.Plus.07") {
    val input =
      s"""def f01: Float32 = +0.0f32
         |def f02: Float32 = +(-0.0f32)
         |def f03: Float32 = +(4.2f32)
         |def f04: Float32 = +999999999999999999999999999999.0f32
         |def f05: Float32 = +0.0000000000000000000000000000001f32
         |def f06: Float32 = +(-999999999999999999999999999999.0f32)
         |def f07: Float32 = +(-0.0000000000000000000000000000001f32)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(0.0f), "f01")
    t.runTest(Value.mkFloat32(-0.0f), "f02")
    t.runTest(Value.mkFloat32(4.2f), "f03")
    t.runTest(Value.mkFloat32(999999999999999999999999999999.0f), "f04")
    t.runTest(Value.mkFloat32(0.0000000000000000000000000000001f), "f05")
    t.runTest(Value.mkFloat32(-999999999999999999999999999999.0f), "f06")
    t.runTest(Value.mkFloat32(-0.0000000000000000000000000000001f), "f07")
  }

  test("Expression.Unary - UnaryOperator.Plus.08") {
    val input =
      s"""def f01: Float64 = +0.0f64
         |def f02: Float64 = +(-0.0f64)
         |def f03: Float64 = +(4.2f64)
         |def f04: Float64 = +99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64
         |def f05: Float64 = +0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64
         |def f06: Float64 = +(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)
         |def f07: Float64 = +(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.0d), "f01")
    t.runTest(Value.mkFloat64(-0.0d), "f02")
    t.runTest(Value.mkFloat64(4.2d), "f03")
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f04")
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f05")
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f06")
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f07")
  }

  test("Expression.Unary - UnaryOperator.Plus.09") {
    val input =
      s"""def f01: BigInt = +0ii
         |def f02: BigInt = +1ii
         |def f03: BigInt = +10ii
         |def f04: BigInt = +9223372036854775808ii
         |def f05: BigInt = +(-9223372036854775809ii)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(0), "f01")
    t.runTest(Value.mkBigInt(1), "f02")
    t.runTest(Value.mkBigInt(10), "f03")
    t.runTest(Value.mkBigInt("9223372036854775808"), "f04")
    t.runTest(Value.mkBigInt("-9223372036854775809"), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.01") {
    val input =
      s"""def f01: Int = -0
         |def f02: Int = -36000
         |def f03: Int = -(-36000)
         |def f04: Int = -${Int.MaxValue}
         |def f05: Int = -${Int.MinValue}
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(-36000), "f02")
    t.runTest(Value.mkInt32(36000), "f03")
    t.runTest(Value.mkInt32(-Int.MaxValue), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.02") {
    val input =
      s"""def f01: Int8 = -0i8
         |def f02: Int8 = -36i8
         |def f03: Int8 = -(-36i8)
         |def f04: Int8 = -${Byte.MaxValue}i8
         |def f05: Int8 = -${Byte.MinValue}i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(0), "f01")
    t.runTest(Value.mkInt8(-36), "f02")
    t.runTest(Value.mkInt8(36), "f03")
    t.runTest(Value.mkInt8(-Byte.MaxValue), "f04")
    t.runTest(Value.mkInt8(Byte.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.03") {
    val input =
      s"""def f01: Int16 = -0i16
         |def f02: Int16 = -3600i16
         |def f03: Int16 = -(-3600i16)
         |def f04: Int16 = -${Short.MaxValue}i16
         |def f05: Int16 = -${Short.MinValue}i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(0), "f01")
    t.runTest(Value.mkInt16(-3600), "f02")
    t.runTest(Value.mkInt16(3600), "f03")
    t.runTest(Value.mkInt16(-Short.MaxValue), "f04")
    t.runTest(Value.mkInt16(Short.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.04") {
    val input =
      s"""def f01: Int32 = -0i32
         |def f02: Int32 = -36000i32
         |def f03: Int32 = -(-36000i32)
         |def f04: Int32 = -${Int.MaxValue}i32
         |def f05: Int32 = -${Int.MinValue}i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(-36000), "f02")
    t.runTest(Value.mkInt32(36000), "f03")
    t.runTest(Value.mkInt32(-Int.MaxValue), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.05") {
    val input =
      s"""def f01: Int64 = -0i64
         |def f02: Int64 = -3600000000i64
         |def f03: Int64 = -(-3600000000i64)
         |def f04: Int64 = -${Long.MaxValue}i64
         |def f05: Int64 = -${Long.MinValue}i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0), "f01")
    t.runTest(Value.mkInt64(-3600000000L), "f02")
    t.runTest(Value.mkInt64(3600000000L), "f03")
    t.runTest(Value.mkInt64(-Long.MaxValue), "f04")
    t.runTest(Value.mkInt64(Long.MinValue), "f05")
  }

  test("Expression.Unary - UnaryOperator.Minus.06") {
    val input =
      s"""def f01: Float = -0.0
         |def f02: Float = -(-0.0)
         |def f03: Float = -(4.2)
         |def f04: Float = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0
         |def f05: Float = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001
         |def f06: Float = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)
         |def f07: Float = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.0), "f01")
    t.runTest(Value.mkFloat64(0.0), "f02")
    t.runTest(Value.mkFloat64(-4.2), "f03")
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f04")
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f05")
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0), "f06")
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001), "f07")
  }

  test("Expression.Unary - UnaryOperator.Minus.07") {
    val input =
      s"""def f01: Float32 = -0.0f32
         |def f02: Float32 = -(-0.0f32)
         |def f03: Float32 = -(4.2f32)
         |def f04: Float32 = -999999999999999999999999999999.0f32
         |def f05: Float32 = -0.0000000000000000000000000000001f32
         |def f06: Float32 = -(-999999999999999999999999999999.0f32)
         |def f07: Float32 = -(-0.0000000000000000000000000000001f32)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(-0.0f), "f01")
    t.runTest(Value.mkFloat32(0.0f), "f02")
    t.runTest(Value.mkFloat32(-4.2f), "f03")
    t.runTest(Value.mkFloat32(-999999999999999999999999999999.0f), "f04")
    t.runTest(Value.mkFloat32(-0.0000000000000000000000000000001f), "f05")
    t.runTest(Value.mkFloat32(999999999999999999999999999999.0f), "f06")
    t.runTest(Value.mkFloat32(0.0000000000000000000000000000001f), "f07")
  }

  test("Expression.Unary - UnaryOperator.Minus.08") {
    val input =
      s"""def f01: Float64 = -0.0f64
         |def f02: Float64 = -(-0.0f64)
         |def f03: Float64 = -(4.2f64)
         |def f04: Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64
         |def f05: Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64
         |def f06: Float64 = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)
         |def f07: Float64 = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-0.0d), "f01")
    t.runTest(Value.mkFloat64(0.0d), "f02")
    t.runTest(Value.mkFloat64(-4.2d), "f03")
    t.runTest(Value.mkFloat64(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f04")
    t.runTest(Value.mkFloat64(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f05")
    t.runTest(Value.mkFloat64(99999999999999999999999999999999999999999999999999999999999999999999999999999999.0d), "f06")
    t.runTest(Value.mkFloat64(0.000000000000000000000000000000000000000000000000000000000000000000000000000000001d), "f07")
  }

  test("Expression.Unary - UnaryOperator.Minus.09") {
    val input =
      s"""def f01: BigInt = -0ii
         |def f02: BigInt = -1ii
         |def f03: BigInt = -10ii
         |def f04: BigInt = -9223372036854775808ii
         |def f05: BigInt = -(-9223372036854775809ii)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(-0), "f01")
    t.runTest(Value.mkBigInt(-1), "f02")
    t.runTest(Value.mkBigInt(-10), "f03")
    t.runTest(Value.mkBigInt("-9223372036854775808"), "f04")
    t.runTest(Value.mkBigInt("9223372036854775809"), "f05")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.01") {
    val input =
      s"""def f01: Int = ~0
         |def f02: Int = ~1
         |def f03: Int = ~(-1)
         |def f04: Int = ~36000
         |def f05: Int = ~(-36000)
         |def f06: Int = ~${Int.MaxValue}
         |def f07: Int = ~${Int.MinValue}
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-1), "f01")
    t.runTest(Value.mkInt32(-2), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(-36001), "f04")
    t.runTest(Value.mkInt32(35999), "f05")
    t.runTest(Value.mkInt32(Int.MinValue), "f06")
    t.runTest(Value.mkInt32(Int.MaxValue), "f07")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.02") {
    val input =
      s"""def f01: Int8 = ~0i8
         |def f02: Int8 = ~1i8
         |def f03: Int8 = ~(-1i8)
         |def f04: Int8 = ~42i8
         |def f05: Int8 = ~(-42i8)
         |def f06: Int8 = ~${Byte.MaxValue}i8
         |def f07: Int8 = ~${Byte.MinValue}i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(-1), "f01")
    t.runTest(Value.mkInt8(-2), "f02")
    t.runTest(Value.mkInt8(0), "f03")
    t.runTest(Value.mkInt8(-43), "f04")
    t.runTest(Value.mkInt8(41), "f05")
    t.runTest(Value.mkInt8(Byte.MinValue), "f06")
    t.runTest(Value.mkInt8(Byte.MaxValue), "f07")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.03") {
    val input =
      s"""def f01: Int16 = ~0i16
         |def f02: Int16 = ~1i16
         |def f03: Int16 = ~(-1i16)
         |def f04: Int16 = ~420i16
         |def f05: Int16 = ~(-420i16)
         |def f06: Int16 = ~${Short.MaxValue}i16
         |def f07: Int16 = ~${Short.MinValue}i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(-1), "f01")
    t.runTest(Value.mkInt16(-2), "f02")
    t.runTest(Value.mkInt16(0), "f03")
    t.runTest(Value.mkInt16(-421), "f04")
    t.runTest(Value.mkInt16(419), "f05")
    t.runTest(Value.mkInt16(Short.MinValue), "f06")
    t.runTest(Value.mkInt16(Short.MaxValue), "f07")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.04") {
    val input =
      s"""def f01: Int32 = ~0i32
         |def f02: Int32 = ~1i32
         |def f03: Int32 = ~(-1i32)
         |def f04: Int32 = ~36000i32
         |def f05: Int32 = ~(-36000i32)
         |def f06: Int32 = ~${Int.MaxValue}i32
         |def f07: Int32 = ~${Int.MinValue}i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-1), "f01")
    t.runTest(Value.mkInt32(-2), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(-36001), "f04")
    t.runTest(Value.mkInt32(35999), "f05")
    t.runTest(Value.mkInt32(Int.MinValue), "f06")
    t.runTest(Value.mkInt32(Int.MaxValue), "f07")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.05") {
    val input =
      s"""def f01: Int64 = ~0i64
         |def f02: Int64 = ~1i64
         |def f03: Int64 = ~(-1i64)
         |def f04: Int64 = ~10000000000i64
         |def f05: Int64 = ~(-10000000000i64)
         |def f06: Int64 = ~${Long.MaxValue}i64
         |def f07: Int64 = ~${Long.MinValue}i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-1), "f01")
    t.runTest(Value.mkInt64(-2), "f02")
    t.runTest(Value.mkInt64(0), "f03")
    t.runTest(Value.mkInt64(-10000000001L), "f04")
    t.runTest(Value.mkInt64(9999999999L), "f05")
    t.runTest(Value.mkInt64(Long.MinValue), "f06")
    t.runTest(Value.mkInt64(Long.MaxValue), "f07")
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.06") {
    val input =
      s"""def f01: BigInt = ~0ii
         |def f02: BigInt = ~1ii
         |def f03: BigInt = ~(-1ii)
         |def f04: BigInt = ~9223372036854775808ii
         |def f05: BigInt = ~(-9223372036854775809ii)
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(-1), "f01")
    t.runTest(Value.mkBigInt(-2), "f02")
    t.runTest(Value.mkBigInt(0), "f03")
    t.runTest(Value.mkBigInt("-9223372036854775809"), "f04")
    t.runTest(Value.mkBigInt("9223372036854775808"), "f05")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Arithmetic)                                          //
  // BinaryOperator.{Plus,Minus,Times,Divide,Modulo,Exponentiate}            //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.Plus.01") {
    val input =
      s"""def f01: Int = ${Int.MaxValue} + 1
         |def f02: Int = 100000 + 400000
         |def f03: Int = -400000 + 100000
         |def f04: Int = -100000 + 400000
         |def f05: Int = ${Int.MinValue} + -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MinValue), "f01")
    t.runTest(Value.mkInt32(500000), "f02")
    t.runTest(Value.mkInt32(-300000), "f03")
    t.runTest(Value.mkInt32(300000), "f04")
    t.runTest(Value.mkInt32(Int.MaxValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.02") {
    val input =
      s"""def f01: Int8 = ${Byte.MaxValue}i8 + 1i8
         |def f02: Int8 = 10i8 + 40i8
         |def f03: Int8 = -40i8 + 10i8
         |def f04: Int8 = -10i8 + 40i8
         |def f05: Int8 = ${Byte.MinValue}i8 + -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(Byte.MinValue), "f01")
    t.runTest(Value.mkInt8(50), "f02")
    t.runTest(Value.mkInt8(-30), "f03")
    t.runTest(Value.mkInt8(30), "f04")
    t.runTest(Value.mkInt8(Byte.MaxValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.03") {
    val input =
      s"""def f01: Int16 = ${Short.MaxValue}i16 + 1i16
         |def f02: Int16 = 1000i16 + 4000i16
         |def f03: Int16 = -4000i16 + 1000i16
         |def f04: Int16 = -1000i16 + 4000i16
         |def f05: Int16 = ${Short.MinValue}i16 + -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Short.MinValue), "f01")
    t.runTest(Value.mkInt16(5000), "f02")
    t.runTest(Value.mkInt16(-3000), "f03")
    t.runTest(Value.mkInt16(3000), "f04")
    t.runTest(Value.mkInt16(Short.MaxValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.04") {
    val input =
      s"""def f01: Int32 = ${Int.MaxValue}i32 + 1i32
         |def f02: Int32 = 100000i32 + 400000i32
         |def f03: Int32 = -400000i32 + 100000i32
         |def f04: Int32 = -100000i32 + 400000i32
         |def f05: Int32 = ${Int.MinValue}i32 + -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MinValue), "f01")
    t.runTest(Value.mkInt32(500000), "f02")
    t.runTest(Value.mkInt32(-300000), "f03")
    t.runTest(Value.mkInt32(300000), "f04")
    t.runTest(Value.mkInt32(Int.MaxValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.05") {
    val input =
      s"""def f01: Int64 = ${Long.MaxValue}i64 + 1i64
         |def f02: Int64 = 10000000000i64 + 40000000000i64
         |def f03: Int64 = -40000000000i64 + 10000000000i64
         |def f04: Int64 = -10000000000i64 + 40000000000i64
         |def f05: Int64 = ${Long.MinValue}i64 + -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Long.MinValue), "f01")
    t.runTest(Value.mkInt64(50000000000L), "f02")
    t.runTest(Value.mkInt64(-30000000000L), "f03")
    t.runTest(Value.mkInt64(30000000000L), "f04")
    t.runTest(Value.mkInt64(Long.MaxValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.06") {
    val input =
      s"""def f01: Float = 12.34 + 56.78
         |def f02: Float = 1234567890000000000000000000000000000000000000000.987654321 + 222.222
         |def f03: Float = -1234567890000000000000000000000000000000000000000.987654321 + 0.0
         |def f04: Float = 0.0000000000000000000000000000000000000000987654321 + 0.222
         |def f05: Float = -0.0000000000000000000000000000000000000000987654321 + 0.222
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(69.12), "f01")
    t.runTest(Value.mkFloat64(1.23456789E48), "f02")
    t.runTest(Value.mkFloat64(-1.23456789E48), "f03")
    t.runTest(Value.mkFloat64(0.222), "f04")
    t.runTest(Value.mkFloat64(0.222), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.07") {
    val input =
      s"""def f01: Float32 = 12.34f32 + 56.78f32
         |def f02: Float32 = 123456789000000000000000000000000000000.987654321f32 + 222.222f32
         |def f03: Float32 = -123456789000000000000000000000000000000.987654321f32 + 0.0f32
         |def f04: Float32 = 0.000000000000000000000000000000987654321f32 + 0.222f32
         |def f05: Float32 = -0.000000000000000000000000000000987654321f32 + 0.222f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(69.119995f), "f01")
    t.runTest(Value.mkFloat32(1.23456789E38f), "f02")
    t.runTest(Value.mkFloat32(-1.23456789E38f), "f03")
    t.runTest(Value.mkFloat32(0.222f), "f04")
    t.runTest(Value.mkFloat32(0.222f), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.08") {
    val input =
      s"""def f01: Float64 = 12.34f64 + 56.78f64
         |def f02: Float64 = 1234567890000000000000000000000000000000000000000.987654321f64 + 222.222f64
         |def f03: Float64 = -1234567890000000000000000000000000000000000000000.987654321f64 + 0.0f64
         |def f04: Float64 = 0.0000000000000000000000000000000000000000987654321f64 + 0.222f64
         |def f05: Float64 = -0.0000000000000000000000000000000000000000987654321f64 + 0.222f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(69.12d), "f01")
    t.runTest(Value.mkFloat64(1.23456789E48d), "f02")
    t.runTest(Value.mkFloat64(-1.23456789E48d), "f03")
    t.runTest(Value.mkFloat64(0.222d), "f04")
    t.runTest(Value.mkFloat64(0.222d), "f05")
  }

  test("Expression.Binary - BinaryOperator.Plus.09") {
    val input =
      s"""def f01: BigInt = 9223372036854775808ii + 1ii
         |def f02: BigInt = 10000000000000000000ii + 40000000000000000000ii
         |def f03: BigInt = -40000000000000000000ii + 10000000000000000000ii
         |def f04: BigInt = -10000000000000000000ii + 40000000000000000000ii
         |def f05: BigInt = -9223372036854775809ii + -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("9223372036854775809"), "f01")
    t.runTest(Value.mkBigInt("50000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("-30000000000000000000"), "f03")
    t.runTest(Value.mkBigInt("30000000000000000000"), "f04")
    t.runTest(Value.mkBigInt("-9223372036854775810"), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.01") {
    val input =
      s"""def f01: Int = ${Int.MinValue} - 1
         |def f02: Int = 400000 - 100000
         |def f03: Int = -400000 - 100000
         |def f04: Int = -100000 - 400000
         |def f05: Int = ${Int.MaxValue} - -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f01")
    t.runTest(Value.mkInt32(300000), "f02")
    t.runTest(Value.mkInt32(-500000), "f03")
    t.runTest(Value.mkInt32(-500000), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.02") {
    val input =
      s"""def f01: Int8 = ${Byte.MinValue}i8 - 1i8
         |def f02: Int8 = 40i8 - 10i8
         |def f03: Int8 = -40i8 - 10i8
         |def f04: Int8 = -10i8 - 40i8
         |def f05: Int8 = ${Byte.MaxValue}i8 - -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(Byte.MaxValue), "f01")
    t.runTest(Value.mkInt8(30), "f02")
    t.runTest(Value.mkInt8(-50), "f03")
    t.runTest(Value.mkInt8(-50), "f04")
    t.runTest(Value.mkInt8(Byte.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.03") {
    val input =
      s"""def f01: Int16 = ${Short.MinValue}i16 - 1i16
         |def f02: Int16 = 4000i16 - 1000i16
         |def f03: Int16 = -4000i16 - 1000i16
         |def f04: Int16 = -1000i16 - 4000i16
         |def f05: Int16 = ${Short.MaxValue}i16 - -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Short.MaxValue), "f01")
    t.runTest(Value.mkInt16(3000), "f02")
    t.runTest(Value.mkInt16(-5000), "f03")
    t.runTest(Value.mkInt16(-5000), "f04")
    t.runTest(Value.mkInt16(Short.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.04") {
    val input =
      s"""def f01: Int32 = ${Int.MinValue}i32 - 1i32
         |def f02: Int32 = 400000i32 - 100000i32
         |def f03: Int32 = -400000i32 - 100000i32
         |def f04: Int32 = -100000i32 - 400000i32
         |def f05: Int32 = ${Int.MaxValue}i32 - -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f01")
    t.runTest(Value.mkInt32(300000), "f02")
    t.runTest(Value.mkInt32(-500000), "f03")
    t.runTest(Value.mkInt32(-500000), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.05") {
    val input =
      s"""def f01: Int64 = ${Long.MinValue}i64 - 1i64
         |def f02: Int64 = 40000000000i64 - 10000000000i64
         |def f03: Int64 = -40000000000i64 - 10000000000i64
         |def f04: Int64 = -10000000000i64 - 40000000000i64
         |def f05: Int64 = ${Long.MaxValue}i64 - -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Long.MaxValue), "f01")
    t.runTest(Value.mkInt64(30000000000L), "f02")
    t.runTest(Value.mkInt64(-50000000000L), "f03")
    t.runTest(Value.mkInt64(-50000000000L), "f04")
    t.runTest(Value.mkInt64(Long.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.06") {
    val input =
      s"""def f01: Float = 12.34 - 56.78
         |def f02: Float = 1234567890000000000000000000000000000000000000000.987654321 - 222.222
         |def f03: Float = -1234567890000000000000000000000000000000000000000.987654321 - 0.0
         |def f04: Float = 0.0000000000000000000000000000000000000000987654321 - 0.222
         |def f05: Float = -0.0000000000000000000000000000000000000000987654321 - 0.222
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-44.44), "f01")
    t.runTest(Value.mkFloat64(1.23456789E48), "f02")
    t.runTest(Value.mkFloat64(-1.23456789E48), "f03")
    t.runTest(Value.mkFloat64(-0.222), "f04")
    t.runTest(Value.mkFloat64(-0.222), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.07") {
    val input =
      s"""def f01: Float32 = 12.34f32 - 56.78f32
         |def f02: Float32 = 123456789000000000000000000000000000000.987654321f32 - 222.222f32
         |def f03: Float32 = -123456789000000000000000000000000000000.987654321f32 - 0.0f32
         |def f04: Float32 = 0.000000000000000000000000000000987654321f32 - 0.222f32
         |def f05: Float32 = -0.000000000000000000000000000000987654321f32 - 0.222f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(-44.44f), "f01")
    t.runTest(Value.mkFloat32(1.23456789E38f), "f02")
    t.runTest(Value.mkFloat32(-1.23456789E38f), "f03")
    t.runTest(Value.mkFloat32(-0.222f), "f04")
    t.runTest(Value.mkFloat32(-0.222f), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.08") {
    val input =
      s"""def f01: Float64 = 12.34f64 - 56.78f64
         |def f02: Float64 = 1234567890000000000000000000000000000000000000000.987654321f64 - 222.222f64
         |def f03: Float64 = -1234567890000000000000000000000000000000000000000.987654321f64 - 0.0f64
         |def f04: Float64 = 0.0000000000000000000000000000000000000000987654321f64 - 0.222f64
         |def f05: Float64 = -0.0000000000000000000000000000000000000000987654321f64 - 0.222f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(-44.44d), "f01")
    t.runTest(Value.mkFloat64(1.23456789E48d), "f02")
    t.runTest(Value.mkFloat64(-1.23456789E48d), "f03")
    t.runTest(Value.mkFloat64(-0.222d), "f04")
    t.runTest(Value.mkFloat64(-0.222d), "f05")
  }

  test("Expression.Binary - BinaryOperator.Minus.09") {
    val input =
      s"""def f01: BigInt = -9223372036854775809ii - 1ii
         |def f02: BigInt = 40000000000000000000ii - 10000000000000000000ii
         |def f03: BigInt = -40000000000000000000ii - 10000000000000000000ii
         |def f04: BigInt = -10000000000000000000ii - 40000000000000000000ii
         |def f05: BigInt = 9223372036854775808ii - -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("-9223372036854775810"), "f01")
    t.runTest(Value.mkBigInt("30000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("-50000000000000000000"), "f03")
    t.runTest(Value.mkBigInt("-50000000000000000000"), "f04")
    t.runTest(Value.mkBigInt("9223372036854775809"), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.01") {
    val input =
      s"""def f01: Int = ${Int.MaxValue} * 2
         |def f02: Int = 300 * 200
         |def f03: Int = -200 * 300
         |def f04: Int = -200 * -300
         |def f05: Int = ${Int.MinValue} * -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-2), "f01")
    t.runTest(Value.mkInt32(60000), "f02")
    t.runTest(Value.mkInt32(-60000), "f03")
    t.runTest(Value.mkInt32(60000), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.02") {
    val input =
      s"""def f01: Int8 = ${Byte.MaxValue}i8 * 2i8
         |def f02: Int8 = 3i8 * 2i8
         |def f03: Int8 = -2i8 * 3i8
         |def f04: Int8 = -2i8 * -3i8
         |def f05: Int8 = ${Byte.MinValue}i8 * -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(-2), "f01")
    t.runTest(Value.mkInt8(6), "f02")
    t.runTest(Value.mkInt8(-6), "f03")
    t.runTest(Value.mkInt8(6), "f04")
    t.runTest(Value.mkInt8(Byte.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.03") {
    val input =
      s"""def f01: Int16 = ${Short.MaxValue}i16 * 2i16
         |def f02: Int16 = 30i16 * 20i16
         |def f03: Int16 = -20i16 * 30i16
         |def f04: Int16 = -20i16 * -30i16
         |def f05: Int16 = ${Short.MinValue}i16 * -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(-2), "f01")
    t.runTest(Value.mkInt16(600), "f02")
    t.runTest(Value.mkInt16(-600), "f03")
    t.runTest(Value.mkInt16(600), "f04")
    t.runTest(Value.mkInt16(Short.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.04") {
    val input =
      s"""def f01: Int32 = ${Int.MaxValue}i32 * 2i32
         |def f02: Int32 = 300i32 * 200i32
         |def f03: Int32 = -200i32 * 300i32
         |def f04: Int32 = -200i32 * -300i32
         |def f05: Int32 = ${Int.MinValue}i32 * -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-2), "f01")
    t.runTest(Value.mkInt32(60000), "f02")
    t.runTest(Value.mkInt32(-60000), "f03")
    t.runTest(Value.mkInt32(60000), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.05") {
    val input =
      s"""def f01: Int64 = ${Long.MaxValue}i64 * 2i64
         |def f02: Int64 = 300000i64 * 200000i64
         |def f03: Int64 = -200000i64 * 300000i64
         |def f04: Int64 = -200000i64 * -300000i64
         |def f05: Int64 = ${Long.MinValue}i64 * -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-2), "f01")
    t.runTest(Value.mkInt64(60000000000L), "f02")
    t.runTest(Value.mkInt64(-60000000000L), "f03")
    t.runTest(Value.mkInt64(60000000000L), "f04")
    t.runTest(Value.mkInt64(Long.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.06") {
    val input =
      s"""def f01: Float = 12.34 * 56.78
         |def f02: Float = 1234567890000000000000000000000000000000000000000.987654321 * 222.222
         |def f03: Float = -1234567890000000000000000000000000000000000000000.987654321 * 222.222
         |def f04: Float = 0.0000000000000000000000000000000000000000987654321 * 0.222
         |def f05: Float = -0.0000000000000000000000000000000000000000987654321 * 0.222
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(700.6652), "f01")
    t.runTest(Value.mkFloat64(2.7434814565158003E50), "f02")
    t.runTest(Value.mkFloat64(-2.7434814565158003E50), "f03")
    t.runTest(Value.mkFloat64(2.19259259262E-41), "f04")
    t.runTest(Value.mkFloat64(-2.19259259262E-41), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.07") {
    val input =
      s"""def f01: Float32 = 12.34f32 * 56.78f32
         |def f02: Float32 = 123456789000000000000000000000000000000.987654321f32 * 0.222f32
         |def f03: Float32 = -123456789000000000000000000000000000000.987654321f32 * 0.222f32
         |def f04: Float32 = 0.000000000000000000000000000000987654321f32 * 222.222f32
         |def f05: Float32 = -0.000000000000000000000000000000987654321f32 * 222.222f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(700.6652f), "f01")
    t.runTest(Value.mkFloat32(2.7407407E37f), "f02")
    t.runTest(Value.mkFloat32(-2.7407407E37f), "f03")
    t.runTest(Value.mkFloat32(2.1947852E-28f), "f04")
    t.runTest(Value.mkFloat32(-2.1947852E-28f), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.08") {
    val input =
      s"""def f01: Float64 = 12.34f64 * 56.78f64
         |def f02: Float64 = 1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64
         |def f03: Float64 = -1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64
         |def f04: Float64 = 0.0000000000000000000000000000000000000000987654321f64 * 0.222f64
         |def f05: Float64 = -0.0000000000000000000000000000000000000000987654321f64 * 0.222f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(700.6652d), "f01")
    t.runTest(Value.mkFloat64(2.7434814565158003E50d), "f02")
    t.runTest(Value.mkFloat64(-2.7434814565158003E50d), "f03")
    t.runTest(Value.mkFloat64(2.19259259262E-41d), "f04")
    t.runTest(Value.mkFloat64(-2.19259259262E-41d), "f05")
  }

  test("Expression.Binary - BinaryOperator.Times.09") {
    val input =
      s"""def f01: BigInt = 9223372036854775808ii * 2ii
         |def f02: BigInt = 30000000000ii * 20000000000ii
         |def f03: BigInt = -20000000000ii * 30000000000ii
         |def f04: BigInt = -20000000000ii * -30000000000ii
         |def f05: BigInt = -9223372036854775809ii * -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("18446744073709551616"), "f01")
    t.runTest(Value.mkBigInt("600000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("-600000000000000000000"), "f03")
    t.runTest(Value.mkBigInt("600000000000000000000"), "f04")
    t.runTest(Value.mkBigInt("9223372036854775809"), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.01") {
    val input =
      s"""def f01: Int = ${Int.MaxValue} / 1
         |def f02: Int = 1200000 / 3
         |def f03: Int = -1200000 / 3
         |def f04: Int = -3 / 1200000
         |def f05: Int = ${Int.MinValue} / -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f01")
    t.runTest(Value.mkInt32(400000), "f02")
    t.runTest(Value.mkInt32(-400000), "f03")
    t.runTest(Value.mkInt32(0), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.02") {
    val input =
      s"""def f01: Int8 = ${Byte.MaxValue}i8 / 1i8
         |def f02: Int8 = 12i8 / 3i8
         |def f03: Int8 = -12i8 / 3i8
         |def f04: Int8 = -3i8 / 12i8
         |def f05: Int8 = ${Byte.MinValue}i8 / -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(Byte.MaxValue), "f01")
    t.runTest(Value.mkInt8(4), "f02")
    t.runTest(Value.mkInt8(-4), "f03")
    t.runTest(Value.mkInt8(0), "f04")
    t.runTest(Value.mkInt8(Byte.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.03") {
    val input =
      s"""def f01: Int16 = ${Short.MaxValue}i16 / 1i16
         |def f02: Int16 = 12000i16 / 3i16
         |def f03: Int16 = -12000i16 / 3i16
         |def f04: Int16 = -3i16 / 12000i16
         |def f05: Int16 = ${Short.MinValue}i16 / -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(Short.MaxValue), "f01")
    t.runTest(Value.mkInt16(4000), "f02")
    t.runTest(Value.mkInt16(-4000), "f03")
    t.runTest(Value.mkInt16(0), "f04")
    t.runTest(Value.mkInt16(Short.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.04") {
    val input =
      s"""def f01: Int32 = ${Int.MaxValue}i32 / 1i32
         |def f02: Int32 = 1200000i32 / 3i32
         |def f03: Int32 = -1200000i32 / 3i32
         |def f04: Int32 = -3i32 / 1200000i32
         |def f05: Int32 = ${Int.MinValue}i32 / -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(Int.MaxValue), "f01")
    t.runTest(Value.mkInt32(400000), "f02")
    t.runTest(Value.mkInt32(-400000), "f03")
    t.runTest(Value.mkInt32(0), "f04")
    t.runTest(Value.mkInt32(Int.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.05") {
    val input =
      s"""def f01: Int64 = ${Long.MaxValue}i64 / 1i64
         |def f02: Int64 = 120000000000i64 / 3i64
         |def f03: Int64 = -120000000000i64 / 3i64
         |def f04: Int64 = -3i64 / 120000000000i64
         |def f05: Int64 = ${Long.MinValue}i64 / -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(Long.MaxValue), "f01")
    t.runTest(Value.mkInt64(40000000000L), "f02")
    t.runTest(Value.mkInt64(-40000000000L), "f03")
    t.runTest(Value.mkInt64(0), "f04")
    t.runTest(Value.mkInt64(Long.MinValue), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.06") {
    val input =
      s"""def f01: Float = 12.34 / 56.78
         |def f02: Float = 1234567890000000000000000000000000000000000000000.987654321 / 222.222
         |def f03: Float = -1234567890000000000000000000000000000000000000000.987654321 / 222.222
         |def f04: Float = 0.0000000000000000000000000000000000000000987654321 / 0.222
         |def f05: Float = -0.0000000000000000000000000000000000000000987654321 / 0.222
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.2173300457907714), "f01")
    t.runTest(Value.mkFloat64(5.5555610605610604E45), "f02")
    t.runTest(Value.mkFloat64(-5.5555610605610604E45), "f03")
    t.runTest(Value.mkFloat64(4.4488933378378374E-40), "f04")
    t.runTest(Value.mkFloat64(-4.4488933378378374E-40), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.07") {
    val input =
      s"""def f01: Float32 = 12.34f32 / 56.78f32
         |def f02: Float32 = 123456789000000000000000000000000000000.987654321f32 / 222.222f32
         |def f03: Float32 = -123456789000000000000000000000000000000.987654321f32 / 222.222f32
         |def f04: Float32 = 0.000000000000000000000000000000987654321f32 / 0.222f32
         |def f05: Float32 = -0.000000000000000000000000000000987654321f32 / 0.222f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(0.21733005f), "f01")
    t.runTest(Value.mkFloat32(5.5555608E35f), "f02")
    t.runTest(Value.mkFloat32(-5.5555608E35f), "f03")
    t.runTest(Value.mkFloat32(4.4488933E-30f), "f04")
    t.runTest(Value.mkFloat32(-4.4488933E-30f), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.08") {
    val input =
      s"""def f01: Float64 = 12.34f64 / 56.78f64
         |def f02: Float64 = 1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64
         |def f03: Float64 = -1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64
         |def f04: Float64 = 0.0000000000000000000000000000000000000000987654321f64 / 0.222f64
         |def f05: Float64 = -0.0000000000000000000000000000000000000000987654321f64 / 0.222f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(0.2173300457907714d), "f01")
    t.runTest(Value.mkFloat64(5.5555610605610604E45d), "f02")
    t.runTest(Value.mkFloat64(-5.5555610605610604E45d), "f03")
    t.runTest(Value.mkFloat64(4.4488933378378374E-40d), "f04")
    t.runTest(Value.mkFloat64(-4.4488933378378374E-40d), "f05")
  }

  test("Expression.Binary - BinaryOperator.Divide.09") {
    val input =
      s"""def f01: BigInt = 9223372036854775808ii / 1ii
         |def f02: BigInt = 1200000000000000000000ii / 3ii
         |def f03: BigInt = -1200000000000000000000ii / 3ii
         |def f04: BigInt = -3ii / 1200000000000000000000ii
         |def f05: BigInt = -9223372036854775809ii / -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("9223372036854775808"), "f01")
    t.runTest(Value.mkBigInt("400000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("-400000000000000000000"), "f03")
    t.runTest(Value.mkBigInt(0), "f04")
    t.runTest(Value.mkBigInt("9223372036854775809"), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.01") {
    val input =
      s"""def f01: Int = 1200000 % 200000
         |def f02: Int = 1200000 % 500000
         |def f03: Int = -1200000 % 500000
         |def f04: Int = 1200000 % -500000
         |def f05: Int = -1200000 % -500000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(200000), "f02")
    t.runTest(Value.mkInt32(-200000), "f03")
    t.runTest(Value.mkInt32(200000), "f04")
    t.runTest(Value.mkInt32(-200000), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.02") {
    val input =
      s"""def f01: Int8 = 12i8 % 2i8
         |def f02: Int8 = 12i8 % 5i8
         |def f03: Int8 = -12i8 % 5i8
         |def f04: Int8 = 12i8 % -5i8
         |def f05: Int8 = -12i8 % -5i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(0), "f01")
    t.runTest(Value.mkInt8(2), "f02")
    t.runTest(Value.mkInt8(-2), "f03")
    t.runTest(Value.mkInt8(2), "f04")
    t.runTest(Value.mkInt8(-2), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.03") {
    val input =
      s"""def f01: Int16 = 12000i16 % 2000i16
         |def f02: Int16 = 12000i16 % 5000i16
         |def f03: Int16 = -12000i16 % 5000i16
         |def f04: Int16 = 12000i16 % -5000i16
         |def f05: Int16 = -12000i16 % -5000i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(0), "f01")
    t.runTest(Value.mkInt16(2000), "f02")
    t.runTest(Value.mkInt16(-2000), "f03")
    t.runTest(Value.mkInt16(2000), "f04")
    t.runTest(Value.mkInt16(-2000), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.04") {
    val input =
      s"""def f01: Int32 = 1200000i32 % 200000i32
         |def f02: Int32 = 1200000i32 % 500000i32
         |def f03: Int32 = -1200000i32 % 500000i32
         |def f04: Int32 = 1200000i32 % -500000i32
         |def f05: Int32 = -1200000i32 % -500000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "f01")
    t.runTest(Value.mkInt32(200000), "f02")
    t.runTest(Value.mkInt32(-200000), "f03")
    t.runTest(Value.mkInt32(200000), "f04")
    t.runTest(Value.mkInt32(-200000), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.05") {
    val input =
      s"""def f01: Int64 = 120000000000i64 % 20000000000i64
         |def f02: Int64 = 120000000000i64 % 50000000000i64
         |def f03: Int64 = -120000000000i64 % 50000000000i64
         |def f04: Int64 = 120000000000i64 % -50000000000i64
         |def f05: Int64 = -120000000000i64 % -50000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0), "f01")
    t.runTest(Value.mkInt64(20000000000L), "f02")
    t.runTest(Value.mkInt64(-20000000000L), "f03")
    t.runTest(Value.mkInt64(20000000000L), "f04")
    t.runTest(Value.mkInt64(-20000000000L), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.06") {
    val input =
      s"""def f01: Float = 12.34 % 56.78
         |def f02: Float = 1234567890000000000000000000000000000000000000000.987654321 % 222.222
         |def f03: Float = -1234567890000000000000000000000000000000000000000.987654321 % 222.222
         |def f04: Float = 0.0000000000000000000000000000000000000000987654321 % 0.222
         |def f05: Float = -0.0000000000000000000000000000000000000000987654321 % 0.222
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(12.34), "f01")
    t.runTest(Value.mkFloat64(88.53722751835619), "f02")
    t.runTest(Value.mkFloat64(-88.53722751835619), "f03")
    t.runTest(Value.mkFloat64(9.87654321E-41), "f04")
    t.runTest(Value.mkFloat64(-9.87654321E-41), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.07") {
    val input =
      s"""def f01: Float32 = 12.34f32 % 56.78f32
         |def f02: Float32 = 123456789000000000000000000000000000000.987654321f32 % 222.222f32
         |def f03: Float32 = -123456789000000000000000000000000000000.987654321f32 % 222.222f32
         |def f04: Float32 = 0.000000000000000000000000000000987654321f32 % 0.222f32
         |def f05: Float32 = -0.000000000000000000000000000000987654321f32 % 0.222f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(12.34f), "f01")
    t.runTest(Value.mkFloat32(29.297333f), "f02")
    t.runTest(Value.mkFloat32(-29.297333f), "f03")
    t.runTest(Value.mkFloat32(9.876543E-31f), "f04")
    t.runTest(Value.mkFloat32(-9.876543E-31f), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.08") {
    val input =
      s"""def f01: Float64 = 12.34f64 % 56.78f64
         |def f02: Float64 = 1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64
         |def f03: Float64 = -1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64
         |def f04: Float64 = 0.0000000000000000000000000000000000000000987654321f64 % 0.222f64
         |def f05: Float64 = -0.0000000000000000000000000000000000000000987654321f64 % 0.222f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(12.34d), "f01")
    t.runTest(Value.mkFloat64(88.53722751835619d), "f02")
    t.runTest(Value.mkFloat64(-88.53722751835619d), "f03")
    t.runTest(Value.mkFloat64(9.87654321E-41d), "f04")
    t.runTest(Value.mkFloat64(-9.87654321E-41d), "f05")
  }

  test("Expression.Binary - BinaryOperator.Modulo.09") {
    val input =
      s"""def f01: BigInt = 1200000000000000000000ii % 200000000000000000000ii
         |def f02: BigInt = 1200000000000000000000ii % 500000000000000000000ii
         |def f03: BigInt = -1200000000000000000000ii % 500000000000000000000ii
         |def f04: BigInt = 1200000000000000000000ii % -500000000000000000000ii
         |def f05: BigInt = -1200000000000000000000ii % -500000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(0), "f01")
    t.runTest(Value.mkBigInt("200000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("-200000000000000000000"), "f03")
    t.runTest(Value.mkBigInt("200000000000000000000"), "f04")
    t.runTest(Value.mkBigInt("-200000000000000000000"), "f05")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.01") {
    val input =
      s"""def f01: Int = 2 ** 0
         |def f02: Int = -2 ** 1
         |def f03: Int = 2 ** 2
         |def f04: Int = -2 ** 31
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "f01")
    t.runTest(Value.mkInt32(-2), "f02")
    t.runTest(Value.mkInt32(4), "f03")
    t.runTest(Value.mkInt32(-2147483648), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.02") {
    val input =
      s"""def f01: Int8 = 2i8 ** 0i8
         |def f02: Int8 = -2i8 ** 1i8
         |def f03: Int8 = 2i8 ** 2i8
         |def f04: Int8 = -2i8 ** 7i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(1), "f01")
    t.runTest(Value.mkInt8(-2), "f02")
    t.runTest(Value.mkInt8(4), "f03")
    t.runTest(Value.mkInt8(-128), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.03") {
    val input =
      s"""def f01: Int16 = 2i16 ** 0i16
         |def f02: Int16 = -2i16 ** 1i16
         |def f03: Int16 = 2i16 ** 2i16
         |def f04: Int16 = -2i16 ** 15i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(1), "f01")
    t.runTest(Value.mkInt16(-2), "f02")
    t.runTest(Value.mkInt16(4), "f03")
    t.runTest(Value.mkInt16(-32768), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.04") {
    val input =
      s"""def f01: Int32 = 2i32 ** 0i32
         |def f02: Int32 = -2i32 ** 1i32
         |def f03: Int32 = 2i32 ** 2i32
         |def f04: Int32 = -2i32 ** 31i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "f01")
    t.runTest(Value.mkInt32(-2), "f02")
    t.runTest(Value.mkInt32(4), "f03")
    t.runTest(Value.mkInt32(-2147483648), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.05") {
    val input =
      s"""def f01: Int64 = 2i64 ** 0i64
         |def f02: Int64 = -2i64 ** 1i64
         |def f03: Int64 = 2i64 ** 2i64
         |def f04: Int64 = -2i64 ** 63i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(1L), "f01")
    t.runTest(Value.mkInt64(-2L), "f02")
    t.runTest(Value.mkInt64(4L), "f03")
    t.runTest(Value.mkInt64(-9223372036854775808L), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.06") {
    val input =
      s"""def f01: Float = 2.0 ** 0.0
         |def f02: Float = -2.0 ** -1.0
         |def f03: Float = 0.01 ** 0.5
         |def f04: Float = -2.0 ** 100.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(1.0d), "f01")
    t.runTest(Value.mkFloat64(-0.5d), "f02")
    t.runTest(Value.mkFloat64(0.1d), "f03")
    t.runTest(Value.mkFloat64(1.2676506002282294E30d), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.07") {
    val input =
      s"""def f01: Float32 = 2.0f32 ** 0.0f32
         |def f02: Float32 = -2.0f32 ** -1.0f32
         |def f03: Float32 = 0.01f32 ** 0.5f32
         |def f04: Float32 = -2.0f32 ** 100.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(1.0f), "f01")
    t.runTest(Value.mkFloat32(-0.5f), "f02")
    t.runTest(Value.mkFloat32(0.1f), "f03")
    t.runTest(Value.mkFloat32(1.2676506E30f), "f04")
  }

  test("Expression.Binary - BinaryOperator.Exponentiate.08") {
    val input =
      s"""def f01: Float64 = 2.0f64 ** 0.0f64
         |def f02: Float64 = -2.0f64 ** -1.0f64
         |def f03: Float64 = 0.01f64 ** 0.5f64
         |def f04: Float64 = -2.0f64 ** 100.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(1.0d), "f01")
    t.runTest(Value.mkFloat64(-0.5d), "f02")
    t.runTest(Value.mkFloat64(0.1d), "f03")
    t.runTest(Value.mkFloat64(1.2676506002282294E30d), "f04")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Comparison)                                          //
  // BinaryOperator.{Less,LessEqual,Greater,GreaterEqual}                    //
  // BinaryOperator.{Equal,NotEqual}                                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.Less.01") {
    val input =
      s"""def f01: Bool = 120000 < 30000
         |def f02: Bool = 30000 < 120000
         |def f03: Bool = 30000 < 30000
         |def f04: Bool = -120000 < -30000
         |def f05: Bool = -30000 < -120000
         |def f06: Bool = -30000 < -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.02") {
    val input =
      s"""def f01: Bool = 12i8 < 3i8
         |def f02: Bool = 3i8 < 12i8
         |def f03: Bool = 3i8 < 3i8
         |def f04: Bool = -12i8 < -3i8
         |def f05: Bool = -3i8 < -12i8
         |def f06: Bool = -3i8 < -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.03") {
    val input =
      s"""def f01: Bool = 1200i16 < 300i16
         |def f02: Bool = 300i16 < 1200i16
         |def f03: Bool = 300i16 < 300i16
         |def f04: Bool = -1200i16 < -300i16
         |def f05: Bool = -300i16 < -1200i16
         |def f06: Bool = -300i16 < -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.04") {
    val input =
      s"""def f01: Bool = 120000i32 < 30000i32
         |def f02: Bool = 30000i32 < 120000i32
         |def f03: Bool = 30000i32 < 30000i32
         |def f04: Bool = -120000i32 < -30000i32
         |def f05: Bool = -30000i32 < -120000i32
         |def f06: Bool = -30000i32 < -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 < 3000000000i64
         |def f02: Bool = 3000000000i64 < 12000000000i64
         |def f03: Bool = 3000000000i64 < 3000000000i64
         |def f04: Bool = -12000000000i64 < -3000000000i64
         |def f05: Bool = -3000000000i64 < -12000000000i64
         |def f06: Bool = -3000000000i64 < -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 < 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 < -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 < 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 < 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 < 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 < -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 < -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 < -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 < 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 < -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii < 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii < 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii < 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii < -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii < -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii < -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Less.10") {
    val input =
      s"""def f01: Bool = '${'十'}' < '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' < '${'十'}'
         |def f03: Bool = '${'\u0000'}' < '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.01") {
    val input =
      s"""def f01: Bool = 120000 <= 30000
         |def f02: Bool = 30000 <= 120000
         |def f03: Bool = 30000 <= 30000
         |def f04: Bool = -120000 <= -30000
         |def f05: Bool = -30000 <= -120000
         |def f06: Bool = -30000 <= -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.02") {
    val input =
      s"""def f01: Bool = 12i8 <= 3i8
         |def f02: Bool = 3i8 <= 12i8
         |def f03: Bool = 3i8 <= 3i8
         |def f04: Bool = -12i8 <= -3i8
         |def f05: Bool = -3i8 <= -12i8
         |def f06: Bool = -3i8 <= -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.03") {
    val input =
      s"""def f01: Bool = 1200i16 <= 300i16
         |def f02: Bool = 300i16 <= 1200i16
         |def f03: Bool = 300i16 <= 300i16
         |def f04: Bool = -1200i16 <= -300i16
         |def f05: Bool = -300i16 <= -1200i16
         |def f06: Bool = -300i16 <= -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.04") {
    val input =
      s"""def f01: Bool = 120000i32 <= 30000i32
         |def f02: Bool = 30000i32 <= 120000i32
         |def f03: Bool = 30000i32 <= 30000i32
         |def f04: Bool = -120000i32 <= -30000i32
         |def f05: Bool = -30000i32 <= -120000i32
         |def f06: Bool = -30000i32 <= -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 <= 3000000000i64
         |def f02: Bool = 3000000000i64 <= 12000000000i64
         |def f03: Bool = 3000000000i64 <= 3000000000i64
         |def f04: Bool = -12000000000i64 <= -3000000000i64
         |def f05: Bool = -3000000000i64 <= -12000000000i64
         |def f06: Bool = -3000000000i64 <= -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 <= 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 <= -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 <= 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 <= 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 <= 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 <= -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 <= -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 <= -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 <= 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 <= -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii <= 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii <= 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii <= 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii <= -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii <= -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii <= -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.LessEqual.10") {
    val input =
      s"""def f01: Bool = '${'十'}' <= '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' <= '${'十'}'
         |def f03: Bool = '${'\u0000'}' <= '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
  }

  test("Expression.Binary - BinaryOperator.Greater.01") {
    val input =
      s"""def f01: Bool = 120000 > 30000
         |def f02: Bool = 30000 > 120000
         |def f03: Bool = 30000 > 30000
         |def f04: Bool = -120000 > -30000
         |def f05: Bool = -30000 > -120000
         |def f06: Bool = -30000 > -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.02") {
    val input =
      s"""def f01: Bool = 12i8 > 3i8
         |def f02: Bool = 3i8 > 12i8
         |def f03: Bool = 3i8 > 3i8
         |def f04: Bool = -12i8 > -3i8
         |def f05: Bool = -3i8 > -12i8
         |def f06: Bool = -3i8 > -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.03") {
    val input =
      s"""def f01: Bool = 1200i16 > 300i16
         |def f02: Bool = 300i16 > 1200i16
         |def f03: Bool = 300i16 > 300i16
         |def f04: Bool = -1200i16 > -300i16
         |def f05: Bool = -300i16 > -1200i16
         |def f06: Bool = -300i16 > -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.04") {
    val input =
      s"""def f01: Bool = 120000i32 > 30000i32
         |def f02: Bool = 30000i32 > 120000i32
         |def f03: Bool = 30000i32 > 30000i32
         |def f04: Bool = -120000i32 > -30000i32
         |def f05: Bool = -30000i32 > -120000i32
         |def f06: Bool = -30000i32 > -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 > 3000000000i64
         |def f02: Bool = 3000000000i64 > 12000000000i64
         |def f03: Bool = 3000000000i64 > 3000000000i64
         |def f04: Bool = -12000000000i64 > -3000000000i64
         |def f05: Bool = -3000000000i64 > -12000000000i64
         |def f06: Bool = -3000000000i64 > -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 > 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 > -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 > 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 > 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 > 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 > -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 > -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 > -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 > 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 > -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii > 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii > 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii > 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii > -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii > -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii > -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Greater.10") {
    val input =
      s"""def f01: Bool = '${'十'}' > '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' > '${'十'}'
         |def f03: Bool = '${'\u0000'}' > '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.01") {
    val input =
      s"""def f01: Bool = 120000 >= 30000
         |def f02: Bool = 30000 >= 120000
         |def f03: Bool = 30000 >= 30000
         |def f04: Bool = -120000 >= -30000
         |def f05: Bool = -30000 >= -120000
         |def f06: Bool = -30000 >= -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.02") {
    val input =
      s"""def f01: Bool = 12i8 >= 3i8
         |def f02: Bool = 3i8 >= 12i8
         |def f03: Bool = 3i8 >= 3i8
         |def f04: Bool = -12i8 >= -3i8
         |def f05: Bool = -3i8 >= -12i8
         |def f06: Bool = -3i8 >= -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.03") {
    val input =
      s"""def f01: Bool = 1200i16 >= 300i16
         |def f02: Bool = 300i16 >= 1200i16
         |def f03: Bool = 300i16 >= 300i16
         |def f04: Bool = -1200i16 >= -300i16
         |def f05: Bool = -300i16 >= -1200i16
         |def f06: Bool = -300i16 >= -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.04") {
    val input =
      s"""def f01: Bool = 120000i32 >= 30000i32
         |def f02: Bool = 30000i32 >= 120000i32
         |def f03: Bool = 30000i32 >= 30000i32
         |def f04: Bool = -120000i32 >= -30000i32
         |def f05: Bool = -30000i32 >= -120000i32
         |def f06: Bool = -30000i32 >= -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 >= 3000000000i64
         |def f02: Bool = 3000000000i64 >= 12000000000i64
         |def f03: Bool = 3000000000i64 >= 3000000000i64
         |def f04: Bool = -12000000000i64 >= -3000000000i64
         |def f05: Bool = -3000000000i64 >= -12000000000i64
         |def f06: Bool = -3000000000i64 >= -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 >= 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 >= -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 >= 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 >= 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 >= 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 >= -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 >= -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 >= -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 >= 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 >= -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii >= 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii >= 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii >= 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii >= -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii >= -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii >= -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.10") {
    val input =
      s"""def f01: Bool = '${'十'}' >= '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' >= '${'十'}'
         |def f03: Bool = '${'\u0000'}' >= '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
  }

  test("Expression.Binary - BinaryOperator.Equal.01") {
    val input =
      s"""def f01: Bool = 120000 == 30000
         |def f02: Bool = 30000 == 120000
         |def f03: Bool = 30000 == 30000
         |def f04: Bool = -120000 == -30000
         |def f05: Bool = -30000 == -120000
         |def f06: Bool = -30000 == -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.02") {
    val input =
      s"""def f01: Bool = 12i8 == 3i8
         |def f02: Bool = 3i8 == 12i8
         |def f03: Bool = 3i8 == 3i8
         |def f04: Bool = -12i8 == -3i8
         |def f05: Bool = -3i8 == -12i8
         |def f06: Bool = -3i8 == -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.03") {
    val input =
      s"""def f01: Bool = 1200i16 == 300i16
         |def f02: Bool = 300i16 == 1200i16
         |def f03: Bool = 300i16 == 300i16
         |def f04: Bool = -1200i16 == -300i16
         |def f05: Bool = -300i16 == -1200i16
         |def f06: Bool = -300i16 == -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.04") {
    val input =
      s"""def f01: Bool = 120000i32 == 30000i32
         |def f02: Bool = 30000i32 == 120000i32
         |def f03: Bool = 30000i32 == 30000i32
         |def f04: Bool = -120000i32 == -30000i32
         |def f05: Bool = -30000i32 == -120000i32
         |def f06: Bool = -30000i32 == -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 == 3000000000i64
         |def f02: Bool = 3000000000i64 == 12000000000i64
         |def f03: Bool = 3000000000i64 == 3000000000i64
         |def f04: Bool = -12000000000i64 == -3000000000i64
         |def f05: Bool = -3000000000i64 == -12000000000i64
         |def f06: Bool = -3000000000i64 == -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 == 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 == -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 == 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 == 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 == 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 == -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 == -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 == -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 == 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 == -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii == 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii == 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii == 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii == -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii == -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii == -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.10") {
    val input =
      s"""def f01: Bool = '${'十'}' == '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' == '${'十'}'
         |def f03: Bool = '${'\u0000'}' == '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
  }

  test("Expression.Binary - BinaryOperator.Equal.11") {
    val input = "def f: Bool = () == ()"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Equal.12") {
    val input =
      """def f01: Bool = true == true
        |def f02: Bool = true == false
        |def f03: Bool = false == false
        |def f04: Bool = false == true
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
  }

  test("Expression.Binary - BinaryOperator.Equal.13") {
    val input =
      """def f01: Bool = "hello" == "hello"
        |def f02: Bool = "hello" == "hello!"
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
  }

  test("Expression.Binary - BinaryOperator.Equal.14") {
    val input =
      """enum T { case Top, case Val(Int), case Bot }
        |def f01: Bool = T.Top == T.Top
        |def f02: Bool = T.Top == T.Val(0)
        |def f03: Bool = T.Top == T.Bot
        |def f04: Bool = T.Val(0) == T.Bot
        |def f05: Bool = T.Val(0) == T.Val(0)
        |def f06: Bool = T.Val(1) == T.Val(2)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.False, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.Equal.15") {
    val foo = (1, 2) == (3, 'a')
    val input =
      """def f01: Bool = (1, 2, 3) == (1, 2, 3)
        |def f02: Bool = ('h', 'e', 'l', 'l', 'o') == ('h', 'e', 'l', 'l', 'o')
        |def f03: Bool = (1, 2, 'a') == (1, 2, 'b')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
  }

  test("Expression.Binary - BinaryOperator.Equal.16") {
    val input =
      """def f01: Bool = #{1, 2, 4} == #{4, 2, 1}
        |def f02: Bool = #{1, 2, 4} == #{0, 1, 2, 4}
        |def f03: Bool = #{true, true} == #{true, false}
        |def f04: Bool = #{'a', 'b', 'c'} == #{'c', 'c', 'b', 'b', 'a', 'a'}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.01") {
    val input =
      s"""def f01: Bool = 120000 != 30000
         |def f02: Bool = 30000 != 120000
         |def f03: Bool = 30000 != 30000
         |def f04: Bool = -120000 != -30000
         |def f05: Bool = -30000 != -120000
         |def f06: Bool = -30000 != -30000
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.02") {
    val input =
      s"""def f01: Bool = 12i8 != 3i8
         |def f02: Bool = 3i8 != 12i8
         |def f03: Bool = 3i8 != 3i8
         |def f04: Bool = -12i8 != -3i8
         |def f05: Bool = -3i8 != -12i8
         |def f06: Bool = -3i8 != -3i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.03") {
    val input =
      s"""def f01: Bool = 1200i16 != 300i16
         |def f02: Bool = 300i16 != 1200i16
         |def f03: Bool = 300i16 != 300i16
         |def f04: Bool = -1200i16 != -300i16
         |def f05: Bool = -300i16 != -1200i16
         |def f06: Bool = -300i16 != -300i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.04") {
    val input =
      s"""def f01: Bool = 120000i32 != 30000i32
         |def f02: Bool = 30000i32 != 120000i32
         |def f03: Bool = 30000i32 != 30000i32
         |def f04: Bool = -120000i32 != -30000i32
         |def f05: Bool = -30000i32 != -120000i32
         |def f06: Bool = -30000i32 != -30000i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.05") {
    val input =
      s"""def f01: Bool = 12000000000i64 != 3000000000i64
         |def f02: Bool = 3000000000i64 != 12000000000i64
         |def f03: Bool = 3000000000i64 != 3000000000i64
         |def f04: Bool = -12000000000i64 != -3000000000i64
         |def f05: Bool = -3000000000i64 != -12000000000i64
         |def f06: Bool = -3000000000i64 != -3000000000i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.06") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0
         |def f02: Bool = 30000000000000000000000000000000000000000.0 != 120000000000000000000000000000000000000000.0
         |def f03: Bool = 30000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0
         |def f04: Bool = -120000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0
         |def f05: Bool = -30000000000000000000000000000000000000000.0 != -120000000000000000000000000000000000000000.0
         |def f06: Bool = -30000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.07") {
    val input =
      s"""def f01: Bool = 1200000000000000000000.0f32 != 300000000000000000000.0f32
         |def f02: Bool = 300000000000000000000.0f32 != 1200000000000000000000.0f32
         |def f03: Bool = 300000000000000000000.0f32 != 300000000000000000000.0f32
         |def f04: Bool = -1200000000000000000000.0f32 != -300000000000000000000.0f32
         |def f05: Bool = -300000000000000000000.0f32 != -1200000000000000000000.0f32
         |def f06: Bool = -300000000000000000000.0f32 != -300000000000000000000.0f32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.08") {
    val input =
      s"""def f01: Bool = 120000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64
         |def f02: Bool = 30000000000000000000000000000000000000000.0f64 != 120000000000000000000000000000000000000000.0f64
         |def f03: Bool = 30000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64
         |def f04: Bool = -120000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64
         |def f05: Bool = -30000000000000000000000000000000000000000.0f64 != -120000000000000000000000000000000000000000.0f64
         |def f06: Bool = -30000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.09") {
    val input =
      s"""def f01: Bool = 12000000000000000000ii != 3000000000000000000ii
         |def f02: Bool = 3000000000000000000ii != 12000000000000000000ii
         |def f03: Bool = 3000000000000000000ii != 3000000000000000000ii
         |def f04: Bool = -12000000000000000000ii != -3000000000000000000ii
         |def f05: Bool = -3000000000000000000ii != -12000000000000000000ii
         |def f06: Bool = -3000000000000000000ii != -3000000000000000000ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.True, "f05")
    t.runTest(Value.False, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.10") {
    val input =
      s"""def f01: Bool = '${'十'}' != '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' != '${'十'}'
         |def f03: Bool = '${'\u0000'}' != '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.11") {
    val input = "def f: Bool = () != ()"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.12") {
    val input =
      """def f01: Bool = true != true
        |def f02: Bool = true != false
        |def f03: Bool = false != false
        |def f04: Bool = false != true
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
    t.runTest(Value.True, "f04")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.13") {
    val input =
      """def f01: Bool = "hello" != "hello"
        |def f02: Bool = "hello" != "hello!"
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.14") {
    val input =
      """enum T { case Top, case Val(Int), case Bot }
        |def f01: Bool = T.Top != T.Top
        |def f02: Bool = T.Top != T.Val(0)
        |def f03: Bool = T.Top != T.Bot
        |def f04: Bool = T.Val(0) != T.Bot
        |def f05: Bool = T.Val(0) != T.Val(0)
        |def f06: Bool = T.Val(1) != T.Val(2)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.True, "f04")
    t.runTest(Value.False, "f05")
    t.runTest(Value.True, "f06")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.15") {
    val foo = (1, 2) == (3, 'a')
    val input =
      """def f01: Bool = (1, 2, 3) != (1, 2, 3)
        |def f02: Bool = ('h', 'e', 'l', 'l', 'o') != ('h', 'e', 'l', 'l', 'o')
        |def f03: Bool = (1, 2, 'a') != (1, 2, 'b')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.16") {
    val input =
      """def f01: Bool = #{1, 2, 4} != #{4, 2, 1}
        |def f02: Bool = #{1, 2, 4} != #{0, 1, 2, 4}
        |def f03: Bool = #{true, true} != #{true, false}
        |def f04: Bool = #{'a', 'b', 'c'} != #{'c', 'c', 'b', 'b', 'a', 'a'}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.True, "f03")
    t.runTest(Value.False, "f04")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Logical)                                             //
  // BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.LogicalAnd.01") {
    val input = "def f: Bool = true && true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.02") {
    val input = "def f: Bool = true && false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.03") {
    val input = "def f: Bool = false && false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.04") {
    val input = "def f: Bool = false && true"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.05") {
    val input = "def f: Bool = false && ???"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.06") {
    val input = "def f: Bool = true && ???"
    val t = new Tester(input)
    t.runInterceptTest[UserException]("f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.01") {
    val input = "def f: Bool = true || true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.02") {
    val input = "def f: Bool = true || false"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.03") {
    val input = "def f: Bool = false || false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.04") {
    val input = "def f: Bool = false || true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.05") {
    val input = "def f: Bool = true || ???"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.06") {
    val input = "def f: Bool = false || ???"
    val t = new Tester(input)
    t.runInterceptTest[UserException]("f")
  }

  test("Expression.Binary - BinaryOperator.Implication.01") {
    val input = "def f: Bool = true ==> true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Implication.02") {
    val input = "def f: Bool = true ==> false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.Implication.03") {
    val input = "def f: Bool = false ==> false"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Implication.04") {
    val input = "def f: Bool = false ==> true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Implication.05") {
    val input = "def f: Bool = false ==> ???"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Implication.06") {
    val input = "def f: Bool = true ==> ???"
    val t = new Tester(input)
    t.runInterceptTest[UserException]("f")
  }

  test("Expression.Binary - BinaryOperator.Biconditional.01") {
    val input = "def f: Bool = true <==> true"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Biconditional.02") {
    val input = "def f: Bool = true <==> false"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.Biconditional.03") {
    val input = "def f: Bool = false <==> false"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Biconditional.04") {
    val input = "def f: Bool = false <==> true"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Bitwise)                                             //
  // BinaryOperator.{BitwiseAnd,BitwiseOr,BitwiseXor}                        //
  // BinaryOperator.{BitwiseLeftShift,BitwiseRightShift}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.BitwiseAnd.01") {
    val input =
      s"""def f01: Int = 40000 & ${0xFFFFFFFF}
         |def f02: Int = 40000 & 40000
         |def f03: Int = 40000 & 0
         |def f04: Int = ${0xFFFFFFFF} & ${0xFFFFFFFF}
         |def f05: Int = -1 & -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(40000), "f01")
    t.runTest(Value.mkInt32(40000), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f04")
    t.runTest(Value.mkInt32(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.02") {
    val input =
      s"""def f01: Int8 = 40i8 & ${0xFF.toByte}i8
         |def f02: Int8 = 40i8 & 40i8
         |def f03: Int8 = 40i8 & 0i8
         |def f04: Int8 = ${0xFF.toByte}i8 & ${0xFF.toByte}i8
         |def f05: Int8 = -1i8 & -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(40), "f01")
    t.runTest(Value.mkInt8(40), "f02")
    t.runTest(Value.mkInt8(0), "f03")
    t.runTest(Value.mkInt8(0xFF.toByte), "f04")
    t.runTest(Value.mkInt8(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.03") {
    val input =
      s"""def f01: Int16 = 400i16 & ${0xFFFF.toShort}i16
         |def f02: Int16 = 400i16 & 400i16
         |def f03: Int16 = 400i16 & 0i16
         |def f04: Int16 = ${0xFFFF.toShort}i16 & ${0xFFFF.toShort}i16
         |def f05: Int16 = -1i16 & -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(400), "f01")
    t.runTest(Value.mkInt16(400), "f02")
    t.runTest(Value.mkInt16(0), "f03")
    t.runTest(Value.mkInt16(0xFFFF.toShort), "f04")
    t.runTest(Value.mkInt16(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.04") {
    val input =
      s"""def f01: Int32 = 40000i32 & ${0xFFFFFFFF}i32
         |def f02: Int32 = 40000i32 & 40000i32
         |def f03: Int32 = 40000i32 & 0i32
         |def f04: Int32 = ${0xFFFFFFFF}i32 & ${0xFFFFFFFF}i32
         |def f05: Int32 = -1i32 & -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(40000), "f01")
    t.runTest(Value.mkInt32(40000), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f04")
    t.runTest(Value.mkInt32(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.05") {
    val input =
      s"""def f01: Int64 = 40000000000i64 & ${0xFFFFFFFFFFFFFFFFL}i64
         |def f02: Int64 = 40000000000i64 & 40000000000i64
         |def f03: Int64 = 40000000000i64 & 0i64
         |def f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 & ${0xFFFFFFFFFFFFFFFFL}i64
         |def f05: Int64 = -1i64 & -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(40000000000L), "f01")
    t.runTest(Value.mkInt64(40000000000L), "f02")
    t.runTest(Value.mkInt64(0), "f03")
    t.runTest(Value.mkInt64(0xFFFFFFFFFFFFFFFFL), "f04")
    t.runTest(Value.mkInt64(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.06") {
    val input =
      s"""def f01: BigInt = 40000000000000000000ii & ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f02: BigInt = 40000000000000000000ii & 40000000000000000000ii
         |def f03: BigInt = 40000000000000000000ii & 0ii
         |def f04: BigInt = ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii & ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f05: BigInt = -1ii & -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("40000000000000000000"), "f01")
    t.runTest(Value.mkBigInt("40000000000000000000"), "f02")
    t.runTest(Value.mkBigInt(0), "f03")
    t.runTest(Value.mkBigInt(new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)), "f04")
    t.runTest(Value.mkBigInt(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.01") {
    val input =
      s"""def f01: Int = 40000 | ${0xFFFFFFFF}
         |def f02: Int = 40000 | 40000
         |def f03: Int = 40000 | 0
         |def f04: Int = ${0xFFFFFFFF} | ${0xFFFFFFFF}
         |def f05: Int = -1 | -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f01")
    t.runTest(Value.mkInt32(40000), "f02")
    t.runTest(Value.mkInt32(40000), "f03")
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f04")
    t.runTest(Value.mkInt32(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.02") {
    val input =
      s"""def f01: Int8 = 40i8 | ${0xFF.toByte}i8
         |def f02: Int8 = 40i8 | 40i8
         |def f03: Int8 = 40i8 | 0i8
         |def f04: Int8 = ${0xFF.toByte}i8 | ${0xFF.toByte}i8
         |def f05: Int8 = -1i8 | -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(0xFF.toByte), "f01")
    t.runTest(Value.mkInt8(40), "f02")
    t.runTest(Value.mkInt8(40), "f03")
    t.runTest(Value.mkInt8(0xFF.toByte), "f04")
    t.runTest(Value.mkInt8(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.03") {
    val input =
      s"""def f01: Int16 = 400i16 | ${0xFFFF.toShort}i16
         |def f02: Int16 = 400i16 | 400i16
         |def f03: Int16 = 400i16 | 0i16
         |def f04: Int16 = ${0xFFFF.toShort}i16 | ${0xFFFF.toShort}i16
         |def f05: Int16 = -1i16 | -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(0xFFFF.toShort), "f01")
    t.runTest(Value.mkInt16(400), "f02")
    t.runTest(Value.mkInt16(400), "f03")
    t.runTest(Value.mkInt16(0xFF.toByte), "f04")
    t.runTest(Value.mkInt16(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.04") {
    val input =
      s"""def f01: Int32 = 40000i32 | ${0xFFFFFFFF}i32
         |def f02: Int32 = 40000i32 | 40000i32
         |def f03: Int32 = 40000i32 | 0i32
         |def f04: Int32 = ${0xFFFFFFFF}i32 | ${0xFFFFFFFF}i32
         |def f05: Int32 = -1i32 | -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f01")
    t.runTest(Value.mkInt32(40000), "f02")
    t.runTest(Value.mkInt32(40000), "f03")
    t.runTest(Value.mkInt32(0xFFFFFFFF), "f04")
    t.runTest(Value.mkInt32(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.05") {
    val input =
      s"""def f01: Int64 = 40000000000i64 | ${0xFFFFFFFFFFFFFFFFL}i64
         |def f02: Int64 = 40000000000i64 | 40000000000i64
         |def f03: Int64 = 40000000000i64 | 0i64
         |def f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 | ${0xFFFFFFFFFFFFFFFFL}i64
         |def f05: Int64 = -1i64 | -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0xFFFFFFFFFFFFFFFFL), "f01")
    t.runTest(Value.mkInt64(40000000000L), "f02")
    t.runTest(Value.mkInt64(40000000000L), "f03")
    t.runTest(Value.mkInt64(0xFFFFFFFFFFFFFFFFL), "f04")
    t.runTest(Value.mkInt64(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.06") {
    val input =
      s"""def f01: BigInt = 40000000000000000000ii | ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f02: BigInt = 40000000000000000000ii | 40000000000000000000ii
         |def f03: BigInt = 40000000000000000000ii | 0ii
         |def f04: BigInt = ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii | ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f05: BigInt = -1ii | -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)), "f01")
    t.runTest(Value.mkBigInt("40000000000000000000"), "f02")
    t.runTest(Value.mkBigInt("40000000000000000000"), "f03")
    t.runTest(Value.mkBigInt(new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)), "f04")
    t.runTest(Value.mkBigInt(-1), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.01") {
    val input =
      s"""def f01: Int = 40000 ^ ${0xFFFFFFFF}
         |def f02: Int = 40000 ^ 40000
         |def f03: Int = 40000 ^ 0
         |def f04: Int = ${0xFFFFFFFF} ^ ${0xFFFFFFFF}
         |def f05: Int = -1 ^ -1
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-40001), "f01")
    t.runTest(Value.mkInt32(0), "f02")
    t.runTest(Value.mkInt32(40000), "f03")
    t.runTest(Value.mkInt32(0), "f04")
    t.runTest(Value.mkInt32(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.02") {
    val input =
      s"""def f01: Int8 = 40i8 ^ ${0xFF.toByte}i8
         |def f02: Int8 = 40i8 ^ 40i8
         |def f03: Int8 = 40i8 ^ 0i8
         |def f04: Int8 = ${0xFF.toByte}i8 ^ ${0xFF.toByte}i8
         |def f05: Int8 = -1i8 ^ -1i8
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(-41), "f01")
    t.runTest(Value.mkInt8(0), "f02")
    t.runTest(Value.mkInt8(40), "f03")
    t.runTest(Value.mkInt8(0), "f04")
    t.runTest(Value.mkInt8(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.03") {
    val input =
      s"""def f01: Int16 = 400i16 ^ ${0xFFFF.toShort}i16
         |def f02: Int16 = 400i16 ^ 400i16
         |def f03: Int16 = 400i16 ^ 0i16
         |def f04: Int16 = ${0xFFFF.toShort}i16 ^ ${0xFFFF.toShort}i16
         |def f05: Int16 = -1i16 ^ -1i16
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(-401), "f01")
    t.runTest(Value.mkInt16(0), "f02")
    t.runTest(Value.mkInt16(400), "f03")
    t.runTest(Value.mkInt16(0), "f04")
    t.runTest(Value.mkInt16(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.04") {
    val input =
      s"""def f01: Int32 = 40000i32 ^ ${0xFFFFFFFF}i32
         |def f02: Int32 = 40000i32 ^ 40000i32
         |def f03: Int32 = 40000i32 ^ 0i32
         |def f04: Int32 = ${0xFFFFFFFF}i32 ^ ${0xFFFFFFFF}i32
         |def f05: Int32 = -1i32 ^ -1i32
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-40001), "f01")
    t.runTest(Value.mkInt32(0), "f02")
    t.runTest(Value.mkInt32(40000), "f03")
    t.runTest(Value.mkInt32(0), "f04")
    t.runTest(Value.mkInt32(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.05") {
    val input =
      s"""def f01: Int64 = 40000000000i64 ^ ${0xFFFFFFFFFFFFFFFFL}i64
         |def f02: Int64 = 40000000000i64 ^ 40000000000i64
         |def f03: Int64 = 40000000000i64 ^ 0i64
         |def f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 ^ ${0xFFFFFFFFFFFFFFFFL}i64
         |def f05: Int64 = -1i64 ^ -1i64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-40000000001L), "f01")
    t.runTest(Value.mkInt64(0), "f02")
    t.runTest(Value.mkInt64(40000000000L), "f03")
    t.runTest(Value.mkInt64(0), "f04")
    t.runTest(Value.mkInt64(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.06") {
    val input =
      s"""def f01: BigInt = 40000000000000000000ii ^ ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f02: BigInt = 40000000000000000000ii ^ 40000000000000000000ii
         |def f03: BigInt = 40000000000000000000ii ^ 0ii
         |def f04: BigInt = ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii ^ ${new java.math.BigInteger("FFFFFFFFFFFFFFFFF", 16)}ii
         |def f05: BigInt = -1ii ^ -1ii
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("255147905179352825855"), "f01")
    t.runTest(Value.mkBigInt(0), "f02")
    t.runTest(Value.mkBigInt("40000000000000000000"), "f03")
    t.runTest(Value.mkBigInt(0), "f04")
    t.runTest(Value.mkBigInt(0), "f05")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.01") {
    val input =
      s"""def f01: Int = ${0x08} << 0
         |def f02: Int = ${0x08} << 16
         |def f03: Int = ${0x08} << 28
         |def f04: Int = ${0x08} << 29
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0x08), "f01")
    t.runTest(Value.mkInt32(0x00080000), "f02")
    t.runTest(Value.mkInt32(Int.MinValue), "f03")
    t.runTest(Value.mkInt32(0), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.02") {
    val input =
      s"""def f01: Int8 = ${0x08}i8 << 0
         |def f02: Int8 = ${0x08}i8 << 2
         |def f03: Int8 = ${0x08}i8 << 4
         |def f04: Int8 = ${0x08}i8 << 5
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(0x08), "f01")
    t.runTest(Value.mkInt8(0x20), "f02")
    t.runTest(Value.mkInt8(Byte.MinValue), "f03")
    t.runTest(Value.mkInt8(0), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.03") {
    val input =
      s"""def f01: Int16 = ${0x08}i16 << 0
         |def f02: Int16 = ${0x08}i16 << 8
         |def f03: Int16 = ${0x08}i16 << 12
         |def f04: Int16 = ${0x08}i16 << 13
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(0x08), "f01")
    t.runTest(Value.mkInt16(0x0800), "f02")
    t.runTest(Value.mkInt16(Short.MinValue), "f03")
    t.runTest(Value.mkInt16(0), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.04") {
    val input =
      s"""def f01: Int32 = ${0x08}i32 << 0
         |def f02: Int32 = ${0x08}i32 << 16
         |def f03: Int32 = ${0x08}i32 << 28
         |def f04: Int32 = ${0x08}i32 << 29
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0x08), "f01")
    t.runTest(Value.mkInt32(0x00080000), "f02")
    t.runTest(Value.mkInt32(Int.MinValue), "f03")
    t.runTest(Value.mkInt32(0), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.05") {
    val input =
      s"""def f01: Int64 = ${0x08}i64 << 0
         |def f02: Int64 = ${0x08}i64 << 32
         |def f03: Int64 = ${0x08}i64 << 60
         |def f04: Int64 = ${0x08}i64 << 61
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0x08), "f01")
    t.runTest(Value.mkInt64(0x0000000800000000L), "f02")
    t.runTest(Value.mkInt64(Long.MinValue), "f03")
    t.runTest(Value.mkInt64(0), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.06") {
    val input =
      s"""def f01: BigInt = ${0x08}ii << 0
         |def f02: BigInt = ${0x08}ii << 32
         |def f03: BigInt = ${0x08}ii << 64
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt(0x08), "f01")
    t.runTest(Value.mkBigInt(0x0000000800000000L), "f02")
    t.runTest(Value.mkBigInt(new java.math.BigInteger("80000000000000000", 16)), "f03")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.01") {
    val input =
      s"""def f01: Int = 120000 >> 0
         |def f02: Int = 120000 >> 2
         |def f03: Int = 120000 >> 31
         |def f04: Int = -120000 >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(120000), "f01")
    t.runTest(Value.mkInt32(30000), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(-30000), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.02") {
    val input =
      s"""def f01: Int8 = 120i8 >> 0
         |def f02: Int8 = 120i8 >> 2
         |def f03: Int8 = 120i8 >> 7
         |def f04: Int8 = -120i8 >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(120), "f01")
    t.runTest(Value.mkInt8(30), "f02")
    t.runTest(Value.mkInt8(0), "f03")
    t.runTest(Value.mkInt8(-30), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.03") {
    val input =
      s"""def f01: Int16 = 12000i16 >> 0
         |def f02: Int16 = 12000i16 >> 2
         |def f03: Int16 = 12000i16 >> 15
         |def f04: Int16 = -12000i16 >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(12000), "f01")
    t.runTest(Value.mkInt16(3000), "f02")
    t.runTest(Value.mkInt16(0), "f03")
    t.runTest(Value.mkInt16(-3000), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.04") {
    val input =
      s"""def f01: Int32 = 120000i32 >> 0
         |def f02: Int32 = 120000i32 >> 2
         |def f03: Int32 = 120000i32 >> 31
         |def f04: Int32 = -120000i32 >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(120000), "f01")
    t.runTest(Value.mkInt32(30000), "f02")
    t.runTest(Value.mkInt32(0), "f03")
    t.runTest(Value.mkInt32(-30000), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.05") {
    val input =
      s"""def f01: Int64 = 12000000000i64 >> 0
         |def f02: Int64 = 12000000000i64 >> 2
         |def f03: Int64 = 12000000000i64 >> 63
         |def f04: Int64 = -12000000000i64 >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(12000000000L), "f01")
    t.runTest(Value.mkInt64(3000000000L), "f02")
    t.runTest(Value.mkInt64(0), "f03")
    t.runTest(Value.mkInt64(-3000000000L), "f04")
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.06") {
    val input =
      s"""def f01: BigInt = 1200000000000000000000ii >> 0
         |def f02: BigInt = 1200000000000000000000ii >> 2
         |def f03: BigInt = 1200000000000000000000ii >> 72
         |def f04: BigInt = -1200000000000000000000ii >> 2
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("1200000000000000000000"), "f01")
    t.runTest(Value.mkBigInt("300000000000000000000"), "f02")
    t.runTest(Value.mkBigInt(0), "f03")
    t.runTest(Value.mkBigInt("-300000000000000000000"), "f04")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.IfThenElse                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.IfThenElse.01") {
    val input = "def f: Int = if (false) 42 + 10 else 42 - 10"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(32), "f")
  }

  test("Expression.IfThenElse.02") {
    val input = "def f: Int = if (true) 42 + 10 else 42 - 10"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(52), "f")
  }

  test("Expression.IfThenElse.03") {
    val input =
      """def f(x: Bool): Int = if (x) (if (false) 1 else 2) else (if (true) 3 else 4)
        |def g01: Int = f(true)
        |def g02: Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(2), "g01")
    t.runTest(Value.mkInt32(3), "g02")
  }

  test("Expression.IfThenElse.04") {
    val input =
      """def f(x: Bool): Int = if (if (!x) true else false) 1234 else 5678
        |def g01: Int = f(true)
        |def g02: Int = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(5678), "g01")
    t.runTest(Value.mkInt32(1234), "g02")
  }

  test("Expression.IfThenElse.05") {
    val input =
      """def f(x: Bool, y: Bool): Int = if (x && y) 1234 else 5678
        |def g01: Int = f(true, true)
        |def g02: Int = f(false, true)
        |def g03: Int = f(true, false)
        |def g04: Int = f(false, false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1234), "g01")
    t.runTest(Value.mkInt32(5678), "g02")
    t.runTest(Value.mkInt32(5678), "g03")
    t.runTest(Value.mkInt32(5678), "g04")
  }

  test("Expression.IfThenElse.06") {
    val input =
      """def f(x: Bool, y: Bool): Int = if (x || y) 1234 else 5678
        |def g01: Int = f(true, true)
        |def g02: Int = f(false, true)
        |def g03: Int = f(true, false)
        |def g04: Int = f(false, false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1234), "g01")
    t.runTest(Value.mkInt32(1234), "g02")
    t.runTest(Value.mkInt32(1234), "g03")
    t.runTest(Value.mkInt32(5678), "g04")
  }

  test("Expression.IfThenElse.07") {
    val input =
      """def f(x: Int8, y: Int8): Int8 = if (x < y) 12i8 else 56i8
        |def g01: Int8 = f(5i8, 24i8)
        |def g02: Int8 = f(5i8, 5i8)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(12), "g01")
    t.runTest(Value.mkInt8(56), "g02")
  }

  test("Expression.IfThenElse.08") {
    val input =
      """def f(x: Int16, y: Int16): Int16 = if (x <= y) 1234i16 else 5678i16
        |def g01: Int16 = f(500i16, 500i16)
        |def g02: Int16 = f(500i16, 200i16)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(1234), "g01")
    t.runTest(Value.mkInt16(5678), "g02")
  }

  test("Expression.IfThenElse.09") {
    val input =
      """def f(x: Int32, y: Int32): Int32 = if (x > y) 12341234i32 else 56785678i32
        |def g01: Int32 = f(2400000i32, 500000i32)
        |def g02: Int32 = f(500000i32, 500000i32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(12341234), "g01")
    t.runTest(Value.mkInt32(56785678), "g02")
  }

  test("Expression.IfThenElse.10") {
    val input =
      """def f(x: Int64, y: Int64): Int64 = if (x >= y) 123412341234i64 else 567856785678i64
        |def g01: Int64 = f(50000000000i64, 50000000000i64)
        |def g02: Int64 = f(20000000000i64, 50000000000i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(123412341234L), "g01")
    t.runTest(Value.mkInt64(567856785678L), "g02")
  }

  test("Expression.IfThenElse.11") {
    val input =
      """def f(x: Int, y: Int): Int = if (x == y) 1234 else 5678
        |def g01: Int = f(5, 5)
        |def g02: Int = f(2, 5)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1234), "g01")
    t.runTest(Value.mkInt32(5678), "g02")
  }

  test("Expression.IfThenElse.12") {
    val input =
      """def f(x: Int, y: Int): Int = if (x != y) 1234 else 5678
        |def g01: Int = f(2, 5)
        |def g02: Int = f(5, 5)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1234), "g01")
    t.runTest(Value.mkInt32(5678), "g02")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Let                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Let.01") {
    val input = "def f: Int = let x = true in 42"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "f")
  }

  test("Expression.Let.02") {
    val input = "def f: Int8 = let x = 42i8 in x"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(42), "f")
  }

  test("Expression.Let.03") {
    val input = "def f: Int16 = let x = 1i16 in x + 2i16"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(3), "f")
  }

  test("Expression.Let.04") {
    val input = """def f: Str = let x = false in if (x) "abz" else "xyz""""
    val t = new Tester(input)
    t.runTest(Value.mkStr("xyz"), "f")
  }

  test("Expression.Let.05") {
    val input = "def f: Int = let x = 14 - 3 in x + 2"
    val t = new Tester(input)
    t.runTest(Value.mkInt32(13), "f")
  }

  test("Expression.Let.06") {
    val input =
      """def f: Int =
        |  let x = 14 - 3 in
        |    let y = 2 * 4 in
        |      x + y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(19), "f")
  }

  test("Expression.Let.07") {
    val input =
      """def f: Int =
        |  let x = 1 in
        |    let y = x + 2 in
        |      let z = y + 3 in
        |        z
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(6), "f")
  }

  test("Expression.Let.08") {
    val input =
      """def f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337 in
        |    let y = -101010 in
        |      let z = 42 in
        |        y
        |def g: Int = f(-1337, 101010, -42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(-101010), "g")
  }

  test("Expression.Let.09") {
    val input =
      """def f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337 in
        |    let y = -101010 in
        |      let z = 42 in
        |        b
        |def g: Int = f(-1337, 101010, -42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(101010), "g")
  }

  test("Expression.Let.10") {
    val input = "def f: Int64 = let x = 0i64 in x"
    val t = new Tester(input)
    t.runTest(Value.mkInt64(0), "f")
  }

  test("Expression.Let.11") {
    val input =
      """def f: Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "f")
  }

  test("Expression.Let.12") {
    val input =
      """def f: Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "f")
  }

  test("Expression.Let.13") {
    val input =
      """def f(a: Int64, b: Int64, c: Int64): Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
        |def g: Int64 = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "g")
  }

  test("Expression.Let.14") {
    val input =
      """def f(a: Int32, b: Int64, c: Int64): Int64 =
        |  let x = 1337i32 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
        |def g: Int64 = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(-101010), "g")
  }

  test("Expression.Let.15") {
    val input =
      """def f(a: Int64, b: Int64, c: Int64): Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        b
        |def g: Int64 = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(101010), "g")
  }

  test("Expression.Let.16") {
    val input =
      """def f(a: Int32, b: Int64, c: Int64): Int64 =
        |  let x = 1337i32 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        b
        |def g: Int64 = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(101010), "g")
  }

  test("Expression.Let.17") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f: ConstProp = let x = ConstProp.Val(42) in x
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(42)), "f")
  }

  test("Expression.Let.18") {
    val input = "def f: () = let x = () in x"
    val t = new Tester(input)
    t.runTest(Value.Unit, "f")
  }

  test("Expression.Let.19") {
    val input = """def f: Str = let x = "helloworld" in x"""
    val t = new Tester(input)
    t.runTest(Value.mkStr("helloworld"), "f")
  }

  test("Expression.Let.20") {
    val input = "def f: (Int, Int) = let x = (123, 456) in x"
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(123, 456).map(Value.mkInt32)), "f")
  }

  test("Expression.Let.21") {
    val input = "def f: Set[Int] = let x = #{9, 99, 999} in x"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(9, 99, 999).map(Value.mkInt32)), "f")
  }

  test("Expression.Let.22") {
    val input =
      """def f: Char =
        |  let x = 'a' in
        |    let y = 'b' in
        |      y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkChar('b'), "f")
  }

  test("Expression.Let.23") {
    val input =
      """def f: Float32 =
        |  let x = 1.2f32 in
        |    let y = 3.4f32 in
        |      y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(3.4f), "f")
  }

  test("Expression.Let.24") {
    val input =
      """def f: Float64 =
        |  let x = 1.2f64 in
        |    let y = 3.4f64 in
        |      y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(3.4d), "f")
  }

  test("Expression.Let.25") {
    val input =
      """def f(x: Int): Int32 =
        |  let x = x + 1 in
        |    let x = x + 2 in
        |      x + 3
        |def g: Int = f(0)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(6), "g")
  }

  test("Expression.Let.26") {
    val input =
      """def f(x: Int): Int64 =
        |  let x = x + 1 in
        |    let x = 40i64 in
        |      let x = x + 2i64 in
        |        x
        |def g: Int64 = f(0)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(42), "g")
  }

  test("Expression.Let.27") {
    val input =
      """def f: BigInt =
        |  let x = 12345678901234567890ii in
        |    let y = 98765432109876543210ii in
        |      y
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("98765432109876543210"), "f")
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
        |def f: ConstProp = ConstProp.Top
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Top", Value.Unit), "f")
  }

  test("Expression.Tag.02") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f: ConstProp = ConstProp.Val(42)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(42)), "f")
  }

  test("Expression.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f: ConstProp = ConstProp.Bot
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Bot", Value.Unit), "f")
  }

  test("Expression.Tag.04") {
    val input =
      """enum Val { case Val(Bool) }
        |def f: Val = Val.Val(true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.True), "f")
  }

  test("Expression.Tag.05") {
    val input =
      """enum Val { case Val(Bool) }
        |def f(x: Bool): Val = Val.Val(x)
        |def g01: Val = f(true)
        |def g02: Val = f(false)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.True), "g01")
    t.runTest(Value.mkTag("Val", Value.False), "g02")
  }

  test("Expression.Tag.06") {
    val input =
      """enum Val { case Val(Str) }
        |def f: Val = Val.Val("hi")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkStr("hi")), "f")
  }

  test("Expression.Tag.07") {
    val input =
      """enum Val { case Val(Int, Str) }
        |def f: Val = Val.Val(1, "one")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.Tuple(Array(Value.mkInt32(1), "one"))), "f")
  }

  test("Expression.Tag.08") {
    val input =
      """enum Val { case Val(Str) }
        |def f: Val = Val.Val(if (!(4 != 4)) "foo" else "bar")
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkStr("foo")), "f")
  }

  test("Expression.Tag.09") {
    val input =
      """enum Val { case Val(Str, Int) }
        |def f: Val = Val.Val("ABC", 20 + 22)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.Tuple(Array("ABC", Value.mkInt32(42)))), "f")
  }

  test("Expression.Tag.10") {
    val input =
      """enum Val { case Val((Str, Int)) }
        |def f: Val = Val.Val(("ABC", 20 + 22))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.Tuple(Array("ABC", Value.mkInt32(42)))), "f")
  }

  test("Expression.Tag.11") {
    val input =
      """enum Val { case Val(Int8) }
        |def f: Val = Val.Val(32i8)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt8(32)), "f")
  }

  test("Expression.Tag.12") {
    val input =
      """enum Val { case Val(Int16) }
        |def f: Val = Val.Val(3200i16)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt16(3200)), "f")
  }

  test("Expression.Tag.13") {
    val input =
      """enum Val { case Val(Int32) }
        |def f: Val = Val.Val(32000000i32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt32(32000000)), "f")
  }

  test("Expression.Tag.14") {
    val input =
      """enum Val { case Val(Int64) }
        |def f: Val = Val.Val(320000000000i64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkInt64(320000000000L)), "f")
  }

  test("Expression.Tag.15") {
    val input =
      """enum Val { case Val(Char) }
        |def f: Val = Val.Val('a')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkChar('a')), "f")
  }

  test("Expression.Tag.16") {
    val input =
      """enum Val { case Val(Float32) }
        |def f: Val = Val.Val(4.2f32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkFloat32(4.2f)), "f")
  }

  test("Expression.Tag.17") {
    val input =
      """enum Val { case Val(Float64) }
        |def f: Val = Val.Val(4.2f64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkFloat64(4.2d)), "f")
  }

  test("Expression.Tag.18") {
    val input =
      """enum A { case AA(Int) }
        |enum B { case BB(A) }
        |def f: B = B.BB(A.AA(42))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("BB", Value.mkTag("AA", Value.mkInt32(42))), "f")
  }

  test("Expression.Tag.19") {
    val input =
      """enum Val { case Val(Set[Int]) }
        |def f: Val = Val.Val(#{1, 2, 3})
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkSet(Set(1, 2, 3).map(Value.mkInt32))), "f")
  }

  test("Expression.Tag.20") {
    val input =
      """enum Val { case Val(BigInt) }
        |def f: Val = Val.Val(12345678901234567890ii)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkTag("Val", Value.mkBigInt("12345678901234567890")), "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.GetTupleIndex                                                //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Tuple                                                        //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Tuple.01") {
    val input = "def f: (Int16, Int32) = (321i16, 5i32)"
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkInt16(321), Value.mkInt32(5))), "f")
  }

  test("Expression.Tuple.02") {
    val input = "def f: (Bool, Bool, Bool) = (true, true, false)"
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(true, true, false).map(Value.mkBool)), "f")
  }

  test("Expression.Tuple.03") {
    val input = """def f: (Str, Str, Str, Str) = ("un", "deux", "trois", "quatre")"""
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array("un", "deux", "trois", "quatre").map(Value.mkStr)), "f")
  }

  test("Expression.Tuple.04") {
    val input = """def f: (Str, Bool, Int64, (), Int8) = ("un", false, 12345i64, (), -2i8)"""
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkStr("un"), Value.False, Value.mkInt64(12345), Value.Unit, Value.mkInt8(-2))), "f")
  }

  test("Expression.Tuple.05") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |def f: (ConstProp, ConstProp) = (ConstProp.Val(111), ConstProp.Bot)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkTag("Val", Value.mkInt32(111)), Value.mkTag("Bot", Value.Unit))), "f")
  }

  test("Expression.Tuple.06") {
    val input = """def f: ((Int, Int), (Str, Str)) = ((123, 456), ("654", "321"))"""
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.Tuple(Array(123, 456).map(Value.mkInt32)), Value.Tuple(Array("654", "321").map(Value.mkStr)))), "f")
  }

  test("Expression.Tuple.07") {
    val input = """def f: (BigInt, Bool, Str) = (40ii + 2ii, !(-12 < 22), if (true) "hi" else "hello")"""
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkBigInt(42), Value.False, Value.mkStr("hi"))), "f")
  }

  test("Expression.Tuple.08") {
    val input = "def f: (Char, Float32, Float64) = ('a', 1.2f32, 3.4f64)"
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkChar('a'), Value.mkFloat32(1.2f), Value.mkFloat64(3.4d))), "f")
  }

  test("Expression.Tuple.09") {
    val input = "def f: (Set[Int], Set[Char]) = (#{1, 2, 3}, #{'a', 'b'})"
    val t = new Tester(input)
    t.runTest(Value.Tuple(Array(Value.mkSet(Set(1, 2, 3).map(Value.mkInt32)), Value.mkSet(Set('a', 'b').map(Value.mkChar)))), "f")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{CheckNil,CheckCons}                                         //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Set                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Set.01") {
    val input = "def f: Set[Int] = #{1, 4, 2}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(1, 4, 2).map(Value.mkInt32)), "f")
  }

  test("Expression.Set.02") {
    val input = "def f: Set[Int8] = #{1i8 + 2i8, 3i8 * 4i8, 5i8 - 6i8}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(3, 12, -1).map(Value.mkInt8)), "f")
  }

  test("Expression.Set.03") {
    val input = "def f: Set[(Int16, Bool)] = #{(1i16 + 2i16, true), (2i16 + 1i16, !false), (4i16 * 7i16, true), (5i16, true && false)}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(
      Value.Tuple(Array(Value.mkInt16(3), Value.True)),
      Value.Tuple(Array(Value.mkInt16(28), Value.True)),
      Value.Tuple(Array(Value.mkInt16(5), Value.False))
    )), "f")
  }

  test("Expression.Set.04") {
    val input = "def f: Set[Int64] = #{10000000000i64}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkInt64(10000000000L))), "f")
  }

  test("Expression.Set.05") {
    val input = "def f: Set[Char] = #{'a', 'b', 'c'}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkChar('a'), Value.mkChar('b'), Value.mkChar('c'))), "f")
  }

  test("Expression.Set.06") {
    val input = "def f: Set[Float32] = #{0.0f32, -0.0f32}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkFloat32(0.0f), Value.mkFloat32(-0.0f))), "f")
  }

  test("Expression.Set.07") {
    val input = "def f: Set[Float64] = #{0.0f64, -0.0f64}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkFloat64(0.0d), Value.mkFloat64(-0.0d))), "f")
  }

  test("Expression.Set.08") {
    val input =
      """enum T { case Top, case Val(Int), case Bot }
        |def f: Set[T] = #{T.Top, T.Top, T.Val(1), T.Val(0)}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(
      Value.mkTag("Top", Value.Unit),
      Value.mkTag("Val", Value.mkInt32(1)),
      Value.mkTag("Val", Value.mkInt32(0))
    )), "f")
  }

  test("Expression.Set.09") {
    val input = "def f: Set[BigInt] = #{1000000000000000000000000000000ii}"
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkBigInt("1000000000000000000000000000000"))), "f")
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
        |def g01: Int = f(true)
        |def g02: Int = f(false)
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
        |def g01: Int = f(true)
        |def g02: Int = f(false)
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
        |def g01: Int = f(true)
        |def g02: Int = f(false)
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
        |def g01: Str = f(-2)
        |def g02: Str = f(-1)
        |def g03: Str = f(0)
        |def g04: Str = f(1)
        |def g05: Str = f(2)
        |def g06: Str = f(3)
        |def g07: Str = f(4)
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
        |def g: Int = f(true)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g")
  }

  test("Switch.06") {
    val input =
      """def f(x: Bool): Int = switch {
        |  case x => 1
        |}
        |def g: Int = f(false)
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
      """def f: Int = match () with {
        |  case _ => 11
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(11), "f")
  }

  test("Match.Wildcard.02") {
    val input =
      """def f: Int = match 42 with {
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
        |def g01: Int = f(-1)
        |def g02: Int = f(0)
        |def g03: Int = f(1)
        |def g04: Int = f(99999)
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
        |def g: Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(1), "g")
  }

  test("Match.Var.02") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case a => a
        |}
        |def g: Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(3), "g")
  }

  test("Match.Var.03") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case a => a + 11
        |}
        |def g: Int = f(3)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(14), "g")
  }

  test("Match.Literal.01") {
    val input =
      """def f(x: Unit): Bool = match x with {
        |  case () => true
        |}
        |def g: Bool = f(())
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
        |def g01: Int = f(true)
        |def g02: Int = f(false)
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
        |def g01: Int = f(true)
        |def g02: Int = f(false)
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
        |def g01: Str = f(-1)
        |def g02: Str = f(0)
        |def g03: Str = f(1)
        |def g04: Str = f(2)
        |def g05: Str = f(3)
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
         |def g01: Str = f(${Byte.MinValue}i8)
         |def g02: Str = f(-2i8)
         |def g03: Str = f(6i8)
         |def g04: Str = f(${Byte.MaxValue}i8)
         |def g05: Str = f(0i8)
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
         |def g01: Str = f(${Short.MinValue}i16)
         |def g02: Str = f(-211i16)
         |def g03: Str = f(623i16)
         |def g04: Str = f(${Short.MaxValue}i16)
         |def g05: Str = f(0i16)
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
         |def g01: Str = f(${Int.MinValue}i32)
         |def g02: Str = f(-2136541i32)
         |def g03: Str = f(6254523i32)
         |def g04: Str = f(${Int.MaxValue}i32)
         |def g05: Str = f(0i32)
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
         |def g01: Str = f(${Long.MinValue}i64)
         |def g02: Str = f(-213645454545541i64)
         |def g03: Str = f(6287816254523i64)
         |def g04: Str = f(${Long.MaxValue}i64)
         |def g05: Str = f(0i64)
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
        |def g01: Str = f("one")
        |def g02: Str = f("two")
        |def g03: Str = f("three")
        |def g04: Str = f("four")
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
        |def g01: Int = f(Foo.Bar)
        |def g02: Int = f(Foo.Baz)
        |def g03: Int = f(Foo.Abc(42, "hi"))
        |def g04: Int = f(Foo.Abc(42, "hi!"))
        |def g05: Int = f(Foo.Abc(41, "hi"))
        |def g06: Int = f(Foo.Abc(40, "a"))
        |def g07: Int = f(Foo.Xyz)
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
        |def g01: Int = f("hi", true)
        |def g02: Int = f("hi", false)
        |def g03: Int = f("abc", true)
        |def g04: Int = f("abc", false)
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
        |def g01: Int = f((4, (12, 8)))
        |def g02: Int = f((4, (12, 0)))
        |def g03: Int = f((1, (12, 8)))
        |def g04: Int = f((1, (12, 0)))
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
        |def g01: Int = f(0, 0)
        |def g02: Int = f(1, 0)
        |def g03: Int = f(0, 2)
        |def g04: Int = f(3, 4)
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
         |def g01: Str = f(-9223372036854775809ii)
         |def g02: Str = f(9223372036854775809ii)
         |def g03: Str = f(6287816254523ii)
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
        |  let NameAndAge.T(_, age) = x in
        |    age
        |def g01: Int = f(NameAndAge.T("James", 42))
        |def g02: Int = f(NameAndAge.T("John", 21))
        |def g03: Int = f(NameAndAge.T("James", 5))
        |def g04: Int = f(NameAndAge.T("Mary", 33))
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
        |def g01: Int = f(NameAndAge.T("James", 42))
        |def g02: Int = f(NameAndAge.T("John", 21))
        |def g03: Int = f(NameAndAge.T("James", 5))
        |def g04: Int = f(NameAndAge.T("Mary", 33))
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
        |def g01: Int = f(ConstProp.Top)
        |def g02: Int = f(ConstProp.Val(42))
        |def g03: Int = f(ConstProp.Val(-24))
        |def g04: Int = f(ConstProp.Bot)
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
        |def g01: Int = f(BoolTag.Top)
        |def g02: Int = f(BoolTag.B(true))
        |def g03: Int = f(BoolTag.B(false))
        |def g04: Int = f(BoolTag.Bot)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(-1), "g03")
    t.runTest(Value.mkInt32(0), "g04")
  }

  test("Match.Tag.05") {
    val input =
      """enum Val { case Nil, case Val((Str, Int)) }
        |def f(x: Val): Int = match x with {
        |  case Val.Nil => 0
        |  case Val.Val(v) => match v with {
        |    case ("x", y) => -1
        |    case (_, y) => y
        |  }
        |}
        |def g01: Int = f(Val.Nil)
        |def g02: Int = f(Val.Val(("a", 1)))
        |def g03: Int = f(Val.Val(("b", 2)))
        |def g04: Int = f(Val.Val(("x", 3)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(-1), "g04")
  }

  test("Match.Tag.06") {
    val input =
      """enum Val { case Nil, case Val((Str, Int)) }
        |def f(x: Val): Int = match x with {
        |  case Val.Nil => 0
        |  case Val.Val(v) => match v with {
        |    case (x, y) => match x with {
        |      case "x" => -1
        |      case _ => match y with {
        |        case z => z
        |      }
        |    }
        |  }
        |}
        |def g01: Int = f(Val.Nil)
        |def g02: Int = f(Val.Val(("a", 1)))
        |def g03: Int = f(Val.Val(("b", 2)))
        |def g04: Int = f(Val.Val(("x", 3)))
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
        |def g01: Str = f(B.Top)
        |def g02: Str = f(B.Bot)
        |def g03: Str = f(B.BB(A.AA(0)))
        |def g04: Str = f(B.BB(A.AA(1)))
        |def g05: Str = f(B.BB(A.AB(0)))
        |def g06: Str = f(B.BB(A.AB(-1)))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkStr("top"), "g01")
    t.runTest(Value.mkStr("bot"), "g02")
    t.runTest(Value.mkStr("a0"), "g03")
    t.runTest(Value.mkStr("aaa"), "g04")
    t.runTest(Value.mkStr("b0"), "g05")
    t.runTest(Value.mkStr("bbb"), "g06")
  }

  test("Match.Tag.08") {
    val input =
      """enum Val { case Nil, case Val(Set[Int]) }
        |def f(x: Val): Set[Int] = match x with {
        |  case Val.Nil => #{0}
        |  case Val.Val(s) => s
        |}
        |def g01: Set[Int] = f(Val.Nil)
        |def g02: Set[Int] = f(Val.Val(#{1, 2, 3}))
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkSet(Set(Value.mkInt32(0))), "g01")
    t.runTest(Value.mkSet(Set(1, 2, 3).map(Value.mkInt32)), "g02")
  }

  test("Match.Tag.09") {
    val input =
      """enum Val { case Val(Int8) }
        |def f: Int8 = match Val.Val(32i8) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt8(32), "f")
  }

  test("Match.Tag.10") {
    val input =
      """enum Val { case Val(Int16) }
        |def f: Int16 = match Val.Val(3200i16) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt16(3200), "f")
  }

  test("Match.Tag.11") {
    val input =
      """enum Val { case Val(Int32) }
        |def f: Int32 = match Val.Val(32000000i32) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(32000000), "f")
  }

  test("Match.Tag.12") {
    val input =
      """enum Val { case Val(Int64) }
        |def f: Int64 = match Val.Val(320000000000i64) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt64(320000000000L), "f")
  }

  test("Match.Tag.13") {
    val input =
      """enum Val { case Val(Char) }
        |def f: Char = match Val.Val('a') with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkChar('a'), "f")
  }

  test("Match.Tag.14") {
    val input =
      """enum Val { case Val(Float32) }
        |def f: Float32 = match Val.Val(4.2f32) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(4.2f), "f")
  }

  test("Match.Tag.15") {
    val input =
      """enum Val { case Val(Float64) }
        |def f: Float64 = match Val.Val(4.2f64) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(4.2d), "f")
  }

  test("Match.Tag.16") {
    val input =
      """enum Val { case Val(BigInt) }
        |def f: BigInt = match Val.Val(100000000000000000000ii) with {
        |  case Val.Val(x) => x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkBigInt("100000000000000000000"), "f")
  }

  test("Match.Tuple.01") {
    val input =
      """def f(x: Int, y: Int): Int =
        |  let (a, b) = (x, y) in a + b
        |def g01: Int = f(5, 6)
        |def g02: Int = f(6, 5)
        |def g03: Int = f(100, 23)
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
        |def g01: Str = f(5, true)
        |def g02: Str = f(5, false)
        |def g03: Str = f(6, true)
        |def g04: Str = f(0, false)
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
        |def g01: Int = f(1ii, 2, 3)
        |def g02: Int = f(1ii, 2, 4)
        |def g03: Int = f(1ii, 3, 3)
        |def g04: Int = f(1ii, 5, 5)
        |def g05: Int = f(2ii, 2, 3)
        |def g06: Int = f(2ii, 10, 20)
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
        |def g01: Int = f(ConstProp.Top, ConstProp.Top)
        |def g02: Int = f(ConstProp.Bot, ConstProp.Bot)
        |def g03: Int = f(ConstProp.Val(42), ConstProp.Val(42))
        |def g04: Int = f(ConstProp.Val(42), ConstProp.Val(0))
        |def g05: Int = f(ConstProp.Val(0), ConstProp.Val(42))
        |def g06: Int = f(ConstProp.Top, ConstProp.Bot)
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
        |def g01: Int = f(1, NameAndAge.T("James", 20))
        |def g02: Int = f(1, NameAndAge.T("John", 53))
        |def g03: Int = f(2, NameAndAge.T("James", 20))
        |def g04: Int = f(2, NameAndAge.T("John", 53))
        |def g05: Int = f(3, NameAndAge.T("Mary", 24))
        |def g06: Int = f(3, NameAndAge.T("Anne", 18))
        |def g07: Int = f(4, NameAndAge.T("Charles", 64))
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
        |def g01: Int = f(0, 0)
        |def g02: Int = f(1, 0)
        |def g03: Int = f(0, 2)
        |def g04: Int = f(3, 4)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(0), "g01")
    t.runTest(Value.mkInt32(1), "g02")
    t.runTest(Value.mkInt32(2), "g03")
    t.runTest(Value.mkInt32(0), "g04")
  }

  test("Match.Tuple.07") {
    import HookUnsafeHelpers._
    val input =
      """def fst(t: (Native, Native)): Native =
        |  let (x, _) = t in x
        |def g: (Native, Native) = f(12)
        |def h: Native = fst(g())
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkNativeType, flix.mkNativeType)))
    def nativeF(x: Int): (MyObject, MyObject) = (MyObject(x), MyObject(x * 2))

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(MyObject(12), "h")
  }

  test("Match.Tuple.08") {
    import HookUnsafeHelpers._
    val input =
      """def fst(t: (Native, Native)): Native =
        |  let (x, _) = t in x
        |def g: (Native, Native) = f(12)
        |def h: Native = fst(g())
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkNativeType, flix.mkNativeType)))
    def nativeF(x: Int): (Int, String) = (x, x.toString)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(12), "h")
  }

  test("Match.Tuple.09") {
    import HookUnsafeHelpers._
    val input =
      """def fst(t: (Int, Str)): Int =
        |  let (x, _) = t in x
        |def g: (Int, Str) = f(12)
        |def h: Int = fst(g())
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: Int): (Int, String) = (x, x.toString)

    t.addHook("f", tpe, nativeF _).run()
    t.runTest(Value.mkInt32(12), "h")
  }

  test("Match.Error.01") {
    val input =
      """def f(x: Int): Bool = match x with {
        |  case 321 => true
        |}
        |def g: Bool = f(123)
      """.stripMargin
    val t = new Tester(input)
    t.runInterceptTest[MatchException]("g")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Var                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Var.01") {
    val input =
      """rel A(x: Bool);
        |rel B(x: Bool);
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
      """rel A(x: Int);
        |rel B(x: Int);
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
      """rel A(x: Str);
        |rel B(x: Str);
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
      """rel A(x: Unit);
        |
        |A(()).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Unit)), "A")
  }

  test("Term.Head.Exp.02") {
    val input =
      """rel A(x: Bool);
        |
        |A(true).
        |A(false).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(true, false).map(x => List(Value.mkBool(x))), "A")
  }

  test("Term.Head.Exp.03") {
    val input =
      """rel A(x: Int8);
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
      """rel A(x: Int16);
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
      """rel A(x: Int32);
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
      """rel A(x: Int64);
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
      """rel A(x: Str);
        |
        |A("one").
        |A("two").
        |A("three").
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  test("Term.Head.Exp.08") {
    val input =
      """rel A(x: (Int, Str));
        |
        |A((1, "one")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  test("Term.Head.Exp.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(Foo.Foo(1, "one")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.mkTag("Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))), "A")
  }

  test("Term.Head.Exp.10") {
    val input =
      """rel A(x: (Int, Int));
        |
        |A((1, 2)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))), "A")
  }

  test("Term.Head.Exp.11") {
    val input =
      """rel A(x: Char);
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
      """rel A(x: Float32);
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
      """rel A(x: Float64);
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
      """rel A(x: BigInt);
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
      """rel A(x: ());
        |def f(x: Int): () = ()
        |
        |A(f(0)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Unit)), "A")
  }

  test("Term.Head.Apply.02") {
    val input =
      """rel A(x: Bool);
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
      """rel A(x: Int8);
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
      """rel A(x: Int16);
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
      """rel A(x: Int32);
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
      """rel A(x: Int64);
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
      """rel A(x: Str);
        |def f(x: Str): Str = x
        |
        |A(f("one")).
        |A(f("two")).
        |A(f("three")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  test("Term.Head.Apply.08") {
    val input =
      """rel A(x: (Int, Str));
        |def f(x: Int): (Int, Str) = (x, "one")
        |
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  test("Term.Head.Apply.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |def f(x: Str): Foo = Foo.Foo(1, x)
        |
        |A(f("one")).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.mkTag("Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))), "A")
  }

  test("Term.Head.Apply.10") {
    val input =
      """rel A(x: (Int, Int));
        |def f(x: Int, y: Int): (Int, Int) = (x, y)
        |
        |A(f(1, 2)).
      """.stripMargin
    val t = new Tester(input)
    t.checkModel(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))), "A")
  }

  test("Term.Head.Apply.11") {
    val input =
      """rel A(x: Char);
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
      """rel A(x: Float32);
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
      """rel A(x: Float64);
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
      """rel A(x: BigInt);
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
  // Term.Head.ApplyHook - Hook.Safe                                         //
  // Reimplements Term.Head.Exp tests but with ApplyHook (safe).             //
  // Note that some tests can't be reimplemented here and vice versa.        //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.ApplyHook - Hook.Safe.01") {
    import HookSafeHelpers._
    val input =
      """rel A(x: ());
        |
        |A(f(0)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkUnitType)
    def nativeF(x: IValue): IValue = flix.mkUnit

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Unit)), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.02") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Bool);
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    def nativeF(x: IValue): IValue = flix.mkBool(x.getInt32 == 0)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(true, false).map(x => List(Value.mkBool(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.03") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int8);
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: IValue): IValue = flix.mkInt8(x.getInt8 + 1)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt8(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.04") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int16);
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: IValue): IValue = flix.mkInt16(x.getInt16 + 1)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt16(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.05") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int32);
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = flix.mkInt32(x.getInt32 + 1)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.06") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int64);
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: IValue): IValue = flix.mkInt64(x.getInt64 + 1)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt64(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.07") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Str);
        |
        |A(f("one")).
        |A(f("two")).
        |A(f("three")).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkStrType)
    def nativeF(x: IValue): IValue = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.08") {
    import HookSafeHelpers._
    val input =
      """rel A(x: (Int, Str));
        |
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: IValue): IValue = flix.mkTuple(Array(x, flix.mkStr("one")))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  ignore("Term.Head.ApplyHook - Hook.Safe.09") {
    import HookSafeHelpers._
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(f("one")).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tupleTpe = flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType))
    val tagTpe = flix.mkTagType("Foo", "Foo", tupleTpe)
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkEnumType("Foo", Array(tagTpe)))
    def nativeF(x: IValue): IValue = flix.mkTag("Foo", flix.mkTuple(Array(flix.mkInt32(1), x)))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.mkTag("Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.10") {
    import HookSafeHelpers._
    val input =
      """rel A(x: (Int, Int));
        |
        |A(f(1, 2)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkInt32Type)))
    def nativeF(x: IValue, y: IValue): IValue = flix.mkTuple(Array(x, y))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.11") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Char);
        |
        |A(f('a')).
        |A(f('b')).
        |A(f('c')).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkCharType), flix.mkCharType)
    def nativeF(x: IValue): IValue = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set('a', 'b', 'c').map(x => List(Value.mkChar(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.12") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Float32);
        |
        |A(f(1.0f32)).
        |A(f(2.0f32)).
        |A(f(3.0f32)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat32Type), flix.mkFloat32Type)
    def nativeF(x: IValue): IValue = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1.0f, 2.0f, 3.0f).map(x => List(Value.mkFloat32(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.13") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Float64);
        |
        |A(f(1.0f64)).
        |A(f(2.0f64)).
        |A(f(3.0f64)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat64Type), flix.mkFloat64Type)
    def nativeF(x: IValue): IValue = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1.0d, 2.0d, 3.0d).map(x => List(Value.mkFloat64(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.14") {
    import HookSafeHelpers._
    val input =
      """rel A(x: BigInt);
        |
        |A(f(0ii)).
        |A(f(1ii)).
        |A(f(2ii)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkBigIntType), flix.mkBigIntType)
    def nativeF(x: IValue): IValue = flix.mkBigInt(x.getBigInt add java.math.BigInteger.valueOf(1))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkBigInt(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Safe.15") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Native);
        |
        |A(f(1)).
        |A(f(2)).
        |A(f(3)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    def nativeF(x: IValue): IValue = flix.mkNative(MyObject(x.getInt32))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(MyObject(x))), "A")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.ApplyHook - Hook.Unsafe                                       //
  // Reimplements Term.Head.Exp tests but with ApplyHook (unsafe).           //
  // Note that some tests can't be reimplemented here and vice versa.        //
  // Note that native functions need to be annotated with JBool, JInt, etc.  //
  // This is necessary so that implicits are properly called.                //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.ApplyHook - Hook.Unsafe.01") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: ());
        |
        |A(f(0)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkUnitType)
    def nativeF(x: JInt): Value.Unit.type = Value.Unit

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Unit)), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.02") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Bool);
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    def nativeF(x: JInt): JBool = x == 0

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(true, false).map(x => List(Value.mkBool(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.03") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int8);
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: JByte): JByte = (x + 1).toByte

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt8(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.04") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int16);
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: JShort): JShort = (x + 1).toShort

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt16(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.05") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int32);
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = x + 1

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt32(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.06") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int64);
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: JLong): JLong = x + 1

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkInt64(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.07") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Str);
        |
        |A(f("one")).
        |A(f("two")).
        |A(f("three")).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkStrType)
    def nativeF(x: String): String = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set("one", "two", "three").map(x => List(Value.mkStr(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.08") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: (Int, Str));
        |
        |A(f(1)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: JInt): Value.Tuple = Value.Tuple(Array(x, "one"))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))), "A")
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  ignore("Term.Head.ApplyHook - Hook.Unsafe.09") {
    import HookUnsafeHelpers._
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(f("one")).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tupleTpe = flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType))
    val tagTpe = flix.mkTagType("Foo", "Foo", tupleTpe)
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkEnumType("Foo", Array(tagTpe)))
    def nativeF(x: String): Value.Tag = Value.mkTag("Foo", Value.Tuple(Array(Value.mkInt32(1), x)))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.mkTag("Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.10") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: (Int, Int));
        |
        |A(f(1, 2)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkInt32Type)))
    def nativeF(x: JInt, y: JInt): Value.Tuple = Value.Tuple(Array(x, y))

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.11") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Char);
        |
        |A(f('a')).
        |A(f('b')).
        |A(f('c')).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkCharType), flix.mkCharType)
    def nativeF(x: JChar): JChar = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set('a', 'b', 'c').map(x => List(Value.mkChar(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.12") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Float32);
        |
        |A(f(1.0f32)).
        |A(f(2.0f32)).
        |A(f(3.0f32)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat32Type), flix.mkFloat32Type)
    def nativeF(x: JFloat): JFloat = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1.0f, 2.0f, 3.0f).map(x => List(Value.mkFloat32(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.13") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Float64);
        |
        |A(f(1.0f64)).
        |A(f(2.0f64)).
        |A(f(3.0f64)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkFloat64Type), flix.mkFloat64Type)
    def nativeF(x: JDouble): JDouble = x

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1.0d, 2.0d, 3.0d).map(x => List(Value.mkFloat64(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.14") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: BigInt);
        |
        |A(f(0ii)).
        |A(f(1ii)).
        |A(f(2ii)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkBigIntType), flix.mkBigIntType)
    def nativeF(x: JBigInt): JBigInt = x add java.math.BigInteger.valueOf(1)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(Value.mkBigInt(x))), "A")
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.15") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Native);
        |
        |A(f(1)).
        |A(f(2)).
        |A(f(3)).
      """.stripMargin
    val t = new Tester(input, solve = false)

    val flix = t.flix
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    def nativeF(x: JInt): MyObject = MyObject(x)

    t.addHook("f", tpe, nativeF _).run()
    t.checkModel(Set(1, 2, 3).map(x => List(MyObject(x))), "A")
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
      """rel A(x: Bool, y: Bool);
        |rel B(x: Bool);
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
      """rel A(x: Int);
        |rel B(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
      """rel A(x: Int);
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
