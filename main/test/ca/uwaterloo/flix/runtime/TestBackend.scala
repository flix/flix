package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.util.{DebugBytecode, _}
import org.scalatest.FunSuite

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

  private class Tester(input: String, dumpBytecode: Boolean = false) {
    private def getModel(codegen: Boolean) = {
      val options = Options(
        debugger = Debugger.Disabled,
        print = Nil,
        verbosity = Verbosity.Silent,
        verify = Verify.Disabled,
        codegen = if (codegen) CodeGeneration.Enabled else CodeGeneration.Disabled,
        debugBytecode = if (dumpBytecode) DebugBytecode.Enabled else DebugBytecode.Disabled
      )
      new Flix().setOptions(options).addStr(input).solve().get
    }

    def runTest(expected: AnyRef, const: String): Unit = {
      assertResult(expected, s"- interpreter produced wrong value for $const")(interpreted.getConstant(const))
      assertResult(expected, s"- compiler produced wrong value for $const")(compiled.getConstant(const))
    }

    def runInterceptTest[T <: AnyRef](const:String)(implicit manifest: Manifest[T]): Unit = {
      withClue(s"interpreted value $const:") { intercept[T](interpreted.getConstant(const)) }
      withClue(s"compiled value $const:") { intercept[T](compiled.getConstant(const)) }
    }

    val interpreted = getModel(codegen = false)
    val compiled = getModel(codegen = true)
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
        |  def x: Int = 5
        |  def f: Int = x
        |}
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkInt32(5), "Foo/f")
  }

  test("Expression.Ref.03") {
    val input =
      """namespace Foo {
        |  def x: Bool = true
        |  def y: Bool = false
        |  def f: Bool = y
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
        |  def x: Str = Foo/x
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
        |  def b: Bool = !A.B/a
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

}
