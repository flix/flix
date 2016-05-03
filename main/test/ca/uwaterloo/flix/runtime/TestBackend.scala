package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.Symbol
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
        |  def g: Bool = A.B/f
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
    t.runTest(Value.mkTag(Symbol.Resolved.mk("Val"), "Val", Value.mkInt32(111)), "g")
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

  test("Expression.Lambda.17") {
    val input =
      """def f(a: Char, b: Char): Bool = a == b
        |def g: Bool = f('a', 'b')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "g")
  }

  test("Expression.Lambda.18") {
    val input =
      """def f(a: Float32, b: Float32): Float32 = a + b
        |def g: Float32 = f(1.2f32, 2.1f32)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat32(3.3f), "g")
  }

  test("Expression.Lambda.19") {
    val input =
      """def f(a: Float64, b: Float64): Float64 = a + b
        |def g: Float64 = f(1.2f64, 2.1f64)
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.mkFloat64(3.3d), "g")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Safe                                     //
  // Re-implements Expression.Lambda tests but using (safe) hooks instead.   //
  // Note that some Lambda tests can't be reimplemented here and vice versa. //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Tests when interop (Hook) is implemented in codegen. The Tester class will need to be updated.
  // There are some subtleties with codegen that might make us revisit the design, and affect the interpreter.
  // Also, note that we can only interop with 0-arg native functions, not native values. addHook() and addHookUnsafe()
  // will complain if you give them a non-function type. However, we don't allow 0-arg functions in Flix.

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Unsafe                                   //
  // Re-implements Expression.Lambda tests but using (unsafe) hooks instead. //
  // Note that native functions need to be annotated with JBool, JInt, etc.  //
  // This is necessary so that implicits are properly called.                //
  // Note that some Lambda tests can't be reimplemented here and vice versa. //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Tests when interop (Hook) is implemented in codegen. The Tester class will need to be updated.
  // There are some subtleties with codegen that might make us revisit the design, and affect the interpreter.
  // Also, note that we can only interop with 0-arg native functions, not native values. addHook() and addHookUnsafe()
  // will complain if you give them a non-function type. However, we don't allow 0-arg functions in Flix.

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
      s"""def f01: Bool = '${'十'}' == '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' == '${'十'}'
         |def f03: Bool = '${'\u0000'}' == '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.False, "f02")
    t.runTest(Value.True, "f03")
  }

  test("Expression.Binary - BinaryOperator.Equal.10") {
    val input = "def f: Bool = () == ()"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  test("Expression.Binary - BinaryOperator.Equal.11") {
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

  test("Expression.Binary - BinaryOperator.Equal.12") {
    val input =
      """def f01: Bool = "hello" == "hello"
        |def f02: Bool = "hello" == "hello!"
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.False, "f02")
  }

  test("Expression.Binary - BinaryOperator.Equal.13") {
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

  test("Expression.Binary - BinaryOperator.Equal.14") {
    val foo = (1, 2) == (3, 'a')
    val input =
      """def f01: Bool = (1, 2, 3) == (1, 2, 3)
        |def f02: Bool = ('h', 'e', 'l', 'l', 'o') == ('h', 'e', 'l', 'l', 'o')
        |def f03: Bool = (1, 2, 'a') == (1, 2, 'b')
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
  }

  test("Expression.Binary - BinaryOperator.Equal.15") {
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
      s"""def f01: Bool = '${'十'}' != '${'\u0000'}'
         |def f02: Bool = '${'\u0000'}' != '${'十'}'
         |def f03: Bool = '${'\u0000'}' != '${'\u0000'}'
       """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.True, "f01")
    t.runTest(Value.True, "f02")
    t.runTest(Value.False, "f03")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.10") {
    val input = "def f: Bool = () != ()"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.11") {
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

  test("Expression.Binary - BinaryOperator.NotEqual.12") {
    val input =
      """def f01: Bool = "hello" != "hello"
        |def f02: Bool = "hello" != "hello!"
      """.stripMargin
    val t = new Tester(input)
    t.runTest(Value.False, "f01")
    t.runTest(Value.True, "f02")
  }

  test("Expression.Binary - BinaryOperator.NotEqual.13") {
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

  test("Expression.Binary - BinaryOperator.NotEqual.14") {
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

  test("Expression.Binary - BinaryOperator.NotEqual.15") {
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

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.LogicalAnd.05") {
    val input = "def f: Bool = false && ???"
    val t = new Tester(input)
    t.runTest(Value.False, "f")
  }

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.LogicalAnd.06") {
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

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.LogicalOr.05") {
    val input = "def f: Bool = true || ???"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.LogicalOr.06") {
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

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.Implication.05") {
    val input = "def f: Bool = false ==> ???"
    val t = new Tester(input)
    t.runTest(Value.True, "f")
  }

  // TODO: Typechecker doesn't properly handle ???
  ignore("Expression.Binary - BinaryOperator.Implication.06") {
    val input = "def f: Bool = True ==> ???"
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
    t.runTest(Value.mkTag(Symbol.Resolved.mk("ConstProp"), "Val", Value.mkInt32(42)), "f")
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

}
