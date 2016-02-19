package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{Flix, IValue, Invokable, InvokableUnsafe}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Type}
import ca.uwaterloo.flix.util.{Debugger, Options, Verbosity, Verify}
import org.scalatest.FunSuite

// TODO: Intercept tests should catch a more specific exception, otherwise real bugs will be masked.

class TestInterpreter extends FunSuite {

  object HookSafeHelpers {
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

  object HookUnsafeHelpers {
    type JBool = java.lang.Boolean
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

  def createFlix() = {
    val options = Options(
      debugger = Debugger.Disabled,
      print = Nil,
      verbosity = Verbosity.Silent,
      verify = Verify.Disabled
    )
    new Flix().setOptions(options)
  }

  def getModel(input: String) = createFlix().addStr(input).solve().get

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Unit,Bool,Int8,Int16,Int32,Int64,Str}                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unit") {
    val input = "fn f: () = ()"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Unit)(result)
  }

  test("Expression.Bool.01") {
    val input = "fn f: Bool = true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Bool.02") {
    val input = "fn f: Bool = false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Int.01") {
    val input = "fn f: Int = 0"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(0))(result)
  }

  test("Expression.Int.02") {
    val input = "fn f: Int = -254542"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(-254542))(result)
  }

  test("Expression.Int.03") {
    val input = "fn f: Int = 45649878"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(45649878))(result)
  }

  test("Expression.Int.04") {
    val input = s"fn f: Int = ${Int.MaxValue}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MaxValue))(result)
  }

  test("Expression.Int.05") {
    val input = s"fn f: Int = ${Int.MinValue}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MinValue))(result)
  }

  ignore("Expression.Int8.01") {
    val input = "fn f: Int8 = -105i8"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(-105))(result)
  }

  ignore("Expression.Int8.02") {
    val input = "fn f: Int8 = 121i8"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(121))(result)
  }

  ignore("Expression.Int8.03") {
    val input = s"fn f: Int8 = ${Byte.MaxValue}i8"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result)
  }

  ignore("Expression.Int8.04") {
    val input = s"fn f: Int8 = ${Byte.MinValue}i8"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(Byte.MinValue))(result)
  }

  ignore("Expression.Int16.01") {
    val input = "fn f: Int16 = -5320i16"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(-5320))(result)
  }

  ignore("Expression.Int16.02") {
    val input = "fn f: Int16 = 4568i16"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(4568))(result)
  }

  ignore("Expression.Int16.03") {
    val input = s"fn f: Int16 = ${Short.MaxValue}i16"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(Short.MaxValue))(result)
  }

  ignore("Expression.Int16.04") {
    val input = s"fn f: Int16 = ${Short.MinValue}i16"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(Short.MinValue))(result)
  }

  ignore("Expression.Int32.01") {
    val input = "fn f: Int32 = -254542i32"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(-254542))(result)
  }

  ignore("Expression.Int32.02") {
    val input = "fn f: Int32 = 45649878i32"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(45649878))(result)
  }

  ignore("Expression.Int32.03") {
    val input = s"fn f: Int32 = ${Int.MaxValue}i32"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MaxValue))(result)
  }

  ignore("Expression.Int32.04") {
    val input = s"fn f: Int32 = ${Int.MinValue}i32"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MinValue))(result)
  }

  ignore("Expression.Int64.01") {
    val input = "fn f: Int64 = -254454121542i64"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(-254454121542L))(result)
  }

  ignore("Expression.Int64.02") {
    val input = "fn f: Int64 = 45641198784545i64"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(45641198784545L))(result)
  }

  ignore("Expression.Int64.03") {
    val input = s"fn f: Int64 = ${Long.MaxValue}i64"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(Long.MaxValue))(result)
  }

  ignore("Expression.Int64.04") {
    val input = s"fn f: Int64 = ${Long.MinValue}i64"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(Long.MinValue))(result)
  }

  test("Expression.Str.01") {
    val input = """fn f: Str = """""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr(""))(result)
  }

  test("Expression.Str.02") {
    val input = """fn f: Str = "Hello World!""""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("Hello World!"))(result)
  }

  test("Expression.Str.03") {
    val input = """fn f: Str = "asdf""""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("asdf"))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // LoadExpression and StoreExpression                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: LoadExpression and StoreExpression tests
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
      """namespace Foo::Bar {
        |  fn x: Bool = false
        |  fn f: Str = "foo"
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("Foo::Bar::f"))
    assertResult(Value.mkStr("foo"))(result)
  }

  test("Expression.Ref.02") {
    val input =
      """namespace Foo {
        |  fn x: Int = 5
        |  fn f: Int = x()
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("Foo::f"))
    assertResult(Value.mkInt32(5))(result)
  }

  test("Expression.Ref.03") {
    val input =
      """namespace Foo {
        |  fn x: Bool = true
        |  fn y: Bool = false
        |  fn f: Bool = y()
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("Foo::f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Ref.04") {
    val input =
      """namespace Foo {
        |  fn x: Str = "hello"
        |}
        |namespace Bar {
        |  fn x: Str = Foo::x()
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("Bar::x"))
    assertResult(Value.mkStr("hello"))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Lambda,Apply}                                               //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Lambda.01") {
    val input =
      """namespace A::B { fn f: Bool = false }
        |namespace A { fn g: Bool = A::B::f() }
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.False)(result)
  }

  test("Expression.Lambda.02") {
    val input =
      """namespace A { fn f(x: Int): Int = 24 }
        |fn g: Int = A::f(3)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(24))(result)
  }

  test("Expression.Lambda.03") {
    val input =
      """namespace A { fn f(x: Int): Int = x }
        |namespace A { fn g: Int = f(3) }
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.mkInt32(3))(result)
  }

  ignore("Expression.Lambda.04") {
    val input =
      """fn f(x: Int64, y: Int64): Int64 = x * y - 6i64
        |fn g: Int64 = f(3i64, 42i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(120))(result)
  }

  ignore("Expression.Lambda.05") {
    val input =
      """namespace A { fn f(x: Int32): Int32 = let y = B::g(x + 1i32) in y * y }
        |namespace B { fn g(x: Int32): Int32 = x - 4i32 }
        |namespace C { fn h: Int32 = A::f(5i32) + B::g(0i32) }
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("C::h"))
    assertResult(Value.mkInt32(0))(result)
  }

  ignore("Expression.Lambda.06") {
    val input =
      """fn f(x: Int16): Int16 = g(x + 1i16)
        |fn g(x: Int16): Int16 = h(x + 10i16)
        |fn h(x: Int16): Int16 = x * x
        |fn x: Int16 = f(3i16)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt32(196))(result)
  }

  ignore("Expression.Lambda.07") {
    val input =
      """fn f(x: Int8, y: Int8): Int = x - y
        |fn g(x: Int8): Int8 = x * 3i8
        |fn h(x: Int8): Int8 = g(x - 1i8)
        |fn x: Int8 = let x = 7 in f(g(3i8), h(h(xi8)))
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt32(-42))(result)
  }

  test("Expression.Lambda.08") {
    val input =
      """fn f(x: Bool, y: Bool): Bool = if (x) true else y
        |fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
  }

  test("Expression.Lambda.09") {
    val input =
      """fn f(x: Bool, y: Bool): Bool = if (x) y else false
        |fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
  }

  test("Expression.Lambda.10") {
    val input =
      """fn f(x: Int, y: Int, z: Int): Int = x + y + z
        |fn g: Int = f(2, 42, 5)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(49))(result)
  }

  test("Expression.Lambda.11") {
    val input =
      """fn f(x: (Int) -> Int, y: Int): Int = x(y)
        |fn g(x: Int): Int = x + 1
        |fn h: Int = f(g, 5)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(6))(result)
  }

  test("Expression.Lambda.12") {
    val input =
      """fn f(x: (Int) -> Int, y: Int): Int = x(y)
        |fn g: Int = f(fn (x: Int): Int = x + 1, 5)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(6))(result)
  }

  test("Expression.Lambda.13") {
    val input =
      """fn f(x: (Int) -> Int): (Int) -> Int = x
        |fn g(x: Int): Int = x + 5
        |fn h: Int = (f(g))(40)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(45))(result)
  }

  test("Expression.Lambda.14") {
    val input =
      """enum Val { case Val(Int) }
        |fn f(x: Int): Val = Val.Val(x)
        |fn g: Val = f(111)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt32(111)))(result)
  }

  test("Expression.Lambda.15") {
    val input =
      """fn f(a: Int, b: Int, c: Str, d: Int, e: Bool, f: ()): (Int, Int, Str, Int, Bool, ()) = (a, b, c, d, e, f)
        |fn g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)))(result)
  }

  test("Expression.Lambda.16") {
    val input =
      """fn f(a: Int, b: Int, c: Int): Set[Int] = #{a, b, c}
        |fn g: Set[Int] = f(24, 53, 24)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Safe                                     //
  // Re-implements Expression.Lambda tests but using (safe) hooks instead.   //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Hook - Hook.Safe.01") {
    import HookSafeHelpers._
    val input = "namespace A { fn g: Bool = A::B::f() }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(), flix.mkBoolType)
    def nativeF(): IValue = { executed = true; flix.mkFalse }
    val model = flix
      .addStr(input)
      .addHook("A::B::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.False)(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.02") {
    import HookSafeHelpers._
    val input = "fn g: Int = A::f(3)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkInt32(24) }
    val model = flix
      .addStr(input)
      .addHook("A::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(24))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.03") {
    import HookSafeHelpers._
    val input = "namespace A { fn g: Int = f(3) }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = { executed = true; x }
    val model = flix
      .addStr(input)
      .addHook("A::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.mkInt32(3))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Safe.04") {
    import HookSafeHelpers._
    val input = "fn g: Int64 = f(3i64, 42i64)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type, flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: IValue, y: IValue): IValue = { executed = true; flix.mkInt64(x.getInt64 * y.getInt64 - 6) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(120))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Safe.05") {
    import HookSafeHelpers._
    val input = "namespace C { fn h: Int32 = A::f(5i32) + B::g(0i32) }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = {
      val y = nativeG(flix.mkInt32(x.getInt32 + 1))
      flix.mkInt32(y.getInt32 * y.getInt32)
    }
    def nativeG(x: IValue): IValue = { executed = true; flix.mkInt32(x.getInt32 - 4) }
    val model = flix
      .addStr(input)
      .addHook("A::f", tpe, nativeF _)
      .addHook("B::g", tpe, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("C::h"))
    assertResult(Value.mkInt32(0))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Safe.06") {
    import HookSafeHelpers._
    val input = "fn x: Int16 = f(3i16)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: IValue): IValue = nativeG(flix.mkInt16(x.getInt16 + 1))
    def nativeG(x: IValue): IValue = nativeH(flix.mkInt16(x.getInt16 + 10))
    def nativeH(x: IValue): IValue = { executed = true; flix.mkInt16(x.getInt16 * x.getInt16) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .addHook("g", tpe, nativeG _)
      .addHook("h", tpe, nativeH _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt16(196))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Safe.07") {
    import HookSafeHelpers._
    val input = "fn x: Int8 = let x = 7i8 in f(g(3i8), h(h(x)))"
    var executed = false
    val flix = createFlix()
    val tpe1 = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    val tpe2 = flix.mkFunctionType(Array(flix.mkInt8Type, flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: IValue, y: IValue): IValue = flix.mkInt8(x.getInt8 - y.getInt8)
    def nativeG(x: IValue): IValue = flix.mkInt8(x.getInt8 * 3)
    def nativeH(x: IValue): IValue = { executed = true; nativeG(flix.mkInt8(x.getInt8 - 1)) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe2, nativeF _)
      .addHook("g", tpe1, nativeG _)
      .addHook("h", tpe1, nativeH _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt8(-42))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.08") {
    import HookSafeHelpers._
    val input =
      """fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: IValue, y: IValue): IValue = { executed = true; if (x.getBool) flix.mkTrue else y }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.09") {
    import HookSafeHelpers._
    val input =
      """fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: IValue, y: IValue): IValue = { executed = true; if (x.getBool) y else flix.mkFalse }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.10") {
    import HookSafeHelpers._
    val input = "fn g: Int = f(2, 42, 5)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue, y: IValue, z: IValue): IValue = {
      executed = true
      flix.mkInt32(x.getInt32 + y.getInt32 + z.getInt32)
    }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(49))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.11") {
    import HookSafeHelpers._
    val input = "fn h: Int = f(g, 5)"
    var executed = false
    val flix = createFlix()
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    val tpeF = flix.mkFunctionType(Array(tpeG, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue, y: IValue): IValue = {
      val closure = x.getUnsafeRef.asInstanceOf[Value.HookClosure]
      val hook = closure.hook.asInstanceOf[Ast.Hook.Safe]
      hook.inv(Array(y))
    }
    def nativeG(x: IValue): IValue = { executed = true; flix.mkInt32(x.getInt32 + 1) }
    val model = flix
      .addStr(input)
      .addHook("f", tpeF, nativeF _)
      .addHook("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(6))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.12") {
    import HookSafeHelpers._
    val input = "fn h: Int = (f(g))(40)"
    var executed = false
    val flix = createFlix()
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    val tpeF = flix.mkFunctionType(Array(tpeG), tpeG)
    def nativeF(x: IValue): IValue = x
    def nativeG(x: IValue): IValue = { executed = true; flix.mkInt32(x.getInt32 + 5) }
    val model = flix
      .addStr(input)
      .addHook("f", tpeF, nativeF _)
      .addHook("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(45))(result)
    assert(executed)
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  // TODO: mkTagType should be taking an IType instead of a Type?
  ignore("Expression.Hook - Hook.Safe.13") {
    import HookSafeHelpers._
    val input =
      """enum Val { case Val(Int) }
        |fn g: Val = f(111)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tagTpe = flix.mkTagType("Val", "Val", Type.Int32)
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkEnumType("Val", Array(tagTpe)))
    def nativeF(x: IValue): IValue = { executed = true; flix.mkTag("Val", "Val", x) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt32(111)))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.14") {
    import HookSafeHelpers._
    val input = """fn g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())"""
    var executed = false
    val flix = createFlix()
    val tpes = Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkStrType, flix.mkInt32Type, flix.mkBoolType, flix.mkUnitType)
    val tpe = flix.mkFunctionType(tpes, flix.mkTupleType(tpes))
    def nativeF(a: IValue, b: IValue, c: IValue, d: IValue, e: IValue, f: IValue): IValue = {
      executed = true
      flix.mkTuple(Array(a, b, c, d, e, f))
    }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
    .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.15") {
    import HookSafeHelpers._
    val input = "fn g: Set[Int] = f(24, 53, 24)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkSetType(flix.mkInt32Type))
    def nativeF(x: IValue, y: IValue, z: IValue): IValue = { executed = true; flix.mkSet(Set(x, y, z)) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Safe.16") {
    import HookSafeHelpers._
    val input = "fn h: Native = g(f(999))"
    var executed = false
    val flix = createFlix()
    val tpeF = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    val tpeG = flix.mkFunctionType(Array(flix.mkNativeType), flix.mkNativeType)
    def nativeF(x: IValue): IValue = flix.mkNative(MyObject(x.getInt32))
    def nativeG(o: IValue): IValue = {
      executed = true
      val obj = o.getUnsafeRef.asInstanceOf[MyObject]
      flix.mkNative(MyObject(obj.x + 1))
    }
    val model = flix
      .addStr(input)
      .addHook("f", tpeF, nativeF _)
      .addHook("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(MyObject(1000))(result)
    assert(executed)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Hook,Apply} - Hook.Unsafe                                   //
  // Re-implements Expression.Lambda tests but using (unsafe) hooks instead. //
  // Note that native functions need to be annotated with JBool, JInt, etc.  //
  // This is necessary so that implicits are properly called.                //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Hook - Hook.Unsafe.01") {
    import HookUnsafeHelpers._
    val input = "namespace A { fn g: Bool = A::B::f() }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(), flix.mkBoolType)
    def nativeF(): JBool = { executed = true; false }
    val model = flix
      .addStr(input)
      .addHookUnsafe("A::B::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.False)(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.02") {
    import HookUnsafeHelpers._
    val input = "fn g: Int = A::f(3)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = { executed = true; 24 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("A::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(24))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.03") {
    import HookUnsafeHelpers._
    val input = "namespace A { fn g: Int = f(3) }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = { executed = true; x }
    val model = flix
      .addStr(input)
      .addHookUnsafe("A::f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("A::g"))
    assertResult(Value.mkInt32(3))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Unsafe.04") {
    import HookUnsafeHelpers._
    val input = "fn g: Int64 = f(3i64, 42i64)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type, flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: JLong, y: JLong): JLong = { executed = true; x * y - 6 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(120))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Unsafe.05") {
    import HookUnsafeHelpers._
    val input = "namespace C { fn h: Int32 = A::f(5i32) + B::g(0i32) }"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = { val y = nativeG(x + 1); y * y }
    def nativeG(x: JInt): JInt = { executed = true; x - 4 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("A::f", tpe, nativeF _)
      .addHookUnsafe("B::g", tpe, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("C::h"))
    assertResult(Value.mkInt32(0))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Unsafe.06") {
    import HookUnsafeHelpers._
    val input = "fn x: Int16 = f(3i16)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: JShort): JShort = nativeG((x + 1).toShort)
    def nativeG(x: JShort): JShort = nativeH((x + 10).toShort)
    def nativeH(x: JShort): JShort = { executed = true; (x * x).toShort }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .addHookUnsafe("g", tpe, nativeG _)
      .addHookUnsafe("h", tpe, nativeH _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt16(196))(result)
    assert(executed)
  }

  ignore("Expression.Hook - Hook.Unsafe.07") {
    import HookUnsafeHelpers._
    val input = "fn x: Int8 = let x = 7i8 in f(g(3i8), h(h(x)))"
    var executed = false
    val flix = createFlix()
    val tpe1 = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    val tpe2 = flix.mkFunctionType(Array(flix.mkInt8Type, flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: JByte, y: JByte): JByte = (x - y).toByte
    def nativeG(x: JByte): JByte = (x * 3).toByte
    def nativeH(x: JByte): JByte = { executed = true; nativeG((x - 1).toByte) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe2, nativeF _)
      .addHookUnsafe("g", tpe1, nativeG _)
      .addHookUnsafe("h", tpe1, nativeH _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("x"))
    assertResult(Value.mkInt8(-42))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.08") {
    import HookUnsafeHelpers._
    val input =
      """fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: JBool, y: JBool): JBool = { executed = true; if (x) true else y }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.09") {
    import HookUnsafeHelpers._
    val input =
      """fn g01: Bool = f(true, true)
        |fn g02: Bool = f(true, false)
        |fn g03: Bool = f(false, false)
        |fn g04: Bool = f(false, true)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkBoolType), flix.mkBoolType)
    def nativeF(x: JBool, y: JBool): JBool = { executed = true; if (x) y else false }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.10") {
    import HookUnsafeHelpers._
    val input = "fn g: Int = f(2, 42, 5)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt, y: JInt, z: JInt): JInt = { executed = true; x + y + z }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(49))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.11") {
    import HookUnsafeHelpers._
    val input = "fn h: Int = f(g, 5)"
    var executed = false
    val flix = createFlix()
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    val tpeF = flix.mkFunctionType(Array(tpeG, flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: Value.HookClosure, y: JInt) = x.hook.asInstanceOf[Ast.Hook.Unsafe].inv(Array(y))
    def nativeG(x: JInt): JInt = { executed = true; x + 1 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpeF, nativeF _)
      .addHookUnsafe("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(6))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.12") {
    import HookUnsafeHelpers._
    val input = "fn h: Int = (f(g))(40)"
    var executed = false
    val flix = createFlix()
    val tpeG = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    val tpeF = flix.mkFunctionType(Array(tpeG), tpeG)
    def nativeF(x: Value.HookClosure): Value.HookClosure = x
    def nativeG(x: JInt): JInt = { executed = true; x + 5 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpeF, nativeF _)
      .addHookUnsafe("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(45))(result)
    assert(executed)
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  // TODO: mkTagType should be taking an IType instead of a Type?
  ignore("Expression.Hook - Hook.Unsafe.13") {
    import HookUnsafeHelpers._
    val input =
      """enum Val { case Val(Int) }
        |fn g: Val = f(111)
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tagTpe = flix.mkTagType("Val", "Val", Type.Int32)
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkEnumType("Val", Array(tagTpe)))
    def nativeF(x: JInt): Value.Tag = { executed = true; Value.mkTag(Name.Resolved.mk("Val"), "Val", x) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt32(111)))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.14") {
    import HookUnsafeHelpers._
    val input = """fn g: (Int, Int, Str, Int, Bool, ()) = f(24, 53, "qwertyuiop", 9978, false, ())"""
    var executed = false
    val flix = createFlix()
    val tpes = Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkStrType, flix.mkInt32Type, flix.mkBoolType, flix.mkUnitType)
    val tpe = flix.mkFunctionType(tpes, flix.mkTupleType(tpes))
    def nativeF(a: JInt, b: JInt, c: String, d: JInt, e: JBool, f: Value.Unit.type): Value.Tuple = {
      executed = true
      Value.Tuple(Array(a, b, c, d, e, f))
    }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
    .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.Tuple(Array(Value.mkInt32(24), Value.mkInt32(53), Value.mkStr("qwertyuiop"), Value.mkInt32(9978), Value.False, Value.Unit)))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.15") {
    import HookUnsafeHelpers._
    val input = "fn g: Set[Int] = f(24, 53, 24)"
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type, flix.mkInt32Type), flix.mkSetType(flix.mkInt32Type))
    def nativeF(x: JInt, y: JInt, z: JInt): Set[JInt] = { executed = true; Set(x, y, z) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkSet(Set(Value.mkInt32(24), Value.mkInt32(53), Value.mkInt32(24))))(result)
    assert(executed)
  }

  test("Expression.Hook - Hook.Unsafe.16") {
    import HookUnsafeHelpers._
    val input = "fn h: Native = g(f(999))"
    var executed = false
    val flix = createFlix()
    val tpeF = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    val tpeG = flix.mkFunctionType(Array(flix.mkNativeType), flix.mkNativeType)
    def nativeF(x: JInt): MyObject = MyObject(x)
    def nativeG(o: MyObject): MyObject = {
      executed = true
      MyObject(o.x + 1)
    }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpeF, nativeF _)
      .addHookUnsafe("g", tpeG, nativeG _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(MyObject(1000))(result)
    assert(executed)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Unary                                                        //
  // UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unary - UnaryOperator.LogicalNot.01") {
    val input = "fn f: Bool = !true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Unary - UnaryOperator.LogicalNot.02") {
    val input = "fn f: Bool = !false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Unary - UnaryOperator.Plus.01") {
    val input =
      s"""fn f01: Int = +0
         |fn f02: Int = +36000
         |fn f03: Int = +(-36000)
         |fn f04: Int = +${Int.MaxValue}
         |fn f05: Int = +${Int.MinValue}
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(36000))(result02)
    assertResult(Value.mkInt32(-36000))(result03)
    assertResult(Value.mkInt32(Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.02") {
    val input =
      s"""fn f01: Int8 = +0i8
         |fn f02: Int8 = +36i8
         |fn f03: Int8 = +(-36i8)
         |fn f04: Int8 = +${Byte.MaxValue}i8
         |fn f05: Int8 = +${Byte.MinValue}i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(36))(result02)
    assertResult(Value.mkInt8(-36))(result03)
    assertResult(Value.mkInt8(Byte.MaxValue))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.03") {
    val input =
      s"""fn f01: Int16 = +0i16
         |fn f02: Int16 = +3600i16
         |fn f03: Int16 = +(-3600i16)
         |fn f04: Int16 = +${Short.MaxValue}i16
         |fn f05: Int16 = +${Short.MinValue}i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(3600))(result02)
    assertResult(Value.mkInt16(-3600))(result03)
    assertResult(Value.mkInt16(Short.MaxValue))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.04") {
    val input =
      s"""fn f01: Int32 = +0i32
         |fn f02: Int32 = +36000i32
         |fn f03: Int32 = +(-36000i32)
         |fn f04: Int32 = +${Int.MaxValue}i32
         |fn f05: Int32 = +${Int.MinValue}i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(36000))(result02)
    assertResult(Value.mkInt32(-36000))(result03)
    assertResult(Value.mkInt32(Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.05") {
    val input =
      s"""fn f01: Int64 = +0i64
         |fn f02: Int64 = +3600000000i64
         |fn f03: Int64 = +(-3600000000i64)
         |fn f04: Int64 = +${Long.MaxValue}i64
         |fn f05: Int64 = +${Long.MinValue}i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(3600000000L))(result02)
    assertResult(Value.mkInt64(-3600000000L))(result03)
    assertResult(Value.mkInt64(Long.MaxValue))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.Minus.01") {
    val input =
      s"""fn f01: Int = -0
         |fn f02: Int = -36000
         |fn f03: Int = -(-36000)
         |fn f04: Int = -${Int.MaxValue}
         |fn f05: Int = -${Int.MinValue}
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(-36000))(result02)
    assertResult(Value.mkInt32(36000))(result03)
    assertResult(Value.mkInt32(-Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.02") {
    val input =
      s"""fn f01: Int8 = -0i8
         |fn f02: Int8 = -36i8
         |fn f03: Int8 = -(-36i8)
         |fn f04: Int8 = -${Byte.MaxValue}i8
         |fn f05: Int8 = -${Byte.MinValue}i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(-36))(result02)
    assertResult(Value.mkInt8(36))(result03)
    assertResult(Value.mkInt8(-Byte.MaxValue))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.03") {
    val input =
      s"""fn f01: Int16 = -0i16
         |fn f02: Int16 = -3600i16
         |fn f03: Int16 = -(-3600i16)
         |fn f04: Int16 = -${Short.MaxValue}i16
         |fn f05: Int16 = -${Short.MinValue}i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(-3600))(result02)
    assertResult(Value.mkInt16(3600))(result03)
    assertResult(Value.mkInt16(-Short.MaxValue))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.04") {
    val input =
      s"""fn f01: Int32 = -0i32
         |fn f02: Int32 = -36000i32
         |fn f03: Int32 = -(-36000i32)
         |fn f04: Int32 = -${Int.MaxValue}i32
         |fn f05: Int32 = -${Int.MinValue}i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(-36000))(result02)
    assertResult(Value.mkInt32(36000))(result03)
    assertResult(Value.mkInt32(-Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.05") {
    val input =
      s"""fn f01: Int64 = -0i64
         |fn f02: Int64 = -3600000000i64
         |fn f03: Int64 = -(-3600000000i64)
         |fn f04: Int64 = -${Long.MaxValue}i64
         |fn f05: Int64 = -${Long.MinValue}i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(-3600000000L))(result02)
    assertResult(Value.mkInt64(3600000000L))(result03)
    assertResult(Value.mkInt64(-Long.MaxValue))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.01") {
    val input =
      s"""fn f01: Int = ~0
         |fn f02: Int = ~1
         |fn f03: Int = ~(-1)
         |fn f04: Int = ~36000
         |fn f05: Int = ~(-36000)
         |fn f06: Int = ~${Int.MaxValue}
         |fn f07: Int = ~${Int.MinValue}
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(-2))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-36001))(result04)
    assertResult(Value.mkInt32(35999))(result05)
    assertResult(Value.mkInt32(Int.MinValue))(result06)
    assertResult(Value.mkInt32(Int.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.02") {
    val input =
      s"""fn f01: Int8 = ~0i8
         |fn f02: Int8 = ~1i8
         |fn f03: Int8 = ~(-1i8)
         |fn f04: Int8 = ~42i8
         |fn f05: Int8 = ~(-42i8)
         |fn f06: Int8 = ~${Byte.MaxValue}i8
         |fn f07: Int8 = ~${Byte.MinValue}i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt8(-1))(result01)
    assertResult(Value.mkInt8(-2))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(-43))(result04)
    assertResult(Value.mkInt8(41))(result05)
    assertResult(Value.mkInt8(Byte.MinValue))(result06)
    assertResult(Value.mkInt8(Byte.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.03") {
    val input =
      s"""fn f01: Int16 = ~0i16
         |fn f02: Int16 = ~1i16
         |fn f03: Int16 = ~(-1i16)
         |fn f04: Int16 = ~420i16
         |fn f05: Int16 = ~(-420i16)
         |fn f06: Int16 = ~${Short.MaxValue}i16
         |fn f07: Int16 = ~${Short.MinValue}i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt16(-1))(result01)
    assertResult(Value.mkInt16(-2))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(-421))(result04)
    assertResult(Value.mkInt16(419))(result05)
    assertResult(Value.mkInt16(Short.MinValue))(result06)
    assertResult(Value.mkInt16(Short.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.04") {
    val input =
      s"""fn f01: Int32 = ~0i32
         |fn f02: Int32 = ~1i32
         |fn f03: Int32 = ~(-1i32)
         |fn f04: Int32 = ~36000i32
         |fn f05: Int32 = ~(-36000i32)
         |fn f06: Int32 = ~${Int.MaxValue}i32
         |fn f07: Int32 = ~${Int.MinValue}i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(-2))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-36001))(result04)
    assertResult(Value.mkInt32(35999))(result05)
    assertResult(Value.mkInt32(Int.MinValue))(result06)
    assertResult(Value.mkInt32(Int.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.05") {
    val input =
      s"""fn f01: Int64 = ~0i64
         |fn f02: Int64 = ~1i64
         |fn f03: Int64 = ~(-1i64)
         |fn f04: Int64 = ~10000000000i64
         |fn f05: Int64 = ~(-10000000000i64)
         |fn f06: Int64 = ~${Long.MaxValue}i64
         |fn f07: Int64 = ~${Long.MinValue}i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt64(-1))(result01)
    assertResult(Value.mkInt64(-2))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(-10000000001L))(result04)
    assertResult(Value.mkInt64(9999999999L))(result05)
    assertResult(Value.mkInt64(Long.MinValue))(result06)
    assertResult(Value.mkInt64(Long.MaxValue))(result07)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Arithmetic)                                          //
  // BinaryOperator.{Plus,Minus,Times,Divide,Modulo}                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.Plus.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} + 1
         |fn f02: Int = 100000 + 400000
         |fn f03: Int = -400000 + 100000
         |fn f04: Int = -100000 + 400000
         |fn f05: Int = ${Int.MinValue} + -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MinValue))(result01)
    assertResult(Value.mkInt32(500000))(result02)
    assertResult(Value.mkInt32(-300000))(result03)
    assertResult(Value.mkInt32(300000))(result04)
    assertResult(Value.mkInt32(Int.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue}i8 + 1i8
         |fn f02: Int8 = 10i8 + 40i8
         |fn f03: Int8 = -40i8 + 10i8
         |fn f04: Int8 = -10i8 + 40i8
         |fn f05: Int8 = ${Byte.MinValue}i8 + -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MinValue))(result01)
    assertResult(Value.mkInt8(50))(result02)
    assertResult(Value.mkInt8(-30))(result03)
    assertResult(Value.mkInt8(30))(result04)
    assertResult(Value.mkInt8(Byte.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue}i16 + 1i16
         |fn f02: Int16 = 1000i16 + 4000i16
         |fn f03: Int16 = -4000i16 + 1000i16
         |fn f04: Int16 = -1000i16 + 4000i16
         |fn f05: Int16 = ${Short.MinValue}i16 + -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MinValue))(result01)
    assertResult(Value.mkInt16(5000))(result02)
    assertResult(Value.mkInt16(-3000))(result03)
    assertResult(Value.mkInt16(3000))(result04)
    assertResult(Value.mkInt16(Short.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue}i32 + 1i32
         |fn f02: Int32 = 100000i32 + 400000i32
         |fn f03: Int32 = -400000i32 + 100000i32
         |fn f04: Int32 = -100000i32 + 400000i32
         |fn f05: Int32 = ${Int.MinValue}i32 + -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MinValue))(result01)
    assertResult(Value.mkInt32(500000))(result02)
    assertResult(Value.mkInt32(-300000))(result03)
    assertResult(Value.mkInt32(300000))(result04)
    assertResult(Value.mkInt32(Int.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue}i64 + 1i64
         |fn f02: Int64 = 10000000000i64 + 40000000000i64
         |fn f03: Int64 = -40000000000i64 + 10000000000i64
         |fn f04: Int64 = -10000000000i64 + 40000000000i64
         |fn f05: Int64 = ${Long.MinValue}i64 + -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MinValue))(result01)
    assertResult(Value.mkInt64(50000000000L))(result02)
    assertResult(Value.mkInt64(-30000000000L))(result03)
    assertResult(Value.mkInt64(30000000000L))(result04)
    assertResult(Value.mkInt64(Long.MaxValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Minus.01") {
    val input =
      s"""fn f01: Int = ${Int.MinValue} - 1
         |fn f02: Int = 400000 - 100000
         |fn f03: Int = -400000 - 100000
         |fn f04: Int = -100000 - 400000
         |fn f05: Int = ${Int.MaxValue} - -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(300000))(result02)
    assertResult(Value.mkInt32(-500000))(result03)
    assertResult(Value.mkInt32(-500000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MinValue}i8 - 1i8
         |fn f02: Int8 = 40i8 - 10i8
         |fn f03: Int8 = -40i8 - 10i8
         |fn f04: Int8 = -10i8 - 40i8
         |fn f05: Int8 = ${Byte.MaxValue}i8 - -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result01)
    assertResult(Value.mkInt8(30))(result02)
    assertResult(Value.mkInt8(-50))(result03)
    assertResult(Value.mkInt8(-50))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MinValue}i16 - 1i16
         |fn f02: Int16 = 4000i16 - 1000i16
         |fn f03: Int16 = -4000i16 - 1000i16
         |fn f04: Int16 = -1000i16 - 4000i16
         |fn f05: Int16 = ${Short.MaxValue}i16 - -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MaxValue))(result01)
    assertResult(Value.mkInt16(3000))(result02)
    assertResult(Value.mkInt16(-5000))(result03)
    assertResult(Value.mkInt16(-5000))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MinValue}i32 - 1i32
         |fn f02: Int32 = 400000i32 - 100000i32
         |fn f03: Int32 = -400000i32 - 100000i32
         |fn f04: Int32 = -100000i32 - 400000i32
         |fn f05: Int32 = ${Int.MaxValue}i32 - -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(300000))(result02)
    assertResult(Value.mkInt32(-500000))(result03)
    assertResult(Value.mkInt32(-500000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MinValue}i64 - 1i64
         |fn f02: Int64 = 40000000000i64 - 10000000000i64
         |fn f03: Int64 = -40000000000i64 - 10000000000i64
         |fn f04: Int64 = -10000000000i64 - 40000000000i64
         |fn f05: Int64 = ${Long.MaxValue}i64 - -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MaxValue))(result01)
    assertResult(Value.mkInt64(30000000000L))(result02)
    assertResult(Value.mkInt64(-50000000000L))(result03)
    assertResult(Value.mkInt64(-50000000000L))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Times.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} * 2
         |fn f02: Int = 300 * 200
         |fn f03: Int = -200 * 300
         |fn f04: Int = -200 * -300
         |fn f05: Int = ${Int.MinValue} * -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-2))(result01)
    assertResult(Value.mkInt32(60000))(result02)
    assertResult(Value.mkInt32(-60000))(result03)
    assertResult(Value.mkInt32(60000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue}i8 * 2i8
         |fn f02: Int8 = 3i8 * 2i8
         |fn f03: Int8 = -2i8 * 3i8
         |fn f04: Int8 = -2i8 * -3i8
         |fn f05: Int8 = ${Byte.MinValue}i8 * -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(-2))(result01)
    assertResult(Value.mkInt8(6))(result02)
    assertResult(Value.mkInt8(-6))(result03)
    assertResult(Value.mkInt8(6))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue}i16 * 2i16
         |fn f02: Int16 = 30i16 * 20i16
         |fn f03: Int16 = -20i16 * 30i16
         |fn f04: Int16 = -20i16 * -30i16
         |fn f05: Int16 = ${Short.MinValue}i16 * -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(-2))(result01)
    assertResult(Value.mkInt16(600))(result02)
    assertResult(Value.mkInt16(-600))(result03)
    assertResult(Value.mkInt16(600))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue}i32 * 2i32
         |fn f02: Int32 = 300i32 * 200i32
         |fn f03: Int32 = -200i32 * 300i32
         |fn f04: Int32 = -200i32 * -300i32
         |fn f05: Int32 = ${Int.MinValue}i32 * -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-2))(result01)
    assertResult(Value.mkInt32(60000))(result02)
    assertResult(Value.mkInt32(-60000))(result03)
    assertResult(Value.mkInt32(60000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue}i64 * 2i64
         |fn f02: Int64 = 300000i64 * 200000i64
         |fn f03: Int64 = -200000i64 * 300000i64
         |fn f04: Int64 = -200000i64 * -300000i64
         |fn f05: Int64 = ${Long.MinValue}i64 * -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(-2))(result01)
    assertResult(Value.mkInt64(60000000000L))(result02)
    assertResult(Value.mkInt64(-60000000000L))(result03)
    assertResult(Value.mkInt64(60000000000L))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Divide.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} / 1
         |fn f02: Int = 1200000 / 3
         |fn f03: Int = -1200000 / 3
         |fn f04: Int = -3 / 1200000
         |fn f05: Int = ${Int.MinValue} / -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(400000))(result02)
    assertResult(Value.mkInt32(-400000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue}i8 / 1i8
         |fn f02: Int8 = 12i8 / 3i8
         |fn f03: Int8 = -12i8 / 3i8
         |fn f04: Int8 = -3i8 / 12i8
         |fn f05: Int8 = ${Byte.MinValue}i8 / -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result01)
    assertResult(Value.mkInt8(4))(result02)
    assertResult(Value.mkInt8(-4))(result03)
    assertResult(Value.mkInt8(0))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue}i16 / 1i16
         |fn f02: Int16 = 12000i16 / 3i16
         |fn f03: Int16 = -12000i16 / 3i16
         |fn f04: Int16 = -3i16 / 12000i16
         |fn f05: Int16 = ${Short.MinValue}i16 / -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MaxValue))(result01)
    assertResult(Value.mkInt16(4000))(result02)
    assertResult(Value.mkInt16(-4000))(result03)
    assertResult(Value.mkInt16(0))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue}i32 / 1i32
         |fn f02: Int32 = 1200000i32 / 3i32
         |fn f03: Int32 = -1200000i32 / 3i32
         |fn f04: Int32 = -3i32 / 1200000i32
         |fn f05: Int32 = ${Int.MinValue}i32 / -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(400000))(result02)
    assertResult(Value.mkInt32(-400000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue}i64 / 1i64
         |fn f02: Int64 = 120000000000i64 / 3i64
         |fn f03: Int64 = -120000000000i64 / 3i64
         |fn f04: Int64 = -3i64 / 120000000000i64
         |fn f05: Int64 = ${Long.MinValue}i64 / -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MaxValue))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(-40000000000L))(result03)
    assertResult(Value.mkInt64(0))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Modulo.01") {
    val input =
      s"""fn f01: Int = 1200000 % 200000
         |fn f02: Int = 1200000 % 500000
         |fn f03: Int = -1200000 % 500000
         |fn f04: Int = 1200000 % -500000
         |fn f05: Int = -1200000 % -500000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(200000))(result02)
    assertResult(Value.mkInt32(-200000))(result03)
    assertResult(Value.mkInt32(200000))(result04)
    assertResult(Value.mkInt32(-200000))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.02") {
    val input =
      s"""fn f01: Int8 = 12i8 % 2i8
         |fn f02: Int8 = 12i8 % 5i8
         |fn f03: Int8 = -12i8 % 5i8
         |fn f04: Int8 = 12i8 % -5i8
         |fn f05: Int8 = -12i8 % -5i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(2))(result02)
    assertResult(Value.mkInt8(-2))(result03)
    assertResult(Value.mkInt8(2))(result04)
    assertResult(Value.mkInt8(-2))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.03") {
    val input =
      s"""fn f01: Int16 = 12000i16 % 2000i16
         |fn f02: Int16 = 12000i16 % 5000i16
         |fn f03: Int16 = -12000i16 % 5000i16
         |fn f04: Int16 = 12000i16 % -5000i16
         |fn f05: Int16 = -12000i16 % -5000i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(2000))(result02)
    assertResult(Value.mkInt16(-2000))(result03)
    assertResult(Value.mkInt16(2000))(result04)
    assertResult(Value.mkInt16(-2000))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.04") {
    val input =
      s"""fn f01: Int32 = 1200000i32 % 200000i32
         |fn f02: Int32 = 1200000i32 % 500000i32
         |fn f03: Int32 = -1200000i32 % 500000i32
         |fn f04: Int32 = 1200000i32 % -500000i32
         |fn f05: Int32 = -1200000i32 % -500000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(200000))(result02)
    assertResult(Value.mkInt32(-200000))(result03)
    assertResult(Value.mkInt32(200000))(result04)
    assertResult(Value.mkInt32(-200000))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.05") {
    val input =
      s"""fn f01: Int64 = 120000000000i64 % 20000000000i64
         |fn f02: Int64 = 120000000000i64 % 50000000000i64
         |fn f03: Int64 = -120000000000i64 % 50000000000i64
         |fn f04: Int64 = 120000000000i64 % -50000000000i64
         |fn f05: Int64 = -120000000000i64 % -50000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(20000000000L))(result02)
    assertResult(Value.mkInt64(-20000000000L))(result03)
    assertResult(Value.mkInt64(20000000000L))(result04)
    assertResult(Value.mkInt64(-20000000000L))(result05)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Comparison)                                          //
  // BinaryOperator.{Less,LessEqual,Greater,GreaterEqual}                    //
  // BinaryOperator.{Equal,NotEqual}                                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.Less.01") {
    val input =
      s"""fn f01: Bool = 120000 < 30000
         |fn f02: Bool = 30000 < 120000
         |fn f03: Bool = 30000 < 30000
         |fn f04: Bool = -120000 < -30000
         |fn f05: Bool = -30000 < -120000
         |fn f06: Bool = -30000 < -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Less.02") {
    val input =
      s"""fn f01: Bool = 12i8 < 3i8
         |fn f02: Bool = 3i8 < 12i8
         |fn f03: Bool = 3i8 < 3i8
         |fn f04: Bool = -12i8 < -3i8
         |fn f05: Bool = -3i8 < -12i8
         |fn f06: Bool = -3i8 < -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Less.03") {
    val input =
      s"""fn f01: Bool = 1200i16 < 300i16
         |fn f02: Bool = 300i16 < 1200i16
         |fn f03: Bool = 300i16 < 300i16
         |fn f04: Bool = -1200i16 < -300i16
         |fn f05: Bool = -300i16 < -1200i16
         |fn f06: Bool = -300i16 < -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Less.04") {
    val input =
      s"""fn f01: Bool = 120000i32 < 30000i32
         |fn f02: Bool = 30000i32 < 120000i32
         |fn f03: Bool = 30000i32 < 30000i32
         |fn f04: Bool = -120000i32 < -30000i32
         |fn f05: Bool = -30000i32 < -120000i32
         |fn f06: Bool = -30000i32 < -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Less.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 < 3000000000i64
         |fn f02: Bool = 3000000000i64 < 12000000000i64
         |fn f03: Bool = 3000000000i64 < 3000000000i64
         |fn f04: Bool = -12000000000i64 < -3000000000i64
         |fn f05: Bool = -3000000000i64 < -12000000000i64
         |fn f06: Bool = -3000000000i64 < -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  test("Expression.Binary - BinaryOperator.LessEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 <= 30000
         |fn f02: Bool = 30000 <= 120000
         |fn f03: Bool = 30000 <= 30000
         |fn f04: Bool = -120000 <= -30000
         |fn f05: Bool = -30000 <= -120000
         |fn f06: Bool = -30000 <= -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.LessEqual.02") {
    val input =
      s"""fn f01: Bool = 12i8 <= 3i8
         |fn f02: Bool = 3i8 <= 12i8
         |fn f03: Bool = 3i8 <= 3i8
         |fn f04: Bool = -12i8 <= -3i8
         |fn f05: Bool = -3i8 <= -12i8
         |fn f06: Bool = -3i8 <= -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.LessEqual.03") {
    val input =
      s"""fn f01: Bool = 1200i16 <= 300i16
         |fn f02: Bool = 300i16 <= 1200i16
         |fn f03: Bool = 300i16 <= 300i16
         |fn f04: Bool = -1200i16 <= -300i16
         |fn f05: Bool = -300i16 <= -1200i16
         |fn f06: Bool = -300i16 <= -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.LessEqual.04") {
    val input =
      s"""fn f01: Bool = 120000i32 <= 30000i32
         |fn f02: Bool = 30000i32 <= 120000i32
         |fn f03: Bool = 30000i32 <= 30000i32
         |fn f04: Bool = -120000i32 <= -30000i32
         |fn f05: Bool = -30000i32 <= -120000i32
         |fn f06: Bool = -30000i32 <= -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.LessEqual.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 <= 3000000000i64
         |fn f02: Bool = 3000000000i64 <= 12000000000i64
         |fn f03: Bool = 3000000000i64 <= 3000000000i64
         |fn f04: Bool = -12000000000i64 <= -3000000000i64
         |fn f05: Bool = -3000000000i64 <= -12000000000i64
         |fn f06: Bool = -3000000000i64 <= -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.Greater.01") {
    val input =
      s"""fn f01: Bool = 120000 > 30000
         |fn f02: Bool = 30000 > 120000
         |fn f03: Bool = 30000 > 30000
         |fn f04: Bool = -120000 > -30000
         |fn f05: Bool = -30000 > -120000
         |fn f06: Bool = -30000 > -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Greater.02") {
    val input =
      s"""fn f01: Bool = 12i8 > 3i8
         |fn f02: Bool = 3i8 > 12i8
         |fn f03: Bool = 3i8 > 3i8
         |fn f04: Bool = -12i8 > -3i8
         |fn f05: Bool = -3i8 > -12i8
         |fn f06: Bool = -3i8 > -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Greater.03") {
    val input =
      s"""fn f01: Bool = 1200i16 > 300i16
         |fn f02: Bool = 300i16 > 1200i16
         |fn f03: Bool = 300i16 > 300i16
         |fn f04: Bool = -1200i16 > -300i16
         |fn f05: Bool = -300i16 > -1200i16
         |fn f06: Bool = -300i16 > -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Greater.04") {
    val input =
      s"""fn f01: Bool = 120000i32 > 30000i32
         |fn f02: Bool = 30000i32 > 120000i32
         |fn f03: Bool = 30000i32 > 30000i32
         |fn f04: Bool = -120000i32 > -30000i32
         |fn f05: Bool = -30000i32 > -120000i32
         |fn f06: Bool = -30000i32 > -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Greater.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 > 3000000000i64
         |fn f02: Bool = 3000000000i64 > 12000000000i64
         |fn f03: Bool = 3000000000i64 > 3000000000i64
         |fn f04: Bool = -12000000000i64 > -3000000000i64
         |fn f05: Bool = -3000000000i64 > -12000000000i64
         |fn f06: Bool = -3000000000i64 > -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 >= 30000
         |fn f02: Bool = 30000 >= 120000
         |fn f03: Bool = 30000 >= 30000
         |fn f04: Bool = -120000 >= -30000
         |fn f05: Bool = -30000 >= -120000
         |fn f06: Bool = -30000 >= -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.GreaterEqual.02") {
    val input =
      s"""fn f01: Bool = 12i8 >= 3i8
         |fn f02: Bool = 3i8 >= 12i8
         |fn f03: Bool = 3i8 >= 3i8
         |fn f04: Bool = -12i8 >= -3i8
         |fn f05: Bool = -3i8 >= -12i8
         |fn f06: Bool = -3i8 >= -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.GreaterEqual.03") {
    val input =
      s"""fn f01: Bool = 1200i16 >= 300i16
         |fn f02: Bool = 300i16 >= 1200i16
         |fn f03: Bool = 300i16 >= 300i16
         |fn f04: Bool = -1200i16 >= -300i16
         |fn f05: Bool = -300i16 >= -1200i16
         |fn f06: Bool = -300i16 >= -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.GreaterEqual.04") {
    val input =
      s"""fn f01: Bool = 120000i32 >= 30000i32
         |fn f02: Bool = 30000i32 >= 120000i32
         |fn f03: Bool = 30000i32 >= 30000i32
         |fn f04: Bool = -120000i32 >= -30000i32
         |fn f05: Bool = -30000i32 >= -120000i32
         |fn f06: Bool = -30000i32 >= -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.GreaterEqual.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 >= 3000000000i64
         |fn f02: Bool = 3000000000i64 >= 12000000000i64
         |fn f03: Bool = 3000000000i64 >= 3000000000i64
         |fn f04: Bool = -12000000000i64 >= -3000000000i64
         |fn f05: Bool = -3000000000i64 >= -12000000000i64
         |fn f06: Bool = -3000000000i64 >= -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.Equal.01") {
    val input =
      s"""fn f01: Bool = 120000 == 30000
         |fn f02: Bool = 30000 == 120000
         |fn f03: Bool = 30000 == 30000
         |fn f04: Bool = -120000 == -30000
         |fn f05: Bool = -30000 == -120000
         |fn f06: Bool = -30000 == -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Equal.02") {
    val input =
      s"""fn f01: Bool = 12i8 == 3i8
         |fn f02: Bool = 3i8 == 12i8
         |fn f03: Bool = 3i8 == 3i8
         |fn f04: Bool = -12i8 == -3i8
         |fn f05: Bool = -3i8 == -12i8
         |fn f06: Bool = -3i8 == -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Equal.03") {
    val input =
      s"""fn f01: Bool = 1200i16 == 300i16
         |fn f02: Bool = 300i16 == 1200i16
         |fn f03: Bool = 300i16 == 300i16
         |fn f04: Bool = -1200i16 == -300i16
         |fn f05: Bool = -300i16 == -1200i16
         |fn f06: Bool = -300i16 == -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Equal.04") {
    val input =
      s"""fn f01: Bool = 120000i32 == 30000i32
         |fn f02: Bool = 30000i32 == 120000i32
         |fn f03: Bool = 30000i32 == 30000i32
         |fn f04: Bool = -120000i32 == -30000i32
         |fn f05: Bool = -30000i32 == -120000i32
         |fn f06: Bool = -30000i32 == -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.Equal.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 == 3000000000i64
         |fn f02: Bool = 3000000000i64 == 12000000000i64
         |fn f03: Bool = 3000000000i64 == 3000000000i64
         |fn f04: Bool = -12000000000i64 == -3000000000i64
         |fn f05: Bool = -3000000000i64 == -12000000000i64
         |fn f06: Bool = -3000000000i64 == -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.NotEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 != 30000
         |fn f02: Bool = 30000 != 120000
         |fn f03: Bool = 30000 != 30000
         |fn f04: Bool = -120000 != -30000
         |fn f05: Bool = -30000 != -120000
         |fn f06: Bool = -30000 != -30000
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.NotEqual.02") {
    val input =
      s"""fn f01: Bool = 12i8 != 3i8
         |fn f02: Bool = 3i8 != 12i8
         |fn f03: Bool = 3i8 != 3i8
         |fn f04: Bool = -12i8 != -3i8
         |fn f05: Bool = -3i8 != -12i8
         |fn f06: Bool = -3i8 != -3i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.NotEqual.03") {
    val input =
      s"""fn f01: Bool = 1200i16 != 300i16
         |fn f02: Bool = 300i16 != 1200i16
         |fn f03: Bool = 300i16 != 300i16
         |fn f04: Bool = -1200i16 != -300i16
         |fn f05: Bool = -300i16 != -1200i16
         |fn f06: Bool = -300i16 != -300i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.NotEqual.04") {
    val input =
      s"""fn f01: Bool = 120000i32 != 30000i32
         |fn f02: Bool = 30000i32 != 120000i32
         |fn f03: Bool = 30000i32 != 30000i32
         |fn f04: Bool = -120000i32 != -30000i32
         |fn f05: Bool = -30000i32 != -120000i32
         |fn f06: Bool = -30000i32 != -30000i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  ignore("Expression.Binary - BinaryOperator.NotEqual.05") {
    val input =
      s"""fn f01: Bool = 12000000000i64 != 3000000000i64
         |fn f02: Bool = 3000000000i64 != 12000000000i64
         |fn f03: Bool = 3000000000i64 != 3000000000i64
         |fn f04: Bool = -12000000000i64 != -3000000000i64
         |fn f05: Bool = -3000000000i64 != -12000000000i64
         |fn f06: Bool = -3000000000i64 != -3000000000i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Logical)                                             //
  // BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.LogicalAnd.01") {
    val input = "fn f: Bool = true && true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.02") {
    val input = "fn f: Bool = true && false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.03") {
    val input = "fn f: Bool = false && false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.04") {
    val input = "fn f: Bool = false && true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.05") {
    val input = "fn f: Bool = false && ???: Bool"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.06") {
    val input = "fn f: Bool = true && ???: Bool"
    intercept[RuntimeException] { getModel(input) }
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.01") {
    val input = "fn f: Bool = true || true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.02") {
    val input = "fn f: Bool = true || false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.03") {
    val input = "fn f: Bool = false || false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.04") {
    val input = "fn f: Bool = false || true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.05") {
    val input = "fn f: Bool = true || ???: Bool"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.06") {
    val input = "fn f: Bool = false || ???: Bool"
    intercept[RuntimeException] { getModel(input) }
  }

  test("Expression.Binary - BinaryOperator.Implication.01") {
    val input = "fn f: Bool = true ==> true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.02") {
    val input = "fn f: Bool = true ==> false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.03") {
    val input = "fn f: Bool = false ==> false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.04") {
    val input = "fn f: Bool = false ==> true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.05") {
    val input = "fn f: Bool = false ==> ???: Bool"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.06") {
    val input = "fn f: Bool = true ==> ???: Bool"
    intercept[RuntimeException] { getModel(input) }
  }

  test("Expression.Binary - BinaryOperator.Biconditional.01") {
    val input = "fn f: Bool = true <==> true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.02") {
    val input = "fn f: Bool = true <==> false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.03") {
    val input = "fn f: Bool = false <==> false"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.04") {
    val input = "fn f: Bool = false <==> true"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Bitwise)                                             //
  // BinaryOperator.{BitwiseAnd,BitwiseOr,BitwiseXor}                        //
  // BinaryOperator.{BitwiseLeftShift,BitwiseRightShift}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.BitwiseAnd.01") {
    val input =
      s"""fn f01: Int = 40000 & ${0xFFFFFFFF}
         |fn f02: Int = 40000 & 40000
         |fn f03: Int = 40000 & 0
         |fn f04: Int = ${0xFFFFFFFF} & ${0xFFFFFFFF}
         |fn f05: Int = -1 & -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(40000))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.02") {
    val input =
      s"""fn f01: Int8 = 40i8 & ${0xFF.toByte}i8
         |fn f02: Int8 = 40i8 & 40i8
         |fn f03: Int8 = 40i8 & 0i8
         |fn f04: Int8 = ${0xFF.toByte}i8 & ${0xFF.toByte}i8
         |fn f05: Int8 = -1i8 & -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(40))(result01)
    assertResult(Value.mkInt8(40))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(0xFF.toByte))(result04)
    assertResult(Value.mkInt8(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.03") {
    val input =
      s"""fn f01: Int16 = 400i16 & ${0xFFFF.toShort}i16
         |fn f02: Int16 = 400i16 & 400i16
         |fn f03: Int16 = 400i16 & 0i16
         |fn f04: Int16 = ${0xFFFF.toShort}i16 & ${0xFFFF.toShort}i16
         |fn f05: Int16 = -1i16 & -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(400))(result01)
    assertResult(Value.mkInt16(400))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(0xFFFF.toShort))(result04)
    assertResult(Value.mkInt16(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.04") {
    val input =
      s"""fn f01: Int32 = 40000i32 & ${0xFFFFFFFF}i32
         |fn f02: Int32 = 40000i32 & 40000i32
         |fn f03: Int32 = 40000i32 & 0i32
         |fn f04: Int32 = ${0xFFFFFFFF}i32 & ${0xFFFFFFFF}i32
         |fn f05: Int32 = -1i32 & -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(40000))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.05") {
    val input =
      s"""fn f01: Int64 = 40000000000i64 & ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f02: Int64 = 40000000000i64 & 40000000000i64
         |fn f03: Int64 = 40000000000i64 & 0i64
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 & ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f05: Int64 = -1i64 & -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(40000000000L))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result04)
    assertResult(Value.mkInt64(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.01") {
    val input =
      s"""fn f01: Int = 40000 | ${0xFFFFFFFF}
         |fn f02: Int = 40000 | 40000
         |fn f03: Int = 40000 | 0
         |fn f04: Int = ${0xFFFFFFFF} | ${0xFFFFFFFF}
         |fn f05: Int = -1 | -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0xFFFFFFFF))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.02") {
    val input =
      s"""fn f01: Int8 = 40i8 | ${0xFF.toByte}i8
         |fn f02: Int8 = 40i8 | 40i8
         |fn f03: Int8 = 40i8 | 0i8
         |fn f04: Int8 = ${0xFF.toByte}i8 | ${0xFF.toByte}i8
         |fn f05: Int8 = -1i8 | -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0xFF.toByte))(result01)
    assertResult(Value.mkInt8(40))(result02)
    assertResult(Value.mkInt8(40))(result03)
    assertResult(Value.mkInt8(0xFF.toByte))(result04)
    assertResult(Value.mkInt8(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.03") {
    val input =
      s"""fn f01: Int16 = 400i16 | ${0xFFFF.toShort}i16
         |fn f02: Int16 = 400i16 | 400i16
         |fn f03: Int16 = 400i16 | 0i16
         |fn f04: Int16 = ${0xFFFF.toShort}i16 | ${0xFFFF.toShort}i16
         |fn f05: Int16 = -1i16 | -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0xFFFF.toShort))(result01)
    assertResult(Value.mkInt16(400))(result02)
    assertResult(Value.mkInt16(400))(result03)
    assertResult(Value.mkInt16(0xFF.toByte))(result04)
    assertResult(Value.mkInt16(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.04") {
    val input =
      s"""fn f01: Int32 = 40000i32 | ${0xFFFFFFFF}i32
         |fn f02: Int32 = 40000i32 | 40000i32
         |fn f03: Int32 = 40000i32 | 0i32
         |fn f04: Int32 = ${0xFFFFFFFF}i32 | ${0xFFFFFFFF}i32
         |fn f05: Int32 = -1i32 | -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0xFFFFFFFF))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.05") {
    val input =
      s"""fn f01: Int64 = 40000000000i64 | ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f02: Int64 = 40000000000i64 | 40000000000i64
         |fn f03: Int64 = 40000000000i64 | 0i64
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 | ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f05: Int64 = -1i64 | -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(40000000000L))(result03)
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result04)
    assertResult(Value.mkInt64(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.01") {
    val input =
      s"""fn f01: Int = 40000 ^ ${0xFFFFFFFF}
         |fn f02: Int = 40000 ^ 40000
         |fn f03: Int = 40000 ^ 0
         |fn f04: Int = ${0xFFFFFFFF} ^ ${0xFFFFFFFF}
         |fn f05: Int = -1 ^ -1
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-40001))(result01)
    assertResult(Value.mkInt32(0))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.02") {
    val input =
      s"""fn f01: Int8 = 40i8 ^ ${0xFF.toByte}i8
         |fn f02: Int8 = 40i8 ^ 40i8
         |fn f03: Int8 = 40i8 ^ 0i8
         |fn f04: Int8 = ${0xFF.toByte}i8 ^ ${0xFF.toByte}i8
         |fn f05: Int8 = -1i8 ^ -1i8
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(-41))(result01)
    assertResult(Value.mkInt8(0))(result02)
    assertResult(Value.mkInt8(40))(result03)
    assertResult(Value.mkInt8(0))(result04)
    assertResult(Value.mkInt8(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.03") {
    val input =
      s"""fn f01: Int16 = 400i16 ^ ${0xFFFF.toShort}i16
         |fn f02: Int16 = 400i16 ^ 400i16
         |fn f03: Int16 = 400i16 ^ 0i16
         |fn f04: Int16 = ${0xFFFF.toShort}i16 ^ ${0xFFFF.toShort}i16
         |fn f05: Int16 = -1i16 ^ -1i16
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(-401))(result01)
    assertResult(Value.mkInt16(0))(result02)
    assertResult(Value.mkInt16(400))(result03)
    assertResult(Value.mkInt16(0))(result04)
    assertResult(Value.mkInt16(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.04") {
    val input =
      s"""fn f01: Int32 = 40000i32 ^ ${0xFFFFFFFF}i32
         |fn f02: Int32 = 40000i32 ^ 40000i32
         |fn f03: Int32 = 40000i32 ^ 0i32
         |fn f04: Int32 = ${0xFFFFFFFF}i32 ^ ${0xFFFFFFFF}i32
         |fn f05: Int32 = -1i32 ^ -1i32
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-40001))(result01)
    assertResult(Value.mkInt32(0))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.05") {
    val input =
      s"""fn f01: Int64 = 40000000000i64 ^ ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f02: Int64 = 40000000000i64 ^ 40000000000i64
         |fn f03: Int64 = 40000000000i64 ^ 0i64
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL}i64 ^ ${0xFFFFFFFFFFFFFFFFL}i64
         |fn f05: Int64 = -1i64 ^ -1i64
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(-40000000001L))(result01)
    assertResult(Value.mkInt64(0))(result02)
    assertResult(Value.mkInt64(40000000000L))(result03)
    assertResult(Value.mkInt64(0))(result04)
    assertResult(Value.mkInt64(0))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.01") {
    val input =
      s"""fn f01: Int = ${0x08} << 0
         |fn f02: Int = ${0x08} << 16
         |fn f03: Int = ${0x08} << 28
         |fn f04: Int = ${0x08} << 29
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(0x08))(result01)
    assertResult(Value.mkInt32(0x00080000))(result02)
    assertResult(Value.mkInt32(Int.MinValue))(result03)
    assertResult(Value.mkInt32(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.02") {
    val input =
      s"""fn f01: Int8 = ${0x08}i8 << 0
         |fn f02: Int8 = ${0x08}i8 << 2
         |fn f03: Int8 = ${0x08}i8 << 4
         |fn f04: Int8 = ${0x08}i8 << 5
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt8(0x08))(result01)
    assertResult(Value.mkInt8(0x20))(result02)
    assertResult(Value.mkInt8(Byte.MinValue))(result03)
    assertResult(Value.mkInt8(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.03") {
    val input =
      s"""fn f01: Int16 = ${0x08}i16 << 0
         |fn f02: Int16 = ${0x08}i16 << 8
         |fn f03: Int16 = ${0x08}i16 << 12
         |fn f04: Int16 = ${0x08}i16 << 13
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt16(0x08))(result01)
    assertResult(Value.mkInt16(0x0800))(result02)
    assertResult(Value.mkInt16(Short.MinValue))(result03)
    assertResult(Value.mkInt16(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.04") {
    val input =
      s"""fn f01: Int32 = ${0x08}i32 << 0
         |fn f02: Int32 = ${0x08}i32 << 16
         |fn f03: Int32 = ${0x08}i32 << 28
         |fn f04: Int32 = ${0x08}i32 << 29
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(0x08))(result01)
    assertResult(Value.mkInt32(0x00080000))(result02)
    assertResult(Value.mkInt32(Int.MinValue))(result03)
    assertResult(Value.mkInt32(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.05") {
    val input =
      s"""fn f01: Int64 = ${0x08}i64 << 0
         |fn f02: Int64 = ${0x08}i64 << 32
         |fn f03: Int64 = ${0x08}i64 << 60
         |fn f04: Int64 = ${0x08}i64 << 61
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt64(0x08))(result01)
    assertResult(Value.mkInt64(0x0000000800000000L))(result02)
    assertResult(Value.mkInt64(Long.MinValue))(result03)
    assertResult(Value.mkInt64(0))(result04)
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.01") {
    val input =
      s"""fn f01: Int = 120000 >> 0
         |fn f02: Int = 120000 >> 2
         |fn f03: Int = 120000 >> 31
         |fn f04: Int = -120000 >> 2
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(120000))(result01)
    assertResult(Value.mkInt32(30000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-30000))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.02") {
    val input =
      s"""fn f01: Int8 = 120i8 >> 0
         |fn f02: Int8 = 120i8 >> 2
         |fn f03: Int8 = 120i8 >> 7
         |fn f04: Int8 = -120i8 >> 2
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt8(120))(result01)
    assertResult(Value.mkInt8(30))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(-30))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.03") {
    val input =
      s"""fn f01: Int16 = 12000i16 >> 0
         |fn f02: Int16 = 12000i16 >> 2
         |fn f03: Int16 = 12000i16 >> 15
         |fn f04: Int16 = -12000i16 >> 2
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt16(12000))(result01)
    assertResult(Value.mkInt16(3000))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(-3000))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.04") {
    val input =
      s"""fn f01: Int32 = 120000i32 >> 0
         |fn f02: Int32 = 120000i32 >> 2
         |fn f03: Int32 = 120000i32 >> 31
         |fn f04: Int32 = -120000i32 >> 2
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(120000))(result01)
    assertResult(Value.mkInt32(30000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-30000))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.05") {
    val input =
      s"""fn f01: Int64 = 12000000000i64 >> 0
         |fn f02: Int64 = 12000000000i64 >> 2
         |fn f03: Int64 = 12000000000i64 >> 63
         |fn f04: Int64 = -12000000000i64 >> 2
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt64(12000000000L))(result01)
    assertResult(Value.mkInt64(3000000000L))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(-3000000000L))(result04)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.IfThenElse                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.IfThenElse.01") {
    val input = "fn f: Int = if (false) 42 + 10 else 42 - 10"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(32))(result)
  }

  test("Expression.IfThenElse.02") {
    val input = "fn f: Int = if (true) 42 + 10 else 42 - 10"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(52))(result)
  }

  test("Expression.IfThenElse.03") {
    val input =
      """fn f(x: Bool): Int = if (x) (if (false) 1 else 2) else (if (true) 3 else 4)
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(2))(result01)
    assertResult(Value.mkInt32(3))(result02)
  }

  test("Expression.IfThenElse.04") {
    val input =
      """fn f(x: Bool): Int = if (if (!x) true else false) 1234 else 5678
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(5678))(result01)
    assertResult(Value.mkInt32(1234))(result02)
  }

  test("Expression.IfThenElse.05") {
    val input =
      """fn f(x: Bool, y: Bool): Int = if (x && y) 1234 else 5678
        |fn g01: Int = f(true, true)
        |fn g02: Int = f(false, true)
        |fn g03: Int = f(true, false)
        |fn g04: Int = f(false, false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
    assertResult(Value.mkInt32(5678))(result03)
    assertResult(Value.mkInt32(5678))(result04)
  }

  test("Expression.IfThenElse.06") {
    val input =
      """fn f(x: Bool, y: Bool): Int = if (x || y) 1234 else 5678
        |fn g01: Int = f(true, true)
        |fn g02: Int = f(false, true)
        |fn g03: Int = f(true, false)
        |fn g04: Int = f(false, false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(1234))(result02)
    assertResult(Value.mkInt32(1234))(result03)
    assertResult(Value.mkInt32(5678))(result04)
  }

  ignore("Expression.IfThenElse.07") {
    val input =
      """fn f(x: Int8, y: Int8): Int8 = if (x < y) 12i8 else 56i8
        |fn g01: Int8 = f(5i8, 24i8)
        |fn g02: Int8 = f(i85, 5i8)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt8(12))(result01)
    assertResult(Value.mkInt8(56))(result02)
  }

  ignore("Expression.IfThenElse.08") {
    val input =
      """fn f(x: Int16, y: Int16): Int16 = if (x <= y) 1234i16 else 5678i16
        |fn g01: Int16 = f(500i16, 500i16)
        |fn g02: Int16 = f(500i16, 200i16)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt16(1234))(result01)
    assertResult(Value.mkInt16(5678))(result02)
  }

  ignore("Expression.IfThenElse.09") {
    val input =
      """fn f(x: Int32, y: Int32): Int32 = if (x > y) 12341234i32 else 56785678i32
        |fn g01: Int32 = f(2400000i32, 500000i32)
        |fn g02: Int32 = f(500000i32, 500000i32)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(12341234))(result01)
    assertResult(Value.mkInt32(56785678))(result02)
  }

  ignore("Expression.IfThenElse.10") {
    val input =
      """fn f(x: Int64, y: Int64): Int64 = if (x >= y) 123412341234i64 else 567856785678i64
        |fn g01: Int64 = f(50000000000i64, 50000000000i64)
        |fn g02: Int64 = f(20000000000i64, 50000000000i64)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt64(123412341234L))(result01)
    assertResult(Value.mkInt64(567856785678L))(result02)
  }

  test("Expression.IfThenElse.11") {
    val input =
      """fn f(x: Int, y: Int): Int = if (x == y) 1234 else 5678
        |fn g01: Int = f(5, 5)
        |fn g02: Int = f(2, 5)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
  }

  test("Expression.IfThenElse.12") {
    val input =
      """fn f(x: Int, y: Int): Int = if (x != y) 1234 else 5678
        |fn g01: Int = f(2, 5)
        |fn g02: Int = f(5, 5)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Let                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Let.01") {
    val input = "fn f: Int = let x = true in 42"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(42))(result)
  }

  ignore("Expression.Let.02") {
    val input = "fn f: Int8 = let x = 42i8 in x"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(42))(result)
  }

  ignore("Expression.Let.03") {
    val input = "fn f: Int16 = let x = 1i16 in x + 2i16"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(3))(result)
  }

  test("Expression.Let.04") {
    val input = """fn f: Str = let x = false in if (x) "abz" else "xyz""""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("xyz"))(result)
  }

  test("Expression.Let.05") {
    val input = "fn f: Int = let x = 14 - 3 in x + 2"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(13))(result)
  }

  test("Expression.Let.06") {
    val input =
      """fn f: Int =
        |  let x = 14 - 3 in
        |    let y = 2 * 4 in
        |      x + y
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(19))(result)
  }

  test("Expression.Let.07") {
    val input =
      """fn f: Int =
        |  let x = 1 in
        |    let y = x + 2 in
        |      let z = y + 3 in
        |        z
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(6))(result)
  }

  test("Expression.Let.08") {
    val input =
      """fn f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337 in
        |    let y = -101010 in
        |      let z = 42 in
        |        y
        |fn g: Int = f(-1337, 101010, -42)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(-101010))(result)
  }

  test("Expression.Let.09") {
    val input =
      """fn f(a: Int, b: Int, c: Int): Int =
        |  let x = 1337 in
        |    let y = -101010 in
        |      let z = 42 in
        |        b
        |fn g: Int = f(-1337, 101010, -42)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(101010))(result)
  }

  ignore("Expression.Let.10") {
    val input = "fn f: Int64 = let x = 0i64 in x"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(0))(result)
  }

  ignore("Expression.Let.11") {
    val input =
      """fn f: Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(-101010))(result)
  }

  ignore("Expression.Let.12") {
    val input =
      """fn f: Int64 =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(-101010))(result)
  }

  ignore("Expression.Let.13") {
    val input =
      """fn f(a: Int64, b: Int64, c: Int64): Int =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
        |fn g: Int = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(-101010))(result)
  }

  ignore("Expression.Let.14") {
    val input =
      """fn f(a: Int32, b: Int64, c: Int64): Int =
        |  let x = 1337i32 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        y
        |fn g: Int = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(-101010))(result)
  }

  ignore("Expression.Let.15") {
    val input =
      """fn f(a: Int64, b: Int64, c: Int64): Int =
        |  let x = 1337i64 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        b
        |fn g: Int = f(-1337i64, 101010i64, -42i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(101010))(result)
  }

  ignore("Expression.Let.16") {
    val input =
      """fn f(a: Int32, b: Int64, c: Int64): Int =
        |  let x = 1337i32 in
        |    let y = -101010i64 in
        |      let z = 42i64 in
        |        b
        |fn g: Int = f(-1337i32, 101010i64, -42i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt64(101010))(result)
  }

  test("Expression.Let.17") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = let x = ConstProp.Val(42) in x
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Val", Value.mkInt32(42)))(result)
  }

  test("Expression.Let.18") {
    val input = "fn f: () = let x = () in x"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Unit)(result)
  }

  test("Expression.Let.19") {
    val input = """fn f: Str = let x = "helloworld" in x"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("helloworld"))(result)
  }

  test("Expression.Let.20") {
    val input = """fn f: (Int, Int) = let x = (123, 456) in x"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(123, 456).map(Value.mkInt32)))(result)
  }

  test("Expression.Let.21") {
    val input = """fn f: Set[Int] = let x = #{9, 99, 999} in x"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkSet(Set(9, 99, 999).map(Value.mkInt32)))(result)
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
        |fn f: ConstProp = ConstProp.Top
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Top", Value.Unit))(result)
  }

  test("Expression.Tag.02") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = ConstProp.Val(42)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Val", Value.mkInt32(42)))(result)
  }

  test("Expression.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = ConstProp.Bot
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Bot", Value.Unit))(result)
  }

  test("Expression.Tag.04") {
    val input =
      """enum Val { case Val(Bool) }
        |fn f: Val = Val.Val(true)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.True))(result)
  }

  test("Expression.Tag.05") {
    val input =
      """enum Val { case Val(Bool) }
        |fn f(x: Bool): Val = Val.Val(x)
        |fn g01: Val = f(true)
        |fn g02: Val = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.True))(result01)
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.False))(result02)
  }

  test("Expression.Tag.06") {
    val input =
      """enum Val { case Val(Str) }
        |fn f: Val = Val.Val("hi")
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkStr("hi")))(result)
  }

  test("Expression.Tag.07") {
    val input =
      """enum Val { case Val(Int, Str) }
        |fn f: Val = Val.Val(1, "one")
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.Tuple(Array(Value.mkInt32(1), "one"))))(result)
  }

  test("Expression.Tag.08") {
    val input =
      """enum Val { case Val(Str) }
        |fn f: Val = Val.Val(if (!(4 != 4)) "foo" else "bar")
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkStr("foo")))(result)
  }

  test("Expression.Tag.09") {
    val input =
      """enum Val { case Val(Str, Int) }
        |fn f: Val = Val.Val("ABC", 20 + 22)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.Tuple(Array("ABC", Value.mkInt32(42)))))(result)
  }

  ignore("Expression.Tag.10") {
    val input =
      """enum Val { case Val(Int8) }
        |fn f: Val = Val.Val(32i8)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt8(32)))(result)
  }

  ignore("Expression.Tag.11") {
    val input =
      """enum Val { case Val(Int16) }
        |fn f: Val = Val.Val(3200i16)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt16(3200)))(result)
  }

  ignore("Expression.Tag.12") {
    val input =
      """enum Val { case Val(Int64) }
        |fn f: Val = Val.Val(320000000000i64)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt64(320000000000L)))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.GetTupleIndex                                                //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Tuple                                                        //
  /////////////////////////////////////////////////////////////////////////////

  ignore("Expression.Tuple.01") {
    val input = "fn f: (Int16, Int32) = (321i16, 5i32)"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(Value.mkInt16(321), Value.mkInt32(5))))(result)
  }

  test("Expression.Tuple.02") {
    val input = "fn f: (Bool, Bool, Bool) = (true, true, false)"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(true, true, false).map(Value.mkBool)))(result)
  }

  test("Expression.Tuple.03") {
    val input = """fn f: (Str, Str, Str, Str) = ("un", "deux", "trois", "quatre")"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array("un", "deux", "trois", "quatre").map(Value.mkStr)))(result)
  }

  ignore("Expression.Tuple.04") {
    val input = """fn f: (Str, Bool, Int64, (), Int8) = ("un", false, 12345i64, (), -2i8)"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(Value.mkStr("un"), Value.False, Value.mkInt64(12345), Value.Unit, Value.mkInt8(-2))))(result)
  }

  test("Expression.Tuple.05") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: (ConstProp, ConstProp) = (ConstProp.Val(111), ConstProp.Bot)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(Value.mkTag(Name.Resolved.mk("ConstProp"), "Val", Value.mkInt32(111)), Value.mkTag(Name.Resolved.mk("ConstProp"), "Bot", Value.Unit))))(result)
  }

  test("Expression.Tuple.06") {
    val input = """fn f: ((Int, Int), (Str, Str)) = ((123, 456), ("654", "321"))"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(Value.Tuple(Array(123, 456).map(Value.mkInt32)), Value.Tuple(Array("654", "321").map(Value.mkStr)))))(result)
  }

  test("Expression.Tuple.07") {
    val input = """fn f: (Int, Bool, Str) = (40 + 2, !(-12 < 22), if (true) "hi" else "hello")"""
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{CheckNil,CheckCons}                                         //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Set                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Set.01") {
    val input = "fn f: Set[Int] = #{1, 4, 2}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkSet(Set(1, 4, 2).map(Value.mkInt32)))(result)
  }

  ignore("Expression.Set.02") {
    val input = "fn f: Set[Int8] = #{1i8 + 2i8, 3i8 * 4i8, 5i8 - 6i8}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkSet(Set(3, 12, -1).map(Value.mkInt8)))(result)
  }

  ignore("Expression.Set.03") {
    val input = "fn f: Set[(Int16, Bool)] = #{(1i16 + 2i16, true), (2i16 + 1i16, !false), (4i16 * 7i16, true), (5i16, true && false)}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkSet(Set(
      Value.Tuple(Array(Value.mkInt16(3), Value.True)),
      Value.Tuple(Array(Value.mkInt16(28), Value.True)),
      Value.Tuple(Array(Value.mkInt16(5), Value.False))
    )))(result)
  }

  ignore("Expression.Set.04") {
    val input = "fn f: Set[Int64] = #{10000000000i64}"
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkSet(Set(Value.mkInt64(10000000000L))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Error                                                        //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Error.01") {
    val input = "fn f: Bool = ???: Bool"
    intercept[RuntimeException] { getModel(input) }
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
      """fn f(x: Bool): Int = switch {
        |  case x => 1
        |  case !x => 0
        |}
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(0))(result02)
  }

  test("Switch.02") {
    val input =
      """fn f(x: Bool): Int = switch {
        |  case x => 100
        |  case true => 20
        |}
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(100))(result01)
    assertResult(Value.mkInt32(20))(result02)
  }

  test("Switch.03") {
    val input =
      """fn f(x: Bool): Int = switch {
        |  case x => 1
        |  case !x => 0
        |  case true => ???: Int
        |}
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(0))(result02)
  }

  test("Switch.04") {
    val input =
      """fn f(x: Int): Str = switch {
        |  case x < 0 => "negative"
        |  case x == 0 => "zero"
        |  case x == 1 => "one"
        |  case x == 2 => "two"
        |  case x >= 3 => "many"
        |}
        |fn g01: Str = f(-2)
        |fn g02: Str = f(-1)
        |fn g03: Str = f(0)
        |fn g04: Str = f(1)
        |fn g05: Str = f(2)
        |fn g06: Str = f(3)
        |fn g07: Str = f(4)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    val result06 = model.constants(Name.Resolved.mk("g06"))
    val result07 = model.constants(Name.Resolved.mk("g07"))
    assertResult(Value.mkStr("negative"))(result01)
    assertResult(Value.mkStr("negative"))(result02)
    assertResult(Value.mkStr("zero"))(result03)
    assertResult(Value.mkStr("one"))(result04)
    assertResult(Value.mkStr("two"))(result05)
    assertResult(Value.mkStr("many"))(result06)
    assertResult(Value.mkStr("many"))(result07)
  }

  test("Switch.05") {
    val input =
      """fn f(x: Bool): Int = switch {
        |  case x => 1
        |}
        |fn g01: Int = f(true)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g01"))
    assertResult(Value.mkInt32(1))(result)
  }

  test("Switch.06") {
    val input =
      """fn f(x: Bool): Int = switch {
        |  case x => 1
        |}
        |fn g01: Int = f(false)
      """.stripMargin
    intercept[RuntimeException] { getModel(input) }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Match expressions (pattern matching)                                    //
  // These don't exist in the ExecutableAst because they're desugared into   //
  // primitives (e.g. CheckTag, GetTagValue, GetTupleIndex).                 //
  /////////////////////////////////////////////////////////////////////////////

  test("Match.Wildcard.01") {
    val input =
      """fn f: Int = match () with {
        |  case _ => 11
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(11))(result)
  }

  test("Match.Wildcard.02") {
    val input =
      """fn f: Int = match 42 with {
        |  case _ => 11
        |}
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(11))(result)
  }

  test("Match.Wildcard.03") {
    val input =
      """fn f(x: Int): Int = match x with {
        |  case _ => 11
        |}
        |fn g01: Int = f(-1)
        |fn g02: Int = f(0)
        |fn g03: Int = f(1)
        |fn g04: Int = f(99999)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(11))(result01)
    assertResult(Value.mkInt32(11))(result02)
    assertResult(Value.mkInt32(11))(result03)
    assertResult(Value.mkInt32(11))(result04)
  }

  test("Match.Var.01") {
    val input =
      """fn f(x: Int): Int = match x with {
        |  case a => 1
        |}
        |fn g: Int = f(3)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(1))(result)
  }

  test("Match.Var.02") {
    val input =
      """fn f(x: Int): Int = match x with {
        |  case a => a
        |}
        |fn g: Int = f(3)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(3))(result)
  }

  test("Match.Var.03") {
    val input =
      """fn f(x: Int): Int = match x with {
        |  case a => a + 11
        |}
        |fn g: Int = f(3)
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.mkInt32(14))(result)
  }

  test("Match.Literal.01") {
    val input =
      """fn f(x: ()): Bool = match x with {
        |  case () => true
        |}
        |fn g: Bool = f(())
      """.stripMargin
    val model = getModel(input)
    val result = model.constants(Name.Resolved.mk("g"))
    assertResult(Value.True)(result)
  }

  test("Match.Literal.02") {
    val input =
      """fn f(x: Bool): Int = match x with {
        |  case true => 30
        |  case false => 81
        |}
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(30))(result01)
    assertResult(Value.mkInt32(81))(result02)
  }

  test("Match.Literal.03") {
    val input =
      """fn f(x: Bool): Int = match x with {
        |  case true => 30
        |  case _ => 81
        |}
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(30))(result01)
    assertResult(Value.mkInt32(81))(result02)
  }

  test("Match.Literal.04") {
    val input =
      """fn f(x: Int): Str = match x with {
        |  case -1 => "minus one"
        |  case 0 => "zero"
        |  case 1 => "one"
        |  case _ => "unknown"
        |}
        |fn g01: Str = f(-1)
        |fn g02: Str = f(0)
        |fn g03: Str = f(1)
        |fn g04: Str = f(2)
        |fn g05: Str = f(3)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    assertResult(Value.mkStr("minus one"))(result01)
    assertResult(Value.mkStr("zero"))(result02)
    assertResult(Value.mkStr("one"))(result03)
    assertResult(Value.mkStr("unknown"))(result04)
    assertResult(Value.mkStr("unknown"))(result05)
  }

  ignore("Match.Literal.05") {
    val input =
      s"""fn f(x: Int8): Str = match x with {
         |  case ${Byte.MinValue}i8 => "min"
         |  case -2i8 => "a"
         |  case 6i8 => "b"
         |  case ${Byte.MaxValue}i8 => "max"
         |  case _ => "unknown"
         |}
         |fn g01: Str = f(${Byte.MinValue}i8)
         |fn g02: Str = f(-2i8)
         |fn g03: Str = f(6i8)
         |fn g04: Str = f(${Byte.MaxValue}i8)
         |fn g05: Str = f(0i8)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    assertResult(Value.mkStr("min"))(result01)
    assertResult(Value.mkStr("a"))(result02)
    assertResult(Value.mkStr("b"))(result03)
    assertResult(Value.mkStr("max"))(result04)
    assertResult(Value.mkStr("unknown"))(result05)
  }

  ignore("Match.Literal.06") {
    val input =
      s"""fn f(x: Int16): Str = match x with {
         |  case ${Short.MinValue}i16 => "min"
         |  case -211i16 => "a"
         |  case 623i16 => "b"
         |  case ${Short.MaxValue}i16 => "max"
         |  case _ => "unknown"
         |}
         |fn g01: Str = f(${Short.MinValue}i16)
         |fn g02: Str = f(-211i16)
         |fn g03: Str = f(623i16)
         |fn g04: Str = f(${Short.MaxValue}i16)
         |fn g05: Str = f(0i16)
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    assertResult(Value.mkStr("min"))(result01)
    assertResult(Value.mkStr("a"))(result02)
    assertResult(Value.mkStr("b"))(result03)
    assertResult(Value.mkStr("max"))(result04)
    assertResult(Value.mkStr("unknown"))(result05)
  }

  ignore("Match.Literal.07") {
    val input =
      s"""fn f(x: Int32): Str = match x with {
         |  case ${Int.MinValue}i32 => "min"
         |  case -2136541i32 => "a"
         |  case 6254523i32 => "b"
         |  case ${Int.MaxValue}i32 => "max"
         |  case _ => "unknown"
         |}
         |fn g01: Str = f(${Int.MinValue}i32)
         |fn g02: Str = f(-2136541i32)
         |fn g03: Str = f(6254523i32)
         |fn g04: Str = f(${Int.MaxValue}i32)
         |fn g05: Str = f(0i32)
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    assertResult(Value.mkStr("min"))(result01)
    assertResult(Value.mkStr("a"))(result02)
    assertResult(Value.mkStr("b"))(result03)
    assertResult(Value.mkStr("max"))(result04)
    assertResult(Value.mkStr("unknown"))(result05)
  }

  ignore("Match.Literal.08") {
    val input =
      s"""fn f(x: Int64): Str = match x with {
         |  case ${Long.MinValue}i64 => "min"
         |  case -213645454545541i64 => "a"
         |  case 6287816254523i64 => "b"
         |  case ${Long.MaxValue}i64 => "max"
         |  case _ => "unknown"
         |}
         |fn g01: Str = f(${Long.MinValue}i64)
         |fn g02: Str = f(-213645454545541i64)
         |fn g03: Str = f(6287816254523i64)
         |fn g04: Str = f(${Long.MaxValue}i64)
         |fn g05: Str = f(0i64)
       """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    assertResult(Value.mkStr("min"))(result01)
    assertResult(Value.mkStr("a"))(result02)
    assertResult(Value.mkStr("b"))(result03)
    assertResult(Value.mkStr("max"))(result04)
    assertResult(Value.mkStr("unknown"))(result05)
  }

  test("Match.Literal.09") {
    val input =
      """fn f(x: Str): Str = match x with {
        |  case "one" => "un"
        |  case "two" => "deux"
        |  case "three" => "trois"
        |  case _ => "???"
        |}
        |fn g01: Str = f("one")
        |fn g02: Str = f("two")
        |fn g03: Str = f("three")
        |fn g04: Str = f("four")
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkStr("un"))(result01)
    assertResult(Value.mkStr("deux"))(result02)
    assertResult(Value.mkStr("trois"))(result03)
    assertResult(Value.mkStr("???"))(result04)
  }

  test("Match.Literal.10") {
    val input =
      """enum Foo { case Bar, case Baz, case Abc(Int,Str), case Xyz }
        |fn f(x: Foo): Int = match x with {
        |  case Foo.Bar => 1
        |  case Foo.Baz => 2
        |  case Foo.Abc(42, "hi") => 3
        |  case _ => 0
        |}
        |fn g01: Int = f(Foo.Bar)
        |fn g02: Int = f(Foo.Baz)
        |fn g03: Int = f(Foo.Abc(42, "hi"))
        |fn g04: Int = f(Foo.Abc(42, "hi!"))
        |fn g05: Int = f(Foo.Abc(41, "hi"))
        |fn g06: Int = f(Foo.Abc(40, "a"))
        |fn g07: Int = f(Foo.Xyz)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    val result06 = model.constants(Name.Resolved.mk("g06"))
    val result07 = model.constants(Name.Resolved.mk("g07"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(2))(result02)
    assertResult(Value.mkInt32(3))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(0))(result05)
    assertResult(Value.mkInt32(0))(result06)
    assertResult(Value.mkInt32(0))(result07)
  }

  test("Match.Literal.11") {
    val input =
      """fn f(x: Str, y: Bool): Int = match (x, y) with {
        |  case ("hi", false) => 1
        |  case _ => 2
        |}
        |fn g01: Int = f("hi", true)
        |fn g02: Int = f("hi", false)
        |fn g03: Int = f("abc", true)
        |fn g04: Int = f("abc", false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(2))(result01)
    assertResult(Value.mkInt32(1))(result02)
    assertResult(Value.mkInt32(2))(result03)
    assertResult(Value.mkInt32(2))(result04)
  }

  test("Match.Literal.12") {
    val input =
      """fn f(x: (Int, (Int, Int))): Int = match x with {
        |  case (4, (12, 8)) => 1
        |  case (4, (12, 0)) => 2
        |  case (1, (12, 8)) => 3
        |  case _ => 4
        |}
        |fn g01: Int = f((4, (12, 8)))
        |fn g02: Int = f((4, (12, 0)))
        |fn g03: Int = f((1, (12, 8)))
        |fn g04: Int = f((1, (12, 0)))
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(2))(result02)
    assertResult(Value.mkInt32(3))(result03)
    assertResult(Value.mkInt32(4))(result04)
  }

  test("Match.Tag.01") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |fn f(x: NameAndAge): Int = match x with {
        |  case NameAndAge.T(_, age) => age
        |}
        |fn g01: Int = f(NameAndAge.T("James", 42))
        |fn g02: Int = f(NameAndAge.T("John", 21))
        |fn g03: Int = f(NameAndAge.T("James", 5))
        |fn g04: Int = f(NameAndAge.T("Mary", 33))
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(42))(result01)
    assertResult(Value.mkInt32(21))(result02)
    assertResult(Value.mkInt32(5))(result03)
    assertResult(Value.mkInt32(33))(result04)
  }

  test("Match.Tag.02") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |fn f(x: NameAndAge): Int = match x with {
        |  case NameAndAge.T("James", age) => age
        |  case _ => -1
        |}
        |fn g01: Int = f(NameAndAge.T("James", 42))
        |fn g02: Int = f(NameAndAge.T("John", 21))
        |fn g03: Int = f(NameAndAge.T("James", 5))
        |fn g04: Int = f(NameAndAge.T("Mary", 33))
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(42))(result01)
    assertResult(Value.mkInt32(-1))(result02)
    assertResult(Value.mkInt32(5))(result03)
    assertResult(Value.mkInt32(-1))(result04)
  }

  test("Match.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f(x: ConstProp): Int = match x with {
        |  case ConstProp.Top => -1
        |  case ConstProp.Val(v) => v
        |  case ConstProp.Bot => -2
        |}
        |fn g01: Int = f(ConstProp.Top)
        |fn g02: Int = f(ConstProp.Val(42))
        |fn g03: Int = f(ConstProp.Val(-24))
        |fn g04: Int = f(ConstProp.Bot)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(42))(result02)
    assertResult(Value.mkInt32(-24))(result03)
    assertResult(Value.mkInt32(-2))(result04)
  }

  test("Match.Tag.04") {
    val input =
      """enum BoolTag { case Top, case B(Bool), case Bot }
        |fn f(x: BoolTag): Int = match x with {
        |  case BoolTag.Top => 0
        |  case BoolTag.B(b) => if (b) 1 else -1
        |  case BoolTag.Bot => 0
        |}
        |fn g01: Int = f(BoolTag.Top)
        |fn g02: Int = f(BoolTag.B(true))
        |fn g03: Int = f(BoolTag.B(false))
        |fn g04: Int = f(BoolTag.Bot)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(1))(result02)
    assertResult(Value.mkInt32(-1))(result03)
    assertResult(Value.mkInt32(0))(result04)
  }

  test("Match.Tuple.01") {
    val input =
      """fn f(x: Int, y: Int): Int = match (x, y) with {
        |  case (a, b) => a + b
        |}
        |fn g01: Int = f(5, 6)
        |fn g02: Int = f(6, 5)
        |fn g03: Int = f(100, 23)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    assertResult(Value.mkInt32(11))(result01)
    assertResult(Value.mkInt32(11))(result02)
    assertResult(Value.mkInt32(123))(result03)
  }

  test("Match.Tuple.02") {
    val input =
      """fn f(x: Int, y: Bool): Str = match (x, y) with {
        |  case (5, true) => "abc"
        |  case (5, _) => "def"
        |  case (_, true) => "ghi"
        |  case (_, _) => "jkl"
        |}
        |fn g01: Str = f(5, true)
        |fn g02: Str = f(5, false)
        |fn g03: Str = f(6, true)
        |fn g04: Str = f(0, false)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkStr("abc"))(result01)
    assertResult(Value.mkStr("def"))(result02)
    assertResult(Value.mkStr("ghi"))(result03)
    assertResult(Value.mkStr("jkl"))(result04)
  }

  test("Match.Tuple.03") {
    val input =
      """fn f(x: Int, y: Int, z: Int): Int = match (x, (y, z)) with {
        |  case (1, (2, 3)) => -1
        |  case (1, (2, _)) => -2
        |  case (1, (_, 3)) => -3
        |  case (1, _) => -4
        |  case (_, (a, b)) => a + b
        |}
        |fn g01: Int = f(1, 2, 3)
        |fn g02: Int = f(1, 2, 4)
        |fn g03: Int = f(1, 3, 3)
        |fn g04: Int = f(1, 5, 5)
        |fn g05: Int = f(2, 2, 3)
        |fn g06: Int = f(2, 10, 20)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    val result06 = model.constants(Name.Resolved.mk("g06"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(-2))(result02)
    assertResult(Value.mkInt32(-3))(result03)
    assertResult(Value.mkInt32(-4))(result04)
    assertResult(Value.mkInt32(5))(result05)
    assertResult(Value.mkInt32(30))(result06)
  }

  test("Match.Tuple.04") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f(x: ConstProp, y: ConstProp): Int = match (x, y) with {
        |  case (ConstProp.Top, ConstProp.Top) => 1
        |  case (ConstProp.Bot, ConstProp.Bot) => 2
        |  case (ConstProp.Val(v1), ConstProp.Val(v2)) => if (v1 == v2) 3 else 4
        |  case _ => 5
        |}
        |fn g01: Int = f(ConstProp.Top, ConstProp.Top)
        |fn g02: Int = f(ConstProp.Bot, ConstProp.Bot)
        |fn g03: Int = f(ConstProp.Val(42), ConstProp.Val(42))
        |fn g04: Int = f(ConstProp.Val(42), ConstProp.Val(0))
        |fn g05: Int = f(ConstProp.Val(0), ConstProp.Val(42))
        |fn g06: Int = f(ConstProp.Top, ConstProp.Bot)
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    val result06 = model.constants(Name.Resolved.mk("g06"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(2))(result02)
    assertResult(Value.mkInt32(3))(result03)
    assertResult(Value.mkInt32(4))(result04)
    assertResult(Value.mkInt32(4))(result05)
    assertResult(Value.mkInt32(5))(result06)
  }

  test("Match.Tuple.05") {
    val input =
      """enum NameAndAge { case T(Str,Int) }
        |fn f(x: Int, y: NameAndAge): Int = match (x, y) with {
        |  case (1, NameAndAge.T("James", _)) => 1
        |  case (a, NameAndAge.T("James", b)) => a + b
        |  case (_, NameAndAge.T(_, 24)) => 2
        |  case _ => -1
        |}
        |fn g01: Int = f(1, NameAndAge.T("James", 20))
        |fn g02: Int = f(1, NameAndAge.T("John", 53))
        |fn g03: Int = f(2, NameAndAge.T("James", 20))
        |fn g04: Int = f(2, NameAndAge.T("John", 53))
        |fn g05: Int = f(3, NameAndAge.T("Mary", 24))
        |fn g06: Int = f(3, NameAndAge.T("Anne", 18))
        |fn g07: Int = f(4, NameAndAge.T("Charles", 64))
      """.stripMargin
    val model = getModel(input)
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    val result05 = model.constants(Name.Resolved.mk("g05"))
    val result06 = model.constants(Name.Resolved.mk("g06"))
    val result07 = model.constants(Name.Resolved.mk("g07"))
    assertResult(Value.mkInt32(1))(result01)
    assertResult(Value.mkInt32(-1))(result02)
    assertResult(Value.mkInt32(22))(result03)
    assertResult(Value.mkInt32(-1))(result04)
    assertResult(Value.mkInt32(2))(result05)
    assertResult(Value.mkInt32(-1))(result06)
    assertResult(Value.mkInt32(-1))(result07)
  }

  test("Match.Tuple.06") {
    import HookUnsafeHelpers._
    val input =
      """fn fst(t: (Native, Native)): Native = match t with {
        |  case (x, _) => x
        |}
        |fn g: (Native, Native) = f(12)
        |fn h: Native = fst(g())
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkNativeType, flix.mkNativeType)))
    def nativeF(x: Int): (MyObject, MyObject) = { executed = true; (MyObject(x), MyObject(x * 2)) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(MyObject(12))(result)
    assert(executed)
  }

  test("Match.Tuple.07") {
    import HookUnsafeHelpers._
    val input =
      """fn fst(t: (Native, Native)): Native = match t with {
        |  case (x, _) => x
        |}
        |fn g: (Native, Native) = f(12)
        |fn h: Native = fst(g())
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkNativeType, flix.mkNativeType)))
    def nativeF(x: Int): (Int, String) = { executed = true; (x, x.toString) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(12))(result)
    assert(executed)
  }

  test("Match.Tuple.08") {
    import HookUnsafeHelpers._
    val input =
      """fn fst(t: (Int, Str)): Int = match t with {
        |  case (x, _) => x
        |}
        |fn g: (Int, Str) = f(12)
        |fn h: Int = fst(g())
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: Int): (Int, String) = { executed = true; (x, x.toString) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val result = model.constants(Name.Resolved.mk("h"))
    assertResult(Value.mkInt32(12))(result)
    assert(executed)
  }

  // TODO: Bug in the simplifier causes a NoSuchElementException.
  // However, the test catches this exception and so the test passes.
  // See https://github.com/magnus-madsen/flix/issues/118
  test("Match.Error.01") {
    val input =
      """fn f(x: Int): Bool = match x with {
        |  case 321 => true
        |}
        |fn g: Bool = f(123)
      """.stripMargin
    intercept[RuntimeException] { getModel(input) }
  }

  // TODO: Tests for future expressions, e.g. opt, list, map

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
    val model = getModel(input)
    val B = model.relations(Name.Resolved.mk("B")).toSet
    assertResult(B)(Set(List(Value.True)))
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
    val model = getModel(input)
    val B = model.relations(Name.Resolved.mk("B")).toSet
    assertResult(B)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
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
    val model = getModel(input)
    val B = model.relations(Name.Resolved.mk("B")).toSet
    assertResult(B)(Set("one", "two", "three").map(x => List(Value.mkStr(x))))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Exp                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Exp.01") {
    val input =
      """rel A(x: ());
        |
        |A(()).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Unit)))
  }

  test("Term.Head.Exp.02") {
    val input =
      """rel A(x: Bool);
        |
        |A(true).
        |A(false).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(true, false).map(x => List(Value.mkBool(x))))
  }

  ignore("Term.Head.Exp.03") {
    val input =
      """rel A(x: Int8);
        |
        |A(1i8).
        |A(2i8).
        |A(3i8).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt8(x))))
  }

  ignore("Term.Head.Exp.04") {
    val input =
      """rel A(x: Int16);
        |
        |A(1i16).
        |A(2i16).
        |A(3i16).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt16(x))))
  }

  ignore("Term.Head.Exp.05") {
    val input =
      """rel A(x: Int32);
        |
        |A(1i32).
        |A(2i32).
        |A(3i32).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Head.Exp.06") {
    val input =
      """rel A(x: Int64);
        |
        |A(1i64).
        |A(2i64).
        |A(3i64).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt64(x))))
  }

  test("Term.Head.Exp.07") {
    val input =
      """rel A(x: Str);
        |
        |A("one").
        |A("two").
        |A("three").
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set("one", "two", "three").map(x => List(Value.mkStr(x))))
  }

  test("Term.Head.Exp.08") {
    val input =
      """rel A(x: (Int, Str));
        |
        |A((1, "one")).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))))
  }

  test("Term.Head.Exp.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(Foo.Foo(1, "one")).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.mkTag(Name.Resolved.mk("Foo"), "Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))))
  }

  test("Term.Head.Exp.10") {
    val input =
      """rel A(x: (Int, Int));
        |
        |A((1, 2)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.Apply                                                         //
  // These tests simply re-implement the Term.Head.Exp tests using Apply.    //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.Apply.01") {
    val input =
      """rel A(x: ());
        |fn f(x: Int): () = ()
        |
        |A(f(0)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Unit)))
  }

  test("Term.Head.Apply.02") {
    val input =
      """rel A(x: Bool);
        |fn f(x: Int): Bool = x == 0
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(true, false).map(x => List(Value.mkBool(x))))
  }

  ignore("Term.Head.Apply.03") {
    val input =
      """rel A(x: Int8);
        |fn f(x: Int8): Int8 = x + 1i8
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt8(x))))
  }

  ignore("Term.Head.Apply.04") {
    val input =
      """rel A(x: Int16);
        |fn f(x: Int16): Int16 = x + 1i16
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt16(x))))
  }

  ignore("Term.Head.Apply.05") {
    val input =
      """rel A(x: Int32);
        |fn f(x: Int32): Int32 = x + 1i32
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Head.Apply.06") {
    val input =
      """rel A(x: Int64);
        |fn f(x: Int64): Int64 = x + 1i64
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt64(x))))
  }

  test("Term.Head.Apply.07") {
    val input =
      """rel A(x: Str);
        |fn f(x: Str): Str = x
        |
        |A(f("one")).
        |A(f("two")).
        |A(f("three")).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set("one", "two", "three").map(x => List(Value.mkStr(x))))
  }

  test("Term.Head.Apply.08") {
    val input =
      """rel A(x: (Int, Str));
        |fn f(x: Int): (Int, Str) = (x, "one")
        |
        |A(f(1)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))))
  }

  test("Term.Head.Apply.09") {
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |fn f(x: Str): Foo = Foo.Foo(1, x)
        |
        |A(f("one")).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.mkTag(Name.Resolved.mk("Foo"), "Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))))
  }

  test("Term.Head.Apply.10") {
    val input =
      """rel A(x: (Int, Int));
        |fn f(x: Int, y: Int): (Int, Int) = (x, y)
        |
        |A(f(1, 2)).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.ApplyHook - Hook.Safe                                         //
  // These tests simply re-implement the Term.Head.Exp tests using ApplyHook.//
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Head.ApplyHook - Hook.Safe.01") {
    import HookSafeHelpers._
    val input =
      """rel A(x: ());
        |
        |A(f(0)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkUnitType)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkUnit }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Unit)))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Safe.02") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Bool);
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkBool(x.getInt32 == 0) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(true, false).map(x => List(Value.mkBool(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Safe.03") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int8);
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkInt8(x.getInt8 + 1) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt8(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Safe.04") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int16);
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkInt16(x.getInt16 + 1) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt16(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Safe.05") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int32);
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkInt32(x.getInt32 + 1) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Safe.06") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Int64);
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkInt64(x.getInt64 + 1) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt64(x))))
    assert(executed)
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
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkStrType)
    def nativeF(x: IValue): IValue = { executed = true; x }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set("one", "two", "three").map(x => List(Value.mkStr(x))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Safe.08") {
    import HookSafeHelpers._
    val input =
      """rel A(x: (Int, Str));
        |
        |A(f(1)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: IValue): IValue = { executed = true; flix.mkTuple(Array(x, flix.mkStr("one"))) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))))
    assert(executed)
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  // TODO: mkTagType should be taking an IType instead of a Type?
  ignore("Term.Head.ApplyHook - Hook.Safe.09") {
    import HookSafeHelpers._
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(f("one")).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tagTpe = flix.mkTagType("Foo", "Foo", Type.Tuple(List(Type.Int32, Type.Str)))
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkEnumType("Foo", Array(tagTpe)))
    def nativeF(x: IValue): IValue = {
      executed = true
      flix.mkTag("Foo", "Foo", flix.mkTuple(Array(flix.mkInt32(1), x)))
    }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.mkTag(Name.Resolved.mk("Foo"), "Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Safe.10") {
    import HookSafeHelpers._
    val input =
      """rel A(x: (Int, Int));
        |
        |A(f(1, 2)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkInt32Type)))
    def nativeF(x: IValue, y: IValue): IValue = { executed = true; flix.mkTuple(Array(x, y)) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Safe.11") {
    import HookSafeHelpers._
    val input =
      """rel A(x: Native);
        |
        |A(f(1)).
        |A(f(2)).
        |A(f(3)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    def nativeF(x: IValue): IValue = { executed = true; flix.mkNative(MyObject(x.getInt32)) }
    val model = flix
      .addStr(input)
      .addHook("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(MyObject(x))))
    assert(executed)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Head.ApplyHook - Hook.Unsafe                                       //
  // These tests simply re-implement the Term.Head.Exp tests using ApplyHook.//
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
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkUnitType)
    def nativeF(x: JInt): Value.Unit.type = { executed = true; Value.Unit }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Unit)))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.02") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Bool);
        |
        |A(f(0)).
        |A(f(1)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    def nativeF(x: JInt): JBool = { executed = true; x == 0 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(true, false).map(x => List(Value.mkBool(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Unsafe.03") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int8);
        |
        |A(f(0i8)).
        |A(f(1i8)).
        |A(f(2i8)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt8Type), flix.mkInt8Type)
    def nativeF(x: JByte): JByte = { executed = true; (x + 1).toByte }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt8(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Unsafe.04") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int16);
        |
        |A(f(0i16)).
        |A(f(1i16)).
        |A(f(2i16)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt16Type), flix.mkInt16Type)
    def nativeF(x: JShort): JShort = { executed = true; (x + 1).toShort }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt16(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Unsafe.05") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int32);
        |
        |A(f(0i32)).
        |A(f(1i32)).
        |A(f(2i32)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkInt32Type)
    def nativeF(x: JInt): JInt = { executed = true; x + 1 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
    assert(executed)
  }

  ignore("Term.Head.ApplyHook - Hook.Unsafe.06") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Int64);
        |
        |A(f(0i64)).
        |A(f(1i64)).
        |A(f(2i64)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt64Type), flix.mkInt64Type)
    def nativeF(x: JLong): JLong = { executed = true; x + 1 }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt64(x))))
    assert(executed)
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
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkStrType)
    def nativeF(x: String): String = { executed = true; x }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set("one", "two", "three").map(x => List(Value.mkStr(x))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.08") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: (Int, Str));
        |
        |A(f(1)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkStrType)))
    def nativeF(x: JInt): Value.Tuple = { executed = true; Value.Tuple(Array(x, "one")) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))))
    assert(executed)
  }

  // TODO: This test fails because Tag.tag (a Name.Ident) compares the source location.
  // See https://github.com/magnus-madsen/flix/issues/119
  // TODO: mkTagType should be taking an IType instead of a Type?
  ignore("Term.Head.ApplyHook - Hook.Unsafe.09") {
    import HookUnsafeHelpers._
    val input =
      """enum Foo { case Foo(Int,Str) }
        |rel A(x: Foo);
        |
        |A(f("one")).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tagTpe = flix.mkTagType("Foo", "Foo", Type.Tuple(List(Type.Int32, Type.Str)))
    val tpe = flix.mkFunctionType(Array(flix.mkStrType), flix.mkEnumType("Foo", Array(tagTpe)))
    def nativeF(x: String): Value.Tag = {
      executed = true
      Value.mkTag(Name.Resolved.mk("Foo"), "Foo", Value.Tuple(Array(Value.mkInt32(1), x)))
    }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.mkTag(Name.Resolved.mk("Foo"), "Foo", Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.10") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: (Int, Int));
        |
        |A(f(1, 2)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkTupleType(Array(flix.mkInt32Type, flix.mkInt32Type)))
    def nativeF(x: JInt, y: JInt): Value.Tuple = { executed = true; Value.Tuple(Array(x, y)) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(List(Value.Tuple(Array(1, 2).map(Value.mkInt32)))))
    assert(executed)
  }

  test("Term.Head.ApplyHook - Hook.Unsafe.11") {
    import HookUnsafeHelpers._
    val input =
      """rel A(x: Native);
        |
        |A(f(1)).
        |A(f(2)).
        |A(f(3)).
      """.stripMargin
    var executed = false
    val flix = createFlix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkNativeType)
    def nativeF(x: JInt): MyObject = { executed = true; MyObject(x) }
    val model = flix
      .addStr(input)
      .addHookUnsafe("f", tpe, nativeF _)
      .solve().get
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(MyObject(x))))
    assert(executed)
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
        |fn f(x: Bool): Bool = x
        |
        |A(true, true).
        |A(false, true).
        |
        |B(y) :- f(x), A(x, y).
      """.stripMargin
    val model = getModel(input)
    val B = model.relations(Name.Resolved.mk("B")).toSet
    assertResult(B)(Set(List(Value.True)))
  }

  test("Term.Body.Var.02") {
    val input =
      """rel A(x: Int);
        |rel B(x: Int);
        |fn f(x: Int): Bool = x % 2 == 0
        |
        |A(0).
        |A(1).
        |A(2).
        |A(3).
        |
        |B(x) :- f(x), A(x).
      """.stripMargin
    val model = getModel(input)
    val B = model.relations(Name.Resolved.mk("B")).toSet
    assertResult(B)(Set(0, 2).map(x => List(Value.mkInt32(x))))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Term.Body.Exp                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Term.Body.Exp.01") {
    val input =
      """rel A(x: Int);
        |fn f(x: Bool): Bool = x
        |
        |A(1) :- f(true).
        |A(2) :- f(true).
        |A(3) :- f(true).
        |A(4) :- f(false).
        |A(5) :- f(false).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Body.Exp.02") {
    val input =
      """rel A(x: Int);
        |fn f(x: Int8): Bool = x >= 0i8
        |
        |A(1) :- f(0i8).
        |A(2) :- f(0i8).
        |A(3) :- f(0i8).
        |A(4) :- f(-1i8).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Body.Exp.03") {
    val input =
      """rel A(x: Int);
        |fn f(x: Int16): Bool = x >= 0i16
        |
        |A(1) :- f(0i16).
        |A(2) :- f(0i16).
        |A(3) :- f(0i16).
        |A(4) :- f(-200i16).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Body.Exp.04") {
    val input =
      """rel A(x: Int);
        |fn f(x: Int32): Bool = x >= 0i32
        |
        |A(1) :- f(0i32).
        |A(2) :- f(0i32).
        |A(3) :- f(0i32).
        |A(4) :- f(-200000i32).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  ignore("Term.Body.Exp.05") {
    val input =
      """rel A(x: Int);
        |fn f(x: Int64): Bool = x >= 0i64
        |
        |A(1) :- f(0i64).
        |A(2) :- f(0i64).
        |A(3) :- f(0i64).
        |A(4) :- f(-20000000000i64).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  test("Term.Body.Exp.06") {
    val input =
      """rel A(x: Int);
        |fn f(x: Str): Bool = true
        |
        |A(1) :- f("foo").
        |A(2) :- f("bar").
        |A(3) :- f("baz").
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  test("Term.Body.Exp.07") {
    val input =
      """rel A(x: Int);
        |fn f(x: (Int, Str)): Bool = match x with {
        |  case (a, "abc") => a >= 0
        |  case _ => false
        |}
        |
        |A(1) :- f((0, "abc")).
        |A(2) :- f((0, "abc")).
        |A(3) :- f((0, "abc")).
        |A(4) :- f((-1, "abc")).
        |A(5) :- f((0, "xyz")).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

  test("Term.Body.Exp.08") {
    val input =
      """enum Val { case Top, case Val(Int), case Bot }
        |rel A(x: Int);
        |fn f(x: Val): Bool = match x with {
        |  case Val.Val(v) => v >= 0
        |  case _ => false
        |}
        |
        |A(1) :- f(Val.Val(0)).
        |A(2) :- f(Val.Val(0)).
        |A(3) :- f(Val.Val(0)).
        |A(4) :- f(Val.Val(-1)).
        |A(5) :- f(Val.Top).
      """.stripMargin
    val model = getModel(input)
    val A = model.relations(Name.Resolved.mk("A")).toSet
    assertResult(A)(Set(1, 2, 3).map(x => List(Value.mkInt32(x))))
  }

}
