package ca.uwaterloo.flix.language.backend.phase

import java.nio.file.{Paths, Files}

import ca.uwaterloo.flix.language.ast.{BinaryOperator, UnaryOperator, Name, SourceLocation}
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR._
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.Expression._
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.Definition.Function

import org.scalatest.FunSuite

class TestCodegen extends FunSuite {

  val loc = SourceLocation.Unknown
  val compiledClassName = "ca.uwaterloo.flix.runtime.compiled.FlixDefinitions"

  class CompiledCode(definitions: List[Definition]) {
    object Loader extends ClassLoader {
      def apply(name: String, b: Array[Byte]): Class[_] = {
        defineClass(name, b, 0, b.length)
      }
    }

    val code = Codegen.compile(definitions)
    val clazz = Loader(compiledClassName, code)

    // Write to a class file, for debugging.
    def dumpBytecode(path: String = "FlixBytecode.class"): Unit = {
      Files.write(Paths.get(path), code)
    }

    def call(name: String, tpes: List[Class[_]] = List(), args: List[Object] = List()): Any = {
      val method = clazz.getMethod(name, tpes: _*)
      method.invoke(null, args: _*)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int constants                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Int01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Const(42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate, List())

    assertResult(42)(result)
  }

  test("Codegen - Int02") {
    // Constants -1 to 5 (inclusive) each have their own instruction

    val name_m1 = Name.Resolved.mk(List("foo", "bar", "f_m1"))
    val name_0 = Name.Resolved.mk(List("foo", "bar", "f_0"))
    val name_1 = Name.Resolved.mk(List("foo", "bar", "f_1"))
    val name_2 = Name.Resolved.mk(List("foo", "bar", "f_2"))
    val name_3 = Name.Resolved.mk(List("foo", "bar", "f_3"))
    val name_4 = Name.Resolved.mk(List("foo", "bar", "f_4"))
    val name_5 = Name.Resolved.mk(List("foo", "bar", "f_5"))

    val def_m1 = Function(name_m1, args = List(),
      body = Const(-1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_0 = Function(name_0, args = List(),
      body = Const(0, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_1 = Function(name_1, args = List(),
      body = Const(1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_2 = Function(name_2, args = List(),
      body = Const(2, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_3 = Function(name_3, args = List(),
      body = Const(3, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_4 = Function(name_4, args = List(),
      body = Const(4, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def_5 = Function(name_5, args = List(),
      body = Const(5, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def_m1, def_0, def_1, def_2, def_3, def_4, def_5))

    val result_m1 = code.call(name_m1.decorate)
    val result_0 = code.call(name_0.decorate)
    val result_1 = code.call(name_1.decorate)
    val result_2 = code.call(name_2.decorate)
    val result_3 = code.call(name_3.decorate)
    val result_4 = code.call(name_4.decorate)
    val result_5 = code.call(name_5.decorate)

    assertResult(-1)(result_m1)
    assertResult(0)(result_0)
    assertResult(1)(result_1)
    assertResult(2)(result_2)
    assertResult(3)(result_3)
    assertResult(4)(result_4)
    assertResult(5)(result_5)
  }

  test("Codegen - Int03") {
    // Test some constants that are loaded with a BIPUSH, i.e. i <- [Byte.MinValue, -1) UNION (5, Byte,MaxValue]

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Const(Byte.MinValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Const(Byte.MinValue + 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Const(-2, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Const(6, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Const(Byte.MaxValue - 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Const(Byte.MaxValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01.decorate)
    val result02 = code.call(name02.decorate)
    val result03 = code.call(name03.decorate)
    val result04 = code.call(name04.decorate)
    val result05 = code.call(name05.decorate)
    val result06 = code.call(name06.decorate)

    assertResult(Byte.MinValue)(result01)
    assertResult(Byte.MinValue + 42)(result02)
    assertResult(-2)(result03)
    assertResult(6)(result04)
    assertResult(Byte.MaxValue - 42)(result05)
    assertResult(Byte.MaxValue)(result06)
  }

  test("Codegen - Int04") {
    // Test some constants that are loaded with an SIPUSH, i.e. i <- [Short.MinValue, Byte.MinValue) UNION (Byte.MaxValue, Short,MaxValue]

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Const(Short.MinValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Const(Short.MinValue + 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Const(Byte.MinValue - 1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Const(Byte.MaxValue + 1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Const(Short.MaxValue - 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Const(Short.MaxValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01.decorate)
    val result02 = code.call(name02.decorate)
    val result03 = code.call(name03.decorate)
    val result04 = code.call(name04.decorate)
    val result05 = code.call(name05.decorate)
    val result06 = code.call(name06.decorate)

    assertResult(Short.MinValue)(result01)
    assertResult(Short.MinValue + 42)(result02)
    assertResult(Byte.MinValue - 1)(result03)
    assertResult(Byte.MaxValue + 1)(result04)
    assertResult(Short.MaxValue - 42)(result05)
    assertResult(Short.MaxValue)(result06)
  }

  test("Codegen - Int05") {
    // Test some constants that are loaded with an LDC, i.e. i <- [Int.MinValue, Short.MinValue) UNION (Short.MaxValue, Int,MaxValue]

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Const(Int.MinValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Const(Int.MinValue + 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Const(Short.MinValue - 1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Const(Short.MaxValue + 1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Const(Int.MaxValue - 42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Const(Int.MaxValue, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01.decorate)
    val result02 = code.call(name02.decorate)
    val result03 = code.call(name03.decorate)
    val result04 = code.call(name04.decorate)
    val result05 = code.call(name05.decorate)
    val result06 = code.call(name06.decorate)

    assertResult(Int.MinValue)(result01)
    assertResult(Int.MinValue + 42)(result02)
    assertResult(Short.MinValue - 1)(result03)
    assertResult(Short.MaxValue + 1)(result04)
    assertResult(Int.MaxValue - 42)(result05)
    assertResult(Int.MaxValue)(result06)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Variables                                                               //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Var01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List("x"),
      body = Const(-1, Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val method = code.clazz.getMethod(name.decorate, Integer.TYPE)
    val result = method.invoke(null, 42.asInstanceOf[Object])

    assertResult(-1)(result)
  }

  test("Codegen - Var02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List("x"),
      body = Var(LocalVar(0, "x"), Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate, List(Integer.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(42)(result)
  }

  test("Codegen - Var03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(LocalVar(1, "y"), Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate,
      List(Integer.TYPE, Integer.TYPE, Integer.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unary operators                                                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Unary.Plus01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Const(42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42)(result)
  }

  test("Codegen - Unary.Plus02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Const(-42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-42)(result)
  }

  test("Codegen - Unary.Minus01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Minus, Const(42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-42)(result)
  }

  test("Codegen - Unary.Minus02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Minus, Const(-42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42)(result)
  }

  test("Codegen - Unary.Negate01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Negate, Const(42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(~42)(result)
  }

  test("Codegen - Unary.Negate02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(~(-42))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators                                                        //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Binary.Plus01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(400, Type.Int32, loc),
        Const(100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(500)(result)
  }

  test("Codegen - Binary.Plus02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(100, Type.Int32, loc),
        Const(400, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(500)(result)
  }

  test("Codegen - Binary.Plus03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-400, Type.Int32, loc),
        Const(100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-300)(result)
  }

  test("Codegen - Binary.Plus04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-100, Type.Int32, loc),
        Const(400, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(300)(result)
  }

  test("Codegen - Binary.Plus05") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-400, Type.Int32, loc),
        Const(-100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-500)(result)
  }

  test("Codegen - Binary.Minus01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(400, Type.Int32, loc),
        Const(100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(300)(result)
  }

  test("Codegen - Binary.Minus02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(100, Type.Int32, loc),
        Const(400, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-300)(result)
  }

  test("Codegen - Binary.Minus03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-400, Type.Int32, loc),
        Const(100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-500)(result)
  }

  test("Codegen - Binary.Minus04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-100, Type.Int32, loc),
        Const(400, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-500)(result)
  }

  test("Codegen - Binary.Minus05") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-400, Type.Int32, loc),
        Const(-100, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-300)(result)
  }

  test("Codegen - Binary.Times01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(2, Type.Int32, loc),
        Const(3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(6)(result)
  }

  test("Codegen - Binary.Times02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(3, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(6)(result)
  }

  test("Codegen - Binary.Times03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-2, Type.Int32, loc),
        Const(3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-6)(result)
  }

  test("Codegen - Binary.Times04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-3, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-6)(result)
  }

  test("Codegen - Binary.Times05") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-2, Type.Int32, loc),
        Const(-3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(6)(result)
  }

  test("Codegen - Binary.Divide01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(12, Type.Int32, loc),
        Const(3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4)(result)
  }

  test("Codegen - Binary.Divide02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(3, Type.Int32, loc),
        Const(12, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(0)(result)
  }

  test("Codegen - Binary.Divide03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12, Type.Int32, loc),
        Const(3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-4)(result)
  }

  test("Codegen - Binary.Divide04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-3, Type.Int32, loc),
        Const(12, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(0)(result)
  }

  test("Codegen - Binary.Divide05") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12, Type.Int32, loc),
        Const(-3, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4)(result)
  }

  test("Codegen - Binary.Modulo01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(0)(result)
  }

  test("Codegen - Binary.Modulo02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int32, loc),
        Const(5, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(2)(result)
  }

  test("Codegen - Binary.Modulo03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12, Type.Int32, loc),
        Const(5, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-2)(result)
  }

  test("Codegen - Binary.Modulo04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int32, loc),
        Const(-5, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(2)(result)
  }

  test("Codegen - Binary.Modulo05") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12, Type.Int32, loc),
        Const(-5, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(-2)(result)
  }

  test("Codegen - Binary.BitwiseAnd01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int32, loc),
        Const(0xFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 & 0xFFFF)(result)
  }

  test("Codegen - Binary.BitwiseAnd02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int32, loc),
        Const(42, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 & 42)(result)
  }

  test("Codegen - Binary.BitwiseAnd03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 & 0)(result)
  }

  test("Codegen - Binary.BitwiseOr01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int32, loc),
        Const(0xFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 | 0xFFFF)(result)
  }

  test("Codegen - Binary.BitwiseOr02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int32, loc),
        Const(42, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 | 42)(result)
  }

  test("Codegen - Binary.BitwiseOr03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 | 0)(result)
  }

  test("Codegen - Binary.BitwiseXor01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int32, loc),
        Const(0xFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 ^ 0xFFFF)(result)
  }

  test("Codegen - Binary.BitwiseXor02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int32, loc),
        Const(42, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 ^ 42)(result)
  }

  test("Codegen - Binary.BitwiseXor03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(42 ^ 0)(result)
  }

  test("Codegen - Binary.BitwiseLeftShift01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(4, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4 << 0)(result)
  }

  test("Codegen - Binary.BitwiseLeftShift02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(4, Type.Int32, loc),
        Const(14, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4 << 14)(result)
  }

  test("Codegen - Binary.BitwiseLeftShift03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(4, Type.Int32, loc),
        Const(29, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4 << 29)(result)
  }

  test("Codegen - Binary.BitwiseLeftShift04") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(4, Type.Int32, loc),
        Const(30, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(4 << 30)(result)
  }

  test("Codegen - Binary.BitwiseRightShift01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12345, Type.Int32, loc),
        Const(20, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(12345 >> 20)(result)
  }

  test("Codegen - Binary.BitwiseRightShift02") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12345, Type.Int32, loc),
        Const(10, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(12345 >> 10)(result)
  }

  test("Codegen - Binary.BitwiseRightShift03") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12345, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name.decorate)

    assertResult(12345 >> 0)(result)
  }

}
