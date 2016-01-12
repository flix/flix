package ca.uwaterloo.flix.language.backend.phase

import java.nio.file.{Paths, Files}

import ca.uwaterloo.flix.language.ast.{BinaryOperator, UnaryOperator, Name, SourceLocation}
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.{Definition, LocalVar, Type}
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.Expression._
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.Definition.Function

import org.scalatest.FunSuite

class TestCodegen extends FunSuite {

  val name = Name.Resolved.mk(List("foo", "bar", "main"))
  val name01 = Name.Resolved.mk(List("foo", "bar", "f"))
  val name02 = Name.Resolved.mk(List("foo", "bar", "g"))
  val name03 = Name.Resolved.mk(List("foo", "bar", "h"))

  val loc = SourceLocation.Unknown
  val compiledClassName = "ca.uwaterloo.flix.compiled.FlixDefinitions"

  class CompiledCode(definitions: List[Definition], debug: Boolean = false) {
    object Loader extends ClassLoader {
      def apply(name: String, b: Array[Byte]): Class[_] = {
        defineClass(name, b, 0, b.length)
      }
    }

    val code = Codegen.compile(new Codegen.Context(definitions, compiledClassName.replace(".", "/")))
    if (debug) dumpBytecode()
    val clazz = Loader(compiledClassName, code)

    // Write to a class file, for debugging.
    def dumpBytecode(path: String = "FlixBytecode.class"): Unit = {
      Files.write(Paths.get(path), code)
    }

    def call(name: Name.Resolved, tpes: List[Class[_]] = List(), args: List[Object] = List()): Any = {
      val decorated = Codegen.decorate(name)
      val method = clazz.getMethod(decorated, tpes: _*)
      method.invoke(null, args: _*)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Loading (unpacking bits)                                                //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - LoadBool - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadBool(Const(0, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadBool - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadBool(Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(1)(result)
    }
  }

  test("Codegen - LoadBool - 03") {
    // Binary number is: 1001100110011001100110011001100110011001100110011001100110011001
    // So the bits that are set are i = 0, 3, 4, 7, ..., i.e. i % 4 == 0 or i % 4 == 3
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadBool(Const(0x9999999999999999L, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(if (i % 4 == 0 || i % 4 == 3) 1 else 0)(result)
    }
  }

  test("Codegen - LoadInt8 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt8(Const(0, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt8 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt8(Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0xFF)(result)
    }
  }

  test("Codegen - LoadInt8 - 03") {
    // Binary number is: 1010101110101011101010111010101110101011101010111010101110101011
    // So every 8 bits looks like 10101011 = 0xAB
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt8(Const(0xABABABABABABABABL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0xAB)(result)
    }
  }

  test("Codegen - LoadInt16 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt16(Const(0, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt16 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt16(Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0xFFFF)(result)
    }
  }

  test("Codegen - LoadInt16 - 03") {
    // Binary number is: 1100101011111110110010101111111011001010111111101100101011111110
    // So every 16 bits looks like 1100101011111110 = 0xCAFE
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt16(Const(0xCAFECAFECAFECAFEL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0xCAFE)(result)
    }
  }

  test("Codegen - LoadInt32 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt32(Const(0, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt32 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt32(Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0xFFFFFFFF)(result)
    }
  }

  test("Codegen - LoadInt32 - 03") {
    // Binary number is: 1101111010101101101111101110111111011110101011011011111011101111
    // So every 32 bits looks like 11011110101011011011111011101111 = 0xDEADBEEF
    def definition(offset: Int) = Function(name, args = List(),
      body = LoadInt32(Const(0xDEADBEEFDEADBEEFL, Type.Int64, loc), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0xDEADBEEF)(result)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Storing (packing bits)                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - StoreBool - 01") {
    val tru = Const(1, Type.Bool, loc)
    val definition = Function(name, args = List(),
      StoreBool(Const(0, Type.Int64, loc), 42, tru),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000040000000000L)(result)
  }

  test("Codegen - StoreBool - 02") {
    val tru = Const(1, Type.Bool, loc)
    val definition = Function(name, args = List(),
      body = StoreBool(StoreBool(StoreBool(StoreBool(StoreBool(StoreBool(
        Const(1230000, Type.Int64, loc), 2, tru), 5, tru), 10, tru), 12, tru), 49, tru), 61, tru),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x200200000012D4B4L)(result)
  }

  test("Codegen - StoreBool - 03") {
    val tru = Const(1, Type.Bool, loc)
    def definition(offset: Int) = Function(name, args = List(),
      body = StoreBool(Const(0, Type.Int64, loc), offset, tru),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(1L << i)(result)
    }
  }

  test("Codegen - StoreInt8 - 01") {
    val v = Const(0xAB, Type.Int8, loc)
    val definition = Function(name, args = List(),
      StoreInt8(Const(0, Type.Int64, loc), 40, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000AB0000000000L)(result)
  }

  test("Codegen - StoreInt8 - 02") {
    val v = Const(0xAB, Type.Int8, loc)
    val definition = Function(name, args = List(),
      body = StoreInt8(StoreInt8(StoreInt8(StoreInt8(
        Const(123, Type.Int64, loc), 16, v), 32, v), 40, v), 56, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xAB00ABAB00AB007BL)(result)
  }

  test("Codegen - StoreInt8 - 03") {
    val v = Const(0xFF, Type.Int8, loc)
    def definition(offset: Int) = Function(name, args = List(),
      body = StoreInt8(Const(0, Type.Int64, loc), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0xFFL << (i * 8))(result)
    }
  }

  test("Codegen - StoreInt8 - 04") {
    val v = Unary(UnaryOperator.Negate, Const(0xAB, Type.Int8, loc), Type.Int8, loc)
    val definition = Function(name, args = List(),
      StoreInt8(Const(0, Type.Int64, loc), 40, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000540000000000L)(result)
  }

  test("Codegen - StoreInt16 - 01") {
    val v = Const(0xCAFE, Type.Int16, loc)
    val definition = Function(name, args = List(),
      StoreInt16(Const(0, Type.Int64, loc), 48, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xCAFE000000000000L)(result)
  }

  test("Codegen - StoreInt16 - 02") {
    val v = Const(0xCAFE, Type.Int16, loc)
    val definition = Function(name, args = List(),
      body = StoreInt16(StoreInt16(
        Const(0x00000000007B0000L, Type.Int64, loc), 0, v), 32, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000CAFE007BCAFEL)(result)
  }

  test("Codegen - StoreInt16 - 03") {
    val v = Const(0xFFFF, Type.Int16, loc)
    def definition(offset: Int) = Function(name, args = List(),
      body = StoreInt16(Const(0, Type.Int64, loc), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0xFFFFL << (i * 16))(result)
    }
  }

  test("Codegen - StoreInt16 - 04") {
    val v = Unary(UnaryOperator.Negate, Const(0xCAFE, Type.Int16, loc), Type.Int16, loc)
    val definition = Function(name, args = List(),
      StoreInt16(Const(0, Type.Int64, loc), 48, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x3501000000000000L)(result)
  }

  test("Codegen - StoreInt32 - 01") {
    val v = Const(0xDEADBEEF, Type.Int32, loc)
    val definition = Function(name, args = List(),
      StoreInt32(Const(0, Type.Int64, loc), 0, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x00000000DEADBEEFL)(result)
  }

  test("Codegen - StoreInt32 - 02") {
    val v = Const(0xDEADBEEF, Type.Int32, loc)
    val definition = Function(name, args = List(),
      body = StoreInt32(
        Const(123, Type.Int64, loc), 32, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xDEADBEEF0000007BL)(result)
  }

  test("Codegen - StoreInt32 - 03") {
    val v = Const(0xFFFFFFFF, Type.Int32, loc)
    def definition(offset: Int) = Function(name, args = List(),
      body = StoreInt32(Const(0, Type.Int64, loc), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0xFFFFFFFFL << (i * 32))(result)
    }
  }

  test("Codegen - StoreInt32 - 04") {
    val v = Unary(UnaryOperator.Negate, Const(0xDEADBEEF, Type.Int32, loc), Type.Int32, loc)
    val definition = Function(name, args = List(),
      StoreInt32(Const(0, Type.Int64, loc), 0, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000000021524110L)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int constants                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Const01") {
    val definition = Function(name, args = List(),
      body = Const(42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(42)(result)
  }

  test("Codegen - Const02") {
    // Constants -1 to 5 (inclusive) each have their own instruction

    val name_m1 = Name.Resolved.mk(List("foo", "bar", "f_m1"))
    val name_0 = Name.Resolved.mk(List("foo", "bar", "f_0"))
    val name_1 = Name.Resolved.mk(List("foo", "bar", "f_1"))
    val name_2 = Name.Resolved.mk(List("foo", "bar", "f_2"))
    val name_3 = Name.Resolved.mk(List("foo", "bar", "f_3"))
    val name_4 = Name.Resolved.mk(List("foo", "bar", "f_4"))
    val name_5 = Name.Resolved.mk(List("foo", "bar", "f_5"))

    val def_m1 = Function(name_m1, args = List(),
      body = Const(-1, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_0 = Function(name_0, args = List(),
      body = Const(0, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_1 = Function(name_1, args = List(),
      body = Const(1, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_2 = Function(name_2, args = List(),
      body = Const(2, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_3 = Function(name_3, args = List(),
      body = Const(3, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_4 = Function(name_4, args = List(),
      body = Const(4, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def_5 = Function(name_5, args = List(),
      body = Const(5, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(def_m1, def_0, def_1, def_2, def_3, def_4, def_5))

    val result_m1 = code.call(name_m1)
    val result_0 = code.call(name_0)
    val result_1 = code.call(name_1)
    val result_2 = code.call(name_2)
    val result_3 = code.call(name_3)
    val result_4 = code.call(name_4)
    val result_5 = code.call(name_5)

    assertResult(-1)(result_m1)
    assertResult(0)(result_0)
    assertResult(1)(result_1)
    assertResult(2)(result_2)
    assertResult(3)(result_3)
    assertResult(4)(result_4)
    assertResult(5)(result_5)
  }

  test("Codegen - Const03") {
    // Test some constants that are loaded with a BIPUSH, i.e. i <- [Byte.MinValue, -1) UNION (5, Byte,MaxValue]

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Const(Byte.MinValue, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def02 = Function(name02, args = List(),
      body = Const(Byte.MinValue + 42, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def03 = Function(name03, args = List(),
      body = Const(-2, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def04 = Function(name04, args = List(),
      body = Const(6, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def05 = Function(name05, args = List(),
      body = Const(Byte.MaxValue - 42, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)
    val def06 = Function(name06, args = List(),
      body = Const(Byte.MaxValue, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(Byte.MinValue)(result01)
    assertResult(Byte.MinValue + 42)(result02)
    assertResult(-2)(result03)
    assertResult(6)(result04)
    assertResult(Byte.MaxValue - 42)(result05)
    assertResult(Byte.MaxValue)(result06)
  }

  test("Codegen - Const04") {
    // Test some constants that are loaded with an SIPUSH, i.e. i <- [Short.MinValue, Byte.MinValue) UNION (Byte.MaxValue, Short,MaxValue]

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Const(Short.MinValue, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)
    val def02 = Function(name02, args = List(),
      body = Const(Short.MinValue + 42, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)
    val def03 = Function(name03, args = List(),
      body = Const(Byte.MinValue - 1, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)
    val def04 = Function(name04, args = List(),
      body = Const(Byte.MaxValue + 1, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)
    val def05 = Function(name05, args = List(),
      body = Const(Short.MaxValue - 42, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)
    val def06 = Function(name06, args = List(),
      body = Const(Short.MaxValue, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(Short.MinValue)(result01)
    assertResult(Short.MinValue + 42)(result02)
    assertResult(Byte.MinValue - 1)(result03)
    assertResult(Byte.MaxValue + 1)(result04)
    assertResult(Short.MaxValue - 42)(result05)
    assertResult(Short.MaxValue)(result06)
  }

  test("Codegen - Const05") {
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

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(Int.MinValue)(result01)
    assertResult(Int.MinValue + 42)(result02)
    assertResult(Short.MinValue - 1)(result03)
    assertResult(Short.MaxValue + 1)(result04)
    assertResult(Int.MaxValue - 42)(result05)
    assertResult(Int.MaxValue)(result06)
  }

  test("Codegen - Const06") {
    val definition = Function(name, args = List(),
      body = Const(1, Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(true)(result)
  }

  test("Codegen - Const07") {
    val definition = Function(name, args = List(),
      body = Const(1, Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const08") {
    val definition = Function(name, args = List(),
      body = Const(1, Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const09") {
    val definition = Function(name, args = List(),
      body = Const(1, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const10") {
    val definition = Function(name, args = List(),
      body = Const(0, Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(0)(result)
  }

  test("Codegen - Const11") {
    val definition = Function(name, args = List(),
      body = Const(1, Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const12") {
    val definition = Function(name, args = List(),
      body = Const(123456789123456789L, Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(123456789123456789L)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Variables                                                               //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Var01") {
    val definition = Function(name, args = List("x"),
      body = Const(-1, Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(Integer.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(-1)(result)
  }

  test("Codegen - Var02") {
    val definition = Function(name, args = List("x"),
      body = Var(LocalVar(0, "x"), Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(Integer.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(42)(result)
  }

  test("Codegen - Var03") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(LocalVar(1, "y"), Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, Integer.TYPE, Integer.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Var04") {
    val definition = Function(name, args = List("x"),
      body = Var(LocalVar(0, "x"), Type.Int64, loc),
      Type.Lambda(List(Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(java.lang.Long.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(42)(result)
  }

  test("Codegen - Var05") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(LocalVar(2, "y"), Type.Int64, loc),
      Type.Lambda(List(Type.Int64, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Long.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Var06") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(LocalVar(1, "y"), Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Let expression                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Let01") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(42, Type.Int32, loc),
        Const(-1, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-1)(result)
  }

  test("Codegen - Let02") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(42, Type.Int32, loc),
        Var(LocalVar(0, "x"), Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Let03") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(1, "y"), Const(-101010, Type.Int32, loc),
          Let(LocalVar(2, "z"), Const(42, Type.Int32, loc),
            Var(LocalVar(1, "y"), Type.Int32, loc),
            Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-101010)(result)
  }

  test("Codegen - Let04") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(3, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(4, "y"), Const(-101010, Type.Int32, loc),
          Let(LocalVar(5, "z"), Const(42, Type.Int32, loc),
            Var(LocalVar(4, "y"), Type.Int32, loc),
            Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, Integer.TYPE, Integer.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Let05") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(3, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(4, "y"), Const(-101010, Type.Int32, loc),
          Let(LocalVar(5, "z"), Const(42, Type.Int32, loc),
            Var(LocalVar(1, "b"), Type.Int32, loc),
            Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, Integer.TYPE, Integer.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(101010)(result)
  }

  test("Codegen - Let06") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(42, Type.Int64, loc),
        Var(LocalVar(0, "x"), Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Let07") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(1337, Type.Int64, loc),
        Let(LocalVar(2, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(4, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(2, "y"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-101010)(result)
  }

  test("Codegen - Let08") {
    val definition = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(1, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(3, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(1, "y"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-101010)(result)
  }

  test("Codegen - Let09") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(6, "x"), Const(1337, Type.Int64, loc),
        Let(LocalVar(8, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(10, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(8, "y"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int64, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Long.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Let10") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(5, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(6, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(8, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(6, "y"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }


  test("Codegen - Let11") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(6, "x"), Const(1337, Type.Int64, loc),
        Let(LocalVar(8, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(10, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(2, "b"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int64, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Long.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(101010)(result)
  }

  test("Codegen - Let12") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(LocalVar(5, "x"), Const(1337, Type.Int32, loc),
        Let(LocalVar(6, "y"), Const(-101010, Type.Int64, loc),
          Let(LocalVar(8, "z"), Const(42, Type.Int64, loc),
            Var(LocalVar(1, "b"), Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(101010)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Function application                                                    //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Apply01") {
    // def main(): Int = f()
    // def f(): Int = 24
    val main = Function(name, args = List(),
      body = Apply(name01, List(), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List(),
      body = Const(24, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(24)(result)
  }

  test("Codegen - Apply02") {
    // def main(): Int = f(3)
    // def f(x: Int): Int = 24
    val main = Function(name, args = List(),
      body = Apply(name01, List(Const(3, Type.Int32, loc)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Const(24, Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(24)(result)
  }

  test("Codegen - Apply03") {
    // def main(): Int = f(3)
    // def f(x: Int): Int = x
    val main = Function(name, args = List(),
      body = Apply(name01, List(Const(3, Type.Int32, loc)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Var(LocalVar(0, "x"), Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(3)(result)
  }

  test("Codegen - Apply04") {
    // def main(): Int = f(3, 42)
    // def f(x: Int, y: Int): Int = x * y - 6
    val main = Function(name, args = List(),
      body = Apply(name01, List(Const(3, Type.Int32, loc), Const(42, Type.Int32, loc)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x", "y"),
      body = Binary(BinaryOperator.Minus,
        Binary(BinaryOperator.Times,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Type.Int32, loc),
        Const(6, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(120)(result)
  }

  test("Codegen - Apply05") {
    // def main(): Int = f(5)
    // def f(x: Int): Int = let y = g(x + 1) in y * y
    // def g(x: Int): Int = x - 4
    val main = Function(name, args = List(),
      body = Apply(name01, List(Const(5, Type.Int32, loc)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Let(LocalVar(1, "y"), Apply(name02,
        List(Binary(BinaryOperator.Plus,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Const(1, Type.Int32, loc), Type.Int32, loc)), Type.Int32, loc),
        Binary(BinaryOperator.Times,
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body =Binary(BinaryOperator.Minus,
        Var(LocalVar(0, "x"), Type.Int32, loc),
        Const(4, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f, g))
    val result = code.call(name)

    assertResult(4)(result)
  }

  test("Codegen - Apply06") {
    // def main(): Int = f(3)
    // def f(x: Int): Int = g(x + 1)
    // def g(x: Int): Int = h(x + 10)
    // def h(x: Int): Int = x * x
    val main = Function(name, args = List(),
      body = Apply(name01, List(Const(3, Type.Int32, loc)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Apply(name02, List(
        Binary(BinaryOperator.Plus,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Const(1, Type.Int32, loc),
          Type.Int32, loc)),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body = Apply(name03, List(
        Binary(BinaryOperator.Plus,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Const(10, Type.Int32, loc),
          Type.Int32, loc)),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val h = Function(name03, args = List("x"),
      body = Binary(BinaryOperator.Times,
        Var(LocalVar(0, "x"), Type.Int32, loc),
        Var(LocalVar(0, "x"), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f, g, h))
    val result = code.call(name)

    assertResult(196)(result)
  }

  test("Codegen - Apply07") {
    // def main(): Int = let x = 7 in f(g(3), h(h(x)))
    // def f(x: Int, y: Int): Int = x - y
    // def g(x: Int): Int = x * 3
    // def h(x: Int): Int = g(x - 1)
    val main = Function(name, args = List(),
      body = Let(LocalVar(0, "x"), Const(7, Type.Int32, loc),
        Apply(name01, List(
          Apply(name02, List(Const(3, Type.Int32, loc)), Type.Int32, loc),
          Apply(name03, List(
            Apply(name03, List(Var(LocalVar(0, "x"), Type.Int32, loc)),
              Type.Int32, loc)), Type.Int32, loc)),
          Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x", "y"),
      body = Binary(BinaryOperator.Minus,
        Var(LocalVar(0, "x"), Type.Int32, loc),
        Var(LocalVar(1, "y"), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body = Binary(BinaryOperator.Times,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Const(3, Type.Int32, loc),
          Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val h = Function(name03, args = List("x"),
      body = Apply(name02, List(
        Binary(BinaryOperator.Minus,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Const(1, Type.Int32, loc),
          Type.Int32, loc)),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f, g, h))
    val result = code.call(name)

    assertResult(-42)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unary operators                                                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Unary.Not01") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Not, Const(0, Type.Bool, loc), Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Unary.Not02") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Not, Const(1, Type.Bool, loc), Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Unary.Plus01") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Const(42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Unary.Plus02") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Const(-42, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-42)(result)
  }

  test("Codegen - Unary.Minus01") {
    // Unary minus operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Byte.MaxValue, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Const(42, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Const(0, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Const(-42, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Byte.MinValue, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-Byte.MaxValue)(result01)
    assertResult(-42)(result02)
    assertResult(0)(result03)
    assertResult(42)(result04)
    assertResult(Byte.MinValue)(result05)
  }


  test("Codegen - Unary.Minus02") {
    // Unary minus operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Short.MaxValue, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Const(420, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Const(0, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Const(-420, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Short.MinValue, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-Short.MaxValue)(result01)
    assertResult(-420)(result02)
    assertResult(0)(result03)
    assertResult(420)(result04)
    assertResult(Short.MinValue)(result05)
  }

  test("Codegen - Unary.Minus03") {
    // Unary minus operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Int.MaxValue, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Const(36000, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Const(0, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Const(-36000, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Int.MinValue, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-Int.MaxValue)(result01)
    assertResult(-36000)(result02)
    assertResult(0)(result03)
    assertResult(36000)(result04)
    assertResult(Int.MinValue)(result05)
  }

  test("Codegen - Unary.Minus04") {
    // Unary minus operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Long.MaxValue, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Const(10000000000L, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Const(0, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Const(-10000000000L, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Const(Long.MinValue, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-Long.MaxValue)(result01)
    assertResult(-10000000000L)(result02)
    assertResult(0)(result03)
    assertResult(10000000000L)(result04)
    assertResult(Long.MinValue)(result05)
  }

  test("Codegen - Unary.Negate01") {
    // Unary negation operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))
    val name07 = Name.Resolved.mk(List("foo", "bar", "f07"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Byte.MaxValue, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Negate, Const(42, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Negate, Const(1, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Negate, Const(0, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-1, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-42, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Byte.MinValue, Type.Int8, loc), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(Byte.MinValue)(result01)
    assertResult(-43)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(41)(result06)
    assertResult(Byte.MaxValue)(result07)
  }

  test("Codegen - Unary.Negate02") {
    // Unary negation operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))
    val name07 = Name.Resolved.mk(List("foo", "bar", "f07"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Short.MaxValue, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Negate, Const(420, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Negate, Const(1, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Negate, Const(0, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-1, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-420, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Short.MinValue, Type.Int16, loc), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(Short.MinValue)(result01)
    assertResult(-421)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(419)(result06)
    assertResult(Short.MaxValue)(result07)
  }

  test("Codegen - Unary.Negate03") {
    // Unary negation operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))
    val name07 = Name.Resolved.mk(List("foo", "bar", "f07"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Int.MaxValue, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Negate, Const(36000, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Negate, Const(1, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Negate, Const(0, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-1, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-36000, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Int.MinValue, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(Int.MinValue)(result01)
    assertResult(-36001)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(35999)(result06)
    assertResult(Int.MaxValue)(result07)
  }

  test("Codegen - Unary.Negate04") {
    // Unary negation operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))
    val name07 = Name.Resolved.mk(List("foo", "bar", "f07"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Long.MaxValue, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Negate, Const(10000000000L, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Negate, Const(1, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Negate, Const(0, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-1, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.Negate, Const(-10000000000L, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.Negate, Const(Long.MinValue, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(Long.MinValue)(result01)
    assertResult(-10000000001L)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(9999999999L)(result06)
    assertResult(Long.MaxValue)(result07)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators                                                        //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Binary.And01") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.And,
        Const(1, Type.Bool, loc),
        Const(1, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.And02") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.And,
        Const(1, Type.Bool, loc),
        Const(0, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And03") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.And,
        Const(0, Type.Bool, loc),
        Const(0, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And04") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.And,
        Const(0, Type.Bool, loc),
        Const(1, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.Or01") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Or,
        Const(1, Type.Bool, loc),
        Const(1, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or02") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Or,
        Const(1, Type.Bool, loc),
        Const(0, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or03") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Or,
        Const(0, Type.Bool, loc),
        Const(0, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.Or04") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.Or,
        Const(0, Type.Bool, loc),
        Const(1, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Plus01") {
    // Binary plus operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Byte.MaxValue, Type.Int8, loc),
        Const(1, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(10, Type.Int8, loc),
        Const(40, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-40, Type.Int8, loc),
        Const(10, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-10, Type.Int8, loc),
        Const(40, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Byte.MinValue, Type.Int8, loc),
        Const(-1, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Byte.MinValue)(result01)
    assertResult(50)(result02)
    assertResult(-30)(result03)
    assertResult(30)(result04)
    assertResult(Byte.MaxValue)(result05)
  }

  test("Codegen - Binary.Plus02") {
    // Binary plus operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Short.MaxValue, Type.Int16, loc),
        Const(1, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(1000, Type.Int16, loc),
        Const(4000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-4000, Type.Int16, loc),
        Const(1000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-1000, Type.Int16, loc),
        Const(4000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Short.MinValue, Type.Int16, loc),
        Const(-1, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Short.MinValue)(result01)
    assertResult(5000)(result02)
    assertResult(-3000)(result03)
    assertResult(3000)(result04)
    assertResult(Short.MaxValue)(result05)
  }

  test("Codegen - Binary.Plus03") {
    // Binary plus operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Int.MaxValue, Type.Int32, loc),
        Const(1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(100000, Type.Int32, loc),
        Const(400000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-400000, Type.Int32, loc),
        Const(100000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-100000, Type.Int32, loc),
        Const(400000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Int.MinValue, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Int.MinValue)(result01)
    assertResult(500000)(result02)
    assertResult(-300000)(result03)
    assertResult(300000)(result04)
    assertResult(Int.MaxValue)(result05)
  }

  test("Codegen - Binary.Plus04") {
    // Binary plus operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Long.MaxValue, Type.Int64, loc),
        Const(1, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(10000000000L, Type.Int64, loc),
        Const(40000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-40000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(-10000000000L, Type.Int64, loc),
        Const(40000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Const(Long.MinValue, Type.Int64, loc),
        Const(-1, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Long.MinValue)(result01)
    assertResult(50000000000L)(result02)
    assertResult(-30000000000L)(result03)
    assertResult(30000000000L)(result04)
    assertResult(Long.MaxValue)(result05)
  }

  test("Codegen - Binary.Minus01") {
    // Binary minus operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Byte.MinValue, Type.Int8, loc),
        Const(1, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(40, Type.Int8, loc),
        Const(10, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-40, Type.Int8, loc),
        Const(10, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-10, Type.Int8, loc),
        Const(40, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Byte.MaxValue, Type.Int8, loc),
        Const(-1, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Byte.MaxValue)(result01)
    assertResult(30)(result02)
    assertResult(-50)(result03)
    assertResult(-50)(result04)
    assertResult(Byte.MinValue)(result05)
  }

  test("Codegen - Binary.Minus02") {
    // Binary minus operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Short.MinValue, Type.Int16, loc),
        Const(1, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(4000, Type.Int16, loc),
        Const(1000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-4000, Type.Int16, loc),
        Const(1000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-1000, Type.Int16, loc),
        Const(4000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Short.MaxValue, Type.Int16, loc),
        Const(-1, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Short.MaxValue)(result01)
    assertResult(3000)(result02)
    assertResult(-5000)(result03)
    assertResult(-5000)(result04)
    assertResult(Short.MinValue)(result05)
  }

  test("Codegen - Binary.Minus03") {
    // Binary minus operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Int.MinValue, Type.Int32, loc),
        Const(1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(400000, Type.Int32, loc),
        Const(100000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-400000, Type.Int32, loc),
        Const(100000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-100000, Type.Int32, loc),
        Const(400000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Int.MaxValue, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Int.MaxValue)(result01)
    assertResult(300000)(result02)
    assertResult(-500000)(result03)
    assertResult(-500000)(result04)
    assertResult(Int.MinValue)(result05)
  }

  test("Codegen - Binary.Minus04") {
    // Binary minus operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Long.MinValue, Type.Int64, loc),
        Const(1, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(40000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-40000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(-10000000000L, Type.Int64, loc),
        Const(40000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Const(Long.MaxValue, Type.Int64, loc),
        Const(-1, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(Long.MaxValue)(result01)
    assertResult(30000000000L)(result02)
    assertResult(-50000000000L)(result03)
    assertResult(-50000000000L)(result04)
    assertResult(Long.MinValue)(result05)
  }

  test("Codegen - Binary.Times01") {
    // Binary times operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Byte.MaxValue, Type.Int8, loc),
        Const(2, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(3, Type.Int8, loc),
        Const(2, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-2, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-2, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Byte.MinValue, Type.Int8, loc),
        Const(-1, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-2)(result01)
    assertResult(6)(result02)
    assertResult(-6)(result03)
    assertResult(6)(result04)
    assertResult(Byte.MinValue)(result05)
  }

  test("Codegen - Binary.Times02") {
    // Binary times operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Short.MaxValue, Type.Int16, loc),
        Const(2, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(30, Type.Int16, loc),
        Const(20, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-20, Type.Int16, loc),
        Const(30, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-20, Type.Int16, loc),
        Const(-30, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Short.MinValue, Type.Int16, loc),
        Const(-1, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-2)(result01)
    assertResult(600)(result02)
    assertResult(-600)(result03)
    assertResult(600)(result04)
    assertResult(Short.MinValue)(result05)
  }

  test("Codegen - Binary.Times03") {
    // Binary times operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Int.MaxValue, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(300, Type.Int32, loc),
        Const(200, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-200, Type.Int32, loc),
        Const(300, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-200, Type.Int32, loc),
        Const(-300, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Int.MinValue, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-2)(result01)
    assertResult(60000)(result02)
    assertResult(-60000)(result03)
    assertResult(60000)(result04)
    assertResult(Int.MinValue)(result05)
  }

  test("Codegen - Binary.Times04") {
    // Binary times operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Long.MaxValue, Type.Int64, loc),
        Const(2, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(300000, Type.Int64, loc),
        Const(200000, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-200000, Type.Int64, loc),
        Const(300000, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(-200000, Type.Int64, loc),
        Const(-300000, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Const(Long.MinValue, Type.Int64, loc),
        Const(-1, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-2)(result01)
    assertResult(60000000000L)(result02)
    assertResult(-60000000000L)(result03)
    assertResult(60000000000L)(result04)
    assertResult(Long.MinValue)(result05)
  }

  test("Codegen - Binary.Divide01") {
    // Binary divide operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(4)(result01)
    assertResult(0)(result02)
    assertResult(-4)(result03)
    assertResult(0)(result04)
    assertResult(4)(result05)
  }

  test("Codegen - Binary.Divide02") {
    // Binary divide operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(12000, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(3000, Type.Int16, loc),
        Const(12000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12000, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-3000, Type.Int16, loc),
        Const(12000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-12000, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(40)(result01)
    assertResult(0)(result02)
    assertResult(-40)(result03)
    assertResult(0)(result04)
    assertResult(40)(result05)
  }

  test("Codegen - Binary.Divide03") {
    // Binary divide operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(1200000, Type.Int32, loc),
        Const(3000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(300000, Type.Int32, loc),
        Const(1200000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-1200000, Type.Int32, loc),
        Const(3000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-300000, Type.Int32, loc),
        Const(1200000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-1200000, Type.Int32, loc),
        Const(-3000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(400)(result01)
    assertResult(0)(result02)
    assertResult(-400)(result03)
    assertResult(0)(result04)
    assertResult(400)(result05)
  }

  test("Codegen - Binary.Divide04") {
    // Binary divide operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(120000000000L, Type.Int64, loc),
        Const(3, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(30000000000L, Type.Int64, loc),
        Const(120000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-120000000000L, Type.Int64, loc),
        Const(3, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-30000000000L, Type.Int64, loc),
        Const(120000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Const(-120000000000L, Type.Int64, loc),
        Const(-3, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(40000000000L)(result01)
    assertResult(0)(result02)
    assertResult(-40000000000L)(result03)
    assertResult(0)(result04)
    assertResult(40000000000L)(result05)
  }

  test("Codegen - Binary.Modulo01") {
    // Binary modulo operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int8, loc),
        Const(2, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int8, loc),
        Const(5, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12, Type.Int8, loc),
        Const(5, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12, Type.Int8, loc),
        Const(-5, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12, Type.Int8, loc),
        Const(-5, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0)(result01)
    assertResult(2)(result02)
    assertResult(-2)(result03)
    assertResult(2)(result04)
    assertResult(-2)(result05)
  }

  test("Codegen - Binary.Modulo02") {
    // Binary modulo operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12000, Type.Int16, loc),
        Const(2000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12000, Type.Int16, loc),
        Const(5000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12000, Type.Int16, loc),
        Const(5000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(12000, Type.Int16, loc),
        Const(-5000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-12000, Type.Int16, loc),
        Const(-5000, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0)(result01)
    assertResult(2000)(result02)
    assertResult(-2000)(result03)
    assertResult(2000)(result04)
    assertResult(-2000)(result05)
  }

  test("Codegen - Binary.Modulo03") {
    // Binary modulo operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(1200000, Type.Int32, loc),
        Const(200000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(1200000, Type.Int32, loc),
        Const(500000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-1200000, Type.Int32, loc),
        Const(500000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(1200000, Type.Int32, loc),
        Const(-500000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-1200000, Type.Int32, loc),
        Const(-500000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0)(result01)
    assertResult(200000)(result02)
    assertResult(-200000)(result03)
    assertResult(200000)(result04)
    assertResult(-200000)(result05)
  }

  test("Codegen - Binary.Modulo04") {
    // Binary modulo operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(120000000000L, Type.Int64, loc),
        Const(20000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(120000000000L, Type.Int64, loc),
        Const(50000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-120000000000L, Type.Int64, loc),
        Const(50000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(120000000000L, Type.Int64, loc),
        Const(-50000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Const(-120000000000L, Type.Int64, loc),
        Const(-50000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0)(result01)
    assertResult(20000000000L)(result02)
    assertResult(-20000000000L)(result03)
    assertResult(20000000000L)(result04)
    assertResult(-20000000000L)(result05)
  }

  test("Codegen - Binary.BitwiseAnd01") {
    // Binary bitwise and operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int8, loc),
        Const(42, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(42, Type.Int8, loc),
        Const(0, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(0xFF, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(-1.toByte, Type.Int8, loc),
        Const(-1.toByte, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(42)(result01)
    assertResult(42)(result02)
    assertResult(0)(result03)
    assertResult(0xFF)(result04)
    assertResult(-1.toByte)(result05)
  }

  test("Codegen - Binary.BitwiseAnd02") {
    // Binary bitwise and operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(420, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(420, Type.Int16, loc),
        Const(420, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(420, Type.Int16, loc),
        Const(0, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(0xFFFF, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(-1.toShort, Type.Int16, loc),
        Const(-1.toShort, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(420)(result01)
    assertResult(420)(result02)
    assertResult(0)(result03)
    assertResult(0xFFFF)(result04)
    assertResult(-1.toShort)(result05)
  }

  test("Codegen - Binary.BitwiseAnd03") {
    // Binary bitwise and operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(40000, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(40000, Type.Int32, loc),
        Const(40000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(40000, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(0xFFFFFFFF, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(-1, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(40000)(result01)
    assertResult(40000)(result02)
    assertResult(0)(result03)
    assertResult(0xFFFFFFFF)(result04)
    assertResult(-1)(result05)
  }

  test("Codegen - Binary.BitwiseAnd04") {
    // Binary bitwise and operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(10000000000L, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(10000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(10000000000L, Type.Int64, loc),
        Const(0, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Const(-1L, Type.Int64, loc),
        Const(-1L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(10000000000L)(result01)
    assertResult(10000000000L)(result02)
    assertResult(0L)(result03)
    assertResult(0xFFFFFFFFFFFFFFFFL)(result04)
    assertResult(-1L)(result05)
  }

  test("Codegen - Binary.BitwiseOr01") {
    // Binary bitwise or operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int8, loc),
        Const(42, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(42, Type.Int8, loc),
        Const(0, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(0xFF, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(-1.toByte, Type.Int8, loc),
        Const(-1.toByte, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFF)(result01)
    assertResult(42)(result02)
    assertResult(42)(result03)
    assertResult(0xFF)(result04)
    assertResult(-1.toByte)(result05)
  }

  test("Codegen - Binary.BitwiseOr02") {
    // Binary bitwise or operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(420, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(420, Type.Int16, loc),
        Const(420, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(420, Type.Int16, loc),
        Const(0, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(0xFFFF, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(-1.toShort, Type.Int16, loc),
        Const(-1.toShort, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFFFF)(result01)
    assertResult(420)(result02)
    assertResult(420)(result03)
    assertResult(0xFFFF)(result04)
    assertResult(-1.toShort)(result05)
  }

  test("Codegen - Binary.BitwiseOr03") {
    // Binary bitwise or operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(40000, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(40000, Type.Int32, loc),
        Const(40000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(40000, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(0xFFFFFFFF, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(-1, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFFFFFFFF)(result01)
    assertResult(40000)(result02)
    assertResult(40000)(result03)
    assertResult(0xFFFFFFFF)(result04)
    assertResult(-1)(result05)
  }

  test("Codegen - Binary.BitwiseOr04") {
    // Binary bitwise or operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(10000000000L, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(10000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(10000000000L, Type.Int64, loc),
        Const(0, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Const(-1L, Type.Int64, loc),
        Const(-1L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFFFFFFFFFFFFFFFFL)(result01)
    assertResult(10000000000L)(result02)
    assertResult(10000000000L)(result03)
    assertResult(0xFFFFFFFFFFFFFFFFL)(result04)
    assertResult(-1L)(result05)
  }

  test("Codegen - Binary.BitwiseXor01") {
    // Binary bitwise xor operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int8, loc),
        Const(42, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(42, Type.Int8, loc),
        Const(0, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(0xFF, Type.Int8, loc),
        Const(0xFF, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(-1.toByte, Type.Int8, loc),
        Const(-1.toByte, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(213)(result01)
    assertResult(0)(result02)
    assertResult(42)(result03)
    assertResult(0)(result04)
    assertResult(0)(result05)
  }

  test("Codegen - Binary.BitwiseXor02") {
    // Binary bitwise xor operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(420, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(420, Type.Int16, loc),
        Const(420, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(420, Type.Int16, loc),
        Const(0, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(0xFFFF, Type.Int16, loc),
        Const(0xFFFF, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(-1.toShort, Type.Int16, loc),
        Const(-1.toShort, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(65115)(result01)
    assertResult(0)(result02)
    assertResult(420)(result03)
    assertResult(0)(result04)
    assertResult(0)(result05)
  }

  test("Codegen - Binary.BitwiseXor03") {
    // Binary bitwise xor operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(40000, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(40000, Type.Int32, loc),
        Const(40000, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(40000, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(0xFFFFFFFF, Type.Int32, loc),
        Const(0xFFFFFFFF, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(-1, Type.Int32, loc),
        Const(-1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-40001)(result01)
    assertResult(0)(result02)
    assertResult(40000)(result03)
    assertResult(0)(result04)
    assertResult(0)(result05)
  }

  test("Codegen - Binary.BitwiseXor04") {
    // Binary bitwise xor operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(10000000000L, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(10000000000L, Type.Int64, loc),
        Const(10000000000L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(10000000000L, Type.Int64, loc),
        Const(0, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Const(0xFFFFFFFFFFFFFFFFL, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Const(-1L, Type.Int64, loc),
        Const(-1L, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-10000000001L)(result01)
    assertResult(0L)(result02)
    assertResult(10000000000L)(result03)
    assertResult(0L)(result04)
    assertResult(0L)(result05)
  }

  test("Codegen - Binary.BitwiseLeftShift01") {
    // Bitwise left shift applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int8, loc),
        Const(0, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int8, loc),
        Const(2, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int8, loc),
        Const(4, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int8, loc),
        Const(5, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x20)(result02)
    assertResult(Byte.MinValue)(result03)
    assertResult(0)(result04)
  }

  test("Codegen - Binary.BitwiseLeftShift02") {
    // Bitwise left shift applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int16, loc),
        Const(0, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int16, loc),
        Const(8, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int16, loc),
        Const(12, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int16, loc),
        Const(13, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x0800)(result02)
    assertResult(Short.MinValue)(result03)
    assertResult(0)(result04)
  }

  test("Codegen - Binary.BitwiseLeftShift03") {
    // Bitwise left shift applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int32, loc),
        Const(16, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int32, loc),
        Const(28, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int32, loc),
        Const(29, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x00080000)(result02)
    assertResult(Int.MinValue)(result03)
    assertResult(0)(result04)
  }

  test("Codegen - Binary.BitwiseLeftShift04") {
    // Bitwise left shift applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int64, loc),
        Const(0, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int64, loc),
        Const(32, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int64, loc),
        Const(60, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Const(0x08, Type.Int64, loc),
        Const(61, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x0000000800000000L)(result02)
    assertResult(Long.MinValue)(result03)
    assertResult(0)(result04)
  }

  test("Codegen - Binary.BitwiseRightShift01") {
    // Bitwise right shift applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(123, Type.Int8, loc),
        Const(0, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(123, Type.Int8, loc),
        Const(2, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(123, Type.Int8, loc),
        Const(7, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(-123.toByte, Type.Int8, loc),
        Const(2, Type.Int32, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(123)(result01)
    assertResult(30)(result02)
    assertResult(0)(result03)
    assertResult(-31)(result04)
  }

  test("Codegen - Binary.BitwiseRightShift02") {
    // Bitwise right shift applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000, Type.Int16, loc),
        Const(0, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000, Type.Int16, loc),
        Const(2, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000, Type.Int16, loc),
        Const(15, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(-12000.toShort, Type.Int16, loc),
        Const(2, Type.Int32, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(12000)(result01)
    assertResult(3000)(result02)
    assertResult(0)(result03)
    assertResult(-3000)(result04)
  }

  test("Codegen - Binary.BitwiseRightShift03") {
    // Bitwise right shift applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(120000, Type.Int32, loc),
        Const(0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(120000, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(120000, Type.Int32, loc),
        Const(31, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(-120000, Type.Int32, loc),
        Const(2, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(120000)(result01)
    assertResult(30000)(result02)
    assertResult(0)(result03)
    assertResult(-30000)(result04)
  }

  test("Codegen - Binary.BitwiseRightShift04") {
    // Bitwise right shift applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000000000L, Type.Int64, loc),
        Const(0, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000000000L, Type.Int64, loc),
        Const(2, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(12000000000L, Type.Int64, loc),
        Const(63, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Const(-12000000000L, Type.Int64, loc),
        Const(2, Type.Int32, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(12000000000L)(result01)
    assertResult(3000000000L)(result02)
    assertResult(0)(result03)
    assertResult(-3000000000L)(result04)
  }

  test("Codegen - Binary.Less01") {
    // < operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Less02") {
    // < operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Less03") {
    // < operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Less04") {
    // < operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.LessEqual01") {
    // <= operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(true)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.LessEqual02") {
    // <= operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(true)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.LessEqual03") {
    // <= operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(true)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.LessEqual04") {
    // <= operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(true)(result02)
    assertResult(true)(result03)
    assertResult(true)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.Greater01") {
    // > operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(false)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Greater02") {
    // > operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(false)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Greater03") {
    // > operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(false)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.Greater04") {
    // > operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(false)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.GreaterEqual01") {
    // >= operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.GreaterEqual02") {
    // >= operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.GreaterEqual03") {
    // >= operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.GreaterEqual04") {
    // >= operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(true)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.Equal01") {
    // == operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.Equal02") {
    // == operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.Equal03") {
    // == operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.Equal04") {
    // == operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(false)(result01)
    assertResult(false)(result02)
    assertResult(true)(result03)
    assertResult(false)(result04)
    assertResult(false)(result05)
    assertResult(true)(result06)
  }

  test("Codegen - Binary.NotEqual01") {
    // != operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(12, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(3, Type.Int8, loc),
        Const(12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(3, Type.Int8, loc),
        Const(3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-12, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-3, Type.Int8, loc),
        Const(-12, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-3, Type.Int8, loc),
        Const(-3, Type.Int8, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.NotEqual02") {
    // != operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(1200, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(300, Type.Int16, loc),
        Const(1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(300, Type.Int16, loc),
        Const(300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-1200, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-300, Type.Int16, loc),
        Const(-1200, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-300, Type.Int16, loc),
        Const(-300, Type.Int16, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.NotEqual03") {
    // != operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(120000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(30000, Type.Int32, loc),
        Const(120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(30000, Type.Int32, loc),
        Const(30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-120000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-30000, Type.Int32, loc),
        Const(-120000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-30000, Type.Int32, loc),
        Const(-30000, Type.Int32, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - Binary.NotEqual04") {
    // != operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))
    val name06 = Name.Resolved.mk(List("foo", "bar", "f06"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(12000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(3000000000L, Type.Int64, loc),
        Const(3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-12000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-12000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Const(-3000000000L, Type.Int64, loc),
        Const(-3000000000L, Type.Int64, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(true)(result01)
    assertResult(true)(result02)
    assertResult(false)(result03)
    assertResult(true)(result04)
    assertResult(true)(result05)
    assertResult(false)(result06)
  }

  test("Codegen - IfThenElse01") {
    val definition = Function(name, args = List(),
      body = IfThenElse(Const(0, Type.Bool, loc),
        Binary(BinaryOperator.Plus, Const(42, Type.Int32, loc), Const(10, Type.Int32, loc), Type.Int32, loc),
        Binary(BinaryOperator.Minus, Const(42, Type.Int32, loc), Const(10, Type.Int32, loc), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(32)(result)
  }

  test("Codegen - IfThenElse02") {
    val definition = Function(name, args = List(),
      body = IfThenElse(Const(1, Type.Bool, loc),
        Binary(BinaryOperator.Plus, Const(42, Type.Int32, loc), Const(10, Type.Int32, loc), Type.Int32, loc),
        Binary(BinaryOperator.Minus, Const(42, Type.Int32, loc), Const(10, Type.Int32, loc), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(52)(result)
  }

  test("Codegen - IfThenElse03") {
    val definition = Function(name, args = List("x"),
      body = IfThenElse(Var(LocalVar(0, "x"), Type.Bool, loc),
        IfThenElse(Const(0, Type.Bool, loc), Const(1, Type.Int32, loc), Const(2, Type.Int32, loc), Type.Int32, loc),
        IfThenElse(Const(1, Type.Bool, loc), Const(3, Type.Int32, loc), Const(4, Type.Int32, loc), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Bool), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Boolean.TYPE), List(true).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Boolean.TYPE), List(false).map(_.asInstanceOf[Object]))

    assertResult(2)(result01)
    assertResult(3)(result02)
  }

  test("Codegen - IfThenElse04") {
    val definition = Function(name, args = List("x"),
      body = IfThenElse(
        IfThenElse(
          Unary(UnaryOperator.Not, Var(LocalVar(0, "x"), Type.Bool, loc), Type.Bool, loc),
          Const(1, Type.Bool, loc),
          Const(0, Type.Bool, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Bool), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Boolean.TYPE), List(true).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Boolean.TYPE), List(false).map(_.asInstanceOf[Object]))

    assertResult(5678)(result01)
    assertResult(1234)(result02)
  }

  test("Codegen - IfThenElse05") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.And,
          Var(LocalVar(0, "x"), Type.Bool, loc),
          Var(LocalVar(1, "y"), Type.Bool, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Bool, Type.Bool), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(true, true).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(false, true).map(_.asInstanceOf[Object]))
    val result03 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(true, false).map(_.asInstanceOf[Object]))
    val result04 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(false, false).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
    assertResult(5678)(result03)
    assertResult(5678)(result04)
  }

  test("Codegen - IfThenElse06") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.Or,
          Var(LocalVar(0, "x"), Type.Bool, loc),
          Var(LocalVar(1, "y"), Type.Bool, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Bool, Type.Bool), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(true, true).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(false, true).map(_.asInstanceOf[Object]))
    val result03 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(true, false).map(_.asInstanceOf[Object]))
    val result04 = code.call(name, List(java.lang.Boolean.TYPE, java.lang.Boolean.TYPE),
      List(false, false).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(1234)(result02)
    assertResult(1234)(result03)
    assertResult(5678)(result04)
  }

  test("Codegen - IfThenElse07") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.Less,
          Var(LocalVar(0, "x"), Type.Int8, loc),
          Var(LocalVar(1, "y"), Type.Int8, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int8, Type.Int8), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Byte.TYPE, java.lang.Byte.TYPE), List(5.toByte, 24.toByte).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Byte.TYPE, java.lang.Byte.TYPE), List(5.toByte, 5.toByte).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse08") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.LessEqual,
          Var(LocalVar(0, "x"), Type.Int16, loc),
          Var(LocalVar(1, "y"), Type.Int16, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int16, Type.Int16), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Short.TYPE, java.lang.Short.TYPE), List(500.toShort, 500.toShort).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Short.TYPE, java.lang.Short.TYPE), List(500.toShort, 200.toShort).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse09") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.Greater,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(2400000, 500000).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(500000, 500000).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse10") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.GreaterEqual,
          Var(LocalVar(0, "x"), Type.Int64, loc),
          Var(LocalVar(2, "y"), Type.Int64, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int64, Type.Int64), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Long.TYPE, java.lang.Long.TYPE), List(50000000000L, 50000000000L).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Long.TYPE, java.lang.Long.TYPE), List(20000000000L, 50000000000L).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse11") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.Equal,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(5, 5).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(2, 5).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse12") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.NotEqual,
          Var(LocalVar(0, "x"), Type.Int32, loc),
          Var(LocalVar(1, "y"), Type.Int32, loc),
          Type.Bool, loc),
        Const(1234, Type.Int32, loc),
        Const(5678, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(2, 5).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(Integer.TYPE, Integer.TYPE), List(5, 5).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }
}
