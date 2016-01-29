package ca.uwaterloo.flix.language.backend.phase

import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Function
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.Codegen
import ca.uwaterloo.flix.runtime.Value
import org.scalatest.FunSuite

class TestCodegen extends FunSuite {

  val loc = SourceLocation.Unknown
  val sp = SourcePosition.Unknown
  val compiledClassName = "ca.uwaterloo.flix.compiled.FlixDefinitions"

  val name = Name.Resolved.mk(List("foo", "bar", "main"))
  val name01 = Name.Resolved.mk(List("foo", "bar", "f"))
  val name02 = Name.Resolved.mk(List("foo", "bar", "g"))
  val name03 = Name.Resolved.mk(List("foo", "bar", "h"))

  def toIdent(s: String): Name.Ident = Name.Ident(sp, s, sp)

  val constPropName = Name.Resolved.mk(List("foo", "bar", "baz", "ConstProp"))
  val identB = toIdent("Bot")
  val identV = toIdent("Val")
  val identT = toIdent("Top")

  val tagTpeB = Type.Tag(constPropName, identB, Type.Unit)
  val tagTpeV = Type.Tag(constPropName, identV, Type.Int)
  val tagTpeT = Type.Tag(constPropName, identT, Type.Unit)
  val enumTpe = Type.Enum(Map("ConstProp.Bot" -> tagTpeB, "ConstProp.Val" -> tagTpeV, "ConstProp.Top" -> tagTpeT))

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

      try {
        method.invoke(null, args: _*)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the real exception
          throw e.getTargetException
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Loading (unpacking bits)                                                //
  /////////////////////////////////////////////////////////////////////////////

  /*
   * Test methods always return an Int32 (or an Int64). Otherwise, the return
   * value is implicitly cast to an Int8 or an Int16 (short). This can hide
   * potential bugs -- we return an Int32 so we can inspect the higher-order bits.
   */

  test("Codegen - LoadBool - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadBool(Int64(0), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadBool - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadBool(Int64(0xFFFFFFFFFFFFFFFFL), offset),
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
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadBool(Int64(0x9999999999999999L), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(if (i % 4 == 0 || i % 4 == 3) 1 else 0)(result)
    }
  }

  test("Codegen - LoadInt8 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt8(Int64(0), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt8 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt8(Int64(0xFFFFFFFFFFFFFFFFL), offset),
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
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt8(Int64(0xABABABABABABABABL), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0xAB)(result)
    }
  }

  test("Codegen - LoadInt16 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt16(Int64(0), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt16 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt16(Int64(0xFFFFFFFFFFFFFFFFL), offset),
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
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt16(Int64(0xCAFECAFECAFECAFEL), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0xCAFE)(result)
    }
  }

  test("Codegen - LoadInt32 - 01") {
    // Binary number is: 0000000000000000000000000000000000000000000000000000000000000000
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt32(Int64(0), offset),
      Type.Lambda(List(), Type.Int32), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0)(result)
    }
  }

  test("Codegen - LoadInt32 - 02") {
    // Binary number is: 1111111111111111111111111111111111111111111111111111111111111111
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt32(Int64(0xFFFFFFFFFFFFFFFFL), offset),
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
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = LoadInt32(Int64(0xDEADBEEFDEADBEEFL), offset),
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
    val definition = Function(name, args = List(),
      StoreBool(Int64(0), 42, True),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000040000000000L)(result)
  }

  test("Codegen - StoreBool - 02") {
    val definition = Function(name, args = List(),
      body = StoreBool(StoreBool(StoreBool(StoreBool(StoreBool(StoreBool(
        Int64(1230000), 2, True), 5, True), 10, True), 12, True), 49, True), 61, True),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x200200000012D4B4L)(result)
  }

  test("Codegen - StoreBool - 03") {
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = StoreBool(Int64(0), offset, True),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 64) {
      val code = new CompiledCode(List(definition(i)))
      val result = code.call(name, List())
      assertResult(1L << i)(result)
    }
  }

  test("Codegen - StoreInt8 - 01") {
    val v = Int8(0xAB.toByte)
    val definition = Function(name, args = List(),
      StoreInt8(Int64(0), 40, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000AB0000000000L)(result)
  }

  test("Codegen - StoreInt8 - 02") {
    val v = Int8(0xAB.toByte)
    val definition = Function(name, args = List(),
      body = StoreInt8(StoreInt8(StoreInt8(StoreInt8(
        Int64(123), 16, v), 32, v), 40, v), 56, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xAB00ABAB00AB007BL)(result)
  }

  test("Codegen - StoreInt8 - 03") {
    val v = Int8(0xFF.toByte)
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = StoreInt8(Int64(0), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 8) {
      val code = new CompiledCode(List(definition(i * 8)))
      val result = code.call(name, List())
      assertResult(0xFFL << (i * 8))(result)
    }
  }

  test("Codegen - StoreInt8 - 04") {
    val v = Unary(UnaryOperator.BitwiseNegate, Int8(0xAB.toByte), Type.Int8, loc)
    val definition = Function(name, args = List(),
      StoreInt8(Int64(0), 40, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000540000000000L)(result)
  }

  test("Codegen - StoreInt16 - 01") {
    val v = Int16(0xCAFE.toShort)
    val definition = Function(name, args = List(),
      StoreInt16(Int64(0), 48, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xCAFE000000000000L)(result)
  }

  test("Codegen - StoreInt16 - 02") {
    val v = Int16(0xCAFE.toShort)
    val definition = Function(name, args = List(),
      body = StoreInt16(StoreInt16(
        Int64(0x00000000007B0000L), 0, v), 32, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000CAFE007BCAFEL)(result)
  }

  test("Codegen - StoreInt16 - 03") {
    val v = Int16(0xFFFF.toShort)
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = StoreInt16(Int64(0), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 4) {
      val code = new CompiledCode(List(definition(i * 16)))
      val result = code.call(name, List())
      assertResult(0xFFFFL << (i * 16))(result)
    }
  }

  test("Codegen - StoreInt16 - 04") {
    val v = Unary(UnaryOperator.BitwiseNegate, Int16(0xCAFE.toShort), Type.Int16, loc)
    val definition = Function(name, args = List(),
      StoreInt16(Int64(0), 48, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x3501000000000000L)(result)
  }

  test("Codegen - StoreInt32 - 01") {
    val v = Int32(0xDEADBEEF)
    val definition = Function(name, args = List(),
      StoreInt32(Int64(0), 0, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x00000000DEADBEEFL)(result)
  }

  test("Codegen - StoreInt32 - 02") {
    val v = Int32(0xDEADBEEF)
    val definition = Function(name, args = List(),
      body = StoreInt32(
        Int64(123), 32, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0xDEADBEEF0000007BL)(result)
  }

  test("Codegen - StoreInt32 - 03") {
    val v = Int32(0xFFFFFFFF)
    def definition(offset: scala.Int) = Function(name, args = List(),
      body = StoreInt32(Int64(0), offset, v),
      Type.Lambda(List(), Type.Int64), loc)

    for (i <- 0 until 2) {
      val code = new CompiledCode(List(definition(i * 32)))
      val result = code.call(name, List())
      assertResult(0xFFFFFFFFL << (i * 32))(result)
    }
  }

  test("Codegen - StoreInt32 - 04") {
    val v = Unary(UnaryOperator.BitwiseNegate, Int32(0xDEADBEEF), Type.Int32, loc)
    val definition = Function(name, args = List(),
      StoreInt32(Int64(0), 0, v),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())
    assertResult(0x0000000021524110L)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unit                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Unit") {
    val definition = Function(name, args = List(),
      body = Unit,
      Type.Lambda(List(), Type.Unit), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(Value.Unit)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int constants                                                           //
  /////////////////////////////////////////////////////////////////////////////

  /*
   * Here we're testing methods that return Int8/Int16, so the implicit casting is OK.
   */

  test("Codegen - Const01") {
    val definition = Function(name, args = List(),
      body = Int32(42),
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
      body = Int8(-1),
      Type.Lambda(List(), Type.Int8), loc)
    val def_0 = Function(name_0, args = List(),
      body = Int8(0),
      Type.Lambda(List(), Type.Int8), loc)
    val def_1 = Function(name_1, args = List(),
      body = Int8(1),
      Type.Lambda(List(), Type.Int8), loc)
    val def_2 = Function(name_2, args = List(),
      body = Int8(2),
      Type.Lambda(List(), Type.Int8), loc)
    val def_3 = Function(name_3, args = List(),
      body = Int8(3),
      Type.Lambda(List(), Type.Int8), loc)
    val def_4 = Function(name_4, args = List(),
      body = Int8(4),
      Type.Lambda(List(), Type.Int8), loc)
    val def_5 = Function(name_5, args = List(),
      body = Int8(5),
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
      body = Int8(scala.Byte.MinValue),
      Type.Lambda(List(), Type.Int8), loc)
    val def02 = Function(name02, args = List(),
      body = Int8((scala.Byte.MinValue + 42).toByte),
      Type.Lambda(List(), Type.Int8), loc)
    val def03 = Function(name03, args = List(),
      body = Int8(-2),
      Type.Lambda(List(), Type.Int8), loc)
    val def04 = Function(name04, args = List(),
      body = Int8(6),
      Type.Lambda(List(), Type.Int8), loc)
    val def05 = Function(name05, args = List(),
      body = Int8((scala.Byte.MaxValue - 42).toByte),
      Type.Lambda(List(), Type.Int8), loc)
    val def06 = Function(name06, args = List(),
      body = Int8(scala.Byte.MaxValue),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(scala.Byte.MinValue)(result01)
    assertResult(scala.Byte.MinValue + 42)(result02)
    assertResult(-2)(result03)
    assertResult(6)(result04)
    assertResult(scala.Byte.MaxValue - 42)(result05)
    assertResult(scala.Byte.MaxValue)(result06)
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
      body = Int16(scala.Short.MinValue),
      Type.Lambda(List(), Type.Int16), loc)
    val def02 = Function(name02, args = List(),
      body = Int16((scala.Short.MinValue + 42).toShort), 
      Type.Lambda(List(), Type.Int16), loc)
    val def03 = Function(name03, args = List(),
      body = Int16((scala.Byte.MinValue - 1).toShort),
      Type.Lambda(List(), Type.Int16), loc)
    val def04 = Function(name04, args = List(),
      body = Int16((scala.Byte.MaxValue + 1).toShort), 
      Type.Lambda(List(), Type.Int16), loc)
    val def05 = Function(name05, args = List(),
      body = Int16((scala.Short.MaxValue - 42).toShort),
      Type.Lambda(List(), Type.Int16), loc)
    val def06 = Function(name06, args = List(),
      body = Int16(scala.Short.MaxValue),
      Type.Lambda(List(), Type.Int16), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(scala.Short.MinValue)(result01)
    assertResult(scala.Short.MinValue + 42)(result02)
    assertResult(scala.Byte.MinValue - 1)(result03)
    assertResult(scala.Byte.MaxValue + 1)(result04)
    assertResult(scala.Short.MaxValue - 42)(result05)
    assertResult(scala.Short.MaxValue)(result06)
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
      body = Int32(scala.Int.MinValue),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Int32(scala.Int.MinValue + 42), 
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Int32(scala.Short.MinValue - 1),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Int32(scala.Short.MaxValue + 1),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Int32(scala.Int.MaxValue - 42), 
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Int32(scala.Int.MaxValue),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)

    assertResult(scala.Int.MinValue)(result01)
    assertResult(scala.Int.MinValue + 42)(result02)
    assertResult(scala.Short.MinValue - 1)(result03)
    assertResult(scala.Short.MaxValue + 1)(result04)
    assertResult(scala.Int.MaxValue - 42)(result05)
    assertResult(scala.Int.MaxValue)(result06)
  }

  test("Codegen - Const06") {
    val definition = Function(name, args = List(),
      body = True,
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(true)(result)
  }

  test("Codegen - Const07") {
    val definition = Function(name, args = List(),
      body = Int8(1),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const08") {
    val definition = Function(name, args = List(),
      body = Int16(1),
      Type.Lambda(List(), Type.Int16), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const09") {
    val definition = Function(name, args = List(),
      body = Int32(1),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const10") {
    val definition = Function(name, args = List(),
      body = Int64(0),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(0)(result)
  }

  test("Codegen - Const11") {
    val definition = Function(name, args = List(),
      body = Int64(1),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(1)(result)
  }

  test("Codegen - Const12") {
    val definition = Function(name, args = List(),
      body = Int64(123456789123456789L),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(123456789123456789L)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Strings                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Str01") {
    val definition = Function(name, args = List(),
      body = Str("foobar", loc),
      Type.Lambda(List(), Type.Str), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult("foobar")(result)
  }

  test("Codegen - Str02") {
    val definition = Function(name, args = List(),
      body = Str("", loc),
      Type.Lambda(List(), Type.Str), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult("")(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Variables                                                               //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Var01") {
    val definition = Function(name, args = List("x"),
      body = Int32(-1),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(java.lang.Integer.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(-1)(result)
  }

  test("Codegen - Var02") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(java.lang.Integer.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(42)(result)
  }

  test("Codegen - Var03") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(toIdent("y"), 1, Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Integer.TYPE, java.lang.Integer.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Var04") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Int64, loc),
      Type.Lambda(List(Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(java.lang.Long.TYPE), List(42).map(_.asInstanceOf[Object]))

    assertResult(42)(result)
  }

  test("Codegen - Var05") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(toIdent("y"), 2, Type.Int64, loc),
      Type.Lambda(List(Type.Int64, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Long.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Var06") {
    val definition = Function(name, args = List("x", "y", "z"),
      body = Var(toIdent("y"), 1, Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(1337, -101010, 42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Var07") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, enumTpe, loc),
      Type.Lambda(List(enumTpe), enumTpe), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(classOf[Value.Tag]),
      List(Value.mkTag(constPropName, identV.name, Value.mkInt32(987))))

    assertResult(Value.mkTag(constPropName, identV.name, Value.mkInt32(987)))(result)
  }

  test("Codegen - Var08") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Unit, loc),
      Type.Lambda(List(Type.Unit), Type.Unit), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(Value.Unit.getClass), List(Value.Unit))

    assertResult(Value.Unit)(result)
  }

  test("Codegen - Var09") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Str, loc),
      Type.Lambda(List(Type.Str), Type.Str), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(classOf[java.lang.String]), List("helloworld"))

    assertResult("helloworld")(result)
  }

  test("Codegen - Var10") {
    val definition = Function(name, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Tuple(List(Type.Int32, Type.Int32)), loc),
      Type.Lambda(List(Type.Tuple(List(Type.Int32, Type.Int32))), Type.Tuple(List(Type.Int32, Type.Int32))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(classOf[Value.Tuple]), List(Value.Tuple(Array(321, 5).map(Value.mkInt32))))

    assertResult(Value.Tuple(Array(321, 5).map(Value.mkInt32)))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Let expression                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Let01") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int32(42),
        Int32(-1), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-1)(result)
  }

  test("Codegen - Let02") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int32(42),
        Var(toIdent("x"), 0, Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Let03") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int32(1337),
        Let(toIdent("y"), 1, Int32(-101010),
          Let(toIdent("z"), 2, Int32(42),
            Var(toIdent("y"), 1, Type.Int32, loc),
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
      body = Let(toIdent("x"), 3, Int32(1337),
        Let(toIdent("y"), 4, Int32(-101010),
          Let(toIdent("z"), 5, Int32(42),
            Var(toIdent("y"), 4, Type.Int32, loc),
            Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Integer.TYPE, java.lang.Integer.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }

  test("Codegen - Let05") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(toIdent("x"), 3, Int32(1337),
        Let(toIdent("y"), 4, Int32(-101010),
          Let(toIdent("z"), 5, Int32(42),
            Var(toIdent("b"), 1, Type.Int32, loc),
            Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Integer.TYPE, java.lang.Integer.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(101010)(result)
  }

  test("Codegen - Let06") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int64(42),
        Var(toIdent("x"), 0, Type.Int64, loc), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Let07") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int64(1337),
        Let(toIdent("y"), 2, Int64(-101010),
          Let(toIdent("z"), 4, Int64(42),
            Var(toIdent("y"), 2, Type.Int64, loc),
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
      body = Let(toIdent("x"), 0, Int32(1337),
        Let(toIdent("y"), 1, Int64(-101010),
          Let(toIdent("z"), 3, Int64(42),
            Var(toIdent("y"), 1, Type.Int64, loc),
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
      body = Let(toIdent("x"), 6, Int64(1337),
        Let(toIdent("y"), 8, Int64(-101010),
          Let(toIdent("z"), 10, Int64(42),
            Var(toIdent("y"), 8, Type.Int64, loc),
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
      body = Let(toIdent("x"), 5, Int32(1337),
        Let(toIdent("y"), 6, Int64(-101010),
          Let(toIdent("z"), 8, Int64(42),
            Var(toIdent("y"), 6, Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(-101010)(result)
  }


  test("Codegen - Let11") {
    val definition = Function(name, args = List("a", "b", "c"),
      body = Let(toIdent("x"), 6, Int64(1337),
        Let(toIdent("y"), 8, Int64(-101010),
          Let(toIdent("z"), 10, Int64(42),
            Var(toIdent("b"), 2, Type.Int64, loc),
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
      body = Let(toIdent("x"), 5, Int32(1337),
        Let(toIdent("y"), 6, Int64(-101010),
          Let(toIdent("z"), 8, Int64(42),
            Var(toIdent("b"), 1, Type.Int64, loc),
            Type.Int64, loc),
          Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(Type.Int32, Type.Int64, Type.Int64), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name,
      List(java.lang.Integer.TYPE, java.lang.Long.TYPE, java.lang.Long.TYPE),
      List(-1337, 101010, -42).map(_.asInstanceOf[Object]))

    assertResult(101010)(result)
  }

  test("Codegen - Let13") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(constPropName, identV, Int32(42), enumTpe, loc),
        exp2 = Var(toIdent("x"), 0, enumTpe, loc),
        enumTpe, loc),
      Type.Lambda(List(), enumTpe), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(constPropName, identV.name, Value.mkInt32(42)))(result)
  }

  test("Codegen - Let14") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Unit,
        exp2 = Var(toIdent("x"), 0, Type.Unit, loc),
        Type.Unit, loc),
      Type.Lambda(List(), Type.Unit), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult(Value.Unit)(result)
  }

  test("Codegen - Let15") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Str("helloworld", loc),
        exp2 = Var(toIdent("x"), 0, Type.Str, loc),
        Type.Str, loc),
      Type.Lambda(List(), Type.Str), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List())

    assertResult("helloworld")(result)
  }

  test("Codegen - Let16") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Int32(321), Int32(5)), Type.Tuple(List(Type.Int32, Type.Int32)), loc),
        exp2 = Var(toIdent("x"), 0, Type.Tuple(List(Type.Int32, Type.Int32)), loc),
        Type.Tuple(List(Type.Int32, Type.Int32)), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Int32, Type.Int32))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(321, 5).map(Value.mkInt32)))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Function application                                                    //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Apply01") {
    // def main(): scala.Int = f()
    // def f(): scala.Int = 24
    val main = Function(name, args = List(),
      body = Apply(name01, List(), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List(),
      body = Int32(24),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(24)(result)
  }

  test("Codegen - Apply02") {
    // def main(): scala.Int = f(3)
    // def f(x: scala.Int): scala.Int = 24
    val main = Function(name, args = List(),
      body = Apply(name01, List(Int32(3)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Int32(24),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(24)(result)
  }

  test("Codegen - Apply03") {
    // def main(): scala.Int = f(3)
    // def f(x: scala.Int): scala.Int = x
    val main = Function(name, args = List(),
      body = Apply(name01, List(Int32(3)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Var(toIdent("x"), 0, Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(3)(result)
  }

  test("Codegen - Apply04") {
    // def main(): scala.Int = f(3, 42)
    // def f(x: scala.Int, y: scala.Int): scala.Int = x * y - 6
    val main = Function(name, args = List(),
      body = Apply(name01, List(Int32(3), Int32(42)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x", "y"),
      body = Binary(BinaryOperator.Minus,
        Binary(BinaryOperator.Times,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Var(toIdent("y"), 1, Type.Int32, loc),
          Type.Int32, loc),
        Int32(6),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f))
    val result = code.call(name)

    assertResult(120)(result)
  }

  test("Codegen - Apply05") {
    // def main(): scala.Int = f(5)
    // def f(x: scala.Int): scala.Int = let y = g(x + 1) in y * y
    // def g(x: scala.Int): scala.Int = x - 4
    val main = Function(name, args = List(),
      body = Apply(name01, List(Int32(5)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Let(toIdent("y"), 1, Apply(name02,
        List(Binary(BinaryOperator.Plus,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Int32(1), Type.Int32, loc)), Type.Int32, loc),
        Binary(BinaryOperator.Times,
          Var(toIdent("y"), 1, Type.Int32, loc),
          Var(toIdent("y"), 1, Type.Int32, loc),
          Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body =Binary(BinaryOperator.Minus,
        Var(toIdent("x"), 0, Type.Int32, loc),
        Int32(4),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f, g))
    val result = code.call(name)

    assertResult(4)(result)
  }

  test("Codegen - Apply06") {
    // def main(): scala.Int = f(3)
    // def f(x: scala.Int): scala.Int = g(x + 1)
    // def g(x: scala.Int): scala.Int = h(x + 10)
    // def h(x: scala.Int): scala.Int = x * x
    val main = Function(name, args = List(),
      body = Apply(name01, List(Int32(3)), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x"),
      body = Apply(name02, List(
        Binary(BinaryOperator.Plus,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Int32(1),
          Type.Int32, loc)),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body = Apply(name03, List(
        Binary(BinaryOperator.Plus,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Int32(10),
          Type.Int32, loc)),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val h = Function(name03, args = List("x"),
      body = Binary(BinaryOperator.Times,
        Var(toIdent("x"), 0, Type.Int32, loc),
        Var(toIdent("x"), 0, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(main, f, g, h))
    val result = code.call(name)

    assertResult(196)(result)
  }

  test("Codegen - Apply07") {
    // def main(): scala.Int = let x = 7 in f(g(3), h(h(x)))
    // def f(x: scala.Int, y: scala.Int): scala.Int = x - y
    // def g(x: scala.Int): scala.Int = x * 3
    // def h(x: scala.Int): scala.Int = g(x - 1)
    val main = Function(name, args = List(),
      body = Let(toIdent("x"), 0, Int32(7),
        Apply(name01, List(
          Apply(name02, List(Int32(3)), Type.Int32, loc),
          Apply(name03, List(
            Apply(name03, List(Var(toIdent("x"), 0, Type.Int32, loc)),
              Type.Int32, loc)), Type.Int32, loc)),
          Type.Int32, loc), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val f = Function(name01, args = List("x", "y"),
      body = Binary(BinaryOperator.Minus,
        Var(toIdent("x"), 0, Type.Int32, loc),
        Var(toIdent("y"), 1, Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)
    val g = Function(name02, args = List("x"),
      body = Binary(BinaryOperator.Times,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Int32(3),
          Type.Int32, loc),
      Type.Lambda(List(Type.Int32), Type.Int32), loc)
    val h = Function(name03, args = List("x"),
      body = Apply(name02, List(
        Binary(BinaryOperator.Minus,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Int32(1),
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
      body = Unary(UnaryOperator.LogicalNot, False, Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Unary.Not02") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.LogicalNot, True, Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Unary.Plus01") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Int32(42), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - Unary.Plus02") {
    val definition = Function(name, args = List(),
      body = Unary(UnaryOperator.Plus, Int32(-42), Type.Int32, loc),
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
      body = Unary(UnaryOperator.Minus, Int8(scala.Byte.MaxValue), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Int8(42), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Int8(0), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Int8(-42), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Int8(scala.Byte.MinValue), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-scala.Byte.MaxValue)(result01)
    assertResult(-42)(result02)
    assertResult(0)(result03)
    assertResult(42)(result04)
    assertResult(scala.Byte.MinValue)(result05)
  }


  test("Codegen - Unary.Minus02") {
    // Unary minus operator applied to Int16

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Int16(scala.Short.MaxValue), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Int16(420), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Int16(0), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Int16(-420), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Int16(scala.Short.MinValue), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-scala.Short.MaxValue)(result01)
    assertResult(-420)(result02)
    assertResult(0)(result03)
    assertResult(420)(result04)
    assertResult(scala.Short.MinValue)(result05)
  }

  test("Codegen - Unary.Minus03") {
    // Unary minus operator applied to Int32

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Int32(scala.Int.MaxValue), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Int32(36000), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Int32(0), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Int32(-36000), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Int32(scala.Int.MinValue), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-scala.Int.MaxValue)(result01)
    assertResult(-36000)(result02)
    assertResult(0)(result03)
    assertResult(36000)(result04)
    assertResult(scala.Int.MinValue)(result05)
  }

  test("Codegen - Unary.Minus04") {
    // Unary minus operator applied to Int64

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Unary(UnaryOperator.Minus, Int64(scala.Long.MaxValue), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.Minus, Int64(10000000000L), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.Minus, Int64(0), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.Minus, Int64(-10000000000L), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.Minus, Int64(scala.Long.MinValue), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-scala.Long.MaxValue)(result01)
    assertResult(-10000000000L)(result02)
    assertResult(0)(result03)
    assertResult(10000000000L)(result04)
    assertResult(scala.Long.MinValue)(result05)
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
      body = Unary(UnaryOperator.BitwiseNegate, Int8(scala.Byte.MaxValue), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(42), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(1), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(0), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(-1), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(-42), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int8(scala.Byte.MinValue), Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(scala.Byte.MinValue)(result01)
    assertResult(-43)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(41)(result06)
    assertResult(scala.Byte.MaxValue)(result07)
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
      body = Unary(UnaryOperator.BitwiseNegate, Int16(scala.Short.MaxValue), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(420), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(1), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(0), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(-1), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(-420), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int16(scala.Short.MinValue), Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(scala.Short.MinValue)(result01)
    assertResult(-421)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(419)(result06)
    assertResult(scala.Short.MaxValue)(result07)
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
      body = Unary(UnaryOperator.BitwiseNegate, Int32(scala.Int.MaxValue), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(36000), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(1), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(0), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(-1), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(-36000), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int32(scala.Int.MinValue), Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(scala.Int.MinValue)(result01)
    assertResult(-36001)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(35999)(result06)
    assertResult(scala.Int.MaxValue)(result07)
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
      body = Unary(UnaryOperator.BitwiseNegate, Int64(scala.Long.MaxValue), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(10000000000L), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(1), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(0), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(-1), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def06 = Function(name06, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(-10000000000L), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def07 = Function(name07, args = List(),
      body = Unary(UnaryOperator.BitwiseNegate, Int64(scala.Long.MinValue), Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05, def06, def07))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)
    val result06 = code.call(name06)
    val result07 = code.call(name07)

    assertResult(scala.Long.MinValue)(result01)
    assertResult(-10000000001L)(result02)
    assertResult(-2)(result03)
    assertResult(-1)(result04)
    assertResult(0)(result05)
    assertResult(9999999999L)(result06)
    assertResult(scala.Long.MaxValue)(result07)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators -- arithmetic                                          //
  //                                                                         //
  // Test methods for arithmetic and bitwise operations always return an     //
  // Int32 (or an Int64). Otherwise, the return value is implicitly cast to  //
  // an Int8 or an Int16 (short). This can hide potential bugs -- we return  //
  // an Int32 so we can inspect the higher-order bits.                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Binary.Plus01") {
    // Binary plus operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int8(scala.Byte.MaxValue),
        Int8(1),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int8(10),
        Int8(40),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int8(-40),
        Int8(10),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int8(-10),
        Int8(40),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int8(scala.Byte.MinValue),
        Int8(-1),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Byte.MinValue)(result01)
    assertResult(50)(result02)
    assertResult(-30)(result03)
    assertResult(30)(result04)
    assertResult(scala.Byte.MaxValue)(result05)
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
        Int16(scala.Short.MaxValue),
        Int16(1),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int16(1000),
        Int16(4000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int16(-4000),
        Int16(1000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int16(-1000),
        Int16(4000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int16(scala.Short.MinValue),
        Int16(-1),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Short.MinValue)(result01)
    assertResult(5000)(result02)
    assertResult(-3000)(result03)
    assertResult(3000)(result04)
    assertResult(scala.Short.MaxValue)(result05)
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
        Int32(scala.Int.MaxValue),
        Int32(1),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int32(100000),
        Int32(400000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int32(-400000),
        Int32(100000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int32(-100000),
        Int32(400000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int32(scala.Int.MinValue),
        Int32(-1),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Int.MinValue)(result01)
    assertResult(500000)(result02)
    assertResult(-300000)(result03)
    assertResult(300000)(result04)
    assertResult(scala.Int.MaxValue)(result05)
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
        Int64(scala.Long.MaxValue),
        Int64(1),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int64(10000000000L),
        Int64(40000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int64(-40000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int64(-10000000000L),
        Int64(40000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Plus,
        Int64(scala.Long.MinValue),
        Int64(-1),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Long.MinValue)(result01)
    assertResult(50000000000L)(result02)
    assertResult(-30000000000L)(result03)
    assertResult(30000000000L)(result04)
    assertResult(scala.Long.MaxValue)(result05)
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
        Int8(scala.Byte.MinValue),
        Int8(1),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int8(40),
        Int8(10),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int8(-40),
        Int8(10),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int8(-10),
        Int8(40),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int8(scala.Byte.MaxValue),
        Int8(-1),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Byte.MaxValue)(result01)
    assertResult(30)(result02)
    assertResult(-50)(result03)
    assertResult(-50)(result04)
    assertResult(scala.Byte.MinValue)(result05)
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
        Int16(scala.Short.MinValue),
        Int16(1),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int16(4000),
        Int16(1000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int16(-4000),
        Int16(1000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int16(-1000),
        Int16(4000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int16(scala.Short.MaxValue),
        Int16(-1),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Short.MaxValue)(result01)
    assertResult(3000)(result02)
    assertResult(-5000)(result03)
    assertResult(-5000)(result04)
    assertResult(scala.Short.MinValue)(result05)
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
        Int32(scala.Int.MinValue),
        Int32(1),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int32(400000),
        Int32(100000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int32(-400000),
        Int32(100000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int32(-100000),
        Int32(400000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int32(scala.Int.MaxValue),
        Int32(-1),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Int.MaxValue)(result01)
    assertResult(300000)(result02)
    assertResult(-500000)(result03)
    assertResult(-500000)(result04)
    assertResult(scala.Int.MinValue)(result05)
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
        Int64(scala.Long.MinValue),
        Int64(1),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int64(40000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int64(-40000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int64(-10000000000L),
        Int64(40000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Minus,
        Int64(scala.Long.MaxValue),
        Int64(-1),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(scala.Long.MaxValue)(result01)
    assertResult(30000000000L)(result02)
    assertResult(-50000000000L)(result03)
    assertResult(-50000000000L)(result04)
    assertResult(scala.Long.MinValue)(result05)
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
        Int8(scala.Byte.MaxValue),
        Int8(2),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Int8(3),
        Int8(2),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Int8(-2),
        Int8(3),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Int8(-2),
        Int8(-3),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Int8(scala.Byte.MinValue),
        Int8(-1),
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
    assertResult(scala.Byte.MinValue)(result05)
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
        Int16(scala.Short.MaxValue),
        Int16(2),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Int16(30),
        Int16(20),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Int16(-20),
        Int16(30),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Int16(-20),
        Int16(-30),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Int16(scala.Short.MinValue),
        Int16(-1),
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
    assertResult(scala.Short.MinValue)(result05)
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
        Int32(scala.Int.MaxValue),
        Int32(2),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Int32(300),
        Int32(200),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Int32(-200),
        Int32(300),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Int32(-200),
        Int32(-300),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Int32(scala.Int.MinValue),
        Int32(-1),
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
    assertResult(scala.Int.MinValue)(result05)
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
        Int64(scala.Long.MaxValue),
        Int64(2),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Times,
        Int64(300000),
        Int64(200000),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Times,
        Int64(-200000),
        Int64(300000),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Times,
        Int64(-200000),
        Int64(-300000),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Times,
        Int64(scala.Long.MinValue),
        Int64(-1),
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
    assertResult(scala.Long.MinValue)(result05)
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
        Int8(12),
        Int8(3),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int8(3),
        Int8(12),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int8(-12),
        Int8(3),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int8(-3),
        Int8(12),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int8(-12),
        Int8(-3),
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
        Int16(12000),
        Int16(300),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int16(3000),
        Int16(12000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int16(-12000),
        Int16(300),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int16(-3000),
        Int16(12000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int16(-12000),
        Int16(-300),
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
        Int32(1200000),
        Int32(3000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int32(300000),
        Int32(1200000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int32(-1200000),
        Int32(3000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int32(-300000),
        Int32(1200000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int32(-1200000),
        Int32(-3000),
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
        Int64(120000000000L),
        Int64(3),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int64(30000000000L),
        Int64(120000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int64(-120000000000L),
        Int64(3),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int64(-30000000000L),
        Int64(120000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Divide,
        Int64(-120000000000L),
        Int64(-3),
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
        Int8(12),
        Int8(2),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int8(12),
        Int8(5),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int8(-12),
        Int8(5),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int8(12),
        Int8(-5),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int8(-12),
        Int8(-5),
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
        Int16(12000),
        Int16(2000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int16(12000),
        Int16(5000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int16(-12000),
        Int16(5000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int16(12000),
        Int16(-5000),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int16(-12000),
        Int16(-5000),
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
        Int32(1200000),
        Int32(200000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int32(1200000),
        Int32(500000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int32(-1200000),
        Int32(500000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int32(1200000),
        Int32(-500000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int32(-1200000),
        Int32(-500000),
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
        Int64(120000000000L),
        Int64(20000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int64(120000000000L),
        Int64(50000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int64(-120000000000L),
        Int64(50000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int64(120000000000L),
        Int64(-50000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Modulo,
        Int64(-120000000000L),
        Int64(-50000000000L),
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

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators -- comparison                                          //
  /////////////////////////////////////////////////////////////////////////////

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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Less,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Less,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Less,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Less,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Less,
        Int64(-3000000000L),
        Int64(-3000000000L),
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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.LessEqual,
        Int64(-3000000000L),
        Int64(-3000000000L),
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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Greater,
        Int64(-3000000000L),
        Int64(-3000000000L),
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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.GreaterEqual,
        Int64(-3000000000L),
        Int64(-3000000000L),
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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.Equal,
        Int64(-3000000000L),
        Int64(-3000000000L),
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
        Int8(12),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int8(3),
        Int8(12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int8(3),
        Int8(3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int8(-12),
        Int8(-3),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int8(-3),
        Int8(-12),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int8(-3),
        Int8(-3),
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
        Int16(1200),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int16(300),
        Int16(1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int16(300),
        Int16(300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int16(-1200),
        Int16(-300),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int16(-300),
        Int16(-1200),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int16(-300),
        Int16(-300),
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
        Int32(120000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int32(30000),
        Int32(120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int32(30000),
        Int32(30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int32(-120000),
        Int32(-30000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int32(-30000),
        Int32(-120000),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int32(-30000),
        Int32(-30000),
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
        Int64(12000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int64(3000000000L),
        Int64(12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int64(3000000000L),
        Int64(3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int64(-12000000000L),
        Int64(-3000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int64(-3000000000L),
        Int64(-12000000000L),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)
    val def06 = Function(name06, args = List(),
      body = Binary(BinaryOperator.NotEqual,
        Int64(-3000000000L),
        Int64(-3000000000L),
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

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators -- logical                                             //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Binary.And01") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        True,
        True,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.And02") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        True,
        False,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And03") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        False,
        False,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And04") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        False,
        True,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And05") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        False,
        Error(Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.And06") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalAnd,
        True,
        Error(Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    intercept[RuntimeException] { code.call(name) }
  }

  test("Codegen - Binary.Or01") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        True,
        True,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or02") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        True,
        False,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or03") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        False,
        False,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - Binary.Or04") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        False,
        True,
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or05") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        True,
        Error(Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - Binary.Or06") {
    val definition = Function(name, args = List(),
      body = Binary(BinaryOperator.LogicalOr,
        False,
        Error(Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    intercept[RuntimeException] { code.call(name) }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary operators -- bitwise                                             //
  //                                                                         //
  // Test methods for arithmetic and bitwise operations always return an     //
  // Int32 (or an Int64). Otherwise, the return value is implicitly cast to  //
  // an Int8 or an Int16 (short). This can hide potential bugs -- we return  //
  // an Int32 so we can inspect the higher-order bits.                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Binary.BitwiseAnd01") {
    // Binary bitwise and operator applied to Int8

    val name01 = Name.Resolved.mk(List("foo", "bar", "f01"))
    val name02 = Name.Resolved.mk(List("foo", "bar", "f02"))
    val name03 = Name.Resolved.mk(List("foo", "bar", "f03"))
    val name04 = Name.Resolved.mk(List("foo", "bar", "f04"))
    val name05 = Name.Resolved.mk(List("foo", "bar", "f05"))

    val def01 = Function(name01, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int8(42),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int8(42),
        Int8(42),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int8(42),
        Int8(0),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int8(0xFF.toByte),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int8((-1).toByte),
        Int8((-1).toByte),
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
    assertResult(0xFF.toByte)(result04)
    assertResult((-1).toByte)(result05)
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
        Int16(420),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int16(420),
        Int16(420),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int16(420),
        Int16(0),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int16(0xFFFF.toShort),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int16((-1).toShort),
        Int16((-1).toShort),
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
    assertResult(0xFFFF.toShort)(result04)
    assertResult((-1).toShort)(result05)
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
        Int32(40000),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int32(40000),
        Int32(40000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int32(40000),
        Int32(0),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int32(0xFFFFFFFF),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int32(-1),
        Int32(-1),
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
        Int64(10000000000L),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int64(10000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int64(10000000000L),
        Int64(0),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int64(0xFFFFFFFFFFFFFFFFL),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseAnd,
        Int64(-1L),
        Int64(-1L),
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
        Int8(42),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int8(42),
        Int8(42),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int8(42),
        Int8(0),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int8(0xFF.toByte),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int8((-1).toByte),
        Int8((-1).toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFF.toByte)(result01)
    assertResult(42)(result02)
    assertResult(42)(result03)
    assertResult(0xFF.toByte)(result04)
    assertResult((-1).toByte)(result05)
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
        Int16(420),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int16(420),
        Int16(420),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int16(420),
        Int16(0),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int16(0xFFFF.toShort),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int16((-1).toShort),
        Int16((-1).toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(0xFFFF.toShort)(result01)
    assertResult(420)(result02)
    assertResult(420)(result03)
    assertResult(0xFFFF.toShort)(result04)
    assertResult((-1).toShort)(result05)
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
        Int32(40000),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int32(40000),
        Int32(40000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int32(40000),
        Int32(0),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int32(0xFFFFFFFF),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int32(-1),
        Int32(-1),
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
        Int64(10000000000L),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int64(10000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int64(10000000000L),
        Int64(0),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int64(0xFFFFFFFFFFFFFFFFL),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseOr,
        Int64(-1L),
        Int64(-1L),
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
        Int8(42),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int8(42),
        Int8(42),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int8(42),
        Int8(0),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int8(0xFF.toByte),
        Int8(0xFF.toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int8((-1).toByte),
        Int8((-1).toByte),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-43)(result01)
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
        Int16(420),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int16(420),
        Int16(420),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int16(420),
        Int16(0),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int16(0xFFFF.toShort),
        Int16(0xFFFF.toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int16((-1).toShort),
        Int16((-1).toShort),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04, def05))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)
    val result05 = code.call(name05)

    assertResult(-421)(result01)
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
        Int32(40000),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int32(40000),
        Int32(40000),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int32(40000),
        Int32(0),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int32(0xFFFFFFFF),
        Int32(0xFFFFFFFF),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int32(-1),
        Int32(-1),
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
        Int64(10000000000L),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int64(10000000000L),
        Int64(10000000000L),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int64(10000000000L),
        Int64(0),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int64(0xFFFFFFFFFFFFFFFFL),
        Int64(0xFFFFFFFFFFFFFFFFL),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def05 = Function(name05, args = List(),
      body = Binary(BinaryOperator.BitwiseXor,
        Int64(-1L),
        Int64(-1L),
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
        Int8(0x08),
        Int32(0),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int8(0x08),
        Int32(2),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int8(0x08),
        Int32(4),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int8(0x08),
        Int32(5),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x20)(result02)
    assertResult(scala.Byte.MinValue)(result03)
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
        Int16(0x08),
        Int32(0),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int16(0x08),
        Int32(8),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int16(0x08),
        Int32(12),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int16(0x08),
        Int32(13),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x0800)(result02)
    assertResult(scala.Short.MinValue)(result03)
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
        Int32(0x08),
        Int32(0),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int32(0x08),
        Int32(16),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int32(0x08),
        Int32(28),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int32(0x08),
        Int32(29),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x00080000)(result02)
    assertResult(scala.Int.MinValue)(result03)
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
        Int64(0x08),
        Int32(0),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int64(0x08),
        Int32(32),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int64(0x08),
        Int32(60),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseLeftShift,
        Int64(0x08),
        Int32(61),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(def01, def02, def03, def04))

    val result01 = code.call(name01)
    val result02 = code.call(name02)
    val result03 = code.call(name03)
    val result04 = code.call(name04)

    assertResult(0x08)(result01)
    assertResult(0x0000000800000000L)(result02)
    assertResult(scala.Long.MinValue)(result03)
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
        Int8(123),
        Int32(0),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int8(123),
        Int32(2),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int8(123),
        Int32(7),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int8(-123),
        Int32(2),
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
        Int16(12000),
        Int32(0),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int16(12000),
        Int32(2),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int16(12000),
        Int32(15),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int16(-12000),
        Int32(2),
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
        Int32(120000),
        Int32(0),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int32(120000),
        Int32(2),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int32(120000),
        Int32(31),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int32(-120000),
        Int32(2),
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
        Int64(12000000000L),
        Int32(0),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def02 = Function(name02, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int64(12000000000L),
        Int32(2),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def03 = Function(name03, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int64(12000000000L),
        Int32(63),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)
    val def04 = Function(name04, args = List(),
      body = Binary(BinaryOperator.BitwiseRightShift,
        Int64(-12000000000L),
        Int32(2),
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

  /////////////////////////////////////////////////////////////////////////////
  // IfThenElse                                                              //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - IfThenElse01") {
    val definition = Function(name, args = List(),
      body = IfThenElse(False,
        Binary(BinaryOperator.Plus, Int32(42), Int32(10), Type.Int32, loc),
        Binary(BinaryOperator.Minus, Int32(42), Int32(10), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(32)(result)
  }

  test("Codegen - IfThenElse02") {
    val definition = Function(name, args = List(),
      body = IfThenElse(True,
        Binary(BinaryOperator.Plus, Int32(42), Int32(10), Type.Int32, loc),
        Binary(BinaryOperator.Minus, Int32(42), Int32(10), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(52)(result)
  }

  test("Codegen - IfThenElse03") {
    val definition = Function(name, args = List("x"),
      body = IfThenElse(Var(toIdent("x"), 0, Type.Bool, loc),
        IfThenElse(False, Int32(1), Int32(2), Type.Int32, loc),
        IfThenElse(True, Int32(3), Int32(4), Type.Int32, loc),
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
          Unary(UnaryOperator.LogicalNot, Var(toIdent("x"), 0, Type.Bool, loc), Type.Bool, loc),
          True,
          False,
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
        Binary(BinaryOperator.LogicalAnd,
          Var(toIdent("x"), 0, Type.Bool, loc),
          Var(toIdent("y"), 1, Type.Bool, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
        Binary(BinaryOperator.LogicalOr,
          Var(toIdent("x"), 0, Type.Bool, loc),
          Var(toIdent("y"), 1, Type.Bool, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
          Var(toIdent("x"), 0, Type.Int8, loc),
          Var(toIdent("y"), 1, Type.Int8, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
          Var(toIdent("x"), 0, Type.Int16, loc),
          Var(toIdent("y"), 1, Type.Int16, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
          Var(toIdent("x"), 0, Type.Int32, loc),
          Var(toIdent("y"), 1, Type.Int32, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(2400000, 500000).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(500000, 500000).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse10") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.GreaterEqual,
          Var(toIdent("x"), 0, Type.Int64, loc),
          Var(toIdent("y"), 2, Type.Int64, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
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
          Var(toIdent("x"), 0, Type.Int32, loc),
          Var(toIdent("y"), 1, Type.Int32, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(5, 5).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(2, 5).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  test("Codegen - IfThenElse12") {
    val definition = Function(name, args = List("x", "y"),
      body = IfThenElse(
        Binary(BinaryOperator.NotEqual,
          Var(toIdent("x"), 0, Type.Int32, loc),
          Var(toIdent("y"), 1, Type.Int32, loc),
          Type.Bool, loc),
        Int32(1234),
        Int32(5678),
        Type.Int32, loc),
      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result01 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(2, 5).map(_.asInstanceOf[Object]))
    val result02 = code.call(name, List(java.lang.Integer.TYPE, java.lang.Integer.TYPE), List(5, 5).map(_.asInstanceOf[Object]))

    assertResult(1234)(result01)
    assertResult(5678)(result02)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tag                                                                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Tag01") {
    val definition = Function(name, args = List(),
      body = Tag(constPropName, identV, Int32(42), enumTpe, loc),
      Type.Lambda(List(), enumTpe), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(constPropName, identV.name, Value.mkInt32(42)))(result)
  }

  test("Codegen - Tag02") {
    val enum = Type.Enum(Map("ConstProp.Val" -> Type.Tag(constPropName, identV, Type.Bool)))
    val definition = Function(name, args = List(),
      body = Tag(constPropName, identV, True, enum, loc),
      Type.Lambda(List(), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(constPropName, identV.name, Value.True))(result)
  }

  test("Codegen - Tag03") {
    val definition = Function(name, args = List(),
      body = Tag(constPropName, identT, Unit, enumTpe, loc),
      Type.Lambda(List(), enumTpe), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(constPropName, identT.name, Value.Unit))(result)
  }

  test("Codegen - Tag04") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Bool)))
    val definition = Function(name, args = List(),
      body = Tag(tagName, ident, True, enum, loc),
      Type.Lambda(List(), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(tagName, ident.name, Value.True))(result)
  }

  test("Codegen - Tag05") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Bool)))
    val definition = Function(name, args = List(),
      body = Tag(tagName, ident, False, enum, loc),
      Type.Lambda(List(), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(tagName, ident.name, Value.False))(result)
  }

  test("Codegen - Tag06") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Bool)))
    val definition = Function(name, args = List("x"),
      body = Tag(tagName, ident, Var(toIdent("x"), 0, Type.Bool, loc), enum, loc),
      Type.Lambda(List(Type.Bool), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name, List(java.lang.Boolean.TYPE), List(false.asInstanceOf[Object]))

    assertResult(Value.mkTag(tagName, ident.name, Value.False))(result)
  }

  test("Codegen - Tag07") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Str)))
    val definition = Function(name, args = List(),
      body = Tag(tagName, ident, Str("hello", loc), enum, loc),
      Type.Lambda(List(), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(tagName, ident.name, Value.mkStr("hello")))(result)
  }

  test("Codegen - Tag08") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Tuple(List(Type.Int, Type.Str)))))
    val definition = Function(name, args = List(),
      body = Tag(tagName, ident, Tuple(List(Int32(1), Str("one", loc)),
        Type.Tuple(List(Type.Int, Type.Str)), loc), enum, loc),
      Type.Lambda(List(), enum), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(tagName, ident.name, Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one")))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // CheckTag                                                                //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - CheckTag01") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(constPropName, identV, Int32(42), enumTpe, loc),
        exp2 = CheckTag(identV, Var(toIdent("x"), 0, enumTpe, loc), loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(true)(result)
  }

  test("Codegen - CheckTag02") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(constPropName, identV, Int32(42), enumTpe, loc),
        exp2 = CheckTag(identB, Var(toIdent("x"), 0, enumTpe, loc), loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // GetTagValue                                                             //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - GetTagValue01") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(constPropName, identV, Int32(42), enumTpe, loc),
        exp2 = GetTagValue(Var(toIdent("x"), 0, enumTpe, loc), Type.Int32, loc),
        Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(42)(result)
  }

  test("Codegen - GetTagValue02") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(constPropName, identT, Unit, enumTpe, loc),
        exp2 = GetTagValue(Var(toIdent("x"), 0, enumTpe, loc), Type.Unit, loc),
        Type.Unit, loc),
      Type.Lambda(List(), Type.Unit), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Unit)(result)
  }

  test("Codegen - GetTagValue03") {
    val tagName = Name.Resolved.mk("abc")
    val ident = toIdent("def")
    val enum = Type.Enum(Map("abc.bar" -> Type.Tag(tagName, ident, Type.Tuple(List(Type.Int, Type.Str)))))

    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tag(tagName, ident, Tuple(List(Int32(1), Str("one", loc)),
          Type.Tuple(List(Type.Int, Type.Str)), loc), enum, loc),
        exp2 = GetTagValue(Var(toIdent("x"), 0, enum, loc), Type.Tuple(List(Type.Int32, Type.Str)), loc),
        Type.Unit, loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Int32, Type.Str))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.mkInt32(1), Value.mkStr("one"))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tuple                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Tuple01") {
    val definition = Function(name, args = List(),
      body = Tuple(List(Int16(321), Int32(5)), Type.Tuple(List(Type.Int16, Type.Int32)), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Int16, Type.Int32))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.mkInt16(321), Value.mkInt32(5))))(result)
  }

  test("Codegen - Tuple02") {
    val definition = Function(name, args = List(),
      body = Tuple(List(True, True, False), Type.Tuple(List(Type.Bool, Type.Bool, Type.Bool)), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Bool, Type.Bool, Type.Bool))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.True, Value.True, Value.False)))(result)
  }

  test("Codegen - Tuple03") {
    val definition = Function(name, args = List(),
      body = Tuple(List(Str("un", loc), Str("deux", loc), Str("trois", loc), Str("quatre", loc)),
        Type.Tuple(List(Type.Str, Type.Str, Type.Str, Type.Str)), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Str, Type.Str, Type.Str, Type.Str))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array("un", "deux", "trois", "quatre").map(Value.mkStr)))(result)
  }

  test("Codegen - Tuple04") {
    val definition = Function(name, args = List(),
      body = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
        Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.mkStr("un"), Value.False, Value.mkInt64(12345), Value.Unit, Value.mkInt8(-2))))(result)
  }

  test("Codegen - Tuple05") {
    val definition = Function(name, args = List(),
      body = Tuple(List(
        Tag(constPropName, identV, Int32(111), enumTpe, loc),
        Tag(constPropName, identB, Unit, enumTpe, loc)),
        Type.Tuple(List(enumTpe, enumTpe)), loc),
      Type.Lambda(List(), Type.Tuple(List(enumTpe, enumTpe))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.mkTag(constPropName, identV.name, Value.mkInt32(111)), Value.mkTag(constPropName, identB.name, Value.Unit))))(result)
  }

  test("Codegen - Tuple06") {
    val definition = Function(name, args = List(),
      body = Tuple(List(
        Tuple(List(Int32(123), Int32(456)), Type.Tuple(List(Type.Int32, Type.Int32)), loc),
        Tuple(List(Str("654", loc), Str("321", loc)), Type.Tuple(List(Type.Str, Type.Str)), loc)),
        Type.Tuple(List(Type.Tuple(List(Type.Int32, Type.Int32)), Type.Tuple(List(Type.Str, Type.Str)))), loc),
      Type.Lambda(List(), Type.Tuple(List(Type.Tuple(List(Type.Int32, Type.Int32)), Type.Tuple(List(Type.Str, Type.Str))))), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Tuple(Array(Value.Tuple(Array(123, 456).map(Value.mkInt32)), Value.Tuple(Array("654", "321").map(Value.mkStr)))))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // TupleAt                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - TupleAt01") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
          Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc), 0, Type.Str, loc),
        Type.Str, loc),
      Type.Lambda(List(), Type.Str), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult("un")(result)
  }

  test("Codegen - TupleAt02") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
          Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc), 1, Type.Bool, loc),
        Type.Bool, loc),
      Type.Lambda(List(), Type.Bool), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(false)(result)
  }

  test("Codegen - TupleAt03") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
          Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc), 2, Type.Int64, loc),
        Type.Int64, loc),
      Type.Lambda(List(), Type.Int64), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(12345)(result)
  }

  test("Codegen - TupleAt04") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
          Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc), 3, Type.Unit, loc),
        Type.Unit, loc),
      Type.Lambda(List(), Type.Unit), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.Unit)(result)
  }

  test("Codegen - TupleAt05") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Str("un", loc), False, Int64(12345), Unit, Int8(-2)),
          Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Str, Type.Bool, Type.Int64, Type.Unit, Type.Int8)), loc), 4, Type.Int8, loc),
        Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(-2)(result)
  }

  test("Codegen - TupleAt06") {
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = Tuple(List(Tag(constPropName, identV, Int32(111), enumTpe, loc), Tag(constPropName, identB, Unit, enumTpe, loc)),
          Type.Tuple(List(enumTpe, enumTpe)), loc),
        exp2 = TupleAt(Var(toIdent("x"), 0, enumTpe, loc), 1, enumTpe, loc),
        enumTpe, loc),
      Type.Lambda(List(), enumTpe), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(Value.mkTag(constPropName, identB.name, Value.Unit))(result)
  }

  test("Codegen - TupleAt07") {
    val innerLet = Let(toIdent("y"), 1,
      exp1 = Tuple(List(
        Tuple(List(Int16(123), Int32(456)), Type.Tuple(List(Type.Int16, Type.Int32)), loc),
        Tuple(List(Str("654", loc), Str("321", loc)), Type.Tuple(List(Type.Str, Type.Str)), loc)),
        Type.Tuple(List(Type.Tuple(List(Type.Int16, Type.Int32)), Type.Tuple(List(Type.Str, Type.Str)))), loc),
      exp2 = TupleAt(Var(toIdent("y"), 1, Type.Tuple(List(Type.Tuple(List(Type.Int16, Type.Int32)), Type.Tuple(List(Type.Str, Type.Str)))), loc), 0, Type.Tuple(List(Type.Int32, Type.Int32)), loc),
      Type.Tuple(List(Type.Int16, Type.Int32)), loc)
    val definition = Function(name, args = List(),
      body = Let(toIdent("x"), 0,
        exp1 = innerLet,
        exp2 = TupleAt(Var(toIdent("x"), 0, Type.Tuple(List(Type.Int16, Type.Int32)), loc), 0, Type.Int16, loc),
        Type.Int16, loc),
      Type.Lambda(List(), Type.Int16), loc)

    val code = new CompiledCode(List(definition))
    val result = code.call(name)

    assertResult(123)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Error and MatchError                                                    //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Error01") {
    val definition = Function(name, args = List(),
      body = Error(Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(definition))
    intercept[RuntimeException] { code.call(name) }
  }

  test("Codegen - MatchError01") {
    val definition = Function(name, args = List(),
      body = MatchError(Type.Int8, loc),
      Type.Lambda(List(), Type.Int8), loc)

    val code = new CompiledCode(List(definition))
    intercept[RuntimeException] { code.call(name) }
  }

}
