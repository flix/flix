package ca.uwaterloo.flix.language.backend.phase

import java.nio.file.{Paths, Files}

import ca.uwaterloo.flix.language.ast.{Name,SourceLocation}
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

    lazy val code = Codegen.compile(definitions)
    lazy val clazz = Loader(compiledClassName, code)

    // Write to a class file, for debugging.
    def dumpBytecode(path: String = "FlixBytecode.class"): Unit = {
      Files.write(Paths.get(path), code)
    }

    def call(name: String, args: List[Object]): Any = {
      val method = clazz.getMethod(name)
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
    val result = code.call(name.decorate, List()).asInstanceOf[Int]

    assertResult(42)(result)
  }
}
