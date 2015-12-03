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

  // Write to a class file, for debugging.
  def dumpBytecode(code: Array[Byte], path: String = "FlixBytecode.class"): Unit = {
    Files.write(Paths.get(path), code)
  }

  object Loader extends ClassLoader {
    def apply(name: String, b: Array[Byte]): Class[_] = {
      defineClass(name, b, 0, b.length)
    }
  }

  def call(clazz: Class[_], name: String, args: List[Object]): Any = {
    val method = clazz.getMethod(name)
    method.invoke(null, args: _*)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int constants                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Codegen - Int01") {
    val name = Name.Resolved.mk(List("foo", "bar", "f"))
    val definition = Function(name, args = List(),
      body = Int(42, Type.Int32, loc),
      Type.Lambda(List(), Type.Int32), loc)

    val code = Codegen.compile(List(definition))
    val clazz = Loader(compiledClassName, code)
    val result = call(clazz, name.decorate, List()).asInstanceOf[scala.Int]

    assertResult(42)(result)
  }
}
