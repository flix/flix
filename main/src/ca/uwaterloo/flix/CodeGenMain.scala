package ca.uwaterloo.flix

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.language.backend.phase.Codegen

// Entry point (test harness) for the code generator
object CodeGenMain {

  def main(argv: Array[String]): Unit = {
    // Create the byte array.
    val code = Codegen.genTestAsm()

    // Write to a class file, for debugging.
    Files.write(Paths.get("FlixBytecode.class"), code)

    // Directly load the byte array as a class. Get the method and invoke it.
    val clazz = MyClassLoader.load("ca.uwaterloo.flix.TestAsm", code)
    val method = clazz.getMethod("f")
    val result = method.invoke(null)

    println(result)
  }

  object MyClassLoader extends ClassLoader {
    def load(name: String, b: Array[Byte]): Class[_] = {
      defineClass(name, b, 0, b.length)
    }
  }

}
