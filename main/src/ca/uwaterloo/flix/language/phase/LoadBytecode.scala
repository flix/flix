package ca.uwaterloo.flix.language.phase

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Type}
import ca.uwaterloo.flix.util.{CodeGeneration, DebugBytecode, Options}

import scala.collection.mutable

object LoadBytecode {

  object Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)
  }

  /**
    * Returns the prefix of a Symbol.Resolved as a list of strings.
    * For example, the prefix of the symbol "A.B.C/f" is simply List("A", "B", "C").
    * A symbol "f" corresponds to "Root/f", so its prefix is List("Root").
    */
  def prefixOf(sym: Symbol.Resolved): List[String] = sym.parts match {
    case x :: Nil => List("Root")
    case xs => xs.init
  }

  /**
    * Returns the suffix of a Symbol.Resolved as a string.
    * For example, the suffix of the symbol "A.B.C/f" is simply "f".
    */
  def suffixOf(sym: Symbol.Resolved): String = sym.parts match {
    case x :: Nil => x
    case xs => xs.last
  }

  // Write to a class file, for debugging.
  def dump(path: String, code: Array[Byte]): Unit = Files.write(Paths.get(path), code)

  /**
    * Generate bytecode, load the class, and attach references to compiled methods in the AST.
    */
  def load(root: ExecutableAst.Root, options: Options): ExecutableAst.Root = {
     // Check if code generation is enabled. Otherwise return immediately.
    if (options.codegen != CodeGeneration.Enabled) {
      return root
    }

    // First, we group all the constants by their prefixes (i.e. classes)
    val constants: Map[List[String], List[ExecutableAst.Definition.Constant]] =
      root.constants.values.toList.groupBy { c => prefixOf(c.name) }

    // For each prefix (class), generate bytecode for its constants, then load the bytecode
    val classes = mutable.Map.empty[List[String], Class[_]]
    for ((prefix, consts) <- constants) {
      val bytecode = Codegen.compile(new Codegen.Context(consts, prefix.mkString("/")))
      classes(prefix) = Loader(prefix.mkString("."), bytecode)
      if (options.debugBytecode == DebugBytecode.Enabled) {
        dump(prefix.mkString("", "$", ".class"), bytecode)
      }
    }

    // Iterate over all constants, updating their fields to point to the method objects
    for ((name, const) <- root.constants) {
      val clazz = classes(prefixOf(name))
      val decorated = Codegen.decorate(const.name)
      val argTpes = const.tpe match {
        case Type.Lambda(args, retTpe) => args.map(Codegen.toJavaClass)
        case t => List()
      }
      const.method = clazz.getMethod(decorated, argTpes: _*)
    }

    root
  }

}
