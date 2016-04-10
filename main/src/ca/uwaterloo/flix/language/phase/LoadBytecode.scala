package ca.uwaterloo.flix.language.phase

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Type}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{CodeGeneration, DebugBytecode, InternalCompilerException, Options}

import scala.collection.mutable

object LoadBytecode {

  class Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)
  }

  /**
    * Generate bytecode, load the class, and attach references to compiled methods in the AST.
    */
  def load(root: ExecutableAst.Root, options: Options): ExecutableAst.Root = {
     // Check if code generation is enabled. Otherwise return immediately.
    if (options.codegen != CodeGeneration.Enabled) {
      return root
    }

    // Create a new classloader for each root we compile.
    val loader = new Loader

    // First, we group all the constants by their prefixes (i.e. classes)
    val constants: Map[List[String], List[ExecutableAst.Definition.Constant]] =
      root.constants.values.toList.groupBy { c => c.name.prefix }

    // For each prefix (class), generate bytecode for its constants, then load the bytecode
    val classes = mutable.Map.empty[List[String], Class[_]]
    for ((prefix, consts) <- constants) {
      val bytecode = Codegen.compile(new Codegen.Context(consts, prefix.mkString("/")))
      classes(prefix) = loader(prefix.mkString("."), bytecode)
      if (options.debugBytecode == DebugBytecode.Enabled) {
        dump(prefix.mkString("", "$", ".class"), bytecode)
      }
    }

    // Iterate over all constants, updating their fields to point to the method objects
    for ((name, const) <- root.constants) {
      val clazz = classes(name.prefix)
      val decorated = Codegen.decorate(const.name)
      val argTpes = const.tpe match {
        case Type.Lambda(args, retTpe) => args.map(toJavaClass)
        case t => List()
      }
      const.method = clazz.getMethod(decorated, argTpes: _*)
    }

    root
  }

  // Write to a class file, for debugging.
  def dump(path: String, code: Array[Byte]): Unit = Files.write(Paths.get(path), code)

  /**
    * Convert a Flix type `tpe` into a representation of a Java type, i.e. an instance of `Class[_]`.
    *
    * Used for reflection.
    */
  def toJavaClass(tpe: Type): Class[_] = tpe match {
    case Type.Unit => Value.Unit.getClass
    case Type.Bool => classOf[Boolean]
    case Type.Char => classOf[Char]
    case Type.Float32 => classOf[Float]
    case Type.Float64 => classOf[Double]
    case Type.Int8 => classOf[Byte]
    case Type.Int16 => classOf[Short]
    case Type.Int32 => classOf[Int]
    case Type.Int64 => classOf[Long]
    case Type.Str => classOf[java.lang.String]
    case Type.Enum(_, _) => classOf[Value.Tag]
    case Type.Tuple(elms) => classOf[Value.Tuple]
    case Type.Lambda(_, _) => ???
    case Type.Tag(_, _, _) => throw InternalCompilerException(s"No corresponding JVM type for $tpe.")
    case _ => ???
  }

}
