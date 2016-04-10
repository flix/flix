package ca.uwaterloo.flix.language.phase

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Type}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{CodeGeneration, DebugBytecode, InternalCompilerException, Options}

import scala.collection.mutable

object LoadBytecode {

  class Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)

    def apply(prefix: List[String], bytes: Array[Byte]): Class[_] = apply(prefix.mkString("."), bytes)
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
    val loader = new Loader()

    // First, we group all the constants by their prefixes (i.e. classes).
    // We also transform all non-function constants into 0-arg functions, which codegen expects.
    val constants: Map[List[String], List[ExecutableAst.Definition.Constant]] =
      root.constants.values.map { f =>
        f.tpe match {
          case _: Type.Lambda => f
          case t => f.copy(tpe = Type.Lambda(List(), t))
        }
      }.toList.groupBy(_.name.prefix)

    // Create a map of names to types (i.e. declarations), so we can generate code for function calls.
    // Note: We use `constants` (instead of `root.constants`) because we need the rewritten 0-arg functions.
    val declarations: Map[Symbol.Resolved, Type] = constants.values.flatten.map { f => (f.name, f.tpe) }.toMap

    // For each prefix (class), generate bytecode for its constants, then load the bytecode.
    val classes = mutable.Map.empty[List[String], Class[_]]
    for ((prefix, consts) <- constants) {
      val bytecode = Codegen.compile(Codegen.Context(prefix, consts, declarations))
      classes(prefix) = loader(prefix, bytecode)

      if (options.debugBytecode == DebugBytecode.Enabled) {
        dump(prefix, bytecode)
      }
    }

    // Iterate over all constants, updating their fields to point to the method objects.
    for ((name, const) <- root.constants) {
      val clazz = classes(name.prefix)
      val argTpes = const.tpe match {
        case Type.Lambda(args, retTpe) => args.map(toJavaClass)
        case t => List()
      }
      const.method = clazz.getMethod(name.suffix, argTpes: _*)
    }

    root
  }

  def dump(path: String, code: Array[Byte]): Unit = Files.write(Paths.get(path), code)

  def dump(prefix: List[String], code: Array[Byte]): Unit = dump(prefix.mkString("", "$", ".class"), code)

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
