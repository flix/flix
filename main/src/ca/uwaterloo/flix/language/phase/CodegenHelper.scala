package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Definition
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Type => _, _}

/**
  * Created by ramin on 2017-05-22.
  */
object CodegenHelper {
  /*
   * Decorate (mangle) a prefix (list of strings) to get the internal JVM name.
   */
  def decorate(prefix: List[String]): String = prefix.mkString("/")

  /*
   * Returns the internal name of the JVM type that `tpe` maps to.
   *
   * The descriptor of a type must be associated with a Context, because the descriptor of a closure object (i.e. a
   * lambda function and not a JVM method) is the descriptor of a generated interface.
   *
   * The inner function may seem weird at first, but we need it because Type.Lambda has two different descriptors
   * depending on how it's used. If we're compiling a method that takes two ints and returns an int,
   * (i.e. its type is Type.Lambda(List(Int, Int), Int)) we need to generate the JVM descriptor `(II)I`.
   * On the other hand, if that same method is being passed as a closure, we need to use the interface that was
   * generated for that closure, and not its JVM type descriptor. We don't want a type descriptor to look like
   * `((II)I)I`.
   */
  //TODO: We should get rid of `check` when we fully implement enum interfaces
  def descriptor(tpe: Type, interfaces: Map[Type, List[String]], enumInterfaces: Map[Type, Class[_]], check: Boolean = true): String = {
    def inner(tpe: Type): String = tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Unit => asm.Type.getDescriptor(Constants.unitClass)
      case Type.Bool => asm.Type.BOOLEAN_TYPE.getDescriptor
      case Type.Char => asm.Type.CHAR_TYPE.getDescriptor
      case Type.Float32 => asm.Type.FLOAT_TYPE.getDescriptor
      case Type.Float64 => asm.Type.DOUBLE_TYPE.getDescriptor
      case Type.Int8 => asm.Type.BYTE_TYPE.getDescriptor
      case Type.Int16 => asm.Type.SHORT_TYPE.getDescriptor
      case Type.Int32 => asm.Type.INT_TYPE.getDescriptor
      case Type.Int64 => asm.Type.LONG_TYPE.getDescriptor
      case Type.BigInt => asm.Type.getDescriptor(Constants.bigIntegerClass)
      case Type.Str => asm.Type.getDescriptor(Constants.stringClass)
      case Type.Native => asm.Type.getDescriptor(Constants.objectClass)
      case Type.Apply(Type.Arrow(l), _) => s"L${decorate(interfaces(tpe))};"
      case Type.Apply(Type.FTuple(l), _) => asm.Type.getDescriptor(Constants.arrayObjectClass)
      case _ if tpe.isEnum => if(check) {
        asm.Type.getDescriptor(Constants.tagClass) //This case will be deleted
      } else {
        asm.Type.getDescriptor(enumInterfaces(tpe))
      }
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    tpe match {
      case Type.Apply(Type.Arrow(l), ts) => s"(${ts.take(l - 1).map(inner).mkString})${inner(ts.last)}"
      case _ => inner(tpe)
    }
  }

  /*
  * Constants passed to the asm library to generate bytecode.
  *
  * Originally, we simply used string literals (to encode internal class names, descriptors, etc.) and passed them to
  * the library. This was very brittle for two reasons: 1) changing a class or method would break things and we
  * wouldn't know until run time; and 2) multiple string literals would have to be updated.
  *
  * The solution is to use compile-time reflection whenever possible, and move commonly-used constants into this
  * object. Constants that are used only once are left inline.
  *
  * Compile-time reflection gives us compile-time errors if we change a class and don't update here. However, we are
  * stuck with run-time reflection for certain things, e.g. getting a method. So if we change a method and don't
  * update here, we won't know until we run the tests.
  *
  * Note that we can't easily use reflection to get singleton or package objects. (It is possible with run-time
  * reflection, but it is awkward and we don't get the benefit of compile-time errors.)
  *
  * TODO: Refactor the Value object to be Java-like style so reflection is easier. Or use run-time reflection?
  */
  object Constants {
    val objectClass : Class[_] = classOf[Object]
    val stringClass : Class[_] = classOf[java.lang.String]
    val bigIntegerClass : Class[_]= classOf[java.math.BigInteger]
    val arrayObjectClass : Class[_] = classOf[Array[Object]]
    val setClass : Class[_] = classOf[scala.collection.immutable.Set[Object]]
    val flixClass : Class[_] = classOf[Flix]

    val unitClass : Class[_] = Value.Unit.getClass
    val tagClass : Class[_] = classOf[Value.Tag]

    val valueObject = "ca/uwaterloo/flix/runtime/Value$"
    val scalaPredef = "scala/Predef$"
    val scalaMathPkg = "scala/math/package$"

    def loadValueObject(visitor: MethodVisitor): Unit =
      visitor.visitFieldInsn(GETSTATIC, valueObject, "MODULE$", s"L$valueObject;")
  }

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  case class Context(prefix: List[String],
                     functions: List[Definition.Constant],
                     declarations: Map[Symbol.DefnSym, Type],
                     interfaces: Map[Type, List[String]],
                     enumInterfaces: Map[Type, Class[_]],
                     enums: Map[Symbol.EnumSym, (Class[_], Map[String, Class[_]])]) {
    // wrapper around descriptor
    def descriptor(tpe: Type, check: Boolean = true): String = CodegenHelper.descriptor(tpe, interfaces, enumInterfaces, check)
  }
}
