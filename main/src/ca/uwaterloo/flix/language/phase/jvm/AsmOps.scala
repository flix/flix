package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object AsmOps {

  /**
    * Returns the target JVM version.
    */
  def JavaVersion(implicit flix: Flix): Int = flix.options.target match {
    case JvmTarget.Version16 => V1_6
    case JvmTarget.Version17 => V1_7
    case JvmTarget.Version18 => V1_8
    case JvmTarget.Version19 => throw InternalCompilerException(s"Unsupported Java version: '1.9'.")
  }

  /**
    * Returns a freshly created class writer object.
    */
  def mkClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmType.Object.name.toInternalName
    }
  }

  /**
    * Returns the descriptor of a method take takes the given `argumentTypes` and returns the given `resultType`.
    */
  def getMethodDescriptor(argumentTypes: List[JvmType], resultType: Option[JvmType] = None): String = {
    // Descriptor of result
    val resultDescriptor = resultType.map(_.toDescriptor).getOrElse("V")

    // Descriptor of arguments
    val argumentDescriptor = argumentTypes.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  /**
    * Generates a field for the class with with name `name`, with descriptor `descriptor` using `visitor`. If `isStatic = true`
    * then the field is static, otherwise the field will be non-static.
    * For example calling this method with name = `field01`, descriptor = `I`, isStatic = `false` and isPrivate = `true`
    * creates the following field:
    *
    * private int field01;
    *
    * calling this method with name = `value`, descriptor = `java/lang/Object`, isStatic = `false` and isPrivate = `true`
    * creates the following:
    *
    * private Object value;
    *
    * calling this method with name = `unitInstance`, descriptor = `ca/waterloo/flix/enums/List/object/Nil`, `isStatic = true`
    * and isPrivate = `false` generates the following:
    *
    * public static Nil unitInstance;
    *
    * @param visitor    class visitor
    * @param name       name of the field
    * @param descriptor descriptor of field
    * @param isStatic   if this is true the the field is static
    * @param isPrivate  if this is set then the field is private
    */
  def compileField(visitor: ClassWriter, name: String, descriptor: String, isStatic: Boolean, isPrivate: Boolean): Unit = {
    // TODO: Why can the descriptor not be a JvmType?
    // TODO: isStatic and isPrivate should be ADTs.
    val visibility =
    if (isPrivate) {
      ACC_PRIVATE
    } else {
      ACC_PUBLIC
    }

    val fieldType =
      if (isStatic) {
        ACC_STATIC
      } else {
        0
      }

    val field = visitor.visitField(visibility + fieldType, name, descriptor, null, null)
    field.visitEnd()
  }

}
