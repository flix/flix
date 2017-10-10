package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.{ClassWriter, MethodVisitor}
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
  def getMethodDescriptor(argumentTypes: List[JvmType], resultType: JvmType = JvmType.Void): String = {
    // TODO: Avoid use of default arguments.

    // Descriptor of result
    val resultDescriptor = resultType.toDescriptor

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

  /**
    * Generate the `methodName` method for fetching the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * For example, `Val[Char]` has following `getValue()`method:
    *
    * public final char getValue() {
    * return this.value;
    * }
    *
    * @param visitor      class visitor
    * @param internalName Internal name of the class
    * @param fieldType    JvmType of the field
    * @param methodName   method name of getter of `fieldName`
    * @param iReturn      opcode for returning the value of the field
    */
  def compileGetFieldMethod(visitor: ClassWriter, internalName: String, fieldType: JvmType, fieldName: String,
                            methodName: String, iReturn: Int): Unit = {
    // TODO: Derive iReturn from fieldType.
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, getMethodDescriptor(Nil, fieldType), null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, internalName, fieldName, fieldType.toDescriptor)
    method.visitInsn(iReturn)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `methodName` method for setting the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * For example, the class of `Tuple[Int32, Int32]` has the following `setField0` method:
    *
    * public final void setField0(int var) {
    *   this.field0 = var;
    * }
    *
    * @param visitor      class visitor
    * @param internalName Internal name of the class
    * @param fieldType    JvmType of the field
    * @param methodName   method name of getter of `fieldName`
    * @param iLoad        opcode for loading the single parameter of the method
    */
  def compileSetFieldMethod(visitor: ClassWriter, internalName: String, fieldType: JvmType, fieldName: String,
                            methodName: String, iLoad: Int): Unit = {
    // TODO: Derive iReturn from fieldType.
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, getMethodDescriptor(List(fieldType)), null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(iLoad, 1)
    method.visitFieldInsn(PUTFIELD, internalName, fieldName, fieldType.toDescriptor)
    method.visitInsn(RETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Returns the load instruction corresponding to the `fType`
    *
    * @param fType type
    * @return A load instruction
    */
  def getReturnInsn(fType: JvmType): Int = fType match {
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => IRETURN
    case JvmType.PrimLong => LRETURN
    case JvmType.PrimFloat => FRETURN
    case JvmType.PrimDouble => DRETURN
    case _ => ARETURN
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    *
    * @param tpe Wrapped type to be loaded
    * @return Appropriate load instruction for the given type
    */
  def getLoadInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => ILOAD
    case JvmType.PrimLong => LLOAD
    case JvmType.PrimFloat => FLOAD
    case JvmType.PrimDouble => DLOAD
    case _ => ALOAD
  }

  /**
    * This will generate a method which will throw an exception in case of getting called.
    *
    * @param modifiers  Modifiers of the generated method
    * @param methodName Name of the method
    * @param descriptor Descriptor of the method
    * @param message    Message of the exception to be thrown
    */
  def exceptionThrowerMethod(visitor: ClassWriter, modifiers: Int, methodName: String, descriptor: String, message: String): Unit = {
    // Method visitor.
    val method = visitor.visitMethod(modifiers, methodName, descriptor, null, Array(JvmName.Exception.toInternalName))
    method.visitCode()

    // Create a new `Exception` object
    method.visitTypeInsn(NEW, JvmName.UnsupportedOperationException.toInternalName)
    method.visitInsn(DUP)

    // add the message to the stack
    method.visitLdcInsn(message)

    // invoke the constructor of the `Exception` object
    method.visitMethodInsn(INVOKESPECIAL, JvmName.UnsupportedOperationException.toInternalName, "<init>",
      getMethodDescriptor(List(JvmType.String)), false)

    // throw the exception
    method.visitInsn(ATHROW)

    method.visitMaxs(3, 0)
    method.visitEnd()
  }

  /**
    * This method box a field with name `name` with type `tpe` on the class `className`
    * If the field is a primitive then it is boxed using the appropriate java type, if it is not a primitive
    * then we just return the field
    *
    * @param method     MethodVisitor used to emit the code to a method
    * @param fieldType  type of the field to be boxed
    * @param classType  class that the field is defined on
    * @param getterName name of the field to be boxed
    */
  def boxField(method: MethodVisitor, fieldType: JvmType, classType: JvmType.Reference, getterName: String): Unit = {

    /**
      * This method will box the primitive on top of the stack
      *
      * @param boxedObjectInternalName descriptor of the boxed version of the primitive
      * @param signature               signature of the constructor of the boxer
      */
    def box(boxedObjectInternalName: String, signature: String): Unit = {
      method.visitTypeInsn(NEW, boxedObjectInternalName)
      method.visitInsn(DUP)
      method.visitVarInsn(ALOAD, 0)
      method.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, getterName, getMethodDescriptor(Nil, fieldType), false)
      method.visitMethodInsn(INVOKESPECIAL, boxedObjectInternalName, "<init>", signature, false)
    }

    // based on the type of the field, we pick the appropriate class that boxes the primitive
    fieldType match {
      case JvmType.PrimBool => box(JvmName.Boolean.toInternalName, getMethodDescriptor(List(JvmType.PrimBool)))
      case JvmType.PrimChar => box(JvmName.Character.toInternalName, getMethodDescriptor(List(JvmType.PrimChar)))
      case JvmType.PrimByte => box(JvmName.Byte.toInternalName, getMethodDescriptor(List(JvmType.PrimByte)))
      case JvmType.PrimShort => box(JvmName.Short.toInternalName, getMethodDescriptor(List(JvmType.PrimShort)))
      case JvmType.PrimInt => box(JvmName.Integer.toInternalName, getMethodDescriptor(List(JvmType.PrimInt)))
      case JvmType.PrimLong => box(JvmName.Long.toInternalName, getMethodDescriptor(List(JvmType.PrimLong)))
      case JvmType.PrimFloat => box(JvmName.Float.toInternalName, getMethodDescriptor(List(JvmType.PrimFloat)))
      case JvmType.PrimDouble => box(JvmName.Double.toInternalName, getMethodDescriptor(List(JvmType.PrimDouble)))
      case _ =>
        method.visitVarInsn(ALOAD, 0)
        method.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, getterName, getMethodDescriptor(Nil, fieldType), false)
    }
  }

}
