package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.{MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}

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
    *
    * The object is constructed to compute stack map frames automatically.
    */
  def mkClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmType.Object.name.toInternalName
    }
  }

  /**
    * Returns the stack size of a variable of type `tpe` in jvm.
    */
  def getStackSize(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type: $tpe")
    case JvmType.PrimBool => 1
    case JvmType.PrimChar => 1
    case JvmType.PrimFloat => 1
    case JvmType.PrimDouble => 2
    case JvmType.PrimByte => 1
    case JvmType.PrimShort => 1
    case JvmType.PrimInt => 1
    case JvmType.PrimLong => 2
    case JvmType.Reference(_) => 1
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    */
  def getLoadInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => ILOAD
    case JvmType.PrimLong => LLOAD
    case JvmType.PrimFloat => FLOAD
    case JvmType.PrimDouble => DLOAD
    case JvmType.Reference(_) => ALOAD
  }

  /**
    * Returns the store instruction for the value of the type specified by `tpe`
    */
  def getStoreInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => ISTORE
    case JvmType.PrimLong => LSTORE
    case JvmType.PrimFloat => FSTORE
    case JvmType.PrimDouble => DSTORE
    case JvmType.Reference(_) => ASTORE
  }

  /**
    * Returns the array load instruction for arrays of the given JvmType tpe
    */
  def getArrayLoadInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => BALOAD
    case JvmType.PrimChar => CALOAD
    case JvmType.PrimByte => BALOAD
    case JvmType.PrimShort => SALOAD
    case JvmType.PrimInt => IALOAD
    case JvmType.PrimLong => LALOAD
    case JvmType.PrimFloat => FALOAD
    case JvmType.PrimDouble => DALOAD
    case JvmType.Reference(_) => AALOAD
  }

  /**
    * Returns the array store instruction for arrays of the given JvmType tpe
    */
  def getArrayStoreInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => BASTORE
    case JvmType.PrimChar => CASTORE
    case JvmType.PrimByte => BASTORE
    case JvmType.PrimShort => SASTORE
    case JvmType.PrimInt => IASTORE
    case JvmType.PrimLong => LASTORE
    case JvmType.PrimFloat => FASTORE
    case JvmType.PrimDouble => DASTORE
    case JvmType.Reference(_) => AASTORE
  }

  /**
    * Returns the array type code for the value of the type specified by `tpe`
    */
  def getArrayTypeCode(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => T_BOOLEAN
    case JvmType.PrimChar => T_CHAR
    case JvmType.PrimFloat => T_FLOAT
    case JvmType.PrimDouble => T_DOUBLE
    case JvmType.PrimByte => T_BYTE
    case JvmType.PrimShort => T_SHORT
    case JvmType.PrimInt => T_INT
    case JvmType.PrimLong => T_LONG
    case JvmType.Reference(_) => throw InternalCompilerException(s"Expected primitive type. Actual type: $tpe")
  }

  /**
    * Returns the CheckCast type for the value of the type specified by `tpe`
    */
  def getArrayType(tpe: JvmType): String = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => "[Z"
    case JvmType.PrimChar => "[C"
    case JvmType.PrimByte => "[B"
    case JvmType.PrimShort => "[S"
    case JvmType.PrimInt => "[I"
    case JvmType.PrimLong => "[J"
    case JvmType.PrimFloat => "[F"
    case JvmType.PrimDouble => "[D"
    case JvmType.String => "[Ljava/lang/String;"
    case JvmType.Reference(_) => "[Ljava/lang/Object;"
  }

  /**
    * Returns the Array fill type for the value of the type specified by `tpe`
    */
  def getArrayFillType(tpe: JvmType): String = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => "([ZZ)V"
    case JvmType.PrimChar => "([CC)V"
    case JvmType.PrimByte => "([BB)V"
    case JvmType.PrimShort => "([SS)V"
    case JvmType.PrimInt => "([II)V"
    case JvmType.PrimLong => "([JJ)V"
    case JvmType.PrimFloat => "([FF)V"
    case JvmType.PrimDouble => "([DD)V"
    case JvmType.Reference(_) => "([Ljava/lang/Object;Ljava/lang/Object;)V"
  }


  /**
    * Returns the load instruction corresponding to the given type `tpe`
    */
  def getReturnInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => IRETURN
    case JvmType.PrimLong => LRETURN
    case JvmType.PrimFloat => FRETURN
    case JvmType.PrimDouble => DRETURN
    case JvmType.Reference(_) => ARETURN
  }

  /**
    * Returns the descriptor of a method take takes the given `argumentTypes` and returns the given `resultType`.
    */
  def getMethodDescriptor(argumentTypes: List[JvmType], resultType: JvmType): String = {
    // Descriptor of result
    val resultDescriptor = resultType.toDescriptor

    // Descriptor of arguments
    val argumentDescriptor = argumentTypes.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  /**
    * `tpe` is jvm type of value on top of the stack. If the value is not primitive, then we cast it to it's specific type,
    * if the value is a primitive then since there is no boxing, then no casting is necessary.
    */
  def castIfNotPrim(visitor: MethodVisitor, tpe: JvmType): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => ()
    case JvmType.PrimChar => ()
    case JvmType.PrimFloat => ()
    case JvmType.PrimDouble => ()
    case JvmType.PrimByte => ()
    case JvmType.PrimShort => ()
    case JvmType.PrimInt => ()
    case JvmType.PrimLong => ()
    case JvmType.Reference(name) => visitor.visitTypeInsn(CHECKCAST, name.toInternalName)
  }

  def castIfNotPrimAndUnbox(visitor: MethodVisitor, tpe: JvmType): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)
    case JvmType.PrimChar =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)
    case JvmType.PrimFloat =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Float")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)
    case JvmType.PrimDouble =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Double")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)
    case JvmType.PrimByte =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Byte")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false)
    case JvmType.PrimShort =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Short")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)
    case JvmType.PrimInt =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Integer")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
    case JvmType.PrimLong =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Long")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)
    case JvmType.Reference(name) => visitor.visitTypeInsn(CHECKCAST, name.toInternalName)
  }

  def boxIfPrim(visitor: MethodVisitor, tpe: JvmType): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)
    case JvmType.PrimChar => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)
    case JvmType.PrimFloat => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false)
    case JvmType.PrimDouble => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false)
    case JvmType.PrimByte => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)
    case JvmType.PrimShort => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)
    case JvmType.PrimInt => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
    case JvmType.PrimLong => visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)
    case JvmType.Reference(name) => ()
  }

  /**
    * Generates a field for the class with with name `fieldName`, with descriptor `descriptor` using `visitor`.
    *
    * If `isStatic = true` then the field is static, otherwise the field will be non-static.
    *
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
    */
  def compileField(visitor: ClassWriter, fieldName: String, fieldType: JvmType, isStatic: Boolean, isPrivate: Boolean): Unit = {
    val visibility =
      if (isPrivate) {
        ACC_PRIVATE
      } else {
        ACC_PUBLIC
      }

    val access =
      if (isStatic) {
        ACC_STATIC
      } else {
        0
      }

    val field = visitor.visitField(visibility + access, fieldName, fieldType.toDescriptor, null, null)
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
    */
  def compileGetFieldMethod(visitor: ClassWriter, className: JvmName, fieldName: String, methodName: String, fieldType: JvmType): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, getMethodDescriptor(Nil, fieldType), null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, className.toInternalName, fieldName, fieldType.toDescriptor)
    method.visitInsn(getReturnInstruction(fieldType))
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
    */
  def compileSetFieldMethod(visitor: ClassWriter, classType: JvmName, fieldName: String, methodName: String, fieldType: JvmType): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, getMethodDescriptor(List(fieldType), JvmType.Void), null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(getLoadInstruction(fieldType), 1)
    method.visitFieldInsn(PUTFIELD, classType.toInternalName, fieldName, fieldType.toDescriptor)
    method.visitInsn(RETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `getBoxedTagValue` method which returns the boxed value of `value` field of the class.
    * The generated method will return the `value` field if the field is of object type, otherwise, it will
    * box the object using the appropriate type.
    * For example, we generate the following method for `Ok[Int32]`:
    *
    * public final Object getBoxedTagValue() {
    * return new Integer(this.value);
    * }
    *
    * And we generate the following method for `Ok[List[Int32]]`
    *
    * public final Object getBoxedTagValue() {
    * return this.value;
    * }
    */
  def compileGetBoxedTagValueMethod(visitor: ClassWriter, classType: JvmType.Reference, valueType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getBoxedTagValue", AsmOps.getMethodDescriptor(Nil, JvmType.Object), null, null)

    method.visitCode()

    AsmOps.boxField(method, valueType, classType, "getValue")

    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generates code to throw a MatchError.
    */
  def compileThrowFlixError(mv: MethodVisitor, className: JvmName, loc: SourceLocation): Unit = {
    compileReifiedSourceLocation(mv, loc)
    mv.visitTypeInsn(NEW, className.toInternalName)
    mv.visitInsn(DUP2)
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKESPECIAL, className.toInternalName, "<init>", "(Lflix/runtime/ReifiedSourceLocation;)V", false)
    mv.visitInsn(ATHROW)
  }

  /**
    * Generates code to throw a MatchError.
    */
  def compileThrowHoleError(mv: MethodVisitor, hole: String, loc: SourceLocation): Unit = {
    compileReifiedSourceLocation(mv, loc)
    val className = JvmName.Runtime.HoleError
    mv.visitTypeInsn(NEW, className.toInternalName)
    mv.visitInsn(DUP2)
    mv.visitInsn(SWAP)
    mv.visitLdcInsn(hole)
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKESPECIAL, className.toInternalName, "<init>", "(Ljava/lang/String;Lflix/runtime/ReifiedSourceLocation;)V", false)
    mv.visitInsn(ATHROW)
  }

  /**
    * Generates code which instantiate a reified source location.
    */
  def compileReifiedSourceLocation(mv: MethodVisitor, loc: SourceLocation): Unit = {
    mv.visitTypeInsn(NEW, JvmName.Runtime.ReifiedSourceLocation.toInternalName)
    mv.visitInsn(DUP)
    mv.visitLdcInsn(loc.source.format)
    mv.visitLdcInsn(loc.beginLine)
    mv.visitLdcInsn(loc.beginCol)
    mv.visitLdcInsn(loc.endLine)
    mv.visitLdcInsn(loc.endCol)
    mv.visitMethodInsn(INVOKESPECIAL, JvmName.Runtime.ReifiedSourceLocation.toInternalName, "<init>", "(Ljava/lang/String;IIII)V", false)
  }

  /**
    * This will generate a method which will throw an exception in case of getting called.
    */
  def compileExceptionThrowerMethod(visitor: ClassWriter, modifiers: Int, methodName: String, descriptor: String, message: String): Unit = {
    // TODO: Ramin: The descriptor argument should be a JvmType, not a string.
    // Method visitor.
    val method = visitor.visitMethod(modifiers, methodName, descriptor, null, Array())
    method.visitCode()

    // Create a new `Exception` object
    method.visitTypeInsn(NEW, JvmName.UnsupportedOperationException.toInternalName)
    method.visitInsn(DUP)

    // add the message to the stack
    method.visitLdcInsn(message)

    // invoke the constructor of the `Exception` object
    method.visitMethodInsn(INVOKESPECIAL, JvmName.UnsupportedOperationException.toInternalName, "<init>",
      getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    // throw the exception
    method.visitInsn(ATHROW)

    method.visitMaxs(3, 0)
    method.visitEnd()
  }

  /**
    * Emits code that puts the function object of the def symbol `def` on top of the stack.
    */
  def compileDefSymbol(sym: Symbol.DefnSym, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    // Retrieve the namespace of the def symbol.
    val ns = JvmOps.getNamespace(sym)

    // Retrieve the JVM type of the namespace.
    val nsJvmType = JvmOps.getNamespaceClassType(ns)

    // Retrieve the name of the namespace field on the context object.
    val nsFieldName = JvmOps.getNamespaceFieldNameInContextClass(ns)

    // Retrieve the name of the def on the namespace object.
    val defFieldName = JvmOps.getDefFieldNameInNamespaceClass(sym)

    // Retrieve the type of the function def class.
    val defJvmType = JvmOps.getFunctionDefinitionClassType(sym)

    // The java.util.function.Function interface type.
    val interfaceType = JvmType.Function

    // Load the current context.
    mv.visitVarInsn(ALOAD, 1)

    // Load the namespace object.
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, nsFieldName, nsJvmType.toDescriptor)

    // Load the def object.
    mv.visitFieldInsn(GETFIELD, nsJvmType.name.toInternalName, defFieldName, defJvmType.toDescriptor)
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
      case JvmType.Void => throw InternalCompilerException(s"Unexpected type $fieldType")
      case JvmType.PrimBool => box(JvmName.Boolean.toInternalName, getMethodDescriptor(List(JvmType.PrimBool), JvmType.Void))
      case JvmType.PrimChar => box(JvmName.Character.toInternalName, getMethodDescriptor(List(JvmType.PrimChar), JvmType.Void))
      case JvmType.PrimByte => box(JvmName.Byte.toInternalName, getMethodDescriptor(List(JvmType.PrimByte), JvmType.Void))
      case JvmType.PrimShort => box(JvmName.Short.toInternalName, getMethodDescriptor(List(JvmType.PrimShort), JvmType.Void))
      case JvmType.PrimInt => box(JvmName.Integer.toInternalName, getMethodDescriptor(List(JvmType.PrimInt), JvmType.Void))
      case JvmType.PrimLong => box(JvmName.Long.toInternalName, getMethodDescriptor(List(JvmType.PrimLong), JvmType.Void))
      case JvmType.PrimFloat => box(JvmName.Float.toInternalName, getMethodDescriptor(List(JvmType.PrimFloat), JvmType.Void))
      case JvmType.PrimDouble => box(JvmName.Double.toInternalName, getMethodDescriptor(List(JvmType.PrimDouble), JvmType.Void))
      case JvmType.Reference(_) =>
        method.visitVarInsn(ALOAD, 0)
        method.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, getterName, getMethodDescriptor(Nil, fieldType), false)
    }
  }

  def castIfNotPrimAndUnbox(tpe: JvmType, mv: MethodVisitor): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)
    case JvmType.PrimChar =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Character")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)
    case JvmType.PrimFloat =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Float")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)
    case JvmType.PrimDouble =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Double")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)
    case JvmType.PrimByte =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Byte")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false)
    case JvmType.PrimShort =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Short")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)
    case JvmType.PrimInt =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
    case JvmType.PrimLong =>
      mv.visitTypeInsn(CHECKCAST, "java/lang/Long")
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)
    case JvmType.Reference(name) => mv.visitTypeInsn(CHECKCAST, name.toInternalName)
  }

  def boxIfPrim(tpe: JvmType, mv: MethodVisitor): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case JvmType.PrimBool => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)
    case JvmType.PrimChar => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)
    case JvmType.PrimFloat => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false)
    case JvmType.PrimDouble => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false)
    case JvmType.PrimByte => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)
    case JvmType.PrimShort => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)
    case JvmType.PrimInt => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
    case JvmType.PrimLong => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)
    case JvmType.Reference(name) => ()
  }

  /**
    * Emits code to construct a new proxy object for the value on top of the stack of the given type `tpe`.
    */
  def newProxyObject(tpe: MonoType, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    // Construct the equal function object.
    mv.visitInsn(ACONST_NULL)

    // Construct the hash function object.
    mv.visitInsn(ACONST_NULL)

    // Construct the toStr function object.
    mv.visitInsn(ACONST_NULL)

    // Construct the proxy object.
    mv.visitMethodInsn(INVOKESTATIC, JvmName.Runtime.ProxyObject.toInternalName, "of", "(Ljava/lang/Object;Ljava/util/function/Function;Ljava/util/function/Function;Ljava/util/function/Function;)Lflix/runtime/ProxyObject;", false);
  }

  /**
    * Emits code to construct a new proxy array for the array value on top of the stack of the given type `tpe`.
    */
  def newProxyArray(tpe: MonoType.Array, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    // The type of the elements of the array.
    val elementType = tpe.tpe

    // The type of the elements of the array, as a JVM type.
    val jvmElementType = JvmOps.getErasedJvmType(elementType)

    // The local variable index of the original array.
    val originalArrayIndex = 2

    // The local variable index of the new array.
    val resultArrayIndex = 3

    // The local variable index of the loop counter.
    val loopCounterIndex = 4

    // Cast the value to an array of objects.
    mv.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmElementType))

    // Store the original array in a local variable.
    mv.visitVarInsn(ASTORE, originalArrayIndex)

    // Compute the length of the original array.
    mv.visitVarInsn(ALOAD, originalArrayIndex)
    mv.visitInsn(ARRAYLENGTH)

    // Allocate a new array of proxy objects of the same length as the original array and store it in a local variable.
    mv.visitTypeInsn(ANEWARRAY, JvmName.Runtime.ProxyObject.toInternalName)
    mv.visitVarInsn(ASTORE, resultArrayIndex)

    // Initialize the loop counter to zero.
    mv.visitInsn(ICONST_0)
    mv.visitVarInsn(ISTORE, loopCounterIndex)

    // The labels for the loop entry and exit.
    val loopEntry = new Label()
    val loopExit = new Label()

    // Visit the entry label.
    mv.visitLabel(loopEntry)

    // Push the loop counter
    mv.visitVarInsn(ILOAD, loopCounterIndex)

    // Push the length of the result array.
    mv.visitVarInsn(ALOAD, resultArrayIndex)
    mv.visitInsn(ARRAYLENGTH)

    // Branch if we are done.
    mv.visitJumpInsn(IF_ICMPGE, loopExit)

    // The loop body.

    // Load the result array and the current index (to prepare for the later store)
    mv.visitVarInsn(ALOAD, resultArrayIndex)
    mv.visitVarInsn(ILOAD, loopCounterIndex)

    // Push the original array.
    mv.visitVarInsn(ALOAD, originalArrayIndex)

    // Load the element at the current index.
    mv.visitVarInsn(ILOAD, loopCounterIndex)
    mv.visitInsn(AsmOps.getArrayLoadInstruction(jvmElementType))

    // Box the element, if necessary.
    boxIfPrim(jvmElementType, mv)

    // Allocate a new proxy object for the element.
    newProxyObject(elementType, mv)

    // The result array, current index, and proxy object is on the stack. Store the element.
    mv.visitInsn(AASTORE)

    // Increment the loop counter.
    mv.visitVarInsn(ILOAD, loopCounterIndex)
    mv.visitInsn(ICONST_1)
    mv.visitInsn(IADD)
    mv.visitVarInsn(ISTORE, loopCounterIndex)

    // Branch to the loop entry.
    mv.visitJumpInsn(GOTO, loopEntry)

    // Visit the exit label.
    mv.visitLabel(loopExit)

    // Loop the result array.
    mv.visitVarInsn(ALOAD, resultArrayIndex)

  }

  /**
    * Emits code to call a closure (not in tail position). fType is the type of the called closure. argsType is the type of its arguments, and resultType is the type of its result.
    */
  def compileClosureApplication(visitor: MethodVisitor, fType: MonoType, argsTypes: List[MonoType], resultType: MonoType)(implicit root: Root, flix: Flix) = {
    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(fType)
    // Type of the function interface
    val functionInterface = JvmOps.getFunctionInterfaceType(fType)
    // Label for the loop
    val loop = new Label
    // Saving args on the continuation interface in reverse
    for ((argType, ind) <- argsTypes.zipWithIndex.reverse) {
      val argErasedType = JvmOps.getErasedJvmType(argType)
      visitor.visitMethodInsn(INVOKEINTERFACE, functionInterface.name.toInternalName, s"setArg$ind",
        AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), true)
    }
    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Begin of the loop
    visitor.visitLabel(loop)
    // Getting `continuation` field on `Context`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Setting `continuation` field of global to `null`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitInsn(ACONST_NULL)
    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Cast to the continuation
    visitor.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
    // Duplicate
    visitor.visitInsn(DUP)
    // Save it on the IFO local variable
    visitor.visitVarInsn(ASTORE, 2)
    // Call invoke
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)
    // Getting `continuation` field on `Context`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    visitor.visitJumpInsn(IFNONNULL, loop)
    // Load IFO from local variable and invoke `getResult` on it
    visitor.visitVarInsn(ALOAD, 2)
    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(resultType)), true)
    AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(resultType))
  }
}
