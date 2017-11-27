package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the cell classes.
  */
object GenCellClasses {

  /**
    * Returns the bytecode for the cell classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Type that we need a cell class for
    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.PrimLong, JvmType.Object)

    // Generating each cell class
    types.map{ tpe =>
      val classType = JvmName.getCellClassType(tpe)
      classType.name -> JvmClass(classType.name, genCellClass(classType, tpe))
    }.toMap
  }

  /**
    * Generating class `classType` with value of type `subType`
    */
  def genCellClass(classType: JvmType.Reference, subType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, null)


    // Generate the instance field
    AsmOps.compileField(visitor, "value", subType, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(classType, subType, visitor)

    // Generate `getValue` method
    genGetValue(classType, subType, visitor)

    // Generate `setValue` method
    genSetValue(classType, subType, visitor)

    // Generate the `toString` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate the `hashCode` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate the `equals` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
      "equals method shouldn't be called")

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generating constructor for the class `classType` with value of type `subType`
    */
  def genConstructor(classType: JvmType.Reference, subType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(subType)
    val initMethod = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(subType), JvmType.Void), null, null)
    initMethod.visitCode()
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitVarInsn(iLoad, 1)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "value", subType.toDescriptor)
    initMethod.visitInsn(RETURN)
    initMethod.visitMaxs(2, 2)
    initMethod.visitEnd()
  }

  /**
    * Generating `getValue` method for the class `classType` with value of type `subType`
    */
  def genGetValue(classType: JvmType.Reference, subType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iRet = AsmOps.getReturnInstruction(subType)
    val getValue = visitor.visitMethod(ACC_PUBLIC, "getValue", AsmOps.getMethodDescriptor(Nil, subType), null, null)
    getValue.visitCode()
    getValue.visitVarInsn(ALOAD, 0)
    getValue.visitFieldInsn(GETFIELD, classType.name.toInternalName, "value", subType.toDescriptor)
    getValue.visitInsn(iRet)
    getValue.visitMaxs(1, 1)
    getValue.visitEnd()
  }

  /**
    * Generating `setValue` method for the class `classType` with value of type `subType`
    */
  def genSetValue(classType: JvmType.Reference, subType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(subType)
    val setValue = visitor.visitMethod(ACC_PUBLIC, "setValue", AsmOps.getMethodDescriptor(List(subType), JvmType.Void), null, null)
    setValue.visitCode()
    setValue.visitVarInsn(ALOAD, 0)
    setValue.visitVarInsn(iLoad, 1)
    setValue.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "value", subType.toDescriptor)
    setValue.visitInsn(RETURN)
    setValue.visitMaxs(2, 2)
    setValue.visitEnd()
  }

}
