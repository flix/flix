package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.ClassWriter

/**
  * Generates bytecode for the Unit class.
  */
object GenUnitClass {

  /**
    * Returns the bytecode for the Unit class built-in to the Flix language,
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, JvmName.Unit.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Instance field
    AsmOps.compileField(visitor, "INSTANCE", JvmType.Unit, isStatic = true, isPrivate = true)

    // Generate the constructor
    genConstructor(visitor)

    // Generate `getInstance` method
    genGetInstance(visitor)

    // Generate `toString` method
    genToString(visitor)

    // Generate `equals` method
    genEquals(visitor)

    // Initialize the static field
    initializeStaticField(visitor)

    Map(JvmName.Unit -> JvmClass(JvmName.Unit, visitor.toByteArray))
  }

  /**
    * Generates the constructor of the unit class
    */
  def genConstructor(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val initMethod = visitor.visitMethod(ACC_PRIVATE, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    initMethod.visitCode()
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitInsn(RETURN)
    initMethod.visitMaxs(1, 1)
    initMethod.visitEnd()
  }

  /**
    * Generates the `getInstance()` method of the class
    */
  def genGetInstance(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val getInstanceMethod = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "getInstance", AsmOps.getMethodDescriptor(Nil, JvmType.Unit), null, null)
    getInstanceMethod.visitCode()
    getInstanceMethod.visitFieldInsn(GETSTATIC, JvmName.Unit.toInternalName, "INSTANCE", JvmType.Unit.toDescriptor)
    getInstanceMethod.visitInsn(ARETURN)
    getInstanceMethod.visitMaxs(1, 0)
    getInstanceMethod.visitEnd()
  }

  /**
    * Generates the `toString()` method of the class
    */
  def genToString(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val toStringMethod = visitor.visitMethod(ACC_PUBLIC, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), null, null)
    toStringMethod.visitCode()
    toStringMethod.visitLdcInsn("()")
    toStringMethod.visitInsn(ARETURN)
    toStringMethod.visitMaxs(1, 1)
    toStringMethod.visitEnd()
  }

  /**
    * Generates the `equals(obj)` method of the class
    */
  def genEquals(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val l0 = new asm.Label
    val l1 = new asm.Label
    val equalsMethod = visitor.visitMethod(ACC_PUBLIC, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), null, null)
    equalsMethod.visitCode()
    equalsMethod.visitVarInsn(ALOAD, 1)
    equalsMethod.visitVarInsn(ALOAD, 0)
    equalsMethod.visitJumpInsn(IF_ACMPNE, l0)
    equalsMethod.visitInsn(ICONST_1)
    equalsMethod.visitJumpInsn(GOTO, l1)
    equalsMethod.visitLabel(l0)
    equalsMethod.visitInsn(ICONST_0)
    equalsMethod.visitLabel(l1)
    equalsMethod.visitInsn(IRETURN)
    equalsMethod.visitEnd()
    equalsMethod.visitMaxs(2, 2)
    equalsMethod.visitEnd()
  }

  def initializeStaticField(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val clInitMethod = visitor.visitMethod(ACC_STATIC, "<clinit>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    clInitMethod.visitCode()
    clInitMethod.visitTypeInsn(NEW, JvmName.Unit.toInternalName)
    clInitMethod.visitInsn(DUP)
    clInitMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Unit.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    clInitMethod.visitFieldInsn(PUTSTATIC, JvmName.Unit.toInternalName, "INSTANCE", JvmType.Unit.toDescriptor)
    clInitMethod.visitInsn(RETURN)
    clInitMethod.visitMaxs(2, 0)
    clInitMethod.visitEnd()
  }

}
