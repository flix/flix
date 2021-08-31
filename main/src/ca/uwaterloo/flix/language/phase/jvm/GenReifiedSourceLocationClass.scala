package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenReifiedSourceLocationClass {

  val sourceFieldName: String = "source"
  val beginLineFieldName: String = "beginLine"
  val beginColFieldName: String = "beginCol"
  val endLineFieldName: String = "endLine"
  val endColFieldName: String = "endCol"
  val constructorDescriptor: String = AsmOps.getMethodDescriptor(List(JvmType.String, JvmType.PrimInt, JvmType.PrimInt, JvmType.PrimInt, JvmType.PrimInt), JvmType.Void)

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.ReifiedSourceLocation
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass.toInternalName, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    def mkIntField(name: String): Unit = visitor.visitField(ACC_PUBLIC + ACC_FINAL, name, JvmType.PrimInt.toDescriptor, null, null).visitEnd()
    visitor.visitField(ACC_PUBLIC + ACC_FINAL, sourceFieldName, JvmType.String.toDescriptor, null, null).visitEnd()
    mkIntField(beginLineFieldName)
    mkIntField(beginColFieldName)
    mkIntField(endLineFieldName)
    mkIntField(endColFieldName)

    genConstructor(name, superClass, visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def genConstructor(name: JvmName, superClass: JvmName, visitor: ClassWriter)(implicit flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "<init>", constructorDescriptor, null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, sourceFieldName, JvmType.String.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 2)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 3)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 4)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, endLineFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 5)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, endColFieldName, JvmType.PrimInt.toDescriptor)

    method.visitInsn(RETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}
