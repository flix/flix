package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenHoleErrorClass {

  val holeFieldName: String = "hole"
  val locationFieldName: String = "location"

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.HoleError
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.FlixError

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass.toInternalName, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    genConstructor(name, superClass, visitor)
    visitor.visitField(ACC_PUBLIC + ACC_FINAL, holeFieldName, JvmName.String.toDescriptor, null, null).visitEnd()
    visitor.visitField(ACC_PUBLIC + ACC_FINAL, locationFieldName, JvmName.ReifiedSourceLocation.toDescriptor, null, null).visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

  def genConstructor(name: JvmName, superClass: JvmName, visitor: ClassWriter): Unit = {
    val stringToBuilderDescriptor = s"(${JvmName.String.toDescriptor})${JvmName.StringBuilder.toDescriptor}"
    val builderName = JvmName.StringBuilder.toInternalName

    val method = visitor.visitMethod(ACC_PUBLIC, "<init>", s"(${JvmName.String.toDescriptor}${JvmName.ReifiedSourceLocation.toDescriptor})${JvmType.Void.toDescriptor}", null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitTypeInsn(NEW, builderName)
    method.visitInsn(DUP)
    method.visitMethodInsn(INVOKESPECIAL, builderName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    method.visitLdcInsn("Hole '")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 1)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitLdcInsn("' at ")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 2)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.ReifiedSourceLocation.toInternalName, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), false)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), false)
    method.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, "<init>", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, holeFieldName, JvmName.String.toDescriptor)
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, locationFieldName, JvmName.ReifiedSourceLocation.toDescriptor)
    method.visitInsn(RETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}
