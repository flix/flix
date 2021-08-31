package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenFlixErrorClass {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.FlixError
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.RuntimeException.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT, name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    // Constructor
    genConstructor(visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def genConstructor(visitor: ClassWriter)(implicit flix: Flix): Unit = {
    val constructorDescriptor = AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void)
    val method = visitor.visitMethod(ACC_PUBLIC, "<init>", constructorDescriptor, null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    method.visitMethodInsn(INVOKESPECIAL, JvmName.RuntimeException.toInternalName, "<init>", constructorDescriptor, false)
    method.visitInsn(RETURN)
    method.visitMaxs(999, 999)
    method.visitEnd()

  }

}
