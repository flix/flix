package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, MethodVisitor}

/**
  * Generates bytecode for the exceptions.
  */
object GenExceptionClasses {

  /**
    * Returns the bytecode for the exception classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // TODO: Ramin.
    //
    // Emit code for:
    //
    //   HoleException.
    //   MatchException.
    //   SwitchException.
    //   UserException.
    //
    // And emit an interface FlixException that all of these extend.
    //
    // Note: Skip `RuleException`.

    val flixException = genFlixException()



    Map.empty
  }

  def genFlixException()(implicit root: Root, flix: Flix): Unit = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_SUPER + ACC_ABSTRACT, JvmName.FlixException.toInternalName, null,
      JvmName.RuntimeException.toInternalName, null)

    // Constructor
    genFlixExceptionConstructor(visitor)

  }


  def genFlixExceptionConstructor(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val init = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), null, null)
    init.visitVarInsn(ALOAD, 0)
    init.visitVarInsn(ALOAD, 1)
    init.visitMethodInsn(INVOKESPECIAL, JvmName.RuntimeException.toInternalName, "<init>",
    AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
    init.visitInsn(RETURN)
    init.visitMaxs(2, 2)
    init.visitEnd()
  }



}
