package ca.uwaterloo.flix.language.backend.phase

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter

object Codegen {
  def genTestAsm(): Array[Byte] = {
    val cw = new ClassWriter(0)
    val cv = new CheckClassAdapter(cw)

    cv.visit(V1_7, ACC_PUBLIC + ACC_SUPER, "ca/uwaterloo/flix/TestAsm", null, "java/lang/Object", null)

    // Constructor
    {
      val mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
      mv.visitInsn(RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }
    // f
    {
      val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, "f", "()I", null, null)
      mv.visitCode()
      mv.visitInsn(ICONST_3)
      mv.visitInsn(ICONST_4)
      mv.visitInsn(IADD)
      mv.visitInsn(IRETURN)
      mv.visitMaxs(2, 0)
      mv.visitEnd()
    }
    cv.visitEnd()

    cw.toByteArray
  }
}

// TODO: What other pieces of the interface/pipeline do we need?
// How is Codegen called? Where do we put the generated methods? How are the generated methods called?
// TODO: Constructor should take a list of (Flix) functions? Does it need anything else? Flix file = class name?
class Codegen {
  private val classWriter = new ClassWriter(0)
  // Note: CheckClassAdapter requires us to manually calculate maxs and frames
  private val visitor = new CheckClassAdapter(classWriter)

  // TODO: generateMethod needs to take a (Flix) function, so it can get the name, signature, and body expression
  def generateMethod(): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "f", "()V", null, null)
    mv.visitCode()

    // TODO: Method body

    mv.visitInsn(RETURN)  // TODO: Proper return instruction
    mv.visitMaxs(0, 0)    // TODO: Calculate maxs
    mv.visitEnd()
  }

  lazy val bytecode: Array[Byte] = {
    // Initialize the visitor to create "public class ca.uwaterloo.flix.TestAsm extends java.lang.Object"
    // with no generic types and no interfaces
    // TODO: Proper name for the class
    visitor.visit(V1_7, ACC_PUBLIC + ACC_SUPER, "ca/uwaterloo/flix/TestAsm", null, "java/lang/Object", null)

    // Generate the constructor for the class
    {
      val mv = visitor.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
      mv.visitInsn(RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }

    // TODO: Generate code for each of the Flix functions

    // Finish the traversal and convert to a byte array
    visitor.visitEnd()
    classWriter.toByteArray
  }
}