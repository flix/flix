package ca.uwaterloo.flix.language.backend.phase

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter

object CodeGen {

  /* Generates code for:

     package ca.uwaterloo.flix;
     public class TestAsm {
       public static int f() {
         return 3 + 4;
       }
     }
   */
  def gencode(): Array[Byte] = {
    val cw = new ClassWriter(0)
    val cv = new CheckClassAdapter(cw)
    var mv: MethodVisitor = null

    cv.visit(V1_7, ACC_PUBLIC + ACC_SUPER, "ca/uwaterloo/flix/TestAsm", null, "java/lang/Object", null)

    // Constructor
    {
      mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
      mv.visitInsn(RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }
    // f
    {
      mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, "f", "()I", null, null)
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
