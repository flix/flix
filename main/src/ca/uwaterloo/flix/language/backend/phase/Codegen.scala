package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.Definition

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter

object Codegen {
  // Example code that will be deleted later
  def genTestAsm(): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
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

  /*
   * Given a list of Flix definitions, compile the definitions to bytecode and put them in a JVM class.
   * For now, we put all definitions in a single class: ca.uwaterloo.flix.runtime.compiled.FlixDefinitions.
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  def compile(definitions: List[Definition]): Array[Byte] = {
    val functions = definitions.collect { case f: Definition.Function => f }
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val visitor = new CheckClassAdapter(classWriter)

    // Initialize the visitor to create a class
    visitor.visit(V1_7, ACC_PUBLIC + ACC_SUPER, "ca/uwaterloo/flix/runtime/compiled/FlixDefinitions", null, "java/lang/Object", null)

    // Generate the constructor for the class
    compileConstructor(visitor)

    // Generate code for each of the Flix functions
    functions.foreach { f => compileFunction(visitor, f) }

    // Finish the traversal and convert to a byte array
    visitor.visitEnd()
    classWriter.toByteArray
  }

  /*
   * Generate the constructor.
   */
  def compileConstructor(visitor: ClassVisitor): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /*
   * Given a definition for a Flix function, generate bytecode.
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  def compileFunction(visitor: ClassVisitor, function: Definition.Function): Unit = {
    // TODO: Proper function name and signature
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "f", "()V", null, null)
    mv.visitCode()

    // TODO: Method body

    mv.visitInsn(RETURN)  // TODO: Proper return instruction
    mv.visitMaxs(0, 0)    // TODO: Calculate maxs
    mv.visitEnd()
  }
}