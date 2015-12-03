package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.{Definition, Expression}
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.Expression._

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
   * Generate the constructor. Takes a ClassVisitor (that has already been initialized).
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
   * Takes a ClassVisitor (that has already been initialized).
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  def compileFunction(visitor: ClassVisitor, function: Definition.Function): Unit = {
    // TODO: Proper signature
    val tpe = function.tpe

    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, function.name.decorate, "()V", null, null)
    mv.visitCode()

    // Compile the method body
    compileExpression(mv, function.body)

    mv.visitInsn(RETURN)  // TODO: Proper return instruction
    mv.visitMaxs(0, 0)    // TODO: Calculate maxs
    mv.visitEnd()
  }

  def compileExpression(visitor: MethodVisitor, exp: Expression): Unit = exp match {
    case Int(i, tpe, loc) => ???
    case Var(localVar, tpe, loc) => ???
    case Apply(name, args, tpe, loc) => ???
    case Let(localVar, exp1, exp2, tpe, loc) => ???
    case Unary(op, exp, tpe, loc) => ???
    case Binary(op, exp1, exp2, tpe, loc) => ???
    case IfThenElse(exp1, exp2, exp3, tpe, loc) => ???
    case Tag(tag, tpe, loc) => ???
    case Tuple(elms, tpe, loc) => ???
    case Set(elms, tpe, loc) => ???
    case ElementAt(exp1, exp2, tpe, loc) => ???
    case DerefTag() => ???
    case Error(loc) => ???
  }
}