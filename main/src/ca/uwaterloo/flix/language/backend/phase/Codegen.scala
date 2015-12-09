package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.{Name, BinaryOperator, UnaryOperator}
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.{Definition, Expression}
import ca.uwaterloo.flix.language.backend.ir.CodeGenIR.Expression._

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter

object Codegen {

  case class Context(definitions: List[Definition], clazz: String = "ca/uwaterloo/flix/runtime/compiled/FlixDefinitions") {
    val functions = definitions.collect { case f: Definition.Function => f }
    val getFunction = functions.map { f => (f.name, f) }.toMap
  }

  /*
   * Given a list of Flix definitions, compile the definitions to bytecode and put them in a JVM class.
   * For now, we put all definitions in a single class: ca.uwaterloo.flix.runtime.compiled.FlixDefinitions.
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  def compile(context: Context): Array[Byte] = {
    val functions = context.functions
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val visitor = new CheckClassAdapter(classWriter)

    // Initialize the visitor to create a class
    visitor.visit(V1_7, ACC_PUBLIC + ACC_SUPER, context.clazz, null, "java/lang/Object", null)

    // Generate the constructor for the class
    compileConstructor(context, visitor)

    // Generate code for each of the Flix functions
    functions.foreach(compileFunction(context, visitor))

    // Finish the traversal and convert to a byte array
    visitor.visitEnd()
    classWriter.toByteArray
  }

  /*
   * Generate the constructor. Takes a ClassVisitor (that has already been initialized).
   */
  def compileConstructor(context: Context, visitor: ClassVisitor): Unit = {
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
  def compileFunction(context: Context, visitor: ClassVisitor)(function: Definition.Function): Unit = {
    // TODO: Properly handle types. Start implementing other types (e.g. bool)
    // TODO: How are complex types represented?
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, function.name.decorate, function.descriptor, null, null)
    mv.visitCode()

    // Compile the method body
    compileExpression(context, mv)(function.body)

    mv.visitInsn(IRETURN)     // TODO: Proper return instruction
    mv.visitMaxs(999, 999)    // TODO: Calculate maxs
    mv.visitEnd()
  }

  def compileExpression(context: Context, visitor: MethodVisitor)(expr: Expression): Unit = expr match {
    case LoadBool(exp, offset) => ???
    case LoadInt8(exp, offset) => ???
    case LoadInt16(exp, offset) => ???
    case LoadInt32(exp, offset) => ???
    case StoreBool(exp, offset, v) => ???
    case StoreInt8(exp, offset, v) => ???
    case StoreInt16(exp, offset, v) => ???
    case StoreInt32(exp, offset, v) => ???
    case Const(i, tpe, loc) => i match {
      case -1 => visitor.visitInsn(ICONST_M1)
      case 0 => visitor.visitInsn(ICONST_0)
      case 1 => visitor.visitInsn(ICONST_1)
      case 2 => visitor.visitInsn(ICONST_2)
      case 3 => visitor.visitInsn(ICONST_3)
      case 4 => visitor.visitInsn(ICONST_4)
      case 5 => visitor.visitInsn(ICONST_5)
      case _ if Byte.MinValue <= i && i <= Byte.MaxValue => visitor.visitIntInsn(BIPUSH, i)
      case _ if Short.MinValue <= i && i <= Short.MaxValue => visitor.visitIntInsn(SIPUSH, i)
      case _ => visitor.visitLdcInsn(i)
    }
    case Var(v, tpe, loc) => visitor.visitVarInsn(ILOAD, v.offset)
    case Apply(name, args, tpe, loc) =>
      args.foreach(compileExpression(context, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, context.clazz, name.decorate, context.getFunction(name).descriptor, false)
    case Let(v, exp1, exp2, tpe, loc) =>
      compileExpression(context, visitor)(exp1)
      visitor.visitVarInsn(ISTORE, v.offset)
      compileExpression(context, visitor)(exp2)
    case Unary(op, exp, tpe, loc) =>
      compileExpression(context, visitor)(exp)
      op match {
        case UnaryOperator.Not => ???
        case UnaryOperator.Plus => // Unary plus is a nop
        case UnaryOperator.Minus => visitor.visitInsn(INEG)
        case UnaryOperator.Negate =>
          // Note that ~bbbb = bbbb ^ 1111, and since the JVM uses two's complement, -1 = 0xFFFFFFFF, so ~x = x ^ -1
          visitor.visitInsn(ICONST_M1)
          visitor.visitInsn(IXOR)
        case UnaryOperator.Set.IsEmpty => ???
        case UnaryOperator.Set.NonEmpty => ???
        case UnaryOperator.Set.Singleton => ???
        case UnaryOperator.Set.Size => ???
      }
    case Binary(op, exp1, exp2, tpe, loc) =>
      compileExpression(context, visitor)(exp1)
      compileExpression(context, visitor)(exp2)
      op match {
        case BinaryOperator.Plus => visitor.visitInsn(IADD)
        case BinaryOperator.Minus => visitor.visitInsn(ISUB)
        case BinaryOperator.Times => visitor.visitInsn(IMUL)
        case BinaryOperator.Divide => visitor.visitInsn(IDIV)
        case BinaryOperator.Modulo => visitor.visitInsn(IREM)
        case BinaryOperator.Less => ???
        case BinaryOperator.LessEqual => ???
        case BinaryOperator.Greater => ???
        case BinaryOperator.GreaterEqual => ???
        case BinaryOperator.Equal => ???
        case BinaryOperator.NotEqual => ???
        case BinaryOperator.And => ???
        case BinaryOperator.Or => ???
        case BinaryOperator.BitwiseAnd => visitor.visitInsn(IAND)
        case BinaryOperator.BitwiseOr => visitor.visitInsn(IOR)
        case BinaryOperator.BitwiseXor => visitor.visitInsn(IXOR)
        case BinaryOperator.BitwiseLeftShift => visitor.visitInsn(ISHL)
        case BinaryOperator.BitwiseRightShift => visitor.visitInsn(ISHR)
        case BinaryOperator.Set.Member => ???
        case BinaryOperator.Set.SubsetOf => ???
        case BinaryOperator.Set.ProperSubsetOf => ???
        case BinaryOperator.Set.Insert => ???
        case BinaryOperator.Set.Remove => ???
        case BinaryOperator.Set.Union => ???
        case BinaryOperator.Set.Intersection => ???
        case BinaryOperator.Set.Difference => ???
      }
    case IfThenElse(exp1, exp2, exp3, tpe, loc) => ???
    case Tag(name, tag, exp, tpe, loc) => ???
    case TagOf(exp, name, tag, tpe, loc) => ???
    case Tuple(elms, tpe, loc) => ???
    case TupleAt(base, offset, tpe, loc) => ???
    case Set(elms, tpe, loc) => ???
    case Error(loc) => ???
  }
}