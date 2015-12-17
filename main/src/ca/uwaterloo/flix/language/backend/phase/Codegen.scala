package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.{Name, UnaryOperator, BinaryOperator, ComparisonOperator}
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.{Definition, Expression, Type}
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.Expression._

import org.objectweb.asm.{ClassVisitor, ClassWriter, MethodVisitor, Label}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter

object Codegen {

  case class Context(definitions: List[Definition], clazz: String = "ca/uwaterloo/flix/compiled/FlixDefinitions") {
    val functions = definitions.collect { case f: Definition.Function => f }
    val getFunction = functions.map { f => (f.name, f) }.toMap
  }

  /**
   * Decorate (mangle) a Name.Resolved to get the internal JVM name.
   */
  def decorate(name: Name.Resolved): String = name.parts.mkString("$")

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
   * Generate the constructor. Takes a Context and a ClassVisitor (that has already been initialized).
   */
  private def compileConstructor(context: Context, visitor: ClassVisitor): Unit = {
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
   * Takes a Context and a ClassVisitor (that has already been initialized).
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  private def compileFunction(context: Context, visitor: ClassVisitor)(function: Definition.Function): Unit = {
    // TODO: Debug information
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, decorate(function.name), function.descriptor, null, null)
    mv.visitCode()

    // Compile the method body
    compileExpression(context, mv)(function.body)

    // Return the value. No integer casts since bool is returned as int, and Flix treats Int8, Int16, and Int32 as int.
    function.tpe.retTpe match {
      case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 => mv.visitInsn(IRETURN)
      case Type.Int64 => mv.visitInsn(LRETURN)
      case Type.Tag(name, ident, tpe) => ???
      case Type.Enum(cases) => ???
      case Type.Tuple(elms) => ???
      case Type.Set(elmType) => ???
      case Type.Lambda(args, retTpe) => ???
    }

    // Dummy large numbers so the bytecode checker can run. Afterwards, the library calculates the proper maxes.
    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def compileExpression(context: Context, visitor: MethodVisitor)(expr: Expression): Unit = expr match {
    case LoadBool(exp, offset) => ???
    case LoadInt8(exp, offset) => ???
    case LoadInt16(exp, offset) => ???
    case LoadInt32(exp, offset) => ???
    case StoreBool(exp, offset, v) => ???
    case StoreInt8(exp, offset, v) => ???
    case StoreInt16(exp, offset, v) => ???
    case StoreInt32(exp, offset, v) => ???

    case Const(i, tpe, loc) => compileConst(visitor)(i)
    case Var(v, tpe, loc) => visitor.visitVarInsn(ILOAD, v.offset)
    case Apply(name, args, tpe, loc) =>
      args.foreach(compileExpression(context, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, context.clazz, decorate(name), context.getFunction(name).descriptor, false)
    case Let(v, exp1, exp2, tpe, loc) =>
      compileExpression(context, visitor)(exp1)
      visitor.visitVarInsn(ISTORE, v.offset)
      compileExpression(context, visitor)(exp2)
    case Unary(op, exp, tpe, loc) => compileUnaryExpression(context, visitor)(op, exp)
    case Binary(op, exp1, exp2, tpe, loc) => compileBinaryExpression(context, visitor)(op, exp1, exp2)
    case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(context, visitor)(exp1)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(context, visitor)(exp2)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(context, visitor)(exp3)
      visitor.visitLabel(ifEnd)
    case Tag(name, tag, exp, tpe, loc) => ???
    case TagOf(exp, name, tag, tpe, loc) => ???
    case Tuple(elms, tpe, loc) => ???
    case TupleAt(base, offset, tpe, loc) => ???
    case Set(elms, tpe, loc) => ???
    case Error(loc) => ???
  }

  /*
   * Generate code to load a constant. Uses the minimal number of instructions necessary.
   */
  private def compileConst(visitor: MethodVisitor)(i: Long): Unit = i match {
    case -1 => visitor.visitInsn(ICONST_M1)
    case 0 => visitor.visitInsn(ICONST_0)
    case 1 => visitor.visitInsn(ICONST_1)
    case 2 => visitor.visitInsn(ICONST_2)
    case 3 => visitor.visitInsn(ICONST_3)
    case 4 => visitor.visitInsn(ICONST_4)
    case 5 => visitor.visitInsn(ICONST_5)
    case _ if Byte.MinValue <= i && i <= Byte.MaxValue => visitor.visitIntInsn(BIPUSH, i.toInt)
    case _ if Short.MinValue <= i && i <= Short.MaxValue => visitor.visitIntInsn(SIPUSH, i.toInt)
    case _ if Int.MinValue <= i && i <= Int.MaxValue => visitor.visitLdcInsn(i.toInt)
    case _ => visitor.visitLdcInsn(i)
  }

  private def compileUnaryExpression(context: Context, visitor: MethodVisitor)(op: UnaryOperator, expr: Expression): Unit = {
    compileExpression(context, visitor)(expr)
    op match {
      case UnaryOperator.Not =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNE, condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)
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
  }

  // Binary operations And and Or are handled first because of short-circuit evaluation
  private def compileBinaryExpression(context: Context, visitor: MethodVisitor)(op: BinaryOperator, expr1: Expression, expr2: Expression): Unit = op match {
    case BinaryOperator.And =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(context, visitor)(expr1)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(context, visitor)(expr2)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, andEnd)
      visitor.visitLabel(andFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(andEnd)
    case BinaryOperator.Or =>
      val orTrueBranch = new Label()
      val orFalseBranch = new Label()
      val orEnd = new Label()
      compileExpression(context, visitor)(expr1)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(context, visitor)(expr2)
      visitor.visitJumpInsn(IFEQ, orFalseBranch)
      visitor.visitLabel(orTrueBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, orEnd)
      visitor.visitLabel(orFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(orEnd)
    case _ =>
      compileExpression(context, visitor)(expr1)
      compileExpression(context, visitor)(expr2)
      op match {
        case BinaryOperator.Plus => visitor.visitInsn(IADD)
        case BinaryOperator.Minus => visitor.visitInsn(ISUB)
        case BinaryOperator.Times => visitor.visitInsn(IMUL)
        case BinaryOperator.Divide => visitor.visitInsn(IDIV)
        case BinaryOperator.Modulo => visitor.visitInsn(IREM)

        case o: ComparisonOperator =>
          val condElse = new Label()
          val condEnd = new Label()
          val cmp = o match {
            case BinaryOperator.Less => IF_ICMPGE
            case BinaryOperator.LessEqual => IF_ICMPGT
            case BinaryOperator.Greater => IF_ICMPLE
            case BinaryOperator.GreaterEqual => IF_ICMPLT
            case BinaryOperator.Equal => IF_ICMPNE
            case BinaryOperator.NotEqual => IF_ICMPEQ
          }
          visitor.visitJumpInsn(cmp, condElse)
          visitor.visitInsn(ICONST_1)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitLabel(condEnd)

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

        case BinaryOperator.And | BinaryOperator.Or =>
          throw new InternalCompilerError("BinaryOperator.And and BinaryOperator.Or should already have been handled.")
      }
  }
}
