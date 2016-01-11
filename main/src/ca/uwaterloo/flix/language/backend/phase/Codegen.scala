package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.backend.ir.ReducedIR.{LoadExpression, StoreExpression, Definition, Expression, Type}
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

    compileConstructor(context, visitor)
    functions.foreach(compileFunction(context, visitor))

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

    compileExpression(context, mv)(function.body)

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
    case load: LoadExpression =>
      // (e >> offset).toInt & mask
      compileExpression(context, visitor)(load.e)
      if (load.offset > 0) {
        compileConst(visitor)(load.offset)
        visitor.visitInsn(LSHR)
      }
      visitor.visitInsn(L2I)
      compileConst(visitor)(load.mask)
      visitor.visitInsn(IAND)
    case store: StoreExpression =>
      // (e & mask') | ((v.toLong & 0xFFFFFFFFL) << offset) where mask' = ~(mask << offset)
      // Note that toLong does a sign extension which we need to mask out.
      compileExpression(context, visitor)(store.e)
      compileConst(visitor)(store.mask, isLong = true)
      visitor.visitInsn(LAND)
      compileExpression(context, visitor)(store.v)
      visitor.visitInsn(I2L)
      compileConst(visitor)(0xFFFFFFFFL, isLong = true)
      visitor.visitInsn(LAND)
      if (store.offset > 0) {
        compileConst(visitor)(store.offset)
        visitor.visitInsn(LSHL)
      }
      visitor.visitInsn(LOR)
    case Const(i, Type.Int64, loc) => compileConst(visitor)(i, isLong = true)
    case Const(i, tpe, loc) => compileConst(visitor)(i)
    case Var(v, tpe, loc) =>
      tpe match {
        case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ILOAD, v.offset)
        case Type.Int64 => visitor.visitVarInsn(LLOAD, v.offset)
        case Type.Tag(name, ident, typ) => ???
        case Type.Enum(cases) => ???
        case Type.Tuple(elms) => ???
        case Type.Set(elmType) => ???
        case Type.Lambda(args, retTpe) => ???
      }
    case Apply(name, args, tpe, loc) =>
      args.foreach(compileExpression(context, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, context.clazz, decorate(name), context.getFunction(name).descriptor, false)
    case Let(v, exp1, exp2, tpe, loc) =>
      compileExpression(context, visitor)(exp1)
      exp1.tpe match {
        case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ISTORE, v.offset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, v.offset)
        case Type.Tag(name, ident, typ) => ???
        case Type.Enum(cases) => ???
        case Type.Tuple(elms) => ???
        case Type.Set(elmType) => ???
        case Type.Lambda(args, retTpe) => ???
      }
      compileExpression(context, visitor)(exp2)
    case Unary(op, exp, tpe, loc) => compileUnaryExpression(context, visitor)(op, exp, tpe)
    case Binary(op, exp1, exp2, tpe, loc) => compileBinaryExpression(context, visitor)(op, exp1, exp2, tpe)
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
    case Error(loc, tpe) => ???
  }

  /*
   * Generate code to load a constant.
   *
   * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
   * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
   * bytes. The exception is if we set `isLong` to true, in which case a cast will be performed if necessary.
   *
   * This is needed because sometimes we need the operands to be a long and two (int) values are popped from the
   * stack (and concatenated to form a long).
   */
  private def compileConst(visitor: MethodVisitor)(i: Long, isLong: Boolean = false): Unit = {
    i match {
      case -1 => visitor.visitInsn(ICONST_M1)
      case 0 => if (!isLong) visitor.visitInsn(ICONST_0) else visitor.visitInsn(LCONST_0)
      case 1 => if (!isLong) visitor.visitInsn(ICONST_1) else visitor.visitInsn(LCONST_1)
      case 2 => visitor.visitInsn(ICONST_2)
      case 3 => visitor.visitInsn(ICONST_3)
      case 4 => visitor.visitInsn(ICONST_4)
      case 5 => visitor.visitInsn(ICONST_5)
      case _ if Byte.MinValue <= i && i <= Byte.MaxValue => visitor.visitIntInsn(BIPUSH, i.toInt)
      case _ if Short.MinValue <= i && i <= Short.MaxValue => visitor.visitIntInsn(SIPUSH, i.toInt)
      case _ if Int.MinValue <= i && i <= Int.MaxValue => visitor.visitLdcInsn(i.toInt)
      case _ => visitor.visitLdcInsn(i)
    }
    if (Int.MinValue <= i && i <= Int.MaxValue && i != 0 && i != 1 && isLong) visitor.visitInsn(I2L)
  }

  private def compileUnaryExpression(context: Context, visitor: MethodVisitor)(op: UnaryOperator, expr: Expression, tpe: Type): Unit = {
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
      case UnaryOperator.Plus => // nop
      case UnaryOperator.Minus =>
        // For Int8 and Int16, we need to truncate (and sign-extend) the result.
        tpe match {
          case Type.Int8 =>
            visitor.visitInsn(INEG)
            visitor.visitInsn(I2B)
          case Type.Int16 =>
            visitor.visitInsn(INEG)
            visitor.visitInsn(I2S)
          case Type.Int32 => visitor.visitInsn(INEG)
          case Type.Int64 => visitor.visitInsn(LNEG)
          case Type.Bool | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) =>
            throw new InternalCompilerError(s"Can't apply $op to type $tpe.")
        }
      case UnaryOperator.Negate =>
        // Note that ~bbbb = bbbb ^ 1111, and since the JVM uses two's complement, -1 = 0xFFFFFFFF, so ~x = x ^ -1.
        // No need to truncate because Int8/Int16 are sign-extended to Int32, and s.ext(negate(X) = negate(s.ext(X)).
        visitor.visitInsn(ICONST_M1)
        tpe match {
          case Type.Int8 | Type.Int16 | Type.Int32 =>
            visitor.visitInsn(IXOR)
          case Type.Int64 =>
            visitor.visitInsn(I2L)
            visitor.visitInsn(LXOR)
          case Type.Bool | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) =>
            throw new InternalCompilerError(s"Can't apply $op to type $tpe.")
        }

      case UnaryOperator.Set.IsEmpty => ???
      case UnaryOperator.Set.NonEmpty => ???
      case UnaryOperator.Set.Singleton => ???
      case UnaryOperator.Set.Size => ???
    }
  }

  // Binary operations And and Or are handled first because of short-circuit evaluation
  private def compileBinaryExpression(context: Context, visitor: MethodVisitor)(op: BinaryOperator, expr1: Expression, expr2: Expression, tpe: Type): Unit = op match {
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
        case o: ArithmeticOperator =>
          // Results are truncated, so that adding two IntN's will always return an IntN. Overflow can occur.
          val (intOp, longOp) = o match {
            case BinaryOperator.Plus => (IADD, LADD)
            case BinaryOperator.Minus => (ISUB, LSUB)
            case BinaryOperator.Times => (IMUL, LMUL)
            case BinaryOperator.Divide => (IDIV, LDIV)
            case BinaryOperator.Modulo => (IREM, LREM)
          }
          tpe match {
            case Type.Int8 =>
              visitor.visitInsn(intOp)
              visitor.visitInsn(I2B)
            case Type.Int16 =>
              visitor.visitInsn(intOp)
              visitor.visitInsn(I2S)
            case Type.Int32 =>
              visitor.visitInsn(intOp)
            case Type.Int64 =>
              visitor.visitInsn(longOp)
            case Type.Bool | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) =>
              throw new InternalCompilerError(s"Can't apply $op to type $tpe.")
          }
        case o: ComparisonOperator =>
          val condElse = new Label()
          val condEnd = new Label()
          val (intOp, longOp) = o match {
            case BinaryOperator.Less => (IF_ICMPGE, IFGE)
            case BinaryOperator.LessEqual => (IF_ICMPGT, IFGT)
            case BinaryOperator.Greater => (IF_ICMPLE, IFLE)
            case BinaryOperator.GreaterEqual => (IF_ICMPLT, IFLT)
            case BinaryOperator.Equal => (IF_ICMPNE, IFNE)
            case BinaryOperator.NotEqual => (IF_ICMPEQ, IFEQ)
          }
          expr1.tpe match {
            case Type.Int8 | Type.Int16 | Type.Int32 =>
              visitor.visitJumpInsn(intOp, condElse)
            case Type.Int64 =>
              visitor.visitInsn(LCMP)
              visitor.visitJumpInsn(longOp, condElse)
            case Type.Bool | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) =>
              throw new InternalCompilerError(s"Can't apply $op to type $tpe.")
          }
          visitor.visitInsn(ICONST_1)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitLabel(condEnd)
        case o: BitwiseOperator =>
          // Don't need to truncate because there are no higher-order bits.
          // An Int8 has the upper 24 bits set to zero, so when you AND two Int8s, the upper 24 bits are unchanged.
          // Exception: we *do* want to truncate after bitwise shifts.
          // TODO: Consider creating and handling BitwiseShiftOperator separately?
          // Note: the right-hand operand of a shift (i.e. the shift amount) *must* be Int32.
          val (intOp, longOp) = o match {
            case BinaryOperator.BitwiseAnd => (IAND, LAND)
            case BinaryOperator.BitwiseOr => (IOR, LOR)
            case BinaryOperator.BitwiseXor => (IXOR, LXOR)
            case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL)
            case BinaryOperator.BitwiseRightShift => (ISHR, LSHR)
          }
          tpe match {
            case Type.Int8 =>
              visitor.visitInsn(intOp)
              if (intOp == ISHL || intOp == ISHR) {
                compileConst(visitor)(0xFF)
                visitor.visitInsn(IAND)
              }
            case Type.Int16 =>
              visitor.visitInsn(intOp)
              if (intOp == ISHL || intOp == ISHR) {
                compileConst(visitor)(0xFFFF)
                visitor.visitInsn(IAND)
              }
            case Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitInsn(intOp)
            case Type.Int64 => visitor.visitInsn(longOp)
            case Type.Bool | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) =>
              throw new InternalCompilerError(s"Can't apply $op to type $tpe.")
          }

        case BinaryOperator.Set.Member => ???
        case BinaryOperator.Set.SubsetOf => ???
        case BinaryOperator.Set.ProperSubsetOf => ???
        case BinaryOperator.Set.Insert => ???
        case BinaryOperator.Set.Remove => ???
        case BinaryOperator.Set.Union => ???
        case BinaryOperator.Set.Intersection => ???
        case BinaryOperator.Set.Difference => ???

        case BinaryOperator.And | BinaryOperator.Or =>
          throw new InternalCompilerError(s"$op should already have been handled.")
      }
  }
}
