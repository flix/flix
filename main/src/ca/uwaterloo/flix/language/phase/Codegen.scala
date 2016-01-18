package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.ReducedIR.Expression._
import ca.uwaterloo.flix.language.ast.ReducedIR.{Definition, Expression, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm.{ClassVisitor, ClassWriter, Label, MethodVisitor}

object Codegen {

  case class Context(definitions: List[Definition], clazz: String = "ca/uwaterloo/flix/compiled/FlixDefinitions") {
    val functions = definitions.collect { case f: Definition.Function => f }
    val getFunction = functions.map { f => (f.name, f) }.toMap
  }

  /*
   * Decorate (mangle) a Name.Resolved to get the internal JVM name.
   */
  def decorate(name: Name.Resolved): String = name.parts.mkString("$")

  /*
   * Returns the internal name of the JVM type that `tpe` maps to.
   */
  def descriptor(tpe: Type): String = tpe match {
    case Type.Var(x) => ???
    case Type.Unit => ???
    case Type.Bool => "Z"   // JVM boolean
    case Type.Int8 => "B"   // JVM byte (8 bits)
    case Type.Int16 => "S"  // JVM short (16 bits)
    case Type.Int32 | Type.Int => "I"  // JVM int (32 bits)
    case Type.Int64 => "J"  // JVM long (64 bits)
    case Type.Str => ???
    case Type.Tag(enum, tag, t) => ???
    case Type.Enum(cases) => ???
    case Type.Tuple(elms) => ???
    case Type.Opt(elmType) => ???
    case Type.Lst(elmType) => ???
    case Type.Set(elmType) => ???
    case Type.Map(k, v) => ???
    case Type.Lambda(args, retTpe) => s"""(${ args.map(descriptor).mkString })${descriptor(retTpe)}"""
    case Type.Predicate(terms) => ???
    case Type.Native(name) => ???
    case Type.Char => ???
    case Type.Abs(name, t) => ???
    case Type.Any => ???
  }

  /*
   * Given a list of Flix definitions, compile the definitions to bytecode and put them in a JVM class. For now, we put
   * all definitions in a single class: ca.uwaterloo.flix.runtime.compiled.FlixDefinitions. The Flix function
   * A::B::C::foo is compiled as the method A$B$C$foo.
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
   * Generate the constructor. Takes a Context and an initialized ClassVisitor.
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
   * Takes a Context and an initialized ClassVisitor.
   * The Flix function A::B::C::foo is compiled as the method A$B$C$foo.
   */
  private def compileFunction(context: Context, visitor: ClassVisitor)(function: Definition.Function): Unit = {
    // TODO: Debug information
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, decorate(function.name), function.tpe.descriptor, null, null)
    mv.visitCode()

    compileExpression(context, mv)(function.body)

    function.tpe.retTpe match {
      case ReducedIR.Type.Bool | ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 => mv.visitInsn(IRETURN)
      case ReducedIR.Type.Int64 => mv.visitInsn(LRETURN)
      case ReducedIR.Type.Tag(name, ident, tpe) => ???
      case ReducedIR.Type.Enum(cases) => ???
      case ReducedIR.Type.Tuple(elms) => ???
      case ReducedIR.Type.Set(elmType) => ???
      case ReducedIR.Type.Lambda(args, retTpe) => ???
    }

    // Dummy large numbers so the bytecode checker can run. Afterwards, the ASM library calculates the proper maxes.
    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def compileExpression(context: Context, visitor: MethodVisitor)(expr: Expression): Unit = expr match {
    case load: LoadExpression => compileLoadExpr(context, visitor)(load)
    case store: StoreExpression => compileStoreExpr(context, visitor)(store)
    case Const(i, ReducedIR.Type.Int64, loc) => compileConst(visitor)(i, isLong = true)
    case Const(i, tpe, loc) => compileConst(visitor)(i)
    case Var(v, tpe, loc) =>
      tpe match {
        case ReducedIR.Type.Bool | ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 => visitor.visitVarInsn(ILOAD, v.offset)
        case ReducedIR.Type.Int64 => visitor.visitVarInsn(LLOAD, v.offset)
        case ReducedIR.Type.Tag(name, ident, typ) => ???
        case ReducedIR.Type.Enum(cases) => ???
        case ReducedIR.Type.Tuple(elms) => ???
        case ReducedIR.Type.Set(elmType) => ???
        case ReducedIR.Type.Lambda(args, retTpe) => ???
      }
    case Apply(name, args, tpe, loc) =>
      args.foreach(compileExpression(context, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, context.clazz, decorate(name), context.getFunction(name).tpe.descriptor, false)
    case Let(v, exp1, exp2, tpe, loc) =>
      compileExpression(context, visitor)(exp1)
      exp1.tpe match {
        case ReducedIR.Type.Bool | ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 => visitor.visitVarInsn(ISTORE, v.offset)
        case ReducedIR.Type.Int64 => visitor.visitVarInsn(LSTORE, v.offset)
        case ReducedIR.Type.Tag(name, ident, typ) => ???
        case ReducedIR.Type.Enum(cases) => ???
        case ReducedIR.Type.Tuple(elms) => ???
        case ReducedIR.Type.Set(elmType) => ???
        case ReducedIR.Type.Lambda(args, retTpe) => ???
      }
      compileExpression(context, visitor)(exp2)
    case Unary(op, exp, tpe, loc) => compileUnaryExpr(context, visitor)(op, exp, tpe)
    case Binary(op, exp1, exp2, tpe, loc) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(context, visitor)(o, exp1, exp2, tpe)
      case o: ComparisonOperator => compileComparisonExpr(context, visitor)(o, exp1, exp2, tpe)
      case o: LogicalOperator => compileLogicalExpr(context, visitor)(o, exp1, exp2, tpe)
      case o: BitwiseOperator => compileBitwiseExpr(context, visitor)(o, exp1, exp2, tpe)
    }
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
    case Error(tpe, loc) => ???
  }

  /*
   * (e >> offset).toInt & mask
   *
   * Example:
   * x represents a bit with unknown value (0 or 1)
   *   load   = LoadInt8(e, 16)
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 10101010 xxxxxxxx xxxxxxxx
   *
   * First we do a right shift (with sign extension) (LSHR):
   *            xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 10101010
   * Then we convert/truncate to an int, discarding the higher-order bits (L2I):
   *            xxxxxxxx xxxxxxxx xxxxxxxx 10101010
   * We bitwise-and with the mask, clearing the higher-order bits (IAND):
   *   mask   = 00000000 00000000 00000000 11111111
   *   result = 00000000 00000000 00000000 10101010
   *
   * If we used I2B instead of a mask, sign extension would give:
   *            11111111 11111111 11111111 10101010
   */
  private def compileLoadExpr(context: Context, visitor: MethodVisitor)(load: LoadExpression): Unit = {
    compileExpression(context, visitor)(load.e)
    if (load.offset > 0) {
      compileConst(visitor)(load.offset)
      visitor.visitInsn(LSHR)
    }
    visitor.visitInsn(L2I)
    compileConst(visitor)(load.mask)
    visitor.visitInsn(IAND)
  }

  /*
   * (e & targetMask) | ((v.toLong & mask) << offset)
   *
   * Example:
   * x represents a bit with unknown value (0 or 1)
   *   store  = StoreInt32(e, 0, v)
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
   *   v      = 11110000 11110000 11110000 11110000
   *
   * First we bitwise-and with targetMask to clear target/destination bits (LAND):
   *   tMask  = 11111111 11111111 11111111 11111111 00000000 00000000 00000000 00000000
   *   result = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   * Then we convert v to a long (with sign extension) (I2L):
   *            11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   * Then we bitwise-and with the mask, clearing the higher-order bits (LAND):
   *   mask   = 00000000 00000000 00000000 00000000 11111111 11111111 11111111 11111111
   *   result = 00000000 00000000 00000000 00000000 11110000 11110000 11110000 11110000
   * In this example, we don't left shift because the shift offset is 0 (LSHL). We bitwise-or with e to get the final
   * result (LOR):
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   *   result = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 11110000 11110000 11110000 11110000
   *
   * Note: (v & mask).toLong instead of (v.toLong & mask) gives the wrong result because of sign extension.
   * Bitwise-and of v and mask (IAND):
   *   mask   = 11111111 11111111 11111111 11111111
   *   result = 11110000 11110000 11110000 11110000
   * Convert int to long (doing a sign extension) (I2L):
   *   result = 11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   * Again in this example, we don't do a left shift. But when we do a bitwise-or, we overwrite the bits of e (LOR):
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   *   result = 11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   */
  private def compileStoreExpr(context: Context, visitor: MethodVisitor)(store: StoreExpression): Unit = {
    compileExpression(context, visitor)(store.e)
    compileConst(visitor)(store.targetMask, isLong = true)
    visitor.visitInsn(LAND)
    compileExpression(context, visitor)(store.v)
    visitor.visitInsn(I2L)
    compileConst(visitor)(store.mask, isLong = true)
    visitor.visitInsn(LAND)
    if (store.offset > 0) {
      compileConst(visitor)(store.offset)
      visitor.visitInsn(LSHL)
    }
    visitor.visitInsn(LOR)
  }

  /*
   * Generate code to load a constant.
   *
   * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
   * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
   * bytes. The exception is if we set `isLong` to true, in which case a cast will be performed if necessary.
   *
   * This is needed because sometimes we expect the operands to be a long, which means two (int) values are popped from
   * the stack and concatenated to form a long.
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
    if (isLong && Int.MinValue <= i && i <= Int.MaxValue && i != 0 && i != 1) visitor.visitInsn(I2L)
  }

  private def compileUnaryExpr(context: Context, visitor: MethodVisitor)
                              (op: UnaryOperator, expr: Expression, tpe: ReducedIR.Type): Unit = {
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
      case UnaryOperator.Minus => compileUnaryMinusExpr(context, visitor)(tpe)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(context, visitor)(tpe)
    }
  }

  /*
   * For Int8/Int16, we need to truncate and sign extend the result.
   *
   * Example:
   * Suppose we store the value -128 into an Int8 (byte). The number is represented as (in two's complement):
   *   10000000
   * But on the JVM, the value is sign extended and stored as an Int32 (int):
   *   11111111 11111111 11111111 10000000
   * If we simply negate -128, we get the value 128, which is represented as:
   *   00000000 00000000 00000000 10000000
   * But this is greater than the maximum value (127) for an Int8 (byte). We use I2B to convert the Int32 (int) to an
   * Int8 (byte), which does a truncation and sign extension:
   *   11111111 11111111 11111111 10000000
   * And the final value is -128.
   *
   * Note that in Java semantics, the unary minus operator returns an Int32 (int), so the programmer must explicitly
   * cast to an Int8 (byte).
   */
  private def compileUnaryMinusExpr(context: Context, visitor: MethodVisitor)(tpe: ReducedIR.Type): Unit = tpe match {
    case ReducedIR.Type.Int8 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case ReducedIR.Type.Int16 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case ReducedIR.Type.Int32 => visitor.visitInsn(INEG)
    case ReducedIR.Type.Int64 => visitor.visitInsn(LNEG)
    case ReducedIR.Type.Bool | ReducedIR.Type.Tag(_, _, _) | ReducedIR.Type.Enum(_) | ReducedIR.Type.Tuple(_) | ReducedIR.Type.Set(_) | ReducedIR.Type.Lambda(_, _) =>
      throw new InternalCompilerError(s"Can't apply UnaryOperator.Minus to type $tpe.")
  }

  /*
   * Note that ~xxxx = xxxx ^ 1111, and since the JVM uses two's complement, -1 = 0xFFFFFFFF, so ~b = b ^ -1. No need to
   * truncate because Int8/Int16 (byte/short) are sign extended to Int32 (int), and s.ext(negate(b) = negate(s.ext(b)).
   *
   * Example:
   * Consider two Int8s:
   *     b = 11000011    c = 00001111
   * Conceptually, ~b and ~c would be:
   *    ~b = 00111100   ~c = 11110000
   * On the JVM, b, ~b, c, and ~c would be stored as an Int32s:
   *    b' = 11111111 11111111 11111111 11000011    c' = 00000000 00000000 00000000 00001111
   *   ~b' = 00000000 00000000 00000000 00111100   ~c' = 11111111 11111111 11111111 11110000
   *
   * Note that sign extending and then negating a value is equal to negating and then sign extending it.
   */
  private def compileUnaryNegateExpr(context: Context, visitor: MethodVisitor)(tpe: ReducedIR.Type): Unit = {
    visitor.visitInsn(ICONST_M1)
    tpe match {
      case ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 =>
        visitor.visitInsn(IXOR)
      case ReducedIR.Type.Int64 =>
        visitor.visitInsn(I2L)
        visitor.visitInsn(LXOR)
      case ReducedIR.Type.Bool | ReducedIR.Type.Tag(_, _, _) | ReducedIR.Type.Enum(_) | ReducedIR.Type.Tuple(_) | ReducedIR.Type.Set(_) | ReducedIR.Type.Lambda(_, _) =>
        throw new InternalCompilerError(s"Can't apply UnaryOperator.Negate to type $tpe.")
    }
  }

  /*
   * Results are truncated (and sign extended), so that adding two IntN's will always return an IntN. Overflow can
   * occur. Note that in Java semantics, the result of an arithmetic operation is an Int32 (int) or an Int64 (long), and
   * the user must explicitly downcast to an Int8 (byte) or Int16 (short).
   *
   * Example:
   * Consider adding two Int8s (bytes), 127 and 1. The result overflows:
   *     01111111 =  127
   *   + 00000001 =    1
   * --------------------
   *     10000000 = -128
   * However, on the JVM, Int8s (bytes) are represented as Int32s (ints). The result of an arithmetic operation is an
   * Int32 (int), and there is no overflow (in this case):
   *     00000000 00000000 00000000 01111111 =  127
   *   + 00000000 00000000 00000000 00000001 =    1
   * -----------------------------------------------
   *     00000000 00000000 00000000 10000000 =  128
   * We want the value to be an Int8 (byte), so we use I2B to truncate and sign extend:
   *     11111111 11111111 11111111 10000000 = -128
   */
  private def compileArithmeticExpr(context: Context, visitor: MethodVisitor)
                                   (o: ArithmeticOperator, e1: Expression, e2: Expression, tpe: ReducedIR.Type): Unit = {
    compileExpression(context, visitor)(e1)
    compileExpression(context, visitor)(e2)
    val (intOp, longOp) = o match {
      case BinaryOperator.Plus => (IADD, LADD)
      case BinaryOperator.Minus => (ISUB, LSUB)
      case BinaryOperator.Times => (IMUL, LMUL)
      case BinaryOperator.Divide => (IDIV, LDIV)
      case BinaryOperator.Modulo => (IREM, LREM)
    }
    tpe match {
      case ReducedIR.Type.Int8 =>
        visitor.visitInsn(intOp)
        visitor.visitInsn(I2B)
      case ReducedIR.Type.Int16 =>
        visitor.visitInsn(intOp)
        visitor.visitInsn(I2S)
      case ReducedIR.Type.Int32 => visitor.visitInsn(intOp)
      case ReducedIR.Type.Int64 => visitor.visitInsn(longOp)
      case ReducedIR.Type.Bool | ReducedIR.Type.Tag(_, _, _) | ReducedIR.Type.Enum(_) | ReducedIR.Type.Tuple(_) | ReducedIR.Type.Set(_) | ReducedIR.Type.Lambda(_, _) =>
        throw new InternalCompilerError(s"Can't apply $o to type $tpe.")
    }
  }

  private def compileComparisonExpr(context: Context, visitor: MethodVisitor)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression, tpe: ReducedIR.Type): Unit = {
    compileExpression(context, visitor)(e1)
    compileExpression(context, visitor)(e2)
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
    e1.tpe match {
      case ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 => visitor.visitJumpInsn(intOp, condElse)
      case ReducedIR.Type.Int64 =>
        visitor.visitInsn(LCMP)
        visitor.visitJumpInsn(longOp, condElse)
      case ReducedIR.Type.Bool | ReducedIR.Type.Tag(_, _, _) | ReducedIR.Type.Enum(_) | ReducedIR.Type.Tuple(_) | ReducedIR.Type.Set(_) | ReducedIR.Type.Lambda(_, _) =>
        throw new InternalCompilerError(s"Can't apply $o to type $tpe.")
    }
    visitor.visitInsn(ICONST_1)
    visitor.visitJumpInsn(GOTO, condEnd)
    visitor.visitLabel(condElse)
    visitor.visitInsn(ICONST_0)
    visitor.visitLabel(condEnd)
  }

  /*
   * Note that these expressions do short-circuit evaluation.
   */
  private def compileLogicalExpr(context: Context, visitor: MethodVisitor)
                                (o: LogicalOperator, e1: Expression, e2: Expression, tpe: ReducedIR.Type): Unit = o match {
    case BinaryOperator.And =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(context, visitor)(e1)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(context, visitor)(e2)
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
      compileExpression(context, visitor)(e1)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(context, visitor)(e2)
      visitor.visitJumpInsn(IFEQ, orFalseBranch)
      visitor.visitLabel(orTrueBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, orEnd)
      visitor.visitLabel(orFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(orEnd)
  }

  /*
   * In general we don't do any truncation, because it doesn't matter what the higher-order bits are.
   *
   * Example:
   * Consider the bitwise-and of the following Int8s:
   *     11110000             00000011
   *   & 11000000           & 11001111
   * -------------        -------------
   *     11000000             00000011
   * On the JVM, these Int8s (bytes) would be represented as Int32s (ints):
   *    11111111 11111111 11111111 11110000        00000000 00000000 00000000 00000011
   *  & 11111111 11111111 11111111 11000000      & 00000000 00000000 00000000 11001111
   * ---------------------------------------    ---------------------------------------
   *    11111111 11111111 11111111 11000000        00000000 00000000 00000000 00000011
   *
   * As with Unary.Negate, sign extension before or after the operation yields the same result.
   *
   *
   * The exception is with bitwise left shifts. The higher-order bits matter because we might sign extend.
   *
   * Example:
   * Consider the following left shift, where x and y each represent unknown values (0 or 1):
   *   x000y000 << 4 = y0000000
   * But because Int8s (bytes) are represented as Int32s (ints), and the x is sign extended, we get:
   *   xxxxxxxx xxxxxxxx xxxxxxxx x000y000 << 4 = xxxxxxxx xxxxxxxx xxxxx000 y0000000
   * We truncate and sign extend (I2B), which gives:
   *   yyyyyyyy yyyyyyyy yyyyyyyy y0000000
   *
   * It doesn't matter that we left shifted x, because we (generally) ignore the higher-order bits. However, it *does*
   * matter that we shifted y into the sign bit of an Int8. If y = 1, then the Int8 (byte) 10000000 has value -128,
   * which needs to be sign extended to represent that value as an Int32 (int).
   *
   * Example:
   * Consider the following (signed) right shift, where x represents an unknown value (0 or 1):
   *   x0000000 >> 4 = xxxxx000
   * These Int8s (bytes) are represented as Int32s (ints), so the x is sign extended:
   *   xxxxxxxx xxxxxxxx xxxxxxxx x0000000 >> 4 = xxxxxxxx xxxxxxxx xxxxxxxx xxxxx000
   *
   * We don't need to truncate, because it is impossible for random data to be in the higher-order bits. Either those
   * bits are all 0, or they are 1 (because of sign extension).
   *
   * Note: the right-hand operand of a shift (i.e. the shift amount) *must* be Int32.
   */
  private def compileBitwiseExpr(context: Context, visitor: MethodVisitor)
                                (o: BitwiseOperator, e1: Expression, e2: Expression, tpe: ReducedIR.Type): Unit = {
    compileExpression(context, visitor)(e1)
    compileExpression(context, visitor)(e2)
    val (intOp, longOp) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND)
      case BinaryOperator.BitwiseOr => (IOR, LOR)
      case BinaryOperator.BitwiseXor => (IXOR, LXOR)
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL)
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR)
    }
    tpe match {
      case ReducedIR.Type.Int8 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2B)
      case ReducedIR.Type.Int16 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2S)
      case ReducedIR.Type.Int8 | ReducedIR.Type.Int16 | ReducedIR.Type.Int32 => visitor.visitInsn(intOp)
      case ReducedIR.Type.Int64 => visitor.visitInsn(longOp)
      case ReducedIR.Type.Bool | ReducedIR.Type.Tag(_, _, _) | ReducedIR.Type.Enum(_) | ReducedIR.Type.Tuple(_) | ReducedIR.Type.Set(_) | ReducedIR.Type.Lambda(_, _) =>
        throw new InternalCompilerError(s"Can't apply $o to type $tpe.")
    }
  }

}
