package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Definition, Expression, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.Value
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm.{ClassVisitor, ClassWriter, Label, MethodVisitor}

// TODO: For now, we hardcode the type descriptors for all the Value objects
// There's no nice way of using reflection to get the type of a companion object.
// Later, we'll rewrite Value in a Java-like style so reflection is easier

// TODO: Debugging information

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
    case Type.Unit => "Lca/uwaterloo/flix/runtime/Value$Unit$;"
    case Type.Bool => asm.Type.BOOLEAN_TYPE.getDescriptor
    case Type.Int8 => asm.Type.BYTE_TYPE.getDescriptor
    case Type.Int16 => asm.Type.SHORT_TYPE.getDescriptor
    case Type.Int32 | Type.Int32 => asm.Type.INT_TYPE.getDescriptor
    case Type.Int64 => asm.Type.LONG_TYPE.getDescriptor
    case Type.Str => asm.Type.getDescriptor(classOf[java.lang.String])
    case Type.Enum(_, _) => asm.Type.getDescriptor(classOf[Value.Tag])
    case Type.Tuple(elms) => asm.Type.getDescriptor(classOf[Value.Tuple])
    case Type.Lambda(args, retTpe) => s"""(${ args.map(descriptor).mkString })${descriptor(retTpe)}"""
    case Type.Tag(_, _, _) => throw new InternalCompilerError(s"No corresponding JVM type for $tpe.")
    case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) | Type.Native |
         Type.Char | Type.Abs(_, _) | Type.Any => ???
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

    // Initialize the visitor to create a class.
    visitor.visit(V1_8, ACC_PUBLIC + ACC_SUPER, context.clazz, null, "java/lang/Object", null)

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
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, decorate(function.name), descriptor(function.tpe), null, null)
    mv.visitCode()

    compileExpression(context, mv)(function.body)

    function.tpe.retTpe match {
      case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 | Type.Int32 => mv.visitInsn(IRETURN)
      case Type.Int64 => mv.visitInsn(LRETURN)
      case Type.Unit | Type.Str | Type.Enum(_, _) | Type.Tuple(_) => mv.visitInsn(ARETURN)
      case Type.Tag(_, _, _) =>
        throw new InternalCompilerError(s"Functions can't return type ${function.tpe.retTpe}.")
      case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any => ???
    }

    // Dummy large numbers so the bytecode checker can run. Afterwards, the ASM library calculates the proper maxes.
    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def compileExpression(context: Context, visitor: MethodVisitor)(expr: Expression): Unit = expr match {
    case Expression.Unit =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$Unit$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$Unit$;")

    case Expression.True => visitor.visitInsn(ICONST_1)
    case Expression.False => visitor.visitInsn(ICONST_0)
    case Expression.Int8(b) => compileInt(visitor)(b)
    case Expression.Int16(s) => compileInt(visitor)(s)
    case Expression.Int32(i) => compileInt(visitor)(i)
    case Expression.Int64(l) => compileInt(visitor)(l, isLong = true)
    case Expression.Str(s) => visitor.visitLdcInsn(s)

    case load: LoadExpression => compileLoadExpr(context, visitor)(load)
    case store: StoreExpression => compileStoreExpr(context, visitor)(store)

    case Expression.Var(ident, offset, tpe, loc) => tpe match {
      case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 | Type.Int32 => visitor.visitVarInsn(ILOAD, offset)
      case Type.Int64 => visitor.visitVarInsn(LLOAD, offset)
      case Type.Unit | Type.Str | Type.Enum(_, _) | Type.Tuple(_) =>
        visitor.visitVarInsn(ALOAD, offset)
      case Type.Tag(_, _, _) => throw new InternalCompilerError(s"Can't have a value of type $tpe.")
      case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any => ???
    }

    case Expression.Ref(name, tpe, loc) => ???
    case Expression.Lambda(annotations, args, body, tpe, loc) => ???

    case Expression.Apply(name, args, _, _) =>
      args.foreach(compileExpression(context, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, context.clazz, decorate(name),
        descriptor(context.getFunction(name).tpe), false)
    case Expression.Apply3(lambda, args, tpe, loc) => ???

    case Expression.Unary(op, exp, _, _) => compileUnaryExpr(context, visitor)(op, exp)
    case Expression.Binary(op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(context, visitor)(o, exp1, exp2)
      case o: ComparisonOperator => compileComparisonExpr(context, visitor)(o, exp1, exp2)
      case o: LogicalOperator => compileLogicalExpr(context, visitor)(o, exp1, exp2)
      case o: BitwiseOperator => compileBitwiseExpr(context, visitor)(o, exp1, exp2)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(context, visitor)(exp1)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(context, visitor)(exp2)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(context, visitor)(exp3)
      visitor.visitLabel(ifEnd)

    case Expression.Let(ident, offset, exp1, exp2, _, _) =>
      compileExpression(context, visitor)(exp1)
      exp1.tpe match {
        case Type.Bool | Type.Int8 | Type.Int16 | Type.Int32 | Type.Int32 => visitor.visitVarInsn(ISTORE, offset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, offset)
        case Type.Unit | Type.Str | Type.Enum(_, _) | Type.Tuple(_) =>
          visitor.visitVarInsn(ASTORE, offset)
        case Type.Tag(_, _, _) => throw new InternalCompilerError(s"Can't have a value of type ${exp1.tpe}.")
        case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
             Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any => ???
      }
      compileExpression(context, visitor)(exp2)

    case Expression.CheckTag(tag, exp, _) =>
      // Get the tag string of `exp` (compiled as a tag) and compare to `tag.name`.
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tag", "tag", "()Ljava/lang/String;",
        false)
      visitor.visitLdcInsn(tag.name)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false)

    case Expression.GetTagValue(exp, tpe, _) =>
      // Compile `exp` as a tag expression, get its inner `value`, and unbox if necessary.
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tag", "value",
        "()Ljava/lang/Object;", false)
      compileUnbox(context, visitor)(tpe)

    case Expression.Tag(enum, tag, exp, _, _) =>
      // Load the Value companion object, then the arguments, and finally call `Value.mkTag`.
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")

      // Load `enum` as a string, by calling `Name.Resolved.mk`
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/Name$Resolved$", "MODULE$",
        "Lca/uwaterloo/flix/language/ast/Name$Resolved$;")
      visitor.visitLdcInsn(enum.parts.mkString("::"))
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/Name$Resolved$", "mk",
        "(Ljava/lang/String;)Lca/uwaterloo/flix/language/ast/Name$Resolved;", false)

      // Load `tag.name` and box `exp` if necessary.
      visitor.visitLdcInsn(tag.name)
      compileBoxedExpr(context, visitor)(exp)

      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkTag",
        "(Lca/uwaterloo/flix/language/ast/Name$Resolved;Ljava/lang/String;Ljava/lang/Object;)Lca/uwaterloo/flix/runtime/Value$Tag;", false)

    case Expression.GetTupleIndex(base, offset, tpe, _) =>
      // Compile the tuple expression, load the tuple array, compile the offset, load the array element, and unbox if
      // necessary.
      compileExpression(context, visitor)(base)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "elms",
        "()[Ljava/lang/Object;", false)
      compileInt(visitor)(offset)
      visitor.visitInsn(AALOAD)
      compileUnbox(context, visitor)(tpe)

    case Expression.Tuple(elms, _, _) =>
      // Create the array to hold the tuple elements.
      compileInt(visitor)(elms.size)
      visitor.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(classOf[AnyRef]))

      // Iterate over elms, boxing them and slotting each one into the array.
      elms.zipWithIndex.foreach { case (e, i) =>
        // Duplicate the array reference, otherwise AASTORE will consume it.
        visitor.visitInsn(DUP)
        compileInt(visitor)(i)
        compileBoxedExpr(context, visitor)(e)
        visitor.visitInsn(AASTORE)
      }

      // Now construct a Value.Tuple: create a reference, load the arguments, and call the constructor.
      visitor.visitTypeInsn(NEW, "ca/uwaterloo/flix/runtime/Value$Tuple")

      // We use dup_x1 and swap to manipulate the stack so we can avoid using a local variable.
      // Stack before: array, tuple (top)
      // Stack after: tuple, tuple, array (top)
      visitor.visitInsn(DUP_X1)
      visitor.visitInsn(SWAP)

      // Finally, call the constructor, which pops the reference (tuple) and argument (array).
      visitor.visitMethodInsn(INVOKESPECIAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "<init>",
        "([Ljava/lang/Object;)V", false)

    case Expression.Set(elms, tpe, loc) => ???

    case Expression.UserError(_, loc) => compileThrow(context, visitor)(s"Runtime error at ${loc.format}")
    case Expression.MatchError(_, loc) => compileThrow(context, visitor)(s"Match error at ${loc.format}")
  }

  /*
   * Some types (e.g. bool, int, str) need to be boxed as Flix values.
   * Other types (e.g. tag, tuple) are already Flix values.
   */
  private def compileBoxedExpr(context: Context, visitor: MethodVisitor)(exp: Expression): Unit = exp.tpe match {
    case Type.Bool =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      exp match {
        case Expression.True =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "True",
            "()Ljava/lang/Object;", false)
        case Expression.False =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "False",
            "()Ljava/lang/Object;", false)
        case _ =>
          compileExpression(context, visitor)(exp)
          visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkBool",
            "(Z)Ljava/lang/Object;", false)
      }
    case Type.Int8 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt8",
        "(I)Ljava/lang/Object;", false)
    case Type.Int16 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt16",
        "(I)Ljava/lang/Object;", false)
    case Type.Int32 | Type.Int32 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt32",
        "(I)Ljava/lang/Object;", false)
    case Type.Int64 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt64",
        "(J)Ljava/lang/Object;", false)
    case Type.Str =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$",
        "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(context, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkStr",
        "(Ljava/lang/String;)Ljava/lang/Object;", false)
    case Type.Unit | Type.Enum(_, _) | Type.Tuple(_) =>
      compileExpression(context, visitor)(exp)
    case Type.Tag(_, _, _) => throw new InternalCompilerError(s"Can't have a value of type ${exp.tpe}.")
    case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
         Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any => ???
  }

  /*
   * The value at the top of the stack is boxed as a Flix Value, and needs to be unboxed (e.g. to an int, boolean, or
   * String), or it needs to be cast to a specific Value type (e.g. Value.Unit.type, Value.Tag).
   */
  private def compileUnbox(context: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Unit => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Unit$")
    case Type.Bool =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)
    case Type.Int8 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Byte")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false)
    case Type.Int16 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Short")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)
    case Type.Int32 | Type.Int32 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Integer")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
    case Type.Int64 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Long")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)
    case Type.Str => visitor.visitTypeInsn(CHECKCAST, "java/lang/String")
    case Type.Enum(_, _) => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Tag")
    case Type.Tuple(_) => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Tuple")
    case Type.Tag(_, _, _) => throw new InternalCompilerError(s"Can't have a value of type $tpe.")
    case Type.Var(_) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
         Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any => ???
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
      compileInt(visitor)(load.offset)
      visitor.visitInsn(LSHR)
    }
    visitor.visitInsn(L2I)
    compileInt(visitor)(load.mask)
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
    compileInt(visitor)(store.targetMask, isLong = true)
    visitor.visitInsn(LAND)
    compileExpression(context, visitor)(store.v)
    visitor.visitInsn(I2L)
    compileInt(visitor)(store.mask, isLong = true)
    visitor.visitInsn(LAND)
    if (store.offset > 0) {
      compileInt(visitor)(store.offset)
      visitor.visitInsn(LSHL)
    }
    visitor.visitInsn(LOR)
  }

  /*
   * Generate code to load an integer constant.
   *
   * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
   * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
   * bytes. The exception is if we set `isLong` to true, in which case a cast will be performed if necessary.
   *
   * This is needed because sometimes we expect the operands to be a long, which means two (int) values are popped from
   * the stack and concatenated to form a long.
   */
  private def compileInt(visitor: MethodVisitor)(i: Long, isLong: Boolean = false): Unit = {
    i match {
      case -1 => visitor.visitInsn(ICONST_M1)
      case 0 => if (!isLong) visitor.visitInsn(ICONST_0) else visitor.visitInsn(LCONST_0)
      case 1 => if (!isLong) visitor.visitInsn(ICONST_1) else visitor.visitInsn(LCONST_1)
      case 2 => visitor.visitInsn(ICONST_2)
      case 3 => visitor.visitInsn(ICONST_3)
      case 4 => visitor.visitInsn(ICONST_4)
      case 5 => visitor.visitInsn(ICONST_5)
      case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => visitor.visitIntInsn(BIPUSH, i.toInt)
      case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => visitor.visitIntInsn(SIPUSH, i.toInt)
      case _ if scala.Int.MinValue <= i && i <= scala.Int.MaxValue => visitor.visitLdcInsn(i.toInt)
      case _ => visitor.visitLdcInsn(i)
    }
    if (isLong && scala.Int.MinValue <= i && i <= scala.Int.MaxValue && i != 0 && i != 1) visitor.visitInsn(I2L)
  }

  private def compileUnaryExpr(context: Context, visitor: MethodVisitor)(op: UnaryOperator, e: Expression): Unit = {
    compileExpression(context, visitor)(e)
    op match {
      case UnaryOperator.LogicalNot =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNE, condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)
      case UnaryOperator.Plus => // nop
      case UnaryOperator.Minus => compileUnaryMinusExpr(context, visitor)(e.tpe)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(context, visitor)(e.tpe)
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
  private def compileUnaryMinusExpr(context: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Int8 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case Type.Int16 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case Type.Int32 | Type.Int32 => visitor.visitInsn(INEG)
    case Type.Int64 => visitor.visitInsn(LNEG)
    case Type.Var(_) | Type.Unit | Type.Bool | Type.Str | Type.Tag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
         Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
         Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any =>
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
  private def compileUnaryNegateExpr(context: Context, visitor: MethodVisitor)(tpe: Type): Unit = {
    visitor.visitInsn(ICONST_M1)
    tpe match {
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int32 =>
        visitor.visitInsn(IXOR)
      case Type.Int64 =>
        visitor.visitInsn(I2L)
        visitor.visitInsn(LXOR)
      case Type.Var(_) | Type.Unit | Type.Bool | Type.Str | Type.Tag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
           Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any =>
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
                                   (o: ArithmeticOperator, e1: Expression, e2: Expression): Unit = {
    compileExpression(context, visitor)(e1)
    compileExpression(context, visitor)(e2)
    val (intOp, longOp) = o match {
      case BinaryOperator.Plus => (IADD, LADD)
      case BinaryOperator.Minus => (ISUB, LSUB)
      case BinaryOperator.Times => (IMUL, LMUL)
      case BinaryOperator.Divide => (IDIV, LDIV)
      case BinaryOperator.Modulo => (IREM, LREM)
    }
    e1.tpe match {
      case Type.Int8 =>
        visitor.visitInsn(intOp)
        visitor.visitInsn(I2B)
      case Type.Int16 =>
        visitor.visitInsn(intOp)
        visitor.visitInsn(I2S)
      case Type.Int32 | Type.Int32 => visitor.visitInsn(intOp)
      case Type.Int64 => visitor.visitInsn(longOp)
      case Type.Var(_) | Type.Unit | Type.Bool | Type.Str | Type.Tag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
           Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any =>
        throw new InternalCompilerError(s"Can't apply $o to type ${e1.tpe}.")
    }
  }

  private def compileComparisonExpr(context: Context, visitor: MethodVisitor)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression): Unit = {
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
      case Type.Bool if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
        // Booleans can be compared for equality.
        visitor.visitJumpInsn(intOp, condElse)
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int32 => visitor.visitJumpInsn(intOp, condElse)
      case Type.Int64 =>
        visitor.visitInsn(LCMP)
        visitor.visitJumpInsn(longOp, condElse)
      case Type.Var(_) | Type.Unit | Type.Bool | Type.Str | Type.Tag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
           Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any =>
        throw new InternalCompilerError(s"Can't apply $o to type ${e1.tpe}.")
    }
    visitor.visitInsn(ICONST_1)
    visitor.visitJumpInsn(GOTO, condEnd)
    visitor.visitLabel(condElse)
    visitor.visitInsn(ICONST_0)
    visitor.visitLabel(condEnd)
  }

  /*
   * Note that LogicalAnd, LogicalOr, and Implication do short-circuit evaluation.
   * Implication and Biconditional are rewritten to their logical equivalents, and then compiled.
   */
  private def compileLogicalExpr(context: Context, visitor: MethodVisitor)
                                (o: LogicalOperator, e1: Expression, e2: Expression): Unit = o match {
    case BinaryOperator.LogicalAnd =>
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
    case BinaryOperator.LogicalOr =>
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
    case BinaryOperator.Implication =>
      // (e1 ==> e2) === (!e1 || e2)
      val notExp = Expression.Unary(UnaryOperator.LogicalNot, e1, Type.Bool, e1.loc)
      compileLogicalExpr(context, visitor)(BinaryOperator.LogicalOr, notExp, e2)
    case BinaryOperator.Biconditional =>
      // (e1 <==> e2) === (e1 == e2)
      compileComparisonExpr(context, visitor)(BinaryOperator.Equal, e1, e2)
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
                                (o: BitwiseOperator, e1: Expression, e2: Expression): Unit = {
    compileExpression(context, visitor)(e1)
    compileExpression(context, visitor)(e2)
    val (intOp, longOp) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND)
      case BinaryOperator.BitwiseOr => (IOR, LOR)
      case BinaryOperator.BitwiseXor => (IXOR, LXOR)
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL)
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR)
    }
    e1.tpe match {
      case Type.Int8 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2B)
      case Type.Int16 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2S)
      case Type.Int32 | Type.Int32 => visitor.visitInsn(intOp)
      case Type.Int64 => visitor.visitInsn(longOp)
      case Type.Var(_) | Type.Unit | Type.Bool | Type.Str | Type.Tag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
           Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Lambda(_, _) |
           Type.Predicate(_) | Type.Native | Type.Char | Type.Abs(_, _) | Type.Any =>
        throw new InternalCompilerError(s"Can't apply $o to type ${e1.tpe}.")
    }
  }

  // TODO: A better exception to throw?
  private def compileThrow(context: Context, visitor: MethodVisitor)(message: String): Unit = {
    visitor.visitTypeInsn(NEW, "java/lang/RuntimeException")
    visitor.visitInsn(DUP)
    visitor.visitLdcInsn(message)
    visitor.visitMethodInsn(INVOKESPECIAL, "java/lang/RuntimeException", "<init>", "(Ljava/lang/String;)V", false)
    visitor.visitInsn(ATHROW)
  }

}
