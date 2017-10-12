/*
 * Copyright 2017 Ramin Zarifi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{Type, _}
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import java.lang.reflect.Modifier

/**
  * Generate expression
  */
object GenExpression {

  def compileExpression(currentClassType: JvmType.Reference,
                        expr: Expression,
                        jumpLabels: Map[Symbol.LabelSym, Label],
                        entryPoint: Label,
                        visitor: MethodVisitor)(implicit root: Root, flix: Flix): Unit = expr match {
    case Expression.Unit =>
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
        AsmOps.getMethodDescriptor(List(JvmType.Void), JvmType.Unit), false)
    case Expression.True => visitor.visitInsn(ICONST_1)
    case Expression.False => visitor.visitInsn(ICONST_0)
    case Expression.Char(c) => compileInt(visitor)(c)
    case Expression.Float32(f) => f match {
      case 0f => visitor.visitInsn(FCONST_0)
      case 1f => visitor.visitInsn(FCONST_1)
      case 2f => visitor.visitInsn(FCONST_2)
      case _ => visitor.visitLdcInsn(f)
    }
    case Expression.Float64(d) => d match {
      case 0d => visitor.visitInsn(DCONST_0)
      case 1d => visitor.visitInsn(DCONST_1)
      case _ => visitor.visitLdcInsn(d)
    }
    case Expression.Int8(b) => compileInt(visitor)(b)
    case Expression.Int16(s) => compileInt(visitor)(s)
    case Expression.Int32(i) => compileInt(visitor)(i)
    case Expression.Int64(l) => compileInt(visitor)(l, isLong = true)
    case Expression.BigInt(ii) =>
      visitor.visitTypeInsn(NEW, JvmName.BigInteger.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, JvmName.BigInteger.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
    case Expression.Str(s) => visitor.visitLdcInsn(s)

    case Expression.Var(sym, tpe, _) => tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ILOAD, sym.getStackOffset)
      case Type.Int64 => visitor.visitVarInsn(LLOAD, sym.getStackOffset)
      case Type.Float32 => visitor.visitVarInsn(FLOAD, sym.getStackOffset)
      case Type.Float64 => visitor.visitVarInsn(DLOAD, sym.getStackOffset)
      case Type.Unit | Type.BigInt | Type.Str | Type.Native => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ if tpe.isArrow || tpe.isEnum || tpe.isRef || tpe.isTuple => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    case Expression.Closure(sym, freeVars, fnType, tpe, loc) => ???
    case Expression.ApplyClo(exp, args, _, loc) => ???
    case Expression.ApplyDef(name, args, _, loc) => ???
    case Expression.ApplyCloTail(exp, args, _, loc) => ???
    case Expression.ApplyDefTail(name, args, _, loc) => ???
    case Expression.ApplySelfTail(name, formals, actuals, _, loc) => ???
    case Expression.ApplyHook(hook, args, tpe, loc) => ???

    case Expression.Unary(sop, op, exp, _, _) => compileUnaryExpr(currentClassType, visitor, jumpLabels, entryPoint)(op, exp)

    case Expression.Binary(sop, op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(currentClassType, visitor, jumpLabels, entryPoint)(o, exp1, exp2)
      case o: ComparisonOperator => compileComparisonExpr(currentClassType, visitor, jumpLabels, entryPoint)(o, exp1, exp2)
      case o: LogicalOperator => compileLogicalExpr(currentClassType, visitor, jumpLabels, entryPoint)(o, exp1, exp2)
      case o: BitwiseOperator => compileBitwiseExpr(currentClassType, visitor, jumpLabels, entryPoint)(o, exp1, exp2)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(currentClassType, exp1, jumpLabels, entryPoint, visitor)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(currentClassType, exp2, jumpLabels, entryPoint, visitor)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(currentClassType, exp3, jumpLabels, entryPoint, visitor)
      visitor.visitLabel(ifEnd)

    case Expression.Branch(exp, branches, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Calculating the updated jumpLabels map
      val updatedJumpLabels = branches.foldLeft(jumpLabels)((map, branch) => map + (branch._1 -> new Label()))
      // Compiling the exp
      compileExpression(currentClassType, exp, updatedJumpLabels, entryPoint, visitor)
      // Label for the end of all branches
      val endLabel = new Label()
      // Skip branches if `exp` does not jump
      visitor.visitJumpInsn(GOTO, endLabel)
      // Compiling branches
      branches.foreach { case (sym, branchExp) =>
        // Label for the start of the branch
        visitor.visitLabel(updatedJumpLabels(sym))
        // evaluating the expression for the branch
        compileExpression(currentClassType, branchExp, updatedJumpLabels, entryPoint, visitor)
        // Skip the rest of the branches
        visitor.visitJumpInsn(GOTO, endLabel)
      }
      // label for the end of branches
      visitor.visitLabel(endLabel)

    case Expression.JumpTo(sym, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Jumping to the label
      visitor.visitJumpInsn(GOTO, jumpLabels(sym))

    case Expression.Let(sym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      compileExpression(currentClassType, exp1, jumpLabels, entryPoint, visitor)
      exp1.tpe match {
        case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id.")
        case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ISTORE, sym.getStackOffset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, sym.getStackOffset)
        case Type.Float32 => visitor.visitVarInsn(FSTORE, sym.getStackOffset)
        case Type.Float64 => visitor.visitVarInsn(DSTORE, sym.getStackOffset)
        case Type.Unit | Type.BigInt | Type.Str | Type.Native => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case _ if exp1.tpe.isArrow || exp1.tpe.isEnum || exp1.tpe.isRef || exp1.tpe.isTuple => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case tpe => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }
      compileExpression(currentClassType, exp2, jumpLabels, entryPoint, visitor)

    case Expression.LetRec(sym, exp1, exp2, _, _) =>
      ??? // TODO: Add support for LetRec.

    case Expression.Is(enum, tag, exp, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // First we compile the `exp`
      compileExpression(currentClassType, exp, jumpLabels, entryPoint, visitor)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(exp.tpe, tag)
      // We get the JvmType of the class for tag
      val classType = JvmOps.getTagClassType(tagInfo)
      // We check if the enum is `instanceof` the class
      visitor.visitTypeInsn(INSTANCEOF, classType.name.toInternalName)

    case Expression.Tag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(tpe, tag)
      // We get the JvmType of the class for tag
      val classType = JvmOps.getTagClassType(tagInfo)
      /*
       If the definition of the enum case has a `Unit` field, then it is represented by singleton pattern which means
       there is only one instance of the class initiated as a field. We have to fetch this field instead of instantiating
       a new one.
       */
      if (JvmOps.isSingletonEnum(tagInfo)) {
        visitor.visitFieldInsn(GETSTATIC, classType.name.toInternalName, "unitInstance", classType.toDescriptor)
      } else {
        // Creating a new instance of the class
        visitor.visitTypeInsn(NEW, classType.name.toInternalName)
        visitor.visitInsn(DUP)
        // Evaluating the single argument of the class constructor
        compileExpression(currentClassType, exp, jumpLabels, entryPoint, visitor)
        // Descriptor of the constructor
        val constructorDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedType(tagInfo.tagType)), JvmType.Void)
        // Calling the constructor of the class
        visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)
      }

    case Expression.Untag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(exp.tpe, tag)
      // We get the JvmType of the class for the tag
      val classType = JvmOps.getTagClassType(tagInfo)
      // Evaluate the exp
      compileExpression(currentClassType, exp, jumpLabels, entryPoint, visitor)
      // Cast the exp to the type of the tag
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
      // Descriptor of the method
      val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedType(tagInfo.tagType))
      // Invoke `getValue()` method to extract the field of the tag
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
      // Cast the object to it's type if it's not a primitive
      castIfNotPrim(tpe, visitor)

    case Expression.Index(base, offset, tpe, _) =>
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleClassType(base.tpe)
      // evaluating the `base`
      compileExpression(currentClassType, base, jumpLabels, entryPoint, visitor)
      // Descriptor of the method
      val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedType(tpe))
      // Invoking `getField${offset}()` method for fetching the field
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, s"getIndex$offset", methodDescriptor, false)
      // Cast the object to it's type if it's not a primitive
      castIfNotPrim(tpe, visitor)

    case Expression.Tuple(elms, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleClassType(tpe)
      // Instantiating a new object of tuple
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating all the elements to be stored in the tuple class
      elms.foreach(compileExpression(currentClassType, _, jumpLabels, entryPoint, visitor))
      // Erased type of `elms`
      val erasedElmTypes = elms.map(_.tpe).map(JvmOps.getErasedType).toList
      // Descriptor of constructor
      val constructorDescriptor = AsmOps.getMethodDescriptor(erasedElmTypes, JvmType.Void)
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

    case Expression.Ref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(tpe)
      // Create a new reference object
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicate it since one instance will get consumed by constructor
      visitor.visitInsn(DUP)
      // Evaluate the underlying expression
      compileExpression(currentClassType, exp, jumpLabels, entryPoint, visitor)
      // Erased type of the value of the reference
      val valueErasedType = JvmOps.getErasedType(tpe.typeArguments.head)
      // Constructor descriptor
      val constructorDescriptor = AsmOps.getMethodDescriptor(List(valueErasedType), JvmType.Void)
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

    case Expression.Deref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the exp
      compileExpression(currentClassType, exp, jumpLabels, entryPoint, visitor)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(exp.tpe)
      // Get descriptor of `getValue` method
      val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedType(tpe))
      // Dereference the expression
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
      // Cast underlying value to the correct type if the underlying type is Object
      castIfNotPrim(tpe, visitor)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the reference address
      compileExpression(currentClassType, exp1, jumpLabels, entryPoint, visitor)
      // Evaluating the value to be assigned to the reference
      compileExpression(currentClassType, exp2, jumpLabels, entryPoint,visitor)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(exp1.tpe)
      // Get descriptor of `setValue` method
      val methodDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedType(exp2.tpe)), JvmType.Void)
      // Invoke `setValue` method to set the value to the given number
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "setValue", methodDescriptor, false)
      // Since the return type is unit, we put an instance of unit on top of the stack
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
        AsmOps.getMethodDescriptor(Nil, JvmType.Unit), false)


    case Expression.Existential(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.Universal(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val descriptor = asm.Type.getConstructorDescriptor(constructor)
      val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
      // Create a new object of the declaration type
      visitor.visitTypeInsn(NEW, declaration)
      // Duplicate the reference since the first argument for a constructor call is the reference to the object
      visitor.visitInsn(DUP)
      // Evaluate arguments left-to-right and push them onto the stack.
      args.foreach(compileExpression(currentClassType, _, jumpLabels, entryPoint, visitor))
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, declaration, "<init>", descriptor, false)

    case Expression.NativeField(field, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Fetch a field from an object
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      val name = field.getName
      // Use GETSTATIC if the field is static and GETFIELD if the field is on an object
      val getInsn = if (Modifier.isStatic(field.getModifiers)) GETSTATIC else GETFIELD
      visitor.visitFieldInsn(getInsn, declaration, name, JvmOps.getJvmType(tpe).toDescriptor)

    case Expression.NativeMethod(method, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate arguments left-to-right and push them onto the stack.
      args.foreach(compileExpression(currentClassType, _, jumpLabels, entryPoint, visitor))
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)
      // If the method is static, use INVOKESTATIC otherwise use INVOKEVIRTUAL
      val invokeInsn = if (Modifier.isStatic(method.getModifiers)) INVOKESTATIC else INVOKEVIRTUAL
      visitor.visitMethodInsn(invokeInsn, declaration, name, descriptor, false)
      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
        visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
          AsmOps.getMethodDescriptor(List(), JvmType.Unit), false)
      }

    case Expression.UserError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"User exception: ${loc.format}."
      compileException(visitor, JvmName.UserException, msg)

    case Expression.MatchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"Non-exhaustive match expression: ${loc.format}."
      compileException(visitor, JvmName.MatchException, msg)

    case Expression.SwitchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"Non-exhaustive switch expression: ${loc.format}."
      compileException(visitor, JvmName.SwitchException, msg)
  }

  private def compileException(visitor: MethodVisitor, className: JvmName, msg: String): Unit = {
    visitor.visitTypeInsn(NEW, className.toInternalName)
    visitor.visitInsn(DUP)
    visitor.visitLdcInsn(msg)
    // TODO: Load actual source location or change the exception
    visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "MODULE$", "Lca/uwaterloo/flix/language/ast/package$SourceLocation$;")
    visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "Unknown", "()Lca/uwaterloo/flix/language/ast/package$SourceLocation;", false)
    visitor.visitMethodInsn(INVOKESPECIAL, className.toInternalName, "<init>", "(Ljava/lang/String;Lca/uwaterloo/flix/language/ast/package$SourceLocation;)V", false)
    visitor.visitInsn(ATHROW)
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

  private def compileUnaryExpr(currentClassType: JvmType.Reference,
                               visitor: MethodVisitor,
                               jumpLabels: Map[Symbol.LabelSym, Label],
                               entryPoint: Label)(op: UnaryOperator, e: Expression)(implicit root: Root, flix: Flix): Unit = {
    // Adding source line number for debugging
    addSourceLine(visitor, e.loc)

    compileExpression(currentClassType, e, jumpLabels, entryPoint, visitor)
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
      case UnaryOperator.Minus => compileUnaryMinusExpr(visitor)(e.tpe)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(visitor)(e.tpe)
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
  private def compileUnaryMinusExpr(visitor: MethodVisitor)(tpe: Type)(implicit root: Root, flix: Flix): Unit = tpe match {
    case Type.Float32 => visitor.visitInsn(FNEG)
    case Type.Float64 => visitor.visitInsn(DNEG)
    case Type.Int8 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case Type.Int16 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case Type.Int32 => visitor.visitInsn(INEG)
    case Type.Int64 => visitor.visitInsn(LNEG)
    case Type.BigInt =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "negate",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false);
    case _ => throw InternalCompilerException(s"Can't apply UnaryOperator.Minus to type $tpe.")
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
  private def compileUnaryNegateExpr(visitor: MethodVisitor)(tpe: Type)(implicit root: Root, flix: Flix): Unit = tpe match {
    case Type.Int8 | Type.Int16 | Type.Int32 =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(IXOR)
    case Type.Int64 =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(I2L)
      visitor.visitInsn(LXOR)
    case Type.BigInt =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "not",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false);
    case _ => throw InternalCompilerException(s"Can't apply UnaryOperator.Negate to type $tpe.")
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
   *
   * Exponentiation takes a separate codepath. Values must be cast to doubles (F2D, I2D, L2D; note that bytes and shorts
   * are represented as ints and so we use I2D), then we invoke the static method `math.pow`, and then we have to cast
   * back to the original type (D2F, D2I, D2L; note that bytes and shorts need to be cast again with I2B and I2S).
   */
  private def compileArithmeticExpr(currentClassType: JvmType.Reference,
                                    visitor: MethodVisitor,
                                    jumpLabels: Map[Symbol.LabelSym, Label],
                                    entryPoint: Label)
                                   (o: ArithmeticOperator, e1: Expression, e2: Expression)
                                   (implicit root: Root, flix: Flix): Unit = {
    if (o == BinaryOperator.Exponentiate) {
      val (castToDouble, castFromDouble) = e1.tpe match {
        case Type.Float32 => (F2D, D2F)
        case Type.Float64 => (NOP, NOP) // already a double
        case Type.Int8 | Type.Int16 | Type.Int32 => (I2D, D2I)
        case Type.Int64 => (L2D, D2L)
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
      }
      visitor.visitFieldInsn(GETSTATIC, JvmName.ScalaMathPkg.toInternalName, "MODULE$", JvmType.ScalaMathPkg.toDescriptor)
      compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
      visitor.visitInsn(castToDouble)
      compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
      visitor.visitInsn(castToDouble)
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.ScalaMathPkg.toInternalName, "pow",
        AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
      visitor.visitInsn(castFromDouble)
      (e1.tpe: @unchecked) match {
        case Type.Int8 => visitor.visitInsn(I2B)
        case Type.Int16 => visitor.visitInsn(I2S)
        case Type.Float32 | Type.Float64 | Type.Int32 | Type.Int64 => visitor.visitInsn(NOP)
      }
    } else {
      compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
      compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
      val (intOp, longOp, floatOp, doubleOp, bigIntOp) = o match {
        case BinaryOperator.Plus => (IADD, LADD, FADD, DADD, "add")
        case BinaryOperator.Minus => (ISUB, LSUB, FSUB, DSUB, "subtract")
        case BinaryOperator.Times => (IMUL, LMUL, FMUL, DMUL, "multiply")
        case BinaryOperator.Divide => (IDIV, LDIV, FDIV, DDIV, "divide")
        case BinaryOperator.Modulo => (IREM, LREM, FREM, DREM, "remainder")
        case BinaryOperator.Exponentiate => throw InternalCompilerException("BinaryOperator.Exponentiate already handled.")
      }
      e1.tpe match {
        case Type.Float32 => visitor.visitInsn(floatOp)
        case Type.Float64 => visitor.visitInsn(doubleOp)
        case Type.Int8 =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2B)
        case Type.Int16 =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2S)
        case Type.Int32 => visitor.visitInsn(intOp)
        case Type.Int64 => visitor.visitInsn(longOp)
        case Type.BigInt =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, bigIntOp,
            AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.BigInteger), false);
        case Type.Str => (e2.tpe, o) match {
          case (Type.Str, BinaryOperator.Plus) =>
            visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "concat",
              AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.String), false)
          case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
        }
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
      }
    }
  }

  /*
   * Ints, Floats, and Chars support all six comparison operations (LE, LT, GE, GT, EQ, NE), but Unit, Bools, Strings,
   * Enums, Tuples, and Sets only support EQ and NE. Note that the generated code uses the negated condition, i.e.
   * branch if the (source) condition is false.
   *
   * Some reference types (Unit and String) can use reference equality because of interning.
   *
   * Int8/16/32 and Char comparisons only need a single instruction (IF_ICMPyy, where yy is one of
   * {LE, LT, GE, GT, EQ, NE}), which jumps if the yy condition is true, i.e. the (source) condition is false. All other
   * types do a comparison first (LCMP, {F,D}CMP{G,L}), and then a branch (IFyy).
   *
   * Specifically, LCMP can be represented in pseudocode as:
   *
   *     if (v1 > v2)        1
   *     else if (v1 == v2)  0
   *     else if (v1 < v2)  -1
   *
   * Then the result is used in the IFyy comparison to determine which branch to take. So the pair of instructions
   * for comparing longs (LCMP, IFyy) is similar to the single instruction for comparing ints (IF_ICMPyy).
   *
   * Float32/64 is similar, using xCMPz instead of LCMP, where x is one of {F,D} and z is one of {G,L}. z is necessary
   * to handle the fact that a float can be NaN (which is unordered), and any comparison involving NaN must fail.
   * xCMPG and xCMPL are the same, except for how they handle NaN. If either operand is NaN, xCMPG will push 1 onto the
   * stack, while xCMPL will push -1. In pseudocode:
   *
   *     if (v1 > v2)        1
   *     else if (v1 == v2)  0
   *     else if (v1 < v2)  -1
   *     else if (v1 is NaN || v2 is NaN)
   *       if (xCMPG)       1
   *       else if (xCMPL) -1
   *
   * For more information, see the following:
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-3.html#jvms-3.5
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.if_icmp_cond
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.lcmp
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.fcmp_op
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.if_cond
   *
   * BigInts are compared using the `compareTo` method.
   * `bigint1 OP bigint2` is compiled as `bigint1.compareTo(bigint2) OP 0`.
   */
  private def compileComparisonExpr(currentClassType: JvmType.Reference,
                                    visitor: MethodVisitor,
                                    jumpLabels: Map[Symbol.LabelSym, Label],
                                    entryPoint: Label)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression)
                                   (implicit root: Root, flix: Flix): Unit = {
    e1.tpe match {
      case _ if e1.tpe.isEnum && (o == BinaryOperator.Equal || o == BinaryOperator.NotEqual) =>
        (e1.tpe: @unchecked) match {
          case _ if e1.tpe.isEnum =>
            compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
            compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
            visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "equals",
              AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
          case Type.Apply(Type.Tuple(_), _) =>
            compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
            compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
            visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "equals",
              AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
        }
        if (o == BinaryOperator.NotEqual) {
          val condElse = new Label()
          val condEnd = new Label()
          visitor.visitJumpInsn(IFEQ, condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_1)
          visitor.visitLabel(condEnd)
        }
      case _ =>
        compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
        compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
        val condElse = new Label()
        val condEnd = new Label()
        val (intOp, floatOp, doubleOp, cmp) = o match {
          case BinaryOperator.Less => (IF_ICMPGE, FCMPG, DCMPG, IFGE)
          case BinaryOperator.LessEqual => (IF_ICMPGT, FCMPG, DCMPG, IFGT)
          case BinaryOperator.Greater => (IF_ICMPLE, FCMPL, DCMPL, IFLE)
          case BinaryOperator.GreaterEqual => (IF_ICMPLT, FCMPL, DCMPL, IFLT)
          case BinaryOperator.Equal => (IF_ICMPNE, FCMPG, DCMPG, IFNE)
          case BinaryOperator.NotEqual => (IF_ICMPEQ, FCMPG, DCMPG, IFEQ)
        }
        e1.tpe match {
          case Type.Unit if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Unit can only be equal to unit, so objects are poped from the top of the stack
            visitor.visitInsn(POP)
            visitor.visitInsn(POP)
            // A unit value is always equal itself, so no need to branch.
            // A unit value is never unequal to itself, so always branch to else label.
            e2.tpe match {
              case Type.Unit if o == BinaryOperator.NotEqual => visitor.visitJumpInsn(GOTO, condElse)
              case Type.Unit if o == BinaryOperator.Equal =>
              case _ if o == BinaryOperator.Equal => visitor.visitJumpInsn(GOTO, condElse)
              case _ =>
            }
          case Type.Str if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // String can be compared using Object's `equal` method
            visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "equals",
              AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(intOp, condElse)
          case Type.Bool if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Bool can be (value) compared for equality.
            visitor.visitJumpInsn(intOp, condElse)
          case Type.Float32 =>
            visitor.visitInsn(floatOp)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.Float64 =>
            visitor.visitInsn(doubleOp)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitJumpInsn(intOp, condElse)
          case Type.Int64 =>
            visitor.visitInsn(LCMP)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.BigInt =>
            visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(intOp, condElse)
          case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
        }
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)
    }
  }

  /*
   * Note that LogicalAnd, LogicalOr, and Implication do short-circuit evaluation.
   * Implication and Biconditional are rewritten to their logical equivalents, and then compiled.
   */
  private def compileLogicalExpr(currentClassType: JvmType.Reference,
                                 visitor: MethodVisitor,
                                 jumpLabels: Map[Symbol.LabelSym, Label],
                                 entryPoint: Label)
                                (o: LogicalOperator, e1: Expression, e2: Expression)(implicit root: Root, flix: Flix): Unit = o match {
    case BinaryOperator.LogicalAnd =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
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
      compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
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
  private def compileBitwiseExpr(currentClassType: JvmType.Reference,
                                 visitor: MethodVisitor,
                                 jumpLabels: Map[Symbol.LabelSym, Label],
                                 entryPoint: Label)
                                (o: BitwiseOperator, e1: Expression, e2: Expression)(implicit root: Root, flix: Flix): Unit = {
    compileExpression(currentClassType, e1, jumpLabels, entryPoint, visitor)
    compileExpression(currentClassType, e2, jumpLabels, entryPoint, visitor)
    val (intOp, longOp, bigintOp) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND, "and")
      case BinaryOperator.BitwiseOr => (IOR, LOR, "or")
      case BinaryOperator.BitwiseXor => (IXOR, LXOR, "xor")
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL, "shiftLeft")
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR, "shiftRight")
    }
    e1.tpe match {
      case Type.Int8 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2B)
      case Type.Int16 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2S)
      case Type.Int32 => visitor.visitInsn(intOp)
      case Type.Int64 => visitor.visitInsn(longOp)
      case Type.BigInt =>
        visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName,
          bigintOp, AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.BigInteger), false)
      case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
    }
  }

  /*
   * Adding the source of the line for debugging
   */
  private def addSourceLine(visitor: MethodVisitor, loc: SourceLocation): Unit = {
    val label = new Label()
    visitor.visitLabel(label)
    visitor.visitLineNumber(loc.beginLine, label)
  }

  /*
   * `tpe` is type of value on top of the stack. If the value is not primitive, then we cast it to it's specific type,
   * if the value is a primitive then since there is no boxing, then no casting is necessary.
   */
  private def castIfNotPrim(tpe: Type, visitor: MethodVisitor)(implicit root: Root, flix: Flix): Unit = tpe match {
    case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
    case Type.Unit => visitor.visitTypeInsn(CHECKCAST, JvmName.Unit.toInternalName)
    case Type.Bool => ()
    case Type.Char => ()
    case Type.Float32 => ()
    case Type.Float64 => ()
    case Type.Int8 => ()
    case Type.Int16 => ()
    case Type.Int32 => ()
    case Type.Int64 => ()
    case Type.BigInt => visitor.visitTypeInsn(CHECKCAST, JvmName.BigInteger.toInternalName)
    case Type.Str => visitor.visitTypeInsn(CHECKCAST, JvmName.String.toInternalName)
    case Type.Native => visitor.visitTypeInsn(CHECKCAST, JvmName.Object.toInternalName)
    case _ if tpe.isArrow =>
      val classType = JvmOps.getFunctionInterfaceType(tpe)
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
    case _ if tpe.isTuple =>
      val classType = JvmOps.getTupleInterfaceType(tpe)
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
    case _ if tpe.isEnum =>
      val classType = JvmOps.getEnumInterfaceType(tpe)
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }
  
}
