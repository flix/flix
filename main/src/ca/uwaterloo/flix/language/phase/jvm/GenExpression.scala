/*
 * Copyright 2017 Ramin Zarifi
 * Copyright 2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.ast.SemanticOp.*
import ca.uwaterloo.flix.language.ast.shared.{Constant, ExpPosition, Mutability}
import ca.uwaterloo.flix.language.ast.{SimpleType, *}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.StdlibProfile
import ca.uwaterloo.flix.util.collection.ListOps
import org.objectweb.asm
import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*

/**
  * Generate expression
  */
object GenExpression {

  type Ref[T] = Array[T]

  sealed trait MethodContext {

    def entryPoint: Label

    def lenv: Map[Symbol.LabelSym, Label]

    def localOffset: Int

    def addLabels(labels: Map[Symbol.LabelSym, Label]): MethodContext = {
      val updatedLabels = this.lenv ++ labels
      this match {
        case ctx: EffectContext =>
          ctx.copy(lenv = updatedLabels)
        case ctx: DirectInstanceContext =>
          ctx.copy(lenv = updatedLabels)
        case ctx: DirectStaticContext =>
          ctx.copy(lenv = updatedLabels)
      }
    }

  }

  /**
    * A context for methods with effect instrumentation, i.e., control impure functions.
    * Such functions / methods need to record their internal state which `newFrame`,
    * `setPc`, `pcLabels`, and `pcCounter` are for.
    */
  case class EffectContext(entryPoint: Label,
                           lenv: Map[Symbol.LabelSym, Label],
                           newFrame: MethodVisitor => Unit, // [...] -> [..., frame]
                           setPc: MethodVisitor => Unit, // [..., frame, pc] -> [...]
                           localOffset: Int,
                           pcLabels: Vector[Label],
                           pcCounter: Ref[Int]
                          ) extends MethodContext

  /**
    * A context for control pure functions that may capture variables and therefore use
    * fields to store its arguments.
    * Such functions never need to record their state and will always
    * return at the given return expressions except if they loop indefinitely.
    */
  case class DirectInstanceContext(entryPoint: Label,
                                   lenv: Map[Symbol.LabelSym, Label],
                                   localOffset: Int,
                                  ) extends MethodContext

  /**
    * A context for control pure functions that do not closure capture any variables and therefore
    * never use any fields to store arguments.
    * Such functions never need to record their state and will always
    * return at the given return expressions except if they loop indefinitely.
    */
  case class DirectStaticContext(entryPoint: Label,
                                 lenv: Map[Symbol.LabelSym, Label],
                                 localOffset: Int,
                                ) extends MethodContext

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpr(exp0: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = exp0 match {
    case Expr.Cst(cst, loc) => cst match {
      case Constant.Unit =>
        BytecodeInstructions.GETSTATIC(BackendObjType.Unit.SingletonField)

      case Constant.Null =>
        import BytecodeInstructions.*
        ACONST_NULL()

      case Constant.Bool(b) =>
        BytecodeInstructions.pushBool(b)

      case Constant.Char(c) =>
        BytecodeInstructions.pushInt(c)

      case Constant.Float32(f) =>
        f match {
          case 0f => mv.visitInsn(FCONST_0)
          case 1f => mv.visitInsn(FCONST_1)
          case 2f => mv.visitInsn(FCONST_2)
          case _ => mv.visitLdcInsn(f)
        }

      case Constant.Float64(d) =>
        d match {
          case 0d => mv.visitInsn(DCONST_0)
          case 1d => mv.visitInsn(DCONST_1)
          case _ => mv.visitLdcInsn(d)
        }

      case Constant.BigDecimal(dd) =>
        import BytecodeInstructions.*
        // Can fail with NumberFormatException
        addLoc(loc)
        NEW(JvmName.BigDecimal)
        DUP()
        pushString(dd.toString)
        INVOKESPECIAL(ClassConstants.BigDecimal.Constructor)

      case Constant.Int8(b) =>
        BytecodeInstructions.pushInt(b)

      case Constant.Int16(s) =>
        BytecodeInstructions.pushInt(s)

      case Constant.Int32(i) =>
        BytecodeInstructions.pushInt(i)

      case Constant.Int64(l) =>
        compileLong(l)

      case Constant.BigInt(ii) =>
        import BytecodeInstructions.*
        // Add source line number for debugging (can fail with NumberFormatException)
        addLoc(loc)
        NEW(JvmName.BigInteger)
        DUP()
        pushString(ii.toString)
        INVOKESPECIAL(ClassConstants.BigInteger.Constructor)

      case Constant.Str(s) =>
        BytecodeInstructions.pushString(s)

      case Constant.Regex(patt) =>
        import BytecodeInstructions.*
        // Add source line number for debugging (can fail with PatternSyntaxException)
        addLoc(loc)
        pushString(patt.pattern)
        INVOKESTATIC(ClassConstants.Regex.CompileMethod)

      case Constant.RecordEmpty =>
        BytecodeInstructions.GETSTATIC(BackendObjType.RecordEmpty.SingletonField)

      case Constant.Static =>
        import BytecodeInstructions.*
        //!TODO: For now, just emit null
        ACONST_NULL()
        CHECKCAST(BackendObjType.Region.jvmName)

    }

    case Expr.Var(_, offset, tpe, _) =>
      BytecodeInstructions.xLoad(BackendType.toBackendType(tpe), JvmOps.getIndex(offset, ctx.localOffset))

    case Expr.ApplyAtomic(op, exps, tpe, _, loc) => op match {

      case AtomicOp.Closure(sym) =>
        // JvmType of the closure
        val jvmName = JvmOps.getClosureClassName(sym)
        // new closure instance
        mv.visitTypeInsn(NEW, jvmName.toInternalName)
        // Duplicate
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL, jvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
        // Capturing free args
        for ((arg, i) <- exps.zipWithIndex) {
          val argType = BackendType.toBackendType(arg.tpe)
          mv.visitInsn(DUP)
          compileExpr(arg)
          BytecodeInstructions.castIfNotPrim(argType)
          mv.visitFieldInsn(PUTFIELD, jvmName.toInternalName, s"clo$i", argType.toDescriptor)
        }

      case AtomicOp.Unary(sop) =>
        val List(exp) = exps

        sop match {
          case ExnOp.KindId =>
            // Evaluate the operand (for sequencing), discard the value, then push the kind id.
            compileExpr(exp)
            BytecodeInstructions.xPop(BackendType.toBackendType(exp.tpe))
            BytecodeInstructions.pushInt(ExnKindId.of(exp.tpe))

          case SemanticOp.BoolOp.Not =>
            compileExpr(exp)
            val condElse = new Label()
            val condEnd = new Label()
            mv.visitJumpInsn(IFNE, condElse)
            mv.visitInsn(ICONST_1)
            mv.visitJumpInsn(GOTO, condEnd)
            mv.visitLabel(condElse)
            mv.visitInsn(ICONST_0)
            mv.visitLabel(condEnd)

          case Float32Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(FNEG)

          case Float64Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(DNEG)

          case Int8Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(INEG)
            mv.visitInsn(I2B) // Sign extend so sign bit is also changed

          case Int16Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(INEG)
            mv.visitInsn(I2S) // Sign extend so sign bit is also changed

          case Int32Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(INEG)

          case Int64Op.Neg =>
            compileExpr(exp)
            mv.visitInsn(LNEG)

          case BigIntOp.Neg =>
            compileBigIntUnaryOp(BigIntOp.Neg, exp)

          case BigIntOp.Not =>
            compileBigIntUnaryOp(BigIntOp.Not, exp)

          case BigIntOp.BitLength =>
            compileBigIntUnaryOp(BigIntOp.BitLength, exp)

          case BigIntOp.FromInt64 =>
            compileBigIntUnaryOp(BigIntOp.FromInt64, exp)

          case BigDecimalOp.Neg =>
            compileBigDecimalUnaryOp(BigDecimalOp.Neg, exp)

          case BigDecimalOp.Scale =>
            compileBigDecimalUnaryOp(BigDecimalOp.Scale, exp)

          case BigDecimalOp.Precision =>
            compileBigDecimalUnaryOp(BigDecimalOp.Precision, exp)

          case BigDecimalOp.Ceil =>
            compileBigDecimalUnaryOp(BigDecimalOp.Ceil, exp)

          case BigDecimalOp.Floor =>
            compileBigDecimalUnaryOp(BigDecimalOp.Floor, exp)

          case BigDecimalOp.Round =>
            compileBigDecimalUnaryOp(BigDecimalOp.Round, exp)

          case BigDecimalOp.ToBigInt =>
            compileBigDecimalUnaryOp(BigDecimalOp.ToBigInt, exp)

          case BigDecimalOp.ToPlainString =>
            compileBigDecimalUnaryOp(BigDecimalOp.ToPlainString, exp)

          case CodePointOp.GetName =>
            compileCodePointGetName(exp)

          case op: CodePointOp =>
            compileCodePointUnaryOp(op, exp)

          case Int8Op.Not | Int16Op.Not | Int32Op.Not =>
            compileExpr(exp)
            mv.visitInsn(ICONST_M1)
            mv.visitInsn(IXOR)

          case Int64Op.Not =>
            compileExpr(exp)
            mv.visitInsn(ICONST_M1)
            mv.visitInsn(I2L)
            mv.visitInsn(LXOR)

          case ToStringOp.CharToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toString",
              mkDescriptor(BackendType.Char)(BackendType.String).toDescriptor, false)

          case ToStringOp.Float32ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Float.toInternalName, "toString",
              mkDescriptor(BackendType.Float32)(BackendType.String).toDescriptor, false)

          case ToStringOp.Float64ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Double.toInternalName, "toString",
              mkDescriptor(BackendType.Float64)(BackendType.String).toDescriptor, false)

          case ToStringOp.Int8ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Byte.toInternalName, "toString",
              mkDescriptor(BackendType.Int8)(BackendType.String).toDescriptor, false)

          case ToStringOp.Int16ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Short.toInternalName, "toString",
              mkDescriptor(BackendType.Int16)(BackendType.String).toDescriptor, false)

          case ToStringOp.Int32ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "toString",
              mkDescriptor(BackendType.Int32)(BackendType.String).toDescriptor, false)

          case ToStringOp.Int64ToString =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Long.toInternalName, "toString",
              mkDescriptor(BackendType.Int64)(BackendType.String).toDescriptor, false)

          case ToStringOp.BigIntToString =>
            compileBigIntToString(exp)

          case ToStringOp.BigDecimalToString =>
            compileBigDecimalToString(exp)

          case op: ConvertOp =>
            compileExpr(exp)
            op match {
              case ConvertOp.Int8ToInt16 => mv.visitInsn(I2S)
              case ConvertOp.Int8ToInt32 => ()
              case ConvertOp.Int8ToInt64 => mv.visitInsn(I2L)
              case ConvertOp.Int8ToFloat32 => mv.visitInsn(I2F)
              case ConvertOp.Int8ToFloat64 => mv.visitInsn(I2D)
              case ConvertOp.Int16ToInt8 => mv.visitInsn(I2B)
              case ConvertOp.Int16ToInt32 => ()
              case ConvertOp.Int16ToInt64 => mv.visitInsn(I2L)
              case ConvertOp.Int16ToFloat32 => mv.visitInsn(I2F)
              case ConvertOp.Int16ToFloat64 => mv.visitInsn(I2D)
              case ConvertOp.Int32ToInt8 => mv.visitInsn(I2B)
              case ConvertOp.Int32ToInt16 => mv.visitInsn(I2S)
              case ConvertOp.Int32ToInt64 => mv.visitInsn(I2L)
              case ConvertOp.Int32ToFloat32 => mv.visitInsn(I2F)
              case ConvertOp.Int32ToFloat64 => mv.visitInsn(I2D)
              case ConvertOp.Int64ToInt8 =>
                mv.visitInsn(L2I)
                mv.visitInsn(I2B)
              case ConvertOp.Int64ToInt16 =>
                mv.visitInsn(L2I)
                mv.visitInsn(I2S)
              case ConvertOp.Int64ToInt32 => mv.visitInsn(L2I)
              case ConvertOp.Int64ToFloat32 => mv.visitInsn(L2F)
              case ConvertOp.Int64ToFloat64 => mv.visitInsn(L2D)
              case ConvertOp.Float32ToInt8 =>
                mv.visitInsn(F2I)
                mv.visitInsn(I2B)
              case ConvertOp.Float32ToInt16 =>
                mv.visitInsn(F2I)
                mv.visitInsn(I2S)
              case ConvertOp.Float32ToInt32 => mv.visitInsn(F2I)
              case ConvertOp.Float32ToInt64 => mv.visitInsn(F2L)
              case ConvertOp.Float32ToFloat64 => mv.visitInsn(F2D)
              case ConvertOp.Float64ToInt8 =>
                mv.visitInsn(D2I)
                mv.visitInsn(I2B)
              case ConvertOp.Float64ToInt16 =>
                mv.visitInsn(D2I)
                mv.visitInsn(I2S)
              case ConvertOp.Float64ToInt32 => mv.visitInsn(D2I)
              case ConvertOp.Float64ToInt64 => mv.visitInsn(D2L)
              case ConvertOp.Float64ToFloat32 => mv.visitInsn(D2F)
            }

          case PlatformOp.FileSeparator =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.File.toInternalName, "separator", JvmName.String.toDescriptor)

          case PlatformOp.PathSeparator =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.File.toInternalName, "pathSeparator", JvmName.String.toDescriptor)

          case PlatformOp.LineSeparator =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.System.toInternalName, "lineSeparator",
              mkDescriptor()(BackendType.String).toDescriptor, false)

          case ObjectOp.IsNull =>
            import BytecodeInstructions.*
            val erasedExpTpe = BackendType.toErasedBackendType(exp.tpe)
            compileExpr(exp)
            erasedExpTpe match {
              case BackendType.Object =>
                val isNull = new Label()
                val end = new Label()

                DUP()
                mv.visitJumpInsn(IFNULL, isNull)
                POP()
                ICONST_0()
                mv.visitJumpInsn(GOTO, end)

                mv.visitLabel(isNull)
                POP()
                ICONST_1()
                mv.visitLabel(end)

              case _ =>
                if (erasedExpTpe.is64BitWidth) POP2() else POP()
                ICONST_0()
            }

          case StringOp.Length =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "length",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case StringOp.ToLowerCase =>
            compileExpr(exp)
            if (flix.options.stdlibProfile == StdlibProfile.Portable) {
              // Portable semantics: locale-insensitive (Locale.ROOT) casing.
              mv.visitFieldInsn(GETSTATIC, JvmName.Locale.toInternalName, "ROOT", JvmName.Locale.toDescriptor)
              mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "toLowerCase",
                s"(${JvmName.Locale.toDescriptor})${JvmName.String.toDescriptor}", false)
            } else {
              // JVM semantics: uses the default locale (Java's no-arg overload).
              mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "toLowerCase",
                mkDescriptor()(BackendType.String).toDescriptor, false)
            }

          case StringOp.ToUpperCase =>
            compileExpr(exp)
            if (flix.options.stdlibProfile == StdlibProfile.Portable) {
              // Portable semantics: locale-insensitive (Locale.ROOT) casing.
              mv.visitFieldInsn(GETSTATIC, JvmName.Locale.toInternalName, "ROOT", JvmName.Locale.toDescriptor)
              mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "toUpperCase",
                s"(${JvmName.Locale.toDescriptor})${JvmName.String.toDescriptor}", false)
            } else {
              // JVM semantics: uses the default locale (Java's no-arg overload).
              mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "toUpperCase",
                mkDescriptor()(BackendType.String).toDescriptor, false)
            }

          case ParseOp.Int8FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Byte.toInternalName, "parseByte",
              mkDescriptor(BackendType.String)(BackendType.Int8).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Int16FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Short.toInternalName, "parseShort",
              mkDescriptor(BackendType.String)(BackendType.Int16).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Int32FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Integer.toInternalName, "parseInt",
              mkDescriptor(BackendType.String)(BackendType.Int32).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Int64FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Long.toInternalName, "parseLong",
              mkDescriptor(BackendType.String)(BackendType.Int64).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            LCONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Float32FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Float.toInternalName, "parseFloat",
              mkDescriptor(BackendType.String)(BackendType.Float32).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            mv.visitInsn(FCONST_0)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Float64FromString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Double.toInternalName, "parseDouble",
              mkDescriptor(BackendType.String)(BackendType.Float64).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            mv.visitInsn(DCONST_0)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.BigIntFromString =>
            compileBigIntFromString(exp, tpe)

          case ParseOp.BigDecimalFromString =>
            compileBigDecimalFromString(exp, tpe)

          case ParseOp.Int32Parse =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)

            // Extract (radix, string) from the tuple argument.
            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))
            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(argTupleType.IndexField(1)) // tuple, str
            CHECKCAST(JvmName.String) // tuple, str
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            SWAP() // str, tuple
            GETFIELD(argTupleType.IndexField(0)) // str, radix

            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Integer.toInternalName, "parseInt",
              mkDescriptor(BackendType.String, BackendType.Int32)(BackendType.Int32).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case ParseOp.Int64Parse =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)

            // Extract (radix, string) from the tuple argument.
            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))
            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(argTupleType.IndexField(1)) // tuple, str
            CHECKCAST(JvmName.String) // tuple, str
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            SWAP() // str, tuple
            GETFIELD(argTupleType.IndexField(0)) // str, radix

            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Long.toInternalName, "parseLong",
              mkDescriptor(BackendType.String, BackendType.Int32)(BackendType.Int64).toDescriptor, false)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            LCONST_0()
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case StringBuilderOp.New =>
            import BytecodeInstructions.*
            compileExpr(exp)
            POP()
            NEW(JvmName.StringBuilder)
            DUP()
            invokeConstructor(JvmName.StringBuilder, MethodDescriptor.NothingToVoid)

          case CharOp.IsLetter =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isLetter",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsDigit =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isDigit",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsLetterOrDigit =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isLetterOrDigit",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsLowerCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isLowerCase",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsUpperCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isUpperCase",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsTitleCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isTitleCase",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsWhitespace =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isWhitespace",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsDefined =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isDefined",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

	          case CharOp.IsISOControl =>
	            compileExpr(exp)
	            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isISOControl",
	              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

	          case StringBuilderOp.AppendString =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(tupleType.IndexField(1)) // tuple, handle
	            CHECKCAST(JvmName.StringBuilder) // tuple, sb
	            SWAP() // sb, tuple
	            DUP() // sb, tuple, tuple
	            GETFIELD(tupleType.IndexField(2)) // sb, tuple, str
	            CHECKCAST(JvmName.String) // sb, tuple, str
	            SWAP() // sb, str, tuple
	            POP() // sb, str
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "append",
	              mkDescriptor(BackendType.String)(JvmName.StringBuilder.toTpe).toDescriptor, false)
	            POP()
	            GETSTATIC(BackendObjType.Unit.SingletonField)

          case StringBuilderOp.AppendCodePoint =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.StringBuilder) // tuple, sb
            SWAP() // sb, tuple
            DUP() // sb, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // sb, tuple, cp
            SWAP() // sb, cp, tuple
            POP() // sb, cp
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "appendCodePoint",
              mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe).toDescriptor, false)
            POP()
            GETSTATIC(BackendObjType.Unit.SingletonField)

          case StringBuilderOp.CharAt =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.StringBuilder) // tuple, sb
            SWAP() // sb, tuple
            DUP() // sb, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // sb, tuple, idx
            SWAP() // sb, idx, tuple
            POP() // sb, idx
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "charAt",
              mkDescriptor(BackendType.Int32)(BackendType.Char).toDescriptor, false)

          case StringBuilderOp.Length =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            GETFIELD(tupleType.IndexField(1)) // handle
            CHECKCAST(JvmName.StringBuilder) // sb
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "length",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case StringBuilderOp.SetLength =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.StringBuilder) // tuple, sb
            SWAP() // sb, tuple
            DUP() // sb, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // sb, tuple, newLength
            SWAP() // sb, newLength, tuple
            POP() // sb, newLength
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "setLength",
              mkDescriptor(BackendType.Int32)(VoidableType.Void).toDescriptor, false)
            GETSTATIC(BackendObjType.Unit.SingletonField)

          case StringBuilderOp.ToString =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            GETFIELD(tupleType.IndexField(1)) // handle
            CHECKCAST(JvmName.StringBuilder) // sb
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.StringBuilder.toInternalName, "toString",
              mkDescriptor()(BackendType.String).toDescriptor, false)

          case RegexOp.FlagCanonEq =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "CANON_EQ", BackendType.Int32.toDescriptor)

          case RegexOp.FlagCaseInsensitive =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "CASE_INSENSITIVE", BackendType.Int32.toDescriptor)

          case RegexOp.FlagComments =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "COMMENTS", BackendType.Int32.toDescriptor)

          case RegexOp.FlagDotall =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "DOTALL", BackendType.Int32.toDescriptor)

          case RegexOp.FlagLiteral =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "LITERAL", BackendType.Int32.toDescriptor)

          case RegexOp.FlagMultiline =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "MULTILINE", BackendType.Int32.toDescriptor)

          case RegexOp.FlagUnicodeCase =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "UNICODE_CASE", BackendType.Int32.toDescriptor)

          case RegexOp.FlagUnicodeCharacterClass =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "UNICODE_CHARACTER_CLASS", BackendType.Int32.toDescriptor)

          case RegexOp.FlagUnixLines =>
            compileExpr(exp)
            mv.visitInsn(POP)
            mv.visitFieldInsn(GETSTATIC, JvmName.Regex.toInternalName, "UNIX_LINES", BackendType.Int32.toDescriptor)

	          case RegexOp.Compile =>
	            import BytecodeInstructions.*
	            compileExpr(exp)
	            INVOKESTATIC(ClassConstants.Regex.CompileMethod)

	          case RegexOp.CompileWithFlags =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(tupleType.IndexField(1)) // tuple, patt
	            CHECKCAST(JvmName.String) // tuple, patt
	            SWAP() // patt, tuple
	            GETFIELD(tupleType.IndexField(0)) // patt, flags
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
	              mkDescriptor(BackendType.String, BackendType.Int32)(JvmName.Regex.toTpe).toDescriptor, false)

          case RegexOp.TryCompile =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)
            compileExpr(exp)
            INVOKESTATIC(ClassConstants.Regex.CompileMethod)
            pushString("")
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.Throwable.toInternalName, "getMessage",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitVarInsn(org.objectweb.asm.Opcodes.ASTORE, 2000)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            pushString("^\\\\b$")
            INVOKESTATIC(ClassConstants.Regex.CompileMethod)
            mv.visitVarInsn(org.objectweb.asm.Opcodes.ALOAD, 2000)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case RegexOp.TryCompileWithFlags =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

            mv.visitLabel(tryStart)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(true)

            // Extract (flags, pattern) from the tuple argument.
            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))
	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(1)) // tuple, patt
	            CHECKCAST(JvmName.String) // tuple, patt
	            SWAP() // patt, tuple
	            GETFIELD(argTupleType.IndexField(0)) // patt, flags

            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
              mkDescriptor(BackendType.String, BackendType.Int32)(JvmName.Regex.toTpe).toDescriptor, false)
            pushString("")
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.Throwable.toInternalName, "getMessage",
              mkDescriptor()(BackendType.String).toDescriptor, false)
            mv.visitVarInsn(org.objectweb.asm.Opcodes.ASTORE, 2000)
            NEW(tupleType.jvmName)
            DUP()
            pushBool(false)
            pushString("^\\\\b$")
            INVOKESTATIC(ClassConstants.Regex.CompileMethod)
            mv.visitVarInsn(org.objectweb.asm.Opcodes.ALOAD, 2000)
            INVOKESPECIAL(tupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case RegexOp.Quote =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Regex.toInternalName, "quote",
              mkDescriptor(BackendType.String)(BackendType.String).toDescriptor, false)

          case RegexOp.Pattern =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.Regex.toInternalName, "pattern",
              mkDescriptor()(BackendType.String).toDescriptor, false)

          case RegexOp.Flags =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.Regex.toInternalName, "flags",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

	          case RegexOp.Split =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, rgx
            CHECKCAST(JvmName.Regex) // tuple, rgx
	            SWAP() // rgx, tuple
	            DUP() // rgx, tuple, tuple
	            GETFIELD(tupleType.IndexField(2)) // rgx, tuple, str
	            CHECKCAST(JvmName.CharSequence) // rgx, tuple, str
	            SWAP() // rgx, str, tuple
	            POP() // rgx, str
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.Regex.toInternalName, "split",
	              mkDescriptor(JvmName.CharSequence.toTpe)(BackendType.Array(BackendType.String)).toDescriptor, false)

	          case RegexOp.NewMatcher =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, rgx
            CHECKCAST(JvmName.Regex) // tuple, rgx
	            SWAP() // rgx, tuple
	            DUP() // rgx, tuple, tuple
	            GETFIELD(tupleType.IndexField(2)) // rgx, tuple, str
	            CHECKCAST(JvmName.CharSequence) // rgx, tuple, str
	            SWAP() // rgx, str, tuple
	            POP() // rgx, str
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.Regex.toInternalName, "matcher",
	              mkDescriptor(JvmName.CharSequence.toTpe)(JvmName.RegexMatcher.toTpe).toDescriptor, false)

          case RegexOp.MatcherMatches =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "matches",
              mkDescriptor()(BackendType.Bool).toDescriptor, false)

          case RegexOp.MatcherFind =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "find",
              mkDescriptor()(BackendType.Bool).toDescriptor, false)

          case RegexOp.MatcherFindFrom =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.RegexMatcher) // tuple, m
            SWAP() // m, tuple
            DUP() // m, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // m, tuple, pos
            SWAP() // m, pos, tuple
            POP() // m, pos
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "find",
              mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)

          case RegexOp.MatcherLookingAt =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "lookingAt",
              mkDescriptor()(BackendType.Bool).toDescriptor, false)

	          case RegexOp.MatcherReplaceAll =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.RegexMatcher) // tuple, m
	            SWAP() // m, tuple
	            DUP() // m, tuple, tuple
	            GETFIELD(tupleType.IndexField(2)) // m, tuple, replacement
	            CHECKCAST(JvmName.String) // m, tuple, replacement
	            SWAP() // m, replacement, tuple
	            POP() // m, replacement
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "replaceAll",
	              mkDescriptor(BackendType.String)(BackendType.String).toDescriptor, false)

	          case RegexOp.MatcherReplaceFirst =>
	            import BytecodeInstructions.*
	            val SimpleType.Tuple(elmTypes) = exp.tpe
	            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.RegexMatcher) // tuple, m
	            SWAP() // m, tuple
	            DUP() // m, tuple, tuple
	            GETFIELD(tupleType.IndexField(2)) // m, tuple, replacement
	            CHECKCAST(JvmName.String) // m, tuple, replacement
	            SWAP() // m, replacement, tuple
	            POP() // m, replacement
	            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "replaceFirst",
	              mkDescriptor(BackendType.String)(BackendType.String).toDescriptor, false)

          case RegexOp.MatcherSetBounds =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.RegexMatcher) // tuple, m
            SWAP() // m, tuple
            DUP() // m, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // m, tuple, start
            SWAP() // m, start, tuple
            DUP() // m, start, tuple, tuple
            GETFIELD(tupleType.IndexField(3)) // m, start, tuple, end
            SWAP() // m, start, end, tuple
            POP() // m, start, end
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "region",
              mkDescriptor(BackendType.Int32, BackendType.Int32)(JvmName.RegexMatcher.toTpe).toDescriptor, false)
            POP()
            GETSTATIC(BackendObjType.Unit.SingletonField)

          case RegexOp.MatcherStart =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "start",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case RegexOp.MatcherEnd =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "end",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case RegexOp.MatcherGroup =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(tupleType.IndexField(1)) // tuple, handle
            CHECKCAST(JvmName.RegexMatcher) // tuple, m
            SWAP() // m, tuple
            DUP() // m, tuple, tuple
            GETFIELD(tupleType.IndexField(2)) // m, tuple, groupIndex
            SWAP() // m, groupIndex, tuple
            POP() // m, groupIndex
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "group",
              mkDescriptor(BackendType.Int32)(BackendType.String).toDescriptor, false)

          case RegexOp.MatcherGroupCount =>
            import BytecodeInstructions.*
            val SimpleType.Tuple(elmTypes) = exp.tpe
            val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

            compileExpr(exp)
            GETFIELD(tupleType.IndexField(1))
            CHECKCAST(JvmName.RegexMatcher)
            mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.RegexMatcher.toInternalName, "groupCount",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case CharOp.IsMirrored =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isMirrored",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.IsSurrogate =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isSurrogate",
              mkDescriptor(BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.ToLowerCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toLowerCase",
              mkDescriptor(BackendType.Char)(BackendType.Char).toDescriptor, false)

          case CharOp.ToUpperCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toUpperCase",
              mkDescriptor(BackendType.Char)(BackendType.Char).toDescriptor, false)

          case CharOp.ToTitleCase =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toTitleCase",
              mkDescriptor(BackendType.Char)(BackendType.Char).toDescriptor, false)

          case CharOp.GetNumericValue =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "getNumericValue",
              mkDescriptor(BackendType.Char)(BackendType.Int32).toDescriptor, false)

          case HashOp.CharHash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "hashCode",
              mkDescriptor(BackendType.Char)(BackendType.Int32).toDescriptor, false)

          case HashOp.Float32Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Float.toInternalName, "hashCode",
              mkDescriptor(BackendType.Float32)(BackendType.Int32).toDescriptor, false)

          case HashOp.Float64Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Double.toInternalName, "hashCode",
              mkDescriptor(BackendType.Float64)(BackendType.Int32).toDescriptor, false)

          case HashOp.Int8Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Byte.toInternalName, "hashCode",
              mkDescriptor(BackendType.Int8)(BackendType.Int32).toDescriptor, false)

          case HashOp.Int16Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Short.toInternalName, "hashCode",
              mkDescriptor(BackendType.Int16)(BackendType.Int32).toDescriptor, false)

          case HashOp.Int32Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "hashCode",
              mkDescriptor(BackendType.Int32)(BackendType.Int32).toDescriptor, false)

          case HashOp.Int64Hash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Long.toInternalName, "hashCode",
              mkDescriptor(BackendType.Int64)(BackendType.Int32).toDescriptor, false)

          case HashOp.BigIntHash =>
            compileBigIntHash(exp)

          case HashOp.BigDecimalHash =>
            compileBigDecimalHash(exp)

          case HashOp.StringHash =>
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "hashCode",
              mkDescriptor()(BackendType.Int32).toDescriptor, false)

          case IoOp.Print =>
            BytecodeInstructions.addLoc(loc)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "out", JvmName.PrintStream.toDescriptor)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "print",
              mkDescriptor(BackendType.String)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "out", JvmName.PrintStream.toDescriptor)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "flush",
              mkDescriptor()(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.EPrint =>
            BytecodeInstructions.addLoc(loc)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "err", JvmName.PrintStream.toDescriptor)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "print",
              mkDescriptor(BackendType.String)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "err", JvmName.PrintStream.toDescriptor)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "flush",
              mkDescriptor()(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.Readln =>
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            mv.visitInsn(POP)
            val jConsole = JvmName.ofClass(classOf[java.io.Console])
            val consoleTpe = BackendType.Reference(BackendObjType.Native(jConsole))
            mv.visitMethodInsn(INVOKESTATIC, JvmName.System.toInternalName, "console",
              mkDescriptor()(consoleTpe).toDescriptor, false)
            mv.visitMethodInsn(INVOKEVIRTUAL, jConsole.toInternalName, "readLine",
              mkDescriptor()(BackendType.String).toDescriptor, false)

          case IoOp.Println =>
            BytecodeInstructions.addLoc(loc)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "out", JvmName.PrintStream.toDescriptor)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "println",
              mkDescriptor(BackendType.String)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.EPrintln =>
            BytecodeInstructions.addLoc(loc)
            mv.visitFieldInsn(GETSTATIC, JvmName.System.toInternalName, "err", JvmName.PrintStream.toDescriptor)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.PrintStream.toInternalName, "println",
              mkDescriptor(BackendType.String)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.SleepMillis =>
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Thread.toInternalName, "sleep",
              mkDescriptor(BackendType.Int64)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.Exit =>
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.System.toInternalName, "exit",
              mkDescriptor(BackendType.Int32)(VoidableType.Void).toDescriptor, false)
            mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

          case IoOp.NewId =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            POP()
            val jGlobal = JvmName(JvmName.DevFlixRuntime, "Global")
            INVOKESTATIC(jGlobal, "newId", mkDescriptor()(BackendType.Int64))

          case IoOp.TimeNowMillis =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            POP()
            INVOKESTATIC(JvmName.System, "currentTimeMillis", mkDescriptor()(BackendType.Int64))

          case op @ (IoOp.FileExists |
            IoOp.FileIsDirectory |
            IoOp.FileIsRegularFile |
            IoOp.FileIsReadable |
            IoOp.FileIsSymbolicLink |
            IoOp.FileIsWritable |
            IoOp.FileIsExecutable) =>
            compileIoFilePredicate(op, exp, tpe, loc)

          case op @ (IoOp.FileAccessTime |
            IoOp.FileCreationTime |
            IoOp.FileModificationTime |
            IoOp.FileSize) =>
            compileIoFileMetadata(op, exp, tpe, loc)

          case IoOp.FileRead =>
            compileIoFileRead(exp, tpe, loc)

          case IoOp.FileReadLines =>
            compileIoFileReadLines(exp, tpe, loc)

          case IoOp.FileReadBytes =>
            compileIoFileReadBytes(exp, tpe, loc)

          case IoOp.FileList =>
            compileIoFileList(exp, tpe, loc)

          case op @ (IoOp.FileWrite | IoOp.FileAppend) =>
            compileIoFileWriteString(op, exp, tpe, loc)

          case op @ (IoOp.FileWriteBytes | IoOp.FileAppendBytes) =>
            compileIoFileWriteBytes(op, exp, tpe, loc)

          case IoOp.FileTruncate =>
            compileIoFileTruncate(exp, tpe, loc)

          case op @ (IoOp.FileMkDir | IoOp.FileMkDirs) =>
            compileIoFileMkDir(op, exp, tpe, loc)

          case IoOp.FileMkTempDir =>
            compileIoFileMkTempDir(exp, tpe, loc)

          case IoOp.TcpSocketRead =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)

            val SimpleType.Tuple(retElmTypes) = tpe
            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

            val jGlobal = BackendObjType.Global.jvmName
            val jSocket = JvmName.ofClass(classOf[java.net.Socket])
            val socketTpe = BackendType.Reference(BackendObjType.Native(jSocket))
            val jInputStream = JvmName.ofClass(classOf[java.io.InputStream])
            val inputStreamTpe = BackendType.Reference(BackendObjType.Native(jInputStream))
            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

            // Locals.
            val idSlot = 2100
            val bufSlot = 2102
            val numSlot = 2103
            val exSlot = 2104
            val sockSlot = 2105

            // Extract (socketId, buffer) from the tuple argument.
            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(argTupleType.IndexField(0)) // tuple, id
            mv.visitVarInsn(LSTORE, idSlot) // tuple
            GETFIELD(argTupleType.IndexField(1)) // buffer
            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.Int8).toDescriptor)
            ASTORE(bufSlot)

            // socket = Global.getTcpSocket(id)
            LLOAD(idSlot)
            INVOKESTATIC(jGlobal, "getTcpSocket", mkDescriptor(BackendType.Int64)(socketTpe))
            ASTORE(sockSlot)

            val hasSocket = new Label()
            val after = new Label()
            ALOAD(sockSlot)
            mv.visitJumpInsn(IFNONNULL, hasSocket)

            // Return (false, 0, "invalid TCP socket handle.")
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            pushString("invalid TCP socket handle.")
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(hasSocket)

            // Try-catch around socket I/O.
            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

            mv.visitLabel(tryStart)
            // numRead = socket.getInputStream().read(buffer)
            ALOAD(sockSlot)
            INVOKEVIRTUAL(jSocket, "getInputStream", mkDescriptor()(inputStreamTpe))
            ALOAD(bufSlot)
            INVOKEVIRTUAL(jInputStream, "read", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendType.Int32))

            // if (numRead == -1) numRead = 0
            val notEof = new Label()
            val afterEof = new Label()
            DUP()
            ICONST_M1()
            mv.visitJumpInsn(IF_ICMPNE, notEof)
            POP()
            ICONST_0()
            mv.visitLabel(notEof)
            mv.visitLabel(afterEof)
            mv.visitVarInsn(ISTORE, numSlot)

            // Return (true, numRead, "")
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(true)
            ILOAD(numSlot)
            pushString("")
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            ASTORE(exSlot)
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            ALOAD(exSlot)
            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

          case IoOp.TcpSocketWrite =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)

            val SimpleType.Tuple(retElmTypes) = tpe
            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

            val jGlobal = BackendObjType.Global.jvmName
            val jSocket = JvmName.ofClass(classOf[java.net.Socket])
            val socketTpe = BackendType.Reference(BackendObjType.Native(jSocket))
            val jOutputStream = JvmName.ofClass(classOf[java.io.OutputStream])
            val outputStreamTpe = BackendType.Reference(BackendObjType.Native(jOutputStream))
            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

            // Locals.
            val idSlot = 2110
            val bufSlot = 2112
            val lenSlot = 2113
            val exSlot = 2114
            val sockSlot = 2115

            // Extract (socketId, buffer) from the tuple argument.
            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(argTupleType.IndexField(0)) // tuple, id
            mv.visitVarInsn(LSTORE, idSlot) // tuple
            GETFIELD(argTupleType.IndexField(1)) // buffer
            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.Int8).toDescriptor)
            ASTORE(bufSlot)

            // socket = Global.getTcpSocket(id)
            LLOAD(idSlot)
            INVOKESTATIC(jGlobal, "getTcpSocket", mkDescriptor(BackendType.Int64)(socketTpe))
            ASTORE(sockSlot)

            val hasSocket = new Label()
            val after = new Label()
            ALOAD(sockSlot)
            mv.visitJumpInsn(IFNONNULL, hasSocket)

            // Return (false, 0, "invalid TCP socket handle.")
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            pushString("invalid TCP socket handle.")
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(hasSocket)

            // Try-catch around socket I/O.
            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

            mv.visitLabel(tryStart)
            // socket.getOutputStream().write(buffer)
            ALOAD(sockSlot)
            INVOKEVIRTUAL(jSocket, "getOutputStream", mkDescriptor()(outputStreamTpe))
            ALOAD(bufSlot)
            INVOKEVIRTUAL(jOutputStream, "write", mkDescriptor(BackendType.Array(BackendType.Int8))(VoidableType.Void))

            // len = buffer.length
            ALOAD(bufSlot)
            ARRAYLENGTH()
            mv.visitVarInsn(ISTORE, lenSlot)

            // Return (true, len, "")
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(true)
            ILOAD(lenSlot)
            pushString("")
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            ASTORE(exSlot)
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            ICONST_0()
            ALOAD(exSlot)
            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

          case IoOp.TcpSocketConnect =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)

            val SimpleType.Tuple(retElmTypes) = tpe
            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

            val SimpleType.Tuple(argElmTypes) = exp.tpe
            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

            val jGlobal = BackendObjType.Global.jvmName
            val jInetAddress = JvmName.ofClass(classOf[java.net.InetAddress])
            val inetAddressTpe = BackendType.Reference(BackendObjType.Native(jInetAddress))
            val jSocket = JvmName.ofClass(classOf[java.net.Socket])
            val socketTpe = BackendType.Reference(BackendObjType.Native(jSocket))
            val jIOException = JvmName.ofClass(classOf[java.io.IOException])
            val jIllegalArg = JvmName.ofClass(classOf[java.lang.IllegalArgumentException])

            // Locals.
            val bytesSlot = 2160
            val portSlot = 2161
            val inetSlot = 2162
            val sockSlot = 2163
            val idSlot = 2164
            val exSlot = 2166

            // Extract (addrBytes, port) from the tuple argument.
            compileExpr(exp) // tuple
            DUP() // tuple, tuple
            GETFIELD(argTupleType.IndexField(0)) // tuple, bytes
            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.Int8).toDescriptor)
            ASTORE(bytesSlot) // tuple
            GETFIELD(argTupleType.IndexField(1)) // port
            mv.visitVarInsn(ISTORE, portSlot)

            // Try-catch around connect.
            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerInvalid = new Label()
            val handlerIo = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jIllegalArg.toInternalName)
            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

            mv.visitLabel(tryStart)

            // inet = InetAddress.getByAddress(bytes)
            ALOAD(bytesSlot)
            INVOKESTATIC(jInetAddress, "getByAddress", mkDescriptor(BackendType.Array(BackendType.Int8))(inetAddressTpe))
            ASTORE(inetSlot)

            // sock = new Socket(inet, port)
            NEW(jSocket)
            DUP()
            ALOAD(inetSlot)
            ILOAD(portSlot)
            INVOKESPECIAL(jSocket, JvmName.ConstructorMethod, mkDescriptor(inetAddressTpe, BackendType.Int32)(VoidableType.Void))
            ASTORE(sockSlot)

            // id = Global.newId()
            INVOKESTATIC(jGlobal, "newId", mkDescriptor()(BackendType.Int64))
            mv.visitVarInsn(LSTORE, idSlot)

            // Global.putTcpSocket(id, sock)
            LLOAD(idSlot)
            ALOAD(sockSlot)
            INVOKESTATIC(jGlobal, "putTcpSocket", mkDescriptor(BackendType.Int64, socketTpe)(VoidableType.Void))

            // Return (true, id, Other, "")
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(true)
            LLOAD(idSlot)
            pushInt(14)
            pushString("")
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerInvalid)
            ASTORE(exSlot)
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            LCONST_0()
            pushInt(4)
            ALOAD(exSlot)
            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerIo)
            ASTORE(exSlot)
            NEW(retTupleType.jvmName)
            DUP()
            pushBool(false)
            LCONST_0()
            pushInt(14)
            ALOAD(exSlot)
            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
            INVOKESPECIAL(retTupleType.Constructor)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(after)

	          case IoOp.TcpSocketClose =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jSocket = JvmName.ofClass(classOf[java.net.Socket])
	            val socketTpe = BackendType.Reference(BackendObjType.Native(jSocket))
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2120
	            val sockSlot = 2122
	            val exSlot = 2123

	            // Extract socket id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // socket = Global.removeTcpSocket(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "removeTcpSocket", mkDescriptor(BackendType.Int64)(socketTpe))
	            ASTORE(sockSlot)

	            val hasSocket = new Label()
	            val after = new Label()
	            ALOAD(sockSlot)
	            mv.visitJumpInsn(IFNONNULL, hasSocket)

	            // Return (true, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasSocket)

	            // Try-catch around socket.close().
	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerStart = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(sockSlot)
	            INVOKEVIRTUAL(jSocket, "close", mkDescriptor()(VoidableType.Void))

	            // Return (true, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerStart)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.TcpServerBind =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val SimpleType.Tuple(argElmTypes) = exp.tpe
	            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jInetAddress = JvmName.ofClass(classOf[java.net.InetAddress])
	            val inetAddressTpe = BackendType.Reference(BackendObjType.Native(jInetAddress))
	            val jServerSocket = JvmName.ofClass(classOf[java.net.ServerSocket])
	            val serverSocketTpe = BackendType.Reference(BackendObjType.Native(jServerSocket))
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])
	            val jIllegalArg = JvmName.ofClass(classOf[java.lang.IllegalArgumentException])

	            // Locals.
	            val bytesSlot = 2170
	            val portSlot = 2171
	            val inetSlot = 2172
	            val serverSlot = 2173
	            val idSlot = 2174
	            val exSlot = 2176

	            // Extract (addrBytes, port) from the tuple argument.
	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(0)) // tuple, bytes
	            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.Int8).toDescriptor)
	            ASTORE(bytesSlot) // tuple
	            GETFIELD(argTupleType.IndexField(1)) // port
	            mv.visitVarInsn(ISTORE, portSlot)

	            // Try-catch around bind.
	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerInvalid = new Label()
	            val handlerIo = new Label()
	            val after = new Label()

	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jIllegalArg.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)

	            // inet = InetAddress.getByAddress(bytes)
	            ALOAD(bytesSlot)
	            INVOKESTATIC(jInetAddress, "getByAddress", mkDescriptor(BackendType.Array(BackendType.Int8))(inetAddressTpe))
	            ASTORE(inetSlot)

	            // server = new ServerSocket(port, 50, inet)
	            NEW(jServerSocket)
	            DUP()
	            ILOAD(portSlot)
	            pushInt(50)
	            ALOAD(inetSlot)
	            INVOKESPECIAL(jServerSocket, JvmName.ConstructorMethod, mkDescriptor(BackendType.Int32, BackendType.Int32, inetAddressTpe)(VoidableType.Void))
	            ASTORE(serverSlot)

	            // id = Global.newId()
	            INVOKESTATIC(jGlobal, "newId", mkDescriptor()(BackendType.Int64))
	            mv.visitVarInsn(LSTORE, idSlot)

	            // Global.putTcpServer(id, server)
	            LLOAD(idSlot)
	            ALOAD(serverSlot)
	            INVOKESTATIC(jGlobal, "putTcpServer", mkDescriptor(BackendType.Int64, serverSocketTpe)(VoidableType.Void))

	            // Return (true, id, Other, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            LLOAD(idSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerInvalid)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(4)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.TcpServerAccept =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jServerSocket = JvmName.ofClass(classOf[java.net.ServerSocket])
	            val serverSocketTpe = BackendType.Reference(BackendObjType.Native(jServerSocket))
	            val jSocket = JvmName.ofClass(classOf[java.net.Socket])
	            val socketTpe = BackendType.Reference(BackendObjType.Native(jSocket))
	            val jSocketTimeout = JvmName.ofClass(classOf[java.net.SocketTimeoutException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val serverIdSlot = 2180
	            val serverSlot = 2182
	            val sockSlot = 2183
	            val sockIdSlot = 2184
	            val exSlot = 2186

	            // Extract server id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, serverIdSlot)

	            // server = Global.getTcpServer(id)
	            LLOAD(serverIdSlot)
	            INVOKESTATIC(jGlobal, "getTcpServer", mkDescriptor(BackendType.Int64)(serverSocketTpe))
	            ASTORE(serverSlot)

	            val hasServer = new Label()
	            val after = new Label()
	            ALOAD(serverSlot)
	            mv.visitJumpInsn(IFNONNULL, hasServer)

	            // Return (false, 0, Other, "invalid TCP server handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            pushString("invalid TCP server handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasServer)

	            // Try-catch around accept.
	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerTimeout = new Label()
	            val handlerIo = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerTimeout, jSocketTimeout.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)

	            // sock = server.accept()
	            ALOAD(serverSlot)
	            INVOKEVIRTUAL(jServerSocket, "accept", mkDescriptor()(socketTpe))
	            ASTORE(sockSlot)

	            // sockId = Global.newId()
	            INVOKESTATIC(jGlobal, "newId", mkDescriptor()(BackendType.Int64))
	            mv.visitVarInsn(LSTORE, sockIdSlot)

	            // Global.putTcpSocket(sockId, sock)
	            LLOAD(sockIdSlot)
	            ALOAD(sockSlot)
	            INVOKESTATIC(jGlobal, "putTcpSocket", mkDescriptor(BackendType.Int64, socketTpe)(VoidableType.Void))

	            // Return (true, sockId, Other, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            LLOAD(sockIdSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerTimeout)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(10)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.TcpServerLocalPort =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jServerSocket = JvmName.ofClass(classOf[java.net.ServerSocket])
	            val serverSocketTpe = BackendType.Reference(BackendObjType.Native(jServerSocket))

	            // Locals.
	            val serverIdSlot = 2310
	            val serverSlot = 2312
	            val portSlot = 2313

	            // Extract server id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, serverIdSlot)

	            // server = Global.getTcpServer(id)
	            LLOAD(serverIdSlot)
	            INVOKESTATIC(jGlobal, "getTcpServer", mkDescriptor(BackendType.Int64)(serverSocketTpe))
	            ASTORE(serverSlot)

	            val hasServer = new Label()
	            val after = new Label()
	            ALOAD(serverSlot)
	            mv.visitJumpInsn(IFNONNULL, hasServer)

	            // Return (false, 0, "invalid TCP server handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushInt(0)
	            pushString("invalid TCP server handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasServer)

	            // port = server.getLocalPort()
	            ALOAD(serverSlot)
	            INVOKEVIRTUAL(jServerSocket, "getLocalPort", mkDescriptor()(BackendType.Int32))
	            mv.visitVarInsn(ISTORE, portSlot)

	            // Return (true, port, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(portSlot)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.TcpServerClose =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jServerSocket = JvmName.ofClass(classOf[java.net.ServerSocket])
	            val serverSocketTpe = BackendType.Reference(BackendObjType.Native(jServerSocket))
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2130
	            val serverSlot = 2132
	            val exSlot = 2133

	            // Extract server id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // server = Global.removeTcpServer(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "removeTcpServer", mkDescriptor(BackendType.Int64)(serverSocketTpe))
	            ASTORE(serverSlot)

	            val hasServer = new Label()
	            val after = new Label()
	            ALOAD(serverSlot)
	            mv.visitJumpInsn(IFNONNULL, hasServer)

	            // Return (true, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasServer)

	            // Try-catch around server.close().
	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerStart = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(serverSlot)
	            INVOKEVIRTUAL(jServerSocket, "close", mkDescriptor()(VoidableType.Void))

	            // Return (true, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerStart)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessExec =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val SimpleType.Tuple(argElmTypes) = exp.tpe
	            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jProcessBuilder = JvmName.ofClass(classOf[java.lang.ProcessBuilder])
	            val processBuilderTpe = BackendType.Reference(BackendObjType.Native(jProcessBuilder))
	            val jFile = JvmName.ofClass(classOf[java.io.File])
	            val fileTpe = BackendType.Reference(BackendObjType.Native(jFile))
	            val jMap = JvmName(JvmName.JavaUtil, "Map")
	            val jIllegalArg = JvmName.ofClass(classOf[java.lang.IllegalArgumentException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val argvSlot = 2190
	            val hasCwdSlot = 2191
	            val cwdSlot = 2192
	            val envPairsSlot = 2193
	            val pbSlot = 2194
	            val envMapSlot = 2195
	            val idxSlot = 2196
	            val procSlot = 2197
	            val idSlot = 2198
	            val exSlot = 2200

	            // Extract (argv, hasCwd, cwd, envPairs) from the tuple argument.
	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(0)) // tuple, argv
	            ASTORE(argvSlot) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(1)) // tuple, hasCwd
	            mv.visitVarInsn(ISTORE, hasCwdSlot) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(2)) // tuple, cwd
	            ASTORE(cwdSlot) // tuple
	            GETFIELD(argTupleType.IndexField(3)) // envPairs
	            ASTORE(envPairsSlot)

	            // Try-catch around process execution.
	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerInvalid = new Label()
	            val handlerIo = new Label()
	            val after = new Label()

	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jIllegalArg.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)

	            // pb = new ProcessBuilder(argv)
	            NEW(jProcessBuilder)
	            DUP()
	            ALOAD(argvSlot)
	            INVOKESPECIAL(jProcessBuilder, JvmName.ConstructorMethod, mkDescriptor(BackendType.Array(BackendType.String))(VoidableType.Void))
	            ASTORE(pbSlot)

	            // env = pb.environment()
	            ALOAD(pbSlot)
	            INVOKEVIRTUAL(jProcessBuilder, "environment", mkDescriptor()(jMap.toTpe))
	            ASTORE(envMapSlot)

	            // for (i = 0; i < envPairs.length; i += 2) env.put(envPairs[i], envPairs[i+1])
	            ICONST_0()
	            mv.visitVarInsn(ISTORE, idxSlot)

	            val loopStart = new Label()
	            val loopEnd = new Label()
	            mv.visitLabel(loopStart)

	            ILOAD(idxSlot)
	            ALOAD(envPairsSlot)
	            ARRAYLENGTH()
	            mv.visitJumpInsn(IF_ICMPGE, loopEnd)

	            // env.put(envPairs[i], envPairs[i+1])
	            ALOAD(envMapSlot)
	            ALOAD(envPairsSlot)
	            ILOAD(idxSlot)
	            mv.visitInsn(AALOAD)
	            ALOAD(envPairsSlot)
	            ILOAD(idxSlot)
	            ICONST_1()
	            IADD()
	            mv.visitInsn(AALOAD)
	            INVOKEINTERFACE(jMap, "put", mkDescriptor(BackendType.Object, BackendType.Object)(BackendType.Object))
	            POP()

	            mv.visitIincInsn(idxSlot, 2)
	            mv.visitJumpInsn(GOTO, loopStart)

	            mv.visitLabel(loopEnd)

	            // if (hasCwd) pb.directory(new File(cwd))
	            val skipCwd = new Label()
	            ILOAD(hasCwdSlot)
	            mv.visitJumpInsn(IFEQ, skipCwd)

	            ALOAD(pbSlot)
	            NEW(jFile)
	            DUP()
	            ALOAD(cwdSlot)
	            INVOKESPECIAL(jFile, JvmName.ConstructorMethod, mkDescriptor(BackendType.String)(VoidableType.Void))
	            INVOKEVIRTUAL(jProcessBuilder, "directory", mkDescriptor(fileTpe)(processBuilderTpe))
	            POP()

	            mv.visitLabel(skipCwd)

	            // proc = pb.start()
	            ALOAD(pbSlot)
	            INVOKEVIRTUAL(jProcessBuilder, "start", mkDescriptor()(processTpe))
	            ASTORE(procSlot)

	            // id = Global.newId()
	            INVOKESTATIC(jGlobal, "newId", mkDescriptor()(BackendType.Int64))
	            mv.visitVarInsn(LSTORE, idSlot)

	            // Global.putProcess(id, proc)
	            LLOAD(idSlot)
	            ALOAD(procSlot)
	            INVOKESTATIC(jGlobal, "putProcess", mkDescriptor(BackendType.Int64, processTpe)(VoidableType.Void))

	            // Return (true, id, Other, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            LLOAD(idSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerInvalid)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(4)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessExitValue =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jIllegalThreadState = JvmName.ofClass(classOf[java.lang.IllegalThreadStateException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2210
	            val procSlot = 2212
	            val exitSlot = 2213
	            val exSlot = 2214

	            // Extract process id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, 0, Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerStart = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIllegalThreadState.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            INVOKEVIRTUAL(jProcess, "exitValue", mkDescriptor()(BackendType.Int32))
	            mv.visitVarInsn(ISTORE, exitSlot)

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(exitSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerStart)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessIsAlive =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2220
	            val procSlot = 2222
	            val aliveSlot = 2223
	            val exSlot = 2224

	            // Extract process id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, false, Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushBool(false)
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerStart = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            INVOKEVIRTUAL(jProcess, "isAlive", mkDescriptor()(BackendType.Bool))
	            mv.visitVarInsn(ISTORE, aliveSlot)

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(aliveSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerStart)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushBool(false)
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessPid =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2230
	            val procSlot = 2232
	            val pidSlot = 2233
	            val exSlot = 2235

	            // Extract process id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, 0, Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerUnsupported = new Label()
	            val handlerIo = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            INVOKEVIRTUAL(jProcess, "pid", mkDescriptor()(BackendType.Int64))
	            mv.visitVarInsn(LSTORE, pidSlot)

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            LLOAD(pidSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerUnsupported)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(12)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            LCONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessStop =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2240
	            val procSlot = 2242
	            val exSlot = 2243

	            // Extract process id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, (), Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            GETSTATIC(BackendObjType.Unit.SingletonField)
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerStart = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            INVOKEVIRTUAL(jProcess, "destroy", mkDescriptor()(VoidableType.Void))

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            GETSTATIC(BackendObjType.Unit.SingletonField)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerStart)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            GETSTATIC(BackendObjType.Unit.SingletonField)
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessWaitFor =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jInterrupted = JvmName.ofClass(classOf[java.lang.InterruptedException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            // Locals.
	            val idSlot = 2250
	            val procSlot = 2252
	            val exitSlot = 2253
	            val exSlot = 2254

	            // Extract process id.
	            compileExpr(exp)
	            mv.visitVarInsn(LSTORE, idSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, 0, Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerInterrupted = new Label()
	            val handlerIo = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInterrupted, jInterrupted.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            INVOKEVIRTUAL(jProcess, "waitFor", mkDescriptor()(BackendType.Int32))
	            mv.visitVarInsn(ISTORE, exitSlot)

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(exitSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerInterrupted)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(2)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessWaitForTimeout =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val SimpleType.Tuple(argElmTypes) = exp.tpe
	            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
	            val jInterrupted = JvmName.ofClass(classOf[java.lang.InterruptedException])
	            val jIOException = JvmName.ofClass(classOf[java.io.IOException])
	            val jTimeUnit = JvmName.ofClass(classOf[java.util.concurrent.TimeUnit])
	            val timeUnitTpe = BackendType.Reference(BackendObjType.Native(jTimeUnit))

	            // Locals.
	            val idSlot = 2260
	            val msSlot = 2262
	            val procSlot = 2264
	            val resSlot = 2265
	            val exSlot = 2266

	            // Extract (processId, ms) from the tuple argument.
	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(0)) // tuple, id
	            mv.visitVarInsn(LSTORE, idSlot) // tuple
	            GETFIELD(argTupleType.IndexField(1)) // ms
	            mv.visitVarInsn(LSTORE, msSlot)

	            // proc = Global.getProcess(id)
	            LLOAD(idSlot)
	            INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            ASTORE(procSlot)

	            val hasProc = new Label()
	            val after = new Label()
	            ALOAD(procSlot)
	            mv.visitJumpInsn(IFNONNULL, hasProc)

	            // Return (false, false, Other, "invalid process handle.")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushBool(false)
	            pushInt(14)
	            pushString("invalid process handle.")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(hasProc)

	            val tryStart = new Label()
	            val tryEnd = new Label()
	            val handlerInterrupted = new Label()
	            val handlerIo = new Label()
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInterrupted, jInterrupted.toInternalName)
	            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)
	            ALOAD(procSlot)
	            LLOAD(msSlot)
	            mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jTimeUnit.toInternalName, "MILLISECONDS", timeUnitTpe.toDescriptor)
	            INVOKEVIRTUAL(jProcess, "waitFor", mkDescriptor(BackendType.Int64, timeUnitTpe)(BackendType.Bool))
	            mv.visitVarInsn(ISTORE, resSlot)

	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(resSlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerInterrupted)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushBool(false)
	            pushInt(2)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            pushBool(false)
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.ProcessStdinWrite =>
	            compileProcessStdinWrite(exp, tpe, loc)

	          case IoOp.ProcessStdoutRead =>
	            compileProcessStreamRead(exp, tpe, loc, stderr = false)

	          case IoOp.ProcessStderrRead =>
	            compileProcessStreamRead(exp, tpe, loc, stderr = true)

	          case IoOp.ProcessRelease =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val jGlobal = BackendObjType.Global.jvmName
	            val jProcess = JvmName.ofClass(classOf[java.lang.Process])
	            val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))

	            // Remove the process handle (if present) and return (true, "").
	            compileExpr(exp)
	            INVOKESTATIC(jGlobal, "removeProcess", mkDescriptor(BackendType.Int64)(processTpe))
	            POP()
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)

		          case IoOp.HttpRequest =>
		            import BytecodeInstructions.*
		            BytecodeInstructions.addLoc(loc)

	            val SimpleType.Tuple(retElmTypes) = tpe
	            val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

	            val SimpleType.Tuple(argElmTypes) = exp.tpe
	            val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

		            val jIllegalArg = JvmName.ofClass(classOf[java.lang.IllegalArgumentException])
		            val jConnectException = JvmName.ofClass(classOf[java.net.ConnectException])
		            val jUnknownHost = JvmName.ofClass(classOf[java.net.UnknownHostException])
		            val jInterrupted = JvmName.ofClass(classOf[java.lang.InterruptedException])
		            val jHttpTimeout = JvmName.ofClass(classOf[java.net.http.HttpTimeoutException])
		            val jUncheckedIo = JvmName.ofClass(classOf[java.io.UncheckedIOException])
		            val jIOException = JvmName.ofClass(classOf[java.io.IOException])

	            val jURI = JvmName.ofClass(classOf[java.net.URI])
	            val uriTpe = BackendType.Reference(BackendObjType.Native(jURI))

	            val jHttpClient = JvmName.ofClass(classOf[java.net.http.HttpClient])
	            val clientTpe = BackendType.Reference(BackendObjType.Native(jHttpClient))

	            val jHttpClientBuilder = JvmName.ofClass(classOf[java.net.http.HttpClient.Builder])
	            val clientBuilderTpe = BackendType.Reference(BackendObjType.Native(jHttpClientBuilder))

	            val jHttpRedirect = JvmName.ofClass(classOf[java.net.http.HttpClient.Redirect])
	            val redirectTpe = BackendType.Reference(BackendObjType.Native(jHttpRedirect))

	            val jHttpRequest = JvmName.ofClass(classOf[java.net.http.HttpRequest])
	            val requestTpe = BackendType.Reference(BackendObjType.Native(jHttpRequest))

	            val jHttpRequestBuilder = JvmName.ofClass(classOf[java.net.http.HttpRequest.Builder])
	            val builderTpe = BackendType.Reference(BackendObjType.Native(jHttpRequestBuilder))

	            val jBodyPublishers = JvmName.ofClass(classOf[java.net.http.HttpRequest.BodyPublishers])
	            val jBodyPublisher = JvmName.ofClass(classOf[java.net.http.HttpRequest.BodyPublisher])
	            val bodyPublisherTpe = BackendType.Reference(BackendObjType.Native(jBodyPublisher))

	            val jBodyHandlers = JvmName.ofClass(classOf[java.net.http.HttpResponse.BodyHandlers])
	            val jHttpResponse = JvmName(List("java", "net", "http"), "HttpResponse")
	            val responseTpe = BackendType.Reference(BackendObjType.Native(jHttpResponse))
	            val jBodyHandler = JvmName(List("java", "net", "http"), "HttpResponse$BodyHandler")
	            val bodyHandlerTpe = BackendType.Reference(BackendObjType.Native(jBodyHandler))

	            val jHttpHeaders = JvmName.ofClass(classOf[java.net.http.HttpHeaders])
	            val httpHeadersTpe = BackendType.Reference(BackendObjType.Native(jHttpHeaders))

	            val jLocale = JvmName.ofClass(classOf[java.util.Locale])
	            val localeTpe = BackendType.Reference(BackendObjType.Native(jLocale))

	            val jMap = JvmName(JvmName.JavaUtil, "Map")
	            val jSet = JvmName(JvmName.JavaUtil, "Set")
	            val jList = JvmName(JvmName.JavaUtil, "List")
	            val jOptional = JvmName(JvmName.JavaUtil, "Optional")
	            val jIterator = JvmName.Iterator
	            val jMapEntry = JvmName(JvmName.JavaUtil, "Map$Entry")
	            val jArrayList = JvmName(JvmName.JavaUtil, "ArrayList")
	            val optionalTpe = BackendType.Reference(BackendObjType.Native(jOptional))

	            // Locals.
	            val methodSlot = 2280
	            val urlSlot = 2281
	            val reqHeadersSlot = 2282
	            val hasBodySlot = 2283
	            val reqBodySlot = 2284
	            val publisherSlot = 2285
	            val builderSlot = 2286
	            val requestSlot = 2287
	            val clientSlot = 2288
	            val handlerSlot = 2289
	            val responseSlot = 2290
	            val statusSlot = 2291
	            val respBodySlot = 2292
	            val respHeadersSlot = 2293
	            val headerBufSlot = 2294
	            val respMapSlot = 2295
	            val iterSlot = 2296
	            val entrySlot = 2297
	            val keySlot = 2298
	            val valuesSlot = 2299
	            val valuesIterSlot = 2300
	            val valueSlot = 2301
	            val exSlot = 2302
	            val uriSlot = 2303
	            val schemeSlot = 2304
	            val locationOptSlot = 2305
	            val locationSlot = 2306
	            val locationUriSlot = 2307
	            val locationSchemeSlot = 2308

	            val isPortable = flix.options.stdlibProfile == StdlibProfile.Portable

	            // Extract (method, url, reqHeaders, hasBody, body) from the tuple argument.
	            compileExpr(exp) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(0)) // tuple, method
	            CHECKCAST(JvmName.String) // tuple, method
	            ASTORE(methodSlot) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(1)) // tuple, url
	            CHECKCAST(JvmName.String) // tuple, url
	            ASTORE(urlSlot) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(2)) // tuple, headers
	            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.String).toDescriptor) // tuple, headers
	            ASTORE(reqHeadersSlot) // tuple
	            DUP() // tuple, tuple
	            GETFIELD(argTupleType.IndexField(3)) // tuple, hasBody
	            mv.visitVarInsn(ISTORE, hasBodySlot) // tuple
	            GETFIELD(argTupleType.IndexField(4)) // body
	            CHECKCAST(JvmName.String) // body
	            ASTORE(reqBodySlot)

	            val after = new Label()

	            if (isPortable) {
	              // Validate: if hasBody is false then the body must be empty.
	              val inputOk = new Label()
	              ILOAD(hasBodySlot)
	              mv.visitJumpInsn(IFNE, inputOk)
	              ALOAD(reqBodySlot)
	              INVOKEVIRTUAL(JvmName.String, "isEmpty", mkDescriptor()(BackendType.Bool))
	              mv.visitJumpInsn(IFNE, inputOk)

	              // Return (false, 0, [], "", InvalidInput, "invalid input")
	              NEW(retTupleType.jvmName)
	              DUP()
	              pushBool(false)
	              ICONST_0()
	              pushInt(0)
	              ANEWARRAY(JvmName.String)
	              pushString("")
	              pushInt(4)
	              pushString("invalid input")
	              INVOKESPECIAL(retTupleType.Constructor)
	              mv.visitJumpInsn(GOTO, after)

	              mv.visitLabel(inputOk)
	            }

	            // Try-catch around HTTP request execution.
	            val tryStart = new Label()
	            val tryEnd = new Label()
		            val handlerInvalid = new Label()
		            val handlerConnect = new Label()
		            val handlerUnknownHost = new Label()
		            val handlerInterrupted = new Label()
		            val handlerTimeout = new Label()
		            val handlerUncheckedIo = new Label()
		            val handlerIo = new Label()

		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jIllegalArg.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerConnect, jConnectException.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnknownHost, jUnknownHost.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerInterrupted, jInterrupted.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerTimeout, jHttpTimeout.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerUncheckedIo, jUncheckedIo.toInternalName)
		            mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

	            mv.visitLabel(tryStart)

	            // uri = URI.create(url)
	            ALOAD(urlSlot)
	            INVOKESTATIC(jURI, "create", mkDescriptor(BackendType.String)(uriTpe))
	            ASTORE(uriSlot)

	            if (isPortable) {
	              // If scheme is not http/https: return (false, 0, [], "", Unsupported, "unsupported URL scheme")
	              val supportedScheme = new Label()
	              ALOAD(uriSlot)
	              INVOKEVIRTUAL(jURI, "getScheme", mkDescriptor()(BackendType.String))
	              ASTORE(schemeSlot)

	              ALOAD(schemeSlot)
	              // Leave invalid/relative URI detection to HttpRequest.newBuilder (-> InvalidInput).
	              mv.visitJumpInsn(IFNULL, supportedScheme)
	              ALOAD(schemeSlot)
	              pushString("http")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFNE, supportedScheme)
	              ALOAD(schemeSlot)
	              pushString("https")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFNE, supportedScheme)

	              NEW(retTupleType.jvmName)
	              DUP()
	              pushBool(false)
	              ICONST_0()
	              pushInt(0)
	              ANEWARRAY(JvmName.String)
	              pushString("")
	              pushInt(12)
	              pushString("unsupported URL scheme")
	              INVOKESPECIAL(retTupleType.Constructor)
	              mv.visitJumpInsn(GOTO, after)

	              mv.visitLabel(supportedScheme)
	            }

	            // builder = HttpRequest.newBuilder(uri)
	            ALOAD(uriSlot)
	            INVOKESTATIC(jHttpRequest, "newBuilder", mkDescriptor(uriTpe)(builderTpe))
	            ASTORE(builderSlot)

	            // publisher = hasBody ? BodyPublishers.ofString(body) : BodyPublishers.noBody()
	            val noBody = new Label()
	            val afterPub = new Label()
	            ILOAD(hasBodySlot)
	            mv.visitJumpInsn(IFEQ, noBody)
	            ALOAD(reqBodySlot)
	            INVOKESTATIC(jBodyPublishers, "ofString", mkDescriptor(BackendType.String)(bodyPublisherTpe))
	            mv.visitJumpInsn(GOTO, afterPub)
	            mv.visitLabel(noBody)
	            INVOKESTATIC(jBodyPublishers, "noBody", mkDescriptor()(bodyPublisherTpe))
	            mv.visitLabel(afterPub)
	            ASTORE(publisherSlot)

	            // builder = builder.method(method, publisher)
	            ALOAD(builderSlot)
	            ALOAD(methodSlot)
	            ALOAD(publisherSlot)
	            INVOKEINTERFACE(jHttpRequestBuilder, "method", mkDescriptor(BackendType.String, bodyPublisherTpe)(builderTpe))
	            ASTORE(builderSlot)

	            // if (reqHeaders.length > 0) builder.headers(reqHeaders)
	            val skipHeaders = new Label()
	            ALOAD(reqHeadersSlot)
	            ARRAYLENGTH()
	            mv.visitJumpInsn(IFEQ, skipHeaders)
	            ALOAD(builderSlot)
	            ALOAD(reqHeadersSlot)
	            INVOKEINTERFACE(jHttpRequestBuilder, "headers", mkDescriptor(BackendType.Array(BackendType.String))(builderTpe))
	            POP()
	            mv.visitLabel(skipHeaders)

	            // request = builder.build()
	            ALOAD(builderSlot)
	            INVOKEINTERFACE(jHttpRequestBuilder, "build", mkDescriptor()(requestTpe))
	            ASTORE(requestSlot)

	            if (isPortable) {
	              // client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
	              INVOKESTATIC(jHttpClient, "newBuilder", mkDescriptor()(clientBuilderTpe))
	              GETSTATIC(ClassMaker.StaticField(jHttpRedirect, "NORMAL", redirectTpe))
	              INVOKEINTERFACE(jHttpClientBuilder, "followRedirects", mkDescriptor(redirectTpe)(clientBuilderTpe))
	              INVOKEINTERFACE(jHttpClientBuilder, "build", mkDescriptor()(clientTpe))
	              ASTORE(clientSlot)
	            } else {
	              // client = HttpClient.newHttpClient()
	              INVOKESTATIC(jHttpClient, "newHttpClient", mkDescriptor()(clientTpe))
	              ASTORE(clientSlot)
	            }

	            // handler = BodyHandlers.ofString()
	            INVOKESTATIC(jBodyHandlers, "ofString", mkDescriptor()(bodyHandlerTpe))
	            ASTORE(handlerSlot)

	            // response = client.send(request, handler)
	            ALOAD(clientSlot)
	            ALOAD(requestSlot)
	            ALOAD(handlerSlot)
	            INVOKEVIRTUAL(jHttpClient, "send", mkDescriptor(requestTpe, bodyHandlerTpe)(responseTpe))
	            ASTORE(responseSlot)

	            // status = response.statusCode()
	            ALOAD(responseSlot)
	            INVOKEINTERFACE(jHttpResponse, "statusCode", mkDescriptor()(BackendType.Int32))
	            mv.visitVarInsn(ISTORE, statusSlot)

	            if (isPortable) {
	              // Portable contract: redirects are followed by default. If we still see a redirect with a Location header,
	              // treat it as a failure (Unsupported if the target scheme is unsupported; otherwise Other).
	              val notRedirect = new Label()
	              val isRedirect = new Label()

	              ILOAD(statusSlot)
	              pushInt(301)
	              mv.visitJumpInsn(IF_ICMPEQ, isRedirect)
	              ILOAD(statusSlot)
	              pushInt(302)
	              mv.visitJumpInsn(IF_ICMPEQ, isRedirect)
	              ILOAD(statusSlot)
	              pushInt(303)
	              mv.visitJumpInsn(IF_ICMPEQ, isRedirect)
	              ILOAD(statusSlot)
	              pushInt(307)
	              mv.visitJumpInsn(IF_ICMPEQ, isRedirect)
	              ILOAD(statusSlot)
	              pushInt(308)
	              mv.visitJumpInsn(IF_ICMPEQ, isRedirect)
	              mv.visitJumpInsn(GOTO, notRedirect)

	              mv.visitLabel(isRedirect)

	              // locationOpt = response.headers().firstValue("location")
	              ALOAD(responseSlot)
	              INVOKEINTERFACE(jHttpResponse, "headers", mkDescriptor()(httpHeadersTpe))
	              pushString("location")
	              INVOKEVIRTUAL(jHttpHeaders, "firstValue", mkDescriptor(BackendType.String)(optionalTpe))
	              ASTORE(locationOptSlot)

	              // If no Location header: treat as a normal response.
	              ALOAD(locationOptSlot)
	              INVOKEVIRTUAL(jOptional, "isPresent", mkDescriptor()(BackendType.Bool))
	              mv.visitJumpInsn(IFEQ, notRedirect)

	              // location = (String) locationOpt.get()
	              ALOAD(locationOptSlot)
	              INVOKEVIRTUAL(jOptional, "get", mkDescriptor()(BackendType.Object))
	              CHECKCAST(JvmName.String)
	              ASTORE(locationSlot)

	              // scheme = URI.create(location).getScheme()
	              ALOAD(locationSlot)
	              INVOKESTATIC(jURI, "create", mkDescriptor(BackendType.String)(uriTpe))
	              ASTORE(locationUriSlot)
	              ALOAD(locationUriSlot)
	              INVOKEVIRTUAL(jURI, "getScheme", mkDescriptor()(BackendType.String))
	              ASTORE(locationSchemeSlot)

	              val checkRedirectScheme = new Label()
	              val redirectOther = new Label()

	              // If original request is https and redirect target is http, treat as a policy violation.
	              // (Matches HttpClient.Redirect.NORMAL semantics; avoids silent downgrade.)
	              ALOAD(schemeSlot)
	              mv.visitJumpInsn(IFNULL, checkRedirectScheme)
	              ALOAD(schemeSlot)
	              pushString("https")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFEQ, checkRedirectScheme)
	              ALOAD(locationSchemeSlot)
	              mv.visitJumpInsn(IFNULL, checkRedirectScheme)
	              ALOAD(locationSchemeSlot)
	              pushString("http")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFEQ, checkRedirectScheme)

	              NEW(retTupleType.jvmName)
	              DUP()
	              pushBool(false)
	              ICONST_0()
	              pushInt(0)
	              ANEWARRAY(JvmName.String)
	              pushString("")
	              pushInt(9)
	              pushString("redirect disallowed: https -> http")
	              INVOKESPECIAL(retTupleType.Constructor)
	              mv.visitJumpInsn(GOTO, after)

	              mv.visitLabel(checkRedirectScheme)

	              // If scheme is null (relative redirect), treat as Other since we didn't follow it.
	              ALOAD(locationSchemeSlot)
	              mv.visitJumpInsn(IFNULL, redirectOther)
	              // If scheme is http/https, treat as Other since we didn't follow it.
	              ALOAD(locationSchemeSlot)
	              pushString("http")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFNE, redirectOther)
	              ALOAD(locationSchemeSlot)
	              pushString("https")
	              INVOKEVIRTUAL(JvmName.String, "equalsIgnoreCase", mkDescriptor(BackendType.String)(BackendType.Bool))
	              mv.visitJumpInsn(IFNE, redirectOther)

	              // Unsupported redirect target scheme.
	              NEW(retTupleType.jvmName)
	              DUP()
	              pushBool(false)
	              ICONST_0()
	              pushInt(0)
	              ANEWARRAY(JvmName.String)
	              pushString("")
	              pushInt(12)
	              pushString("unsupported redirect scheme")
	              INVOKESPECIAL(retTupleType.Constructor)
	              mv.visitJumpInsn(GOTO, after)

	              mv.visitLabel(redirectOther)
	              NEW(retTupleType.jvmName)
	              DUP()
	              pushBool(false)
	              ICONST_0()
	              pushInt(0)
	              ANEWARRAY(JvmName.String)
	              pushString("")
	              pushInt(14)
	              pushString("redirect not followed")
	              INVOKESPECIAL(retTupleType.Constructor)
	              mv.visitJumpInsn(GOTO, after)

	              mv.visitLabel(notRedirect)
	            }

	            // body = response.body().toString()
	            ALOAD(responseSlot)
	            INVOKEINTERFACE(jHttpResponse, "body", mkDescriptor()(BackendType.Object))
	            INVOKEVIRTUAL(JvmName.Object, "toString", mkDescriptor()(BackendType.String))
	            ASTORE(respBodySlot)

	            // respMap = response.headers().map()
	            ALOAD(responseSlot)
	            INVOKEINTERFACE(jHttpResponse, "headers", mkDescriptor()(httpHeadersTpe))
	            INVOKEVIRTUAL(jHttpHeaders, "map", mkDescriptor()(jMap.toTpe))
	            ASTORE(respMapSlot)

	            // headerBuf = new ArrayList()
	            NEW(jArrayList)
	            DUP()
	            INVOKESPECIAL(jArrayList, JvmName.ConstructorMethod, mkDescriptor()(VoidableType.Void))
	            ASTORE(headerBufSlot)

	            // iter = respMap.entrySet().iterator()
	            ALOAD(respMapSlot)
	            INVOKEINTERFACE(jMap, "entrySet", mkDescriptor()(jSet.toTpe))
	            INVOKEINTERFACE(jSet, "iterator", mkDescriptor()(jIterator.toTpe))
	            ASTORE(iterSlot)

	            val loopStart = new Label()
	            val loopEnd = new Label()
	            mv.visitLabel(loopStart)

	            // while (iter.hasNext())
	            ALOAD(iterSlot)
	            INVOKEINTERFACE(jIterator, "hasNext", mkDescriptor()(BackendType.Bool))
	            mv.visitJumpInsn(IFEQ, loopEnd)

	            // entry = (Map.Entry) iter.next()
	            ALOAD(iterSlot)
	            INVOKEINTERFACE(jIterator, "next", mkDescriptor()(BackendType.Object))
	            CHECKCAST(jMapEntry)
	            ASTORE(entrySlot)

	            // key = ((String) entry.getKey()).toLowerCase(Locale.ROOT)
	            ALOAD(entrySlot)
	            INVOKEINTERFACE(jMapEntry, "getKey", mkDescriptor()(BackendType.Object))
	            CHECKCAST(JvmName.String)
	            GETSTATIC(ClassMaker.StaticField(jLocale, "ROOT", localeTpe))
	            INVOKEVIRTUAL(JvmName.String, "toLowerCase", mkDescriptor(localeTpe)(BackendType.String))
	            ASTORE(keySlot)

	            // values = (List) entry.getValue()
	            ALOAD(entrySlot)
	            INVOKEINTERFACE(jMapEntry, "getValue", mkDescriptor()(BackendType.Object))
	            CHECKCAST(jList)
	            ASTORE(valuesSlot)

	            // valuesIter = values.iterator()
	            ALOAD(valuesSlot)
	            INVOKEINTERFACE(jList, "iterator", mkDescriptor()(jIterator.toTpe))
	            ASTORE(valuesIterSlot)

	            val innerStart = new Label()
	            val innerEnd = new Label()
	            mv.visitLabel(innerStart)

	            // while (valuesIter.hasNext())
	            ALOAD(valuesIterSlot)
	            INVOKEINTERFACE(jIterator, "hasNext", mkDescriptor()(BackendType.Bool))
	            mv.visitJumpInsn(IFEQ, innerEnd)

	            // value = (String) valuesIter.next()
	            ALOAD(valuesIterSlot)
	            INVOKEINTERFACE(jIterator, "next", mkDescriptor()(BackendType.Object))
	            CHECKCAST(JvmName.String)
	            ASTORE(valueSlot)

	            // headerBuf.add(key); headerBuf.add(value)
	            ALOAD(headerBufSlot)
	            ALOAD(keySlot)
	            INVOKEVIRTUAL(jArrayList, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))
	            POP()
	            ALOAD(headerBufSlot)
	            ALOAD(valueSlot)
	            INVOKEVIRTUAL(jArrayList, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))
	            POP()

	            mv.visitJumpInsn(GOTO, innerStart)
	            mv.visitLabel(innerEnd)

	            mv.visitJumpInsn(GOTO, loopStart)
	            mv.visitLabel(loopEnd)

	            // respHeaders = (String[]) headerBuf.toArray(new String[0])
	            ALOAD(headerBufSlot)
	            pushInt(0)
	            ANEWARRAY(JvmName.String)
	            INVOKEVIRTUAL(jArrayList, "toArray", mkDescriptor(BackendType.Array(BackendType.Object))(BackendType.Array(BackendType.Object)))
	            mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.String).toDescriptor)
	            ASTORE(respHeadersSlot)

	            // Return (true, status, respHeaders, body, Other, "")
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(true)
	            ILOAD(statusSlot)
	            ALOAD(respHeadersSlot)
	            ALOAD(respBodySlot)
	            pushInt(14)
	            pushString("")
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitLabel(tryEnd)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerInvalid)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(0)
	            ANEWARRAY(JvmName.String)
	            pushString("")
	            pushInt(4)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

		            mv.visitLabel(handlerConnect)
		            ASTORE(exSlot)
		            NEW(retTupleType.jvmName)
		            DUP()
		            pushBool(false)
		            ICONST_0()
		            pushInt(0)
		            ANEWARRAY(JvmName.String)
		            pushString("")
		            pushInt(1)
		            ALOAD(exSlot)
		            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
		            INVOKESPECIAL(retTupleType.Constructor)
		            mv.visitJumpInsn(GOTO, after)

		            mv.visitLabel(handlerUnknownHost)
		            ASTORE(exSlot)
		            NEW(retTupleType.jvmName)
		            DUP()
		            pushBool(false)
		            ICONST_0()
		            pushInt(0)
		            ANEWARRAY(JvmName.String)
		            pushString("")
		            pushInt(13)
		            ALOAD(exSlot)
		            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
		            INVOKESPECIAL(retTupleType.Constructor)
		            mv.visitJumpInsn(GOTO, after)

		            mv.visitLabel(handlerInterrupted)
		            ASTORE(exSlot)
		            NEW(retTupleType.jvmName)
		            DUP()
		            pushBool(false)
		            ICONST_0()
		            pushInt(0)
		            ANEWARRAY(JvmName.String)
		            pushString("")
		            pushInt(2)
		            ALOAD(exSlot)
		            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
		            INVOKESPECIAL(retTupleType.Constructor)
		            mv.visitJumpInsn(GOTO, after)

		            mv.visitLabel(handlerTimeout)
		            ASTORE(exSlot)
		            NEW(retTupleType.jvmName)
		            DUP()
		            pushBool(false)
		            ICONST_0()
		            pushInt(0)
		            ANEWARRAY(JvmName.String)
		            pushString("")
		            pushInt(10)
		            ALOAD(exSlot)
		            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
		            INVOKESPECIAL(retTupleType.Constructor)
		            mv.visitJumpInsn(GOTO, after)

		            mv.visitLabel(handlerUncheckedIo)
		            ASTORE(exSlot)
		            NEW(retTupleType.jvmName)
		            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(0)
	            ANEWARRAY(JvmName.String)
	            pushString("")
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(handlerIo)
	            ASTORE(exSlot)
	            NEW(retTupleType.jvmName)
	            DUP()
	            pushBool(false)
	            ICONST_0()
	            pushInt(0)
	            ANEWARRAY(JvmName.String)
	            pushString("")
	            pushInt(14)
	            ALOAD(exSlot)
	            INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
	            INVOKESPECIAL(retTupleType.Constructor)
	            mv.visitJumpInsn(GOTO, after)

	            mv.visitLabel(after)

	          case IoOp.EnvGetArgs =>
	            import BytecodeInstructions.*
	            BytecodeInstructions.addLoc(loc)
	            compileExpr(exp)
            POP()
            val jGlobal = JvmName(JvmName.DevFlixRuntime, "Global")
            INVOKESTATIC(jGlobal, "getArgs", mkDescriptor()(BackendType.Array(BackendType.String)))

          case IoOp.EnvGetEnvPairs =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            POP()

            val jMap = JvmName(JvmName.JavaUtil, "Map")
            val jSet = JvmName(JvmName.JavaUtil, "Set")
            val jIterator = JvmName.Iterator
            val jMapEntry = JvmName(JvmName.JavaUtil, "Map$Entry")

            val envSlot = 2000
            val arrSlot = 2001
            val iterSlot = 2002
            val idxSlot = 2003
            val entrySlot = 2004

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

            mv.visitLabel(tryStart)
            // val env = System.getenv()
            INVOKESTATIC(JvmName.System, "getenv", mkDescriptor()(jMap.toTpe))
            ASTORE(envSlot)

            // val arr = new String[env.size() * 2]
            ALOAD(envSlot)
            INVOKEINTERFACE(jMap, "size", mkDescriptor()(BackendType.Int32))
            ICONST_2()
            mv.visitInsn(IMUL)
            ANEWARRAY(JvmName.String)
            ASTORE(arrSlot)

            // val iter = env.entrySet().iterator()
            ALOAD(envSlot)
            INVOKEINTERFACE(jMap, "entrySet", mkDescriptor()(jSet.toTpe))
            INVOKEINTERFACE(jSet, "iterator", mkDescriptor()(jIterator.toTpe))
            ASTORE(iterSlot)

            // var i = 0
            ICONST_0()
            mv.visitVarInsn(ISTORE, idxSlot)

            val loopStart = new Label()
            val loopEnd = new Label()
            mv.visitLabel(loopStart)

            // while (iter.hasNext())
            ALOAD(iterSlot)
            INVOKEINTERFACE(jIterator, "hasNext", mkDescriptor()(BackendType.Bool))
            mv.visitJumpInsn(IFEQ, loopEnd)

            // val entry = (Map.Entry) iter.next()
            ALOAD(iterSlot)
            INVOKEINTERFACE(jIterator, "next", mkDescriptor()(BackendType.Object))
            CHECKCAST(jMapEntry)
            ASTORE(entrySlot)

            // arr[i] = (String) entry.getKey()
            ALOAD(arrSlot)
            ILOAD(idxSlot)
            ALOAD(entrySlot)
            INVOKEINTERFACE(jMapEntry, "getKey", mkDescriptor()(BackendType.Object))
            CHECKCAST(JvmName.String)
            mv.visitInsn(AASTORE)

            // arr[i + 1] = (String) entry.getValue()
            ALOAD(arrSlot)
            ILOAD(idxSlot)
            ICONST_1()
            IADD()
            ALOAD(entrySlot)
            INVOKEINTERFACE(jMapEntry, "getValue", mkDescriptor()(BackendType.Object))
            CHECKCAST(JvmName.String)
            mv.visitInsn(AASTORE)

            // i += 2
            mv.visitIincInsn(idxSlot, 2)
            mv.visitJumpInsn(GOTO, loopStart)

            mv.visitLabel(loopEnd)
            ALOAD(arrSlot)
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            // Ignore the exception and return an empty array.
            POP()
            pushInt(0)
            ANEWARRAY(JvmName.String)

            mv.visitLabel(after)

          case IoOp.EnvGetVar =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)

            val argSlot = 2000
            compileExpr(exp)
            ASTORE(argSlot)

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

            mv.visitLabel(tryStart)
            ALOAD(argSlot)
            INVOKESTATIC(JvmName.System, "getenv", mkDescriptor(BackendType.String)(BackendType.String))
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            pushNull()
            mv.visitLabel(after)

          case IoOp.EnvGetProp =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)

            val argSlot = 2000
            compileExpr(exp)
            ASTORE(argSlot)

            val tryStart = new Label()
            val tryEnd = new Label()
            val handlerStart = new Label()
            val after = new Label()

            mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

            mv.visitLabel(tryStart)
            ALOAD(argSlot)
            INVOKESTATIC(JvmName.System, "getProperty", mkDescriptor(BackendType.String)(BackendType.String))
            mv.visitLabel(tryEnd)
            mv.visitJumpInsn(GOTO, after)

            mv.visitLabel(handlerStart)
            POP()
            pushNull()
            mv.visitLabel(after)

          case IoOp.EnvVirtualProcessors =>
            import BytecodeInstructions.*
            BytecodeInstructions.addLoc(loc)
            compileExpr(exp)
            POP()
            val jRuntime = JvmName(JvmName.JavaLang, "Runtime")
            INVOKESTATIC(jRuntime, "getRuntime", mkDescriptor()(jRuntime.toTpe))
            INVOKEVIRTUAL(jRuntime, "availableProcessors", mkDescriptor()(BackendType.Int32))
        }

      case AtomicOp.Binary(sop) =>
        val List(exp1, exp2) = exps
        sop match {
          case BoolOp.And =>
            val andEnd = new Label()
            compileExpr(exp1)
            mv.visitInsn(DUP)
            mv.visitJumpInsn(IFEQ, andEnd)
            mv.visitInsn(POP)
            compileExpr(exp2)
            mv.visitLabel(andEnd)

          case BoolOp.Or =>
            val orEnd = new Label()
            compileExpr(exp1)
            mv.visitInsn(DUP)
            mv.visitJumpInsn(IFNE, orEnd)
            mv.visitInsn(POP)
            compileExpr(exp2)
            mv.visitLabel(orEnd)

          case Float32Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(F2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(F2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)
            mv.visitInsn(D2F) // Convert double to float

          case Float64Op.Exp =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)

          case Int8Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)
            mv.visitInsn(D2I) // Convert to int
            mv.visitInsn(I2B) // Convert int to byte

          case Int16Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)
            mv.visitInsn(D2I) // Convert to int
            mv.visitInsn(I2S) // Convert int to short

          case Int32Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)
            mv.visitInsn(D2I) // Convert to int

          case Int64Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(L2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(L2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              mkDescriptor(BackendType.Float64, BackendType.Float64)(BackendType.Float64).toDescriptor, false)
            mv.visitInsn(D2L) // Convert to long

          case BigIntOp.Add =>
            compileBigIntBinaryOp(BigIntOp.Add, exp1, exp2)

          case BigIntOp.Sub =>
            compileBigIntBinaryOp(BigIntOp.Sub, exp1, exp2)

          case BigIntOp.Mul =>
            compileBigIntBinaryOp(BigIntOp.Mul, exp1, exp2)

          case BigIntOp.Div =>
            compileBigIntBinaryOp(BigIntOp.Div, exp1, exp2)

          case BigIntOp.Rem =>
            compileBigIntBinaryOp(BigIntOp.Rem, exp1, exp2)

          case BigIntOp.Shl =>
            compileBigIntBinaryOp(BigIntOp.Shl, exp1, exp2)

          case BigIntOp.Shr =>
            compileBigIntBinaryOp(BigIntOp.Shr, exp1, exp2)

          case BigIntOp.And =>
            compileBigIntBinaryOp(BigIntOp.And, exp1, exp2)

          case BigIntOp.Or =>
            compileBigIntBinaryOp(BigIntOp.Or, exp1, exp2)

          case BigIntOp.Xor =>
            compileBigIntBinaryOp(BigIntOp.Xor, exp1, exp2)

          case BigIntOp.Cmp =>
            compileBigIntBinaryOp(BigIntOp.Cmp, exp1, exp2)

          case BigDecimalOp.Add =>
            compileBigDecimalBinaryOp(BigDecimalOp.Add, exp1, exp2)

          case BigDecimalOp.Sub =>
            compileBigDecimalBinaryOp(BigDecimalOp.Sub, exp1, exp2)

          case BigDecimalOp.Mul =>
            compileBigDecimalBinaryOp(BigDecimalOp.Mul, exp1, exp2)

          case BigDecimalOp.Div =>
            compileBigDecimalBinaryOp(BigDecimalOp.Div, exp1, exp2)

          case BigDecimalOp.Cmp =>
            compileBigDecimalBinaryOp(BigDecimalOp.Cmp, exp1, exp2)

          case Int8Op.And | Int16Op.And | Int32Op.And =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IAND)

          case Int8Op.Or | Int16Op.Or | Int32Op.Or =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IOR)

          case Int8Op.Xor | Int16Op.Xor | Int32Op.Xor =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IXOR)

          case Int8Op.Shr | Int16Op.Shr | Int32Op.Shr =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISHR)

          case Int8Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISHL)
            mv.visitInsn(I2B) // Sign extend to make left most bit appear in the sign bit

          case Int16Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISHL)
            mv.visitInsn(I2S) // Sign extend to make left most bit appear in the sign bit

          case Int32Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISHL)

          case Int64Op.And =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LAND)

          case Int64Op.Or =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LOR)

          case Int64Op.Xor =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LXOR)

          case Int64Op.Shr =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LSHR)

          case Int64Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LSHL)

          case Float32Op.Lt => visitComparison2(exp1, exp2, FCMPG, IFGE)

          case Float32Op.Le => visitComparison2(exp1, exp2, FCMPG, IFGT)

          case Float32Op.Eq => visitComparison2(exp1, exp2, FCMPG, IFNE)

          case Float32Op.Neq => visitComparison2(exp1, exp2, FCMPG, IFEQ)

          case Float32Op.Ge => visitComparison2(exp1, exp2, FCMPL, IFLT)

          case Float32Op.Gt => visitComparison2(exp1, exp2, FCMPL, IFLE)

          case Float64Op.Lt => visitComparison2(exp1, exp2, DCMPG, IFGE)

          case Float64Op.Le => visitComparison2(exp1, exp2, DCMPG, IFGT)

          case Float64Op.Eq => visitComparison2(exp1, exp2, DCMPG, IFNE)

          case Float64Op.Neq => visitComparison2(exp1, exp2, DCMPG, IFEQ)

          case Float64Op.Ge => visitComparison2(exp1, exp2, DCMPL, IFLT)

          case Float64Op.Gt => visitComparison2(exp1, exp2, DCMPL, IFLE)

          case Int8Op.Lt | Int16Op.Lt | Int32Op.Lt | CharOp.Lt =>
            visitComparison1(exp1, exp2, IF_ICMPGE)

          case Int8Op.Le | Int16Op.Le | Int32Op.Le | CharOp.Le =>
            visitComparison1(exp1, exp2, IF_ICMPGT)

          case Int8Op.Eq | Int16Op.Eq | Int32Op.Eq | CharOp.Eq | BoolOp.Eq =>
            visitComparison1(exp1, exp2, IF_ICMPNE)

          case Int8Op.Neq | Int16Op.Neq | Int32Op.Neq | CharOp.Neq | BoolOp.Neq =>
            visitComparison1(exp1, exp2, IF_ICMPEQ)

          case Int8Op.Ge | Int16Op.Ge | Int32Op.Ge | CharOp.Ge =>
            visitComparison1(exp1, exp2, IF_ICMPLT)

          case Int8Op.Gt | Int16Op.Gt | Int32Op.Gt | CharOp.Gt =>
            visitComparison1(exp1, exp2, IF_ICMPLE)

          case Int64Op.Lt => visitComparison2(exp1, exp2, LCMP, IFGE)

          case Int64Op.Le => visitComparison2(exp1, exp2, LCMP, IFGT)

          case Int64Op.Eq => visitComparison2(exp1, exp2, LCMP, IFNE)

          case Int64Op.Neq => visitComparison2(exp1, exp2, LCMP, IFEQ)

          case Int64Op.Ge => visitComparison2(exp1, exp2, LCMP, IFLT)

          case Int64Op.Gt => visitComparison2(exp1, exp2, LCMP, IFLE)

          case Float32Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(FADD)

          case Float32Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(FSUB)

          case Float32Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(FMUL)

          case Float32Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(FDIV)

          case Float64Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(DADD)

          case Float64Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(DSUB)

          case Float64Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(DMUL)

          case Float64Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(DDIV)

          case Int8Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IADD)
            mv.visitInsn(I2B) // Sign extend after operation

          case Int8Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISUB)
            mv.visitInsn(I2B) // Sign extend after operation

          case Int8Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IMUL)
            mv.visitInsn(I2B) // Sign extend after operation

          case Int8Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IDIV)
            mv.visitInsn(I2B) // Sign extend after operation

          case Int8Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IREM)
            mv.visitInsn(I2B) // Sign extend after operation

          case Int16Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IADD)
            mv.visitInsn(I2S) // Sign extend after operation

          case Int16Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISUB)
            mv.visitInsn(I2S) // Sign extend after operation

          case Int16Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IMUL)
            mv.visitInsn(I2S) // Sign extend after operation

          case Int16Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IDIV)
            mv.visitInsn(I2S) // Sign extend after operation

          case Int16Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IREM)
            mv.visitInsn(I2S) // Sign extend after operation

          case Int32Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IADD)

          case Int32Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(ISUB)

          case Int32Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IMUL)

          case Int32Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IDIV)

          case Int32Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(IREM)

          case Int64Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LADD)

          case Int64Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LSUB)

          case Int64Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LMUL)

          case Int64Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LDIV)

          case Int64Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(LREM)

          case CharOp.Digit =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "digit",
              mkDescriptor(BackendType.Char, BackendType.Int32)(BackendType.Int32).toDescriptor, false)

          case CharOp.IsSurrogatePair =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isSurrogatePair",
              mkDescriptor(BackendType.Char, BackendType.Char)(BackendType.Bool).toDescriptor, false)

          case CharOp.ToCodePoint =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toCodePoint",
              mkDescriptor(BackendType.Char, BackendType.Char)(BackendType.Int32).toDescriptor, false)

          case CharOp.ForDigit =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "forDigit",
              mkDescriptor(BackendType.Int32, BackendType.Int32)(BackendType.Char).toDescriptor, false)

          case StringOp.CharAt =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "charAt",
              mkDescriptor(BackendType.Int32)(BackendType.Char).toDescriptor, false)

          case StringOp.Repeat =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "repeat",
              mkDescriptor(BackendType.Int32)(BackendType.String).toDescriptor, false)

          case StringOp.Concat =>
            throw InternalCompilerException(s"Unexpected BinaryOperator StringOp.Concat. It should have been eliminated by Simplifier", loc)
        }

      case AtomicOp.Is(sym) =>
        val List(exp) = exps
        val termTypes = root.enums(sym.enumSym).cases(sym).tpes.map(BackendType.toBackendType)
        compileIsTag(sym.name, exp, termTypes)

      case AtomicOp.Tag(sym) =>
        val termTypes = root.enums(sym.enumSym).cases(sym).tpes.map(BackendType.toBackendType)
        compileTag(sym.name, exps, termTypes)

      case AtomicOp.Untag(sym, idx) =>
        val List(exp) = exps
        val termTypes = root.enums(sym.enumSym).cases(sym).tpes.map(BackendType.toBackendType)

        compileUntag(exp, idx, termTypes)

      case AtomicOp.Index(idx) =>
        import BytecodeInstructions.*
        val List(exp) = exps
        val SimpleType.Tuple(elmTypes) = exp.tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

        compileExpr(exp)
        GETFIELD(tupleType.IndexField(idx))

      case AtomicOp.Tuple =>
        import BytecodeInstructions.*
        val SimpleType.Tuple(elmTypes) = tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))
        NEW(tupleType.jvmName)
        DUP()
        exps.foreach(compileExpr)
        INVOKESPECIAL(tupleType.Constructor)

      case AtomicOp.RecordSelect(field) =>
        import BytecodeInstructions.*
        val List(exp) = exps
        val recordType = BackendObjType.RecordExtend(BackendType.toErasedBackendType(tpe))

        compileExpr(exp)
        pushString(field.name)
        INVOKEINTERFACE(BackendObjType.Record.LookupFieldMethod)
        // Now that the specific RecordExtend object is found, we cast it to its exact class and extract the value.
        CHECKCAST(recordType.jvmName)
        GETFIELD(recordType.ValueField)

      case AtomicOp.RecordExtend(field) =>
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        val recordType = BackendObjType.RecordExtend(BackendType.toErasedBackendType(exp1.tpe))
        NEW(recordType.jvmName)
        DUP()
        INVOKESPECIAL(recordType.Constructor)
        DUP()
        pushString(field.name)
        PUTFIELD(recordType.LabelField)
        DUP()
        compileExpr(exp1)
        PUTFIELD(recordType.ValueField)
        DUP()
        compileExpr(exp2)
        PUTFIELD(recordType.RestField)

      case AtomicOp.RecordRestrict(field) =>
        import BytecodeInstructions.*
        val List(exp) = exps

        compileExpr(exp)
        pushString(field.name)
        INVOKEINTERFACE(BackendObjType.Record.RestrictFieldMethod)

      case AtomicOp.ExtIs(sym) =>
        val List(exp) = exps
        val tpes = SimpleType.findExtensibleTermTypes(sym, exp.tpe).map(BackendType.toBackendType)
        compileIsTag(sym.name, exp, tpes)

      case AtomicOp.ExtTag(sym) =>
        val tpes = SimpleType.findExtensibleTermTypes(sym, tpe).map(BackendType.toBackendType)
        compileTag(sym.name, exps, tpes)

      case AtomicOp.ExtUntag(sym, idx) =>
        import BytecodeInstructions.*

        val List(exp) = exps
        val tpes = SimpleType.findExtensibleTermTypes(sym, exp.tpe).map(BackendType.toBackendType)

        compileUntag(exp, idx, tpes)
        castIfNotPrim(BackendType.toBackendType(tpe))

      case AtomicOp.ArrayLit =>
        import BytecodeInstructions.*
        val innerType = tpe.asInstanceOf[SimpleType.Array].tpe
        val backendType = BackendType.toBackendType(innerType)

        pushInt(exps.length)
        xNewArray(backendType)
        for ((e, i) <- exps.zipWithIndex) {
          DUP()
          pushInt(i)
          compileExpr(e)
          xArrayStore(backendType)
        }

      case AtomicOp.ArrayNew =>
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        // We get the inner type of the array
        val innerType = tpe.asInstanceOf[SimpleType.Array].tpe
        val backendType = BackendType.toBackendType(innerType)
        val fillMethod = ClassMaker.StaticMethod(JvmName.Arrays, "fill", mkDescriptor(BackendType.Array(backendType.toErased), backendType.toErased)(VoidableType.Void))
        compileExpr(exp1) // default
        compileExpr(exp2) // default, length
        xNewArray(backendType) // default, arr
        if (backendType.is64BitWidth) DUP_X2() else DUP_X1() // arr, default, arr
        xSwap(lowerLarge = backendType.is64BitWidth, higherLarge = false) // arr, arr, default
        INVOKESTATIC(fillMethod)

      case AtomicOp.ArrayLoad =>
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps

        // Add source line number for debugging (can fail with out of bounds).
        addLoc(loc)
        compileExpr(exp1)
        compileExpr(exp2)
        xArrayLoad(BackendType.toBackendType(tpe))

      case AtomicOp.ArrayStore =>
        import BytecodeInstructions.*
        val List(exp1, exp2, exp3) = exps
        val elmTpe = BackendType.toBackendType(exp3.tpe)

        // Add source line number for debugging (can fail with out of bounds).
        addLoc(loc)
        compileExpr(exp1) // Evaluating the array
        castIfNotPrim(BackendType.Array(elmTpe))
        compileExpr(exp2) // Evaluating the index
        compileExpr(exp3) // Evaluating the element
        xArrayStore(elmTpe)
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.ArrayLength =>
        import BytecodeInstructions.*
        val List(exp) = exps
        compileExpr(exp)
        ARRAYLENGTH()

      case AtomicOp.StructNew(sym, mutability, _) =>
        import BytecodeInstructions.*
        val structType = getStructType(root.structs(sym))
        val (fieldExps, regionOpt) = mutability match {
          case Mutability.Immutable => (exps, None)
          case Mutability.Mutable =>
            val region :: fields = exps
            (fields, Some(region))
        }
        // If we have a region evaluate it and remove the result from the stack.
        regionOpt match {
          case None => ()
          case Some(region) =>
            compileExpr(region)
            xPop(BackendType.toBackendType(region.tpe))
        }
        NEW(structType.jvmName)
        DUP()
        fieldExps.foreach(compileExpr)
        INVOKESPECIAL(structType.Constructor)

      case AtomicOp.StructGet(field) =>
        import BytecodeInstructions.*

        val List(exp) = exps
        val struct = root.structs(field.structSym)
        val structType = getStructType(struct)
        val idx = struct.fields.indexWhere(_.sym == field)

        compileExpr(exp)
        GETFIELD(structType.IndexField(idx))

      case AtomicOp.StructPut(field) =>
        import BytecodeInstructions.*

        val List(exp1, exp2) = exps
        val struct = root.structs(field.structSym)
        val idx = struct.fields.indexWhere(_.sym == field)
        val structType = getStructType(struct)

        compileExpr(exp1)
        compileExpr(exp2)
        PUTFIELD(structType.IndexField(idx))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.InstanceOf(clazz) =>
        import BytecodeInstructions.*
        val List(exp) = exps
        val jvmName = JvmName.ofClass(clazz)
        compileExpr(exp)
        INSTANCEOF(jvmName)

      case AtomicOp.Cast =>
        import BytecodeInstructions.*
        val List(exp) = exps
        compileExpr(exp)
        castIfNotPrim(BackendType.toBackendType(tpe))

      case AtomicOp.Unbox =>
        import BytecodeInstructions.*
        val List(exp) = exps
        compileExpr(exp)
        CHECKCAST(BackendObjType.Value.jvmName)
        GETFIELD(BackendObjType.Value.fieldFromType(BackendType.toBackendType(tpe)))

      case AtomicOp.Box =>
        import BytecodeInstructions.*
        val List(exp) = exps
        val erasedExpTpe = BackendType.toErasedBackendType(exp.tpe)
        val valueField = BackendObjType.Value.fieldFromType(erasedExpTpe)
        compileExpr(exp)
        NEW(BackendObjType.Value.jvmName)
        DUP()
        INVOKESPECIAL(BackendObjType.Value.Constructor)
        DUP()
        xSwap(lowerLarge = erasedExpTpe.is64BitWidth, higherLarge = true) // two objects on top of the stack
        PUTFIELD(valueField)

      case AtomicOp.InvokeConstructor(constructor) =>
        // Add source line number for debugging (can fail when calling unsafe java methods)
        BytecodeInstructions.addLoc(loc)
        val descriptor = asm.Type.getConstructorDescriptor(constructor)
        val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
        // Create a new object of the declaration type
        mv.visitTypeInsn(NEW, declaration)
        // Duplicate the reference since the first argument for a constructor call is the reference to the object
        mv.visitInsn(DUP)
        for ((arg, argType) <- exps.zip(constructor.getParameterTypes)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
        }

        // Call the constructor
        mv.visitMethodInsn(INVOKESPECIAL, declaration, JvmName.ConstructorMethod, descriptor, false)

      case AtomicOp.InvokeMethod(method) =>
        val exp :: args = exps

        // Add source line number for debugging (can fail when calling unsafe java methods)
        BytecodeInstructions.addLoc(loc)

        // Evaluate the receiver object.
        compileExpr(exp)
        val thisType = asm.Type.getInternalName(method.getDeclaringClass)
        mv.visitTypeInsn(CHECKCAST, thisType)

        for ((arg, argType) <- args.zip(method.getParameterTypes)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
        }

        val declaration = asm.Type.getInternalName(method.getDeclaringClass)
        val name = method.getName
        val descriptor = asm.Type.getMethodDescriptor(method)

        // Check if we are invoking an interface or class.
        if (method.getDeclaringClass.isInterface) {
          mv.visitMethodInsn(INVOKEINTERFACE, declaration, name, descriptor, true)
        } else {
          mv.visitMethodInsn(INVOKEVIRTUAL, declaration, name, descriptor, false)
        }

        // If the method is void, put a unit on top of the stack
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.InvokeStaticMethod(method) =>
        // Add source line number for debugging (can fail when calling unsafe java methods)
        BytecodeInstructions.addLoc(loc)
        for ((arg, argType) <- exps.zip(method.getParameterTypes)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
        }
        val declaration = asm.Type.getInternalName(method.getDeclaringClass)
        val name = method.getName
        val descriptor = asm.Type.getMethodDescriptor(method)
        // Check if we are invoking an interface or class.
        if (method.getDeclaringClass.isInterface) {
          mv.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, true)
        } else {
          mv.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, false)
        }
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.GetField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        BytecodeInstructions.addLoc(loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETFIELD, declaration, field.getName, BackendType.toBackendType(tpe).toDescriptor)

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail when calling java)
        BytecodeInstructions.addLoc(loc)
        compileExpr(exp1)
        compileExpr(exp2)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTFIELD, declaration, field.getName, BackendType.toBackendType(exp2.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.GetStaticField(field) =>
        // Add source line number for debugging (can fail when calling java)
        BytecodeInstructions.addLoc(loc)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETSTATIC, declaration, field.getName, BackendType.toBackendType(tpe).toDescriptor)

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        BytecodeInstructions.addLoc(loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTSTATIC, declaration, field.getName, BackendType.toBackendType(exp.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.Throw =>
        import BytecodeInstructions.*
        val List(exp) = exps
        // Add source line number for debugging (can fail when handling exception).
        addLoc(loc)
        val isPortable = flix.options.stdlibProfile == StdlibProfile.Portable
        if (isPortable) {
          // Portable semantics: throw the designated Exn value via a single JVM wrapper type.
          NEW(JvmName.FlixException)
          DUP()
          compileExpr(exp)
          INVOKESPECIAL(ClassConstants.FlixException.Constructor)
          ATHROW()
        } else {
          // JVM semantics: throw the actual Throwable instance.
          compileExpr(exp)
          ATHROW()
        }

      case AtomicOp.Spawn =>
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.Cst(Constant.Static, _) =>
            addLoc(loc)
            compileExpr(exp1)
            CHECKCAST(JvmName.Runnable)
            INVOKESTATIC(ClassConstants.Thread.StartVirtualThreadMethod)
            POP()
            GETSTATIC(BackendObjType.Unit.SingletonField)
          case _ =>
            addLoc(loc)
            compileExpr(exp2)
            CHECKCAST(BackendObjType.Region.jvmName)
            compileExpr(exp1)
            CHECKCAST(JvmName.Runnable)
            INVOKEVIRTUAL(BackendObjType.Region.SpawnMethod)
            GETSTATIC(BackendObjType.Unit.SingletonField)
        }

      case AtomicOp.ChannelNew =>
        import BytecodeInstructions.*
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.ChannelSupport, "newChannel", mkDescriptor(BackendType.Int32)(BackendType.Object))

      case AtomicOp.ChannelGet =>
        import BytecodeInstructions.*
        val List(exp) = exps
        val returnTpe = BackendType.toBackendType(tpe)
        val valueField = BackendObjType.Value.fieldFromType(returnTpe)

        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.ChannelSupport, "take", mkDescriptor(BackendType.Object)(BackendType.Object))
        CHECKCAST(BackendObjType.Value.jvmName)
        GETFIELD(valueField)
        castIfNotPrim(returnTpe)

      case AtomicOp.ChannelPut =>
        import BytecodeInstructions.*
        val List(chan, value) = exps
        val erasedValueTpe = BackendType.toErasedBackendType(value.tpe)
        val valueField = BackendObjType.Value.fieldFromType(erasedValueTpe)

        addLoc(loc)
        compileExpr(chan)

        // Box the element value into dev.flix.runtime.Value.
        compileExpr(value)
        NEW(BackendObjType.Value.jvmName)
        DUP()
        INVOKESPECIAL(BackendObjType.Value.Constructor)
        DUP()
        xSwap(lowerLarge = erasedValueTpe.is64BitWidth, higherLarge = true) // two objects on top of the stack
        PUTFIELD(valueField)

        // Put the boxed element into the channel runtime.
        INVOKESTATIC(JvmName.ChannelSupport, "put", mkDescriptor(BackendType.Object, BackendType.Object)(VoidableType.Void))

        // Push Unit on the stack.
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.ChannelSelect =>
        import BytecodeInstructions.*

        val channels = exps.dropRight(1)
        val blocking = exps.last
        val channelArraySlot = 2300

        addLoc(loc)
        pushInt(channels.length)
        ANEWARRAY(JvmName.Object)
        ASTORE(channelArraySlot)

        channels.zipWithIndex.foreach {
          case (channelExp, index) =>
            ALOAD(channelArraySlot)
            pushInt(index)
            compileExpr(channelExp)
            mv.visitInsn(AASTORE)
        }

        ALOAD(channelArraySlot)
        compileExpr(blocking)
        INVOKESTATIC(JvmName.ChannelSupport, "select", mkDescriptor(BackendType.Array(BackendType.Object), BackendType.Bool)(BackendType.Int64))

      case AtomicOp.ChannelSelectIndex =>
        import BytecodeInstructions.*
        val List(tokenExp) = exps

        addLoc(loc)
        compileExpr(tokenExp)
        INVOKESTATIC(JvmName.ChannelSupport, "selectIndex", mkDescriptor(BackendType.Int64)(BackendType.Int32))

      case AtomicOp.ChannelSelectGet =>
        import BytecodeInstructions.*
        val List(tokenExp) = exps
        val returnTpe = BackendType.toBackendType(tpe)
        val valueField = BackendObjType.Value.fieldFromType(returnTpe)

        addLoc(loc)
        compileExpr(tokenExp)
        INVOKESTATIC(JvmName.ChannelSupport, "getSelected", mkDescriptor(BackendType.Int64)(BackendType.Object))
        CHECKCAST(BackendObjType.Value.jvmName)
        GETFIELD(valueField)
        castIfNotPrim(returnTpe)

      case op @ (AtomicOp.ReentrantLockNew |
          AtomicOp.ReentrantLockLock |
          AtomicOp.ReentrantLockTryLock |
          AtomicOp.ReentrantLockUnlock |
          AtomicOp.ConditionNew |
          AtomicOp.ConditionAwait |
          AtomicOp.ConditionSignal |
          AtomicOp.ConditionSignalAll |
          AtomicOp.CyclicBarrierNew |
          AtomicOp.CyclicBarrierAwait |
          AtomicOp.CountDownLatchNew |
          AtomicOp.CountDownLatchAwait |
          AtomicOp.CountDownLatchCountDown |
          AtomicOp.SemaphoreNew |
          AtomicOp.SemaphoreAcquire |
          AtomicOp.SemaphoreTryAcquire |
          AtomicOp.SemaphoreRelease) =>
        compileSyncAtomic(op, exps, tpe, loc)

      case AtomicOp.Lazy =>
        import BytecodeInstructions.*
        val List(exp) = exps

        // Find the Lazy class name (Lazy$tpe).
        val SimpleType.Lazy(elmType) = tpe
        val lazyType = BackendObjType.Lazy(BackendType.toBackendType(elmType))

        NEW(lazyType.jvmName)
        DUP()
        compileExpr(exp)
        INVOKESPECIAL(lazyType.Constructor)

      case AtomicOp.Force =>
        import BytecodeInstructions.*
        val List(exp) = exps

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val SimpleType.Lazy(elmType) = exp.tpe
        val erasedElmType = BackendType.toBackendType(elmType)
        val lazyType = BackendObjType.Lazy(erasedElmType)

        // Emit code for the lazy expression.
        compileExpr(exp)
        CHECKCAST(lazyType.jvmName)
        DUP()
        GETFIELD(lazyType.ExpField)
        ifConditionElse(Condition.NONNULL)(
          INVOKEVIRTUAL(lazyType.ForceMethod)
        )(
          GETFIELD(lazyType.ValueField)
        )

      case AtomicOp.HoleError(sym) =>
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design).
        addLoc(loc)
        NEW(BackendObjType.HoleError.jvmName) // HoleError
        DUP() // HoleError, HoleError
        pushString(sym.toString) // HoleError, HoleError, Sym
        pushLoc(loc) // HoleError, HoleError, Sym, Loc
        INVOKESPECIAL(BackendObjType.HoleError.Constructor) // HoleError
        ATHROW()

      case AtomicOp.MatchError =>
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design)
        addLoc(loc)
        NEW(BackendObjType.MatchError.jvmName) // MatchError
        DUP() // MatchError, MatchError
        pushLoc(loc) // MatchError, MatchError, Loc
        INVOKESPECIAL(BackendObjType.MatchError.Constructor) // MatchError
        ATHROW()

      case AtomicOp.CastError(from, to) =>
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design)
        addLoc(loc)
        NEW(BackendObjType.CastError.jvmName) // CastError
        DUP() // CastError, CastError
        pushLoc(loc) // CastError, CastError, Loc
        pushString(s"Cannot cast from type '$from' to '$to'") // CastError, CastError, Loc, String
        INVOKESPECIAL(BackendObjType.CastError.Constructor) // CastError
        ATHROW()
    }

    case Expr.ApplyClo(exp1, exp2, ct, pcPointId, _, purity, loc) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getErasedFunctionInterfaceType(exp1.tpe)
      val closureAbstractClass = JvmOps.getErasedClosureAbstractClassType(exp1.tpe)
      ct match {
        case ExpPosition.Tail =>
          // Evaluating the closure
          compileExpr(exp1)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(CHECKCAST, closureAbstractClass.jvmName.toInternalName)
          // retrieving the unique thread object
          mv.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.jvmName.toInternalName, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodDescriptor.mkDescriptor()(closureAbstractClass.toTpe).toDescriptor, false)
          // Putting arg on the Fn class
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(exp2)
          BytecodeInstructions.PUTFIELD(functionInterface.ArgField(0))
          // Return the closure
          mv.visitInsn(ARETURN)

        case ExpPosition.NonTail =>
          compileExpr(exp1)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(CHECKCAST, closureAbstractClass.jvmName.toInternalName)
          // retrieving the unique thread object
          mv.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.jvmName.toInternalName, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodDescriptor.mkDescriptor()(closureAbstractClass.toTpe).toDescriptor, false)
          // Putting arg on the Fn class
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(exp2)
          BytecodeInstructions.PUTFIELD(functionInterface.ArgField(0))

          // Calling unwind and unboxing
          if (Purity.isControlPure(purity)) {
            BackendObjType.Result.unwindSuspensionFreeThunk("in pure closure call", loc)
          } else {
            ctx match {
              case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
                if (pcPointId <= 0 || pcPointId >= pcLabels.length) {
                  throw InternalCompilerException(s"Unexpected pcPointId in ApplyClo: $pcPointId.", loc)
                }
                val pcPointLabel = pcLabels(pcPointId)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                BackendObjType.Result.unwindThunkToValue(pcPointId, newFrame, setPc)
                mv.visitJumpInsn(GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)

                mv.visitVarInsn(ALOAD, 1)

                mv.visitLabel(afterUnboxing)

              case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
                throw InternalCompilerException("Unexpected direct method context in control impure function", loc)
            }
          }
      }

    case Expr.ApplyDef(sym, exps, ct, pcPointId, _, _, loc) => ct match {
      case ExpPosition.Tail =>
        val defJvmName = BackendObjType.Defn(sym).jvmName
        // Type of the function abstract class
        val functionInterface = JvmOps.getErasedFunctionInterfaceType(root.defs(sym).arrowType)

        // Put the def on the stack
        mv.visitTypeInsn(NEW, defJvmName.toInternalName)
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL, defJvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(arg)
          BytecodeInstructions.PUTFIELD(functionInterface.ArgField(i))
        }
        // Return the def
        mv.visitInsn(ARETURN)

      case ExpPosition.NonTail =>
        val defn = root.defs(sym)
        val targetIsFunction = defn.cparams.isEmpty
        val canCallStaticMethod = Purity.isControlPure(defn.expr.purity) && targetIsFunction
        if (canCallStaticMethod) {
          val paramTpes = defn.fparams.map(fp => BackendType.toBackendType(fp.tpe))
          // Call the static method, using exact types
          for ((arg, tpe) <- ListOps.zip(exps, paramTpes)) {
            compileExpr(arg)
            BytecodeInstructions.castIfNotPrim(tpe)
          }
          val resultTpe = BackendObjType.Result.toTpe
          val desc = MethodDescriptor(paramTpes, resultTpe)
          val className = BackendObjType.Defn(sym).jvmName
          mv.visitMethodInsn(INVOKESTATIC, className.toInternalName, JvmName.StaticApply, desc.toDescriptor, false)
          BackendObjType.Result.unwindSuspensionFreeThunk("in pure function call", loc)
        } else {
          // JvmType of Def
          val defJvmName = BackendObjType.Defn(sym).jvmName

          // Put the def on the stack
          mv.visitTypeInsn(NEW, defJvmName.toInternalName)
          mv.visitInsn(DUP)
          mv.visitMethodInsn(INVOKESPECIAL, defJvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)

          // Putting args on the Fn class
          for ((arg, i) <- exps.zipWithIndex) {
            // Duplicate the FunctionInterface
            mv.visitInsn(DUP)
            // Evaluating the expression
            compileExpr(arg)
            mv.visitFieldInsn(PUTFIELD, defJvmName.toInternalName,
              s"arg$i", BackendType.toErasedBackendType(arg.tpe).toDescriptor)
          }
          // Calling unwind and unboxing
          ctx match {
            case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
              val defn = root.defs(sym)
              if (Purity.isControlPure(defn.expr.purity)) {
                BackendObjType.Result.unwindSuspensionFreeThunk("in pure function call", loc)
              } else {
                if (pcPointId <= 0 || pcPointId >= pcLabels.length) {
                  throw InternalCompilerException(s"Unexpected pcPointId in ApplyDef: $pcPointId.", loc)
                }
                val pcPointLabel = pcLabels(pcPointId)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                BackendObjType.Result.unwindThunkToValue(pcPointId, newFrame, setPc)
                mv.visitJumpInsn(GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)
                mv.visitVarInsn(ALOAD, 1)

                mv.visitLabel(afterUnboxing)
              }
            case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
              BackendObjType.Result.unwindSuspensionFreeThunk("in pure function call", loc)
          }
        }
    }

    case Expr.ApplyOp(sym, exps, pcPointId, tpe, _, loc) => ctx match {
      case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
        BackendObjType.Result.crashIfSuspension("Unexpected do-expression in direct method context", loc)

      case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
        import BackendObjType.Suspension
        import BytecodeInstructions.*

        if (pcPointId <= 0 || pcPointId >= pcLabels.length) {
          throw InternalCompilerException(s"Unexpected pcPointId in ApplyOp: $pcPointId.", loc)
        }
        val pcPointLabel = pcLabels(pcPointId)
        val afterUnboxing = new Label()
        val erasedResult = BackendType.toErasedBackendType(tpe)
        pcCounter(0) += 1

        val effectName = JvmOps.getEffectDefinitionClassName(sym.eff)
        val effectStaticMethod = ClassMaker.StaticMethod(
          effectName,
          JvmOps.getEffectOpName(sym),
          GenEffectClasses.opStaticFunctionDescriptor(sym)
        )
        NEW(Suspension.jvmName)
        DUP()
        INVOKESPECIAL(Suspension.Constructor)
        DUP()
        pushString(sym.eff.toString)
        PUTFIELD(Suspension.EffSymField)
        DUP()
        // --- eff op ---
        exps.foreach(compileExpr)
        mkStaticLambda(BackendObjType.EffectCall.ApplyMethod, effectStaticMethod, 2)
        // --------------
        PUTFIELD(Suspension.EffOpField)
        DUP()
        // create continuation
        NEW(BackendObjType.FramesNil.jvmName)
        DUP()
        INVOKESPECIAL(BackendObjType.FramesNil.Constructor)
        newFrame(mv)
        DUP()
        pushInt(pcPointId)
        setPc(mv)
        INVOKEVIRTUAL(BackendObjType.FramesNil.PushMethod)
        // store continuation
        PUTFIELD(Suspension.PrefixField)
        DUP()
        NEW(BackendObjType.ResumptionNil.jvmName)
        DUP()
        INVOKESPECIAL(BackendObjType.ResumptionNil.Constructor)
        PUTFIELD(Suspension.ResumptionField)
        xReturn(Suspension.toTpe)

        mv.visitLabel(pcPointLabel)
        ALOAD(1)
        GETFIELD(BackendObjType.Value.fieldFromType(erasedResult))

        mv.visitLabel(afterUnboxing)
        castIfNotPrim(BackendType.toBackendType(tpe))
    }

    case Expr.ApplySelfTail(sym, exps, _, _, _) => ctx match {
      case EffectContext(_, _, _, setPc, _, _, _) =>
        // The function abstract class name
        val functionInterface = JvmOps.getErasedFunctionInterfaceType(root.defs(sym).arrowType)
        // Evaluate each argument and put the result on the Fn class.
        for ((arg, i) <- exps.zipWithIndex) {
          mv.visitVarInsn(ALOAD, 0)
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
          BytecodeInstructions.PUTFIELD(functionInterface.ArgField(i))
        }
        mv.visitVarInsn(ALOAD, 0)
        BytecodeInstructions.pushInt(0)
        setPc(mv)
        // Jump to the entry point of the method.
        mv.visitJumpInsn(GOTO, ctx.entryPoint)

      case DirectInstanceContext(_, _, _) =>
        // The function abstract class name
        val functionInterface = JvmOps.getErasedFunctionInterfaceType(root.defs(sym).arrowType)
        // Evaluate each argument and put the result on the Fn class.
        for ((arg, i) <- exps.zipWithIndex) {
          mv.visitVarInsn(ALOAD, 0)
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
          BytecodeInstructions.PUTFIELD(functionInterface.ArgField(i))
        }
        // Jump to the entry point of the method.
        mv.visitJumpInsn(GOTO, ctx.entryPoint)

      case DirectStaticContext(_, _, _) =>
        val defn = root.defs(sym)
        for (arg <- exps) {
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
        }
        for ((arg, fp) <- ListOps.zip(exps, defn.fparams).reverse) {
          // Store it in the ith parameter.
          val tpe = BackendType.toBackendType(arg.tpe)
          val offset = JvmOps.getIndex(fp.offset, ctx.localOffset)
          BytecodeInstructions.xStore(tpe, offset)
        }
        // Jump to the entry point of the method.
        mv.visitJumpInsn(GOTO, ctx.entryPoint)
    }

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      import BytecodeInstructions.*
      compileExpr(exp1)
      branch(Condition.Bool) {
        case Branch.TrueBranch => compileExpr(exp2)
        case Branch.FalseBranch => compileExpr(exp3)
      }

    case Expr.Branch(exp, branches, _, _, _) =>
      // Calculating the updated jumpLabels map
      val updatedJumpLabels = branches.map(branch => branch._1 -> new Label())
      val ctx1 = ctx.addLabels(updatedJumpLabels)
      // Compiling the exp
      compileExpr(exp)(mv, ctx1, root, flix)
      // Label for the end of all branches
      val endLabel = new Label()
      // Skip branches if `exp` does not jump
      mv.visitJumpInsn(GOTO, endLabel)
      // Compiling branches
      branches.foreach { case (sym, branchExp) =>
        // Label for the start of the branch
        mv.visitLabel(updatedJumpLabels(sym))
        // evaluating the expression for the branch
        compileExpr(branchExp)(mv, ctx1, root, flix)
        // Skip the rest of the branches
        mv.visitJumpInsn(GOTO, endLabel)
      }
      // label for the end of branches
      mv.visitLabel(endLabel)

    case Expr.JumpTo(sym, _, _, _) =>
      // Jumping to the label
      mv.visitJumpInsn(GOTO, ctx.lenv(sym))

    case Expr.Let(_, offset, exp1, exp2, _) =>
      import BytecodeInstructions.*
      val bType = BackendType.toBackendType(exp1.tpe)
      compileExpr(exp1)
      castIfNotPrim(bType)
      xStore(bType, JvmOps.getIndex(offset, ctx.localOffset))
      compileExpr(exp2)

    case Expr.Stmt(exp1, exp2, _) =>
      import BytecodeInstructions.*
      compileExpr(exp1)
      xPop(BackendType.toBackendType(exp1.tpe))
      compileExpr(exp2)

    case Expr.Region(_, offset, exp, _, _, loc) =>
      // Adding source line number for debugging
      BytecodeInstructions.addLoc(loc)

      // Introduce a label for before the try block.
      val beforeTryBlock = new Label()

      // Introduce a label for after the try block.
      val afterTryBlock = new Label()

      // Introduce a label for the finally block.
      val finallyBlock = new Label()

      // Introduce a label after the finally block.
      val afterFinally = new Label()

      // Create an instance of Region
      mv.visitTypeInsn(NEW, BackendObjType.Region.jvmName.toInternalName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.Region.jvmName.toInternalName, JvmName.ConstructorMethod,
        MethodDescriptor.NothingToVoid.toDescriptor, false)

      BytecodeInstructions.xStore(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))

      // Compile the scope body
      mv.visitLabel(beforeTryBlock)
      compileExpr(exp)

      // Emit try finally block. It's important to do this after compiling sub-expressions to ensure
      // correct catch case ordering.
      mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

      // When we exit the scope, call the region's `exit` method
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ExitMethod.name,
        BackendObjType.Region.ExitMethod.d.toDescriptor, false)
      mv.visitLabel(afterTryBlock)

      // Compile the finally block which gets called if no exception is thrown
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitJumpInsn(GOTO, afterFinally)

      // Compile the finally block which gets called if an exception is thrown
      mv.visitLabel(finallyBlock)
      // On exceptional completion, cancel outstanding child tasks before joining them.
      // This keeps JVM region behavior aligned with LLVM task cancellation semantics.
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.CancelChildrenMethod.name,
        BackendObjType.Region.CancelChildrenMethod.d.toDescriptor, false)
      // Always exit the region, even on exceptional completion, to join children and run `onExit`.
      // This ensures child exceptions are observed deterministically at region exit.
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ExitMethod.name,
        BackendObjType.Region.ExitMethod.d.toDescriptor, false)
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitInsn(ATHROW)
      mv.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      // Add source line number for debugging.
      BytecodeInstructions.addLoc(loc)

      val isPortable = flix.options.stdlibProfile == StdlibProfile.Portable

      if (!isPortable) {
        // JVM semantics: compile directly to JVM try/catch blocks.

        // Introduce a label for before the try block.
        val beforeTryBlock = new Label()

        // Introduce a label for after the try block.
        val afterTryBlock = new Label()

        // Introduce a label after the try block and after all catch rules.
        val afterTryAndCatch = new Label()

        // Introduce a label for each catch rule.
        val rulesAndLabels = rules map {
          rule => rule -> new Label()
        }

        // Emit code for the try block.
        mv.visitLabel(beforeTryBlock)
        compileExpr(exp)
        mv.visitLabel(afterTryBlock)
        mv.visitJumpInsn(GOTO, afterTryAndCatch)

        // Emit code for each catch rule.
        for ((CatchRule(_, offset, _, body), handlerLabel) <- rulesAndLabels) {
          // Emit the label.
          mv.visitLabel(handlerLabel)

          // Store the exception in a local variable.
          BytecodeInstructions.xStore(BackendType.Object, JvmOps.getIndex(offset, ctx.localOffset))

          // Emit code for the handler body expression.
          compileExpr(body)
          mv.visitJumpInsn(GOTO, afterTryAndCatch)
        }

        // Emit a try catch block for each catch rule. It's important to do this after compiling
        // sub-expressions to ensure correct catch case ordering.
        for ((CatchRule(_, _, catchTpe, _), handlerLabel) <- rulesAndLabels) {
          val clazz = catchTpe match {
            case SimpleType.Native(c) => c
            case _ => classOf[Throwable]
          }
          mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
        }

        // Add the label after both the try and catch rules.
        mv.visitLabel(afterTryAndCatch)
      } else {
        // Portable semantics: catch only FlixException and dispatch by Exn.kind_id (exact, ordered).
        import BytecodeInstructions.*

        // Introduce a label for before the try block.
        val beforeTryBlock = new Label()

        // Introduce a label for after the try block.
        val afterTryBlock = new Label()

        // Introduce a label after the try block and after all catch rules.
        val afterTryAndCatch = new Label()

        // Introduce a single handler for FlixException.
        val handlerStart = new Label()

        // Introduce a label for each catch rule body.
        val rulesAndLabels = rules map {
          rule => rule -> new Label()
        }

        // Emit code for the try block.
        mv.visitLabel(beforeTryBlock)
        compileExpr(exp)
        mv.visitLabel(afterTryBlock)
        mv.visitJumpInsn(GOTO, afterTryAndCatch)

        // Emit code for the FlixException handler.
        mv.visitLabel(handlerStart)

        // Stack: [wrapper]
        // Duplicate wrapper and extract exn payload: wrapper.getExn()
        DUP()
        INVOKEVIRTUAL(ClassConstants.FlixException.GetExnMethod)

        // Stack: [wrapper, exn]
        // Duplicate exn and extract kind id (first field of Exn tag).
        DUP()
        val exnTagType = BackendObjType.Tag(List(BackendType.Int32, BackendType.Object, BackendType.Object))
        CHECKCAST(exnTagType.jvmName)
        GETFIELD(exnTagType.IndexField(0))

        // Stack: [wrapper, exn, kindId]

        // Compile ordered dispatch.
        val exnSymOpt = root.enums.keys.find(sym => sym.text == "Exn" && sym.namespace.isEmpty)
        val exnTpeOpt = exnSymOpt.map(sym => SimpleType.Enum(sym, Nil))

        def isCatchAll(tpe: SimpleType): Boolean =
          exnTpeOpt.contains(tpe)

        for ((CatchRule(_, _, catchTpe, _), ruleLabel) <- rulesAndLabels) {
          if (isCatchAll(catchTpe)) {
            // Catch-all: match immediately (first-match-wins).
            mv.visitJumpInsn(GOTO, ruleLabel)
          } else {
            // Typed catch: compare kind id.
            DUP()
            pushInt(ExnKindId.of(catchTpe))
            mv.visitJumpInsn(IF_ICMPEQ, ruleLabel)
          }
        }

        // No rule matched: propagate by rethrowing the wrapper.
        // Stack: [wrapper, exn, kindId]
        POP() // kindId
        POP() // exn
        ATHROW()

        // Emit code for each catch rule body.
        for ((CatchRule(_, offset, _, body), ruleLabel) <- rulesAndLabels) {
          mv.visitLabel(ruleLabel)

          // Stack: [wrapper, exn, kindId]
          // Store exn in local binder slot and clear the dispatch stack.
          SWAP() // [wrapper, kindId, exn]
          xStore(BackendType.Object, JvmOps.getIndex(offset, ctx.localOffset))
          POP() // kindId
          POP() // wrapper

          // Emit handler body.
          compileExpr(body)
          mv.visitJumpInsn(GOTO, afterTryAndCatch)
        }

        // Catch only FlixException.
        mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerStart, JvmName.FlixException.toInternalName)

        // Add the label after both the try and catch rules.
        mv.visitLabel(afterTryAndCatch)
      }

    case Expr.RunWith(exp, effUse, rules, ct, pcPointId, _, _, loc) =>
      import BytecodeInstructions.*
      // exp is a Unit -> exp.tpe closure
      val effectJvmName = JvmOps.getEffectDefinitionClassName(effUse.sym)
      // eff name
      pushString(effUse.sym.toString)
      // handler
      NEW(effectJvmName)
      DUP()
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, effectJvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
      // bind handler closures
      for (HandlerRule(op, _, body) <- rules) {
        mv.visitInsn(Opcodes.DUP)
        compileExpr(body)
        mv.visitFieldInsn(Opcodes.PUTFIELD, effectJvmName.toInternalName, JvmOps.getEffectOpName(op.sym), GenEffectClasses.opFieldType(op.sym).toDescriptor)
      }
      // frames
      NEW(BackendObjType.FramesNil.jvmName)
      DUP()
      INVOKESPECIAL(BackendObjType.FramesNil.Constructor)
      // continuation
      compileExpr(exp)
      // exp.arg0 should be set to unit here but from lifting we know that it is unused so the
      // implicit null is fine.
      // call installHandler
      INVOKESTATIC(BackendObjType.Handler.InstallHandlerMethod)
      // handle value/suspend/thunk if in non-tail position
      if (ct == ExpPosition.NonTail) {
        ctx match {
          case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
            BackendObjType.Result.unwindSuspensionFreeThunk("in pure run-with call", loc)

          case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
            if (pcPointId <= 0 || pcPointId >= pcLabels.length) {
              throw InternalCompilerException(s"Unexpected pcPointId in RunWith: $pcPointId.", loc)
            }
            val pcPointLabel = pcLabels(pcPointId)
            val afterUnboxing = new Label()
            pcCounter(0) += 1
            BackendObjType.Result.unwindThunkToValue(pcPointId, newFrame, setPc)
            mv.visitJumpInsn(GOTO, afterUnboxing)

            mv.visitLabel(pcPointLabel)
            BytecodeInstructions.ALOAD(1)
            mv.visitLabel(afterUnboxing)
        }
      } else {
        ARETURN()
      }

    case Expr.NewObject(name, _, _, _, methods, _) =>
      val exps = methods.map(_.exp)
      val className = JvmName(ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage, name).toInternalName
      mv.visitTypeInsn(NEW, className)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, className, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)

      // For each method, compile the closure which implements the body of that method and store it in a field
      exps.zipWithIndex.foreach { case (e, i) =>
        mv.visitInsn(DUP)
        compileExpr(e)
        mv.visitFieldInsn(PUTFIELD, className, s"clo$i", JvmOps.getErasedClosureAbstractClassType(e.tpe).toDescriptor)
      }

  }

  private def compileIoFilePredicate(op: UnaryOp, exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jLinkOption = JvmName.ofClass(classOf[java.nio.file.LinkOption])
    val linkOptionTpe = BackendType.Reference(BackendObjType.Native(jLinkOption))
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])

    // Locals.
    val filenameSlot = 2600
    val boolSlot = 2601
    val exSlot = 2602

    compileExpr(exp)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)

    mv.visitLabel(tryStart)
    // path = Paths.get(filename, new String[0])
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))

    op match {
      case IoOp.FileExists =>
        ICONST_0()
        ANEWARRAY(jLinkOption)
        INVOKESTATIC(jFiles, "exists", mkDescriptor(pathTpe, BackendType.Array(linkOptionTpe))(BackendType.Bool))
      case IoOp.FileIsDirectory =>
        ICONST_0()
        ANEWARRAY(jLinkOption)
        INVOKESTATIC(jFiles, "isDirectory", mkDescriptor(pathTpe, BackendType.Array(linkOptionTpe))(BackendType.Bool))
      case IoOp.FileIsRegularFile =>
        ICONST_0()
        ANEWARRAY(jLinkOption)
        INVOKESTATIC(jFiles, "isRegularFile", mkDescriptor(pathTpe, BackendType.Array(linkOptionTpe))(BackendType.Bool))
      case IoOp.FileIsReadable =>
        INVOKESTATIC(jFiles, "isReadable", mkDescriptor(pathTpe)(BackendType.Bool))
      case IoOp.FileIsSymbolicLink =>
        INVOKESTATIC(jFiles, "isSymbolicLink", mkDescriptor(pathTpe)(BackendType.Bool))
      case IoOp.FileIsWritable =>
        INVOKESTATIC(jFiles, "isWritable", mkDescriptor(pathTpe)(BackendType.Bool))
      case IoOp.FileIsExecutable =>
        INVOKESTATIC(jFiles, "isExecutable", mkDescriptor(pathTpe)(BackendType.Bool))
      case _ =>
        ICONST_0()
    }

    mv.visitVarInsn(ISTORE, boolSlot)

    // Return (true, value, Other, "")
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ILOAD(boolSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushBool(false)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileMetadata(op: UnaryOp, exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jLinkOption = JvmName.ofClass(classOf[java.nio.file.LinkOption])
    val linkOptionTpe = BackendType.Reference(BackendObjType.Native(jLinkOption))
    val jBasicAttrs = JvmName.ofClass(classOf[java.nio.file.attribute.BasicFileAttributes])
    val basicAttrsTpe = BackendType.Reference(BackendObjType.Native(jBasicAttrs))
    val jFileTime = JvmName.ofClass(classOf[java.nio.file.attribute.FileTime])
    val fileTimeTpe = BackendType.Reference(BackendObjType.Native(jFileTime))
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])

    // Locals.
    val filenameSlot = 2610
    val longSlot = 2611
    val exSlot = 2613

    compileExpr(exp)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    // path = Paths.get(filename, new String[0])
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))

    op match {
      case IoOp.FileSize =>
        INVOKESTATIC(jFiles, "size", mkDescriptor(pathTpe)(BackendType.Int64))

      case _ =>
        // attrs = Files.readAttributes(path, BasicFileAttributes.class, new LinkOption[0])
        mv.visitLdcInsn(asm.Type.getType(jBasicAttrs.toDescriptor))
        ICONST_0()
        ANEWARRAY(jLinkOption)
        INVOKESTATIC(jFiles, "readAttributes", mkDescriptor(pathTpe, BackendType.Reference(BackendObjType.Native(JvmName.Class)), BackendType.Array(linkOptionTpe))(basicAttrsTpe))

        // time = attrs.<...Time>().toMillis()
        op match {
          case IoOp.FileAccessTime =>
            INVOKEINTERFACE(jBasicAttrs, "lastAccessTime", mkDescriptor()(fileTimeTpe))
          case IoOp.FileCreationTime =>
            INVOKEINTERFACE(jBasicAttrs, "creationTime", mkDescriptor()(fileTimeTpe))
          case IoOp.FileModificationTime =>
            INVOKEINTERFACE(jBasicAttrs, "lastModifiedTime", mkDescriptor()(fileTimeTpe))
          case _ =>
            LCONST_0()
        }
        INVOKEVIRTUAL(jFileTime, "toMillis", mkDescriptor()(BackendType.Int64))
    }

    mv.visitVarInsn(LSTORE, longSlot)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    LLOAD(longSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    LCONST_0()
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    LCONST_0()
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileRead(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jStandardCharsets = JvmName.ofClass(classOf[java.nio.charset.StandardCharsets])
    val jCharset = JvmName.ofClass(classOf[java.nio.charset.Charset])
    val charsetTpe = BackendType.Reference(BackendObjType.Native(jCharset))
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val filenameSlot = 2620
    val bytesSlot = 2621
    val exSlot = 2622

    compileExpr(exp)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    // bytes = Files.readAllBytes(Paths.get(filename, new String[0]))
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))
    INVOKESTATIC(jFiles, "readAllBytes", mkDescriptor(pathTpe)(BackendType.Array(BackendType.Int8)))
    ASTORE(bytesSlot)

    // str = new String(bytes, StandardCharsets.UTF_8)
    NEW(JvmName.String)
    DUP()
    ALOAD(bytesSlot)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardCharsets.toInternalName, "UTF_8", charsetTpe.toDescriptor)
    INVOKESPECIAL(JvmName.String, JvmName.ConstructorMethod, mkDescriptor(BackendType.Array(BackendType.Int8), charsetTpe)(VoidableType.Void))

    // Return (true, str, Other, "")
    NEW(retTupleType.jvmName)
    DUP_X1()
    SWAP()
    pushBool(true)
    SWAP()
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushString("")
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushString("")
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileReadLines(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jList = JvmName.ofClass(classOf[java.util.List[?]])
    val listTpe = BackendType.Reference(BackendObjType.Native(jList))
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])

    // Locals.
    val filenameSlot = 2630
    val arrSlot = 2631
    val exSlot = 2632

    // Extract (rc, filename) from tuple argument (ignore rc).
    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    POP()
    GETFIELD(argTupleType.IndexField(1))
    CHECKCAST(JvmName.String)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    // lines = Files.readAllLines(Paths.get(filename, new String[0]))
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))
    INVOKESTATIC(jFiles, "readAllLines", mkDescriptor(pathTpe)(listTpe))

    // arr = lines.toArray(new String[0]) as String[]
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKEINTERFACE(jList, "toArray", mkDescriptor(BackendType.Array(BackendType.Object))(BackendType.Array(BackendType.Object)))
    mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.String).toDescriptor)
    ASTORE(arrSlot)

    // Return (true, arr, Other, "")
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ALOAD(arrSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileReadBytes(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])

    // Locals.
    val filenameSlot = 2640
    val bytesSlot = 2641
    val exSlot = 2642

    // Extract (rc, filename) from tuple argument (ignore rc).
    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    POP()
    GETFIELD(argTupleType.IndexField(1))
    CHECKCAST(JvmName.String)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    // bytes = Files.readAllBytes(Paths.get(filename, new String[0]))
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))
    INVOKESTATIC(jFiles, "readAllBytes", mkDescriptor(pathTpe)(BackendType.Array(BackendType.Int8)))
    ASTORE(bytesSlot)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ALOAD(bytesSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    mv.visitIntInsn(NEWARRAY, T_BYTE)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    mv.visitIntInsn(NEWARRAY, T_BYTE)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileList(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jLinkOption = JvmName.ofClass(classOf[java.nio.file.LinkOption])
    val linkOptionTpe = BackendType.Reference(BackendObjType.Native(jLinkOption))
    val jFile = JvmName.ofClass(classOf[java.io.File])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])

    // Locals.
    val filenameSlot = 2650
    val arrSlot = 2651
    val exSlot = 2652

    // Extract (rc, filename) from tuple argument (ignore rc).
    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    POP()
    GETFIELD(argTupleType.IndexField(1))
    CHECKCAST(JvmName.String)
    ASTORE(filenameSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)

    mv.visitLabel(tryStart)
    // Validate path (may throw InvalidPathException).
    ALOAD(filenameSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))

    // If not a directory: return Err(NotDirectory).
    DUP()
    ICONST_0()
    ANEWARRAY(jLinkOption)
    INVOKESTATIC(jFiles, "isDirectory", mkDescriptor(pathTpe, BackendType.Array(linkOptionTpe))(BackendType.Bool))
    val isDir = new Label()
    val notDir = new Label()
    mv.visitJumpInsn(IFNE, isDir)
    mv.visitLabel(notDir)
    POP()
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    pushInt(8)
    pushString("not a directory")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(isDir)
    POP()

    // names = new File(filename).list()
    NEW(jFile)
    DUP()
    ALOAD(filenameSlot)
    INVOKESPECIAL(jFile, JvmName.ConstructorMethod, mkDescriptor(BackendType.String)(VoidableType.Void))
    INVOKEVIRTUAL(jFile, "list", mkDescriptor()(BackendType.Array(BackendType.String)))
    ASTORE(arrSlot)

    // if (names == null) => Other
    val hasNames = new Label()
    ALOAD(arrSlot)
    mv.visitJumpInsn(IFNONNULL, hasNames)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    pushInt(14)
    pushString("I/O error")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(hasNames)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ALOAD(arrSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileWriteString(op: UnaryOp, exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jOpenOption = JvmName.ofClass(classOf[java.nio.file.OpenOption])
    val openOptionTpe = BackendType.Reference(BackendObjType.Native(jOpenOption))
    val jStandardOpenOption = JvmName.ofClass(classOf[java.nio.file.StandardOpenOption])
    val jStandardCharsets = JvmName.ofClass(classOf[java.nio.charset.StandardCharsets])
    val jCharset = JvmName.ofClass(classOf[java.nio.charset.Charset])
    val charsetTpe = BackendType.Reference(BackendObjType.Native(jCharset))
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])
    val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val dataSlot = 2660
    val fileSlot = 2661
    val bytesSlot = 2662
    val exSlot = 2663

    // Extract (data, file) from tuple argument.
    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    CHECKCAST(JvmName.String)
    ASTORE(dataSlot)
    GETFIELD(argTupleType.IndexField(1))
    CHECKCAST(JvmName.String)
    ASTORE(fileSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerUnsupported = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    // bytes = data.getBytes(StandardCharsets.UTF_8)
    ALOAD(dataSlot)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardCharsets.toInternalName, "UTF_8", charsetTpe.toDescriptor)
    INVOKEVIRTUAL(JvmName.String, "getBytes", mkDescriptor(charsetTpe)(BackendType.Array(BackendType.Int8)))
    ASTORE(bytesSlot)

    // Files.write(Paths.get(file), bytes, opts)
    ALOAD(fileSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))
    ALOAD(bytesSlot)

    op match {
      case IoOp.FileAppend =>
        ICONST_2()
        ANEWARRAY(jOpenOption)
        DUP()
        ICONST_0()
        mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardOpenOption.toInternalName, "APPEND", BackendType.Reference(BackendObjType.Native(jStandardOpenOption)).toDescriptor)
        mv.visitInsn(AASTORE)
        DUP()
        ICONST_1()
        mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardOpenOption.toInternalName, "CREATE", BackendType.Reference(BackendObjType.Native(jStandardOpenOption)).toDescriptor)
        mv.visitInsn(AASTORE)
      case _ =>
        ICONST_0()
        ANEWARRAY(jOpenOption)
    }

    INVOKESTATIC(jFiles, "write", mkDescriptor(pathTpe, BackendType.Array(BackendType.Int8), BackendType.Array(openOptionTpe))(pathTpe))
    POP()

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerUnsupported)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(12)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileWriteBytes(op: UnaryOp, exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jOpenOption = JvmName.ofClass(classOf[java.nio.file.OpenOption])
    val openOptionTpe = BackendType.Reference(BackendObjType.Native(jOpenOption))
    val jStandardOpenOption = JvmName.ofClass(classOf[java.nio.file.StandardOpenOption])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])
    val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val bytesSlot = 2670
    val fileSlot = 2671
    val exSlot = 2672

    // Extract (bytes, file) from tuple argument.
    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    mv.visitTypeInsn(org.objectweb.asm.Opcodes.CHECKCAST, BackendType.Array(BackendType.Int8).toDescriptor)
    ASTORE(bytesSlot)
    GETFIELD(argTupleType.IndexField(1))
    CHECKCAST(JvmName.String)
    ASTORE(fileSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerUnsupported = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(fileSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))
    ALOAD(bytesSlot)

    op match {
      case IoOp.FileAppendBytes =>
        ICONST_2()
        ANEWARRAY(jOpenOption)
        DUP()
        ICONST_0()
        mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardOpenOption.toInternalName, "APPEND", BackendType.Reference(BackendObjType.Native(jStandardOpenOption)).toDescriptor)
        mv.visitInsn(AASTORE)
        DUP()
        ICONST_1()
        mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardOpenOption.toInternalName, "CREATE", BackendType.Reference(BackendObjType.Native(jStandardOpenOption)).toDescriptor)
        mv.visitInsn(AASTORE)
      case _ =>
        ICONST_0()
        ANEWARRAY(jOpenOption)
    }

    INVOKESTATIC(jFiles, "write", mkDescriptor(pathTpe, BackendType.Array(BackendType.Int8), BackendType.Array(openOptionTpe))(pathTpe))
    POP()

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerUnsupported)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(12)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileTruncate(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jOpenOption = JvmName.ofClass(classOf[java.nio.file.OpenOption])
    val openOptionTpe = BackendType.Reference(BackendObjType.Native(jOpenOption))
    val jStandardOpenOption = JvmName.ofClass(classOf[java.nio.file.StandardOpenOption])
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])
    val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val fileSlot = 2680
    val exSlot = 2681

    compileExpr(exp)
    ASTORE(fileSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerUnsupported = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(fileSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))

    ICONST_0()
    mv.visitIntInsn(NEWARRAY, T_BYTE)

    ICONST_1()
    ANEWARRAY(jOpenOption)
    DUP()
    ICONST_0()
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, jStandardOpenOption.toInternalName, "TRUNCATE_EXISTING", BackendType.Reference(BackendObjType.Native(jStandardOpenOption)).toDescriptor)
    mv.visitInsn(AASTORE)

    INVOKESTATIC(jFiles, "write", mkDescriptor(pathTpe, BackendType.Array(BackendType.Int8), BackendType.Array(openOptionTpe))(pathTpe))
    POP()

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerUnsupported)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(12)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileMkDir(op: UnaryOp, exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jPaths = JvmName.ofClass(classOf[java.nio.file.Paths])
    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jFileAttr = JvmName.ofClass(classOf[java.nio.file.attribute.FileAttribute[?]])
    val fileAttrTpe = BackendType.Reference(BackendObjType.Native(jFileAttr))
    val jInvalidPath = JvmName.ofClass(classOf[java.nio.file.InvalidPathException])
    val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
    val jAlreadyExists = JvmName.ofClass(classOf[java.nio.file.FileAlreadyExistsException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val dirSlot = 2690
    val exSlot = 2691

    compileExpr(exp)
    ASTORE(dirSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerInvalid = new Label()
    val handlerUnsupported = new Label()
    val handlerExists = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerInvalid, jInvalidPath.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerExists, jAlreadyExists.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(dirSlot)
    ICONST_0()
    ANEWARRAY(JvmName.String)
    INVOKESTATIC(jPaths, "get", mkDescriptor(BackendType.String, BackendType.Array(BackendType.String))(pathTpe))

    ICONST_0()
    ANEWARRAY(jFileAttr)

    op match {
      case IoOp.FileMkDirs =>
        INVOKESTATIC(jFiles, "createDirectories", mkDescriptor(pathTpe, BackendType.Array(fileAttrTpe))(pathTpe))
      case _ =>
        INVOKESTATIC(jFiles, "createDirectory", mkDescriptor(pathTpe, BackendType.Array(fileAttrTpe))(pathTpe))
    }
    POP()

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerInvalid)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerUnsupported)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(12)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerExists)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(0)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    GETSTATIC(BackendObjType.Unit.SingletonField)
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileIoFileMkTempDir(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val jFiles = JvmName.ofClass(classOf[java.nio.file.Files])
    val jFileAttr = JvmName.ofClass(classOf[java.nio.file.attribute.FileAttribute[?]])
    val fileAttrTpe = BackendType.Reference(BackendObjType.Native(jFileAttr))
    val jPath = JvmName.ofClass(classOf[java.nio.file.Path])
    val pathTpe = BackendType.Reference(BackendObjType.Native(jPath))
    val jIllegalArg = JvmName.ofClass(classOf[java.lang.IllegalArgumentException])
    val jUnsupported = JvmName.ofClass(classOf[java.lang.UnsupportedOperationException])
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    // Locals.
    val prefixSlot = 2700
    val pathSlot = 2701
    val exSlot = 2702

    compileExpr(exp)
    ASTORE(prefixSlot)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerIllegalArg = new Label()
    val handlerUnsupported = new Label()
    val handlerIo = new Label()
    val after = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIllegalArg, jIllegalArg.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerUnsupported, jUnsupported.toInternalName)
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerIo, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(prefixSlot)
    ICONST_0()
    ANEWARRAY(jFileAttr)
    INVOKESTATIC(jFiles, "createTempDirectory", mkDescriptor(BackendType.String, BackendType.Array(fileAttrTpe))(pathTpe))
    ASTORE(pathSlot)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ALOAD(pathSlot)
    INVOKEINTERFACE(jPath, "toString", mkDescriptor()(BackendType.String))
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIllegalArg)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushString("")
    pushInt(3)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerUnsupported)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushString("")
    pushInt(12)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerIo)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    pushString("")
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def getStructType(struct: Struct)(implicit root: Root): BackendObjType.Struct = {
    BackendObjType.Struct(struct.fields.map(field => BackendType.toBackendType(field.tpe)))
  }

  private def compileIsTag(name: String, exp: Expr, tpes: List[BackendType])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    compileExpr(exp)
    tpes match {
      case Nil =>
        INSTANCEOF(BackendObjType.NullaryTag(name).jvmName)
      case _ =>
        CHECKCAST(BackendObjType.Tagged.jvmName)
        GETFIELD(BackendObjType.Tagged.NameField)
        BackendObjType.Tagged.mkTagName(name)
        BackendObjType.Tagged.eqTagName()
    }
  }

  private def compileTag(name: String, exps: List[Expr], tpes: List[BackendType])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    tpes match {
      case Nil =>
        GETSTATIC(BackendObjType.NullaryTag(name).SingletonField)
      case _ =>
        val tagType = BackendObjType.Tag(tpes)
        NEW(tagType.jvmName)
        DUP()
        INVOKESPECIAL(tagType.Constructor)
        DUP()
        BackendObjType.Tagged.mkTagName(name)
        PUTFIELD(tagType.NameField)
        exps.zipWithIndex.foreach {
          case (e, i) => DUP()
            compileExpr(e)
            PUTFIELD(tagType.IndexField(i))
        }
    }
  }

  private def compileUntag(exp: Expr, idx: Int, tpes: List[BackendType])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    // BackendObjType.NullaryTag cannot happen here since terms must be non-empty.
    if (tpes.isEmpty) throw InternalCompilerException(s"Unexpected empty tag types", exp.loc)
    val tagType = BackendObjType.Tag(tpes)
    compileExpr(exp)
    CHECKCAST(tagType.jvmName)
    GETFIELD(tagType.IndexField(idx))
  }

  private def visitComparisonPrologue(exp1: Expr, exp2: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): (Label, Label) = {
    compileExpr(exp1)
    compileExpr(exp2)
    val condElse = new Label()
    val condEnd = new Label()
    (condElse, condEnd)
  }

  private def visitComparisonEpilogue(visitor: MethodVisitor, condElse: Label, condEnd: Label): Unit = {
    visitor.visitInsn(ICONST_1)
    visitor.visitJumpInsn(GOTO, condEnd)
    visitor.visitLabel(condElse)
    visitor.visitInsn(ICONST_0)
    visitor.visitLabel(condEnd)
  }

  private def visitComparison1(exp1: Expr, exp2: Expr, opcode: Int)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
    mv.visitJumpInsn(opcode, condElse)
    visitComparisonEpilogue(mv, condElse, condEnd)
  }

  private def visitComparison2(exp1: Expr, exp2: Expr, opcode: Int, cmpOpcode: Int)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
    mv.visitInsn(opcode)
    mv.visitJumpInsn(cmpOpcode, condElse)
    visitComparisonEpilogue(mv, condElse, condEnd)
  }

  private def compileBigIntUnaryOp(op: BigIntOp, exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    op match {
      case BigIntOp.Neg =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "negate",
          mkDescriptor()(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Not =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "not",
          mkDescriptor()(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.BitLength =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "bitLength",
          mkDescriptor()(BackendType.Int32).toDescriptor, false)
      case BigIntOp.FromInt64 =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.BigInteger.toInternalName, "valueOf",
          mkDescriptor(BackendType.Int64)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case _ =>
        throw InternalCompilerException(s"Unexpected BigInt unary op: '$op'.", SourceLocation.Unknown)
    }
  }

  private def compileBigIntToString(exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "toString",
      mkDescriptor()(BackendType.String).toDescriptor, false)
  }

  private def compileBigIntFromString(exp: Expr, tpe: SimpleType)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    val SimpleType.Tuple(elmTypes) = tpe
    val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerStart = new Label()
    val after = new Label()

    mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

    mv.visitLabel(tryStart)
    NEW(tupleType.jvmName)
    DUP()
    pushBool(true)
    NEW(JvmName.BigInteger)
    DUP()
    compileExpr(exp)
    mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
      mkDescriptor()(BackendType.String).toDescriptor, false)
    INVOKESPECIAL(ClassConstants.BigInteger.Constructor)
    INVOKESPECIAL(tupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerStart)
    POP()
    NEW(tupleType.jvmName)
    DUP()
    pushBool(false)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, JvmName.BigInteger.toInternalName, "ZERO", JvmName.BigInteger.toDescriptor)
    INVOKESPECIAL(tupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileBigIntHash(exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "hashCode",
      mkDescriptor()(BackendType.Int32).toDescriptor, false)
  }

  private def compileBigIntBinaryOp(op: BigIntOp, exp1: Expr, exp2: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp1)
    compileExpr(exp2)
    op match {
      case BigIntOp.Add =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "add",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Sub =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "subtract",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Mul =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "multiply",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Div =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "divide",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Rem =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "remainder",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Shl =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "shiftLeft",
          mkDescriptor(BackendType.Int32)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Shr =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "shiftRight",
          mkDescriptor(BackendType.Int32)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.And =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "and",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Or =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "or",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Xor =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "xor",
          mkDescriptor(JvmName.BigInteger.toTpe)(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigIntOp.Cmp =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "compareTo",
          mkDescriptor(JvmName.BigInteger.toTpe)(BackendType.Int32).toDescriptor, false)
      case _ =>
        throw InternalCompilerException(s"Unexpected BigInt binary op: '$op'.", SourceLocation.Unknown)
    }
  }

  private def compileBigDecimalUnaryOp(op: BigDecimalOp, exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    op match {
      case BigDecimalOp.Neg =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "negate",
          mkDescriptor()(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Scale =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "scale",
          mkDescriptor()(BackendType.Int32).toDescriptor, false)
      case BigDecimalOp.Precision =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "precision",
          mkDescriptor()(BackendType.Int32).toDescriptor, false)
      case BigDecimalOp.Ceil =>
        BytecodeInstructions.pushInt(0)
        mv.visitFieldInsn(GETSTATIC, JvmName.RoundingMode.toInternalName, "CEILING", JvmName.RoundingMode.toDescriptor)
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "setScale",
          mkDescriptor(BackendType.Int32, JvmName.RoundingMode.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Floor =>
        BytecodeInstructions.pushInt(0)
        mv.visitFieldInsn(GETSTATIC, JvmName.RoundingMode.toInternalName, "FLOOR", JvmName.RoundingMode.toDescriptor)
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "setScale",
          mkDescriptor(BackendType.Int32, JvmName.RoundingMode.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Round =>
        BytecodeInstructions.pushInt(0)
        mv.visitFieldInsn(GETSTATIC, JvmName.RoundingMode.toInternalName, "HALF_EVEN", JvmName.RoundingMode.toDescriptor)
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "setScale",
          mkDescriptor(BackendType.Int32, JvmName.RoundingMode.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.ToBigInt =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "toBigInteger",
          mkDescriptor()(JvmName.BigInteger.toTpe).toDescriptor, false)
      case BigDecimalOp.ToPlainString =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "toPlainString",
          mkDescriptor()(BackendType.String).toDescriptor, false)
      case _ =>
        throw InternalCompilerException(s"Unexpected BigDecimal unary op: '$op'.", SourceLocation.Unknown)
    }
  }

  private def compileBigDecimalToString(exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "toString",
      mkDescriptor()(BackendType.String).toDescriptor, false)
  }

  private def compileBigDecimalFromString(exp: Expr, tpe: SimpleType)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    val SimpleType.Tuple(elmTypes) = tpe
    val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.toBackendType))

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerStart = new Label()
    val after = new Label()

    mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.NumberFormatException.toInternalName)

    mv.visitLabel(tryStart)
    NEW(tupleType.jvmName)
    DUP()
    pushBool(true)
    NEW(JvmName.BigDecimal)
    DUP()
    compileExpr(exp)
    mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKEVIRTUAL, JvmName.String.toInternalName, "strip",
      mkDescriptor()(BackendType.String).toDescriptor, false)
    INVOKESPECIAL(ClassConstants.BigDecimal.Constructor)
    INVOKESPECIAL(tupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerStart)
    POP()
    NEW(tupleType.jvmName)
    DUP()
    pushBool(false)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETSTATIC, JvmName.BigDecimal.toInternalName, "ZERO", JvmName.BigDecimal.toDescriptor)
    INVOKESPECIAL(tupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileBigDecimalHash(exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "hashCode",
      mkDescriptor()(BackendType.Int32).toDescriptor, false)
  }

  private def compileBigDecimalBinaryOp(op: BigDecimalOp, exp1: Expr, exp2: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp1)
    compileExpr(exp2)
    op match {
      case BigDecimalOp.Add =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "add",
          mkDescriptor(JvmName.BigDecimal.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Sub =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "subtract",
          mkDescriptor(JvmName.BigDecimal.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Mul =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "multiply",
          mkDescriptor(JvmName.BigDecimal.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Div =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "divide",
          mkDescriptor(JvmName.BigDecimal.toTpe)(JvmName.BigDecimal.toTpe).toDescriptor, false)
      case BigDecimalOp.Cmp =>
        mv.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigDecimal.toInternalName, "compareTo",
          mkDescriptor(JvmName.BigDecimal.toTpe)(BackendType.Int32).toDescriptor, false)
      case _ =>
        throw InternalCompilerException(s"Unexpected BigDecimal binary op: '$op'.", SourceLocation.Unknown)
    }
  }

  private def compileCodePointUnaryOp(op: CodePointOp, exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    op match {
      case CodePointOp.IsLetter =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isLetter",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsDigit =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isDigit",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsLowerCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isLowerCase",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsUpperCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isUpperCase",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsTitleCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isTitleCase",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsWhitespace =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isWhitespace",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsAlphabetic =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isAlphabetic",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsDefined =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isDefined",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsIdeographic =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isIdeographic",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsISOControl =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isISOControl",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.IsMirrored =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "isMirrored",
          mkDescriptor(BackendType.Int32)(BackendType.Bool).toDescriptor, false)
      case CodePointOp.ToLowerCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toLowerCase",
          mkDescriptor(BackendType.Int32)(BackendType.Int32).toDescriptor, false)
      case CodePointOp.ToUpperCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toUpperCase",
          mkDescriptor(BackendType.Int32)(BackendType.Int32).toDescriptor, false)
      case CodePointOp.ToTitleCase =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "toTitleCase",
          mkDescriptor(BackendType.Int32)(BackendType.Int32).toDescriptor, false)
      case CodePointOp.GetNumericValue =>
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Character.toInternalName, "getNumericValue",
          mkDescriptor(BackendType.Int32)(BackendType.Int32).toDescriptor, false)
      case _ =>
        throw InternalCompilerException(s"Unexpected CodePoint unary op: '$op'.", SourceLocation.Unknown)
    }
  }

  private def compileCodePointGetName(exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerStart = new Label()
    val after = new Label()

    mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, JvmName.Exception.toInternalName)

    mv.visitLabel(tryStart)
    compileExpr(exp)
    mv.visitMethodInsn(org.objectweb.asm.Opcodes.INVOKESTATIC, JvmName.Character.toInternalName, "getName",
      mkDescriptor(BackendType.Int32)(BackendType.String).toDescriptor, false)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerStart)
    POP()
    ACONST_NULL()
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileProcessStdinWrite(exp: Expr, tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jGlobal = BackendObjType.Global.jvmName
    val jProcess = JvmName.ofClass(classOf[java.lang.Process])
    val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
    val jOutputStream = JvmName.ofClass(classOf[java.io.OutputStream])
    val outputStreamTpe = BackendType.Reference(BackendObjType.Native(jOutputStream))
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    val idSlot = 2140
    val bufSlot = 2142
    val lenSlot = 2143
    val exSlot = 2144
    val procSlot = 2145

    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    mv.visitVarInsn(LSTORE, idSlot)
    GETFIELD(argTupleType.IndexField(1))
    ASTORE(bufSlot)

    LLOAD(idSlot)
    INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
    ASTORE(procSlot)

    val hasProc = new Label()
    val after = new Label()
    ALOAD(procSlot)
    mv.visitJumpInsn(IFNONNULL, hasProc)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    pushInt(14)
    pushString("invalid process handle.")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(hasProc)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerStart = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(procSlot)
    INVOKEVIRTUAL(jProcess, "getOutputStream", mkDescriptor()(outputStreamTpe))
    ALOAD(bufSlot)
    INVOKEVIRTUAL(jOutputStream, "write", mkDescriptor(BackendType.Array(BackendType.Int8))(VoidableType.Void))

    ALOAD(bufSlot)
    ARRAYLENGTH()
    mv.visitVarInsn(ISTORE, lenSlot)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ILOAD(lenSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerStart)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileProcessStreamRead(exp: Expr, tpe: SimpleType, loc: SourceLocation, stderr: Boolean)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*
    BytecodeInstructions.addLoc(loc)

    val SimpleType.Tuple(retElmTypes) = tpe
    val retTupleType = BackendObjType.Tuple(retElmTypes.map(BackendType.toBackendType))

    val SimpleType.Tuple(argElmTypes) = exp.tpe
    val argTupleType = BackendObjType.Tuple(argElmTypes.map(BackendType.toBackendType))

    val jGlobal = BackendObjType.Global.jvmName
    val jProcess = JvmName.ofClass(classOf[java.lang.Process])
    val processTpe = BackendType.Reference(BackendObjType.Native(jProcess))
    val jInputStream = JvmName.ofClass(classOf[java.io.InputStream])
    val inputStreamTpe = BackendType.Reference(BackendObjType.Native(jInputStream))
    val jIOException = JvmName.ofClass(classOf[java.io.IOException])

    val idSlot = if (stderr) 2160 else 2150
    val bufSlot = if (stderr) 2162 else 2152
    val numSlot = if (stderr) 2163 else 2153
    val exSlot = if (stderr) 2164 else 2154
    val procSlot = if (stderr) 2165 else 2155

    compileExpr(exp)
    DUP()
    GETFIELD(argTupleType.IndexField(0))
    mv.visitVarInsn(LSTORE, idSlot)
    GETFIELD(argTupleType.IndexField(1))
    ASTORE(bufSlot)

    LLOAD(idSlot)
    INVOKESTATIC(jGlobal, "getProcess", mkDescriptor(BackendType.Int64)(processTpe))
    ASTORE(procSlot)

    val hasProc = new Label()
    val after = new Label()
    ALOAD(procSlot)
    mv.visitJumpInsn(IFNONNULL, hasProc)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    pushInt(14)
    pushString("invalid process handle.")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(hasProc)

    val tryStart = new Label()
    val tryEnd = new Label()
    val handlerStart = new Label()
    mv.visitTryCatchBlock(tryStart, tryEnd, handlerStart, jIOException.toInternalName)

    mv.visitLabel(tryStart)
    ALOAD(procSlot)
    INVOKEVIRTUAL(jProcess, if (stderr) "getErrorStream" else "getInputStream", mkDescriptor()(inputStreamTpe))
    ALOAD(bufSlot)
    INVOKEVIRTUAL(jInputStream, "read", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendType.Int32))

    val notEof = new Label()
    val afterEof = new Label()
    DUP()
    ICONST_M1()
    mv.visitJumpInsn(IF_ICMPNE, notEof)
    POP()
    ICONST_0()
    mv.visitLabel(notEof)
    mv.visitLabel(afterEof)
    mv.visitVarInsn(ISTORE, numSlot)

    NEW(retTupleType.jvmName)
    DUP()
    pushBool(true)
    ILOAD(numSlot)
    pushInt(14)
    pushString("")
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(handlerStart)
    ASTORE(exSlot)
    NEW(retTupleType.jvmName)
    DUP()
    pushBool(false)
    ICONST_0()
    pushInt(14)
    ALOAD(exSlot)
    INVOKEVIRTUAL(JvmName.Throwable, "getMessage", mkDescriptor()(BackendType.String))
    INVOKESPECIAL(retTupleType.Constructor)
    mv.visitJumpInsn(GOTO, after)

    mv.visitLabel(after)
  }

  private def compileSyncAtomic(op: AtomicOp, exps: List[Expr], tpe: SimpleType, loc: SourceLocation)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    import BytecodeInstructions.*

    op match {
      case AtomicOp.ReentrantLockNew =>
        addLoc(loc)
        INVOKESTATIC(JvmName.LockSupport, "newLock", mkDescriptor()(BackendType.Object))

      case AtomicOp.ReentrantLockLock =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "lock", mkDescriptor(BackendType.Object)(VoidableType.Void))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.ReentrantLockTryLock =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "tryLock", mkDescriptor(BackendType.Object)(BackendType.Bool))

      case AtomicOp.ReentrantLockUnlock =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "unlock", mkDescriptor(BackendType.Object)(BackendType.Bool))

      case AtomicOp.ConditionNew =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "newCondition", mkDescriptor(BackendType.Object)(BackendType.Object))

      case AtomicOp.ConditionAwait =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "awaitCondition", mkDescriptor(BackendType.Object)(BackendType.Int32))

      case AtomicOp.ConditionSignal =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "signalCondition", mkDescriptor(BackendType.Object)(BackendType.Bool))

      case AtomicOp.ConditionSignalAll =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "signalAllCondition", mkDescriptor(BackendType.Object)(BackendType.Bool))

      case AtomicOp.CyclicBarrierNew =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "newBarrier", mkDescriptor(BackendType.Int32)(BackendType.Object))

      case AtomicOp.CyclicBarrierAwait =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "awaitBarrier", mkDescriptor(BackendType.Object)(BackendType.Int32))

      case AtomicOp.CountDownLatchNew =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "newCountDownLatch", mkDescriptor(BackendType.Int32)(BackendType.Object))

      case AtomicOp.CountDownLatchAwait =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "awaitCountDownLatch", mkDescriptor(BackendType.Object)(VoidableType.Void))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.CountDownLatchCountDown =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "countDownCountDownLatch", mkDescriptor(BackendType.Object)(VoidableType.Void))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.SemaphoreNew =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "newSemaphore", mkDescriptor(BackendType.Int32)(BackendType.Object))

      case AtomicOp.SemaphoreAcquire =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "acquireSemaphore", mkDescriptor(BackendType.Object)(VoidableType.Void))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case AtomicOp.SemaphoreTryAcquire =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "tryAcquireSemaphore", mkDescriptor(BackendType.Object)(BackendType.Bool))

      case AtomicOp.SemaphoreRelease =>
        val List(exp) = exps
        addLoc(loc)
        compileExpr(exp)
        INVOKESTATIC(JvmName.LockSupport, "releaseSemaphore", mkDescriptor(BackendType.Object)(VoidableType.Void))
        GETSTATIC(BackendObjType.Unit.SingletonField)

      case _ =>
        throw InternalCompilerException(s"Unexpected sync atomic op: $op.", loc)
    }
  }

  /**
    * Generate bytecode for the long `i`.
    *
    * Uses the smallest amount of bytes necessary to represent `i`.
    * Similar to `compileInt`, but ensures that values take up 4 bytes
    * on the stack, which is expected for `Long`s.
    */
  private def compileLong(i: Long)(implicit mv: MethodVisitor): Unit = i match {
    case -1 =>
      mv.visitInsn(ICONST_M1)
      mv.visitInsn(I2L) // Sign extend to long

    case 0 =>
      mv.visitInsn(LCONST_0)

    case 1 =>
      mv.visitInsn(LCONST_1)

    case 2 =>
      mv.visitInsn(ICONST_2)
      mv.visitInsn(I2L) // Sign extend to long

    case 3 =>
      mv.visitInsn(ICONST_3)
      mv.visitInsn(I2L) // Sign extend to long

    case 4 =>
      mv.visitInsn(ICONST_4)
      mv.visitInsn(I2L) // Sign extend to long

    case 5 =>
      mv.visitInsn(ICONST_5)
      mv.visitInsn(I2L) // Sign extend to long

    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue =>
      mv.visitIntInsn(BIPUSH, i.toInt)
      mv.visitInsn(I2L) // Sign extend to long

    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue =>
      mv.visitIntInsn(SIPUSH, i.toInt)
      mv.visitInsn(I2L) // Sign extend to long

    case _ if scala.Int.MinValue <= i && i <= scala.Int.MaxValue =>
      mv.visitLdcInsn(i.toInt)
      mv.visitInsn(I2L) // Sign extend to long

    case _ => mv.visitLdcInsn(i)
  }

}
