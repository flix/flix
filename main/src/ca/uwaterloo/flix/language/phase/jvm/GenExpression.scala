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
        compileExpr(exp)

        sop match {
          case SemanticOp.BoolOp.Not =>
            val condElse = new Label()
            val condEnd = new Label()
            mv.visitJumpInsn(IFNE, condElse)
            mv.visitInsn(ICONST_1)
            mv.visitJumpInsn(GOTO, condEnd)
            mv.visitLabel(condElse)
            mv.visitInsn(ICONST_0)
            mv.visitLabel(condEnd)

          case Float32Op.Neg => mv.visitInsn(FNEG)

          case Float64Op.Neg => mv.visitInsn(DNEG)

          case Int8Op.Neg =>
            mv.visitInsn(INEG)
            mv.visitInsn(I2B) // Sign extend so sign bit is also changed

          case Int16Op.Neg =>
            mv.visitInsn(INEG)
            mv.visitInsn(I2S) // Sign extend so sign bit is also changed

          case Int32Op.Neg => mv.visitInsn(INEG)

          case Int64Op.Neg => mv.visitInsn(LNEG)

          case Int8Op.Not | Int16Op.Not | Int32Op.Not =>
            mv.visitInsn(ICONST_M1)
            mv.visitInsn(IXOR)

          case Int64Op.Not =>
            mv.visitInsn(ICONST_M1)
            mv.visitInsn(I2L)
            mv.visitInsn(LXOR)
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
        compileExpr(exp)
        ATHROW()

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

    case Expr.ApplyClo(exp1, exp2, ct, _, purity, loc) =>
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
                val pcPoint = pcCounter(0) + 1
                val pcPointLabel = pcLabels(pcPoint)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc)
                mv.visitJumpInsn(GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)

                mv.visitVarInsn(ALOAD, 1)

                mv.visitLabel(afterUnboxing)

              case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
                throw InternalCompilerException("Unexpected direct method context in control impure function", loc)
            }
          }
      }

    case Expr.ApplyDef(sym, exps, ct, _, _, loc) => ct match {
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
          mv.visitMethodInsn(INVOKESTATIC, className.toInternalName, JvmName.DirectApply, desc.toDescriptor, false)
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
                val pcPoint = pcCounter(0) + 1
                val pcPointLabel = pcLabels(pcPoint)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc)
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

    case Expr.ApplyOp(sym, exps, tpe, _, loc) => ctx match {
      case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
        BackendObjType.Result.crashIfSuspension("Unexpected do-expression in direct method context", loc)

      case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
        import BackendObjType.Suspension
        import BytecodeInstructions.*

        val pcPoint = pcCounter(0) + 1
        val pcPointLabel = pcLabels(pcPoint)
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
        pushInt(pcPoint)
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
      BytecodeInstructions.xLoad(BackendObjType.Region.toTpe, JvmOps.getIndex(offset, ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitInsn(ATHROW)
      mv.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      // Add source line number for debugging.
      BytecodeInstructions.addLoc(loc)

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
      for ((CatchRule(_, _, clazz, _), handlerLabel) <- rulesAndLabels) {
        mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
      }

      // Add the label after both the try and catch rules.
      mv.visitLabel(afterTryAndCatch)

    case Expr.RunWith(exp, effUse, rules, ct, _, _, loc) =>
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
            val pcPoint = pcCounter(0) + 1
            val pcPointLabel = pcLabels(pcPoint)
            val afterUnboxing = new Label()
            pcCounter(0) += 1
            BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc)
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
