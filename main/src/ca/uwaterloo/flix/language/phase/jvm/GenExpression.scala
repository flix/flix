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
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.SemanticOp.*
import ca.uwaterloo.flix.language.ast.shared.{Constant, ExpPosition}
import ca.uwaterloo.flix.language.ast.{MonoType, *}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.{InstructionSet, MethodEnricher}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
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
        case ctx: DirectContext =>
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
                           newFrame: InstructionSet, // [...] -> [..., frame]
                           setPc: InstructionSet, // [..., frame, pc] -> [...]
                           localOffset: Int,
                           pcLabels: Vector[Label],
                           pcCounter: Ref[Int]
                          ) extends MethodContext

  /**
    * A context for control pure functions that do not closure capture any variables.
    * Such functions never need to record their state and will always
    * return at the given return expressions except if they loop indefinitely.
    */
  case class DirectContext(entryPoint: Label,
                           lenv: Map[Symbol.LabelSym, Label],
                           localOffset: Int,
                          ) extends MethodContext

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpr(exp0: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = exp0 match {

    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => mv.visitByteIns({
        BytecodeInstructions.GETSTATIC(BackendObjType.Unit.SingletonField)
      })

      case Constant.Null => mv.visitByteIns({
        import BytecodeInstructions.*
        ACONST_NULL() ~
          castIfNotPrim(BackendType.toBackendType(tpe))
      })

      case Constant.Bool(b) => mv.visitByteIns({
        BytecodeInstructions.pushBool(b)
      })

      case Constant.Char(c) =>
        compileInt(c)

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

      case Constant.BigDecimal(dd) => mv.visitByteIns({
        import BytecodeInstructions.*
        // Can fail with NumberFormatException
        addLoc(loc) ~
          NEW(BackendObjType.BigDecimal.jvmName) ~
          DUP() ~
          pushString(dd.toString) ~
          INVOKESPECIAL(BackendObjType.BigDecimal.Constructor)
      })

      case Constant.Int8(b) =>
        compileInt(b)

      case Constant.Int16(s) =>
        compileInt(s)

      case Constant.Int32(i) =>
        compileInt(i)

      case Constant.Int64(l) =>
        compileLong(l)

      case Constant.BigInt(ii) => mv.visitByteIns({
        import BytecodeInstructions.*
        // Add source line number for debugging (can fail with NumberFormatException)
        addLoc(loc) ~
          NEW(BackendObjType.BigInt.jvmName) ~
          DUP() ~
          pushString(ii.toString) ~
          INVOKESPECIAL(BackendObjType.BigInt.Constructor)
      })

      case Constant.Str(s) => mv.visitByteIns({
        BytecodeInstructions.pushString(s)
      })

      case Constant.Regex(patt) => mv.visitByteIns({
        import BytecodeInstructions.*
        // Add source line number for debugging (can fail with PatternSyntaxException)
        addLoc(loc) ~
          pushString(patt.pattern) ~
          INVOKESTATIC(BackendObjType.Regex.CompileMethod)
      })

      case Constant.RecordEmpty => mv.visitByteIns({
        BytecodeInstructions.GETSTATIC(BackendObjType.RecordEmpty.SingletonField)
      })

    }

    case Expr.Var(sym, tpe, _) => mv.visitByteIns({
      BytecodeInstructions.xLoad(BackendType.toBackendType(tpe), sym.getStackOffset(ctx.localOffset))
    })

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
          val erasedArgType = JvmOps.getErasedJvmType(arg.tpe)
          mv.visitInsn(DUP)
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, jvmName.toInternalName, s"clo$i", erasedArgType.toDescriptor)
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
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            mv.visitInsn(D2F) // Convert double to float

          case Float64Op.Exp =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)

          case Int8Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            mv.visitInsn(D2I) // Convert to int
            mv.visitInsn(I2B) // Convert int to byte

          case Int16Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            mv.visitInsn(D2I) // Convert to int
            mv.visitInsn(I2S) // Convert int to short

          case Int32Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            mv.visitInsn(D2I) // Convert to int

          case Int64Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(L2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(L2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
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

      case AtomicOp.Region => mv.visitByteIns({
        import BytecodeInstructions.*
        //!TODO: For now, just emit null
        ACONST_NULL() ~
          CHECKCAST(BackendObjType.Region.jvmName)
      })

      case AtomicOp.Is(sym) => mv.visitByteIns({
        val List(exp) = exps
        val MonoType.Enum(_, targs) = exp.tpe
        val cases = JvmOps.instantiateEnum(root.enums(sym.enumSym), targs)
        val termTypes = cases(sym)
        compileIsTag(sym.name, exp, termTypes)
      })

      case AtomicOp.Tag(sym) => mv.visitByteIns({
        val MonoType.Enum(_, targs) = tpe
        val cases = JvmOps.instantiateEnum(root.enums(sym.enumSym), targs)
        val termTypes = cases(sym)
        compileTag(sym.name, exps, termTypes)
      })

      case AtomicOp.Untag(sym, idx) => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp) = exps
        val MonoType.Enum(_, targs) = exp.tpe
        val cases = JvmOps.instantiateEnum(root.enums(sym.enumSym), targs)
        val termTypes = cases(sym)

        compileUntag(exp, idx, termTypes) ~
          castIfNotPrim(BackendType.toBackendType(tpe))
      })

      case AtomicOp.Index(idx) =>
        val List(exp) = exps
        val MonoType.Tuple(elmTypes) = exp.tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.asErasedBackendType))
        // evaluating the `base`
        compileExpr(exp)
        // Retrieving the field `field${offset}`
        mv.visitFieldInsn(GETFIELD, tupleType.jvmName.toInternalName, s"field$idx", JvmOps.asErasedJvmType(tpe).toDescriptor)

      case AtomicOp.Tuple => mv.visitByteIns({
        import BytecodeInstructions.*
        val MonoType.Tuple(elmTypes) = tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.asErasedBackendType))
        NEW(tupleType.jvmName) ~
          DUP() ~
          composeN(exps.map(pushExpr)) ~
          INVOKESPECIAL(tupleType.Constructor)
      })

      case AtomicOp.RecordSelect(field) =>
        val List(exp) = exps

        val interfaceType = BackendObjType.Record

        // Compile the expression exp (which should be a record), as we need to have on the stack a record in order to call
        // lookupField
        compileExpr(exp)

        // Push the desired label of the field we want get of the record onto the stack
        mv.visitLdcInsn(field.name)

        // Invoke the lookupField method on the record. (To get the proper record object)
        mv.visitMethodInsn(INVOKEINTERFACE, interfaceType.jvmName.toInternalName, "lookupField",
          MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(interfaceType.toTpe).toDescriptor, true)

        // Now that the specific RecordExtend object is found, we cast it to its exact class
        val recordType = BackendObjType.RecordExtend(BackendType.toErasedBackendType(tpe))
        val recordInternalName = recordType.jvmName.toInternalName

        mv.visitTypeInsn(CHECKCAST, recordInternalName)

        // Retrieve the value field  (To get the proper value)
        mv.visitFieldInsn(GETFIELD, recordInternalName, recordType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)

      case AtomicOp.RecordExtend(field) => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        val recordType = BackendObjType.RecordExtend(BackendType.toErasedBackendType(exp1.tpe))
        NEW(recordType.jvmName) ~
          DUP() ~
          INVOKESPECIAL(recordType.Constructor) ~
          DUP() ~
          pushString(field.name) ~
          PUTFIELD(recordType.LabelField) ~
          DUP() ~
          pushExpr(exp1) ~
          PUTFIELD(recordType.ValueField) ~
          DUP() ~
          pushExpr(exp2) ~
          PUTFIELD(recordType.RestField)
      })

      case AtomicOp.RecordRestrict(field) =>
        val List(exp) = exps

        // We get the JvmType of the record interface
        val interfaceType = BackendObjType.Record

        // Push the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        compileExpr(exp)
        // Push the label of field (which is going to be the removed/restricted).
        mv.visitLdcInsn(field.name)

        // Invoking the restrictField method
        mv.visitMethodInsn(INVOKEINTERFACE, interfaceType.jvmName.toInternalName, interfaceType.RestrictFieldMethod.name,
          MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(interfaceType.toTpe).toDescriptor, true)

      case AtomicOp.ExtensibleIs(sym) => mv.visitByteIns({
        val List(exp) = exps
        val tpes = MonoType.findExtensibleTermTypes(sym, exp.tpe).map(BackendType.asErasedBackendType)
        compileIsTag(sym.name, exp, tpes)
      })

      case AtomicOp.ExtensibleTag(sym) => mv.visitByteIns({
        val tpes = MonoType.findExtensibleTermTypes(sym, tpe).map(BackendType.asErasedBackendType)
        compileTag(sym.name, exps, tpes)
      })

      case AtomicOp.ExtensibleUntag(sym, idx) => mv.visitByteIns({
        import BytecodeInstructions.*

        val List(exp) = exps
        val tpes = MonoType.findExtensibleTermTypes(sym, exp.tpe).map(BackendType.asErasedBackendType)

        compileUntag(exp, idx, tpes) ~
          castIfNotPrim(BackendType.toBackendType(tpe))
      })

      case AtomicOp.ArrayLit => mv.visitByteIns({
        import BytecodeInstructions.*
        val innerType = tpe.asInstanceOf[MonoType.Array].tpe
        val backendType = BackendType.toBackendType(innerType)

        pushInt(exps.length) ~
          xNewArray(backendType) ~
          composeN(for ((e, i) <- exps.zipWithIndex) yield {
            DUP() ~
              pushInt(i) ~
              pushExpr(e) ~
              xArrayStore(backendType)
          })
      })

      case AtomicOp.ArrayNew => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        // We get the inner type of the array
        val innerType = tpe.asInstanceOf[MonoType.Array].tpe
        val backendType = BackendType.toBackendType(innerType)
        val fillMethod = ClassMaker.StaticMethod(JvmName.Arrays, "fill", mkDescriptor(BackendType.Array(backendType.toErased), backendType.toErased)(VoidableType.Void))
        pushExpr(exp1) ~                                                      // default
          pushExpr(exp2) ~                                                    // default, length
          xNewArray(backendType) ~                                            // default, arr
          (if (backendType.is64BitWidth) DUP_X2() else DUP_X1()) ~            // arr, default, arr
          xSwap(lowerLarge = backendType.is64BitWidth, higherLarge = false) ~ // arr, arr, default
          INVOKESTATIC(fillMethod)
      })

      case AtomicOp.ArrayLoad =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail with out of bounds)
        addSourceLine(mv, loc)

        // We get the jvmType of the element to be loaded
        val jvmType = JvmOps.getErasedJvmType(tpe)
        // Evaluating the 'base'
        compileExpr(exp1)
        // Cast the object to Array
        mv.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
        // Evaluating the 'index' to load from
        compileExpr(exp2)
        // Loads the 'element' at the given 'index' from the 'array'
        // with the load instruction corresponding to the loaded element
        mv.visitInsn(AsmOps.getArrayLoadInstruction(jvmType))

      case AtomicOp.ArrayStore => exps match {
        case List(exp1, exp2, exp3) =>
          // Add source line number for debugging (can fail with ???)
          addSourceLine(mv, loc)

          // We get the jvmType of the element to be stored
          val jvmType = JvmOps.getErasedJvmType(exp3.tpe)
          // Evaluating the 'base'
          compileExpr(exp1)
          // Cast the object to Array
          mv.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
          // Evaluating the 'index' to be stored in
          compileExpr(exp2)
          // Evaluating the 'element' to be stored
          compileExpr(exp3)
          // Stores the 'element' at the given 'index' in the 'array'
          // with the store instruction corresponding to the stored element
          mv.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
          // Since the return type is 'unit', we put an instance of 'unit' on top of the stack
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)
        case _ => throw InternalCompilerException("Mismatched Arity", loc)
      }

      case AtomicOp.ArrayLength => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp) = exps
        pushExpr(exp) ~
          ARRAYLENGTH()
      })

      case AtomicOp.StructNew(_, _) => mv.visitByteIns({
        import BytecodeInstructions.*

        val region :: fieldExps = exps
        val MonoType.Struct(sym, targs) = tpe
        val structType = BackendObjType.Struct(JvmOps.instantiateStruct(sym, targs))

        // Evaluate the region and ignore its value
        pushExpr(region) ~
          xPop(BackendType.toBackendType(region.tpe)) ~
          NEW(structType.jvmName) ~
          DUP() ~
          composeN(fieldExps.map(pushExpr)) ~
          INVOKESPECIAL(structType.Constructor)
      })

      case AtomicOp.StructGet(field) => mv.visitByteIns({
        import BytecodeInstructions.*

        val List(exp) = exps
        val idx = root.structs(field.structSym).fields.indexWhere(_.sym == field)
        val MonoType.Struct(sym, targs) = exp.tpe
        val structType = BackendObjType.Struct(JvmOps.instantiateStruct(sym, targs))

        pushExpr(exp) ~
          GETFIELD(structType.IndexField(idx))
      })

      case AtomicOp.StructPut(field) => mv.visitByteIns({
        import BytecodeInstructions.*

        val List(exp1, exp2) = exps
        val idx = root.structs(field.structSym).fields.indexWhere(_.sym == field)
        val MonoType.Struct(sym, targs) = exp1.tpe
        val structType = BackendObjType.Struct(JvmOps.instantiateStruct(sym, targs))

        pushExpr(exp1) ~
          pushExpr(exp2) ~
          PUTFIELD(structType.IndexField(idx)) ~
          GETSTATIC(BackendObjType.Unit.SingletonField)
      })

      case AtomicOp.InstanceOf(clazz) =>
        val List(exp) = exps
        val className = asm.Type.getInternalName(clazz)
        compileExpr(exp)
        mv.visitTypeInsn(INSTANCEOF, className)

      case AtomicOp.Cast =>
        val List(exp) = exps
        compileExpr(exp)
        mv.visitByteIns(BytecodeInstructions.castIfNotPrim(BackendType.toBackendType(tpe)))

      case AtomicOp.Unbox => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp) = exps
        pushExpr(exp) ~
          CHECKCAST(BackendObjType.Value.jvmName) ~
          GETFIELD(BackendObjType.Value.fieldFromType(BackendType.asErasedBackendType(tpe)))
      })

      case AtomicOp.Box => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp) = exps
        val erasedExpTpe = BackendType.toErasedBackendType(exp.tpe)
        val valueField = BackendObjType.Value.fieldFromType(erasedExpTpe)
        pushExpr(exp) ~
          NEW(BackendObjType.Value.jvmName) ~
          DUP() ~
          INVOKESPECIAL(BackendObjType.Value.Constructor) ~
          DUP() ~
          xSwap(lowerLarge = erasedExpTpe.is64BitWidth, higherLarge = true) ~ // two objects on top of the stack
          PUTFIELD(valueField)
      })

      case AtomicOp.InvokeConstructor(constructor) =>
        // Add source line number for debugging (can fail when calling unsafe java methods)
        addSourceLine(mv, loc)
        val descriptor = asm.Type.getConstructorDescriptor(constructor)
        val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
        // Create a new object of the declaration type
        mv.visitTypeInsn(NEW, declaration)
        // Duplicate the reference since the first argument for a constructor call is the reference to the object
        mv.visitInsn(DUP)
        // Retrieve the signature.
        val signature = constructor.getParameterTypes

        pushArgs(exps, signature)

        // Call the constructor
        mv.visitMethodInsn(INVOKESPECIAL, declaration, "<init>", descriptor, false)

      case AtomicOp.InvokeMethod(method) =>
        val exp :: args = exps

        // Add source line number for debugging (can fail when calling unsafe java methods)
        addSourceLine(mv, loc)

        // Evaluate the receiver object.
        compileExpr(exp)
        val thisType = asm.Type.getInternalName(method.getDeclaringClass)
        mv.visitTypeInsn(CHECKCAST, thisType)

        // Retrieve the signature.
        val signature = method.getParameterTypes

        pushArgs(args, signature)

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
        addSourceLine(mv, loc)
        val signature = method.getParameterTypes
        pushArgs(exps, signature)
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
        addSourceLine(mv, loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETFIELD, declaration, field.getName, BackendType.toBackendType(tpe).toDescriptor)

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        compileExpr(exp1)
        compileExpr(exp2)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTFIELD, declaration, field.getName, BackendType.toBackendType(exp2.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.GetStaticField(field) =>
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETSTATIC, declaration, field.getName, BackendType.toBackendType(tpe).toDescriptor)

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTSTATIC, declaration, field.getName, BackendType.toBackendType(exp.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.Throw =>
        // Add source line number for debugging (can fail when handling exception)
        addSourceLine(mv, loc)
        val List(exp) = exps
        compileExpr(exp)
        mv.visitInsn(ATHROW)

      case AtomicOp.Spawn => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp1, exp2) = exps
        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.ApplyAtomic(AtomicOp.Region, _, _, _, _) =>
            addLoc(loc) ~
              pushExpr(exp1) ~
              CHECKCAST(BackendObjType.Runnable.jvmName) ~
              INVOKESTATIC(ClassMaker.StaticMethod(
                JvmName.Thread,
                "startVirtualThread",
                MethodDescriptor.mkDescriptor(BackendObjType.Runnable.toTpe)(BackendObjType.Thread.toTpe)
              )) ~
              POP() ~
              GETSTATIC(BackendObjType.Unit.SingletonField)
          case _ =>
            addLoc(loc) ~
              pushExpr(exp2) ~
              CHECKCAST(BackendObjType.Region.jvmName) ~
              pushExpr(exp1) ~
              CHECKCAST(BackendObjType.Runnable.jvmName) ~
              INVOKEVIRTUAL(BackendObjType.Region.SpawnMethod) ~
              GETSTATIC(BackendObjType.Unit.SingletonField)
        }
      })

      case AtomicOp.Lazy => mv.visitByteIns({
        import BytecodeInstructions.*
        val List(exp) = exps

        // Find the Lazy class name (Lazy$tpe).
        val MonoType.Lazy(elmType) = tpe
        val lazyType = BackendObjType.Lazy(BackendType.asErasedBackendType(elmType))

        NEW(lazyType.jvmName) ~
          DUP() ~
          pushExpr(exp) ~
          INVOKESPECIAL(lazyType.Constructor)
      })

      case AtomicOp.Force =>
        val List(exp) = exps

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val MonoType.Lazy(elmType) = exp.tpe
        val erasedElmType = BackendType.asErasedBackendType(elmType)
        val lazyType = BackendObjType.Lazy(erasedElmType)

        // Emit code for the lazy expression.
        compileExpr(exp)

        val ins = {
          import BytecodeInstructions.*
          CHECKCAST(lazyType.jvmName) ~
            DUP() ~ GETFIELD(lazyType.ExpField) ~
            ifConditionElse(Condition.NONNULL)(
              INVOKEVIRTUAL(lazyType.ForceMethod)
            )(
              GETFIELD(lazyType.ValueField)
            )
        }
        mv.visitByteIns(ins)

      case AtomicOp.HoleError(sym) => mv.visitByteIns({
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design).
        addLoc(loc) ~
          pushLoc(loc) ~                                        // Loc
          NEW(BackendObjType.HoleError.jvmName) ~               // Loc, HoleError
          DUP2() ~                                              // Loc, HoleError, Loc, HoleError
          SWAP() ~                                              // Loc, HoleError, HoleError, Loc
          pushString(sym.toString) ~                            // Loc, HoleError, HoleError, Loc, Sym
          SWAP() ~                                              // Loc, HoleError, HoleError, Sym, Loc
          INVOKESPECIAL(BackendObjType.HoleError.Constructor) ~ // Loc, HoleError
          ATHROW()
      })

      case AtomicOp.MatchError => mv.visitByteIns({
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design)
        addLoc(loc) ~
          pushLoc(loc) ~                                         // Loc
          NEW(BackendObjType.MatchError.jvmName) ~               // Loc, MatchError
          DUP2() ~                                               // Loc, MatchError, Loc, MatchError
          SWAP() ~                                               // Loc, MatchError, MatchError, Loc
          INVOKESPECIAL(BackendObjType.MatchError.Constructor) ~ // Loc, MatchError
          ATHROW()
      })

      case AtomicOp.CastError(from, to) => mv.visitByteIns({
        import BytecodeInstructions.*
        // Add source line number for debugging (failable by design)
        addLoc(loc) ~
          NEW(BackendObjType.CastError.jvmName) ~
          DUP() ~
          pushLoc(loc) ~
          pushString(s"Cannot cast from type '$from' to '$to'") ~
          INVOKESPECIAL(BackendObjType.CastError.Constructor) ~
          ATHROW()
      })
    }

    case Expr.ApplyClo(exp1, exp2, ct, _, purity, loc) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(exp1.tpe).jvmName
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp1.tpe)
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
          mv.visitFieldInsn(PUTFIELD, functionInterface.toInternalName,
            "arg0", JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
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
          mv.visitFieldInsn(PUTFIELD, functionInterface.toInternalName,
            "arg0", JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
          // Calling unwind and unboxing

          if (Purity.isControlPure(purity)) {
            mv.visitByteIns(BackendObjType.Result.unwindSuspensionFreeThunk("in pure closure call", loc))
          } else {
            ctx match {
              case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
                val pcPoint = pcCounter(0) + 1
                val pcPointLabel = pcLabels(pcPoint)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                mv.visitByteIns(BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc))
                mv.visitJumpInsn(GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)

                mv.visitVarInsn(ALOAD, 1)

                mv.visitLabel(afterUnboxing)

              case DirectContext(_, _, _) =>
                throw InternalCompilerException("Unexpected direct method context in control impure function", loc)
            }
          }
      }

    case Expr.ApplyDef(sym, exps, ct, _, purity, loc) => ct match {
      case ExpPosition.Tail =>
        // Type of the function abstract class
        val functionInterface = JvmOps.getFunctionInterfaceType(root.defs(sym).arrowType).jvmName

        // Put the def on the stack
        AsmOps.compileDefSymbol(sym, mv)
        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, functionInterface.toInternalName,
            s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
        }
        // Return the def
        mv.visitInsn(ARETURN)

      case ExpPosition.NonTail =>
        // JvmType of Def
        val defJvmName = JvmOps.getFunctionDefinitionClassName(sym)

        // Put the def on the stack
        AsmOps.compileDefSymbol(sym, mv)

        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, defJvmName.toInternalName,
            s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
        }
        // Calling unwind and unboxing

        if (Purity.isControlPure(purity)) {
          mv.visitByteIns(BackendObjType.Result.unwindSuspensionFreeThunk("in pure function call", loc))
        } else {
          ctx match {
            case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
              val pcPoint = pcCounter(0) + 1
              val pcPointLabel = pcLabels(pcPoint)
              val afterUnboxing = new Label()
              pcCounter(0) += 1
              mv.visitByteIns(BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc))
              mv.visitJumpInsn(GOTO, afterUnboxing)

              mv.visitLabel(pcPointLabel)
              mv.visitVarInsn(ALOAD, 1)

              mv.visitLabel(afterUnboxing)

            case DirectContext(_, _, _) =>
              throw InternalCompilerException("Unexpected direct method context in control impure function", loc)
          }
        }
    }

    case Expr.ApplySelfTail(sym, exps, _, _, _) =>
      // The function abstract class name
      val functionInterface = JvmOps.getFunctionInterfaceType(root.defs(sym).arrowType).jvmName
      // Evaluate each argument and put the result on the Fn class.
      for ((arg, i) <- exps.zipWithIndex) {
        mv.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpr(arg)
        mv.visitFieldInsn(PUTFIELD, functionInterface.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      ctx match {
        case EffectContext(_, _, _, setPc, _, _, _) =>
          mv.visitVarInsn(ALOAD, 0)
          compileInt(0)
          mv.visitByteIns(setPc)

        case DirectContext(_, _, _) =>
          () // Do nothing
      }
      // Jump to the entry point of the method.
      mv.visitJumpInsn(GOTO, ctx.entryPoint)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpr(exp1)
      mv.visitJumpInsn(IFEQ, ifElse)
      compileExpr(exp2)
      mv.visitJumpInsn(GOTO, ifEnd)
      mv.visitLabel(ifElse)
      compileExpr(exp3)
      mv.visitLabel(ifEnd)

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

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      compileExpr(exp1)
      val bType = BackendType.toBackendType(exp1.tpe)
      mv.visitByteIns(BytecodeInstructions.castIfNotPrim(bType))
      mv.visitByteIns(BytecodeInstructions.xStore(bType, sym.getStackOffset(ctx.localOffset)))
      compileExpr(exp2)

    case Expr.Stmt(exp1, exp2, _, _, _) =>
      compileExpr(exp1)
      mv.visitByteIns(BytecodeInstructions.xPop(BackendType.toErasedBackendType(exp1.tpe)))
      compileExpr(exp2)

    case Expr.Scope(sym, exp, _, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)

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
      mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.Region.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(), JvmType.Void), false)

      val iStore = AsmOps.getStoreInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      mv.visitVarInsn(iStore, sym.getStackOffset(ctx.localOffset))

      // Compile the scope body
      mv.visitLabel(beforeTryBlock)
      compileExpr(exp)

      // Emit try finally block. It's important to do this after compiling sub-expressions to ensure
      // correct catch case ordering.
      mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

      // When we exit the scope, call the region's `exit` method
      val iLoad = AsmOps.getLoadInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      mv.visitVarInsn(iLoad, sym.getStackOffset(ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ExitMethod.name,
        BackendObjType.Region.ExitMethod.d.toDescriptor, false)
      mv.visitLabel(afterTryBlock)

      // Compile the finally block which gets called if no exception is thrown
      mv.visitVarInsn(iLoad, sym.getStackOffset(ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitJumpInsn(GOTO, afterFinally)

      // Compile the finally block which gets called if an exception is thrown
      mv.visitLabel(finallyBlock)
      mv.visitVarInsn(iLoad, sym.getStackOffset(ctx.localOffset))
      mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitInsn(ATHROW)
      mv.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      // Add source line number for debugging.
      addSourceLine(mv, loc)

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
      for ((CatchRule(sym, _, body), handlerLabel) <- rulesAndLabels) {
        // Emit the label.
        mv.visitLabel(handlerLabel)

        // Store the exception in a local variable.
        val istore = AsmOps.getStoreInstruction(JvmType.Object)
        mv.visitVarInsn(istore, sym.getStackOffset(ctx.localOffset))

        // Emit code for the handler body expression.
        compileExpr(body)
        mv.visitJumpInsn(GOTO, afterTryAndCatch)
      }

      // Emit a try catch block for each catch rule. It's important to do this after compiling
      // sub-expressions to ensure correct catch case ordering.
      for ((CatchRule(_, clazz, _), handlerLabel) <- rulesAndLabels) {
        mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
      }

      // Add the label after both the try and catch rules.
      mv.visitLabel(afterTryAndCatch)

    case Expr.RunWith(exp, effUse, rules, ct, _, _, loc) =>
      // exp is a Unit -> exp.tpe closure
      val effectJvmName = JvmOps.getEffectDefinitionClassName(effUse.sym)
      val ins = {
        import BytecodeInstructions.*
        // eff name
        pushString(effUse.sym.toString) ~
          // handler
          NEW(effectJvmName) ~ DUP() ~ cheat(_.visitMethodInsn(Opcodes.INVOKESPECIAL, effectJvmName.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)) ~
          // bind handler closures
          cheat(mv => rules.foreach {
            case HandlerRule(op, _, body) =>
              mv.visitInsn(Opcodes.DUP)
              compileExpr(body)(mv, ctx, root, flix)
              mv.visitFieldInsn(Opcodes.PUTFIELD, effectJvmName.toInternalName, JvmOps.getEffectOpName(op.sym), GenEffectClasses.opFieldType(op.sym).toDescriptor)
          }) ~
          // frames
          NEW(BackendObjType.FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.FramesNil.Constructor) ~
          // continuation
          pushExpr(exp) ~
          // exp.arg0 should be set to unit here but from lifting we know that it is unused so the
          // implicit null is fine.
          // call installHandler
          INVOKESTATIC(BackendObjType.Handler.InstallHandlerMethod)
      }
      mv.visitByteIns(ins)
      // handle value/suspend/thunk if in non-tail position
      if (ct == ExpPosition.NonTail) { ctx match {
        case DirectContext(_, _, _) =>
          mv.visitByteIns(BackendObjType.Result.unwindSuspensionFreeThunk("in pure run-with call", loc))

        case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
          val pcPoint = pcCounter(0) + 1
          val pcPointLabel = pcLabels(pcPoint)
          val afterUnboxing = new Label()
          pcCounter(0) += 1
          mv.visitByteIns(BackendObjType.Result.unwindThunkToValue(pcPoint, newFrame, setPc))
          mv.visitJumpInsn(GOTO, afterUnboxing)

          mv.visitLabel(pcPointLabel)
          mv.visitVarInsn(ALOAD, 1)
          mv.visitLabel(afterUnboxing)
      }} else {
        mv.visitInsn(ARETURN)
      }

    case Expr.Do(op, exps, tpe, _, loc) => ctx match {
      case DirectContext(_, _, _) =>
        throw InternalCompilerException("Unexpected do-expression in direct method context", loc)

      case EffectContext(_, _, newFrame, setPc, _, pcLabels, pcCounter) =>
        val pcPoint = pcCounter(0) + 1
        val pcPointLabel = pcLabels(pcPoint)
        val afterUnboxing = new Label()
        val erasedResult = BackendType.toErasedBackendType(tpe)

        pcCounter(0) += 1
        val ins: InstructionSet = {
          import BackendObjType.Suspension
          import BytecodeInstructions.*
          val effectName = JvmOps.getEffectDefinitionClassName(op.sym.eff)
          val effectStaticMethod = ClassMaker.StaticMethod(
            effectName,
            JvmOps.getEffectOpName(op.sym),
          GenEffectClasses.opStaticFunctionDescriptor(op.sym)
          )
          NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~
            DUP() ~ pushString(op.sym.eff.toString) ~ PUTFIELD(Suspension.EffSymField) ~
            DUP() ~
            // --- eff op ---
            composeN(exps.map(pushExpr)) ~
            mkStaticLambda(BackendObjType.EffectCall.ApplyMethod, effectStaticMethod, 2) ~
            // --------------
            PUTFIELD(Suspension.EffOpField) ~
            DUP() ~
            // create continuation
            NEW(BackendObjType.FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.FramesNil.Constructor) ~
            newFrame ~ DUP() ~ pushInt(pcPoint) ~ setPc ~
            INVOKEVIRTUAL(BackendObjType.FramesNil.PushMethod) ~
            // store continuation
            PUTFIELD(Suspension.PrefixField) ~
            DUP() ~ NEW(BackendObjType.ResumptionNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.ResumptionNil.Constructor) ~ PUTFIELD(Suspension.ResumptionField) ~
            xReturn(Suspension.toTpe)
        }
        mv.visitByteIns(ins)

        mv.visitLabel(pcPointLabel)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitByteIns(BytecodeInstructions.GETFIELD(BackendObjType.Value.fieldFromType(erasedResult)))

        mv.visitLabel(afterUnboxing)
        mv.visitByteIns(BytecodeInstructions.castIfNotPrim(BackendType.toBackendType(tpe)))
    }

    case Expr.NewObject(name, _, _, _, methods, _) =>
      val exps = methods.map(_.exp)
      val className = JvmName(ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage, name).toInternalName
      mv.visitTypeInsn(NEW, className)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, className, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

      // For each method, compile the closure which implements the body of that method and store it in a field
      exps.zipWithIndex.foreach { case (e, i) =>
        mv.visitInsn(DUP)
        compileExpr(e)
        mv.visitFieldInsn(PUTFIELD, className, s"clo$i", JvmOps.getClosureAbstractClassType(e.tpe).toDescriptor)
      }

  }

  /** `[] --> [exp]` */
  private def pushExpr(exp: Expr)(implicit ctx: MethodContext, root: Root, flix: Flix): InstructionSet =
    BytecodeInstructions.cheat(mv => compileExpr(exp)(mv, ctx, root, flix))

  private def compileIsTag(name: String, exp: Expr, tpes: List[BackendType])(implicit ctx: MethodContext, root: Root, flix: Flix): InstructionSet = {
    import BytecodeInstructions.*
    tpes match {
      case Nil =>
        pushExpr(exp) ~
          INSTANCEOF(BackendObjType.NullaryTag(name).jvmName)
      case _ =>
        pushExpr(exp) ~
          CHECKCAST(BackendObjType.Tagged.jvmName) ~ GETFIELD(BackendObjType.Tagged.NameField) ~
          BackendObjType.Tagged.mkTagName(name) ~ BackendObjType.Tagged.eqTagName()
    }
  }

  private def compileTag(name: String, exps: List[Expr], tpes: List[BackendType])(implicit ctx: MethodContext, root: Root, flix: Flix): InstructionSet = {
    import BytecodeInstructions.*
    tpes match {
      case Nil =>
        GETSTATIC(BackendObjType.NullaryTag(name).SingletonField)
      case _ =>
        val tagType = BackendObjType.Tag(tpes)
        NEW(tagType.jvmName) ~ DUP() ~ INVOKESPECIAL(tagType.Constructor) ~
          DUP() ~ BackendObjType.Tagged.mkTagName(name) ~ PUTFIELD(tagType.NameField) ~
          composeN(exps.zipWithIndex.map {
            case (e, i) => DUP() ~ pushExpr(e) ~ PUTFIELD(tagType.IndexField(i))
          })
    }
  }

  private def compileUntag(exp: Expr, idx: Int, tpes: List[BackendType])(implicit ctx: MethodContext, root: Root, flix: Flix): InstructionSet = {
    import BytecodeInstructions.*
    // BackendObjType.NullaryTag cannot happen here since terms must be non-empty.
    if (tpes.isEmpty) throw InternalCompilerException(s"Unexpected empty tag types", exp.loc)
    val tagType = BackendObjType.Tag(tpes)
    pushExpr(exp) ~
      CHECKCAST(tagType.jvmName) ~
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
    * Generate code to load an integer constant.
    *
    * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
    * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
    * bytes.
    */
  def compileInt(i: Int)(implicit mv: MethodVisitor): Unit = i match {
    case -1 => mv.visitInsn(ICONST_M1)
    case 0 => mv.visitInsn(ICONST_0)
    case 1 => mv.visitInsn(ICONST_1)
    case 2 => mv.visitInsn(ICONST_2)
    case 3 => mv.visitInsn(ICONST_3)
    case 4 => mv.visitInsn(ICONST_4)
    case 5 => mv.visitInsn(ICONST_5)
    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => mv.visitIntInsn(BIPUSH, i)
    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => mv.visitIntInsn(SIPUSH, i)
    case _ => mv.visitLdcInsn(i)
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

  /**
    * Adds the source of the line for debugging
    */
  private def addSourceLine(visitor: MethodVisitor, loc: SourceLocation): Unit = {
    val label = new Label()
    visitor.visitLabel(label)
    visitor.visitLineNumber(loc.beginLine, label)
  }

  /**
    * Pushes arguments onto the stack ready to invoke a method
    */
  private def pushArgs(args: List[Expr], signature: Array[Class[? <: Object]])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    // Evaluate arguments left-to-right and push them onto the stack.
    for ((arg, argType) <- args.zip(signature)) {
      compileExpr(arg)
      if (!argType.isPrimitive) mv.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
    }
  }
}
