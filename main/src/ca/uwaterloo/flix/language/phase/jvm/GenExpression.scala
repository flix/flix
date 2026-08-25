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
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenCastError, GenEffectCall, GenExtTag, GenExtTagged, GenFrames, GenFramesNil, GenHandler, GenHoleError, GenMatchError, GenNullaryTag, GenRecord, GenRecordEmpty, GenRecordExtend, GenRegion, GenResult, GenResumption, GenResumptionNil, GenSuspension, GenTag, GenTagged, GenThunk, GenUnit, GenValue}
import ca.uwaterloo.flix.util.ClassDescs.internalNameOf
import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.constant.ConstantDescs.{CD_double, CD_int, CD_long, CD_void}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListOps
import org.objectweb.asm
import org.objectweb.asm.*

import scala.jdk.CollectionConverters.*

/**
  * Generate expression
  */
object GenExpression {

  type Ref[T] = Array[T]

  sealed trait MethodContext {

    def entryPoint: Label

    def lenv: Map[Symbol.LabelSym, Label]

    def localOffset: Int

    /** Returns the absolute index of the local variable `varOffset` by adding this context's local offset. */
    def getIndex(varOffset: Int): Int = varOffset + localOffset

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
  case class EffectContext(
    entryPoint: Label,
    lenv: Map[Symbol.LabelSym, Label],
    newFrame: MethodVisitor => Unit, // [...] -> [..., frame]
    setPc: MethodVisitor => Unit, // [..., frame, pc] -> [...]
    narrowLocals: MethodVisitor => Unit, // re-cast locals to their declared types after resume
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
  case class DirectInstanceContext(
    entryPoint: Label,
    lenv: Map[Symbol.LabelSym, Label],
    localOffset: Int,
  ) extends MethodContext

  /**
    * A context for control pure functions that do not closure capture any variables and therefore
    * never use any fields to store arguments.
    * Such functions never need to record their state and will always
    * return at the given return expressions except if they loop indefinitely.
    */
  case class DirectStaticContext(
    entryPoint: Label,
    lenv: Map[Symbol.LabelSym, Label],
    localOffset: Int,
  ) extends MethodContext

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpr(exp0: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = exp0 match {
    case Expr.Cst(cst, loc) => cst match {
      case Constant.Unit =>
        GETSTATIC(GenUnit.SingletonField)

      case Constant.Null =>
        ACONST_NULL()

      case Constant.Bool(b) =>
        pushBool(b)

      case Constant.Char(c) =>
        pushInt(c)

      case Constant.Float32(f) =>
        f match {
          case 0f => mv.visitInsn(Opcodes.FCONST_0)
          case 1f => mv.visitInsn(Opcodes.FCONST_1)
          case 2f => mv.visitInsn(Opcodes.FCONST_2)
          case _ => mv.visitLdcInsn(f)
        }

      case Constant.Float64(d) =>
        d match {
          case 0d => mv.visitInsn(Opcodes.DCONST_0)
          case 1d => mv.visitInsn(Opcodes.DCONST_1)
          case _ => mv.visitLdcInsn(d)
        }

      case Constant.BigDecimal(dd) =>
        // Can fail with NumberFormatException
        addLoc(loc)
        NEW(JavaClasses.BigDecimal)
        DUP()
        pushString(dd.toString)
        INVOKESPECIAL(ClassConstants.BigDecimal.Constructor)

      case Constant.Int8(b) =>
        pushInt(b)

      case Constant.Int16(s) =>
        pushInt(s)

      case Constant.Int32(i) =>
        pushInt(i)

      case Constant.Int64(l) =>
        compileLong(l)

      case Constant.BigInt(ii) =>
        // Add source line number for debugging (can fail with NumberFormatException)
        addLoc(loc)
        NEW(JavaClasses.BigInteger)
        DUP()
        pushString(ii.toString)
        INVOKESPECIAL(ClassConstants.BigInteger.Constructor)

      case Constant.Str(s) =>
        pushString(s)

      case Constant.Regex(patt) =>
        // Add source line number for debugging (can fail with PatternSyntaxException)
        addLoc(loc)
        pushString(patt.pattern)
        INVOKESTATIC(ClassConstants.Regex.CompileMethod)

      case Constant.RecordEmpty =>
        GETSTATIC(GenRecordEmpty.SingletonField)

      case Constant.Static =>
        //!TODO: For now, just emit null
        ACONST_NULL()
        CHECKCAST(GenRegion.desc)

    }

    case Expr.Var(_, offset, tpe, _) =>
      xLoad(TypeDescs.toClassDesc(tpe), ctx.getIndex(offset))

    case Expr.ApplyAtomic(op, exps, tpe, _, loc) => op match {

      case AtomicOp.Closure(sym) =>
        // JvmType of the closure
        val closureName = internalNameOf(GenFunAndClosureClasses.closureDesc(sym))
        // new closure instance
        mv.visitTypeInsn(Opcodes.NEW, closureName)
        // Duplicate
        mv.visitInsn(Opcodes.DUP)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, closureName, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), false)
        // Capturing free args
        for ((arg, i) <- exps.zipWithIndex) {
          val argType = TypeDescs.toClassDesc(arg.tpe)
          mv.visitInsn(Opcodes.DUP)
          compileExpr(arg)
          castIfNotPrim(argType)
          mv.visitFieldInsn(Opcodes.PUTFIELD, closureName, s"clo$i", argType.descriptorString())
        }

      case AtomicOp.Unary(sop) =>
        val List(exp) = exps
        compileExpr(exp)

        sop match {
          case SemanticOp.BoolOp.Not =>
            mv.visitInsn(Opcodes.ICONST_1)
            mv.visitInsn(Opcodes.IXOR)

          case Float32Op.Neg => mv.visitInsn(Opcodes.FNEG)

          case Float64Op.Neg => mv.visitInsn(Opcodes.DNEG)

          case Int8Op.Neg =>
            mv.visitInsn(Opcodes.INEG)
            mv.visitInsn(Opcodes.I2B) // Sign extend so sign bit is also changed

          case Int16Op.Neg =>
            mv.visitInsn(Opcodes.INEG)
            mv.visitInsn(Opcodes.I2S) // Sign extend so sign bit is also changed

          case Int32Op.Neg => mv.visitInsn(Opcodes.INEG)

          case Int64Op.Neg => mv.visitInsn(Opcodes.LNEG)

          case Int8Op.Not | Int16Op.Not | Int32Op.Not =>
            mv.visitInsn(Opcodes.ICONST_M1)
            mv.visitInsn(Opcodes.IXOR)

          case Int64Op.Not =>
            mv.visitInsn(Opcodes.ICONST_M1)
            mv.visitInsn(Opcodes.I2L)
            mv.visitInsn(Opcodes.LXOR)

          case _: ReflectOp =>
            throw InternalCompilerException("ReflectOp should have been resolved in Specialization", loc)

          case ObjectOp.Ordinal =>
            mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(GenTagged.desc))
            mv.visitFieldInsn(Opcodes.GETFIELD, internalNameOf(GenTagged.desc), "ordinal", CD_int.descriptorString())
        }

      case AtomicOp.Binary(sop) =>
        val List(exp1, exp2) = exps
        sop match {
          case BoolOp.And =>
            val andEnd = new Label()
            compileExpr(exp1)
            mv.visitInsn(Opcodes.DUP)
            mv.visitJumpInsn(Opcodes.IFEQ, andEnd)
            mv.visitInsn(Opcodes.POP)
            compileExpr(exp2)
            mv.visitLabel(andEnd)

          case BoolOp.Or =>
            val orEnd = new Label()
            compileExpr(exp1)
            mv.visitInsn(Opcodes.DUP)
            mv.visitJumpInsn(Opcodes.IFNE, orEnd)
            mv.visitInsn(Opcodes.POP)
            compileExpr(exp2)
            mv.visitLabel(orEnd)

          case Float32Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(Opcodes.F2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(Opcodes.F2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)
            mv.visitInsn(Opcodes.D2F) // Convert double to float

          case Float64Op.Exp =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)

          case Int8Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)
            mv.visitInsn(Opcodes.D2I) // Convert to int
            mv.visitInsn(Opcodes.I2B) // Convert int to byte

          case Int16Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)
            mv.visitInsn(Opcodes.D2I) // Convert to int
            mv.visitInsn(Opcodes.I2S) // Convert int to short

          case Int32Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(Opcodes.I2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)
            mv.visitInsn(Opcodes.D2I) // Convert to int

          case Int64Op.Exp =>
            compileExpr(exp1)
            mv.visitInsn(Opcodes.L2D) // Convert to double since "pow" is only defined for doubles
            compileExpr(exp2)
            mv.visitInsn(Opcodes.L2D) // Convert to double since "pow" is only defined for doubles
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalNameOf(JavaClasses.Math), "pow",
              mkDescriptor(CD_double, CD_double)(CD_double).descriptorString(), false)
            mv.visitInsn(Opcodes.D2L) // Convert to long

          case Int8Op.And | Int16Op.And | Int32Op.And =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IAND)

          case Int8Op.Or | Int16Op.Or | Int32Op.Or =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IOR)

          case Int8Op.Xor | Int16Op.Xor | Int32Op.Xor =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IXOR)

          case Int8Op.Shr | Int16Op.Shr | Int32Op.Shr =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISHR)

          case Int8Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISHL)
            mv.visitInsn(Opcodes.I2B) // Sign extend to make left most bit appear in the sign bit

          case Int16Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISHL)
            mv.visitInsn(Opcodes.I2S) // Sign extend to make left most bit appear in the sign bit

          case Int32Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISHL)

          case Int64Op.And =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LAND)

          case Int64Op.Or =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LOR)

          case Int64Op.Xor =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LXOR)

          case Int64Op.Shr =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LSHR)

          case Int64Op.Shl =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LSHL)

          case Float32Op.Lt => visitComparison2(exp1, exp2, Opcodes.FCMPG, Opcodes.IFGE)

          case Float32Op.Le => visitComparison2(exp1, exp2, Opcodes.FCMPG, Opcodes.IFGT)

          case Float32Op.Eq => visitComparison2(exp1, exp2, Opcodes.FCMPG, Opcodes.IFNE)

          case Float32Op.Neq => visitComparison2(exp1, exp2, Opcodes.FCMPG, Opcodes.IFEQ)

          case Float32Op.Ge => visitComparison2(exp1, exp2, Opcodes.FCMPL, Opcodes.IFLT)

          case Float32Op.Gt => visitComparison2(exp1, exp2, Opcodes.FCMPL, Opcodes.IFLE)

          case Float64Op.Lt => visitComparison2(exp1, exp2, Opcodes.DCMPG, Opcodes.IFGE)

          case Float64Op.Le => visitComparison2(exp1, exp2, Opcodes.DCMPG, Opcodes.IFGT)

          case Float64Op.Eq => visitComparison2(exp1, exp2, Opcodes.DCMPG, Opcodes.IFNE)

          case Float64Op.Neq => visitComparison2(exp1, exp2, Opcodes.DCMPG, Opcodes.IFEQ)

          case Float64Op.Ge => visitComparison2(exp1, exp2, Opcodes.DCMPL, Opcodes.IFLT)

          case Float64Op.Gt => visitComparison2(exp1, exp2, Opcodes.DCMPL, Opcodes.IFLE)

          case Int8Op.Lt | Int16Op.Lt | Int32Op.Lt | CharOp.Lt =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPGE)

          case Int8Op.Le | Int16Op.Le | Int32Op.Le | CharOp.Le =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPGT)

          case Int8Op.Eq | Int16Op.Eq | Int32Op.Eq | CharOp.Eq | BoolOp.Eq =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPNE)

          case Int8Op.Neq | Int16Op.Neq | Int32Op.Neq | CharOp.Neq | BoolOp.Neq =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPEQ)

          case Int8Op.Ge | Int16Op.Ge | Int32Op.Ge | CharOp.Ge =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPLT)

          case Int8Op.Gt | Int16Op.Gt | Int32Op.Gt | CharOp.Gt =>
            visitComparison1(exp1, exp2, Opcodes.IF_ICMPLE)

          case Int64Op.Lt => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFGE)

          case Int64Op.Le => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFGT)

          case Int64Op.Eq => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFNE)

          case Int64Op.Neq => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFEQ)

          case Int64Op.Ge => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFLT)

          case Int64Op.Gt => visitComparison2(exp1, exp2, Opcodes.LCMP, Opcodes.IFLE)

          case Float32Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.FADD)

          case Float32Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.FSUB)

          case Float32Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.FMUL)

          case Float32Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.FDIV)

          case Float64Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.DADD)

          case Float64Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.DSUB)

          case Float64Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.DMUL)

          case Float64Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.DDIV)

          case Int8Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IADD)
            mv.visitInsn(Opcodes.I2B) // Sign extend after operation

          case Int8Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISUB)
            mv.visitInsn(Opcodes.I2B) // Sign extend after operation

          case Int8Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IMUL)
            mv.visitInsn(Opcodes.I2B) // Sign extend after operation

          case Int8Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IDIV)
            mv.visitInsn(Opcodes.I2B) // Sign extend after operation

          case Int8Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IREM)
            mv.visitInsn(Opcodes.I2B) // Sign extend after operation

          case Int16Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IADD)
            mv.visitInsn(Opcodes.I2S) // Sign extend after operation

          case Int16Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISUB)
            mv.visitInsn(Opcodes.I2S) // Sign extend after operation

          case Int16Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IMUL)
            mv.visitInsn(Opcodes.I2S) // Sign extend after operation

          case Int16Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IDIV)
            mv.visitInsn(Opcodes.I2S) // Sign extend after operation

          case Int16Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IREM)
            mv.visitInsn(Opcodes.I2S) // Sign extend after operation

          case Int32Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IADD)

          case Int32Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.ISUB)

          case Int32Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IMUL)

          case Int32Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IDIV)

          case Int32Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.IREM)

          case Int64Op.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LADD)

          case Int64Op.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LSUB)

          case Int64Op.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LMUL)

          case Int64Op.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LDIV)

          case Int64Op.Rem =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitInsn(Opcodes.LREM)

          case StringOp.Concat =>
            throw InternalCompilerException(s"Unexpected BinaryOperator StringOp.Concat. It should have been eliminated by Simplifier", loc)

          case ObjectOp.RefEq =>
            val refEqElse = new Label()
            val refEqEnd = new Label()
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitJumpInsn(Opcodes.IF_ACMPNE, refEqElse)
            mv.visitInsn(Opcodes.ICONST_1)
            mv.visitJumpInsn(Opcodes.GOTO, refEqEnd)
            mv.visitLabel(refEqElse)
            mv.visitInsn(Opcodes.ICONST_0)
            mv.visitLabel(refEqEnd)
        }

      case AtomicOp.Is(sym) =>
        val List(exp) = exps
        compileIsTag(sym.ordinal, exp)

      case AtomicOp.Tag(sym) =>
        val caze = root.enums(sym.enumSym).cases(sym)
        val termTypes = caze.tpes.map(TypeDescs.toErasedClassDesc)
        compileTag(sym.enumSym.toString, sym.name, caze.sym.ordinal, exps, termTypes)

      case AtomicOp.Untag(sym, idx) =>
        val List(exp) = exps
        val termTypes = root.enums(sym.enumSym).cases(sym).tpes.map(TypeDescs.toErasedClassDesc)

        compileUntag(exp, idx, termTypes)
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.Index(idx) =>
        val List(exp) = exps
        val SimpleType.Tuple(elmTypes) = exp.tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(TypeDescs.toErasedClassDesc))

        compileExpr(exp)
        GETFIELD(tupleType.IndexField(idx))
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.Tuple =>
        val SimpleType.Tuple(elmTypes) = tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(TypeDescs.toErasedClassDesc))
        NEW(tupleType.desc)
        DUP()
        exps.foreach(compileExpr)
        INVOKESPECIAL(tupleType.Constructor)

      case AtomicOp.RecordSelect(field) =>
        val List(exp) = exps
        val recordValue = TypeDescs.toErasedClassDesc(tpe)

        compileExpr(exp)
        pushString(field.name)
        INVOKEINTERFACE(GenRecord.LookupFieldMethod)
        // Now that the specific RecordExtend object is found, we cast it to its exact class and extract the value.
        CHECKCAST(GenRecordExtend.desc(recordValue))
        GETFIELD(GenRecordExtend.ValueField(recordValue))
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.RecordExtend(field) =>
        val List(exp1, exp2) = exps
        val recordValue = TypeDescs.toErasedClassDesc(exp1.tpe)
        NEW(GenRecordExtend.desc(recordValue))
        DUP()
        INVOKESPECIAL(GenRecordExtend.Constructor(recordValue))
        DUP()
        pushString(field.name)
        PUTFIELD(GenRecordExtend.LabelField(recordValue))
        DUP()
        compileExpr(exp1)
        PUTFIELD(GenRecordExtend.ValueField(recordValue))
        DUP()
        compileExpr(exp2)
        PUTFIELD(GenRecordExtend.RestField(recordValue))

      case AtomicOp.RecordRestrict(field) =>
        val List(exp) = exps

        compileExpr(exp)
        pushString(field.name)
        INVOKEINTERFACE(GenRecord.RestrictFieldMethod)

      case AtomicOp.ExtIs(sym) =>
        val List(exp) = exps
        compileExtIsTag(sym.name, exp)

      case AtomicOp.ExtTag(sym) =>
        val tpes = SimpleType.findExtensibleTermTypes(sym, tpe).map(TypeDescs.toErasedClassDesc)
        compileExtTag(sym.name, exps, tpes)

      case AtomicOp.ExtUntag(sym, idx) =>

        val List(exp) = exps
        val tpes = SimpleType.findExtensibleTermTypes(sym, exp.tpe).map(TypeDescs.toErasedClassDesc)

        compileExtUntag(exp, idx, tpes)
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.ArrayLit =>
        val innerType = tpe.asInstanceOf[SimpleType.Array].tpe
        val elmTpe = TypeDescs.toClassDesc(innerType)

        pushInt(exps.length)
        xNewArray(elmTpe)
        for ((e, i) <- exps.zipWithIndex) {
          DUP()
          pushInt(i)
          compileExpr(e)
          xArrayStore(elmTpe)
        }

      case AtomicOp.ArrayNew =>
        val List(exp1, exp2) = exps
        // We get the inner type of the array
        val innerType = tpe.asInstanceOf[SimpleType.Array].tpe
        val erasedElmTpe = TypeDescs.toErasedClassDesc(innerType)
        val elmIs64BitWidth = erasedElmTpe == CD_long || erasedElmTpe == CD_double
        val fillMethod = ClassMaker.StaticMethod(JavaClasses.Arrays, "fill", MethodTypeDesc.of(CD_void, erasedElmTpe.arrayType(), erasedElmTpe))
        compileExpr(exp1) // default
        compileExpr(exp2) // default, length
        xNewArray(TypeDescs.toClassDesc(innerType)) // default, arr
        if (elmIs64BitWidth) DUP_X2() else DUP_X1() // arr, default, arr
        xSwap(lowerLarge = elmIs64BitWidth, higherLarge = false) // arr, arr, default
        INVOKESTATIC(fillMethod)

      case AtomicOp.ArrayLoad =>
        val List(exp1, exp2) = exps
        val elmTpe = TypeDescs.toClassDesc(tpe)

        // Add source line number for debugging (can fail with out of bounds).
        addLoc(loc)
        compileExpr(exp1)
        compileExpr(exp2)
        xArrayLoad(elmTpe)
        castIfNotPrim(elmTpe)

      case AtomicOp.ArrayStore =>
        val List(exp1, exp2, exp3) = exps
        val elmTpe = TypeDescs.toClassDesc(exp3.tpe)

        // Add source line number for debugging (can fail with out of bounds).
        addLoc(loc)
        compileExpr(exp1) // Evaluating the array
        castIfNotPrim(elmTpe.arrayType())
        compileExpr(exp2) // Evaluating the index
        compileExpr(exp3) // Evaluating the element
        xArrayStore(elmTpe)
        GETSTATIC(GenUnit.SingletonField)

      case AtomicOp.ArrayLength =>
        val List(exp) = exps
        compileExpr(exp)
        ARRAYLENGTH()

      case AtomicOp.StructNew(sym, mutability, _) =>
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
            xPop(TypeDescs.toClassDesc(region.tpe))
        }
        NEW(structType.desc)
        DUP()
        fieldExps.foreach(compileExpr)
        INVOKESPECIAL(structType.Constructor)

      case AtomicOp.StructGet(field) =>

        val List(exp) = exps
        val struct = root.structs(field.structSym)
        val structType = getStructType(struct)
        val idx = struct.fields.indexWhere(_.sym == field)

        compileExpr(exp)
        GETFIELD(structType.IndexField(idx))
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.StructPut(field) =>

        val List(exp1, exp2) = exps
        val struct = root.structs(field.structSym)
        val idx = struct.fields.indexWhere(_.sym == field)
        val structType = getStructType(struct)

        compileExpr(exp1)
        compileExpr(exp2)
        PUTFIELD(structType.IndexField(idx))
        GETSTATIC(GenUnit.SingletonField)

      case AtomicOp.InstanceOf(clazz) =>
        val List(exp) = exps
        compileExpr(exp)
        mv.visitTypeInsn(Opcodes.INSTANCEOF, internalNameOf(clazz))

      case AtomicOp.Cast =>
        val List(exp) = exps
        compileExpr(exp)
        castIfNotPrim(TypeDescs.toClassDesc(tpe))

      case AtomicOp.Unbox =>
        val List(exp) = exps
        val bType = TypeDescs.toClassDesc(tpe)
        compileExpr(exp)
        CHECKCAST(GenValue.desc)
        GETFIELD(GenValue.fieldFromType(bType))
        castIfNotPrim(bType)

      case AtomicOp.Box =>
        val List(exp) = exps
        exp.tpe match {
          case SimpleType.Unit =>
            compileExpr(exp)
            POP()
            GETSTATIC(GenValue.UnitField)
          case SimpleType.Bool =>
            compileExpr(exp)
            val falseLabel = new Label()
            val doneLabel = new Label()
            mv.visitJumpInsn(Opcodes.IFEQ, falseLabel)
            GETSTATIC(GenValue.TrueField)
            mv.visitJumpInsn(Opcodes.GOTO, doneLabel)
            mv.visitLabel(falseLabel)
            GETSTATIC(GenValue.FalseField)
            mv.visitLabel(doneLabel)
          case _ =>
            val erasedExpTpe = TypeDescs.toErasedClassDesc(exp.tpe)
            val valueField = GenValue.fieldFromType(erasedExpTpe)
            compileExpr(exp)
            NEW(GenValue.desc)
            DUP()
            INVOKESPECIAL(GenValue.Constructor)
            DUP()
            xSwap(lowerLarge = isCategory2(erasedExpTpe), higherLarge = true) // two objects on top of the stack
            PUTFIELD(valueField)
        }

      case AtomicOp.InvokeConstructor(constructor) =>
        // Add source line number for debugging (can fail when calling unsafe java methods)
        addLoc(loc)
        val declaration = internalNameOf(constructor.owner)
        // Create a new object of the declaration type
        mv.visitTypeInsn(Opcodes.NEW, declaration)
        // Duplicate the reference since the first argument for a constructor call is the reference to the object
        mv.visitInsn(Opcodes.DUP)
        for ((arg, argType) <- exps.zip(constructor.descriptor.parameterList.asScala)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(argType))
        }

        // Call the constructor
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, declaration, ClassMaker.ConstructorMethodName, constructor.descriptor.descriptorString(), false)

      case AtomicOp.InvokeSuperConstructor(constructor) =>
        // A InvokeSuperConstructor is handled directly in NewObject.
        throw InternalCompilerException(s"Unexpected call to super constructor: '$constructor'.", loc)

      case AtomicOp.InvokeMethod(method) =>
        val exp :: args = exps

        // Add source line number for debugging (can fail when calling unsafe java methods)
        addLoc(loc)

        // Evaluate the receiver object.
        compileExpr(exp)
        val declaration = internalNameOf(method.owner)
        mv.visitTypeInsn(Opcodes.CHECKCAST, declaration)

        for ((arg, argType) <- args.zip(method.descriptor.parameterList.asScala)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(argType))
        }

        // Check if we are invoking an interface or class.
        if (method.isInterface) {
          mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, declaration, method.name, method.descriptor.descriptorString(), true)
        } else {
          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, declaration, method.name, method.descriptor.descriptorString(), false)
        }

        // If the method is void, put a unit on top of the stack
        if (method.descriptor.returnType() == java.lang.constant.ConstantDescs.CD_void) {
          mv.visitFieldInsn(Opcodes.GETSTATIC, internalNameOf(GenUnit.desc), GenUnit.SingletonField.name, GenUnit.desc.descriptorString())
        }

      case AtomicOp.InvokeSuperMethod(sym, method) =>
        // Add source line number for debugging
        addLoc(loc)

        // The first expression is the receiver (the anonymous class instance, i.e. `_this`).
        val receiver :: args = exps

        // Evaluate the receiver object.
        compileExpr(receiver)
        val anonClassInternalName = sym.name.replace('.', '/')
        mv.visitTypeInsn(Opcodes.CHECKCAST, anonClassInternalName)

        // Evaluate and cast each argument.
        for ((arg, argType) <- args.zip(method.descriptor.parameterList.asScala)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(argType))
        }

        // Call the bridge method super$methodName on the anonymous class.
        val bridgeName = s"super$$${method.name}"
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, anonClassInternalName, bridgeName, method.descriptor.descriptorString(), false)

        // If the method is void, put a unit on top of the stack
        if (method.descriptor.returnType() == java.lang.constant.ConstantDescs.CD_void) {
          mv.visitFieldInsn(Opcodes.GETSTATIC, internalNameOf(GenUnit.desc), GenUnit.SingletonField.name, GenUnit.desc.descriptorString())
        }

      case AtomicOp.InvokeStaticMethod(method) =>
        // Add source line number for debugging (can fail when calling unsafe java methods)
        addLoc(loc)
        for ((arg, argType) <- exps.zip(method.descriptor.parameterList.asScala)) {
          compileExpr(arg)
          if (!argType.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(argType))
        }
        val declaration = internalNameOf(method.owner)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, declaration, method.name, method.descriptor.descriptorString(), method.isInterface)
        if (method.descriptor.returnType() == java.lang.constant.ConstantDescs.CD_void) {
          mv.visitFieldInsn(Opcodes.GETSTATIC, internalNameOf(GenUnit.desc), GenUnit.SingletonField.name, GenUnit.desc.descriptorString())
        }

      case AtomicOp.GetField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        addLoc(loc)
        compileExpr(exp)
        val declaration = internalNameOf(field.owner)
        mv.visitFieldInsn(Opcodes.GETFIELD, declaration, field.name, TypeDescs.toClassDesc(tpe).descriptorString())

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail when calling java)
        addLoc(loc)
        compileExpr(exp1)
        compileExpr(exp2)
        val declaration = internalNameOf(field.owner)
        mv.visitFieldInsn(Opcodes.PUTFIELD, declaration, field.name, TypeDescs.toClassDesc(exp2.tpe).descriptorString())

        // Push Unit on the stack.
        mv.visitFieldInsn(Opcodes.GETSTATIC, internalNameOf(GenUnit.desc), GenUnit.SingletonField.name, GenUnit.desc.descriptorString())

      case AtomicOp.GetStaticField(field) =>
        // Add source line number for debugging (can fail when calling java)
        addLoc(loc)
        val declaration = internalNameOf(field.owner)
        mv.visitFieldInsn(Opcodes.GETSTATIC, declaration, field.name, TypeDescs.toClassDesc(tpe).descriptorString())

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        addLoc(loc)
        compileExpr(exp)
        val declaration = internalNameOf(field.owner)
        mv.visitFieldInsn(Opcodes.PUTSTATIC, declaration, field.name, TypeDescs.toClassDesc(exp.tpe).descriptorString())

        // Push Unit on the stack.
        mv.visitFieldInsn(Opcodes.GETSTATIC, internalNameOf(GenUnit.desc), GenUnit.SingletonField.name, GenUnit.desc.descriptorString())

      case AtomicOp.Throw =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when handling exception).
        addLoc(loc)
        compileExpr(exp)
        ATHROW()

      case AtomicOp.Spawn =>
        val List(exp1, exp2) = exps
        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.Cst(Constant.Static, _) =>
            addLoc(loc)
            compileExpr(exp1)
            CHECKCAST(JavaClasses.Runnable)
            INVOKESTATIC(ClassConstants.Thread.StartVirtualThreadMethod)
            POP()
            GETSTATIC(GenUnit.SingletonField)
          case _ =>
            addLoc(loc)
            compileExpr(exp2)
            CHECKCAST(GenRegion.desc)
            compileExpr(exp1)
            CHECKCAST(JavaClasses.Runnable)
            INVOKEVIRTUAL(GenRegion.SpawnMethod)
            GETSTATIC(GenUnit.SingletonField)
        }

      case AtomicOp.Lazy =>
        val List(exp) = exps

        // Find the Lazy class name (Lazy$tpe).
        val SimpleType.Lazy(elmType) = tpe
        val lazyType = BackendObjType.Lazy(TypeDescs.toErasedClassDesc(elmType))

        NEW(lazyType.desc)
        DUP()
        compileExpr(exp)
        INVOKESPECIAL(lazyType.Constructor)

      case AtomicOp.Force =>
        val List(exp) = exps

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val SimpleType.Lazy(elmType) = exp.tpe
        val erasedElmType = TypeDescs.toErasedClassDesc(elmType)
        val lazyType = BackendObjType.Lazy(erasedElmType)

        // Emit code for the lazy expression.
        compileExpr(exp)
        CHECKCAST(lazyType.desc)
        DUP()
        GETFIELD(lazyType.ExpField)
        ifConditionElse(Condition.NONNULL)(
          INVOKEVIRTUAL(lazyType.ForceMethod)
        )(
          GETFIELD(lazyType.ValueField)
        )

      case AtomicOp.HoleError(sym) =>
        // Add source line number for debugging (failable by design).
        addLoc(loc)
        NEW(GenHoleError.desc) // HoleError
        DUP() // HoleError, HoleError
        pushString(sym.toString) // HoleError, HoleError, Sym
        pushLoc(loc) // HoleError, HoleError, Sym, Loc
        INVOKESPECIAL(GenHoleError.Constructor) // HoleError
        ATHROW()

      case AtomicOp.MatchError =>
        // Add source line number for debugging (failable by design)
        addLoc(loc)
        NEW(GenMatchError.desc) // MatchError
        DUP() // MatchError, MatchError
        pushLoc(loc) // MatchError, MatchError, Loc
        INVOKESPECIAL(GenMatchError.Constructor) // MatchError
        ATHROW()

      case AtomicOp.CastError(from, to) =>
        // Add source line number for debugging (failable by design)
        addLoc(loc)
        NEW(GenCastError.desc) // CastError
        DUP() // CastError, CastError
        pushLoc(loc) // CastError, CastError, Loc
        pushString(s"Cannot cast from type '$from' to '$to'") // CastError, CastError, Loc, String
        INVOKESPECIAL(GenCastError.Constructor) // CastError
        ATHROW()

      // Vector operations are simplified to array operations in the Simplifier.
      case AtomicOp.VectorLit => throw InternalCompilerException(s"Unexpected vector operation: '$op'.", loc)
      case AtomicOp.VectorLoad => throw InternalCompilerException(s"Unexpected vector operation: '$op'.", loc)
      case AtomicOp.VectorLength => throw InternalCompilerException(s"Unexpected vector operation: '$op'.", loc)
    }

    case Expr.ApplyClo(exp1, exp2, ct, _, purity, loc) =>
      // Type of the function abstract class
      val functionInterface = BackendObjType.Arrow.fromArrowType(exp1.tpe)
      val closureAbstractClass = BackendObjType.AbstractArrow.fromArrowType(exp1.tpe)
      ct match {
        case ExpPosition.Tail =>
          // Evaluating the closure
          compileExpr(exp1)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(closureAbstractClass.desc))
          // retrieving the unique thread object
          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalNameOf(closureAbstractClass.desc), closureAbstractClass.GetUniqueThreadClosureMethod.name, mkDescriptor()(closureAbstractClass.desc).descriptorString(), false)
          // Putting arg on the Fn class
          // Duplicate the FunctionInterface
          mv.visitInsn(Opcodes.DUP)
          // Evaluating the expression
          compileExpr(exp2)
          PUTFIELD(functionInterface.ArgField(0))
          // Return the closure
          mv.visitInsn(Opcodes.ARETURN)

        case ExpPosition.NonTail =>
          compileExpr(exp1)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(closureAbstractClass.desc))
          // retrieving the unique thread object
          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalNameOf(closureAbstractClass.desc), closureAbstractClass.GetUniqueThreadClosureMethod.name, mkDescriptor()(closureAbstractClass.desc).descriptorString(), false)
          // Putting arg on the Fn class
          // Duplicate the FunctionInterface
          mv.visitInsn(Opcodes.DUP)
          // Evaluating the expression
          compileExpr(exp2)
          PUTFIELD(functionInterface.ArgField(0))

          // Calling unwind and unboxing
          if (Purity.isControlPure(purity)) {
            GenResult.unwindSuspensionFreeThunk("in pure closure call", loc)
          } else {
            ctx match {
              case EffectContext(_, _, newFrame, setPc, narrowLocals, _, pcLabels, pcCounter) =>
                val pcPoint = pcCounter(0) + 1
                val pcPointLabel = pcLabels(pcPoint)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                GenResult.unwindThunkToValue(pcPoint, newFrame, setPc)
                mv.visitJumpInsn(Opcodes.GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)
                narrowLocals(mv)

                mv.visitVarInsn(Opcodes.ALOAD, 1)

                mv.visitLabel(afterUnboxing)

              case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
                throw InternalCompilerException("Unexpected direct method context in control impure function", loc)
            }
          }
      }

    case Expr.ApplyDef(sym, exps, ct, _, _, loc) => ct match {
      case ExpPosition.Tail =>
        val defInternalName = internalNameOf(GenFunAndClosureClasses.defnDesc(sym))
        // Type of the function abstract class
        val functionInterface = BackendObjType.Arrow.fromArrowType(root.defs(sym).arrowType)

        // Put the def on the stack
        mv.visitTypeInsn(Opcodes.NEW, defInternalName)
        mv.visitInsn(Opcodes.DUP)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, defInternalName, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), false)
        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(Opcodes.DUP)
          // Evaluating the expression
          compileExpr(arg)
          PUTFIELD(functionInterface.ArgField(i))
        }
        // Return the def
        mv.visitInsn(Opcodes.ARETURN)

      case ExpPosition.NonTail =>
        val defn = root.defs(sym)
        val targetIsFunction = defn.cparams.isEmpty
        val canCallStaticMethod = Purity.isControlPure(defn.expr.purity) && targetIsFunction
        if (canCallStaticMethod) {
          val paramTpes = defn.fparams.map(fp => TypeDescs.toClassDesc(fp.tpe))
          // Call the static method, using exact types
          for ((arg, tpe) <- ListOps.zip(exps, paramTpes)) {
            compileExpr(arg)
            castIfNotPrim(tpe)
          }
          val desc = mkDescriptor(paramTpes *)(GenResult.desc)
          val className = internalNameOf(GenFunAndClosureClasses.defnDesc(sym))
          mv.visitMethodInsn(Opcodes.INVOKESTATIC, className, ClassMaker.StaticApplyMethodName, desc.descriptorString(), false)
          GenResult.unwindSuspensionFreeThunk("in pure function call", loc)
        } else {
          // JvmType of Def
          val defInternalName = internalNameOf(GenFunAndClosureClasses.defnDesc(sym))

          // Put the def on the stack
          mv.visitTypeInsn(Opcodes.NEW, defInternalName)
          mv.visitInsn(Opcodes.DUP)
          mv.visitMethodInsn(Opcodes.INVOKESPECIAL, defInternalName, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), false)

          // Putting args on the Fn class
          for ((arg, i) <- exps.zipWithIndex) {
            // Duplicate the FunctionInterface
            mv.visitInsn(Opcodes.DUP)
            // Evaluating the expression
            compileExpr(arg)
            mv.visitFieldInsn(Opcodes.PUTFIELD, defInternalName,
              s"arg$i", TypeDescs.toErasedClassDesc(arg.tpe).descriptorString())
          }
          // Calling unwind and unboxing
          ctx match {
            case EffectContext(_, _, newFrame, setPc, narrowLocals, _, pcLabels, pcCounter) =>
              val defn = root.defs(sym)
              if (Purity.isControlPure(defn.expr.purity)) {
                GenResult.unwindSuspensionFreeThunk("in pure function call", loc)
              } else {
                val pcPoint = pcCounter(0) + 1
                val pcPointLabel = pcLabels(pcPoint)
                val afterUnboxing = new Label()
                pcCounter(0) += 1
                GenResult.unwindThunkToValue(pcPoint, newFrame, setPc)
                mv.visitJumpInsn(Opcodes.GOTO, afterUnboxing)

                mv.visitLabel(pcPointLabel)
                narrowLocals(mv)
                mv.visitVarInsn(Opcodes.ALOAD, 1)

                mv.visitLabel(afterUnboxing)
              }
            case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
              GenResult.unwindSuspensionFreeThunk("in pure function call", loc)
          }
        }
    }

    case Expr.ApplyOp(sym, exps, tpe, _, loc) => ctx match {
      case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
        GenResult.crashIfSuspension("Unexpected do-expression in direct method context", loc)

      case EffectContext(_, _, newFrame, setPc, narrowLocals, _, pcLabels, pcCounter) =>
        val pcPoint = pcCounter(0) + 1
        val pcPointLabel = pcLabels(pcPoint)
        val afterUnboxing = new Label()
        val erasedResult = TypeDescs.toErasedClassDesc(tpe)
        pcCounter(0) += 1

        val effectName = GenEffectClasses.effectDesc(sym.eff)
        val effectStaticMethod = ClassMaker.StaticMethod(
          effectName,
          GenEffectClasses.opName(sym),
          GenEffectClasses.opStaticFunctionDescriptor(sym)
        )
        NEW(GenSuspension.desc)
        DUP()
        INVOKESPECIAL(GenSuspension.Constructor)
        DUP()
        pushString(sym.eff.toString)
        PUTFIELD(GenSuspension.EffSymField)
        DUP()
        // --- eff op ---
        exps.foreach(compileExpr)
        mkStaticLambda(GenEffectCall.ApplyMethod, effectStaticMethod, 2)
        // --------------
        PUTFIELD(GenSuspension.EffOpField)
        DUP()
        // create continuation
        NEW(GenFramesNil.desc)
        DUP()
        INVOKESPECIAL(GenFramesNil.Constructor)
        newFrame(mv)
        DUP()
        pushInt(pcPoint)
        setPc(mv)
        INVOKEVIRTUAL(GenFramesNil.PushMethod)
        // store continuation
        PUTFIELD(GenSuspension.PrefixField)
        DUP()
        NEW(GenResumptionNil.desc)
        DUP()
        INVOKESPECIAL(GenResumptionNil.Constructor)
        PUTFIELD(GenSuspension.ResumptionField)
        xReturn(GenSuspension.desc)

        mv.visitLabel(pcPointLabel)
        narrowLocals(mv)
        ALOAD(1)
        GETFIELD(GenValue.fieldFromType(erasedResult))

        mv.visitLabel(afterUnboxing)
        castIfNotPrim(TypeDescs.toClassDesc(tpe))
    }

    case Expr.ApplySelfTail(sym, exps, _, _, _) => ctx match {
      case EffectContext(_, _, _, setPc, _, _, _, _) =>
        // The function abstract class name
        val functionInterface = BackendObjType.Arrow.fromArrowType(root.defs(sym).arrowType)
        // Evaluate each argument and put the result on the Fn class.
        for ((arg, i) <- exps.zipWithIndex) {
          mv.visitVarInsn(Opcodes.ALOAD, 0)
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
          PUTFIELD(functionInterface.ArgField(i))
        }
        mv.visitVarInsn(Opcodes.ALOAD, 0)
        pushInt(0)
        setPc(mv)
        // Jump to the entry point of the method.
        mv.visitJumpInsn(Opcodes.GOTO, ctx.entryPoint)

      case DirectInstanceContext(_, _, _) =>
        // The function abstract class name
        val functionInterface = BackendObjType.Arrow.fromArrowType(root.defs(sym).arrowType)
        // Evaluate each argument and put the result on the Fn class.
        for ((arg, i) <- exps.zipWithIndex) {
          mv.visitVarInsn(Opcodes.ALOAD, 0)
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
          PUTFIELD(functionInterface.ArgField(i))
        }
        // Jump to the entry point of the method.
        mv.visitJumpInsn(Opcodes.GOTO, ctx.entryPoint)

      case DirectStaticContext(_, _, _) =>
        val defn = root.defs(sym)
        for (arg <- exps) {
          // Evaluate the argument and push the result on the stack.
          compileExpr(arg)
        }
        for ((arg, fp) <- ListOps.zip(exps, defn.fparams).reverse) {
          // Store it in the ith parameter.
          val tpe = TypeDescs.toClassDesc(arg.tpe)
          val offset = ctx.getIndex(fp.offset)
          xStore(tpe, offset)
        }
        // Jump to the entry point of the method.
        mv.visitJumpInsn(Opcodes.GOTO, ctx.entryPoint)
    }

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
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
      mv.visitJumpInsn(Opcodes.GOTO, endLabel)
      // Compiling branches
      branches.foreach { case (sym, branchExp) =>
        // Label for the start of the branch
        mv.visitLabel(updatedJumpLabels(sym))
        // evaluating the expression for the branch
        compileExpr(branchExp)(mv, ctx1, root, flix)
        // Skip the rest of the branches
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
      }
      // label for the end of branches
      mv.visitLabel(endLabel)

    case Expr.JumpTo(sym, _, _, _) =>
      // Jumping to the label
      mv.visitJumpInsn(Opcodes.GOTO, ctx.lenv(sym))

    case Expr.Switch(exp, enumSym, cases, defaultExp, _, _, _) =>
      // Compile the scrutinee (pushes enum value onto stack)
      compileExpr(exp)
      // Extract ordinal: checkcast Tagged, getfield ordinal
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(GenTagged.desc))
      mv.visitFieldInsn(Opcodes.GETFIELD, internalNameOf(GenTagged.desc), "ordinal", CD_int.descriptorString())
      // Build labels
      val defaultLabel = new Label()
      val endLabel = new Label()
      val caseLabels = cases.map { case (sym, _) => sym.ordinal -> new Label() }.toMap
      // Choose between tableswitch and lookupswitch based on bytecode size.
      //
      // A tableswitch allocates a slot for every ordinal in the range 0..N-1,
      // even if most slots just point to the default label. A lookupswitch only
      // stores the explicitly listed cases as sorted (key, label) pairs.
      //
      //   tableswitch cost:  12 + 4*N bytes  (N = total enum variants, O(1) dispatch)
      //   lookupswitch cost:  8 + 8*E bytes  (E = explicit cases,  O(log E) dispatch)
      //
      // Example: an enum with 161 variants, matching on 5 cases + wildcard default.
      //
      //   tableswitch:  12 + 4*161 = 656 bytes — a 161-entry jump table, mostly
      //                 pointing to the default. Exceeds the JVM JIT inlining
      //                 threshold (~325 bytes).
      //
      //   lookupswitch:  8 + 8*5  =  48 bytes — 5 sorted (ordinal, label) pairs.
      //                 The JVM binary-searches the keys (≈3 comparisons for 5 entries).
      //
      // The crossover point is when E < (N+1)/2, i.e. when fewer than half the
      // ordinals have explicit cases.
      val numTotal = root.enums(enumSym).cases.size
      val numExplicit = cases.size
      val tableswitchCost = 12 + 4 * numTotal
      val lookupswitchCost = 8 + 8 * numExplicit
      if (lookupswitchCost < tableswitchCost) {
        val sorted = cases.sortBy(_._1.ordinal)
        val keys = sorted.map(_._1.ordinal).toArray
        val labels = sorted.map { case (sym, _) => caseLabels(sym.ordinal) }.toArray
        mv.visitLookupSwitchInsn(defaultLabel, keys, labels)
      } else {
        val table = (0 until numTotal).map(i => caseLabels.getOrElse(i, defaultLabel)).toArray
        mv.visitTableSwitchInsn(0, numTotal - 1, defaultLabel, table: _*)
      }
      // Emit each case branch
      cases.foreach { case (sym, body) =>
        mv.visitLabel(caseLabels(sym.ordinal))
        compileExpr(body)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
      }
      // Default branch
      mv.visitLabel(defaultLabel)
      compileExpr(defaultExp)
      // End label
      mv.visitLabel(endLabel)

    case Expr.Let(_, offset, exp1, exp2, _) =>
      val bType = TypeDescs.toClassDesc(exp1.tpe)
      compileExpr(exp1)
      // No cast needed in most cases: operations self-cast (Untag, Index, etc.),
      // function calls are wrapped in Cast by the Eraser, and effect resume
      // sites use narrowLocals. The exception is NewObject (anonymous Java
      // classes) where the JVM verifier cannot resolve the generated subclass
      // name and needs an explicit cast to the declared superclass type.
      exp1 match {
        case _: Expr.NewObject => castIfNotPrim(bType)
        case _ => ()
      }
      xStore(bType, ctx.getIndex(offset))
      compileExpr(exp2)

    case Expr.Stm(exps, exp, _) =>
      exps.foreach { e =>
        compileExpr(e)
        xPop(TypeDescs.toClassDesc(e.tpe))
      }
      compileExpr(exp)

    case Expr.Region(_, offset, exp, _, _, loc) =>
      // Adding source line number for debugging
      addLoc(loc)

      // Introduce a label for before the try block.
      val beforeTryBlock = new Label()

      // Introduce a label for after the try block.
      val afterTryBlock = new Label()

      // Introduce a label for the finally block.
      val finallyBlock = new Label()

      // Introduce a label after the finally block.
      val afterFinally = new Label()

      // Create an instance of Region
      mv.visitTypeInsn(Opcodes.NEW, internalNameOf(GenRegion.desc))
      mv.visitInsn(Opcodes.DUP)
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, internalNameOf(GenRegion.desc), ClassMaker.ConstructorMethodName,
        MethodTypeDescs.NothingToVoid.descriptorString(), false)

      xStore(GenRegion.desc, ctx.getIndex(offset))

      // Compile the scope body
      mv.visitLabel(beforeTryBlock)
      compileExpr(exp)

      // Emit try finally block. It's important to do this after compiling sub-expressions to ensure
      // correct catch case ordering.
      mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

      // When we exit the scope, call the region's `exit` method
      xLoad(GenRegion.desc, ctx.getIndex(offset))
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(GenRegion.desc))
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalNameOf(GenRegion.desc), GenRegion.ExitMethod.name,
        GenRegion.ExitMethod.d.descriptorString(), false)
      mv.visitLabel(afterTryBlock)

      // Compile the finally block which gets called if no exception is thrown
      xLoad(GenRegion.desc, ctx.getIndex(offset))
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(GenRegion.desc))
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalNameOf(GenRegion.desc), GenRegion.ReThrowChildExceptionMethod.name,
        GenRegion.ReThrowChildExceptionMethod.d.descriptorString(), false)
      mv.visitJumpInsn(Opcodes.GOTO, afterFinally)

      // Compile the finally block which gets called if an exception is thrown
      mv.visitLabel(finallyBlock)
      xLoad(GenRegion.desc, ctx.getIndex(offset))
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(GenRegion.desc))
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalNameOf(GenRegion.desc), GenRegion.ReThrowChildExceptionMethod.name,
        GenRegion.ReThrowChildExceptionMethod.d.descriptorString(), false)
      mv.visitInsn(Opcodes.ATHROW)
      mv.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      // Add source line number for debugging.
      addLoc(loc)

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
      mv.visitJumpInsn(Opcodes.GOTO, afterTryAndCatch)

      // Emit code for each catch rule.
      for ((CatchRule(_, offset, _, body), handlerLabel) <- rulesAndLabels) {
        // Emit the label.
        mv.visitLabel(handlerLabel)

        // Store the exception in a local variable.
        xStore(JavaClasses.Object, ctx.getIndex(offset))

        // Emit code for the handler body expression.
        compileExpr(body)
        mv.visitJumpInsn(Opcodes.GOTO, afterTryAndCatch)
      }

      // Emit a try catch block for each catch rule. It's important to do this after compiling
      // sub-expressions to ensure correct catch case ordering.
      for ((CatchRule(_, _, clazz, _), handlerLabel) <- rulesAndLabels) {
        mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, internalNameOf(clazz))
      }

      // Add the label after both the try and catch rules.
      mv.visitLabel(afterTryAndCatch)

    case Expr.RunWith(exp, effUse, rules, ct, _, _, loc) =>
      // exp is a Unit -> exp.tpe closure
      val effectName = GenEffectClasses.effectDesc(effUse.sym)
      val effectInternalName = internalNameOf(effectName)
      // eff name
      pushString(effUse.sym.toString)
      // handler
      NEW(effectName)
      DUP()
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, effectInternalName, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), false)
      // bind handler closures
      for (HandlerRule(op, _, body) <- rules) {
        mv.visitInsn(Opcodes.DUP)
        compileExpr(body)
        mv.visitFieldInsn(Opcodes.PUTFIELD, effectInternalName, GenEffectClasses.opName(op.sym), GenEffectClasses.opFieldType(op.sym).toDescriptor)
      }
      // frames
      NEW(GenFramesNil.desc)
      DUP()
      INVOKESPECIAL(GenFramesNil.Constructor)
      // continuation
      compileExpr(exp)
      // exp.arg0 should be set to unit here but from lifting we know that it is unused so the
      // implicit null is fine.
      // call installHandler
      INVOKESTATIC(GenHandler.InstallHandlerMethod)
      // handle value/suspend/thunk if in non-tail position
      if (ct == ExpPosition.NonTail) {
        ctx match {
          case DirectInstanceContext(_, _, _) | DirectStaticContext(_, _, _) =>
            GenResult.unwindSuspensionFreeThunk("in pure run-with call", loc)

          case EffectContext(_, _, newFrame, setPc, narrowLocals, _, pcLabels, pcCounter) =>
            val pcPoint = pcCounter(0) + 1
            val pcPointLabel = pcLabels(pcPoint)
            val afterUnboxing = new Label()
            pcCounter(0) += 1
            GenResult.unwindThunkToValue(pcPoint, newFrame, setPc)
            mv.visitJumpInsn(Opcodes.GOTO, afterUnboxing)

            mv.visitLabel(pcPointLabel)
            narrowLocals(mv)
            ALOAD(1)
            mv.visitLabel(afterUnboxing)
        }
      } else {
        ARETURN()
      }

    case Expr.NewObject(sym, _, _, _, constructors, methods, _) =>
      val methodExps = methods.map(_.exp)
      val className = sym.name
      mv.visitTypeInsn(Opcodes.NEW, className)
      mv.visitInsn(Opcodes.DUP)

      // Handle constructors
      if (constructors.nonEmpty) {
        constructors.head.exp match {
          case Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(constructor), superArgs, _, _, _) =>
            // Super-only: compile args and call parameterized <init>
            for ((arg, argType) <- superArgs.zip(constructor.descriptor.parameterList.asScala)) {
              compileExpr(arg)
              if (!argType.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(argType))
            }
            mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, ClassMaker.ConstructorMethodName, constructor.descriptor.descriptorString(), false)
          case _ => throw InternalCompilerException(s"Unexpected non-super constructor body.", constructors.head.loc)
        }
      } else {
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), false)
      }

      // For each method, compile the closure which implements the body of that method and store it in a field
      methodExps.zipWithIndex.foreach { case (e, i) =>
        mv.visitInsn(Opcodes.DUP)
        compileExpr(e)
        mv.visitFieldInsn(Opcodes.PUTFIELD, className, s"clo$i", BackendObjType.AbstractArrow.fromArrowType(e.tpe).toDescriptor)
      }

  }

  private def getStructType(struct: Struct)(implicit root: Root): BackendObjType.Struct = {
    BackendObjType.Struct(struct.fields.map(field => TypeDescs.toErasedClassDesc(field.tpe)))
  }

  private def compileIsTag(ordinal: Int, exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    CHECKCAST(GenTagged.desc)
    GETFIELD(GenTagged.OrdinalField)
    pushInt(ordinal)
    ifConditionElse(Condition.ICMPEQ)(pushBool(true))(pushBool(false))
  }

  private def compileTag(enumName: String, name: String, ordinal: Int, exps: List[Expr], tpes: List[ClassDesc])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    tpes match {
      case Nil =>
        GETSTATIC(GenNullaryTag.SingletonField(enumName, name))
      case _ =>
        NEW(GenTag.desc(tpes))
        DUP()
        INVOKESPECIAL(GenTag.Constructor(tpes))
        DUP()
        pushInt(ordinal)
        PUTFIELD(GenTag.OrdinalField)
        exps.zipWithIndex.foreach {
          case (e, i) => DUP()
            compileExpr(e)
            PUTFIELD(GenTag.IndexField(tpes, i))
        }
    }
  }

  private def compileUntag(exp: Expr, idx: Int, tpes: List[ClassDesc])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    // GenNullaryTag cannot happen here since terms must be non-empty.
    if (tpes.isEmpty) throw InternalCompilerException(s"Unexpected empty tag types", exp.loc)
    compileExpr(exp)
    CHECKCAST(GenTag.desc(tpes))
    GETFIELD(GenTag.IndexField(tpes, idx))
  }

  private def compileExtIsTag(name: String, exp: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    CHECKCAST(GenExtTagged.desc)
    GETFIELD(GenExtTagged.NameField)
    GenExtTagged.mkTagName(name)
    GenExtTagged.eqTagName()
  }

  private def compileExtTag(name: String, exps: List[Expr], tpes: List[ClassDesc])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    NEW(GenExtTag.desc(tpes))
    DUP()
    INVOKESPECIAL(GenExtTag.Constructor(tpes))
    DUP()
    GenExtTagged.mkTagName(name)
    PUTFIELD(GenExtTag.NameField)
    exps.zipWithIndex.foreach {
      case (e, i) => DUP()
        compileExpr(e)
        PUTFIELD(GenExtTag.IndexField(tpes, i))
    }
  }

  private def compileExtUntag(exp: Expr, idx: Int, tpes: List[ClassDesc])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    compileExpr(exp)
    CHECKCAST(GenExtTag.desc(tpes))
    GETFIELD(GenExtTag.IndexField(tpes, idx))
  }

  private def visitComparisonPrologue(exp1: Expr, exp2: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): (Label, Label) = {
    compileExpr(exp1)
    compileExpr(exp2)
    val condElse = new Label()
    val condEnd = new Label()
    (condElse, condEnd)
  }

  private def visitComparisonEpilogue(visitor: MethodVisitor, condElse: Label, condEnd: Label): Unit = {
    visitor.visitInsn(Opcodes.ICONST_1)
    visitor.visitJumpInsn(Opcodes.GOTO, condEnd)
    visitor.visitLabel(condElse)
    visitor.visitInsn(Opcodes.ICONST_0)
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
      mv.visitInsn(Opcodes.ICONST_M1)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case 0 =>
      mv.visitInsn(Opcodes.LCONST_0)

    case 1 =>
      mv.visitInsn(Opcodes.LCONST_1)

    case 2 =>
      mv.visitInsn(Opcodes.ICONST_2)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case 3 =>
      mv.visitInsn(Opcodes.ICONST_3)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case 4 =>
      mv.visitInsn(Opcodes.ICONST_4)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case 5 =>
      mv.visitInsn(Opcodes.ICONST_5)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue =>
      mv.visitIntInsn(Opcodes.BIPUSH, i.toInt)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue =>
      mv.visitIntInsn(Opcodes.SIPUSH, i.toInt)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case _ if scala.Int.MinValue <= i && i <= scala.Int.MaxValue =>
      mv.visitLdcInsn(i.toInt)
      mv.visitInsn(Opcodes.I2L) // Sign extend to long

    case _ => mv.visitLdcInsn(i)
  }

}
