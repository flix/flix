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
import ca.uwaterloo.flix.language.ast.Ast.CallType
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast.{MonoType, _}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

/**
  * Generate expression
  */
object GenExpression {

  case class MethodContext(clazz: JvmType.Reference, entryPoint: Label, lenv: Map[Symbol.LabelSym, Label])

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpr(exp0: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = exp0 match {

    case Expr.Cst(cst, tpe, loc) => cst match {
      case Ast.Constant.Unit =>
        addSourceLine(mv, loc)
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName,
          BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.toDescriptor)

      case Ast.Constant.Null =>
        addSourceLine(mv, loc)
        mv.visitInsn(ACONST_NULL)
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case Ast.Constant.Bool(true) =>
        addSourceLine(mv, loc)
        mv.visitInsn(ICONST_1)

      case Ast.Constant.Bool(false) =>
        addSourceLine(mv, loc)
        mv.visitInsn(ICONST_0)

      case Ast.Constant.Char(c) =>
        addSourceLine(mv, loc)
        compileInt(c)

      case Ast.Constant.Float32(f) =>
        addSourceLine(mv, loc)
        f match {
          case 0f => mv.visitInsn(FCONST_0)
          case 1f => mv.visitInsn(FCONST_1)
          case 2f => mv.visitInsn(FCONST_2)
          case _ => mv.visitLdcInsn(f)
        }

      case Ast.Constant.Float64(d) =>
        addSourceLine(mv, loc)
        d match {
          case 0d => mv.visitInsn(DCONST_0)
          case 1d => mv.visitInsn(DCONST_1)
          case _ => mv.visitLdcInsn(d)
        }

      case Ast.Constant.BigDecimal(dd) =>
        addSourceLine(mv, loc)
        mv.visitTypeInsn(NEW, BackendObjType.BigDecimal.jvmName.toInternalName)
        mv.visitInsn(DUP)
        mv.visitLdcInsn(dd.toString)
        mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigDecimal.jvmName.toInternalName, "<init>",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

      case Ast.Constant.Int8(b) =>
        addSourceLine(mv, loc)
        compileInt(b)

      case Ast.Constant.Int16(s) =>
        addSourceLine(mv, loc)
        compileInt(s)

      case Ast.Constant.Int32(i) =>
        addSourceLine(mv, loc)
        compileInt(i)

      case Ast.Constant.Int64(l) =>
        addSourceLine(mv, loc)
        compileLong(l)

      case Ast.Constant.BigInt(ii) =>
        addSourceLine(mv, loc)
        mv.visitTypeInsn(NEW, BackendObjType.BigInt.jvmName.toInternalName)
        mv.visitInsn(DUP)
        mv.visitLdcInsn(ii.toString)
        mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigInt.jvmName.toInternalName, "<init>",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

      case Ast.Constant.Str(s) =>
        addSourceLine(mv, loc)
        mv.visitLdcInsn(s)

      case Ast.Constant.Regex(patt) =>
        addSourceLine(mv, loc)
        mv.visitLdcInsn(patt.pattern)
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Regex), false)

    }

    case Expr.Var(sym, tpe, _) =>
      val jvmType = JvmOps.getErasedJvmType(tpe)
      val iLOAD = AsmOps.getLoadInstruction(jvmType)
      mv.visitVarInsn(iLOAD, sym.getStackOffset + 1)
      AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

    case Expr.ApplyAtomic(op, exps, tpe, loc) => op match {

      case AtomicOp.Closure(sym) =>
        // JvmType of the closure
        val jvmType = JvmOps.getClosureClassType(sym)
        // new closure instance
        mv.visitTypeInsn(NEW, jvmType.name.toInternalName)
        // Duplicate
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
        // Capturing free args
        for ((arg, i) <- exps.zipWithIndex) {
          val erasedArgType = JvmOps.getErasedJvmType(arg.tpe)
          mv.visitInsn(DUP)
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, jvmType.name.toInternalName, s"clo$i", erasedArgType.toDescriptor)
        }

      case AtomicOp.Unary(sop) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(mv, exp.loc)
        compileExpr(exp)

        sop match {
          case SemanticOperator.BoolOp.Not =>
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

          case BigDecimalOp.Neg =>
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "negate",
              AsmOps.getMethodDescriptor(Nil, JvmType.BigDecimal), false)

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

          case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", exp.loc)
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

          case BigDecimalOp.Lt =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPGE, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case BigDecimalOp.Le =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPGT, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case BigDecimalOp.Eq =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPNE, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case BigDecimalOp.Neq =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPEQ, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case BigDecimalOp.Ge =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPLT, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case BigDecimalOp.Gt =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            mv.visitInsn(ICONST_0)
            mv.visitJumpInsn(IF_ICMPLE, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

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

          case StringOp.Eq =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.JavaObject.jvmName.toInternalName, "equals",
              AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
            mv.visitInsn(ICONST_1)
            mv.visitJumpInsn(IF_ICMPNE, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

          case StringOp.Neq =>
            val (condElse, condEnd) = visitComparisonPrologue(exp1, exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.JavaObject.jvmName.toInternalName, "equals",
              AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
            mv.visitInsn(ICONST_1)
            mv.visitJumpInsn(IF_ICMPEQ, condElse)
            visitComparisonEpilogue(mv, condElse, condEnd)

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

          case BigDecimalOp.Add =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "add",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.BigDecimal), false)

          case BigDecimalOp.Sub =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "subtract",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.BigDecimal), false)

          case BigDecimalOp.Mul =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "multiply",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.BigDecimal), false)

          case BigDecimalOp.Div =>
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "divide",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.BigDecimal), false)

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
            compileExpr(exp1)
            compileExpr(exp2)
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, "concat",
              AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.String), false)

          case _ => InternalCompilerException(s"Unexpected semantic operator: $sop.", exp1.loc)

        }

      case AtomicOp.Region =>
        //!TODO: For now, just emit unit
        val e = Expr.Cst(Ast.Constant.Unit, MonoType.Unit, loc)
        compileExpr(e)

      case AtomicOp.ScopeExit =>
        val List(exp1, exp2) = exps

        // Compile the expression, putting a function implementing the Runnable interface on the stack
        compileExpr(exp1)
        mv.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

        // Compile the expression representing the region
        compileExpr(exp2)
        mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)

        // Call the Region's `runOnExit` method
        mv.visitInsn(SWAP)
        mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.RunOnExitMethod.name, BackendObjType.Region.RunOnExitMethod.d.toDescriptor, false)

        // Put a Unit value on the stack
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.Is(sym) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for tag
        val classType = JvmOps.getTagClassType(sym)

        // First we compile the `exp`
        compileExpr(exp)
        // We check if the enum is `instanceof` the class
        mv.visitTypeInsn(INSTANCEOF, classType.name.toInternalName)

      // Normal Tag
      case AtomicOp.Tag(sym) =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for tag
        val classType = JvmOps.getTagClassType(sym)
        // Creating a new instance of the class
        mv.visitTypeInsn(NEW, classType.name.toInternalName)
        mv.visitInsn(DUP)
        // Evaluating the single argument of the class constructor
        compileExpr(exp)
        // Descriptor of the constructor
        val constructorDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedJvmType(exp.tpe)), JvmType.Void)
        // Calling the constructor of the class
        mv.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

      case AtomicOp.Untag(sym) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for the tag
        val classType = JvmOps.getTagClassType(sym)
        // Evaluate the exp
        compileExpr(exp)
        // Cast the exp to the type of the tag
        mv.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
        // Descriptor of the method
        val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe))
        // Invoke `getValue()` method to extract the field of the tag
        mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
        // Cast the object to it's type if it's not a primitive
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.Index(idx) =>
        val List(exp) = exps
        // We get the JvmType of the class for the tuple
        val classType = JvmOps.getTupleClassType(exp.tpe.asInstanceOf[MonoType.Tuple])
        // evaluating the `base`
        compileExpr(exp)
        // Retrieving the field `field${offset}`
        mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"field$idx", JvmOps.getErasedJvmType(tpe).toDescriptor)
        // Cast the object to it's type if it's not a primitive
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.Tuple =>
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for the tuple
        val classType = JvmOps.getTupleClassType(tpe.asInstanceOf[MonoType.Tuple])
        // Instantiating a new object of tuple
        mv.visitTypeInsn(NEW, classType.name.toInternalName)
        // Duplicating the class
        mv.visitInsn(DUP)
        // Evaluating all the elements to be stored in the tuple class
        exps.foreach(compileExpr)
        // Erased type of `elms`
        val erasedElmTypes = exps.map(_.tpe).map(JvmOps.getErasedJvmType)
        // Descriptor of constructor
        val constructorDescriptor = AsmOps.getMethodDescriptor(erasedElmTypes, JvmType.Void)
        // Invoking the constructor
        mv.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

      case AtomicOp.RecordEmpty =>
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for the RecordEmpty
        val classType = JvmOps.getRecordEmptyClassType()
        // Instantiating a new object of tuple
        mv.visitFieldInsn(GETSTATIC, classType.name.toInternalName, BackendObjType.RecordEmpty.InstanceField.name, classType.toDescriptor)

      case AtomicOp.RecordSelect(field) =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)

        // Get the correct record extend class, given the expression type 'tpe'
        // We get the JvmType of the extended record class to retrieve the proper field
        val classType = JvmOps.getRecordType(tpe)

        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(tpe), BackendObjType.RecordEmpty.toTpe)

        //Compile the expression exp (which should be a record), as we need to have on the stack a record in order to call
        //lookupField
        compileExpr(exp)

        //Push the desired label of the field we want get of the record onto the stack
        mv.visitLdcInsn(field.name)

        //Invoke the lookupField method on the record. (To get the proper record object)
        mv.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, "lookupField",
          AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)

        //Cast to proper record extend class
        mv.visitTypeInsn(CHECKCAST, classType.name.toInternalName)

        //Retrieve the value field  (To get the proper value)
        mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)

        // Cast the field value to the expected type.
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.RecordExtend(field) =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the class for the record extend
        val classType = JvmOps.getRecordExtendClassType(tpe)

        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        // previous functions are already partial matches
        val MonoType.RecordExtend(_, recordValueType, _) = tpe
        val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(recordValueType), BackendObjType.RecordEmpty.toTpe)

        // Instantiating a new object of tuple
        mv.visitTypeInsn(NEW, classType.name.toInternalName)
        mv.visitInsn(DUP)
        // Invoking the constructor
        mv.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)

        //Put the label of field (which is going to be the extension).
        mv.visitInsn(DUP)
        mv.visitLdcInsn(field.name)
        mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.LabelField.name, BackendObjType.String.toDescriptor)

        //Put the value of the field onto the stack, since it is an expression we first need to compile it.
        mv.visitInsn(DUP)
        compileExpr(exp1)
        mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)

        //Put the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        mv.visitInsn(DUP)
        compileExpr(exp2)
        mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.RestField.name, interfaceType.toDescriptor)

      case AtomicOp.RecordRestrict(field) =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        //Push the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        compileExpr(exp)
        //Push the label of field (which is going to be the removed/restricted).
        mv.visitLdcInsn(field.name)

        // Invoking the restrictField method
        mv.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, BackendObjType.Record.RestrictFieldMethod.name,
          AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)

      case AtomicOp.ArrayLit =>
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We push the 'length' of the array on top of stack
        compileInt(exps.length)
        // We get the inner type of the array
        val jvmType = JvmOps.getJvmType(tpe.asInstanceOf[MonoType.Array].tpe)
        // Instantiating a new array of type jvmType
        jvmType match {
          case ref: JvmType.Reference => // Happens if the inner type is an object type
            mv.visitTypeInsn(ANEWARRAY, ref.name.toInternalName)
          case _ => // Happens if the inner type is a primitive type
            mv.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
        }
        // For each element we generate code to store it into the array
        for (i <- exps.indices) {
          // Duplicates the 'array reference'
          mv.visitInsn(DUP)
          // We push the 'index' of the current element on top of stack
          compileInt(i)
          // Evaluating the 'element' to be stored
          compileExpr(exps(i))
          // Stores the 'element' at the given 'index' in the 'array'
          // with the store instruction corresponding to the stored element
          mv.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
        }

      case AtomicOp.ArrayNew =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the inner type of the array
        val elmType = tpe.asInstanceOf[MonoType.Array].tpe
        // We get the erased elm type.
        val jvmType = JvmOps.getErasedJvmType(elmType)
        // Evaluating the value of the 'default element'
        compileExpr(exp1)
        // Evaluating the 'length' of the array
        compileExpr(exp2)
        // Instantiating a new array of type jvmType
        if (elmType == MonoType.Str) {
          mv.visitTypeInsn(ANEWARRAY, "java/lang/String")
        } else if (elmType.isInstanceOf[MonoType.Native]) {
          val native = elmType.asInstanceOf[MonoType.Native]
          val name = native.clazz.getName.replace('.', '/')
          mv.visitTypeInsn(ANEWARRAY, name)
        } else if (jvmType == JvmType.Object) { // Happens if the inner type is an object type
          mv.visitTypeInsn(ANEWARRAY, "java/lang/Object")
        } else { // Happens if the inner type is a primitive type
          mv.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
        }
        if (jvmType == JvmType.PrimLong || jvmType == JvmType.PrimDouble) { // Happens if the inner type is Int64 or Float64
          // Duplicates the 'array reference' three places down the stack
          mv.visitInsn(DUP_X2)
          // Duplicates the 'array reference' three places down the stack
          mv.visitInsn(DUP_X2)
          // Pops the 'ArrayRef' at the top of the stack
          mv.visitInsn(POP)
        } else {
          // Duplicates the 'array reference' two places down the stack
          mv.visitInsn(DUP_X1)
          // Swaps the 'array reference' and 'default element'
          mv.visitInsn(SWAP)
        }
        // We get the array fill type
        val arrayFillType = AsmOps.getArrayFillType(jvmType)
        // Invoking the method to fill the array with the default element
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Arrays", "fill", arrayFillType, false);

      case AtomicOp.ArrayLoad =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
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
          // Adding source line number for debugging
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
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        case _ => throw InternalCompilerException("Mismatched Arity", loc)
      }

      case AtomicOp.ArrayLength =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // We get the inner type of the array
        val jvmType = JvmOps.getErasedJvmType(exp.tpe.asInstanceOf[MonoType.Array].tpe)
        // Evaluating the 'base'
        compileExpr(exp)
        // Cast the object to array
        mv.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
        // Pushes the 'length' of the array on top of stack
        mv.visitInsn(ARRAYLENGTH)

      case AtomicOp.Ref =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Create a new reference object
        mv.visitTypeInsn(NEW, classType.name.toInternalName)
        // Duplicate it since one instance will get consumed by constructor
        mv.visitInsn(DUP)
        // Call the constructor
        mv.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
        // Duplicate it since one instance will get consumed by putfield
        mv.visitInsn(DUP)
        // Evaluate the underlying expression
        compileExpr(exp)
        // Erased type of the value of the reference
        val valueErasedType = JvmOps.getErasedJvmType(tpe.asInstanceOf[MonoType.Ref].tpe)
        // set the field with the ref value
        mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, valueErasedType.toDescriptor)

      case AtomicOp.Deref =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // Evaluate the exp
        compileExpr(exp)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(exp.tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = exp.tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Cast the ref
        mv.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
        // Dereference the expression
        mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)
        // Cast underlying value to the correct type if the underlying type is Object
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.Assign =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(mv, loc)
        // Evaluate the reference address
        compileExpr(exp1)
        // Evaluating the value to be assigned to the reference
        compileExpr(exp2)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(exp1.tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = exp1.tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Invoke `setValue` method to set the value to the given number
        mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
        // Since the return type is unit, we put an instance of unit on top of the stack
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.InstanceOf(clazz) =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        val className = asm.Type.getInternalName(clazz)
        compileExpr(exp)
        mv.visitTypeInsn(INSTANCEOF, className.toString)

      case AtomicOp.Cast =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.InvokeConstructor(constructor) =>
        // Adding source line number for debugging
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

        // Adding source line number for debugging
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
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.InvokeStaticMethod(method) =>
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
          mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.GetField(field) =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETFIELD, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        addSourceLine(mv, loc)
        compileExpr(exp1)
        compileExpr(exp2)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTFIELD, declaration, field.getName, JvmOps.getJvmType(exp2.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.GetStaticField(field) =>
        addSourceLine(mv, loc)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETSTATIC, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTSTATIC, declaration, field.getName, JvmOps.getJvmType(exp.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)


      case AtomicOp.Spawn =>
        val List(exp1, exp2) = exps

        addSourceLine(mv, loc)

        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.ApplyAtomic(AtomicOp.Region, _, tpe, loc) =>

            // Compile the expression, putting a function implementing the Runnable interface on the stack
            compileExpr(exp1)
            mv.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

            // make a thread and run it
            if (flix.options.xvirtualthreads) {
              mv.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "startVirtualThread", s"(${JvmName.Runnable.toDescriptor})${JvmName.Thread.toDescriptor}", false)
              mv.visitInsn(POP)
            } else {
              mv.visitTypeInsn(NEW, "java/lang/Thread")
              mv.visitInsn(DUP_X1)
              mv.visitInsn(SWAP)
              mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Thread", "<init>", s"(${JvmName.Runnable.toDescriptor})${JvmType.Void.toDescriptor}", false)
              mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Thread", "start", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
            }

          case _ =>
            // Compile the expression representing the region
            compileExpr(exp2)
            mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)

            // Compile the expression, putting a function implementing the Runnable interface on the stack
            compileExpr(exp1)
            mv.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

            // Call the Region's `spawn` method
            mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.SpawnMethod.name, BackendObjType.Region.SpawnMethod.d.toDescriptor, false)
        }

        // Put a Unit value on the stack
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)


      case AtomicOp.Lazy =>
        val List(exp) = exps
        // Add source line numbers for debugging.
        addSourceLine(mv, loc)

        // Find the Lazy class name (Lazy$tpe).
        val classType = JvmOps.getLazyClassType(tpe.asInstanceOf[MonoType.Lazy]).name.toInternalName

        // Make a new lazy object and dup it to leave it on the stack.
        mv.visitTypeInsn(NEW, classType)
        mv.visitInsn(DUP)

        // Compile the thunked expression and call new Lazy$erased_tpe(expression).
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESPECIAL, classType, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), false)

      case AtomicOp.Force =>
        val List(exp) = exps
        // Add source line numbers for debugging.
        addSourceLine(mv, loc)

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val classMonoType = exp.tpe.asInstanceOf[MonoType.Lazy]
        val classType = JvmOps.getLazyClassType(classMonoType)
        val internalClassType = classType.name.toInternalName
        val MonoType.Lazy(tpe) = classMonoType
        val erasedType = JvmOps.getErasedJvmType(tpe)

        // Emit code for the lazy expression.
        compileExpr(exp)

        // Lazy$tpe is expected.
        mv.visitTypeInsn(CHECKCAST, internalClassType)

        // Dup for later lazy.value or lazy.force(context)
        mv.visitInsn(DUP)
        // Get expression
        mv.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
        val alreadyInit = new Label()
        val end = new Label()
        // If expression == null the we just use lazy.value, otherwise lazy.force(context)
        mv.visitJumpInsn(IFNULL, alreadyInit)

        // Call force().
        mv.visitMethodInsn(INVOKEVIRTUAL, internalClassType, "force", AsmOps.getMethodDescriptor(Nil, erasedType), false)
        // goto the cast to undo erasure
        mv.visitJumpInsn(GOTO, end)

        mv.visitLabel(alreadyInit)
        // Retrieve the erased value
        mv.visitFieldInsn(GETFIELD, internalClassType, "value", erasedType.toDescriptor)

        mv.visitLabel(end)
        // The result of force is a generic object so a cast is needed.
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.BoxBool =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)

      case AtomicOp.BoxInt8 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)

      case AtomicOp.BoxInt16 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)

      case AtomicOp.BoxInt32 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)

      case AtomicOp.BoxInt64 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)

      case AtomicOp.BoxChar =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)

      case AtomicOp.BoxFloat32 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false)

      case AtomicOp.BoxFloat64 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false)

      case AtomicOp.UnboxBool =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)

      case AtomicOp.UnboxInt8 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Character")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)

      case AtomicOp.UnboxInt16 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Short")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)

      case AtomicOp.UnboxInt32 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)

      case AtomicOp.UnboxInt64 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Long")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)

      case AtomicOp.UnboxChar =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Character")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)

      case AtomicOp.UnboxFloat32 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Float")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)

      case AtomicOp.UnboxFloat64 =>
        val List(exp) = exps
        addSourceLine(mv, loc)
        compileExpr(exp)
        mv.visitTypeInsn(CHECKCAST, "java/lang/Double")
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)


      case AtomicOp.HoleError(sym) =>
        addSourceLine(mv, loc)
        AsmOps.compileThrowHoleError(mv, sym.toString, loc)

      case AtomicOp.MatchError =>
        addSourceLine(mv, loc)
        AsmOps.compileThrowFlixError(mv, BackendObjType.MatchError.jvmName, loc)
    }

    case Expr.ApplyClo(exp, exps, ct, tpe, loc) =>
      ct match {
        case CallType.TailCall =>
          // Type of the function abstract class
          val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
          val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
          // Evaluating the closure
          compileExpr(exp)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
          // retrieving the unique thread object
          mv.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
          // Putting args on the Fn class
          for ((arg, i) <- exps.zipWithIndex) {
            // Duplicate the FunctionInterface
            mv.visitInsn(DUP)
            // Evaluating the expression
            compileExpr(arg)
            mv.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
              s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
          }
          // Return the closure
          mv.visitInsn(ARETURN)

        case CallType.NonTailCall =>
          // Type of the function abstract class
          val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
          val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
          // previous JvmOps functions are already partial pattern matches
          val MonoType.Arrow(_, closureResultType) = exp.tpe
          val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(closureResultType))

          compileExpr(exp)
          // Casting to JvmType of closure abstract class
          mv.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
          // retrieving the unique thread object
          mv.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
          // Putting args on the Fn class
          for ((arg, i) <- exps.zipWithIndex) {
            // Duplicate the FunctionInterface
            mv.visitInsn(DUP)
            // Evaluating the expression
            compileExpr(arg)
            mv.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
              s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
          }
          // Calling unwind and unboxing
          mv.visitMethodInsn(INVOKEVIRTUAL, functionInterface.name.toInternalName,
            backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
          AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))
      }

    case Expr.ApplyDef(sym, exps, ct, tpe, loc) => ct match {
      case CallType.TailCall =>
        // Type of the function
        val fnType = root.defs(sym).tpe
        // Type of the function abstract class
        val functionInterface = JvmOps.getFunctionInterfaceType(fnType)

        // Put the def on the stack
        AsmOps.compileDefSymbol(sym, mv)
        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
            s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
        }
        // Return the def
        mv.visitInsn(ARETURN)

      case CallType.NonTailCall =>
        // JvmType of Def
        val defJvmType = JvmOps.getFunctionDefinitionClassType(sym)
        // previous JvmOps function are already partial pattern matches
        val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(tpe))

        // Put the def on the stack
        AsmOps.compileDefSymbol(sym, mv)

        // Putting args on the Fn class
        for ((arg, i) <- exps.zipWithIndex) {
          // Duplicate the FunctionInterface
          mv.visitInsn(DUP)
          // Evaluating the expression
          compileExpr(arg)
          mv.visitFieldInsn(PUTFIELD, defJvmType.name.toInternalName,
            s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
        }
        // Calling unwind and unboxing
        mv.visitMethodInsn(INVOKEVIRTUAL, defJvmType.name.toInternalName, backendContinuationType.UnwindMethod.name,
          AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))
    }

    case Expr.ApplySelfTail(sym, formals, exps, tpe, loc) =>
      // The function abstract class name
      val functionType = JvmOps.getFunctionInterfaceType(root.defs(sym).tpe)
      // Evaluate each argument and put the result on the Fn class.
      for ((arg, i) <- exps.zipWithIndex) {
        mv.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpr(arg)
        mv.visitFieldInsn(PUTFIELD, functionType.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Jump to the entry point of the method.
      mv.visitJumpInsn(GOTO, ctx.entryPoint)

    case Expr.IfThenElse(exp1, exp2, exp3, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpr(exp1)
      mv.visitJumpInsn(IFEQ, ifElse)
      compileExpr(exp2)
      mv.visitJumpInsn(GOTO, ifEnd)
      mv.visitLabel(ifElse)
      compileExpr(exp3)
      mv.visitLabel(ifEnd)

    case Expr.Branch(exp, branches, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)
      // Calculating the updated jumpLabels map
      val updatedJumpLabels = branches.foldLeft(ctx.lenv)((map, branch) => map + (branch._1 -> new Label()))
      val ctx1 = ctx.copy(lenv = updatedJumpLabels)
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

    case Expr.JumpTo(sym, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)
      // Jumping to the label
      mv.visitJumpInsn(GOTO, ctx.lenv(sym))

    case Expr.Let(sym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)
      compileExpr(exp1)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      mv.visitVarInsn(iStore, sym.getStackOffset + 1)
      compileExpr(exp2)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(mv, loc)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      // JvmType of the closure
      val cloType = JvmOps.getClosureClassType(defSym)

      // Store temp recursive value
      mv.visitInsn(ACONST_NULL)
      mv.visitVarInsn(iStore, varSym.getStackOffset + 1)
      // Compile the closure
      compileExpr(exp1)
      // fix the local and closure reference
      mv.visitInsn(DUP)
      mv.visitInsn(DUP)
      mv.visitFieldInsn(PUTFIELD, cloType.name.toInternalName, s"clo$index", JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)
      // Store the closure locally (maybe not needed?)
      mv.visitVarInsn(iStore, varSym.getStackOffset + 1)
      compileExpr(exp2)

    case Expr.Scope(sym, exp, _, loc) =>
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

      // Emit try finally block.
      mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

      // Create an instance of Region
      mv.visitTypeInsn(NEW, BackendObjType.Region.jvmName.toInternalName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.Region.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(), JvmType.Void), false)

      val iStore = AsmOps.getStoreInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      mv.visitVarInsn(iStore, sym.getStackOffset + 1)

      // Compile the scope body
      mv.visitLabel(beforeTryBlock)
      compileExpr(exp)

      // When we exit the scope, call the region's `exit` method
      val iLoad = AsmOps.getLoadInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      mv.visitVarInsn(iLoad, sym.getStackOffset + 1)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ExitMethod.name,
        BackendObjType.Region.ExitMethod.d.toDescriptor, false)
      mv.visitLabel(afterTryBlock)

      // Compile the finally block which gets called if no exception is thrown
      mv.visitVarInsn(iLoad, sym.getStackOffset + 1)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitJumpInsn(GOTO, afterFinally)

      // Compile the finally block which gets called if an exception is thrown
      mv.visitLabel(finallyBlock)
      mv.visitVarInsn(iLoad, sym.getStackOffset + 1)
      mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      mv.visitInsn(ATHROW)
      mv.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, loc) =>
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

      // Emit a try catch block for each catch rule.
      for ((CatchRule(_, clazz, _), handlerLabel) <- rulesAndLabels) {
        mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
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
        mv.visitVarInsn(istore, sym.getStackOffset + 1)

        // Emit code for the handler body expression.
        compileExpr(body)
        mv.visitJumpInsn(GOTO, afterTryAndCatch)
      }

      // Add the label after both the try and catch rules.
      mv.visitLabel(afterTryAndCatch)

    case Expr.NewObject(name, _, tpe, methods, loc) =>
      addSourceLine(mv, loc)
      val className = JvmName(ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage, name).toInternalName
      mv.visitTypeInsn(NEW, className)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, className, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

      // For each method, compile the closure which implements the body of that method and store it in a field
      methods.zipWithIndex.foreach { case (m, i) =>
        mv.visitInsn(DUP)
        compileExpr(m.clo)
        mv.visitFieldInsn(PUTFIELD, className, s"clo$i", JvmOps.getClosureAbstractClassType(m.clo.tpe).toDescriptor)
      }

  }

  /**
    * Emits code for the given statement `stmt0` to the given method `visitor` in the `currentClass`.
    */
  def compileStmt(stmt0: Stmt)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = stmt0 match {
    case Stmt.Ret(e, _, _) => compileExpr(e)
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
  private def compileInt(i: Int)(implicit mv: MethodVisitor): Unit = i match {
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

  /*
   * Adding the source of the line for debugging
   */
  private def addSourceLine(visitor: MethodVisitor, loc: SourceLocation): Unit = {
    val label = new Label()
    visitor.visitLabel(label)
    visitor.visitLineNumber(loc.beginLine, label)
  }

  /**
    * Pushes arguments onto the stack ready to invoke a method
    */
  private def pushArgs(args: List[Expr], signature: Array[Class[_ <: Object]])(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = {
    // Evaluate arguments left-to-right and push them onto the stack.
    for ((arg, argType) <- args.zip(signature)) {
      compileExpr(arg)
      if (!argType.isPrimitive) {
        // NB: Really just a hack because the backend does not support array JVM types properly.
        mv.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
      } else {
        arg.tpe match {
          // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
          case MonoType.Array(MonoType.Float32) => mv.visitTypeInsn(CHECKCAST, "[F")
          case MonoType.Array(MonoType.Float64) => mv.visitTypeInsn(CHECKCAST, "[D")
          case MonoType.Array(MonoType.Int8) => mv.visitTypeInsn(CHECKCAST, "[B")
          case MonoType.Array(MonoType.Int16) => mv.visitTypeInsn(CHECKCAST, "[S")
          case MonoType.Array(MonoType.Int32) => mv.visitTypeInsn(CHECKCAST, "[I")
          case MonoType.Array(MonoType.Int64) => mv.visitTypeInsn(CHECKCAST, "[J")
          case _ => // nop
        }
      }
    }
  }
}
