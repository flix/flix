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

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpression(exp0: Expr, visitor: MethodVisitor, currentClass: JvmType.Reference, lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix): Unit = exp0 match {

    case Expr.Cst(cst, tpe, loc) =>
      compileConstant(visitor, cst, tpe, loc)

    case Expr.Var(sym, tpe, _) =>
      readVar(sym, tpe, visitor)

    case Expr.ApplyAtomic(op, exps, tpe, loc) => op match {

      case AtomicOp.Closure(sym) =>
        // JvmType of the closure
        val jvmType = JvmOps.getClosureClassType(sym)
        // new closure instance
        visitor.visitTypeInsn(NEW, jvmType.name.toInternalName)
        // Duplicate
        visitor.visitInsn(DUP)
        visitor.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
        // Capturing free args
        for ((arg, i) <- exps.zipWithIndex) {
          val erasedArgType = JvmOps.getErasedJvmType(arg.tpe)
          visitor.visitInsn(DUP)
          compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
          visitor.visitFieldInsn(PUTFIELD, jvmType.name.toInternalName, s"clo$i", erasedArgType.toDescriptor)
        }

      case AtomicOp.Unary(sop) =>
        val List(exp) = exps
        compileUnaryExpr(exp, currentClass, visitor, lenv0, entryPoint, sop)

      case AtomicOp.Binary(sop) =>
        val List(exp1, exp2) = exps
        sop match {
          case BoolOp.And =>
            val andFalseBranch = new Label()
            val andEnd = new Label()
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitJumpInsn(IFEQ, andFalseBranch)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitJumpInsn(IFEQ, andFalseBranch)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, andEnd)
            visitor.visitLabel(andFalseBranch)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(andEnd)

          case BoolOp.Or =>
            val orTrueBranch = new Label()
            val orFalseBranch = new Label()
            val orEnd = new Label()
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitJumpInsn(IFNE, orTrueBranch)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitJumpInsn(IFEQ, orFalseBranch)
            visitor.visitLabel(orTrueBranch)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, orEnd)
            visitor.visitLabel(orFalseBranch)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(orEnd)

          case Float32Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(F2D) // Sign extend to double
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(F2D) // Sign extend to double
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            visitor.visitInsn(D2F)

          case Float64Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)

          case Int8Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            visitor.visitInsn(D2I)
            visitor.visitInsn(I2B)

          case Int16Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            visitor.visitInsn(D2I)
            visitor.visitInsn(I2S)

          case Int32Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(I2D)
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            visitor.visitInsn(D2I)

          case Int64Op.Exp =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(L2D)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(L2D)
            visitor.visitMethodInsn(INVOKESTATIC, JvmName.Math.toInternalName, "pow",
              AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
            visitor.visitInsn(D2L)

          case Int8Op.And | Int16Op.And | Int32Op.And =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(IAND)

          case Int8Op.Or | Int16Op.Or | Int32Op.Or =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(IOR)

          case Int8Op.Xor | Int16Op.Xor | Int32Op.Xor =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(IXOR)

          case Int8Op.Shr | Int16Op.Shr | Int32Op.Shr =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(ISHR)

          case Int8Op.Shl =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(ISHL)
            visitor.visitInsn(I2B) // Sign extend to make left most bit appear in the sign bit

          case Int16Op.Shl =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(ISHL)
            visitor.visitInsn(I2S) // Sign extend to make left most bit appear in the sign bit

          case Int32Op.Shl =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(ISHL)

          case Int64Op.And =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(LAND)

          case Int64Op.Or =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(LOR)

          case Int64Op.Xor =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(LXOR)

          case Int64Op.Shr =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(LSHR)

          case Int64Op.Shl =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitInsn(LSHL)

          case BigIntOp.And =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
              "and", AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(exp2.tpe)), JvmType.BigInteger), false)

          case BigIntOp.Or =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
              "or", AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(exp2.tpe)), JvmType.BigInteger), false)

          case BigIntOp.Xor =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
              "xor", AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(exp2.tpe)), JvmType.BigInteger), false)

          case BigIntOp.Shl =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
              "shiftLeft", AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(exp2.tpe)), JvmType.BigInteger), false)

          case BigIntOp.Shr =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
              "shiftRight", AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(exp2.tpe)), JvmType.BigInteger), false)

          case BoolOp.Eq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPNE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BoolOp.Neq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPEQ, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Lt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPGE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Le =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPGT, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Eq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPNE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Neq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPEQ, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Ge =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPLT, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case CharOp.Gt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitJumpInsn(IF_ICMPLE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Lt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFGE)
            visitor.visitJumpInsn(FCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Le =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFGT)
            visitor.visitJumpInsn(FCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Eq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFNE)
            visitor.visitJumpInsn(FCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Neq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFEQ)
            visitor.visitJumpInsn(FCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Ge =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFLT)
            visitor.visitJumpInsn(FCMPL, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float32Op.Gt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFLE)
            visitor.visitJumpInsn(FCMPL, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Lt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFGE)
            visitor.visitJumpInsn(DCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Le =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFGT)
            visitor.visitJumpInsn(DCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Eq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFNE)
            visitor.visitJumpInsn(DCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Neq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFEQ)
            visitor.visitJumpInsn(DCMPG, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Ge =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFLT)
            visitor.visitJumpInsn(DCMPL, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Float64Op.Gt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitInsn(IFLE)
            visitor.visitJumpInsn(DCMPL, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Lt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPGE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Le =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPGT, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Eq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPNE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Neq =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPEQ, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Ge =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPLT, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case BigDecimalOp.Gt =>
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            val condElse = new Label()
            val condEnd = new Label()
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
              AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(IF_ICMPLE, condElse)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(GOTO, condEnd)
            visitor.visitLabel(condElse)
            visitor.visitInsn(ICONST_0)
            visitor.visitLabel(condEnd)

          case Int8Op.Lt => ???
          case Int8Op.Le => ???
          case Int8Op.Eq => ???
          case Int8Op.Neq => ???
          case Int8Op.Ge => ???
          case Int8Op.Gt => ???

          case Int16Op.Lt => ???
          case Int16Op.Le => ???
          case Int16Op.Eq => ???
          case Int16Op.Neq => ???
          case Int16Op.Ge => ???
          case Int16Op.Gt => ???

          case Int32Op.Lt => ???
          case Int32Op.Le => ???
          case Int32Op.Eq => ???
          case Int32Op.Neq => ???
          case Int32Op.Ge => ???
          case Int32Op.Gt => ???

          case Int64Op.Lt => ???
          case Int64Op.Le => ???
          case Int64Op.Eq => ???
          case Int64Op.Neq => ???
          case Int64Op.Ge => ???
          case Int64Op.Gt => ???

          case BigIntOp.Lt => ???
          case BigIntOp.Le => ???
          case BigIntOp.Eq => ???
          case BigIntOp.Neq => ???
          case BigIntOp.Ge => ???
          case BigIntOp.Gt => ???

          case StringOp.Eq => ???
          case StringOp.Neq => ???

          /*

                  private def semanticOperatorCopmarisonToOpcode(sop: SemanticOperator): Option[(Int, Int)] = sop match {
                    case BoolOp.Eq => Some(IF_ICMPNE, IFNE)
                    case BoolOp.Neq => Some(IF_ICMPEQ, IFEQ)
                    case Float32Op.Eq => Some(FCMPG, IFNE)
                    case Float32Op.Neq => Some(FCMPG, IFEQ)
                    case Float32Op.Lt => Some(FCMPG, IFGE)
                    case Float32Op.Le => Some(FCMPG, IFGT)
                    case Float32Op.Gt => Some(FCMPL, IFLE)
                    case Float32Op.Ge => Some(FCMPL, IFLT)
                    case Float64Op.Eq => Some(DCMPG, IFNE)
                    case Float64Op.Neq => Some(DCMPG, IFEQ)
                    case Float64Op.Lt => Some(DCMPG, IFGE)
                    case Float64Op.Le => Some(DCMPG, IFGT)
                    case Float64Op.Gt => Some(DCMPL, IFLE)
                    case Float64Op.Ge => Some(DCMPL, IFLT)
                    case CharOp.Eq | Int8Op.Eq | Int16Op.Eq | Int32Op.Eq
                         | BigDecimalOp.Eq | BigIntOp.Eq | StringOp.Eq => Some(IF_ICMPNE, IFNE)
                    case CharOp.Neq | Int8Op.Neq | Int16Op.Neq | Int32Op.Neq
                         | BigDecimalOp.Neq | BigIntOp.Neq | StringOp.Neq => Some(IF_ICMPEQ, IFEQ)
                    case CharOp.Lt | Int8Op.Lt | Int16Op.Lt | Int32Op.Lt
                         | BigDecimalOp.Lt | BigIntOp.Lt => Some(IF_ICMPGE, IFGE)
                    case CharOp.Le | Int8Op.Le | Int16Op.Le | Int32Op.Le
                         | BigDecimalOp.Le | BigIntOp.Le => Some(IF_ICMPGT, IFGT)
                    case CharOp.Gt | Int8Op.Gt | Int16Op.Gt | Int32Op.Gt
                         | BigDecimalOp.Gt | BigIntOp.Gt => Some(IF_ICMPLE, IFLE)
                    case CharOp.Ge | Int8Op.Ge | Int16Op.Ge | Int32Op.Ge
                         | BigDecimalOp.Ge | BigIntOp.Ge => Some(IF_ICMPLT, IFLT)
                    case Int64Op.Eq => Some(LCMP, IFNE)
                    case Int64Op.Neq => Some(LCMP, IFEQ)
                    case Int64Op.Lt => Some(LCMP, IFGE)
                    case Int64Op.Le => Some(LCMP, IFGT)
                    case Int64Op.Gt => Some(LCMP, IFLE)
                    case Int64Op.Ge => Some(LCMP, IFLT)

                    case _ => None
                  }

          compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
          compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
          val condElse = new Label()
          val condEnd = new Label()
          semanticOperatorCopmarisonToOpcode(sop) match {
            case Some((op, cmp)) => sop match {
              case StringOp.Eq | StringOp.Neq =>
                // String can be compared using Object's `equal` method
                visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.JavaObject.jvmName.toInternalName, "equals",
                  AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
                visitor.visitInsn(ICONST_1)
                visitor.visitJumpInsn(op, condElse)

              case BoolOp.Eq | BoolOp.Neq =>
                // Bool can be (value) compared for equality.
                visitor.visitJumpInsn(op, condElse)

              case Float32Op.Lt | Float32Op.Le | Float32Op.Gt | Float32Op.Ge | Float32Op.Eq | Float32Op.Neq
                   | Float64Op.Lt | Float64Op.Le | Float64Op.Gt | Float64Op.Ge | Float64Op.Eq | Float64Op.Neq =>
                visitor.visitInsn(op)
                visitor.visitJumpInsn(cmp, condElse)

              case BigDecimalOp.Lt | BigDecimalOp.Le | BigDecimalOp.Gt | BigDecimalOp.Ge | BigDecimalOp.Eq | BigDecimalOp.Neq =>
                visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "compareTo",
                  AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.PrimInt), false)
                visitor.visitInsn(ICONST_0)
                visitor.visitJumpInsn(op, condElse)

              case CharOp.Lt | CharOp.Le | CharOp.Gt | CharOp.Ge | CharOp.Eq | CharOp.Neq
                   | Int8Op.Lt | Int8Op.Le | Int8Op.Gt | Int8Op.Ge | Int8Op.Eq | Int8Op.Neq
                   | Int16Op.Lt | Int16Op.Le | Int16Op.Gt | Int16Op.Ge | Int16Op.Eq | Int16Op.Neq
                   | Int32Op.Lt | Int32Op.Le | Int32Op.Gt | Int32Op.Ge | Int32Op.Eq | Int32Op.Neq => visitor.visitJumpInsn(op, condElse)

              case Int64Op.Lt | Int64Op.Le | Int64Op.Gt | Int64Op.Ge | Int64Op.Eq | Int64Op.Neq =>
                visitor.visitInsn(LCMP)
                visitor.visitJumpInsn(cmp, condElse)

              case BigIntOp.Lt | BigIntOp.Le | BigIntOp.Gt | BigIntOp.Ge | BigIntOp.Eq | BigIntOp.Neq =>
                visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "compareTo",
                  AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.PrimInt), false)
                visitor.visitInsn(ICONST_0)
                visitor.visitJumpInsn(op, condElse)

              case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", e1.loc)
            }
            case None => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", e1.loc)
          }
          visitor.visitInsn(ICONST_1)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitLabel(condEnd)
          }

           */

          case _ => compileBinaryExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, sop)

        }

      case AtomicOp.Region =>
        //!TODO: For now, just emit unit
        compileConstant(visitor, Ast.Constant.Unit, MonoType.Unit, loc)

      case AtomicOp.ScopeExit =>
        val List(exp1, exp2) = exps

        // Compile the expression, putting a function implementing the Runnable interface on the stack
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

        // Compile the expression representing the region
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)

        // Call the Region's `runOnExit` method
        visitor.visitInsn(SWAP)
        visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.RunOnExitMethod.name, BackendObjType.Region.RunOnExitMethod.d.toDescriptor, false)

        // Put a Unit value on the stack
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.Is(sym) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the `TagInfo` for the tag
        val tagInfo = JvmOps.getTagInfo(exp.tpe, sym.name)
        // We get the JvmType of the class for tag
        val classType = JvmOps.getTagClassType(tagInfo)

        // First we compile the `exp`
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // We check if the enum is `instanceof` the class
        visitor.visitTypeInsn(INSTANCEOF, classType.name.toInternalName)

      // Normal Tag
      case AtomicOp.Tag(sym) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // Get the tag info.
        val tagInfo = JvmOps.getTagInfo(tpe, sym.name)
        // We get the JvmType of the class for tag
        val classType = JvmOps.getTagClassType(tagInfo)

        ///
        /// Special Case: A tag with a single argument: The unit argument.
        ///
        // TODO: This is a hack until the new and improved backend arrives.
        val whitelistedEnums = List(
          Symbol.mkEnumSym("Comparison"),
          Symbol.mkEnumSym("RedBlackTree.RedBlackTree"),
          Symbol.mkEnumSym("RedBlackTree.Color"),
        )
        if (exp.tpe == MonoType.Unit && whitelistedEnums.contains(sym.enumSym)) {
          // TODO: This is could introduce errors by if exp has side effects
          // Read the "unitInstance" field of the appropriate class.
          val declaration = classType.name.toInternalName
          val descriptor = classType.toDescriptor
          visitor.visitFieldInsn(GETSTATIC, declaration, "unitInstance", descriptor)
        } else {
          /*
         If the definition of the enum case has a `Unit` field, then it is represented by singleton pattern which means
         there is only one instance of the class initiated as a field. We have to fetch this field instead of instantiating
         a new one.
         */
          // Creating a new instance of the class
          visitor.visitTypeInsn(NEW, classType.name.toInternalName)
          visitor.visitInsn(DUP)
          // Evaluating the single argument of the class constructor
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
          // Descriptor of the constructor
          val constructorDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedJvmType(tagInfo.tagType)), JvmType.Void)
          // Calling the constructor of the class
          visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)
        }

      case AtomicOp.Untag(sym) =>
        val List(exp) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)

        // We get the `TagInfo` for the tag
        val tagInfo = JvmOps.getTagInfo(exp.tpe, sym.name)
        // We get the JvmType of the class for the tag
        val classType = JvmOps.getTagClassType(tagInfo)
        // Evaluate the exp
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // Cast the exp to the type of the tag
        visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
        // Descriptor of the method
        val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tagInfo.tagType))
        // Invoke `getValue()` method to extract the field of the tag
        visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
        // Cast the object to it's type if it's not a primitive
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.Index(idx) =>
        val List(exp) = exps
        // We get the JvmType of the class for the tuple
        val classType = JvmOps.getTupleClassType(exp.tpe.asInstanceOf[MonoType.Tuple])
        // evaluating the `base`
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // Retrieving the field `field${offset}`
        visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"field$idx", JvmOps.getErasedJvmType(tpe).toDescriptor)
        // Cast the object to it's type if it's not a primitive
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.Tuple =>
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the JvmType of the class for the tuple
        val classType = JvmOps.getTupleClassType(tpe.asInstanceOf[MonoType.Tuple])
        // Instantiating a new object of tuple
        visitor.visitTypeInsn(NEW, classType.name.toInternalName)
        // Duplicating the class
        visitor.visitInsn(DUP)
        // Evaluating all the elements to be stored in the tuple class
        exps.foreach(compileExpression(_, visitor, currentClass, lenv0, entryPoint))
        // Erased type of `elms`
        val erasedElmTypes = exps.map(_.tpe).map(JvmOps.getErasedJvmType)
        // Descriptor of constructor
        val constructorDescriptor = AsmOps.getMethodDescriptor(erasedElmTypes, JvmType.Void)
        // Invoking the constructor
        visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

      case AtomicOp.RecordEmpty =>
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the JvmType of the class for the RecordEmpty
        val classType = JvmOps.getRecordEmptyClassType()
        // Instantiating a new object of tuple
        visitor.visitFieldInsn(GETSTATIC, classType.name.toInternalName, BackendObjType.RecordEmpty.InstanceField.name, classType.toDescriptor)

      case AtomicOp.RecordSelect(field) =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(visitor, loc)

        // Get the correct record extend class, given the expression type 'tpe'
        // We get the JvmType of the extended record class to retrieve the proper field
        val classType = JvmOps.getRecordType(tpe)

        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(tpe), BackendObjType.RecordEmpty.toTpe)

        //Compile the expression exp (which should be a record), as we need to have on the stack a record in order to call
        //lookupField
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

        //Push the desired label of the field we want get of the record onto the stack
        visitor.visitLdcInsn(field.name)

        //Invoke the lookupField method on the record. (To get the proper record object)
        visitor.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, "lookupField",
          AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)

        //Cast to proper record extend class
        visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)

        //Retrieve the value field  (To get the proper value)
        visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)

        // Cast the field value to the expected type.
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.RecordExtend(field) =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the JvmType of the class for the record extend
        val classType = JvmOps.getRecordExtendClassType(tpe)

        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        // previous functions are already partial matches
        val MonoType.RecordExtend(_, recordValueType, _) = tpe
        val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(recordValueType), BackendObjType.RecordEmpty.toTpe)

        // Instantiating a new object of tuple
        visitor.visitTypeInsn(NEW, classType.name.toInternalName)
        visitor.visitInsn(DUP)
        // Invoking the constructor
        visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)

        //Put the label of field (which is going to be the extension).
        visitor.visitInsn(DUP)
        visitor.visitLdcInsn(field.name)
        visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.LabelField.name, BackendObjType.String.toDescriptor)

        //Put the value of the field onto the stack, since it is an expression we first need to compile it.
        visitor.visitInsn(DUP)
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)

        //Put the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        visitor.visitInsn(DUP)
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.RestField.name, interfaceType.toDescriptor)

      case AtomicOp.RecordRestrict(field) =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the JvmType of the record interface
        val interfaceType = JvmOps.getRecordInterfaceType()

        //Push the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        //Push the label of field (which is going to be the removed/restricted).
        visitor.visitLdcInsn(field.name)

        // Invoking the restrictField method
        visitor.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, BackendObjType.Record.RestrictFieldMethod.name,
          AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)

      case AtomicOp.ArrayLit =>
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We push the 'length' of the array on top of stack
        compileInt(visitor, exps.length, isLong = false)
        // We get the inner type of the array
        val jvmType = JvmOps.getJvmType(tpe.asInstanceOf[MonoType.Array].tpe)
        // Instantiating a new array of type jvmType
        jvmType match {
          case ref: JvmType.Reference => // Happens if the inner type is an object type
            visitor.visitTypeInsn(ANEWARRAY, ref.name.toInternalName)
          case _ => // Happens if the inner type is a primitive type
            visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
        }
        // For each element we generate code to store it into the array
        for (i <- exps.indices) {
          // Duplicates the 'array reference'
          visitor.visitInsn(DUP)
          // We push the 'index' of the current element on top of stack
          compileInt(visitor, i, isLong = false)
          // Evaluating the 'element' to be stored
          compileExpression(exps(i), visitor, currentClass, lenv0, entryPoint)
          // Stores the 'element' at the given 'index' in the 'array'
          // with the store instruction corresponding to the stored element
          visitor.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
        }

      case AtomicOp.ArrayNew =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the inner type of the array
        val elmType = tpe.asInstanceOf[MonoType.Array].tpe
        // We get the erased elm type.
        val jvmType = JvmOps.getErasedJvmType(elmType)
        // Evaluating the value of the 'default element'
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        // Evaluating the 'length' of the array
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        // Instantiating a new array of type jvmType
        if (elmType == MonoType.Str) {
          visitor.visitTypeInsn(ANEWARRAY, "java/lang/String")
        } else if (elmType.isInstanceOf[MonoType.Native]) {
          val native = elmType.asInstanceOf[MonoType.Native]
          val name = native.clazz.getName.replace('.', '/')
          visitor.visitTypeInsn(ANEWARRAY, name)
        } else if (jvmType == JvmType.Object) { // Happens if the inner type is an object type
          visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
        } else { // Happens if the inner type is a primitive type
          visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
        }
        if (jvmType == JvmType.PrimLong || jvmType == JvmType.PrimDouble) { // Happens if the inner type is Int64 or Float64
          // Duplicates the 'array reference' three places down the stack
          visitor.visitInsn(DUP_X2)
          // Duplicates the 'array reference' three places down the stack
          visitor.visitInsn(DUP_X2)
          // Pops the 'ArrayRef' at the top of the stack
          visitor.visitInsn(POP)
        } else {
          // Duplicates the 'array reference' two places down the stack
          visitor.visitInsn(DUP_X1)
          // Swaps the 'array reference' and 'default element'
          visitor.visitInsn(SWAP)
        }
        // We get the array fill type
        val arrayFillType = AsmOps.getArrayFillType(jvmType)
        // Invoking the method to fill the array with the default element
        visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Arrays", "fill", arrayFillType, false);

      case AtomicOp.ArrayLoad =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the jvmType of the element to be loaded
        val jvmType = JvmOps.getErasedJvmType(tpe)
        // Evaluating the 'base'
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        // Cast the object to Array
        visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
        // Evaluating the 'index' to load from
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        // Loads the 'element' at the given 'index' from the 'array'
        // with the load instruction corresponding to the loaded element
        visitor.visitInsn(AsmOps.getArrayLoadInstruction(jvmType))

      case AtomicOp.ArrayStore => exps match {
        case List(exp1, exp2, exp3) =>
          // Adding source line number for debugging
          addSourceLine(visitor, loc)
          // We get the jvmType of the element to be stored
          val jvmType = JvmOps.getErasedJvmType(exp3.tpe)
          // Evaluating the 'base'
          compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
          // Cast the object to Array
          visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
          // Evaluating the 'index' to be stored in
          compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
          // Evaluating the 'element' to be stored
          compileExpression(exp3, visitor, currentClass, lenv0, entryPoint)
          // Stores the 'element' at the given 'index' in the 'array'
          // with the store instruction corresponding to the stored element
          visitor.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
          // Since the return type is 'unit', we put an instance of 'unit' on top of the stack
          visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        case _ => throw InternalCompilerException("Mismatched Arity", loc)
      }

      case AtomicOp.ArrayLength =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // We get the inner type of the array
        val jvmType = JvmOps.getErasedJvmType(exp.tpe.asInstanceOf[MonoType.Array].tpe)
        // Evaluating the 'base'
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // Cast the object to array
        visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
        // Pushes the 'length' of the array on top of stack
        visitor.visitInsn(ARRAYLENGTH)

      case AtomicOp.Ref =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Create a new reference object
        visitor.visitTypeInsn(NEW, classType.name.toInternalName)
        // Duplicate it since one instance will get consumed by constructor
        visitor.visitInsn(DUP)
        // Call the constructor
        visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
        // Duplicate it since one instance will get consumed by putfield
        visitor.visitInsn(DUP)
        // Evaluate the underlying expression
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // Erased type of the value of the reference
        val valueErasedType = JvmOps.getErasedJvmType(tpe.asInstanceOf[MonoType.Ref].tpe)
        // set the field with the ref value
        visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, valueErasedType.toDescriptor)

      case AtomicOp.Deref =>
        val List(exp) = exps
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // Evaluate the exp
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(exp.tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = exp.tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Cast the ref
        visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
        // Dereference the expression
        visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)
        // Cast underlying value to the correct type if the underlying type is Object
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.Assign =>
        val List(exp1, exp2) = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        // Evaluate the reference address
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        // Evaluating the value to be assigned to the reference
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        // JvmType of the reference class
        val classType = JvmOps.getRefClassType(exp1.tpe)

        // the previous function is already partial
        val MonoType.Ref(refValueType) = exp1.tpe
        val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

        // Invoke `setValue` method to set the value to the given number
        visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
        // Since the return type is unit, we put an instance of unit on top of the stack
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.InstanceOf(clazz) =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        val className = asm.Type.getInternalName(clazz)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(INSTANCEOF, className.toString)

      case AtomicOp.Cast =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.InvokeConstructor(constructor) =>
        // Adding source line number for debugging
        addSourceLine(visitor, loc)
        val descriptor = asm.Type.getConstructorDescriptor(constructor)
        val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
        // Create a new object of the declaration type
        visitor.visitTypeInsn(NEW, declaration)
        // Duplicate the reference since the first argument for a constructor call is the reference to the object
        visitor.visitInsn(DUP)
        // Retrieve the signature.
        val signature = constructor.getParameterTypes

        pushArgs(visitor, exps, signature, currentClass, lenv0, entryPoint)

        // Call the constructor
        visitor.visitMethodInsn(INVOKESPECIAL, declaration, "<init>", descriptor, false)

      case AtomicOp.InvokeMethod(method) =>
        val exp :: args = exps

        // Adding source line number for debugging
        addSourceLine(visitor, loc)

        // Evaluate the receiver object.
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        val thisType = asm.Type.getInternalName(method.getDeclaringClass)
        visitor.visitTypeInsn(CHECKCAST, thisType)

        // Retrieve the signature.
        val signature = method.getParameterTypes

        pushArgs(visitor, args, signature, currentClass, lenv0, entryPoint)

        val declaration = asm.Type.getInternalName(method.getDeclaringClass)
        val name = method.getName
        val descriptor = asm.Type.getMethodDescriptor(method)

        // Check if we are invoking an interface or class.
        if (method.getDeclaringClass.isInterface) {
          visitor.visitMethodInsn(INVOKEINTERFACE, declaration, name, descriptor, true)
        } else {
          visitor.visitMethodInsn(INVOKEVIRTUAL, declaration, name, descriptor, false)
        }

        // If the method is void, put a unit on top of the stack
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
          visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.InvokeStaticMethod(method) =>
        addSourceLine(visitor, loc)
        val signature = method.getParameterTypes
        pushArgs(visitor, exps, signature, currentClass, lenv0, entryPoint)
        val declaration = asm.Type.getInternalName(method.getDeclaringClass)
        val name = method.getName
        val descriptor = asm.Type.getMethodDescriptor(method)
        // Check if we are invoking an interface or class.
        if (method.getDeclaringClass.isInterface) {
          visitor.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, true)
        } else {
          visitor.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, false)
        }
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
          visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
        }

      case AtomicOp.GetField(field) =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        visitor.visitFieldInsn(GETFIELD, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
        compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        visitor.visitFieldInsn(PUTFIELD, declaration, field.getName, JvmOps.getJvmType(exp2.tpe).toDescriptor)

        // Push Unit on the stack.
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.GetStaticField(field) =>
        addSourceLine(visitor, loc)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        visitor.visitFieldInsn(GETSTATIC, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        visitor.visitFieldInsn(PUTSTATIC, declaration, field.getName, JvmOps.getJvmType(exp.tpe).toDescriptor)

        // Push Unit on the stack.
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)


      case AtomicOp.Spawn =>
        val List(exp1, exp2) = exps

        addSourceLine(visitor, loc)

        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.ApplyAtomic(AtomicOp.Region, _, tpe, loc) =>

            // Compile the expression, putting a function implementing the Runnable interface on the stack
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

            // make a thread and run it
            if (flix.options.xvirtualthreads) {
              visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "startVirtualThread", s"(${JvmName.Runnable.toDescriptor})${JvmName.Thread.toDescriptor}", false)
              visitor.visitInsn(POP)
            } else {
              visitor.visitTypeInsn(NEW, "java/lang/Thread")
              visitor.visitInsn(DUP_X1)
              visitor.visitInsn(SWAP)
              visitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Thread", "<init>", s"(${JvmName.Runnable.toDescriptor})${JvmType.Void.toDescriptor}", false)
              visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Thread", "start", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
            }

          case _ =>
            // Compile the expression representing the region
            compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
            visitor.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)

            // Compile the expression, putting a function implementing the Runnable interface on the stack
            compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
            visitor.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

            // Call the Region's `spawn` method
            visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.SpawnMethod.name, BackendObjType.Region.SpawnMethod.d.toDescriptor, false)
        }

        // Put a Unit value on the stack
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)


      case AtomicOp.Lazy =>
        val List(exp) = exps
        // Add source line numbers for debugging.
        addSourceLine(visitor, loc)

        // Find the Lazy class name (Lazy$tpe).
        val classType = JvmOps.getLazyClassType(tpe.asInstanceOf[MonoType.Lazy]).name.toInternalName

        // Make a new lazy object and dup it to leave it on the stack.
        visitor.visitTypeInsn(NEW, classType)
        visitor.visitInsn(DUP)

        // Compile the thunked expression and call new Lazy$erased_tpe(expression).
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESPECIAL, classType, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), false)

      case AtomicOp.Force =>
        val List(exp) = exps
        // Add source line numbers for debugging.
        addSourceLine(visitor, loc)

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val classMonoType = exp.tpe.asInstanceOf[MonoType.Lazy]
        val classType = JvmOps.getLazyClassType(classMonoType)
        val internalClassType = classType.name.toInternalName
        val MonoType.Lazy(tpe) = classMonoType
        val erasedType = JvmOps.getErasedJvmType(tpe)

        // Emit code for the lazy expression.
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

        // Lazy$tpe is expected.
        visitor.visitTypeInsn(CHECKCAST, internalClassType)

        // Dup for later lazy.value or lazy.force(context)
        visitor.visitInsn(DUP)
        // Get expression
        visitor.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
        val alreadyInit = new Label()
        val end = new Label()
        // If expression == null the we just use lazy.value, otherwise lazy.force(context)
        visitor.visitJumpInsn(IFNULL, alreadyInit)

        // Call force().
        visitor.visitMethodInsn(INVOKEVIRTUAL, internalClassType, "force", AsmOps.getMethodDescriptor(Nil, erasedType), false)
        // goto the cast to undo erasure
        visitor.visitJumpInsn(GOTO, end)

        visitor.visitLabel(alreadyInit)
        // Retrieve the erased value
        visitor.visitFieldInsn(GETFIELD, internalClassType, "value", erasedType.toDescriptor)

        visitor.visitLabel(end)
        // The result of force is a generic object so a cast is needed.
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

      case AtomicOp.BoxBool =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)

      case AtomicOp.BoxInt8 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)

      case AtomicOp.BoxInt16 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)

      case AtomicOp.BoxInt32 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)

      case AtomicOp.BoxInt64 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)

      case AtomicOp.BoxChar =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)

      case AtomicOp.BoxFloat32 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false)

      case AtomicOp.BoxFloat64 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false)

      case AtomicOp.UnboxBool =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)

      case AtomicOp.UnboxInt8 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)

      case AtomicOp.UnboxInt16 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Short")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)

      case AtomicOp.UnboxInt32 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Integer")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)

      case AtomicOp.UnboxInt64 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Long")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)

      case AtomicOp.UnboxChar =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)

      case AtomicOp.UnboxFloat32 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Float")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)

      case AtomicOp.UnboxFloat64 =>
        val List(exp) = exps
        addSourceLine(visitor, loc)
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        visitor.visitTypeInsn(CHECKCAST, "java/lang/Double")
        visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)


      case AtomicOp.HoleError(sym) =>
        addSourceLine(visitor, loc)
        AsmOps.compileThrowHoleError(visitor, sym.toString, loc)

      case AtomicOp.MatchError =>
        addSourceLine(visitor, loc)
        AsmOps.compileThrowFlixError(visitor, BackendObjType.MatchError.jvmName, loc)
    }

    case Expr.ApplyClo(exp, exps, tpe, loc) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
      // previous JvmOps functions are already partial pattern matches
      val MonoType.Arrow(_, closureResultType) = exp.tpe
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(closureResultType))

      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of closure abstract class
      visitor.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
      // retrieving the unique thread object
      visitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
      // Putting args on the Fn class
      for ((arg, i) <- exps.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Calling unwind and unboxing
      visitor.visitMethodInsn(INVOKEVIRTUAL, functionInterface.name.toInternalName,
        backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expr.ApplyCloTail(exp, exps, tpe, loc) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
      // Evaluating the closure
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of closure abstract class
      visitor.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
      // retrieving the unique thread object
      visitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
      // Putting args on the Fn class
      for ((arg, i) <- exps.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Return the closure
      visitor.visitInsn(ARETURN)

    case Expr.ApplyDef(sym, exps, tpe, loc) =>
      // JvmType of Def
      val defJvmType = JvmOps.getFunctionDefinitionClassType(sym)
      // previous JvmOps function are already partial pattern matches
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(tpe))

      // Put the def on the stack
      AsmOps.compileDefSymbol(sym, visitor)

      // Putting args on the Fn class
      for ((arg, i) <- exps.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, defJvmType.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Calling unwind and unboxing
      visitor.visitMethodInsn(INVOKEVIRTUAL, defJvmType.name.toInternalName, backendContinuationType.UnwindMethod.name,
        AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expr.ApplyDefTail(sym, exps, tpe, loc) =>
      // Type of the function
      val fnType = root.defs(sym).tpe
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(fnType)

      // Put the def on the stack
      AsmOps.compileDefSymbol(sym, visitor)
      // Putting args on the Fn class
      for ((arg, i) <- exps.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Return the def
      visitor.visitInsn(ARETURN)

    case Expr.ApplySelfTail(sym, formals, exps, tpe, loc) =>
      // The function abstract class name
      val functionType = JvmOps.getFunctionInterfaceType(root.defs(sym).tpe)
      // Evaluate each argument and put the result on the Fn class.
      for ((arg, i) <- exps.zipWithIndex) {
        visitor.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionType.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Jump to the entry point of the method.
      visitor.visitJumpInsn(GOTO, entryPoint)

    case Expr.IfThenElse(exp1, exp2, exp3, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(exp3, visitor, currentClass, lenv0, entryPoint)
      visitor.visitLabel(ifEnd)

    case Expr.Branch(exp, branches, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Calculating the updated jumpLabels map
      val updatedJumpLabels = branches.foldLeft(lenv0)((map, branch) => map + (branch._1 -> new Label()))
      // Compiling the exp
      compileExpression(exp, visitor, currentClass, updatedJumpLabels, entryPoint)
      // Label for the end of all branches
      val endLabel = new Label()
      // Skip branches if `exp` does not jump
      visitor.visitJumpInsn(GOTO, endLabel)
      // Compiling branches
      branches.foreach { case (sym, branchExp) =>
        // Label for the start of the branch
        visitor.visitLabel(updatedJumpLabels(sym))
        // evaluating the expression for the branch
        compileExpression(branchExp, visitor, currentClass, updatedJumpLabels, entryPoint)
        // Skip the rest of the branches
        visitor.visitJumpInsn(GOTO, endLabel)
      }
      // label for the end of branches
      visitor.visitLabel(endLabel)

    case Expr.JumpTo(sym, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Jumping to the label
      visitor.visitJumpInsn(GOTO, lenv0(sym))

    case Expr.Let(sym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      visitor.visitVarInsn(iStore, sym.getStackOffset + 1)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      // JvmType of the closure
      val cloType = JvmOps.getClosureClassType(defSym)

      // Store temp recursive value
      visitor.visitInsn(ACONST_NULL)
      visitor.visitVarInsn(iStore, varSym.getStackOffset + 1)
      // Compile the closure
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // fix the local and closure reference
      visitor.visitInsn(DUP)
      visitor.visitInsn(DUP)
      visitor.visitFieldInsn(PUTFIELD, cloType.name.toInternalName, s"clo$index", JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)
      // Store the closure locally (maybe not needed?)
      visitor.visitVarInsn(iStore, varSym.getStackOffset + 1)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)

    case Expr.Scope(sym, exp, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      // Introduce a label for before the try block.
      val beforeTryBlock = new Label()

      // Introduce a label for after the try block.
      val afterTryBlock = new Label()

      // Introduce a label for the finally block.
      val finallyBlock = new Label()

      // Introduce a label after the finally block.
      val afterFinally = new Label()

      // Emit try finally block.
      visitor.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

      // Create an instance of Region
      visitor.visitTypeInsn(NEW, BackendObjType.Region.jvmName.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitMethodInsn(INVOKESPECIAL, BackendObjType.Region.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(), JvmType.Void), false)

      val iStore = AsmOps.getStoreInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      visitor.visitVarInsn(iStore, sym.getStackOffset + 1)

      // Compile the scope body
      visitor.visitLabel(beforeTryBlock)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

      // When we exit the scope, call the region's `exit` method
      val iLoad = AsmOps.getLoadInstruction(JvmType.Reference(BackendObjType.Region.jvmName))
      visitor.visitVarInsn(iLoad, sym.getStackOffset + 1)
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ExitMethod.name,
        BackendObjType.Region.ExitMethod.d.toDescriptor, false)
      visitor.visitLabel(afterTryBlock)

      // Compile the finally block which gets called if no exception is thrown
      visitor.visitVarInsn(iLoad, sym.getStackOffset + 1)
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      visitor.visitJumpInsn(GOTO, afterFinally)

      // Compile the finally block which gets called if an exception is thrown
      visitor.visitLabel(finallyBlock)
      visitor.visitVarInsn(iLoad, sym.getStackOffset + 1)
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.Region.jvmName.toInternalName, BackendObjType.Region.ReThrowChildExceptionMethod.name,
        BackendObjType.Region.ReThrowChildExceptionMethod.d.toDescriptor, false)
      visitor.visitInsn(ATHROW)
      visitor.visitLabel(afterFinally)

    case Expr.TryCatch(exp, rules, _, loc) =>
      // Add source line number for debugging.
      addSourceLine(visitor, loc)

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
        visitor.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
      }

      // Emit code for the try block.
      visitor.visitLabel(beforeTryBlock)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitLabel(afterTryBlock)
      visitor.visitJumpInsn(GOTO, afterTryAndCatch)

      // Emit code for each catch rule.
      for ((CatchRule(sym, _, body), handlerLabel) <- rulesAndLabels) {
        // Emit the label.
        visitor.visitLabel(handlerLabel)

        // Store the exception in a local variable.
        val istore = AsmOps.getStoreInstruction(JvmType.Object)
        visitor.visitVarInsn(istore, sym.getStackOffset + 1)

        // Emit code for the handler body expression.
        compileExpression(body, visitor, currentClass, lenv0, entryPoint)
        visitor.visitJumpInsn(GOTO, afterTryAndCatch)
      }

      // Add the label after both the try and catch rules.
      visitor.visitLabel(afterTryAndCatch)

    case Expr.NewObject(name, _, tpe, methods, loc) =>
      addSourceLine(visitor, loc)
      val className = JvmName(ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage, name).toInternalName
      visitor.visitTypeInsn(NEW, className)
      visitor.visitInsn(DUP)
      visitor.visitMethodInsn(INVOKESPECIAL, className, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

      // For each method, compile the closure which implements the body of that method and store it in a field
      methods.zipWithIndex.foreach { case (m, i) =>
        visitor.visitInsn(DUP)
        GenExpression.compileExpression(m.clo, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, className, s"clo$i", JvmOps.getClosureAbstractClassType(m.clo.tpe).toDescriptor)
      }

  }

  private def compileConstant(visitor: MethodVisitor, cst: Ast.Constant, tpe: MonoType, loc: SourceLocation)(implicit root: Root, flix: Flix): Unit = cst match {
    case Ast.Constant.Unit =>
      addSourceLine(visitor, loc)
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName,
        BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.toDescriptor)

    case Ast.Constant.Null =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ACONST_NULL)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Ast.Constant.Bool(true) =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ICONST_1)

    case Ast.Constant.Bool(false) =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ICONST_0)

    case Ast.Constant.Char(c) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, c)

    case Ast.Constant.Float32(f) =>
      addSourceLine(visitor, loc)
      f match {
        case 0f => visitor.visitInsn(FCONST_0)
        case 1f => visitor.visitInsn(FCONST_1)
        case 2f => visitor.visitInsn(FCONST_2)
        case _ => visitor.visitLdcInsn(f)
      }

    case Ast.Constant.Float64(d) =>
      addSourceLine(visitor, loc)
      d match {
        case 0d => visitor.visitInsn(DCONST_0)
        case 1d => visitor.visitInsn(DCONST_1)
        case _ => visitor.visitLdcInsn(d)
      }

    case Ast.Constant.BigDecimal(dd) =>
      addSourceLine(visitor, loc)
      visitor.visitTypeInsn(NEW, BackendObjType.BigDecimal.jvmName.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(dd.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigDecimal.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    case Ast.Constant.Int8(b) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, b)

    case Ast.Constant.Int16(s) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, s)

    case Ast.Constant.Int32(i) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, i)

    case Ast.Constant.Int64(l) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, l, isLong = true)

    case Ast.Constant.BigInt(ii) =>
      addSourceLine(visitor, loc)
      visitor.visitTypeInsn(NEW, BackendObjType.BigInt.jvmName.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigInt.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    case Ast.Constant.Str(s) =>
      addSourceLine(visitor, loc)
      visitor.visitLdcInsn(s)

    case Ast.Constant.Regex(patt) =>
      addSourceLine(visitor, loc)
      visitor.visitLdcInsn(patt.pattern)
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Regex), false)

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
  private def compileInt(visitor: MethodVisitor, i: Long, isLong: Boolean = false): Unit = {
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

  private def compileUnaryExpr(e: Expr,
                               currentClassType: JvmType.Reference,
                               visitor: MethodVisitor,
                               jumpLabels: Map[Symbol.LabelSym, Label],
                               entryPoint: Label,
                               sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    // Adding source line number for debugging
    addSourceLine(visitor, e.loc)

    compileExpression(e, visitor, currentClassType, jumpLabels, entryPoint)
    sop match {
      case SemanticOperator.ObjectOp.EqNull =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNULL, condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitLabel(condEnd)

      case SemanticOperator.ObjectOp.NeqNull =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNULL, condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)

      case SemanticOperator.BoolOp.Not =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNE, condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)

      case Float32Op.Neg | Float64Op.Neg | BigDecimalOp.Neg
           | Int8Op.Neg | Int16Op.Neg | Int32Op.Neg
           | Int64Op.Neg | BigIntOp.Neg => compileUnaryMinusExpr(visitor, sop, e.loc)

      case Int8Op.Not | Int16Op.Not | Int32Op.Not
           | Int64Op.Not | BigIntOp.Not => compileUnaryNegateExpr(visitor, sop, e.loc)

      case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", e.loc)
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
  private def compileUnaryMinusExpr(visitor: MethodVisitor, sop: SemanticOperator, loc: SourceLocation)(implicit root: Root, flix: Flix): Unit = sop match {
    case Float32Op.Neg => visitor.visitInsn(FNEG)
    case Float64Op.Neg => visitor.visitInsn(DNEG)
    case BigDecimalOp.Neg =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, "negate",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigDecimal), false)
    case Int8Op.Neg =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case Int16Op.Neg =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case Int32Op.Neg => visitor.visitInsn(INEG)
    case Int64Op.Neg => visitor.visitInsn(LNEG)
    case BigIntOp.Neg =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "negate",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false)
    case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", loc)
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
  private def compileUnaryNegateExpr(visitor: MethodVisitor, sop: SemanticOperator, loc: SourceLocation)(implicit root: Root, flix: Flix): Unit = sop match {
    case Int8Op.Not | Int16Op.Not | Int32Op.Not =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(IXOR)
    case Int64Op.Not =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(I2L)
      visitor.visitInsn(LXOR)
    case BigIntOp.Not =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "not",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false)
    case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", loc)
  }

  private def compileBinaryExpr(exp1: Expr, exp2: Expr,
                                currentClass: JvmType.Reference,
                                visitor: MethodVisitor,
                                lenv0: Map[Symbol.LabelSym, Label],
                                entryPoint: Label,
                                sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = sop match {

    case Float32Op.Add | Float64Op.Add | BigDecimalOp.Add
         | Int8Op.Add | Int16Op.Add | Int16Op.Add
         | Int32Op.Add | Int64Op.Add | BigIntOp.Add
         | Float32Op.Sub | Float64Op.Sub | BigDecimalOp.Sub
         | Int8Op.Sub | Int16Op.Sub | Int16Op.Sub
         | Int32Op.Sub | Int64Op.Sub | BigIntOp.Sub
         | Float32Op.Mul | Float64Op.Mul | BigDecimalOp.Mul
         | Int8Op.Mul | Int16Op.Mul | Int16Op.Mul
         | Int32Op.Mul | Int64Op.Mul | BigIntOp.Mul
         | Float32Op.Div | Float64Op.Div | BigDecimalOp.Div
         | Int8Op.Div | Int16Op.Div | Int16Op.Div
         | Int32Op.Div | Int64Op.Div | BigIntOp.Div
         | Int8Op.Rem | Int16Op.Rem | Int16Op.Rem
         | Int32Op.Rem | Int64Op.Rem
         | BigIntOp.Rem | StringOp.Concat => compileArithmeticExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, sop)

    case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", exp1.loc)
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
  private def compileArithmeticExpr(e1: Expr,
                                    e2: Expr,
                                    currentClassType: JvmType.Reference,
                                    visitor: MethodVisitor,
                                    jumpLabels: Map[Symbol.LabelSym, Label],
                                    entryPoint: Label,
                                    sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
    compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
    (semanticOperatorArithmeticToOpcode(sop), semanticOperatorArithmeticToMethod(sop)) match {
      case (Some(op), _) => sop match {
        case Float32Op.Add | Float32Op.Sub | Float32Op.Mul | Float32Op.Div
             | Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div =>
          visitor.visitInsn(op)

        case Int8Op.Add | Int8Op.Sub | Int8Op.Mul | Int8Op.Div | Int8Op.Rem =>
          visitor.visitInsn(op)
          visitor.visitInsn(I2B)

        case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem =>
          visitor.visitInsn(op)
          visitor.visitInsn(I2S)

        case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem
             | Int64Op.Add | Int64Op.Sub | Int64Op.Mul | Int64Op.Div | Int64Op.Rem =>
          visitor.visitInsn(op)

        case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", e1.loc)
      }

      case (_, Some(op)) => sop match {
        case BigDecimalOp.Add | BigDecimalOp.Sub | BigDecimalOp.Mul | BigDecimalOp.Div =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigDecimal.jvmName.toInternalName, op,
            AsmOps.getMethodDescriptor(List(JvmType.BigDecimal), JvmType.BigDecimal), false)

        case BigIntOp.Add | BigIntOp.Sub | BigIntOp.Mul | BigIntOp.Div | BigIntOp.Rem =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, op,
            AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.BigInteger), false)

        case StringOp.Concat =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, op,
            AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.String), false)

        case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", e1.loc)
      }
      case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.", e1.loc)
    }
  }

  private def semanticOperatorArithmeticToOpcode(sop: SemanticOperator): Option[Int] = sop match {
    case Float32Op.Add => Some(FADD)
    case Float32Op.Sub => Some(FSUB)
    case Float32Op.Mul => Some(FMUL)
    case Float32Op.Div => Some(FDIV)
    case Float64Op.Add => Some(DADD)
    case Float64Op.Sub => Some(DSUB)
    case Float64Op.Mul => Some(DMUL)
    case Float64Op.Div => Some(DDIV)
    case Int8Op.Add => Some(IADD)
    case Int8Op.Sub => Some(ISUB)
    case Int8Op.Mul => Some(IMUL)
    case Int8Op.Div => Some(IDIV)
    case Int8Op.Rem => Some(IREM)
    case Int16Op.Add => Some(IADD)
    case Int16Op.Sub => Some(ISUB)
    case Int16Op.Mul => Some(IMUL)
    case Int16Op.Div => Some(IDIV)
    case Int16Op.Rem => Some(IREM)
    case Int32Op.Add => Some(IADD)
    case Int32Op.Sub => Some(ISUB)
    case Int32Op.Mul => Some(IMUL)
    case Int32Op.Div => Some(IDIV)
    case Int32Op.Rem => Some(IREM)
    case Int64Op.Add => Some(LADD)
    case Int64Op.Sub => Some(LSUB)
    case Int64Op.Mul => Some(LMUL)
    case Int64Op.Div => Some(LDIV)
    case Int64Op.Rem => Some(LREM)
    case _ => None
  }

  private def semanticOperatorArithmeticToMethod(sop: SemanticOperator): Option[String] = sop match {
    case BigDecimalOp.Add | BigIntOp.Add => Some("add")
    case BigDecimalOp.Sub | BigIntOp.Sub => Some("subtract")
    case BigDecimalOp.Mul | BigIntOp.Mul => Some("multiply")
    case BigDecimalOp.Div | BigIntOp.Div => Some("divide")
    case BigIntOp.Rem => Some("remainder")
    case StringOp.Concat => Some("concat")
    case _ => None
  }

  /**
    * Generates code to read the given variable symbol and put it on top of the stack.
    */
  private def readVar(sym: Symbol.VarSym, tpe: MonoType, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    val jvmType = JvmOps.getErasedJvmType(tpe)
    val iLOAD = AsmOps.getLoadInstruction(jvmType)
    mv.visitVarInsn(iLOAD, sym.getStackOffset + 1)
    AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))
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
  private def pushArgs(visitor: MethodVisitor, args: List[Expr], signature: Array[Class[_ <: Object]], currentClass: JvmType.Reference, lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix): Unit = {
    // Evaluate arguments left-to-right and push them onto the stack.
    for ((arg, argType) <- args.zip(signature)) {
      compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
      if (!argType.isPrimitive) {
        // NB: Really just a hack because the backend does not support array JVM types properly.
        visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
      } else {
        arg.tpe match {
          // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
          case MonoType.Array(MonoType.Float32) => visitor.visitTypeInsn(CHECKCAST, "[F")
          case MonoType.Array(MonoType.Float64) => visitor.visitTypeInsn(CHECKCAST, "[D")
          case MonoType.Array(MonoType.Int8) => visitor.visitTypeInsn(CHECKCAST, "[B")
          case MonoType.Array(MonoType.Int16) => visitor.visitTypeInsn(CHECKCAST, "[S")
          case MonoType.Array(MonoType.Int32) => visitor.visitTypeInsn(CHECKCAST, "[I")
          case MonoType.Array(MonoType.Int64) => visitor.visitTypeInsn(CHECKCAST, "[J")
          case _ => // nop
        }
      }
    }
  }
}
