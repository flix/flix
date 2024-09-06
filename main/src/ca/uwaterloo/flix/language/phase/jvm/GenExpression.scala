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
import ca.uwaterloo.flix.language.ast.Ast.ExpPosition
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.{MonoType, _}
import ca.uwaterloo.flix.language.dbg.printer.OpPrinter
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.JavaObject
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.InstructionSet
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

/**
  * Generate expression
  */
object GenExpression {

  type Ref[T] = Array[T]

  case class MethodContext(clazz: JvmType.Reference,
                           entryPoint: Label,
                           lenv: Map[Symbol.LabelSym, Label],
                           newFrame: InstructionSet, // [...] -> [..., frame]
                           setPc: InstructionSet, // [..., frame, pc] -> [...]
                           localOffset: Int,
                           pcLabels: Vector[Label],
                           pcCounter: Ref[Int]
                          )

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpr(exp0: Expr)(implicit mv: MethodVisitor, ctx: MethodContext, root: Root, flix: Flix): Unit = exp0 match {

    case Expr.Cst(cst, tpe, loc) => cst match {
      case Ast.Constant.Unit =>
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName,
          BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.toDescriptor)

      case Ast.Constant.Null =>
        mv.visitInsn(ACONST_NULL)
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case Ast.Constant.Bool(true) =>
        mv.visitInsn(ICONST_1)

      case Ast.Constant.Bool(false) =>
        mv.visitInsn(ICONST_0)

      case Ast.Constant.Char(c) =>
        compileInt(c)

      case Ast.Constant.Float32(f) =>
        f match {
          case 0f => mv.visitInsn(FCONST_0)
          case 1f => mv.visitInsn(FCONST_1)
          case 2f => mv.visitInsn(FCONST_2)
          case _ => mv.visitLdcInsn(f)
        }

      case Ast.Constant.Float64(d) =>
        d match {
          case 0d => mv.visitInsn(DCONST_0)
          case 1d => mv.visitInsn(DCONST_1)
          case _ => mv.visitLdcInsn(d)
        }

      case Ast.Constant.BigDecimal(dd) =>
        // Can fail with NumberFormatException
        addSourceLine(mv, loc)
        mv.visitTypeInsn(NEW, BackendObjType.BigDecimal.jvmName.toInternalName)
        mv.visitInsn(DUP)
        mv.visitLdcInsn(dd.toString)
        mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigDecimal.jvmName.toInternalName, "<init>",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

      case Ast.Constant.Int8(b) =>
        compileInt(b)

      case Ast.Constant.Int16(s) =>
        compileInt(s)

      case Ast.Constant.Int32(i) =>
        compileInt(i)

      case Ast.Constant.Int64(l) =>
        compileLong(l)

      case Ast.Constant.BigInt(ii) =>
        // Add source line number for debugging (can fail with NumberFormatException)
        addSourceLine(mv, loc)
        mv.visitTypeInsn(NEW, BackendObjType.BigInt.jvmName.toInternalName)
        mv.visitInsn(DUP)
        mv.visitLdcInsn(ii.toString)
        mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigInt.jvmName.toInternalName, "<init>",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

      case Ast.Constant.Str(s) =>
        mv.visitLdcInsn(s)

      case Ast.Constant.Regex(patt) =>
        // Add source line number for debugging (can fail with PatternSyntaxException)
        addSourceLine(mv, loc)
        mv.visitLdcInsn(patt.pattern)
        mv.visitMethodInsn(INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
          AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Regex), false)

    }

    case Expr.Var(sym, tpe, _) =>
      val varType = JvmOps.getJvmType(tpe)
      val xLoad = AsmOps.getLoadInstruction(varType)
      mv.visitVarInsn(xLoad, sym.getStackOffset(ctx.localOffset))

    case Expr.ApplyAtomic(op, exps, tpe, _, loc) => op match {

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

      case AtomicOp.Region =>
        //!TODO: For now, just emit null
        mv.visitInsn(ACONST_NULL)
        mv.visitTypeInsn(CHECKCAST, BackendObjType.Region.jvmName.toInternalName)

      case AtomicOp.Is(sym) =>
        val List(exp) = exps
        val taggedType = BackendObjType.Tagged

        compileExpr(exp)
        val ins = {
          import BytecodeInstructions._
          CHECKCAST(taggedType.jvmName) ~ GETFIELD(taggedType.NameField) ~
            BackendObjType.Tagged.mkTagName(sym) ~ BackendObjType.Tagged.eqTagName()
        }
        ins(new BytecodeInstructions.F(mv))

      case AtomicOp.Tag(sym) =>
        val List(exp) = exps

        val tagType = BackendObjType.Tag(BackendType.toErasedBackendType(exp.tpe))

        val ins = {
          import BytecodeInstructions._
          NEW(tagType.jvmName) ~ DUP() ~ INVOKESPECIAL(tagType.Constructor) ~
            DUP() ~ BackendObjType.Tagged.mkTagName(sym) ~ PUTFIELD(tagType.NameField) ~
            DUP() ~ cheat(mv => compileExpr(exp)(mv, ctx, root, flix)) ~ PUTFIELD(tagType.ValueField)
        }
        ins(new BytecodeInstructions.F(mv))

      case AtomicOp.Untag(_) =>
        val List(exp) = exps
        val tagType = BackendObjType.Tag(BackendType.toErasedBackendType(tpe))

        compileExpr(exp)
        val ins = {
          import BytecodeInstructions._
          CHECKCAST(tagType.jvmName) ~ GETFIELD(tagType.ValueField)
        }
        ins(new BytecodeInstructions.F(mv))
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.Index(idx) =>
        val List(exp) = exps
        val MonoType.Tuple(elmTypes) = exp.tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.asErasedBackendType))
        // evaluating the `base`
        compileExpr(exp)
        // Retrieving the field `field${offset}`
        mv.visitFieldInsn(GETFIELD, tupleType.jvmName.toInternalName, s"field$idx", JvmOps.asErasedJvmType(tpe).toDescriptor)

      case AtomicOp.Tuple =>
        // We get the JvmType of the class for the tuple
        val MonoType.Tuple(elmTypes) = tpe
        val tupleType = BackendObjType.Tuple(elmTypes.map(BackendType.asErasedBackendType))
        val internalClassName = tupleType.jvmName.toInternalName
        // Instantiating a new object of tuple
        mv.visitTypeInsn(NEW, internalClassName)
        // Duplicating the class
        mv.visitInsn(DUP)
        // Evaluating all the elements to be stored in the tuple class
        exps.foreach(compileExpr)
        // Descriptor of constructor
        val constructorDescriptor = MethodDescriptor(tupleType.elms, VoidableType.Void)
        // Invoking the constructor
        mv.visitMethodInsn(INVOKESPECIAL, internalClassName, "<init>", constructorDescriptor.toDescriptor, false)

      case AtomicOp.RecordEmpty =>
        // We get the JvmType of the class for the RecordEmpty
        val classType = BackendObjType.RecordEmpty
        // Instantiating a new object of tuple
        mv.visitFieldInsn(GETSTATIC, classType.jvmName.toInternalName, BackendObjType.RecordEmpty.SingletonField.name, classType.toDescriptor)

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

      case AtomicOp.RecordExtend(field) =>
        val List(exp1, exp2) = exps

        // We get the JvmType of the record interface
        val interfaceType = BackendObjType.Record

        val recordType = BackendObjType.RecordExtend(BackendType.toErasedBackendType(exp1.tpe))
        val classInternalName = recordType.jvmName.toInternalName

        // Instantiating a new object of tuple
        mv.visitTypeInsn(NEW, classInternalName)
        mv.visitInsn(DUP)
        // Invoking the constructor
        mv.visitMethodInsn(INVOKESPECIAL, classInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)

        // Put the label of field (which is going to be the extension).
        mv.visitInsn(DUP)
        mv.visitLdcInsn(field.name)
        mv.visitFieldInsn(PUTFIELD, classInternalName, recordType.LabelField.name, BackendObjType.String.toDescriptor)

        // Put the value of the field onto the stack, since it is an expression we first need to compile it.
        mv.visitInsn(DUP)
        compileExpr(exp1)
        mv.visitFieldInsn(PUTFIELD, classInternalName, recordType.ValueField.name, recordType.ValueField.tpe.toDescriptor)

        // Put the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
        mv.visitInsn(DUP)
        compileExpr(exp2)
        mv.visitFieldInsn(PUTFIELD, classInternalName, recordType.RestField.name, recordType.RestField.tpe.toDescriptor)

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

      case AtomicOp.ArrayLit =>
        // We push the 'length' of the array on top of stack
        compileInt(exps.length)
        // We get the inner type of the array
        val innerType = tpe.asInstanceOf[MonoType.Array].tpe
        val backendType = BackendType.toFlixErasedBackendType(innerType)
        // Instantiating a new array of type jvmType
        visitArrayInstantiate(mv, backendType)
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
          mv.visitInsn(backendType.getArrayStoreInstruction)
        }

      case AtomicOp.ArrayNew =>
        val List(exp1, exp2) = exps
        // We get the inner type of the array
        val innerType = tpe.asInstanceOf[MonoType.Array].tpe
        val backendType = BackendType.toFlixErasedBackendType(innerType)
        // Evaluating the value of the 'default element'
        compileExpr(exp1)
        // Evaluating the 'length' of the array
        compileExpr(exp2)
        // Instantiating a new array of type jvmType
        visitArrayInstantiate(mv, backendType)
        if (backendType.is64BitWidth) {
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
        val arrayFillType = backendType.toArrayFillType
        // Invoking the method to fill the array with the default element
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Arrays", "fill", arrayFillType, false);

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

      case AtomicOp.ArrayLength =>
        val List(exp) = exps
        // Add source line number for debugging (can fail with ???)
        addSourceLine(mv, loc)

        // We get the inner type of the array
        val jvmType = JvmOps.getErasedJvmType(exp.tpe.asInstanceOf[MonoType.Array].tpe)
        // Evaluating the 'base'
        compileExpr(exp)
        // Cast the object to array
        mv.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
        // Pushes the 'length' of the array on top of stack
        mv.visitInsn(ARRAYLENGTH)

      case AtomicOp.StructNew(sym, fields) =>
        val region :: fieldExps = exps
        // Evaluate the region and ignore its value
        compileExpr(region)
        BytecodeInstructions.xPop(BackendType.toErasedBackendType(region.tpe))(new BytecodeInstructions.F(mv))
        // We get the JvmType of the class for the struct
        val MonoType.Struct(_, elmTypes, _) = tpe
        val structType = BackendObjType.Struct(elmTypes.map(BackendType.asErasedBackendType))
        val internalClassName = structType.jvmName.toInternalName
        // Instantiating a new object of struct
        mv.visitTypeInsn(NEW, internalClassName)
        // Duplicating the class
        mv.visitInsn(DUP)
        // Evaluating all the elements to be stored in the struct class
        fieldExps.foreach(compileExpr)
        // Descriptor of constructor
        val constructorDescriptor = MethodDescriptor(structType.elms, VoidableType.Void)
        // Invoking the constructor
        mv.visitMethodInsn(INVOKESPECIAL, internalClassName, "<init>", constructorDescriptor.toDescriptor, false)

      case AtomicOp.StructGet(field) =>
        val idx = field.idx
        val List(exp) = exps
        val MonoType.Struct(_, elmTypes, _) = exp.tpe
        val structType = BackendObjType.Struct(elmTypes.map(BackendType.asErasedBackendType))
        // evaluating the `base`
        compileExpr(exp)
        // Retrieving the field `field${offset}`
        mv.visitFieldInsn(GETFIELD, structType.jvmName.toInternalName, s"field$idx", JvmOps.asErasedJvmType(tpe).toDescriptor)

      case AtomicOp.StructPut(field) =>
        val idx = field.idx
        val List(exp1, exp2) = exps
        val MonoType.Struct(_, elmTypes, _) = exp1.tpe
        val structType = BackendObjType.Struct(elmTypes.map(BackendType.asErasedBackendType))
        // evaluating the `base`
        compileExpr(exp1)
        // evaluating the `rhs`
        compileExpr(exp2)
        // set the field `field${offset}`
        mv.visitFieldInsn(PUTFIELD, structType.jvmName.toInternalName, s"field$idx", JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
        // Since the return type is unit, we put an instance of unit on top of the stack
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.InstanceOf(clazz) =>
        val List(exp) = exps
        val className = asm.Type.getInternalName(clazz)
        compileExpr(exp)
        mv.visitTypeInsn(INSTANCEOF, className)

      case AtomicOp.Cast =>
        val List(exp) = exps
        compileExpr(exp)
        AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

      case AtomicOp.Unbox =>
        val List(exp) = exps
        compileExpr(exp)
        // this is a value
        val valueField = BackendObjType.Value.fieldFromType(BackendType.asErasedBackendType(tpe))
        val ins = BytecodeInstructions.GETFIELD(valueField)
        mv.visitTypeInsn(CHECKCAST, BackendObjType.Value.jvmName.toInternalName)
        ins(new BytecodeInstructions.F(mv))

      case AtomicOp.Box =>
        val List(exp) = exps
        compileExpr(exp)
        val erasedExpTpe = BackendType.toErasedBackendType(exp.tpe)
        val valueField = BackendObjType.Value.fieldFromType(erasedExpTpe)
        val ins = {
          import BytecodeInstructions._
          NEW(BackendObjType.Value.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.Value.Constructor) ~ DUP() ~
          xSwap(lowerLarge = erasedExpTpe.is64BitWidth, higherLarge = true) ~ // two objects on top of the stack
          PUTFIELD(valueField)
        }
        ins(new BytecodeInstructions.F(mv))

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
        mv.visitFieldInsn(GETFIELD, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutField(field) =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        compileExpr(exp1)
        compileExpr(exp2)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTFIELD, declaration, field.getName, JvmOps.getJvmType(exp2.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.GetStaticField(field) =>
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(GETSTATIC, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

      case AtomicOp.PutStaticField(field) =>
        val List(exp) = exps
        // Add source line number for debugging (can fail when calling java)
        addSourceLine(mv, loc)
        compileExpr(exp)
        val declaration = asm.Type.getInternalName(field.getDeclaringClass)
        mv.visitFieldInsn(PUTSTATIC, declaration, field.getName, JvmOps.getJvmType(exp.tpe).toDescriptor)

        // Push Unit on the stack.
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)

      case AtomicOp.Throw =>
        // Add source line number for debugging (can fail when handling exception)
        addSourceLine(mv, loc)
        val List(exp) = exps
        compileExpr(exp)
        mv.visitInsn(ATHROW)

      case AtomicOp.Spawn =>
        val List(exp1, exp2) = exps
        // Add source line number for debugging (can fail when spawning thread)
        addSourceLine(mv, loc)

        exp2 match {
          // The expression represents the `Static` region, just start a thread directly
          case Expr.ApplyAtomic(AtomicOp.Region, _, _, _, _) =>

            // Compile the expression, putting a function implementing the Runnable interface on the stack
            compileExpr(exp1)
            mv.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)

            // make a thread and run it
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "startVirtualThread", s"(${JvmName.Runnable.toDescriptor})${JvmName.Thread.toDescriptor}", false)
            mv.visitInsn(POP)

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
        mv.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.SingletonField.name, BackendObjType.Unit.jvmName.toDescriptor)


      case AtomicOp.Lazy =>
        val List(exp) = exps

        // Find the Lazy class name (Lazy$tpe).
        val MonoType.Lazy(elmType) = tpe
        val lazyType = BackendObjType.Lazy(BackendType.asErasedBackendType(elmType))

        val ins = {
          import BytecodeInstructions._
          NEW(lazyType.jvmName) ~
            DUP() ~  cheat(mv => compileExpr(exp)(mv, ctx, root, flix)) ~ INVOKESPECIAL(lazyType.Constructor)
        }
        ins(new BytecodeInstructions.F(mv))

      case AtomicOp.Force =>
        val List(exp) = exps

        // Find the Lazy class type (Lazy$tpe) and the inner value type.
        val MonoType.Lazy(elmType) = exp.tpe
        val erasedElmType = BackendType.asErasedBackendType(elmType)
        val lazyType = BackendObjType.Lazy(erasedElmType)

        // Emit code for the lazy expression.
        compileExpr(exp)

        val ins = {
          import BytecodeInstructions._
          CHECKCAST(lazyType.jvmName) ~
          DUP() ~ GETFIELD(lazyType.ExpField) ~
          ifConditionElse(Condition.NONNULL)(
            INVOKEVIRTUAL(lazyType.ForceMethod)
          )(
            GETFIELD(lazyType.ValueField)
          )
        }
        ins(new BytecodeInstructions.F(mv))

      case AtomicOp.HoleError(sym) =>
        // Add source line number for debugging (failable by design)
        addSourceLine(mv, loc)
        AsmOps.compileReifiedSourceLocation(mv, loc)
        val className = BackendObjType.HoleError.jvmName
        mv.visitTypeInsn(NEW, className.toInternalName)
        mv.visitInsn(DUP2)
        mv.visitInsn(SWAP)
        mv.visitLdcInsn(sym.toString)
        mv.visitInsn(SWAP)
        mv.visitMethodInsn(INVOKESPECIAL, className.toInternalName, "<init>", s"(${BackendObjType.String.toDescriptor}${BackendObjType.ReifiedSourceLocation.toDescriptor})${JvmType.Void.toDescriptor}", false)
        mv.visitInsn(ATHROW)

      case AtomicOp.MatchError =>
        // Add source line number for debugging (failable by design)
        addSourceLine(mv, loc)
        val className = BackendObjType.MatchError.jvmName
        AsmOps.compileReifiedSourceLocation(mv, loc)
        mv.visitTypeInsn(NEW, className.toInternalName)
        mv.visitInsn(DUP2)
        mv.visitInsn(SWAP)
        mv.visitMethodInsn(INVOKESPECIAL, className.toInternalName, "<init>", s"(${BackendObjType.ReifiedSourceLocation.toDescriptor})${JvmType.Void.toDescriptor}", false)
        mv.visitInsn(ATHROW)
    }

    case Expr.ApplyClo(exp, exps, ct, _, purity, loc) =>
      ct match {
        case ExpPosition.Tail =>
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

        case ExpPosition.NonTail =>
          // Type of the function abstract class
          val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
          val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)

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

          if (Purity.isControlPure(purity)) BackendObjType.Result.unwindSuspensionFreeThunk("in pure closure call", loc)(new BytecodeInstructions.F(mv))
          else {
            val pcPoint = ctx.pcCounter(0) + 1
            val pcPointLabel = ctx.pcLabels(pcPoint)
            val afterUnboxing = new Label()
            ctx.pcCounter(0) += 1
            BackendObjType.Result.unwindThunkToValue(pcPoint, ctx.newFrame, ctx.setPc)(new BytecodeInstructions.F(mv))
            mv.visitJumpInsn(GOTO, afterUnboxing)

            mv.visitLabel(pcPointLabel)
            printPc(mv, pcPoint)

            mv.visitVarInsn(ALOAD, 1)

            mv.visitLabel(afterUnboxing)
          }
      }

    case Expr.ApplyDef(sym, exps, ct, _, purity, loc) => ct match {
      case ExpPosition.Tail =>
        // Type of the function abstract class
        val functionInterface = JvmOps.getFunctionInterfaceType(root.defs(sym).arrowType)

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

      case ExpPosition.NonTail =>
        // JvmType of Def
        val defJvmType = JvmOps.getFunctionDefinitionClassType(sym)

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

        if (Purity.isControlPure(purity)) BackendObjType.Result.unwindSuspensionFreeThunk("in pure function call", loc)(new BytecodeInstructions.F(mv))
        else {
          val pcPoint = ctx.pcCounter(0) + 1
          val pcPointLabel = ctx.pcLabels(pcPoint)
          val afterUnboxing = new Label()
          ctx.pcCounter(0) += 1
          BackendObjType.Result.unwindThunkToValue(pcPoint, ctx.newFrame, ctx.setPc)(new BytecodeInstructions.F(mv))
          mv.visitJumpInsn(GOTO, afterUnboxing)

          mv.visitLabel(pcPointLabel)
          printPc(mv, pcPoint)
          mv.visitVarInsn(ALOAD, 1)

          mv.visitLabel(afterUnboxing)
        }
    }

    case Expr.ApplySelfTail(sym, exps, _, _, _) =>
      // The function abstract class name
      val functionInterface = JvmOps.getFunctionInterfaceType(root.defs(sym).arrowType)
      // Evaluate each argument and put the result on the Fn class.
      for ((arg, i) <- exps.zipWithIndex) {
        mv.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpr(arg)
        mv.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      mv.visitVarInsn(ALOAD, 0)
      compileInt(0)
      ctx.setPc(new BytecodeInstructions.F(mv))
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

    case Expr.JumpTo(sym, _, _, _) =>
      // Jumping to the label
      mv.visitJumpInsn(GOTO, ctx.lenv(sym))

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      compileExpr(exp1)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      AsmOps.castIfNotPrim(mv, jvmType)
      mv.visitVarInsn(iStore, sym.getStackOffset(ctx.localOffset))
      compileExpr(exp2)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, _) =>
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      // JvmType of the closure
      val cloType = JvmOps.getClosureClassType(defSym)

      // Store temp recursive value
      mv.visitInsn(ACONST_NULL)
      mv.visitVarInsn(iStore, varSym.getStackOffset(ctx.localOffset))
      // Compile the closure
      compileExpr(exp1)
      // fix the local and closure reference
      mv.visitInsn(DUP)
      mv.visitInsn(DUP)
      mv.visitFieldInsn(PUTFIELD, cloType.name.toInternalName, s"clo$index", JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)
      // Store the closure locally (maybe not needed?)
      mv.visitVarInsn(iStore, varSym.getStackOffset(ctx.localOffset))
      compileExpr(exp2)

    case Expr.Stmt(exp1, exp2, _, _, _) =>
      compileExpr(exp1)
      BytecodeInstructions.xPop(BackendType.toErasedBackendType(exp1.tpe))(new BytecodeInstructions.F(mv))
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

      // Emit try finally block.
      mv.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

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
        mv.visitVarInsn(istore, sym.getStackOffset(ctx.localOffset))

        // Emit code for the handler body expression.
        compileExpr(body)
        mv.visitJumpInsn(GOTO, afterTryAndCatch)
      }

      // Add the label after both the try and catch rules.
      mv.visitLabel(afterTryAndCatch)

    case Expr.TryWith(exp, effUse, rules, ct, _, _, _) =>
      // exp is a Unit -> exp.tpe closure
      val effectJvmName = JvmOps.getEffectDefinitionClassType(effUse.sym).name
      val ins = {
        import BytecodeInstructions._
        // eff name
        pushString(effUse.sym.toString) ~
        // handler
        NEW(effectJvmName) ~ DUP() ~ cheat(_.visitMethodInsn(Opcodes.INVOKESPECIAL, effectJvmName.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)) ~
        // bind handler closures
        cheat(mv => rules.foreach{
          case HandlerRule(op, _, exp) =>
            mv.visitInsn(Opcodes.DUP)
            compileExpr(exp)(mv, ctx, root, flix)
            mv.visitFieldInsn(Opcodes.PUTFIELD, effectJvmName.toInternalName, JvmOps.getEffectOpName(op.sym), GenEffectClasses.opFieldType(op.sym).toDescriptor)
        }) ~
        // frames
        NEW(BackendObjType.FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.FramesNil.Constructor) ~
        // continuation
        cheat(mv => compileExpr(exp)(mv, ctx, root, flix)) ~
        // exp.arg0 should be set to unit here but from lifting we know that it is unused so the
        // implicit null is fine.
        // call installHandler
        INVOKESTATIC(BackendObjType.Handler.InstallHandlerMethod)
      }
      ins(new BytecodeInstructions.F(mv))
      // handle value/suspend/thunk if in non-tail position
      if (ct == ExpPosition.NonTail) {
        val pcPoint = ctx.pcCounter(0) + 1
        val pcPointLabel = ctx.pcLabels(pcPoint)
        val afterUnboxing = new Label()
        ctx.pcCounter(0) += 1
        BackendObjType.Result.unwindThunkToValue(pcPoint, ctx.newFrame, ctx.setPc)(new BytecodeInstructions.F(mv))
        mv.visitJumpInsn(GOTO, afterUnboxing)

        mv.visitLabel(pcPointLabel)
        printPc(mv, pcPoint)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitLabel(afterUnboxing)
      } else {
        mv.visitInsn(ARETURN)
      }

    case Expr.Do(op, exps, tpe, _, _) =>
      val pcPoint = ctx.pcCounter(0) + 1
      val pcPointLabel = ctx.pcLabels(pcPoint)
      val afterUnboxing = new Label()
      val erasedResult = BackendType.toErasedBackendType(tpe)

      ctx.pcCounter(0) += 1
      val ins: InstructionSet = {
        import BytecodeInstructions._
        import BackendObjType.Suspension
        val effectClass = JvmOps.getEffectDefinitionClassType(op.sym.eff)
        val effectStaticMethod = ClassMaker.StaticMethod(
          effectClass.name,
          ClassMaker.Visibility.IsPublic,
          ClassMaker.Final.NotFinal,
          JvmOps.getEffectOpName(op.sym),
          GenEffectClasses.opStaticFunctionDescriptor(op.sym),
          None
        )
        NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~
        DUP() ~ pushString(op.sym.eff.toString) ~ PUTFIELD(Suspension.EffSymField) ~
        DUP() ~
        // --- eff op ---
        cheat(mv => exps.foreach(e => compileExpr(e)(mv, ctx, root, flix))) ~
        mkStaticLambda(BackendObjType.EffectCall.ApplyMethod, effectStaticMethod, 2) ~
        // --------------
        PUTFIELD(Suspension.EffOpField) ~
        DUP() ~
        // create continuation
        NEW(BackendObjType.FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.FramesNil.Constructor) ~
        ctx.newFrame ~ DUP() ~ cheat(m => compileInt(pcPoint)(m)) ~ ctx.setPc ~
        INVOKEVIRTUAL(BackendObjType.FramesNil.PushMethod) ~
        // store continuation
        PUTFIELD(Suspension.PrefixField) ~
        DUP() ~ NEW(BackendObjType.ResumptionNil.jvmName) ~ DUP() ~ INVOKESPECIAL(BackendObjType.ResumptionNil.Constructor) ~ PUTFIELD(Suspension.ResumptionField) ~
        xReturn(Suspension.toTpe)
      }
      ins(new BytecodeInstructions.F(mv))

      mv.visitLabel(pcPointLabel)
      printPc(mv, pcPoint)
      mv.visitVarInsn(ALOAD, 1)
      BytecodeInstructions.GETFIELD(BackendObjType.Value.fieldFromType(erasedResult))(new BytecodeInstructions.F(mv))

      mv.visitLabel(afterUnboxing)
      AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))

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

  private def printPc(mv: MethodVisitor, pcPoint: Int): Unit = if (!GenFunAndClosureClasses.onCallDebugging) () else {
    val printStream = JvmName(List("java", "io"), "PrintStream")
    mv.visitFieldInsn(GETSTATIC, JvmName(List("java", "lang"), "System").toInternalName, "out", printStream.toDescriptor)
    mv.visitLdcInsn("pc = ")
    compileInt(pcPoint)(mv)
    BytecodeInstructions.xToString(BackendType.Int32)(new BytecodeInstructions.F(mv))
    mv.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, "concat", MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(BackendObjType.String.toTpe).toDescriptor, false)
    mv.visitMethodInsn(INVOKEVIRTUAL, printStream.toInternalName, "println", MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void).toDescriptor, false)
  }

  /**
    * Emits code that instantiates an array of the type `tpe`.
    */
  private def visitArrayInstantiate(mv: MethodVisitor, tpe: BackendType): Unit = {
    tpe match {
      case BackendType.Array(_) => mv.visitTypeInsn(ANEWARRAY, tpe.toDescriptor)
      case BackendType.Reference(ref) => mv.visitTypeInsn(ANEWARRAY, ref.jvmName.toInternalName)
      case BackendType.Bool => mv.visitIntInsn(NEWARRAY, T_BOOLEAN)
      case BackendType.Char => mv.visitIntInsn(NEWARRAY, T_CHAR)
      case BackendType.Int8 => mv.visitIntInsn(NEWARRAY, T_BYTE)
      case BackendType.Int16 => mv.visitIntInsn(NEWARRAY, T_SHORT)
      case BackendType.Int32 => mv.visitIntInsn(NEWARRAY, T_INT)
      case BackendType.Int64 => mv.visitIntInsn(NEWARRAY, T_LONG)
      case BackendType.Float32 => mv.visitIntInsn(NEWARRAY, T_FLOAT)
      case BackendType.Float64 => mv.visitIntInsn(NEWARRAY, T_DOUBLE)
    }
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
