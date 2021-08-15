/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm
import org.objectweb.asm.{MethodVisitor, Opcodes}

import scala.language.implicitConversions

object BytecodeCompiler {

  sealed trait Stack

  sealed trait StackNil extends Stack

  sealed trait StackCons[R <: Stack, T <: PType] extends Stack

  sealed trait StackEnd extends Stack

  type **[R <: Stack, T <: PType] = StackCons[R, T]

  sealed case class F[T <: Stack](visitor: MethodVisitor)

  trait Cat1[T]

  implicit val int8Cat1: PInt8 => Cat1[PInt8] = null
  implicit val int16Cat1: PInt16 => Cat1[PInt16] = null
  implicit val int32Cat1: PInt32 => Cat1[PInt32] = null
  implicit val charCat1: PChar => Cat1[PChar] = null
  implicit val float32Cat1: PFloat32 => Cat1[PFloat32] = null

  implicit def referenceCat1[T <: PRefType](t: PReference[T]): Cat1[PReference[T]] = null

  trait Cat2[T]

  implicit val int64Cat1: PInt64 => Cat2[PInt64] = null
  implicit val float64Cat1: PFloat64 => Cat2[PFloat64] = null

  trait Int32Usable[T]

  implicit val int8U: PInt8 => Int32Usable[PInt8] = null
  implicit val int16U: PInt16 => Int32Usable[PInt16] = null
  implicit val int32U: PInt32 => Int32Usable[PInt32] = null

  def compileExp[R <: Stack, T <: PType](exp: Expression[T]): F[R] => F[R ** T] = exp match {
    case Expression.Unit(loc) =>
      WithSource[R](loc) ~
        pushUnit

    case Expression.Null(tpe, loc) =>
      WithSource[R](loc) ~ pushNull(tpe)

    case Expression.True(loc) =>
      WithSource[R](loc) ~ pushBool(true)

    case Expression.False(loc) =>
      WithSource[R](loc) ~ pushBool(false)

    case Expression.Char(lit, loc) =>
      WithSource[R](loc) ~
        pushChar(lit)

    case Expression.Float32(lit, loc) =>
      WithSource[R](loc) ~ pushFloat32(lit)

    case Expression.Float64(lit, loc) =>
      WithSource[R](loc) ~ pushFloat64(lit)

    case Expression.Int8(lit, loc) =>
      WithSource[R](loc) ~ pushInt8(lit)

    case Expression.Int16(lit, loc) =>
      WithSource[R](loc) ~ pushInt16(lit)

    case Expression.Int32(lit, loc) =>
      WithSource[R](loc) ~ pushInt32(lit)

    case Expression.Int64(lit, loc) =>
      WithSource[R](loc) ~ pushInt64(lit)

    case Expression.BigInt(lit, loc) =>
      WithSource[R](loc) ~
        pushBigInt(lit)

    case Expression.Str(lit, loc) =>
      WithSource[R](loc) ~
        pushString(lit)

    case Expression.Var(sym, tpe, loc) =>
      WithSource[R](loc) ~
        XLOAD(tpe, sym.getStackOffset + symOffsetOffset) // TODO(JLS): make this offset exist in F, dependent on static(0)/object function(1)

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      WithSource[R](loc) ~
        Instructions.CREATECLOSURE(freeVars, sym.cloName)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        CALL(args, squeezeFunction(squeezeReference(exp.tpe)))

    case Expression.ApplyDef(sym, args, fnTpe, tpe, loc) =>
      WithSource[R](loc) ~
        CREATEDEF(sym.defName, tpe.tagOf) ~
        CALL(args, squeezeFunction(squeezeReference(fnTpe)))

    case Expression.ApplyCloTail(exp, args, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        TAILCALL(args, squeezeFunction(squeezeReference(exp.tpe)))

    case Expression.ApplyDefTail(sym, args, fnTpe, tpe, loc) =>
      WithSource[R](loc) ~
        CREATEDEF(sym.defName, tpe.tagOf) ~
        TAILCALL(args, squeezeFunction(squeezeReference(fnTpe)))

    case Expression.ApplySelfTail(_, _, actuals, fnTpe, _, loc) =>
      WithSource[R](loc) ~
        SELFTAILCALL(actuals, squeezeFunction(squeezeReference(fnTpe)))

    case Expression.BoolNot(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        IFNE(START[R] ~ pushBool(false))(START[R] ~ pushBool(true))

    // Unary expressions
    case Expression.Float32Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        FNEG

    case Expression.Float64Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        DNEG

    case Expression.Int8Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        INEG ~
        I2B

    case Expression.Int8Not(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        pushInt8(-1) ~
        BXOR

    case Expression.Int16Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        INEG ~
        I2S

    case Expression.Int16Not(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        pushInt16(-1) ~
        SXOR

    case Expression.Int32Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        INEG

    case Expression.Int32Not(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        pushInt32(-1) ~
        IXOR

    case Expression.Int64Neg(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        LNEG

    case Expression.Int64Not(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        pushInt64(-1) ~
        LXOR

    case Expression.BigIntNeg(exp, tpe, loc) => ???
    case Expression.BigIntNot(exp, tpe, loc) => ???
    case Expression.ObjEqNull(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        IFNULL(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))

    case Expression.ObjNeqNull(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        IFNONNULL(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))

    // Binary expressions
    case Expression.BoolLogicalOp(op, exp1, exp2, tpe, loc) => ???
    case Expression.BoolEquality(op, exp1, exp2, tpe, loc) => ???
    case Expression.CharComparison(op, exp1, exp2, tpe, loc) => ???
    case Expression.Float32Arithmetic(op, exp1, exp2, tpe, loc) => ???
    case Expression.Float32Comparison(op, exp1, exp2, tpe, loc) => ???
    case Expression.Float64Arithmetic(op, exp1, exp2, tpe, loc) => ???
    case Expression.Float64Comparison(op, exp1, exp2, tpe, loc) => ???
    case Expression.Int8Arithmetic(op, exp1, exp2, _, loc) =>
      val compileOperands: F[R] => F[R ** PInt8 ** PInt8] =
        START[R] ~ compileExp(exp1) ~ compileExp(exp2)
      WithSource[R](loc) ~ (op match {
        case ArithmeticOp.Add => compileOperands ~ BADD
        case ArithmeticOp.Sub => compileOperands ~ BSUB
        case ArithmeticOp.Mul => compileOperands ~ BMUL
        case ArithmeticOp.Div => compileOperands ~ BDIV
        case ArithmeticOp.Rem => compileOperands ~ BREM
        case ArithmeticOp.Exp =>
          DoublePow[R] {
            START[StackNil] ~
              compileExp(exp1) ~ I2D ~
              compileExp(exp2) ~ I2D
          } ~
            D2I ~
            I2B
      })

    case Expression.Int16Arithmetic(op, exp1, exp2, _, loc) =>
      val compileOperands: F[R] => F[R ** PInt16 ** PInt16] =
        START[R] ~ compileExp(exp1) ~ compileExp(exp2)
      WithSource[R](loc) ~ (op match {
        case ArithmeticOp.Add => compileOperands ~ SADD
        case ArithmeticOp.Sub => compileOperands ~ SSUB
        case ArithmeticOp.Mul => compileOperands ~ SMUL
        case ArithmeticOp.Div => compileOperands ~ SDIV
        case ArithmeticOp.Rem => compileOperands ~ SREM
        case ArithmeticOp.Exp =>
          DoublePow[R] {
            START[StackNil] ~
              compileExp(exp1) ~ I2D ~
              compileExp(exp2) ~ I2D
          } ~
            D2I ~
            I2S
      })

    case Expression.Int32Arithmetic(op, exp1, exp2, _, loc) =>
      val compileOperands: F[R] => F[R ** PInt32 ** PInt32] =
        START[R] ~ compileExp(exp1) ~ compileExp(exp2)
      WithSource[R](loc) ~ (op match {
        case ArithmeticOp.Add => compileOperands ~ IADD
        case ArithmeticOp.Sub => compileOperands ~ ISUB
        case ArithmeticOp.Mul => compileOperands ~ IMUL
        case ArithmeticOp.Div => compileOperands ~ IDIV
        case ArithmeticOp.Rem => compileOperands ~ IREM
        case ArithmeticOp.Exp =>
          DoublePow[R] {
            START[StackNil] ~
              compileExp(exp1) ~ I2D ~
              compileExp(exp2) ~ I2D
          } ~
            D2I
      })

    case Expression.Int64Arithmetic(op, exp1, exp2, _, loc) =>
      val compileOperands: F[R] => F[R ** PInt64 ** PInt64] =
        START[R] ~ compileExp(exp1) ~ compileExp(exp2)
      WithSource[R](loc) ~ (op match {
        case ArithmeticOp.Add => compileOperands ~ LADD
        case ArithmeticOp.Sub => compileOperands ~ LSUB
        case ArithmeticOp.Mul => compileOperands ~ LMUL
        case ArithmeticOp.Div => compileOperands ~ LDIV
        case ArithmeticOp.Rem => compileOperands ~ LREM
        case ArithmeticOp.Exp =>
          DoublePow[R] {
            START[StackNil] ~
              compileExp(exp1) ~ L2D ~
              compileExp(exp2) ~ L2D
          } ~ D2L
      })

    case Expression.BigIntArithmetic(op, exp1, exp2, tpe, loc) => ???
    case Expression.Int8Bitwise(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case BitwiseOp.And => BAND
          case BitwiseOp.Or => BOR
          case BitwiseOp.Xor => BXOR
          case BitwiseOp.Shl => BSHL
          case BitwiseOp.Shr => BSHR
        })

    case Expression.Int16Bitwise(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case BitwiseOp.And => SAND
          case BitwiseOp.Or => SOR
          case BitwiseOp.Xor => SXOR
          case BitwiseOp.Shl => SSHL
          case BitwiseOp.Shr => SSHR
        })

    case Expression.Int32Bitwise(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case BitwiseOp.And => IAND
          case BitwiseOp.Or => IOR
          case BitwiseOp.Xor => IXOR
          case BitwiseOp.Shl => ISHL
          case BitwiseOp.Shr => ISHR
        })

    case Expression.Int64Bitwise(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case BitwiseOp.And => LAND
          case BitwiseOp.Or => LOR
          case BitwiseOp.Xor => LXOR
          case BitwiseOp.Shl => LSHL
          case BitwiseOp.Shr => LSHR
        })

    case Expression.BigIntBitwise(op, exp1, exp2, tpe, loc) => ???
    case Expression.Int8Comparison(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case op: ErasedAst.EqualityOp => op match {
            case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          }
        })

    case Expression.Int16Comparison(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case op: ErasedAst.EqualityOp => op match {
            case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          }
        })


    case Expression.Int32Comparison(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          case op: ErasedAst.EqualityOp => op match {
            case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          }
        })

    case Expression.Int64Comparison(op, exp1, exp2, tpe, loc) => ???
    case Expression.BigIntComparison(op, exp1, exp2, tpe, loc) => ???
    case Expression.StringConcat(exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        stringConcat

    case Expression.StringEquality(op, exp1, exp2, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        (op match {
          case EqualityOp.Eq => ObjEquals
          case EqualityOp.Ne => ObjEquals ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
        })

    case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        IFEQ(START[R] ~ compileExp(exp2))(START[R] ~ compileExp(exp3))

    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      //TODO(JLS): sym is unsafe. localvar stack + reg as types?
      WithSource[R](loc) ~
        compileExp(exp1) ~
        XStore(sym, exp1.tpe) ~
        compileExp(exp2)

    case Expression.Is(sym, tag, exp, loc) => ???
    case Expression.Tag(sym, tag, exp, tpe, loc) => ???
    case Expression.Untag(sym, tag, exp, tpe, loc) => ???
    case Expression.Index(base, offset, tpe, loc) => ???
    case Expression.Tuple(elms, tpe, loc) =>
      val tupleRef = squeezeReference(tpe)
      WithSource[R](loc) ~
        NEW(tupleRef) ~
        DUP ~
        INVOKESPECIAL(tupleRef) ~
        multiComposition(elms.zipWithIndex){ case (elm, elmIndex) =>
          START[R ** PReference[PTuple]] ~
            DUP ~
            PUTFIELD(tupleRef, GenTupleClasses.indexFieldName(elmIndex), elm, erasedType = true)
        }

    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) =>
      def makeAndFillArray[R0 <: Stack, T0 <: PType]
      (elms: List[Expression[T0]], arrayType: RArray[T0]):
      F[R0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]]] = {
        START[R0 ** PReference[PArray[T0]]] ~
          multiComposition(elms.zipWithIndex) { case (elm, index) =>
            START[R0 ** PReference[PArray[T0]]] ~
              DUP ~
              pushInt32(index) ~
              compileExp(elm) ~
              XASTORE(arrayType.tpe)
          }
      }

      WithSource[R](loc) ~
        pushInt32(elms.length) ~
        XNEWARRAY(tpe) ~
        makeAndFillArray(elms, squeezeArray(squeezeReference(tpe)))


    case Expression.ArrayNew(elm, len, tpe, loc) =>
      def elmArraySwitch
      [R0 <: Stack, T0 <: PType]
      (elmType: RType[T0]):
      F[R0 ** T0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]] ** PReference[PArray[T0]] ** T0] =
        elmType match {
          case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => // Cat1
            //TODO(JLS): note: start here is needed because of some implicit overshadowing
            START[R0 ** T0 ** PReference[PArray[T0]]] ~
              DUP_X1 ~
              SWAP
          case RInt64 | RFloat64 => // Cat2
            START[R0 ** T0 ** PReference[PArray[T0]]] ~
              DUP_X2_onCat2 ~
              DUP_X2_onCat2 ~
              POP
        }

      WithSource[R](loc) ~
        compileExp(elm) ~
        compileExp(len) ~
        XNEWARRAY(tpe) ~
        elmArraySwitch(elm.tpe) ~
        arraysFill(elm.tpe)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        XALOAD(tpe)

    case Expression.ArrayStore(base, index, elm, _, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        compileExp(elm) ~
        XASTORE(elm.tpe) ~
        pushUnit

    case Expression.ArrayLength(base, _, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        arrayLength(base.tpe)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(beginIndex) ~
        compileExp(endIndex) ~
        SWAP ~
        DUP_X1 ~
        ISUB ~
        DUP ~
        XNEWARRAY(tpe) ~
        DUP2_X2_cat1_onCat1 ~
        SWAP ~
        pushInt32(0) ~
        SWAP ~
        systemArrayCopy ~
        SWAP ~
        POP

    case Expression.Ref(exp, tpe, loc) =>
      val tpeRRef = squeezeReference(tpe)
      WithSource[R](loc) ~
        NEW(tpeRRef) ~
        DUP ~
        INVOKESPECIAL(tpeRRef) ~
        DUP ~
        compileExp(exp) ~
        PUTFIELD(tpeRRef, GenRefClasses.ValueFieldName, exp.tpe, erasedType = true)

    case Expression.Deref(exp, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        XGETFIELD(squeezeReference(exp.tpe), GenRefClasses.ValueFieldName, tpe, undoErasure = true)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        PUTFIELD(squeezeReference(exp1.tpe), GenRefClasses.ValueFieldName, exp2.tpe, erasedType = true) ~
        pushUnit

    case Expression.Existential(fparam, exp, loc) => ???
    case Expression.Universal(fparam, exp, loc) => ???
    case Expression.Cast(exp, tpe, loc) => {
      // TODO(JLS): implement Cast
      def fixStack[R <: Stack, T1 <: PType, T2 <: PType]: F[R ** T1] => F[R ** T2] = f => f.asInstanceOf[F[R ** T2]]

      WithSource[R](loc) ~
        compileExp(exp) ~
        fixStack
    }

    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => f => {
      // TODO(JLS): fix this
      // Adding source line number for debugging
      WithSource(loc)(f)

      // Evaluate the receiver object.
      compileExp(exp)(f)
      val thisType = asm.Type.getInternalName(method.getDeclaringClass)
      f.visitor.visitTypeInsn(Opcodes.CHECKCAST, thisType)

      // Retrieve the signature.
      val signature = method.getParameterTypes

      // Evaluate arguments left-to-right and push them onto the stack.
      for (((arg, argType), index) <- args.zip(signature).zipWithIndex) {
        compileExp(arg)(f)
        if (!argType.isPrimitive) {
          // NB: Really just a hack because the backend does not support array JVM types properly.
          f.visitor.visitTypeInsn(Opcodes.CHECKCAST, asm.Type.getInternalName(argType))
        } else {
          def castArray(tpe: RRefType[_ <: PRefType]): Unit = tpe match {
            case RArray(arrTpe) =>
              def castArrayElm[T0 <: PType](arrTpe: RType[T0]): Unit = arrTpe match {
                // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
                case RInt8 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[B")
                case RInt16 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[S")
                case RInt32 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[I")
                case RInt64 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[J")
                case RFloat32 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[F")
                case RFloat64 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[D")
                case _ => // nop
              }

              castArrayElm(arrTpe)
            case _ => // nop
          }

          arg.tpe match {
            case RReference(referenceType) => castArray(referenceType)
            case _ => // nop
          }
        }
      }
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)

      // Check if we are invoking an interface or class.
      if (method.getDeclaringClass.isInterface) {
        f.visitor.visitMethodInsn(Opcodes.INVOKEINTERFACE, declaration, name, descriptor, true)
      } else {
        f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, declaration, name, descriptor, false)
      }

      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) pushUnit(f)
      f.asInstanceOf[F[R ** T]]
    }

    case Expression.InvokeStaticMethod(method, args, tpe, loc) => f => {
      // TODO(JLS): fix this
      // Adding source line number for debugging
      WithSource(loc)(f)

      // Retrieve the signature.
      val signature = method.getParameterTypes

      // Evaluate arguments left-to-right and push them onto the stack.
      for (((arg, argType), index) <- args.zip(signature).zipWithIndex) {
        compileExp(arg)(f)
        if (!argType.isPrimitive) {
          // NB: Really just a hack because the backend does not support array JVM types properly.
          f.visitor.visitTypeInsn(Opcodes.CHECKCAST, asm.Type.getInternalName(argType))
        } else {
          def castArray(tpe: RRefType[_ <: PRefType]): Unit = tpe match {
            case RArray(arrTpe) =>
              def castArrayElm[T0 <: PType](arrTpe: RType[T0]): Unit = arrTpe match {
                // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
                case RInt8 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[B")
                case RInt16 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[S")
                case RInt32 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[I")
                case RInt64 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[J")
                case RFloat32 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[F")
                case RFloat64 => f.visitor.visitTypeInsn(Opcodes.CHECKCAST, "[D")
                case _ => // nop
              }

              castArrayElm(arrTpe)
            case _ => // nop
          }

          arg.tpe match {
            case RReference(referenceType) => castArray(referenceType)
            case _ => // nop
          }
        }
      }
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)

      f.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, declaration, name, descriptor, false)

      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) pushUnit(f)
      f.asInstanceOf[F[R ** T]]
    }

    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) =>
      WithSource[R](loc) ~
        getStaticField(field, tpe)

    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewChannel(exp, tpe, loc) =>
      WithSource[R](loc) ~
        pushNull(tpe)

    case Expression.GetChannel(exp, tpe, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, loc) => ???
    case Expression.Spawn(exp, tpe, loc) => ???
    case Expression.Lazy(exp, tpe, loc) =>
      WithSource[R](loc) ~
        mkLazy(tpe, exp.tpe, compileExp(exp))

    case Expression.Force(exp, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        FORCE(exp.tpe)

    case Expression.HoleError(sym, tpe, loc) => ???
    case Expression.MatchError(tpe, loc) => ???

    case Expression.BoxInt8(exp, loc) => ???
    case Expression.BoxInt16(exp, loc) => ???
    case Expression.BoxInt32(exp, loc) => ???
    case Expression.BoxInt64(exp, loc) => ???
    case Expression.BoxChar(exp, loc) => ???
    case Expression.BoxFloat32(exp, loc) => ???
    case Expression.BoxFloat64(exp, loc) => ???
    case Expression.UnboxInt8(exp, loc) => ???
    case Expression.UnboxInt16(exp, loc) => ???
    case Expression.UnboxInt32(exp, loc) => ???
    case Expression.UnboxInt64(exp, loc) => ???
    case Expression.UnboxChar(exp, loc) => ???
    case Expression.UnboxFloat32(exp, loc) => ???
    case Expression.UnboxFloat64(exp, loc) => ???
  }

}
