/*
 * Copyright 2019 Miguel Fialho
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
package ca.uwaterloo.flix.language.phase.njvm


import java.lang.reflect.Modifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.SemanticOperator.{Int32Op, StringOp}
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType, SemanticOperator, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.{ClosureInfo, JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes.{Closure, Context, RecordEmpty, RecordExtend, RefClass, TagClass, TupleClass}
import ca.uwaterloo.flix.language.phase.njvm.interfaces.{ContinuationInterface, FunctionInterface, RecordInterface, TupleInterface}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Label
import org.objectweb.asm

import scala.reflect.runtime.universe._

object GenExpression {
  def compileExpression[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (exp0: Expression, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** T1] = {
    (exp0 match {
      case Expression.Unit => compileUnit[S]
      case Expression.True | Expression.False => compileBoolean[S](exp0)
      case Expression.Char(c) => compileChar[S](c)
      case Expression.Float32(f) => compileFloat[S](f)
      case Expression.Float64(d) => compileDouble[S](d)
      case Expression.Int8(b) => compileInt[S](b)
      case Expression.Int16(s) => compileInt[S](s)
      case Expression.Int32(i) => compileInt[S](i)
      case Expression.Int64(l) => compileLong[S](l)
      case Expression.BigInt(ii) => compileBigInt[S](ii)
      case Expression.Str(s) => compileString[S](s)
      case Expression.Var(sym, tpe, _) => compileVar[S, T1](sym, tpe)

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
        // ClosureInfo
        val closure = ClosureInfo(sym, freeVars, fnType)

        compileClosure[S](closure, freeVars, loc, map, lenv0, entryPoint)
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        compileApplyClo[S, T1](exp, args, tpe, map, lenv0, entryPoint)
      case Expression.ApplyDef(name, args, tpe, loc) =>
        compileApplyDef[S, T1](name, args, tpe, map, lenv0, entryPoint)
      case Expression.ApplyEff(_, _, _, _) =>
        compileApplyEff[S, T1]
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        compileApplyCloTail[S, T1](exp, args, tpe, loc, map, lenv0, entryPoint)
      case Expression.ApplyDefTail(name, args, tpe, loc) => ???
      case Expression.ApplyEffTail(sym, args, tpe, loc) => ???
      case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => ???

      case Expression.Unary(sop, op, exp, _, _) => ???
      case Expression.Binary(sop, op, exp1, exp2, _, _) =>
        sop match {
          case op: SemanticOperator.BoolOp => ???
          case op: SemanticOperator.CharOp => ???
          case op: SemanticOperator.Float32Op => ???
          case op: SemanticOperator.Float64Op => ???
          case op: SemanticOperator.Int8Op => ???
          case op: SemanticOperator.Int16Op => ???
          case op: SemanticOperator.Int32Op =>
            compileInt32Op[S, T1](op, exp1, exp2, map, lenv0, entryPoint)
          case op: SemanticOperator.Int64Op => ???
          case op: SemanticOperator.BigIntOp => ???
          case op: SemanticOperator.StringOp =>
            op match {
              case StringOp.Concat =>
                compileExpression[S ,Ref[MString]](exp1,map, lenv0, entryPoint) |>>
                compileExpression[S ** Ref[MString], Ref[MString]](exp2,map, lenv0, entryPoint)
                Api.Java.Lang.String.concat.INVOKE
              case StringOp.Eq => ???
              case StringOp.Neq => ???
            }
        }
      case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
        compileIfThenElse(exp1, exp2, exp3, loc, map, lenv0, entryPoint)
      case Expression.Branch(exp, branches, tpe, loc) =>
        getJvmType(exp.tpe) match {
          case PrimBool => compileBranch[S, MBool](exp, branches, loc, map, lenv0, entryPoint)
          case PrimChar => compileBranch[S, MChar](exp, branches, loc, map, lenv0, entryPoint)
          case PrimByte => compileBranch[S, MByte](exp, branches, loc, map, lenv0, entryPoint)
          case PrimShort => compileBranch[S, MShort](exp, branches, loc, map, lenv0, entryPoint)
          case PrimInt => compileBranch[S, MInt](exp, branches, loc, map, lenv0, entryPoint)
          case PrimLong => compileBranch[S, MLong](exp, branches, loc, map, lenv0, entryPoint)
          case PrimFloat => compileBranch[S, MFloat](exp, branches, loc, map, lenv0, entryPoint)
          case PrimDouble => compileBranch[S, MDouble](exp, branches, loc, map, lenv0, entryPoint)
          case Reference(_) => compileBranch[S, Ref[MObject]](exp, branches, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type" + getJvmType(exp.tpe))
        }

      case Expression.JumpTo(sym, _, loc) =>
        compileJumpTo[S](lenv0(sym), loc)

      case Expression.Let(sym, exp1, exp2, _, loc) =>
        val jvmType = getErasedJvmType(exp1.tpe)
        jvmType match {
          case PrimBool => compileLet[S, MBool, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimChar => compileLet[S, MChar, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimByte => compileLet[S, MByte, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimShort => compileLet[S, MShort, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimInt => compileLet[S, MInt, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimLong => compileLet[S, MLong, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimFloat => compileLet[S, MFloat, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimDouble => compileLet[S, MDouble, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case Reference(_) => compileLet[S, Ref[MObject], T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmType)
        }
      case Expression.LetRec(sym, exp1, exp2, _, _) => ???

      case Expression.Is(_, tag, exp1, loc) =>
        val tagInfo = getTagInfo(exp1.tpe, tag)
        val tagTpe = getErasedJvmType(tagInfo.tagType)
        tagTpe match {
          case PrimBool => compileIs[S, MBool](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimChar => compileIs[S, MChar](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimByte => compileIs[S, MByte](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimShort => compileIs[S, MShort](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimInt => compileIs[S, MInt](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimLong => compileIs[S, MLong](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimFloat => compileIs[S, MFloat](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case PrimDouble => compileIs[S, MDouble](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case Reference(_) => compileIs[S, Ref[MObject]](tagInfo, exp1, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + tagTpe)
        }
      case Expression.Tag(_, tag, exp, tpe, loc) =>
        val tagInfo = getTagInfo(tpe, tag)
        val tagTpe = getErasedJvmType(tagInfo.tagType)
        tagTpe match {
          case PrimBool => compileTag[S, MBool](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimChar => compileTag[S, MChar](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimByte => compileTag[S, MByte](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimShort => compileTag[S, MShort](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimInt => compileTag[S, MInt](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimLong => compileTag[S, MLong](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimFloat => compileTag[S, MFloat](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimDouble => compileTag[S, MDouble](tagInfo, exp, loc, map, lenv0, entryPoint)
          case Reference(_) => compileTag[S, Ref[MObject]](tagInfo, exp, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + tagTpe)
        }
      case Expression.Untag(_, tag, exp, tpe, loc) =>
        val tagInfo = getTagInfo(exp.tpe, tag)
        val jvmTpe = getJvmType(tpe)
        jvmTpe match {
          case PrimBool => compileUntagPrim[S, MBool](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimChar => compileUntagPrim[S, MChar](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimByte => compileUntagPrim[S, MByte](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimShort => compileUntagPrim[S, MShort](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimInt => compileUntagPrim[S, MInt](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimLong => compileUntagPrim[S, MLong](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimFloat => compileUntagPrim[S, MFloat](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimDouble => compileUntagPrim[S, MDouble](tagInfo, exp, loc, map, lenv0, entryPoint)
          case Reference(name) => compileUntagNonPrim[S](tagInfo, exp, Reference(name), loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }

      case Expression.Index(base, offset, tpe, _) =>
        val jvmTpe = getJvmType(tpe)
        jvmTpe match {
          case PrimBool => compileIndexPrim[S, MBool](base, offset, map, lenv0, entryPoint)
          case PrimChar => compileIndexPrim[S, MChar](base, offset, map, lenv0, entryPoint)
          case PrimByte => compileIndexPrim[S, MByte](base, offset, map, lenv0, entryPoint)
          case PrimShort => compileIndexPrim[S, MShort](base, offset, map, lenv0, entryPoint)
          case PrimInt => compileIndexPrim[S, MInt](base, offset, map, lenv0, entryPoint)
          case PrimLong => compileIndexPrim[S, MLong](base, offset, map, lenv0, entryPoint)
          case PrimFloat => compileIndexPrim[S, MFloat](base, offset, map, lenv0, entryPoint)
          case PrimDouble => compileIndexPrim[S, MDouble](base, offset, map, lenv0, entryPoint)
          case Reference(name) => compileIndexNonPrim[S](base, offset, Reference(name), map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }

      case Expression.Tuple(elms, tpe, loc) =>
        compileTuple[S](elms, tpe, loc, map, lenv0, entryPoint)

      case Expression.RecordEmpty(_, loc) =>
        compileRecordEmpty[S](loc, map)
      case Expression.RecordSelect(exp, label, tpe, loc) =>
        val valueTpe = getErasedJvmType(tpe)
        valueTpe match {
          case PrimBool => compileRecordSelect[S, MBool](exp, label, loc, map, lenv0, entryPoint)
          case PrimChar => compileRecordSelect[S, MChar](exp, label, loc, map, lenv0, entryPoint)
          case PrimByte => compileRecordSelect[S, MByte](exp, label, loc, map, lenv0, entryPoint)
          case PrimShort => compileRecordSelect[S, MShort](exp, label, loc, map, lenv0, entryPoint)
          case PrimInt => compileRecordSelect[S, MInt](exp, label, loc, map, lenv0, entryPoint)
          case PrimLong => compileRecordSelect[S, MLong](exp, label, loc, map, lenv0, entryPoint)
          case PrimFloat => compileRecordSelect[S, MFloat](exp, label, loc, map, lenv0, entryPoint)
          case PrimDouble => compileRecordSelect[S, MDouble](exp, label, loc, map, lenv0, entryPoint)
          case Reference(_) => compileRecordSelect[S, Ref[MObject]](exp, label, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + valueTpe)
        }

      case Expression.RecordExtend(label, value, rest, _, loc) =>
        val valueTpe = getErasedJvmType(value.tpe)
        valueTpe match {
          case PrimBool => compileRecordExtend[S, MBool](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimChar => compileRecordExtend[S, MChar](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimByte => compileRecordExtend[S, MByte](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimShort => compileRecordExtend[S, MShort](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimInt => compileRecordExtend[S, MInt](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimLong => compileRecordExtend[S, MLong](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimFloat => compileRecordExtend[S, MFloat](label, value, rest, loc, map, lenv0, entryPoint)
          case PrimDouble => compileRecordExtend[S, MDouble](label, value, rest, loc, map, lenv0, entryPoint)
          case Reference(_) => compileRecordExtend[S, Ref[MObject]](label, value, rest, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + valueTpe)
        }
      case Expression.RecordRestrict(label, rest, _, loc) =>
        compileRecordRestrict[S](label, rest, loc, map, lenv0, entryPoint)

      case Expression.ArrayLit(elms, tpe, loc) => ???
      case Expression.ArrayNew(elm, len, tpe, loc) => ???
      case Expression.ArrayLoad(base, index, tpe, loc) => ???
      case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
      case Expression.ArrayLength(base, tpe, loc) => ???
      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => ???

      case Expression.Ref(exp, tpe, loc) =>
        val jvmTpe = getErasedJvmType(tpe.asInstanceOf[MonoType.Ref].tpe)
        jvmTpe match {
          case PrimBool => compileRef[S, MBool](exp, loc, map, lenv0, entryPoint)
          case PrimChar => compileRef[S, MChar](exp, loc, map, lenv0, entryPoint)
          case PrimByte => compileRef[S, MByte](exp, loc, map, lenv0, entryPoint)
          case PrimShort => compileRef[S, MShort](exp, loc, map, lenv0, entryPoint)
          case PrimInt => compileRef[S, MInt](exp, loc, map, lenv0, entryPoint)
          case PrimLong => compileRef[S, MLong](exp, loc, map, lenv0, entryPoint)
          case PrimFloat => compileRef[S, MFloat](exp, loc, map, lenv0, entryPoint)
          case PrimDouble => compileRef[S, MDouble](exp, loc, map, lenv0, entryPoint)
          case Reference(_) => compileRef[S, Ref[MObject]](exp, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }
      case Expression.Deref(exp, tpe, loc) =>
        val jvmTpe = getJvmType(tpe)
        jvmTpe match {
          case PrimBool => compileDerefPrim[S, MBool](exp, loc, map, lenv0, entryPoint)
          case PrimChar => compileDerefPrim[S, MChar](exp, loc, map, lenv0, entryPoint)
          case PrimByte => compileDerefPrim[S, MByte](exp, loc, map, lenv0, entryPoint)
          case PrimShort => compileDerefPrim[S, MShort](exp, loc, map, lenv0, entryPoint)
          case PrimInt => compileDerefPrim[S, MInt](exp, loc, map, lenv0, entryPoint)
          case PrimLong => compileDerefPrim[S, MLong](exp, loc, map, lenv0, entryPoint)
          case PrimFloat => compileDerefPrim[S, MFloat](exp, loc, map, lenv0, entryPoint)
          case PrimDouble => compileDerefPrim[S, MDouble](exp, loc, map, lenv0, entryPoint)
          case Reference(name) => compileDerefNonPrim[S](exp, Reference(name), loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }


      case Expression.Assign(exp1, exp2, _, loc) =>
        val jvmTpe = getErasedJvmType(exp1.tpe.asInstanceOf[MonoType.Ref].tpe)
        jvmTpe match {
          case PrimBool => compileAssign[S, MBool](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimChar => compileAssign[S, MChar](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimByte => compileAssign[S, MByte](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimShort => compileAssign[S, MShort](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimInt => compileAssign[S, MInt](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimLong => compileAssign[S, MLong](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimFloat => compileAssign[S, MFloat](exp1, exp2, loc, map, lenv0, entryPoint)
          case PrimDouble => compileAssign[S, MDouble](exp1, exp2, loc, map, lenv0, entryPoint)
          case Reference(_) => compileAssign[S, Ref[MObject]](exp1, exp2, loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }

      case Expression.HandleWith(exp, bindings, tpe, loc) => ???
      case Expression.Existential(_, _, loc) =>
        compileExistential(loc)
      case Expression.Universal(_, _, loc) =>
        compileUniversal(loc)
      case Expression.TryCatch(exp, rules, tpe, loc) => ???
      case Expression.NativeConstructor(constructor, args, tpe, loc) => ???
      case Expression.NativeField(field, tpe, loc) => ???
      case Expression.NativeMethod(method, args, tpe, loc) =>

        val declaration = asm.Type.getInternalName(method.getDeclaringClass)
        val name = method.getName
        val descriptor = asm.Type.getMethodDescriptor(method)
        val invokeInsn = if (Modifier.isStatic(method.getModifiers)) JvmModifier.InvokeStatic else JvmModifier.InvokeVirtual


          // Adding source line number for debugging
          ADD_SOURCE_LINE[S](loc) |>>
            // Evaluate arguments left-to-right and push them onto the stack.
            args.foldLeft(NO_OP[S]) {
              case (ins, arg) =>
                val jvmTpe = getErasedJvmType(arg.tpe)
                ins |>> (jvmTpe match {
                  case PrimBool => compileExpression[S, MBool](arg, map, lenv0, entryPoint)
                  case PrimChar => compileExpression[S, MChar](arg, map, lenv0, entryPoint)
                  case PrimByte => compileExpression[S, MByte](arg, map, lenv0, entryPoint)
                  case PrimShort => compileExpression[S, MShort](arg, map, lenv0, entryPoint)
                  case PrimInt => compileExpression[S, MInt](arg, map, lenv0, entryPoint)
                  case PrimLong => compileExpression[S, MLong](arg, map, lenv0, entryPoint)
                  case PrimFloat => compileExpression[S, MFloat](arg, map, lenv0, entryPoint)
                  case PrimDouble => compileExpression[S, MDouble](arg, map, lenv0, entryPoint)
                  case Reference(_) => compileExpression[S, Ref[MObject]](arg, map, lenv0, entryPoint)
                  case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
                }).asInstanceOf[F[S] => F[S]]
            } |>>
            new UncheckedVoidMethod2(invokeInsn, declaration, name, descriptor).INVOKE |>>
            (if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
              Api.Java.Runtime.Value.Unit.getInstance.INVOKE
            } else {
              NO_OP
            }).asInstanceOf[F[S] => F[S ** T1]]
      case Expression.NewChannel(exp, tpe, loc) =>
        compileNewChannel(exp, loc, map, lenv0, entryPoint)
      case Expression.GetChannel(exp, tpe, loc) => ???
      case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
      case Expression.SelectChannel(rules, default, tpe, loc) => ???

      case Expression.Spawn(exp, _, loc) =>
        compileSpawn(exp, loc, map, lenv0, entryPoint)
      case Expression.Sleep(exp, tpe, loc) => ???

      case Expression.FixpointConstraint(con, tpe, loc) => ???
      case Expression.FixpointCompose(exp1, exp2, _, loc) =>
        compileFixpointCompose(exp1, exp2, loc, map, lenv0, entryPoint)
      case Expression.FixpointSolve(uid, exp, stf, tpe, loc) => ???
      case Expression.FixpointProject(pred, exp, tpe, loc) => ???
      case Expression.FixpointEntails(exp1, exp2, _, loc) =>
        compileFixpointEntails(exp1, exp2, loc, map, lenv0, entryPoint)
      case Expression.UserError(_, loc) =>
        compileUserError(loc)
      case Expression.HoleError(sym, _, loc) =>
        compileHoleError(sym.toString, loc)
      case Expression.MatchError(_, loc) =>
        compileMatchError(loc)
      case Expression.SwitchError(_, loc) =>
        compileSwitchError(loc)

    }).asInstanceOf[F[S] => F[S ** T1]]
  }

  def compileUnit[S <: Stack](implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MUnit]] = Api.Java.Runtime.Value.Unit.getInstance.INVOKE

  def compileBoolean[S <: Stack](exp: Expression): F[S] => F[S ** MBool] =
    exp match {
      case Expression.True => TRUE
      case Expression.False => FALSE
      case _ => throw InternalCompilerException("Unexpected expression" + exp)
    }

  def compileChar[S <: Stack](c: Char): F[S] => F[S ** MChar] = compileInt(c).asInstanceOf[F[S] => F[S ** MChar]]

  def compileFloat[S <: Stack](f: Float): F[S] => F[S ** MFloat] =
    f match {
      case 0f => FCONST(0)
      case 1f => FCONST(1)
      case 2f => FCONST(2)
      case _ => LDC_FLOAT(f)
    }

  def compileDouble[S <: Stack](d: Double): F[S] => F[S ** MDouble] =
    d match {
      case 0d => DCONST(0)
      case 1d => DCONST(1)
      case _ => LDC_DOUBLE(d)
    }

  def compileByte[S <: Stack](b: Byte): F[S] => F[S ** MByte] = {
    (b match {
      case _ if b >= -1 && b <= 5 => ICONST(b)
      case _ => BIPUSH(b)
    }).asInstanceOf[F[S] => F[S ** MByte]]
  }

  def compileShort[S <: Stack](s: Short): F[S] => F[S ** MShort] = {
    (s match {
      case _ if scala.Byte.MinValue <= s && s <= scala.Byte.MaxValue => compileByte(s.asInstanceOf[Byte])
      case _ => SIPUSH(s)
    }).asInstanceOf[F[S] => F[S ** MShort]]
  }

  def compileInt[S <: Stack](i: Int): F[S] => F[S ** MInt] = {
    (i match {
      case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => compileShort(i.asInstanceOf[Short])
      case _ => LDC_INT(i)
    }).asInstanceOf[F[S] => F[S ** MInt]]
  }

  def compileLong[S <: Stack](l: Long): F[S] => F[S ** MLong] = {
    l match {
      case 0 => LCONST(0)
      case 1 => LCONST(1)
      case _ if scala.Int.MinValue <= l && l <= scala.Int.MaxValue =>
        compileInt[S](l.asInstanceOf[Int]) |>>
          I2L
      case _ => LDC_LONG(l)
    }
  }

  def compileBigInt[S <: Stack](ii: java.math.BigInteger)(implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MBigInt]] = {
    NEW[S, Ref[MBigInt]](NJvmType.BigInteger) |>>
      DUP |>>
      LDC_STRING(ii.toString) |>>
      Api.Java.Math.BigIntger.constructor.INVOKE
  }

  def compileString[S <: Stack](s: String): F[S] => F[S ** Ref[MString]] = LDC_STRING(s)

  def compileVar[S <: Stack, T1 <: MnemonicsTypes : TypeTag](sym: Symbol.VarSym, tpe: MonoType)(implicit root: Root, flix: Flix): F[S] => F[S ** T1] = {
    val jvmType = getJvmType(tpe)
    val offset = sym.getStackOffset + 3 // This is `+3` because the first 2 are reserved!
    jvmType match {
      case PrimBool | PrimChar | PrimByte | PrimShort | PrimInt | PrimLong |
           PrimFloat | PrimDouble =>
        new Local[T1](offset).LOAD[S]
      case Reference(name) =>
        (new Local[Ref[MObject]](offset).LOAD[S] |>>
          CHECK_CAST(Reference(name))).asInstanceOf[F[S] => F[S ** T1]]
      case _ => throw InternalCompilerException(s"Unexpected type " + jvmType)
    }
  }

  def compileClosure[S <: Stack]
  (closure: ClosureInfo, freeVars: List[FinalAst.FreeVar], loc : SourceLocation,map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[Closure]]=
  {
    val jvmType = getClosureClassType(closure)
    val varTypes = freeVars.map(_.tpe).map(getErasedJvmType)
    val closureConstructor = new UncheckedVoidMethod(JvmModifier.InvokeSpecial, jvmType, "<init>", NJvmType.Object +: varTypes)
    val contextLocal = new Local[Ref[Context]](1)
    NEW[S, Ref[Closure]](jvmType) |>>
      DUP |>>
      contextLocal.LOAD |>>
      freeVars.foldLeft(NO_OP[S ** Ref[Closure] **Ref[Closure] ** Ref[Context] ]){
        case (ins, f) =>
          val v = Expression.Var(f.sym, f.tpe, loc)
         ins|>> (getJvmType(f.tpe) match {
            case PrimBool => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MBool](v, map, lenv0, entryPoint)
            case PrimChar => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MChar](v, map, lenv0, entryPoint)
            case PrimByte => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MByte](v, map, lenv0, entryPoint)
            case PrimShort => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MShort](v, map, lenv0, entryPoint)
            case PrimInt => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MInt](v, map, lenv0, entryPoint)
            case PrimLong => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MLong](v, map, lenv0, entryPoint)
            case PrimFloat => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MFloat](v, map, lenv0, entryPoint)
            case PrimDouble => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], MDouble](v, map, lenv0, entryPoint)
            case Reference(_) => ins |>> compileExpression[S ** Ref[Closure] **Ref[Closure] ** Ref[Context], Ref[MObject]](v, map, lenv0, entryPoint)
            case _ => throw InternalCompilerException("Unexpected type" + getJvmType(f.tpe))
          }).asInstanceOf[F[S ** Ref[Closure] **Ref[Closure] ** Ref[Context]] => F[S ** Ref[Closure] **Ref[Closure] ** Ref[Context]]]
      }|>> closureConstructor.INVOKE

  }

  def compileApplyClo[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (exp: Expression, args: List[Expression], tpe: MonoType, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):   F[S] => F[S ** T1] = {

    val retTpe = getJvmType(tpe)

    val cont =
      retTpe match {
        case PrimBool =>
          getContinuationInterfaceType[MBool]
        case PrimChar =>
          getContinuationInterfaceType[MChar]
        case PrimByte =>
          getContinuationInterfaceType[MByte]
        case PrimShort =>
          getContinuationInterfaceType[MShort]
        case PrimInt =>
          getContinuationInterfaceType[MInt]
        case PrimLong =>
          getContinuationInterfaceType[MLong]
        case PrimFloat =>
          getContinuationInterfaceType[MFloat]
        case PrimDouble =>
          getContinuationInterfaceType[MDouble]
        case Reference(_) =>
          getContinuationInterfaceType[Ref[MObject]]
        case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
      }


    // Type of the function
    val MonoType.Arrow(targs, tresult) = exp.tpe.asInstanceOf[MonoType.Arrow]
    // Type of the function interface
    val functionInterface = getFunctionInterfaceType(targs.map(getErasedJvmType), getErasedJvmType(tresult))

    val functionInterfaceClass = map(functionInterface.name).asInstanceOf[FunctionInterface]

    val contextLocal = new Local[Ref[Context]](1)

    //generate the capability to acess the continuation field in the context class
    val continuationField = new ClassField[Ref[FunctionInterface]]("continuation", NJvmType.Context)
    val continuationField2 = new UncheckedClassField("continuation", getContextClassType, NJvmType.Object)


    contextLocal.LOAD[S] |>>
    compileExpression[S ** Ref[Context], Ref[MObject]](exp, map, lenv0, entryPoint) |>>
    CHECK_CAST2[S ** Ref[Context], Ref[MObject], Ref[FunctionInterface]](functionInterface) |>>
      args.zipWithIndex.foldLeft(NO_OP[S ** Ref[Context] ** Ref[FunctionInterface]]) {
        case (ins, (arg, ind)) =>
          val argErasedType = getErasedJvmType(arg.tpe)
          ins |>>
            DUP |>>
            (argErasedType match {
              case PrimBool =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MBool](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MBool](ind).INVOKE
              case PrimChar =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MChar](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MChar](ind).INVOKE
              case PrimByte =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MByte](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MByte](ind).INVOKE
              case PrimShort =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MShort](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MShort](ind).INVOKE
              case PrimInt =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MInt](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MInt](ind).INVOKE
              case PrimLong =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MLong](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MLong](ind).INVOKE
              case PrimFloat =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MFloat](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MFloat](ind).INVOKE
              case PrimDouble =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MDouble](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MDouble](ind).INVOKE
              case Reference(_) =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], Ref[MObject]](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[Ref[MObject]](ind).INVOKE
              case _ => throw InternalCompilerException("Unexpected type" + argErasedType)
            })
      }|>>
      continuationField.PUT_FIELD[S, Ref[Context]] |>>
      IFNONNULL(
        contextLocal.LOAD[S] |>>
          continuationField2.GET_FIELD[S,Ref[Context], Ref[MObject]] |>>

          contextLocal.LOAD[S ** Ref[MObject]] |>>
          CONST_NULL[S ** Ref[MObject] ** Ref[Context], Ref[MObject]] |>>
          continuationField2.PUT_FIELD[S ** Ref[MObject], Ref[Context], Ref[MObject]] |>>
          (retTpe match {
            case PrimBool =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MBool]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimChar =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MChar]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimByte =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].invokeMethod
              CHECK_CAST2[S, Ref[MObject], Ref[ContinuationInterface[MByte]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimShort =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MShort]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimInt =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MInt]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimLong =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MLong]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimFloat =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MFloat]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimDouble =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MDouble]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case Reference(_) =>
              val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[Ref[MObject]]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
          }) |>>

          contextLocal.LOAD |>>
          continuationField2.GET_FIELD
      ) |>>
      (retTpe match {
        case PrimBool =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimChar =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimByte =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimShort =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimInt =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimLong =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimFloat =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimDouble =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case Reference(name) =>
          val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE |>>
            CHECK_CAST(Reference(name))
        case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
      }).asInstanceOf[F[S] => F[S ** T1]]
  }


  def compileApplyDef[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (name: Symbol.DefnSym, args: List[Expression], tpe: MonoType, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):   F[S] => F[S ** T1] = {
    // Namespace of the Def
    val ns = getNamespace(name)
    // JvmType of `ns`
    val nsJvmType = getNamespaceClassType(ns)
    // Name of the field for `ns` on `Context`
    val nsFieldName = getNamespaceFieldNameInContextClass(ns)
    // Field for Def on `ns`
    val defFiledName = getDefFieldNameInNamespaceClass(name)
    // JvmType of Def
    val defJvmType = getFunctionDefinitionClassType(name)

    // Type of the function
    val MonoType.Arrow(targs, tresult) = root.defs(name).tpe.asInstanceOf[MonoType.Arrow]
    // Type of the continuation interface
    val retTpe = getJvmType(tpe)

    val cont =
      retTpe match {
        case PrimBool =>
          getContinuationInterfaceType[MBool]
        case PrimChar =>
          getContinuationInterfaceType[MChar]
        case PrimByte =>
          getContinuationInterfaceType[MByte]
        case PrimShort =>
          getContinuationInterfaceType[MShort]
        case PrimInt =>
          getContinuationInterfaceType[MInt]
        case PrimLong =>
          getContinuationInterfaceType[MLong]
        case PrimFloat =>
          getContinuationInterfaceType[MFloat]
        case PrimDouble =>
          getContinuationInterfaceType[MDouble]
        case Reference(_) =>
          getContinuationInterfaceType[Ref[MObject]]
        case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
      }
    // Type of the function interface
    val functionInterface = getFunctionInterfaceType(targs.map(getErasedJvmType), getErasedJvmType(tresult))

    val functionInterfaceClass = map(functionInterface.name).asInstanceOf[FunctionInterface]

    val contextLocal = new Local[Ref[Context]](1)
    //generate the capability to acess the namespace field in the context class
    val nsField = new UncheckedClassField(nsFieldName, NJvmType.Context, nsJvmType)
    //generate the capability to acess the definition field in the namespace class
    val defField = new UncheckedClassField(defFiledName, nsJvmType, defJvmType)

    //generate the capability to acess the continuation field in the context class
    val continuationField = new ClassField[Ref[FunctionInterface]]("continuation", NJvmType.Context)
    val continuationField2 = new UncheckedClassField("continuation", getContextClassType, NJvmType.Object)


    contextLocal.LOAD[S] |>>
      contextLocal.LOAD[S ** Ref[Context]] |>>
      nsField.GET_FIELD[S ** Ref[Context], Ref[Context], Ref[MObject]] |>>
      defField.GET_FIELD[S ** Ref[Context], Ref[MObject], Ref[MObject]] |>>
      CHECK_CAST2[S ** Ref[Context], Ref[MObject], Ref[FunctionInterface]](functionInterface) |>>
      args.zipWithIndex.foldLeft(NO_OP[S ** Ref[Context] ** Ref[FunctionInterface]]) {
        case (ins, (arg, ind)) =>
          val argErasedType = getErasedJvmType(arg.tpe)
          ins |>>
            DUP |>>
            (argErasedType match {
              case PrimBool =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MBool](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MBool](ind).INVOKE
              case PrimChar =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MChar](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MChar](ind).INVOKE
              case PrimByte =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MByte](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MByte](ind).INVOKE
              case PrimShort =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MShort](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MShort](ind).INVOKE
              case PrimInt =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MInt](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MInt](ind).INVOKE
              case PrimLong =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MLong](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MLong](ind).INVOKE
              case PrimFloat =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MFloat](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MFloat](ind).INVOKE
              case PrimDouble =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MDouble](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MDouble](ind).INVOKE
              case Reference(_) =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], Ref[MObject]](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[Ref[MObject]](ind).INVOKE
              case _ => throw InternalCompilerException("Unexpected type" + argErasedType)
            })
      }|>>
      continuationField.PUT_FIELD[S, Ref[Context]] |>>
      IFNONNULL(
        contextLocal.LOAD[S] |>>
          continuationField2.GET_FIELD[S,Ref[Context], Ref[MObject]] |>>

          contextLocal.LOAD[S ** Ref[MObject]] |>>
          CONST_NULL[S ** Ref[MObject] ** Ref[Context], Ref[MObject]] |>>
          continuationField2.PUT_FIELD[S ** Ref[MObject], Ref[Context], Ref[MObject]] |>>
          (retTpe match {
            case PrimBool =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MBool]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimChar =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MChar]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimByte =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].invokeMethod
              CHECK_CAST2[S, Ref[MObject], Ref[ContinuationInterface[MByte]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimShort =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MShort]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimInt =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MInt]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimLong =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MLong]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimFloat =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MFloat]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case PrimDouble =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[MDouble]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case Reference(_) =>
              val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
              val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].invokeMethod
              CHECK_CAST2[S , Ref[MObject], Ref[ContinuationInterface[Ref[MObject]]]](cont) |>>
                DUP |>>
                ifoLocal.STORE |>>
                contextLocal.LOAD |>>
                invokeMethod.INVOKE
            case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
          }) |>>

          contextLocal.LOAD |>>
          continuationField2.GET_FIELD
      ) |>>
      (retTpe match {
        case PrimBool =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimChar =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimByte =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimShort =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimInt =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimLong =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimFloat =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case PrimDouble =>
          val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE
        case Reference(name) =>
          val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
          val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].getResultMethod
          ifoLocal.LOAD[S] |>>
            getResultMethod.INVOKE |>>
            CHECK_CAST(Reference(name))
        case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
      }).asInstanceOf[F[S] => F[S ** T1]]
  }

  def compileApplyEff[S <: Stack, T1 <: MnemonicsTypes : TypeTag]: F[S] => F[S ** T1] =
    throw InternalCompilerException(s"ApplyEff not implemented in JVM backend!")

  def compileApplyCloTail[S <: Stack, T <: MnemonicsTypes : TypeTag]
  (exp: Expression, args: List[Expression], tpe: MonoType, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix) : F[S] => F[S ** T]= {
    // Type of the function interface
    val MonoType.Arrow(targs, tresult) = exp.tpe.asInstanceOf[MonoType.Arrow]
    val functionInterface = getFunctionInterfaceType(targs.map(getErasedJvmType), getErasedJvmType(tresult))
    // Result type
    val continuationField = new ClassField[Ref[FunctionInterface]]("continuation", NJvmType.Context)

    val contextLocal = new Local[Ref[Context]](1)
    val functionInterfaceClass = map(functionInterface.name).asInstanceOf[FunctionInterface]

    contextLocal.LOAD[S] |>>
      compileExpression[S ** Ref[Context], Ref[MObject]](exp, map,lenv0, entryPoint) |>>
      CHECK_CAST2[S ** Ref[Context], Ref[MObject], Ref[FunctionInterface]](functionInterface) |>>
      args.zipWithIndex.foldLeft(NO_OP[S ** Ref[Context] ** Ref[FunctionInterface]]) {
        case (ins, (arg, ind)) =>
          val argErasedType = getErasedJvmType(arg.tpe)
          ins |>>
            DUP |>>
            (argErasedType match {
              case PrimBool =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MBool](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MBool](ind).INVOKE
              case PrimChar =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MChar](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MChar](ind).INVOKE
              case PrimByte =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MByte](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MByte](ind).INVOKE
              case PrimShort =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MShort](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MShort](ind).INVOKE
              case PrimInt =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MInt](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MInt](ind).INVOKE
              case PrimLong =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MLong](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MLong](ind).INVOKE
              case PrimFloat =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MFloat](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MFloat](ind).INVOKE
              case PrimDouble =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], MDouble](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[MDouble](ind).INVOKE
              case Reference(_) =>
                compileExpression[S ** Ref[Context] ** Ref[FunctionInterface] ** Ref[FunctionInterface], Ref[MObject]](arg, map, lenv0, entryPoint) |>>
                  functionInterfaceClass.setArgMethod[Ref[MObject]](ind).INVOKE
              case _ => throw InternalCompilerException("Unexpected type" + argErasedType)
            })
      }|>>
      continuationField.PUT_FIELD |>>
      pushDummyValue[S, T]
  }

  def compileInt32Op[S <: Stack, T <: MnemonicsTypes]
  (op: SemanticOperator.Int32Op, exp1 : Expression, exp2 : Expression, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** T] = {

    compileExpression[S, MInt](exp1, map, lenv0, entryPoint) |>>
    compileExpression[S ** MInt, MInt](exp1, map, lenv0, entryPoint) |>>
    (op match {
      case Int32Op.Neg => ???
      case Int32Op.Not => ???
      case Int32Op.Add => IADD
      case Int32Op.Sub => ???
      case Int32Op.Mul => ???
      case Int32Op.Div => ???
      case Int32Op.Rem => ???
      case Int32Op.Exp => ???
      case Int32Op.And => ???
      case Int32Op.Or => ???
      case Int32Op.Xor => ???
      case Int32Op.Shl => ???
      case Int32Op.Shr => ???
      case Int32Op.Eq => ???
      case Int32Op.Neq => ???
      case Int32Op.Lt => ???
      case Int32Op.Le => ???
      case Int32Op.Gt => ???
      case Int32Op.Ge => IGE
    }).asInstanceOf[F[S ** MInt ** MInt] => F[S ** T]]
  }

  def compileIfThenElse[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (exp1: FinalAst.Expression, exp2: FinalAst.Expression, exp3: FinalAst.Expression, loc: SourceLocation,
   map: Map[JvmName, Mnemonics.MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** T1] =

    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, MBool](exp1, map, lenv0, entryPoint) |>>
      IFEQ_ELSE(
        compileExpression(exp2, map, lenv0, entryPoint),
        compileExpression(exp3, map, lenv0, entryPoint)
      )

  def compileBranch[S <: Stack, T <: MnemonicsTypes : TypeTag]
  (exp: Expression, branches: Map[Symbol.LabelSym, Expression], loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S] = {
    val updatedJumpLabels = branches.foldLeft(lenv0)((map, branch) => map + (branch._1 -> new Label()))
    val endLabel = new Label()
    val mEndLabel = new MLabel(endLabel)

    ADD_SOURCE_LINE[S](loc) |>>
    compileExpression[S, T](exp, map, updatedJumpLabels, entryPoint) |>>
    mEndLabel.GOTO[S ** T] |>>
      branches.foldLeft(NO_OP[S ** T]) { case (ins, (sym, branchExp)) =>
        val mUpdatedLabel = new MLabel(updatedJumpLabels(sym))
        ins |>>
          mUpdatedLabel.EMIT_LABEL[S ** T] |>>
          (getJvmType(branchExp.tpe) match {
          case PrimBool => compileExpression[S ** T, MBool](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimChar => compileExpression[S ** T, MChar](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimByte => compileExpression[S ** T, MByte](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimShort => compileExpression[S ** T, MShort](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimInt =>  compileExpression[S ** T, MInt](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimLong => compileExpression[S ** T, MLong](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimFloat => compileExpression[S ** T, MFloat](branchExp, map, updatedJumpLabels, entryPoint)
          case PrimDouble => compileExpression[S ** T, MDouble](branchExp, map, updatedJumpLabels, entryPoint)
          case Reference(_) => compileExpression[S ** T, Ref[MObject]](branchExp, map, updatedJumpLabels, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type" + getJvmType(branchExp.tpe))
          }).asInstanceOf[F[S ** T] => F[S ** T]] |>>
          mEndLabel.GOTO
      } |>>
      mEndLabel.GOTO[S ** T] |>>
      mEndLabel.EMIT_LABEL[S**T].asInstanceOf[F[S**T] => F[S]]
  }

  def compileJumpTo[S <: Stack](label: Label, loc: SourceLocation): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      new MLabel(label).GOTO


  def compileLet[S <: Stack, T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag]
  (sym: Symbol.VarSym, exp1: Expression, exp2: Expression, loc: SourceLocation,
   map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S**T2]= {
    val letVar = new Local[T1](sym.getStackOffset + 3)

    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, T1](exp1, map, lenv0, entryPoint) |>>
      letVar.STORE |>>
      compileExpression[S,T2](exp2, map, lenv0, entryPoint)
  }

  def compileIs[S <: Stack, T <: MnemonicsTypes : TypeTag]
  (tagInfo: TagInfo, exp: FinalAst.Expression, loc: SourceLocation, map: Map[JvmName, Mnemonics.MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S ** MBool] = {
    val tagTpe = getTagClassType(tagInfo)
      ADD_SOURCE_LINE[S](loc) |>>
        compileExpression[S, Ref[TagClass[T]]](exp, map, lenv0, entryPoint) |>>
        INSTANCE_OF(tagTpe)
  }


  def compileTag[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (tagInfo : TagInfo, exp: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[TagClass[T1]]] = {

        val classType = getTagClassType(tagInfo)
        val tagClass = map(classType.name).asInstanceOf[TagClass[T1]]

        ADD_SOURCE_LINE[S](loc) |>>
        (tagClass.instance match {
        case Some(field) => field.GET_STATIC[S, Ref[TagClass[T1]]]
        case None =>
          NEW[S, Ref[TagClass[T1]]](classType) |>>
          DUP |>>
          compileExpression[S** Ref[TagClass[T1]] **  Ref[TagClass[T1]], T1](exp, map, lenv0, entryPoint)|>>
          tagClass.defaultConstructor.INVOKE
        })
    }

  def compileUntagPrim[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (tagInfo : TagInfo, exp: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S**T1] = {

    val classType = getTagClassType(tagInfo)
    val tagClass = map(classType.name).asInstanceOf[TagClass[T1]]

    ADD_SOURCE_LINE[S](loc) |>>
    compileExpression[S, Ref[TagClass[T1]]](exp, map, lenv0, entryPoint) |>>
    CHECK_CAST2[S, Ref[TagClass[T1]], Ref[TagClass[T1]]](classType) |>>
    tagClass.getValueMethod.INVOKE
  }

  def compileUntagNonPrim[S <: Stack]
  (tagInfo : TagInfo, exp: Expression, tpe : Reference, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S**Ref[MObject]] = {

    val classType = getTagClassType(tagInfo)
    val tagClass = map(classType.name).asInstanceOf[TagClass[Ref[MObject]]]

    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, Ref[TagClass[Ref[MObject]]]](exp, map, lenv0, entryPoint) |>>
      CHECK_CAST2[S, Ref[TagClass[Ref[MObject]]], Ref[TagClass[Ref[MObject]]]](classType) |>>
      tagClass.getValueMethod.INVOKE |>>
      CHECK_CAST[S](tpe)
  }

  def compileIndexPrim[S <: Stack, T1 <:MnemonicsTypes : TypeTag]
  (base: Expression, offset: Int, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** T1] =
  {
    val MonoType.Tuple(elms) = base.tpe.asInstanceOf[MonoType.Tuple]
    val classType = getTupleInterfaceType(elms.map(getErasedJvmType))
    val tupleIFace = map(classType.name).asInstanceOf[TupleInterface]

    compileExpression[S, Ref[TupleInterface]](base, map, lenv0, entryPoint) |>>
      tupleIFace.getIndexMethod[T1](offset).INVOKE
  }

  def compileIndexNonPrim[S <: Stack]
  (base: Expression, offset: Int, ct: Reference, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MObject]] =
  {
    val MonoType.Tuple(elms) = base.tpe.asInstanceOf[MonoType.Tuple]
    val classType = getTupleInterfaceType(elms.map(getErasedJvmType))
    val tupleIFace = map(classType.name).asInstanceOf[TupleInterface]

    compileExpression[S, Ref[TupleInterface]](base, map, lenv0, entryPoint) |>>
      tupleIFace.getIndexMethod[Ref[MObject]](offset).INVOKE |>>
      CHECK_CAST(ct)
  }

  def compileTuple[S <: Stack](elms: List[Expression], tpe: MonoType, loc: SourceLocation,
                               map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
                              (implicit root: Root, flix: Flix): F[S] => F[S ** Ref[TupleClass]] = {
    val MonoType.Tuple(erasedElms) = tpe
    val tupleClassType = getTupleClassType(erasedElms.map(getErasedJvmType))

    val tupleClass = map(tupleClassType.name).asInstanceOf[TupleClass]

    ADD_SOURCE_LINE[S](loc) |>>
      NEW[S, Ref[TupleClass]](tupleClassType) |>>
      DUP |>>
      elms.foldLeft(NO_OP[S ** Ref[TupleClass] ** Ref[TupleClass]]) {
        case (ins, exp) =>
          ins |>>
            (getErasedJvmType(exp.tpe) match {
              case PrimBool => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MBool](exp, map, lenv0, entryPoint)
              case PrimChar => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MChar](exp, map, lenv0, entryPoint)
              case PrimByte => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MByte](exp, map, lenv0, entryPoint)
              case PrimShort => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MShort](exp, map, lenv0, entryPoint)
              case PrimInt => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MInt](exp, map, lenv0, entryPoint)
              case PrimLong => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MLong](exp, map, lenv0, entryPoint)
              case PrimFloat => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MFloat](exp, map, lenv0, entryPoint)
              case PrimDouble => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass], MDouble](exp, map, lenv0, entryPoint)
              case Reference(_) => compileExpression[S ** Ref[TupleClass] ** Ref[TupleClass] , Ref[MObject]](exp, map, lenv0, entryPoint)
              case _ => throw InternalCompilerException("Unexpected type" + getJvmType(exp.tpe))
            }).asInstanceOf[F[S ** Ref[TupleClass] ** Ref[TupleClass]] => F[S ** Ref[TupleClass] ** Ref[TupleClass]]]

      } |>>
      tupleClass.defaultConstructor.INVOKE
  }

  def compileRecordEmpty[S <: Stack](loc: SourceLocation, map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[RecordEmpty]] = {
    val recEmptClassTpe = getRecordEmptyClassType
    val recEmptClass = map(recEmptClassTpe.name).asInstanceOf[RecordEmpty]

    ADD_SOURCE_LINE[S](loc) |>>
      NEW[S, Ref[RecordEmpty]](recEmptClassTpe) |>>
      DUP |>>
      recEmptClass.defaultConstructor.INVOKE
  }


  def compileRecordSelect[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (exp: Expression, label: String, loc: SourceLocation, map: Map[JvmName, MnemonicsClass],
   lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** T1] = {

    // Get the correct record extend class, given the expression type 'tpe'
    // We get the JvmType of the extended record class to call the proper getField
    val classType = getRecordExtendClassType[T1]

    // We get the JvmType of the record interface
    val interfaceType = getRecordInterfaceType

    val recordExtend = map(classType.name).asInstanceOf[RecordExtend[T1]]
    val recordInterface = map(interfaceType.name).asInstanceOf[RecordInterface]


    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, Ref[RecordInterface]](exp, map, lenv0, entryPoint) |>>
      LDC_STRING(label) |>>
      recordInterface.lookupFieldMethod.INVOKE |>>
      CHECK_CAST2[S, Ref[RecordInterface], Ref[RecordExtend[T1]]](classType) |>>
      recordExtend.getFieldMethod.INVOKE
  }

  def compileRecordExtend[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (label: String, value: Expression, rest: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[RecordExtend[T1]]] = {
    val classTpe = getRecordExtendClassType[T1]
    val recClass = map(classTpe.name).asInstanceOf[RecordExtend[T1]]
    ADD_SOURCE_LINE[S](loc) |>>
      NEW[S, Ref[RecordExtend[T1]]](classTpe) |>>
      DUP |>>
      LDC_STRING(label) |>>
      compileExpression
        [S ** Ref[RecordExtend[T1]] ** Ref[RecordExtend[T1]] ** Ref[MString], T1](value, map, lenv0, entryPoint) |>>
      compileExpression
        [S ** Ref[RecordExtend[T1]] ** Ref[RecordExtend[T1]] ** Ref[MString] ** T1, Ref[RecordInterface]](rest, map, lenv0, entryPoint) |>>
      recClass.defaultConstructor.INVOKE
  }

  def compileRecordRestrict[S <: Stack]
  (label: String, rest: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** Ref[RecordInterface]] = {

    val interfaceType = getRecordInterfaceType
    val recordInterface = map(interfaceType.name).asInstanceOf[RecordInterface]

    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, Ref[RecordInterface]](rest, map, lenv0, entryPoint) |>>
      LDC_STRING(label) |>>
      recordInterface.restrictFieldMethod.INVOKE
  }


  def compileRef[S<: Stack, T <: MnemonicsTypes : TypeTag]
  (exp: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[RefClass[T]]] = {
    val classTpe = getRefClassType[T]
    val refClass = map(classTpe.name).asInstanceOf[RefClass[T]]

    ADD_SOURCE_LINE[S](loc) |>>
      NEW[S, Ref[RefClass[T]]](classTpe) |>>
      DUP |>>
      compileExpression[S**Ref[RefClass[T]] ** Ref[RefClass[T]], T](exp, map, lenv0, entryPoint) |>>
      refClass.defaultConstructor.INVOKE
  }

  def compileDerefPrim[S <: Stack, T <:MnemonicsTypes : TypeTag]
  (exp: Expression, loc : SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** T] =
  {
    val classTpe = getRefClassType[T]
    val refClass = map(classTpe.name).asInstanceOf[RefClass[T]]

    ADD_SOURCE_LINE[S](loc)
    compileExpression[S, Ref[RefClass[T]]](exp, map, lenv0, entryPoint) |>>
    refClass.getValueMethod.INVOKE
  }

  def compileDerefNonPrim[S <: Stack]
  (exp: Expression, ct: Reference, loc : SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MObject]] =
  {
    val classTpe = getRefClassType[Ref[MObject]]
    val refClass = map(classTpe.name).asInstanceOf[RefClass[Ref[MObject]]]

    ADD_SOURCE_LINE[S](loc)
    compileExpression[S, Ref[RefClass[Ref[MObject]]]](exp, map, lenv0, entryPoint) |>>
    refClass.getValueMethod.INVOKE |>>
    CHECK_CAST(ct)
  }

  def compileAssign[S <: Stack, T <: MnemonicsTypes : TypeTag]
  (exp1: Expression, exp2: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[MUnit]] =
  {
    val classTpe = getRefClassType[T]
    val refClass = map(classTpe.name).asInstanceOf[RefClass[T]]
    ADD_SOURCE_LINE[S](loc) |>>
    compileExpression[S, Ref[RefClass[T]]](exp1, map, lenv0, entryPoint) |>>
    compileExpression[S ** Ref[RefClass[T]], T](exp2, map, lenv0, entryPoint) |>>
    refClass.setValueMethod.INVOKE |>>
    Api.Java.Runtime.Value.Unit.getInstance.INVOKE
  }

  def compileNewChannel[S <: Stack]
  (exp: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S** Ref[MChannel]]={
    ADD_SOURCE_LINE[S](loc) |>>
    NEW[S, Ref[MChannel]](Reference(JvmName.Channel)) |>>
    DUP |>>
    compileExpression[S** Ref[MChannel] ** Ref[MChannel], MInt](exp, map, lenv0, entryPoint) |>>
    Api.Java.Runtime.Channel.constructor.INVOKE
  }




  def compileSpawn[S <: Stack]
  (exp: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S** Ref[MUnit]]={
    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, Ref[MSpawn]](exp, map, lenv0, entryPoint) |>>
      Api.Java.Runtime.Channel.spawn.INVOKE |>>
      Api.Java.Runtime.Value.Unit.getInstance.INVOKE
  }


  def compileFixpointCompose[S <: Stack]
  (exp1: Expression, exp2: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S** Ref[MConstraintSystem]]={
    ADD_SOURCE_LINE[S](loc) |>>
      compileExpression[S, Ref[MConstraintSystem]](exp1, map, lenv0, entryPoint) |>>
      compileExpression[S**Ref[MConstraintSystem], Ref[MConstraintSystem]](exp2, map, lenv0, entryPoint) |>>
      Api.Java.Runtime.FixPoint.Solver.compose.INVOKE
  }

  def compileFixpointEntails[S <: Stack]
  (exp1: Expression, exp2: Expression, loc: SourceLocation, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)
  (implicit root: Root, flix: Flix):
  F[S] => F[S** MBool]={
    ADD_SOURCE_LINE[S](loc) |>>
    compileExpression[S, Ref[MConstraintSystem]](exp1, map, lenv0, entryPoint) |>>
    compileExpression[S**Ref[MConstraintSystem], Ref[MConstraintSystem]](exp2, map, lenv0, entryPoint) |>>
    Api.Java.Runtime.FixPoint.Solver.entails.INVOKE
  }

  def compileExistential[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileThrowFlixError(Reference(JvmName.Runtime.NotImplementedError), loc)

  def compileUniversal[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileThrowFlixError(Reference(JvmName.Runtime.NotImplementedError), loc)

  def compileUserError[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileThrowFlixError(Reference(JvmName.Runtime.NotImplementedError), loc)

  def compileHoleError[S <: Stack](hole: String, loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileHoleError(hole, loc)

  def compileMatchError[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileThrowFlixError(Reference(JvmName.Runtime.MatchError), loc)

  def compileSwitchError[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] =
    ADD_SOURCE_LINE[S](loc) |>>
      compileThrowFlixError(Reference(JvmName.Runtime.SwitchError), loc)


  def pushDummyValue[S <: Stack, T <: MnemonicsTypes : TypeTag ] (implicit root: Root, flix: Flix): F[S] => F[S ** T] ={
    val erasedType = getJvmType[T]
    (erasedType match {
      case PrimBool => ICONST(1)
      case PrimChar => ICONST(-1)
      case PrimByte => ICONST(-1)
      case PrimShort => ICONST(-1)
      case PrimInt => ICONST(-1)
      case PrimLong =>
        ICONST[S](-1) |>>
          I2L
      case PrimFloat => FCONST(1)
      case PrimDouble => DCONST(1)
      case Reference(_) => CONST_NULL
      case _ => throw InternalCompilerException(s"Unexpected type: $erasedType")
    }).asInstanceOf[F[S]  => F[S ** T]]
  }

  private def compileReifiedSourceLocation[S <: Stack](loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MRefiedSource]] = {

    NEW[S, Ref[MRefiedSource]](Reference(JvmName.Runtime.ReifiedSourceLocation)) |>>
      DUP |>>
      LDC_STRING(loc.source.format) |>>
      LDC_INT(loc.beginLine) |>>
      LDC_INT(loc.beginCol) |>>
      LDC_INT(loc.endLine) |>>
      LDC_INT(loc.endCol) |>>
      Api.Java.Runtime.RefiedSourceLocation.constructor.INVOKE
  }


  def compileThrowFlixError[S <: Stack](ct: Reference, loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] = {


    val constructor = new VoidMethod2[Ref[MObject], Ref[MRefiedSource]](JvmModifier.InvokeSpecial, ct, "<init>")
    NEW[S, Ref[MObject]](ct) |>>
      DUP |>>
      compileReifiedSourceLocation[S ** Ref[MObject] ** Ref[MObject]](loc) |>>
      constructor.INVOKE |>>
      THROW2
  }


  def compileThrowHoleError[S <: Stack](hole: String, loc: SourceLocation)(implicit root: Root, flix: Flix): F[S] => F[S] = {

    NEW[S, Ref[MObject]](Reference(JvmName.Runtime.HoleError)) |>>
      DUP |>>
      LDC_STRING(hole) |>>
      compileReifiedSourceLocation(loc) |>>
      Api.Java.Runtime.HoleError.constructor2.INVOKE |>>
      THROW2
  }
}
