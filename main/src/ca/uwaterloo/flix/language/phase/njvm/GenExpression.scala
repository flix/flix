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


import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes.{RecordEmpty, RecordExtend, TagClass, TupleClass}
import ca.uwaterloo.flix.language.phase.njvm.interfaces.RecordInterface
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Label

import scala.reflect.runtime.universe._

object GenExpression {

  def compileExpression[S <: Stack, T1 <: MnemonicsTypes : TypeTag]
  (exp0: Expression, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** T1] =
    (exp0 match {
      case Expression.Unit => compileUnit
      case Expression.True | Expression.False => compileBoolean(exp0)
      case Expression.Char(c) => compileChar(c)
      case Expression.Float32(f) => compileFloat(f)
      case Expression.Float64(d) => compileDouble(d)
      case Expression.Int8(b) => compileInt(b)
      case Expression.Int16(s) => compileInt(s)
      case Expression.Int32(i) => compileInt(i)
      case Expression.Int64(l) => compileLong(l)
      case Expression.BigInt(ii) => compileBigInt(ii)
      case Expression.Str(s) => compileString(s)
      case Expression.Var(sym, tpe, _) => compileVar(sym, tpe)

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) => ???
      case Expression.ApplyClo(exp, args, tpe, loc) => ???
      case Expression.ApplyDef(name, args, tpe, loc) => ???
      case Expression.ApplyEff(_, _, _, _) =>
        compileApplyEff[S, T1]
      case Expression.ApplyCloTail(exp, args, tpe, loc) => ???
      case Expression.ApplyDefTail(name, args, tpe, loc) => ???
      case Expression.ApplyEffTail(sym, args, tpe, loc) => ???
      case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => ???

      case Expression.Unary(sop, op, exp, _, _) => ???
      case Expression.Binary(sop, op, exp1, exp2, _, _) => ???
      case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
        compileIfThenElse(exp1, exp2, exp3, loc, map, lenv0, entryPoint)
      case Expression.Branch(exp, branches, tpe, loc) => ???
      case Expression.JumpTo(sym, _, loc) =>
        compileJumpTo(lenv0(sym), loc)

      case Expression.Let(sym, exp1, exp2, _, loc) =>
        val jvmType = getErasedJvmType(exp1.tpe)
        jvmType match{
          case PrimBool =>  compileLet[S, MBool, T1](sym, exp1, exp2, loc, map, lenv0, entryPoint)
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
        tagTpe match{
          case PrimBool =>  compileIs[S, MBool](tagInfo, exp1, loc, map, lenv0, entryPoint)
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
        tagTpe match{
          case PrimBool =>  compileTag[S, MBool](tagInfo, exp, loc, map, lenv0, entryPoint)
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
        jvmTpe match{
          case PrimBool =>  compileUntagPrim[S, MBool](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimChar => compileUntagPrim[S, MChar](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimByte => compileUntagPrim[S, MByte](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimShort => compileUntagPrim[S, MShort](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimInt => compileUntagPrim[S, MInt](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimLong => compileUntagPrim[S, MLong](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimFloat => compileUntagPrim[S, MFloat](tagInfo, exp, loc, map, lenv0, entryPoint)
          case PrimDouble => compileUntagPrim[S, MDouble](tagInfo, exp, loc, map, lenv0, entryPoint)
          case Reference(name) => compileUntagNonPrim[S](tagInfo, exp, Reference(name),loc, map, lenv0, entryPoint)
          case _ => throw InternalCompilerException("Unexpected type " + jvmTpe)
        }

      case Expression.Index(base, offset, tpe, loc) => ???

      case Expression.Tuple(elms, tpe, loc) =>
        compileTuple(elms, tpe, loc, map, lenv0, entryPoint)

      case Expression.RecordEmpty(_, loc) =>
        compileRecordEmpty(loc, map)
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
        compileRecordRestrict(label, rest, loc, map, lenv0, entryPoint)

      case Expression.ArrayLit(elms, tpe, loc) => ???
      case Expression.ArrayNew(elm, len, tpe, loc) => ???
      case Expression.ArrayLoad(base, index, tpe, loc) => ???
      case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
      case Expression.ArrayLength(base, tpe, loc) => ???
      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => ???

      case Expression.Ref(exp, tpe, loc) => ???
      case Expression.Deref(exp, tpe, loc) => ???
      case Expression.Assign(exp1, exp2, tpe, loc) => ???

      case Expression.HandleWith(exp, bindings, tpe, loc) => ???
      case Expression.Existential(_, _, loc) =>
        compileExistential(loc)
      case Expression.Universal(_, _, loc) =>
        compileUniversal(loc)
      case Expression.TryCatch(exp, rules, tpe, loc) => ???
      case Expression.NativeConstructor(constructor, args, tpe, loc) => ???
      case Expression.NativeField(field, tpe, loc) => ???
      case Expression.NativeMethod(method, args, tpe, loc) => ???

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

  def compileApplyEff[S <: Stack, T1 <: MnemonicsTypes : TypeTag]: F[S] => F[S ** T1] =
    throw InternalCompilerException(s"ApplyEff not implemented in JVM backend!")

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
            compileExpression(exp, map, lenv0, entryPoint).asInstanceOf
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
