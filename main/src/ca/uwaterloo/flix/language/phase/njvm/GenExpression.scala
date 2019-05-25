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
import ca.uwaterloo.flix.language.phase.jvm.AsmOps.compileReifiedSourceLocation
import ca.uwaterloo.flix.language.phase.jvm.JvmName
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

      case Expression.Let(sym, exp1, exp2, _, loc) => ???
      case Expression.LetRec(sym, exp1, exp2, _, _) => ???

      case Expression.Is(_, _, _, _) | Expression.Tag(_, _, _, _, _) |
           Expression.Untag(_, _, _, _, _) => compileTagExpression(exp0, map, lenv0, entryPoint)

      case Expression.Index(base, offset, tpe, _) => ???

      case Expression.Tuple(elms, tpe, loc) =>
        compileTuple(elms, tpe, loc, map, lenv0, entryPoint)

      case Expression.RecordEmpty(_, loc) =>
        compileRecordEmpty(loc, map)
      case Expression.RecordSelect(_, _, _, _) =>
        ???
      case Expression.RecordSelect(_, _, _, _) | Expression.RecordExtend(_, _, _, _, _) |
           Expression.RecordRestrict(_, _, _, _) => compileRecordExpression(exp0, map, lenv0, entryPoint)

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

      case Expression.NewChannel(exp, tpe, loc) => ???
      case Expression.GetChannel(exp, tpe, loc) => ???
      case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
      case Expression.SelectChannel(rules, default, tpe, loc) => ???

      case Expression.Spawn(exp, tpe, loc) => ???
      case Expression.Sleep(exp, tpe, loc) => ???

      case Expression.FixpointConstraint(con, tpe, loc) => ???
      case Expression.FixpointCompose(exp1, exp2, tpe, loc) => ???
      case Expression.FixpointSolve(uid, exp, stf, tpe, loc) => ???
      case Expression.FixpointProject(pred, exp, tpe, loc) => ???
      case Expression.FixpointEntails(exp1, exp2, tpe, loc) => ???

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

  def compileApplyEff[S <: Stack, T1 <: MnemonicsTypes : TypeTag](s: String): F[S] => F[S ** T1] =
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

  def compileTagExpression[S <: Stack, T1 <: MnemonicsTypes]
  (exp: Expression, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** T1] =
    (exp match {
      case Expression.Is(enum, tag, exp1, loc) =>
        val tagInfo = getTagInfo(exp.tpe, tag)
        val tagTpe = getTagClassType(tagInfo)
        ADD_SOURCE_LINE[S](loc) |>>
          compileExpression(exp1, map, lenv0, entryPoint) |>>
          INSTANCE_OF(tagTpe)

      case Expression.Tag(enum, tag, exp, tpe, loc) =>

        val tagInfo = getTagInfo(tpe, tag)
        val classType = getTagClassType(tagInfo)
        val tagClass = map(classType.name).asInstanceOf[TagClass[_]]

      //        ADD_SOURCE_LINE(loc) |>>
      //          (tagClass.instance match {
      //          case Some(field) => field.GET_STATIC
      //          case None =>
      //              NEW[S, Ref[TupleClass[_]]](classType) |>>
      //              DUP |>>
      //              compileExpression(exp, map, lenv0, entryPoint)|>>
      //              tagClass.defaultConstructor.INVOKE
      //          })

      case Expression.Untag(enum, tag, exp, tpe, loc) => ???
      case _ => throw InternalCompilerException(s"Unexpected expression " + exp)

    }).asInstanceOf[F[S] => F[S ** T1]]


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


  def compileRecordExpression[S <: Stack, T1 <: MnemonicsTypes]
  (exp: Expression, map: Map[JvmName, MnemonicsClass], lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix):
  F[S] => F[S ** T1] =
    (exp match {

      case Expression.RecordSelect(exp, label, tpe, loc) => ???

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
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
      case Expression.RecordRestrict(label, rest, tpe, loc) => ???
      case _ => throw InternalCompilerException(s"Unexpected expression " + exp)
    }).asInstanceOf[F[S] => F[S ** T1]]

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
