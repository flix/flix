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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.language.ast.ErasedAst.PRefType._
import ca.uwaterloo.flix.language.ast.ErasedAst.PType._

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  latticeOps: Map[EType[PType], ErasedAst.LatticeOps],
                  properties: List[ErasedAst.Property],
                  specialOps: Map[SpecialOperator, Map[EType[PType], Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[PType], tpe: EType[PType], loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ErasedAst.Expression[PrimInt32]) {
    def loc: SourceLocation = defn.loc
  }

  case class LatticeOps(tpe: EType[PType], bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression[T <: PType] {
    def tpe: EType[T]

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]] {
      final val tpe = EType.Reference(ERefType.Unit())
    }

    case class Null[S <: PRefType](tpe: EType[PReference[S]], loc: SourceLocation) extends ErasedAst.Expression[PReference[S]]

    case class True(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = EType.Bool()
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = EType.Bool()
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression[PrimChar] {
      final val tpe = EType.Char()
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32] {
      final val tpe = EType.Float32()
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64] {
      final val tpe = EType.Float64()
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression[PrimInt8] {
      final val tpe = EType.Int8()
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression[PrimInt16] {
      final val tpe = EType.Int16()
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = EType.Int32()
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression[PrimInt64] {
      final val tpe = EType.Int64()
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression[PReference[PBigInt]] {
      final val tpe = EType.Reference(ERefType.BigInt())
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression[PReference[PStr]] {
      final val tpe = EType.Reference(ERefType.Str())
    }

    case class Var[T <: PType](sym: Symbol.VarSym, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class ApplyClo[T <: PType](exp: ErasedAst.Expression[PReference[AnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDef[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyCloTail[T <: PType](exp: ErasedAst.Expression[PReference[AnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDefTail[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplySelfTail[T <: PType](sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    // TODO: maybe make multiple classes for different exp types
    case class Unary[T <: PType](sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Binary[T <: PType](sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class IfThenElse[T <: PType](exp1: ErasedAst.Expression[PrimInt32], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Branch[T <: PType](exp: ErasedAst.Expression[T], branches: Map[Symbol.LabelSym, ErasedAst.Expression[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class JumpTo[T <: PType](sym: Symbol.LabelSym, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Let[T <: PType](sym: Symbol.VarSym, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[T], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe: EType[PrimInt32] = EType.Bool()
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PType], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class Untag[T <: PType](sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Index[T <: PType](base: ErasedAst.Expression[PReference[AnyObject]], offset: scala.Int, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Tuple(elms: List[ErasedAst.Expression[PType]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class RecordEmpty(tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class RecordSelect[T <: PType](exp: ErasedAst.Expression[PReference[AnyObject]], field: Name.Field, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[PType], rest: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class ArrayLit[T <: PType](elms: List[ErasedAst.Expression[T]], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayNew[T <: PType](elm: ErasedAst.Expression[T], len: ErasedAst.Expression[PrimInt32], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayLoad[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PrimInt32], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ArrayStore[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PrimInt32], elm: ErasedAst.Expression[T], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class ArrayLength[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], tpe: EType[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class ArraySlice[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], beginIndex: ErasedAst.Expression[PrimInt32], endIndex: ErasedAst.Expression[PrimInt32], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class Ref[T <: PType](exp: ErasedAst.Expression[T], tpe: EType[PReference[PRef[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PRef[T]]]

    case class Deref[T <: PType](exp: ErasedAst.Expression[PReference[PRef[T]]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Assign[T <: PType](exp1: ErasedAst.Expression[PReference[PRef[T]]], exp2: ErasedAst.Expression[T], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = EType.Bool()
    }

    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = EType.Bool()
    }

    case class Cast[T <: PType](exp: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class TryCatch[T <: PType](exp: ErasedAst.Expression[T], rules: List[ErasedAst.CatchRule[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[PType]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class InvokeMethod[T <: PType](method: Method, exp: ErasedAst.Expression[PReference[AnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeStaticMethod[T <: PType](method: Method, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class GetField[T <: PType](field: Field, exp: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutField(field: Field, exp1: ErasedAst.Expression[PReference[AnyObject]], exp2: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class GetStaticField[T <: PType](field: Field, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutStaticField(field: Field, exp: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class NewChannel[T <: PType](exp: ErasedAst.Expression[PrimInt32], tpe: EType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class GetChannel[T <: PType](exp: ErasedAst.Expression[PReference[PChan[T]]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutChannel[T <: PType](exp1: ErasedAst.Expression[PReference[PChan[T]]], exp2: ErasedAst.Expression[T], tpe: EType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class SelectChannel[T <: PType](rules: List[ErasedAst.SelectChannelRule[T]], default: Option[ErasedAst.Expression[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Spawn(exp: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Lazy[T <: PType](exp: ErasedAst.Expression[T], tpe: EType[PReference[PLazy[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PLazy[T]]]

    case class Force[T <: PType](exp: ErasedAst.Expression[PReference[PLazy[T]]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class FixpointConstraintSet(cs: List[ErasedAst.Constraint], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class FixpointCompose(exp1: ErasedAst.Expression[PReference[AnyObject]], exp2: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class FixpointSolve(exp: ErasedAst.Expression[PReference[AnyObject]], stf: Ast.Stratification, tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class FixpointProject(pred: Name.Pred, exp: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[PReference[AnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[AnyObject]]

    case class FixpointEntails(exp1: ErasedAst.Expression[PReference[AnyObject]], exp2: ErasedAst.Expression[PReference[AnyObject]], tpe: EType[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class FixpointFold[T <: PType](pred: Name.Pred, init: ErasedAst.Expression.Var[PReference[AnyObject]], f: ErasedAst.Expression.Var[PReference[AnyObject]], constraints: ErasedAst.Expression.Var[PReference[AnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class HoleError[T <: PType](sym: Symbol.HoleSym, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class MatchError[T <: PType](tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

  }

  case class SelectChannelRule[T <: PType](sym: Symbol.VarSym, chan: ErasedAst.Expression[PReference[PChan[PType]]], exp: ErasedAst.Expression[T])

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ErasedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ErasedAst.Term.Head], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Predicate.Head

      case class Union(exp: ErasedAst.Expression[PReference[AnyObject]], terms: List[ErasedAst.Term.Head], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Predicate.Head

    }

    sealed trait Body extends ErasedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ErasedAst.Term.Body], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Predicate.Body

      case class Guard(exp: ErasedAst.Expression[PrimInt32], terms: List[ErasedAst.Term.Body], loc: SourceLocation) extends ErasedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head {
      def tpe: EType[PType]
    }

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class Lit(sym: Symbol.DefnSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class App(exp: ErasedAst.Expression[PReference[AnyObject]], args: List[Symbol.VarSym], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Head

    }

    sealed trait Body {
      def tpe: EType[PType]
    }

    object Body {

      case class Wild(tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class Lit(sym: Symbol.DefnSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: EType[PType])

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: EType[PType], loc: SourceLocation)

  case class CatchRule[T <: PType](sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression[T])

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: EType[PType], loc: SourceLocation) extends ErasedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: EType[PType])

  case class FreeVar(sym: Symbol.VarSym, tpe: EType[PType])

  case class BoxInt8(exp: ErasedAst.Expression[PrimInt8], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt8]] {
    final val tpe = EType.Reference(ERefType.BoxedInt8())
  }

  case class BoxInt16(exp: ErasedAst.Expression[PrimInt16], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt16]] {
    final val tpe = EType.Reference(ERefType.BoxedInt16())
  }

  case class BoxInt32(exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt32]] {
    final val tpe = EType.Reference(ERefType.BoxedInt32())
  }

  case class BoxInt64(exp: ErasedAst.Expression[PrimInt64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt64]] {
    final val tpe = EType.Reference(ERefType.BoxedInt64())
  }

  case class BoxChar(exp: ErasedAst.Expression[PrimChar], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedChar]] {
    final val tpe = EType.Reference(ERefType.BoxedChar())
  }

  case class BoxFloat32(exp: ErasedAst.Expression[PrimFloat32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat32]] {
    final val tpe = EType.Reference(ERefType.BoxedFloat32())
  }

  case class BoxFloat64(exp: ErasedAst.Expression[PrimFloat64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat64]] {
    final val tpe = EType.Reference(ERefType.BoxedFloat64())
  }

  case class UnboxInt8(exp: ErasedAst.Expression[PReference[PBoxedInt8]], loc: SourceLocation) extends ErasedAst.Expression[PrimInt8] {
    final val tpe = EType.Int8()
  }

  case class UnboxInt16(exp: ErasedAst.Expression[PReference[PBoxedInt16]], loc: SourceLocation) extends ErasedAst.Expression[PrimInt16] {
    final val tpe = EType.Int16()
  }

  case class UnboxInt32(exp: ErasedAst.Expression[PReference[PBoxedInt32]], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
    final val tpe = EType.Int32()
  }

  case class UnboxInt64(exp: ErasedAst.Expression[PReference[PBoxedInt64]], loc: SourceLocation) extends ErasedAst.Expression[PrimInt64] {
    final val tpe = EType.Int64()
  }

  case class UnboxChar(exp: ErasedAst.Expression[PReference[PBoxedChar]], loc: SourceLocation) extends ErasedAst.Expression[PrimChar] {
    final val tpe = EType.Char()
  }

  case class UnboxFloat32(exp: ErasedAst.Expression[PReference[PBoxedFloat32]], loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32] {
    final val tpe = EType.Float32()
  }

  case class UnboxFloat64(exp: ErasedAst.Expression[PReference[PBoxedFloat64]], loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64] {
    final val tpe = EType.Float64()
  }

  sealed trait PType

  sealed trait Cat1

  sealed trait Cat2

  // only exist at scala compile time, to help compiler writers
  object PType {

    sealed trait PrimInt8 extends PType with Cat1

    sealed trait PrimInt16 extends PType with Cat1

    sealed trait PrimInt32 extends PType with Cat1

    sealed trait PrimInt64 extends PType with Cat2

    sealed trait PrimChar extends PType with Cat1

    sealed trait PrimFloat32 extends PType with Cat1

    sealed trait PrimFloat64 extends PType with Cat2

    sealed trait PReference[T <: PRefType] extends PType with Cat1

  }

  sealed trait PRefType

  object PRefType {

    sealed trait PBoxedBool extends PRefType

    sealed trait PBoxedInt8 extends PRefType

    sealed trait PBoxedInt16 extends PRefType

    sealed trait PBoxedInt32 extends PRefType

    sealed trait PBoxedInt64 extends PRefType

    sealed trait PBoxedChar extends PRefType

    sealed trait PBoxedFloat32 extends PRefType

    sealed trait PBoxedFloat64 extends PRefType

    sealed trait AnyObject extends PRefType

    sealed trait PUnit extends PRefType

    sealed trait PRef[T <: PType] extends PRefType

    sealed trait PArray[T <: PType] extends PRefType

    sealed trait PChan[T <: PType] extends PRefType

    sealed trait PLazy[T <: PType] extends PRefType

    sealed trait PStr extends PRefType

    sealed trait PBigInt extends PRefType

  }

  // actual flix types
  sealed trait EType[T <: PType]

  object EType {

    case class Bool() extends EType[PrimInt32]

    case class Int8() extends EType[PrimInt8]

    case class Int16() extends EType[PrimInt16]

    case class Int32() extends EType[PrimInt32]

    case class Int64() extends EType[PrimInt64]

    case class Char() extends EType[PrimChar]

    case class Float32() extends EType[PrimFloat32]

    case class Float64() extends EType[PrimFloat64]

    case class Reference[T <: PRefType](referenceType: ERefType[T]) extends EType[PReference[T]]

  }


  sealed trait ERefType[T <: PRefType]

  object ERefType {

    case class BoxedBool() extends ERefType[PBoxedBool]

    case class BoxedInt8() extends ERefType[PBoxedInt8]

    case class BoxedInt16() extends ERefType[PBoxedInt16]

    case class BoxedInt32() extends ERefType[PBoxedInt32]

    case class BoxedInt64() extends ERefType[PBoxedInt64]

    case class BoxedChar() extends ERefType[PBoxedChar]

    case class BoxedFloat32() extends ERefType[PBoxedFloat32]

    case class BoxedFloat64() extends ERefType[PBoxedFloat64]

    case class Unit() extends ERefType[PUnit]

    case class Array[T <: PType](tpe: EType[T]) extends ERefType[PArray[T]]

    case class Channel[T <: PType](tpe: EType[T]) extends ERefType[PChan[T]]

    case class Lazy[T <: PType](tpe: EType[T]) extends ERefType[PLazy[T]]

    case class Ref[T <: PType](tpe: EType[T]) extends ERefType[PRef[T]]

    // TODO: Should be removed.
    case class Var(id: Int) extends ERefType[AnyObject]

    case class Tuple(elms: List[EType[PType]]) extends ERefType[AnyObject]

    case class Enum(sym: Symbol.EnumSym, args: List[EType[PType]]) extends ERefType[AnyObject]

    case class BigInt() extends ERefType[PBigInt]

    case class Str() extends ERefType[PStr]

    case class Arrow(args: List[EType[PType]], result: EType[PType]) extends ERefType[AnyObject]

    case class RecordEmpty() extends ERefType[AnyObject]

    case class RecordExtend(field: String, value: EType[PType], rest: EType[PReference[AnyObject]]) extends ERefType[AnyObject]

    case class SchemaEmpty() extends ERefType[AnyObject]

    case class SchemaExtend(name: String, tpe: EType[PType], rest: EType[PReference[AnyObject]]) extends ERefType[AnyObject]

    case class Relation(tpes: List[EType[PType]]) extends ERefType[AnyObject]

    case class Lattice(tpes: List[EType[PType]]) extends ERefType[AnyObject]

    case class Native(clazz: Class[_]) extends ERefType[AnyObject]

  }

}

