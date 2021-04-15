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
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  properties: List[ErasedAst.Property],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[PType], tpe: EType[PType], loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ErasedAst.Expression[PInt32]) {
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

    case class True(loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = EType.Bool()
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = EType.Bool()
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression[PChar] {
      final val tpe = EType.Char()
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression[PFloat32] {
      final val tpe = EType.Float32()
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression[PFloat64] {
      final val tpe = EType.Float64()
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression[PInt8] {
      final val tpe = EType.Int8()
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression[PInt16] {
      final val tpe = EType.Int16()
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = EType.Int32()
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression[PInt64] {
      final val tpe = EType.Int64()
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression[PReference[PBigInt]] {
      final val tpe = EType.Reference(ERefType.BigInt())
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression[PReference[PStr]] {
      final val tpe = EType.Reference(ERefType.Str())
    }

    case class Var[T <: PType](sym: Symbol.VarSym, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class ApplyClo[T <: PType](exp: ErasedAst.Expression[PReference[PAnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDef[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyCloTail[T <: PType](exp: ErasedAst.Expression[PReference[PAnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDefTail[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplySelfTail[T <: PType](sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    // TODO: maybe make multiple classes for different exp types
    case class Unary[T <: PType](sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Binary[T <: PType](sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class IfThenElse[T <: PType](exp1: ErasedAst.Expression[PInt32], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Branch[T <: PType](exp: ErasedAst.Expression[T], branches: Map[Symbol.LabelSym, ErasedAst.Expression[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class JumpTo[T <: PType](sym: Symbol.LabelSym, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Let[T <: PType](sym: Symbol.VarSym, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[T], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe: EType[PInt32] = EType.Bool()
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PType], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class Untag[T <: PType](sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Index[T <: PType](base: ErasedAst.Expression[PReference[PAnyObject]], offset: scala.Int, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Tuple(elms: List[ErasedAst.Expression[PType]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordEmpty(tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordSelect[T <: PType](exp: ErasedAst.Expression[PReference[PAnyObject]], field: Name.Field, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[PType], rest: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class ArrayLit[T <: PType](elms: List[ErasedAst.Expression[T]], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayNew[T <: PType](elm: ErasedAst.Expression[T], len: ErasedAst.Expression[PInt32], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayLoad[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PInt32], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ArrayStore[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PInt32], elm: ErasedAst.Expression[T], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class ArrayLength[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], tpe: EType[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32]

    case class ArraySlice[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], beginIndex: ErasedAst.Expression[PInt32], endIndex: ErasedAst.Expression[PInt32], tpe: EType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class Ref[T <: PType](exp: ErasedAst.Expression[T], className: String, tpe: EType[PReference[PRef[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PRef[T]]]

    case class Deref[T <: PType](exp: ErasedAst.Expression[PReference[PRef[T]]], className: String, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Assign[T <: PType](exp1: ErasedAst.Expression[PReference[PRef[T]]], exp2: ErasedAst.Expression[T], className: String, tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = EType.Bool()
    }

    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = EType.Bool()
    }

    case class Cast[T <: PType](exp: ErasedAst.Expression[PType], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class TryCatch[T <: PType](exp: ErasedAst.Expression[T], rules: List[ErasedAst.CatchRule[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[PType]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class InvokeMethod[T <: PType](method: Method, exp: ErasedAst.Expression[PReference[PAnyObject]], args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeStaticMethod[T <: PType](method: Method, args: List[ErasedAst.Expression[PType]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class GetField[T <: PType](field: Field, exp: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutField(field: Field, exp1: ErasedAst.Expression[PReference[PAnyObject]], exp2: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class GetStaticField[T <: PType](field: Field, tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutStaticField(field: Field, exp: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class NewChannel[T <: PType](exp: ErasedAst.Expression[PInt32], tpe: EType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class GetChannel[T <: PType](exp: ErasedAst.Expression[PReference[PChan[T]]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutChannel[T <: PType](exp1: ErasedAst.Expression[PReference[PChan[T]]], exp2: ErasedAst.Expression[T], tpe: EType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class SelectChannel[T <: PType](rules: List[ErasedAst.SelectChannelRule[T]], default: Option[ErasedAst.Expression[T]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Spawn(exp: ErasedAst.Expression[PType], tpe: EType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Lazy[T <: PType](exp: ErasedAst.Expression[T], tpe: EType[PReference[PLazy[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PLazy[T]]]

    case class Force[T <: PType](exp: ErasedAst.Expression[PReference[PLazy[T]]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class FixpointConstraintSet(cs: List[ErasedAst.Constraint], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class FixpointCompose(exp1: ErasedAst.Expression[PReference[PAnyObject]], exp2: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class FixpointSolve(exp: ErasedAst.Expression[PReference[PAnyObject]], stf: Ast.Stratification, tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class FixpointProject(pred: Name.Pred, exp: ErasedAst.Expression[PReference[PAnyObject]], tpe: EType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class FixpointFold[T <: PType](pred: Name.Pred, init: ErasedAst.Expression.Var[PReference[PAnyObject]], f: ErasedAst.Expression.Var[PReference[PAnyObject]], constraints: ErasedAst.Expression.Var[PReference[PAnyObject]], tpe: EType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

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

      case class Union(exp: ErasedAst.Expression[PReference[PAnyObject]], terms: List[ErasedAst.Term.Head], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Predicate.Head

    }

    sealed trait Body extends ErasedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ErasedAst.Term.Body], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Predicate.Body

      case class Guard(exp: ErasedAst.Expression[PInt32], terms: List[ErasedAst.Term.Body], loc: SourceLocation) extends ErasedAst.Predicate.Body

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

      case class App(exp: ErasedAst.Expression[PReference[PAnyObject]], args: List[Symbol.VarSym], tpe: EType[PType], loc: SourceLocation) extends ErasedAst.Term.Head

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

  case class BoxInt8(exp: ErasedAst.Expression[PInt8], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt8]] {
    final val tpe = EType.Reference(ERefType.BoxedInt8())
  }

  case class BoxInt16(exp: ErasedAst.Expression[PInt16], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt16]] {
    final val tpe = EType.Reference(ERefType.BoxedInt16())
  }

  case class BoxInt32(exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt32]] {
    final val tpe = EType.Reference(ERefType.BoxedInt32())
  }

  case class BoxInt64(exp: ErasedAst.Expression[PInt64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt64]] {
    final val tpe = EType.Reference(ERefType.BoxedInt64())
  }

  case class BoxChar(exp: ErasedAst.Expression[PChar], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedChar]] {
    final val tpe = EType.Reference(ERefType.BoxedChar())
  }

  case class BoxFloat32(exp: ErasedAst.Expression[PFloat32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat32]] {
    final val tpe = EType.Reference(ERefType.BoxedFloat32())
  }

  case class BoxFloat64(exp: ErasedAst.Expression[PFloat64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat64]] {
    final val tpe = EType.Reference(ERefType.BoxedFloat64())
  }

  case class UnboxInt8(exp: ErasedAst.Expression[PReference[PBoxedInt8]], loc: SourceLocation) extends ErasedAst.Expression[PInt8] {
    final val tpe = EType.Int8()
  }

  case class UnboxInt16(exp: ErasedAst.Expression[PReference[PBoxedInt16]], loc: SourceLocation) extends ErasedAst.Expression[PInt16] {
    final val tpe = EType.Int16()
  }

  case class UnboxInt32(exp: ErasedAst.Expression[PReference[PBoxedInt32]], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
    final val tpe = EType.Int32()
  }

  case class UnboxInt64(exp: ErasedAst.Expression[PReference[PBoxedInt64]], loc: SourceLocation) extends ErasedAst.Expression[PInt64] {
    final val tpe = EType.Int64()
  }

  case class UnboxChar(exp: ErasedAst.Expression[PReference[PBoxedChar]], loc: SourceLocation) extends ErasedAst.Expression[PChar] {
    final val tpe = EType.Char()
  }

  case class UnboxFloat32(exp: ErasedAst.Expression[PReference[PBoxedFloat32]], loc: SourceLocation) extends ErasedAst.Expression[PFloat32] {
    final val tpe = EType.Float32()
  }

  case class UnboxFloat64(exp: ErasedAst.Expression[PReference[PBoxedFloat64]], loc: SourceLocation) extends ErasedAst.Expression[PFloat64] {
    final val tpe = EType.Float64()
  }

}

