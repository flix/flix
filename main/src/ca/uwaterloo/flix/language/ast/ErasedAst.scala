/*
 * Copyright 2020 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  latticeOps: Map[ErasedType[JType], ErasedAst.LatticeOps],
                  properties: List[ErasedAst.Property],
                  specialOps: Map[SpecialOperator, Map[ErasedType[JType], Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[JType], tpe: ErasedType[JType], loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ErasedAst.Expression[PrimInt32]) {
    def loc: SourceLocation = defn.loc
  }

  case class LatticeOps(tpe: ErasedType[JType], bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression[T <: JType] {
    def tpe: ErasedType[T]
    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends ErasedAst.Expression[JUnit] {
      final val tpe = ErasedType.Unit()
    }

    case class Null(tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class True(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = ErasedType.Bool()
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = ErasedType.Bool()
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression[PrimChar] {
      final val tpe = ErasedType.Char()
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32] {
      final val tpe = ErasedType.Float32()
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64] {
      final val tpe = ErasedType.Float64()
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression[PrimInt8] {
      final val tpe = ErasedType.Int8()
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression[PrimInt16] {
      final val tpe = ErasedType.Int16()
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = ErasedType.Int32()
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression[PrimInt64] {
      final val tpe = ErasedType.Int64()
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression[JObject] {
      final val tpe = ErasedType.BigInt()
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression[JObject] {
      final val tpe = ErasedType.Str()
    }

    case class Var[T <: JType](sym: Symbol.VarSym, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class ApplyClo[T <: JType](exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDef[T <: JType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyCloTail[T <: JType](exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDefTail[T <: JType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplySelfTail[T <: JType](sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    // TODO: maybe make multiple classes for different exp types
    case class Unary[T <: JType](sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[JType], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Binary[T <: JType](sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[JType], exp2: ErasedAst.Expression[JType], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class IfThenElse[T <: JType](exp1: ErasedAst.Expression[PrimInt32], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Branch[T <: JType](exp: ErasedAst.Expression[T], branches: Map[Symbol.LabelSym, ErasedAst.Expression[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class JumpTo[T <: JType](sym: Symbol.LabelSym, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Let[T <: JType](sym: Symbol.VarSym, exp1: ErasedAst.Expression[JType], exp2: ErasedAst.Expression[T], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JObject], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe: ErasedType[PrimInt32] = ErasedType.Bool()
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JType], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class Untag[T <: JType](sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JObject], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Index[T <: JType](base: ErasedAst.Expression[JObject], offset: scala.Int, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Tuple(elms: List[ErasedAst.Expression[JType]], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordEmpty(tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordSelect[T <: JType](exp: ErasedAst.Expression[JObject], field: Name.Field, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[JType], rest: ErasedAst.Expression[JObject], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[JObject], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class ArrayLit[T <: JType](elms: List[ErasedAst.Expression[T]], tpe: ErasedType[JArray[T]], loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class ArrayNew[T <: JType](elm: ErasedAst.Expression[T], len: ErasedAst.Expression[PrimInt32], tpe: ErasedType[JArray[T]], loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class ArrayLoad[T <: JType](base: ErasedAst.Expression[JArray[T]], index: ErasedAst.Expression[PrimInt32], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ArrayStore[T <: JType](base: ErasedAst.Expression[JArray[T]], index: ErasedAst.Expression[PrimInt32], elm: ErasedAst.Expression[T], tpe: ErasedType[JUnit], loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class ArrayLength[T <: JType](base: ErasedAst.Expression[JArray[T]], tpe: ErasedType[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class ArraySlice[T <: JType](base: ErasedAst.Expression[JArray[T]], beginIndex: ErasedAst.Expression[PrimInt32], endIndex: ErasedAst.Expression[PrimInt32], tpe: ErasedType[JArray[T]], loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class Ref[T <: JType](exp: ErasedAst.Expression[T], tpe: ErasedType[JRef[T]], loc: SourceLocation) extends ErasedAst.Expression[JRef[T]]

    case class Deref[T <: JType](exp: ErasedAst.Expression[JRef[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Assign[T <: JType](exp1: ErasedAst.Expression[JRef[T]], exp2: ErasedAst.Expression[T], tpe: ErasedType[JUnit], loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = ErasedType.Bool()
    }

    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = ErasedType.Bool()
    }

    case class Cast[T <: JType](exp: ErasedAst.Expression[JType], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class TryCatch[T <: JType](exp: ErasedAst.Expression[T], rules: List[ErasedAst.CatchRule[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[JType]], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class InvokeMethod[T <: JType](method: Method, exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeStaticMethod[T <: JType](method: Method, args: List[ErasedAst.Expression[JType]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class GetField[T <: JType](field: Field, exp: ErasedAst.Expression[JObject], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutField(field: Field, exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JType], tpe: ErasedType[JUnit], loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class GetStaticField[T <: JType](field: Field, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutStaticField(field: Field, exp: ErasedAst.Expression[JType], tpe: ErasedType[JUnit], loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class NewChannel[T <: JType](exp: ErasedAst.Expression[PrimInt32], tpe: ErasedType[JChan[T]], loc: SourceLocation) extends ErasedAst.Expression[JChan[T]]

    case class GetChannel[T <: JType](exp: ErasedAst.Expression[JChan[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutChannel[T <: JType](exp1: ErasedAst.Expression[JChan[T]], exp2: ErasedAst.Expression[T], tpe: ErasedType[JChan[T]], loc: SourceLocation) extends ErasedAst.Expression[JChan[T]]

    case class SelectChannel[T <: JType](rules: List[ErasedAst.SelectChannelRule[T]], default: Option[ErasedAst.Expression[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Spawn(exp: ErasedAst.Expression[JType], tpe: ErasedType[JUnit], loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class Lazy[T <: JType](exp: ErasedAst.Expression[T], tpe: ErasedType[JLazy[T]], loc: SourceLocation) extends ErasedAst.Expression[JLazy[T]]

    case class Force[T <: JType](exp: ErasedAst.Expression[JLazy[T]], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class FixpointConstraintSet(cs: List[ErasedAst.Constraint], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointCompose(exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JObject], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointSolve(exp: ErasedAst.Expression[JObject], stf: Ast.Stratification, tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointProject(pred: Name.Pred, exp: ErasedAst.Expression[JObject], tpe: ErasedType[JObject], loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointEntails(exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JObject], tpe: ErasedType[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class FixpointFold[T <: JType](pred: Name.Pred, init: ErasedAst.Expression.Var[JObject], f: ErasedAst.Expression.Var[JObject], constraints: ErasedAst.Expression.Var[JObject], tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class HoleError[T <: JType](sym: Symbol.HoleSym, tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class MatchError[T <: JType](tpe: ErasedType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

  }

  case class SelectChannelRule[T <: JType](sym: Symbol.VarSym, chan: ErasedAst.Expression[JChan[JType]], exp: ErasedAst.Expression[T])

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ErasedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ErasedAst.Term.Head], tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Predicate.Head

      case class Union(exp: ErasedAst.Expression[JObject], terms: List[ErasedAst.Term.Head], tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Predicate.Head

    }

    sealed trait Body extends ErasedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ErasedAst.Term.Body], tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Predicate.Body

      case class Guard(exp: ErasedAst.Expression[PrimInt32], terms: List[ErasedAst.Term.Body], loc: SourceLocation) extends ErasedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head {
      def tpe: ErasedType[JType]
    }

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class Lit(sym: Symbol.DefnSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Head

      case class App(exp: ErasedAst.Expression[JObject], args: List[Symbol.VarSym], tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Head

    }

    sealed trait Body {
      def tpe: ErasedType[JType]
    }

    object Body {

      case class Wild(tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Body

      case class Lit(sym: Symbol.DefnSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: ErasedType[JType])

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: ErasedType[JType], loc: SourceLocation)

  case class CatchRule[T <: JType](sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression[T])

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: ErasedType[JType], loc: SourceLocation) extends ErasedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: ErasedType[JType])

  case class FreeVar(sym: Symbol.VarSym, tpe: ErasedType[JType])

  case class BoxInt8(exp: ErasedAst.Expression[PrimInt8], final val tpe: ErasedType[BoxedInt8], loc: SourceLocation) extends ErasedAst.Expression[BoxedInt8]
  case class BoxInt16(exp: ErasedAst.Expression[PrimInt16], final val tpe: ErasedType[BoxedInt16], loc: SourceLocation) extends ErasedAst.Expression[BoxedInt16]
  case class BoxInt32(exp: ErasedAst.Expression[PrimInt32], final val tpe: ErasedType[BoxedInt32], loc: SourceLocation) extends ErasedAst.Expression[BoxedInt32]
  case class BoxInt64(exp: ErasedAst.Expression[PrimInt64], final val tpe: ErasedType[BoxedInt64], loc: SourceLocation) extends ErasedAst.Expression[BoxedInt64]
  case class BoxChar(exp: ErasedAst.Expression[PrimChar], final val tpe: ErasedType[BoxedChar], loc: SourceLocation) extends ErasedAst.Expression[BoxedChar]
  case class BoxFloat32(exp: ErasedAst.Expression[PrimFloat32], final val tpe: ErasedType[BoxedFloat32], loc: SourceLocation) extends ErasedAst.Expression[BoxedFloat32]
  case class BoxFloat64(exp: ErasedAst.Expression[PrimFloat64], final val tpe: ErasedType[BoxedFloat64], loc: SourceLocation) extends ErasedAst.Expression[BoxedFloat64]

  case class UnboxInt8(exp: ErasedAst.Expression[BoxedInt8], final val tpe: ErasedType[PrimInt8], loc: SourceLocation) extends ErasedAst.Expression[PrimInt8]
  case class UnboxInt16(exp: ErasedAst.Expression[BoxedInt16], final val tpe: ErasedType[PrimInt16], loc: SourceLocation) extends ErasedAst.Expression[PrimInt16]
  case class UnboxInt32(exp: ErasedAst.Expression[BoxedInt32], final val tpe: ErasedType[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]
  case class UnboxInt64(exp: ErasedAst.Expression[BoxedInt64], final val tpe: ErasedType[PrimInt64], loc: SourceLocation) extends ErasedAst.Expression[PrimInt64]
  case class UnboxChar(exp: ErasedAst.Expression[BoxedChar], final val tpe: ErasedType[PrimChar], loc: SourceLocation) extends ErasedAst.Expression[PrimChar]
  case class UnboxFloat32(exp: ErasedAst.Expression[BoxedFloat32], final val tpe: ErasedType[PrimFloat32], loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32]
  case class UnboxFloat64(exp: ErasedAst.Expression[BoxedFloat64], final val tpe: ErasedType[PrimFloat64], loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64]

  sealed trait JType

  object JType {

    sealed trait PrimInt8 extends JType
    sealed trait BoxedInt8 extends JType

    sealed trait PrimInt16 extends JType
    sealed trait BoxedInt16 extends JType

    sealed trait PrimInt32 extends JType
    sealed trait BoxedInt32 extends JType

    sealed trait PrimInt64 extends JType
    sealed trait BoxedInt64 extends JType

    sealed trait PrimChar extends JType
    sealed trait BoxedChar extends JType

    sealed trait PrimFloat32 extends JType
    sealed trait BoxedFloat32 extends JType

    sealed trait PrimFloat64 extends JType
    sealed trait BoxedFloat64 extends JType

    sealed trait JObject extends JType
    sealed trait JUnit extends JType

    sealed trait JRef[T <: JType] extends JType
    sealed trait JArray[T <: JType] extends JType
    sealed trait JChan[T <: JType] extends JType
    sealed trait JLazy[T <: JType] extends JType
  }

  sealed trait ErasedType[T <: JType]

  object ErasedType {

    case class Unit() extends ErasedType[JUnit]

    case class Bool() extends ErasedType[PrimInt32]

    case class Char() extends ErasedType[PrimChar]

    case class Float32() extends ErasedType[PrimFloat32]

    case class Float64() extends ErasedType[PrimFloat64]

    case class Int8() extends ErasedType[PrimInt8]

    case class Int16() extends ErasedType[PrimInt16]

    case class Int32() extends ErasedType[PrimInt32]

    case class Int64() extends ErasedType[PrimInt64]

    case class BigInt() extends ErasedType[JObject]

    case class Str() extends ErasedType[JObject]

    ///
    /// Compound Types.
    ///

    case class Array[T <: JType](tpe: ErasedType[T]) extends ErasedType[JArray[T]]

    case class Channel[T <: JType](tpe: ErasedType[T]) extends ErasedType[JChan[T]]

    case class Lazy[T <: JType](tpe: ErasedType[T]) extends ErasedType[JLazy[T]]

    case class Ref[T <: JType](tpe: ErasedType[T]) extends ErasedType[JRef[T]]

    case class Tuple(elms: List[ErasedType[JType]]) extends ErasedType[JObject]

    case class Enum(sym: Symbol.EnumSym, args: List[ErasedType[JType]]) extends ErasedType[JObject]

    case class Arrow(args: List[ErasedType[JType]], result: ErasedType[JType]) extends ErasedType[JObject]

    case class RecordEmpty() extends ErasedType[JObject]

    case class RecordExtend(field: String, value: ErasedType[JType], rest: ErasedType[JObject]) extends ErasedType[JObject]

    case class SchemaEmpty() extends ErasedType[JObject]

    case class SchemaExtend(name: String, tpe: ErasedType[JType], rest: ErasedType[JObject]) extends ErasedType[JObject]

    case class Relation(tpes: List[ErasedType[JType]]) extends ErasedType[JObject]

    case class Lattice(tpes: List[ErasedType[JType]]) extends ErasedType[JObject]

    case class Native(clazz: Class[_]) extends ErasedType[JObject]

    // TODO: Should be removed.
    case class Var(id: Int) extends ErasedType[Nothing]
  }
}

