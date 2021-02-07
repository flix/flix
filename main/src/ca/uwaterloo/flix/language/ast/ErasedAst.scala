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


// TODO: replace JType with T <: JType type variable
object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  latticeOps: Map[MonoType, ErasedAst.LatticeOps],
                  properties: List[ErasedAst.Property],
                  specialOps: Map[SpecialOperator, Map[MonoType, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ErasedAst.Expression[PrimInt32]) {
    def loc: SourceLocation = defn.loc
  }

  case class LatticeOps(tpe: MonoType, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation)

  object Expression {

    case class Unit(loc: SourceLocation) extends ErasedAst.Expression[JUnit] {
      final val tpe = MonoType.Unit
    }

    case class Null(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class True(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = MonoType.Bool
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = MonoType.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression[PrimChar] {
      final val tpe = MonoType.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32] {
      final val tpe = MonoType.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64] {
      final val tpe = MonoType.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression[PrimInt8] {
      final val tpe = MonoType.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression[PrimInt16] {
      final val tpe = MonoType.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32] {
      final val tpe = MonoType.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression[PrimInt64] {
      final val tpe = MonoType.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression[JObject] {
      final val tpe = MonoType.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression[JObject] {
      final val tpe = MonoType.Str
    }

    case class Var[T <: JType](sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class ApplyClo[T <: JType](exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDef[T <: JType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyCloTail[T <: JType](exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDefTail[T <: JType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplySelfTail[T <: JType](sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    // TODO: maybe make multiple classes for different exp types
    case class Unary[T <: JType](sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Binary[T <: JType](sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[JType], exp2: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class IfThenElse[T <: JType](exp1: ErasedAst.Expression[PrimInt32], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Branch[T <: JType](exp: ErasedAst.Expression[T], branches: Map[Symbol.LabelSym, ErasedAst.Expression[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class JumpTo[T <: JType](sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Let[T <: JType](sym: Symbol.VarSym, exp1: ErasedAst.Expression[JType], exp2: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class Index[T <: JType](base: ErasedAst.Expression[JObject], offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Tuple(elms: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordSelect(exp: ErasedAst.Expression[JObject], field: Name.Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[JType], rest: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class ArrayLit[T <: JType](elms: List[ErasedAst.Expression[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class ArrayNew[T <: JType](elm: ErasedAst.Expression[T], len: ErasedAst.Expression[PrimInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class ArrayLoad[T <: JType](base: ErasedAst.Expression[JArray[T]], index: ErasedAst.Expression[PrimInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ArrayStore[T <: JType](base: ErasedAst.Expression[JArray[T]], index: ErasedAst.Expression[PrimInt32], elm: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class ArrayLength[T <: JType](base: ErasedAst.Expression[JArray[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class ArraySlice[T <: JType](base: ErasedAst.Expression[JArray[T]], beginIndex: ErasedAst.Expression[PrimInt32], endIndex: ErasedAst.Expression[PrimInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JArray[T]]

    case class Ref[T <: JType](exp: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JRef[T]]

    case class Deref[T <: JType](exp: ErasedAst.Expression[JRef[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Assign[T <: JType](exp1: ErasedAst.Expression[JRef[T]], exp2: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PrimInt32], loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class Cast[T <: JType](exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class TryCatch[T <: JType](exp: ErasedAst.Expression[T], rules: List[ErasedAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class InvokeMethod(method: Method, exp: ErasedAst.Expression[JObject], args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class InvokeStaticMethod(method: Method, args: List[ErasedAst.Expression[JType]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class GetField(field: Field, exp: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class PutField(field: Field, exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class GetStaticField(field: Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class PutStaticField(field: Field, exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class NewChannel[T <: JType](exp: ErasedAst.Expression[PrimInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JChan[T]]

    case class GetChannel[T <: JType](exp: ErasedAst.Expression[JChan[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutChannel[T <: JType](exp1: ErasedAst.Expression[JChan[T]], exp2: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JChan[T]]

    case class SelectChannel[T <: JType](rules: List[ErasedAst.SelectChannelRule[T]], default: Option[ErasedAst.Expression[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Spawn(exp: ErasedAst.Expression[JType], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JUnit]

    case class Lazy[T <: JType](exp: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JLazy[T]]

    case class Force[T <: JType](exp: ErasedAst.Expression[JLazy[T]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class FixpointConstraintSet(cs: List[ErasedAst.Constraint], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointCompose(exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointSolve(exp: ErasedAst.Expression[JObject], stf: Ast.Stratification, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointProject(pred: Name.Pred, exp: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JObject]

    case class FixpointEntails(exp1: ErasedAst.Expression[JObject], exp2: ErasedAst.Expression[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]

    case class FixpointFold(pred: Name.Pred, init: ErasedAst.Expression.Var[JObject], f: ErasedAst.Expression.Var[JObject], constraints: ErasedAst.Expression.Var[JObject], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[JType]

    case class HoleError[T <: JType](sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

    case class MatchError[T <: JType](tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]

  }

  case class SelectChannelRule[T <: JType](sym: Symbol.VarSym, chan: ErasedAst.Expression[JChan[JType]], exp: ErasedAst.Expression[T])

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ErasedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ErasedAst.Term.Head], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Head

      case class Union(exp: ErasedAst.Expression[JUnknown[Nothing]], terms: List[ErasedAst.Term.Head], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Head

    }

    sealed trait Body extends ErasedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ErasedAst.Term.Body], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Body

      case class Guard(exp: ErasedAst.Expression[JUnknown[Nothing]], terms: List[ErasedAst.Term.Body], loc: SourceLocation) extends ErasedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head {
      def tpe: MonoType
    }

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head

      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head

      case class App(exp: ErasedAst.Expression[JUnknown[Nothing]], args: List[Symbol.VarSym], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head

    }

    sealed trait Body {
      def tpe: MonoType
    }

    object Body {

      case class Wild(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body

      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: MonoType)

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression[JType])

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType)

  case class BoxInt8(exp: ErasedAst.Expression[PrimInt8], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedInt8]
  case class BoxInt16(exp: ErasedAst.Expression[PrimInt16], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedInt16]
  case class BoxInt32(exp: ErasedAst.Expression[PrimInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedInt32]
  case class BoxInt64(exp: ErasedAst.Expression[PrimInt64], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedInt64]
  case class BoxChar(exp: ErasedAst.Expression[PrimChar], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedChar]
  case class BoxFloat32(exp: ErasedAst.Expression[PrimFloat32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedFloat32]
  case class BoxFloat64(exp: ErasedAst.Expression[PrimFloat64], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[BoxedFloat64]

  case class UnboxInt8(exp: ErasedAst.Expression[BoxedInt8], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt8]
  case class UnboxInt16(exp: ErasedAst.Expression[BoxedInt16], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt16]
  case class UnboxInt32(exp: ErasedAst.Expression[BoxedInt32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt32]
  case class UnboxInt64(exp: ErasedAst.Expression[BoxedInt64], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimInt64]
  case class UnboxChar(exp: ErasedAst.Expression[BoxedChar], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimChar]
  case class UnboxFloat32(exp: ErasedAst.Expression[BoxedFloat32], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat32]
  case class UnboxFloat64(exp: ErasedAst.Expression[BoxedFloat64], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[PrimFloat64]

  sealed trait Expression[+T <: JType] {
    def loc: SourceLocation
  }

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

    // Indicates an undecided type, this is to be removed
    sealed trait JUnknown[T <: JType] extends JType
  }
}

