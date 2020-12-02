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

import java.lang.reflect.{Constructor, Field, Method}

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._

object ErasedAst {

//  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
//                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
//                  latticeOps: Map[MonoType, ErasedAst.LatticeOps],
//                  properties: List[ErasedAst.Property],
//                  specialOps: Map[SpecialOperator, Map[MonoType, Symbol.DefnSym]],
//                  reachable: Set[Symbol.DefnSym],
//                  sources: Map[Source, SourceLocation])
//
//  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) {
//    var method: Method = _
//  }
//
//  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)
//
//  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ErasedAst.Expression[_]) {
//    def loc: SourceLocation = defn.loc
//  }
//
//  case class LatticeOps(tpe: MonoType, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation)

  object Expression {

    case object Unit extends ErasedAst.Expression[JObject] {
      final val tpe = MonoType.Unit
      final val loc = SourceLocation.Unknown
    }

    case class Null(tpe: MonoType) extends ErasedAst.Expression[JObject] {
      final val loc = SourceLocation.Unknown
    }

    case object True extends ErasedAst.Expression[JBool] {
      final val tpe = MonoType.Bool
      final val loc = SourceLocation.Unknown
    }

    case object False extends ErasedAst.Expression[JBool] {
      final val tpe = MonoType.Bool
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends ErasedAst.Expression[JChar] {
      final val tpe = MonoType.Char
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends ErasedAst.Expression[JFloat32] {
      final val tpe = MonoType.Float32
      final val loc = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends ErasedAst.Expression[JDouble64] {
      final val tpe = MonoType.Float64
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends ErasedAst.Expression[JByte8] {
      final val tpe = MonoType.Int8
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends ErasedAst.Expression[JShort16] {
      final val tpe = MonoType.Int16
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends ErasedAst.Expression[JInt32] {
      final val tpe = MonoType.Int32
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends ErasedAst.Expression[JLong64] {
      final val tpe = MonoType.Int64
      final val loc = SourceLocation.Unknown
    }

//    case class BigInt(lit: java.math.BigInteger) extends ErasedAst.Expression[_] {
//      final val tpe = MonoType.BigInt
//      final val loc = SourceLocation.Unknown
//    }
//
//    case class Str(lit: java.lang.String) extends ErasedAst.Expression[_] {
//      final val tpe = MonoType.Str
//      final val loc = SourceLocation.Unknown
//    }
//
//    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    // TODO: Get rid of the fnMonoType here.
//    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], fnMonoType: MonoType, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ApplyClo(exp: ErasedAst.Expression[_], args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ApplyDef(sym: Symbol.DefnSym, args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ApplyCloTail(exp: ErasedAst.Expression[_], args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
    case class IfThenElse[T <: JType](exp1: ErasedAst.Expression[JBool], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[T]
//
//    case class Branch(exp: ErasedAst.Expression[_], branches: Map[Symbol.LabelSym, ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Let(sym: Symbol.VarSym, exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[_], loc: SourceLocation) extends ErasedAst.Expression[_] {
//      final val tpe: MonoType = MonoType.Bool
//    }
//
//    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Index(base: ErasedAst.Expression[_], offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Tuple(elms: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class RecordSelect(exp: ErasedAst.Expression[_], field: Name.Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[_], rest: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArrayLit(elms: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArrayNew(elm: ErasedAst.Expression[_], len: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArrayLoad(base: ErasedAst.Expression[_], index: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArrayStore(base: ErasedAst.Expression[_], index: ErasedAst.Expression[_], elm: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArrayLength(base: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class ArraySlice(base: ErasedAst.Expression[_], beginIndex: ErasedAst.Expression[_], endIndex: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Ref(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Deref(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Assign(exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[_], loc: SourceLocation) extends ErasedAst.Expression[_] {
//      def tpe: MonoType = MonoType.Bool
//    }
//
//    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[_], loc: SourceLocation) extends ErasedAst.Expression[_] {
//      def tpe: MonoType = MonoType.Bool
//    }
//
//    case class Cast(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class TryCatch(exp: ErasedAst.Expression[_], rules: List[ErasedAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class InvokeMethod(method: Method, exp: ErasedAst.Expression[_], args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class InvokeStaticMethod(method: Method, args: List[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class GetField(field: Field, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class PutField(field: Field, exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class GetStaticField(field: Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class PutStaticField(field: Field, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class NewChannel(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class GetChannel(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class PutChannel(exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class SelectChannel(rules: List[ErasedAst.SelectChannelRule], default: Option[ErasedAst.Expression[_]], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Spawn(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Lazy(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class Force(exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointConstraintSet(cs: List[ErasedAst.Constraint], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointCompose(exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointSolve(exp: ErasedAst.Expression[_], stf: Ast.Stratification, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointProject(pred: Name.Pred, exp: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointEntails(exp1: ErasedAst.Expression[_], exp2: ErasedAst.Expression[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class FixpointFold(pred: Name.Pred, init: ErasedAst.Expression.Var, f: ErasedAst.Expression.Var, constraints: ErasedAst.Expression.Var, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class HoleError(sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
//    case class MatchError(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression[_]
//
  }
//
//  case class SelectChannelRule(sym: Symbol.VarSym, chan: ErasedAst.Expression[_], exp: ErasedAst.Expression[_])
//
//  sealed trait Predicate {
//    def loc: SourceLocation
//  }
//
//  object Predicate {
//
//    sealed trait Head extends ErasedAst.Predicate
//
//    object Head {
//
//      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ErasedAst.Term.Head], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Head
//
//      case class Union(exp: ErasedAst.Expression[_], terms: List[ErasedAst.Term.Head], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Head
//
//    }
//
//    sealed trait Body extends ErasedAst.Predicate
//
//    object Body {
//
//      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ErasedAst.Term.Body], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Predicate.Body
//
//      case class Guard(exp: ErasedAst.Expression[_], terms: List[ErasedAst.Term.Body], loc: SourceLocation) extends ErasedAst.Predicate.Body
//
//    }
//
//  }
//
//  object Term {
//
//    sealed trait Head {
//      def tpe: MonoType
//    }
//
//    object Head {
//
//      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head
//
//      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head
//
//      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head
//
//      case class App(exp: ErasedAst.Expression[_], args: List[Symbol.VarSym], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Head
//
//    }
//
//    sealed trait Body {
//      def tpe: MonoType
//    }
//
//    object Body {
//
//      case class Wild(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body
//
//      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body
//
//      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body
//
//      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Term.Body
//
//    }
//
//  }
//
//  case class Attribute(name: String, tpe: MonoType)
//
//  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: MonoType, loc: SourceLocation)
//
//  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression[_])
//
//  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)
//
//  sealed trait ConstraintParam {
//    def sym: Symbol.VarSym
//  }
//
//  object ConstraintParam {
//
//    case class HeadParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.ConstraintParam
//
//    case class RuleParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.ConstraintParam
//
//  }
//
//  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)
//
//  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType)

  sealed trait Expression[T <: JType] {
    def loc: SourceLocation
  }

  sealed trait JType

  object JType {

    sealed trait JByte8 extends JType

    sealed trait JShort16 extends JType

    sealed trait JInt32 extends JType

    sealed trait JLong64 extends JType

    sealed trait JChar extends JType

    sealed trait JFloat32 extends JType

    sealed trait JDouble64 extends JType

    sealed trait JBool extends JType

    sealed trait JObject extends JType

  }
}

