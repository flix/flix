/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, IntroducedBy, Source}
import ca.uwaterloo.flix.language.phase.{ClosureConv, LambdaLift}

object SimplifiedAst {

  case class Root(defs: Map[Symbol.DefnSym, SimplifiedAst.Def],
                  enums: Map[Symbol.EnumSym, SimplifiedAst.Enum],
                  latticeOps: Map[Type, SimplifiedAst.LatticeOps],
                  properties: List[SimplifiedAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation)

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, SimplifiedAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: SimplifiedAst.Expression)

  case class LatticeOps(tpe: Type, bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case object Unit extends SimplifiedAst.Expression {
      def tpe: Type = Type.Unit

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Null(tpe: Type) extends SimplifiedAst.Expression {
      def loc: SourceLocation = SourceLocation.Unknown
    }

    case object True extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case object False extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Char

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Float32

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Float64

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Int8

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Int16

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Int32

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Int64

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends SimplifiedAst.Expression {
      def tpe: Type = Type.BigInt

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Str

      def loc: SourceLocation = SourceLocation.Unknown
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Lambda(fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class LambdaClosure(fparams: List[SimplifiedAst.FormalParam], freeVars: List[FreeVar], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(LambdaLift.getClass)
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyDef(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Index(base: SimplifiedAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordSelect(exp: SimplifiedAst.Expression, field: Name.Field, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordExtend(field: Name.Field, value: SimplifiedAst.Expression, rest: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLit(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayNew(elm: SimplifiedAst.Expression, len: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLoad(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayStore(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, elm: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLength(base: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArraySlice(base: SimplifiedAst.Expression, beginIndex: SimplifiedAst.Expression, endIndex: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Ref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Deref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Assign(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Existential(fparam: SimplifiedAst.FormalParam, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: SimplifiedAst.FormalParam, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Cast(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class TryCatch(exp: SimplifiedAst.Expression, rules: List[SimplifiedAst.CatchRule], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeMethod(method: Method, exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetField(field: Field, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutField(field: Field, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetStaticField(field: Field, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutStaticField(field: Field, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NewChannel(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetChannel(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutChannel(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class SelectChannel(rules: List[SimplifiedAst.SelectChannelRule], default: Option[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Spawn(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Lazy(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Force(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointConstraintSet(cs: List[SimplifiedAst.Constraint], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointCompose(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointSolve(exp: SimplifiedAst.Expression, stf: Ast.Stratification, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointEntails(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointFold(pred: Name.Pred, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

  }

  case class SelectChannelRule(sym: Symbol.VarSym, chan: SimplifiedAst.Expression, exp: SimplifiedAst.Expression)

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[SimplifiedAst.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Head

      case class Union(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }

    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[SimplifiedAst.Term.Body], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Guard(exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class Lit(lit: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class App(exp: SimplifiedAst.Expression, args: List[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

    }

    sealed trait Body

    object Body {

      case class Wild(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class Lit(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: Type)

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: Type, loc: SourceLocation)

  case class Constraint(cparams: List[SimplifiedAst.ConstraintParam], head: SimplifiedAst.Predicate.Head, body: List[SimplifiedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.ConstraintParam

  }

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: SimplifiedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type)

}
