/*
 * Copyright 2020 Magnus Madsen
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

object LiftedAst {

  case class Root(defs: Map[Symbol.DefnSym, LiftedAst.Def],
                  enums: Map[Symbol.EnumSym, LiftedAst.Enum],
                  latticeOps: Map[Type, LiftedAst.LatticeOps],
                  properties: List[LiftedAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[LiftedAst.FormalParam], exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation)

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, LiftedAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: LiftedAst.Expression)

  case class LatticeOps(tpe: Type, bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Unit
    }

    case class Null(tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class True(loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class False(loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Str
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ApplyClo(exp: LiftedAst.Expression, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ApplyCloTail(exp: LiftedAst.Expression, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[LiftedAst.FormalParam], actuals: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class IfThenElse(exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, exp3: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: LiftedAst.Expression, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Index(base: LiftedAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Tuple(elms: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class RecordSelect(exp: LiftedAst.Expression, field: Name.Field, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class RecordExtend(field: Name.Field, value: LiftedAst.Expression, rest: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArrayLit(elms: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArrayNew(elm: LiftedAst.Expression, len: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArrayLoad(base: LiftedAst.Expression, index: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArrayStore(base: LiftedAst.Expression, index: LiftedAst.Expression, elm: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArrayLength(base: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class ArraySlice(base: LiftedAst.Expression, beginIndex: LiftedAst.Expression, endIndex: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Ref(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Deref(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Assign(exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Existential(fparam: LiftedAst.FormalParam, exp: LiftedAst.Expression, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: LiftedAst.FormalParam, exp: LiftedAst.Expression, loc: SourceLocation) extends LiftedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Cast(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class TryCatch(exp: LiftedAst.Expression, rules: List[LiftedAst.CatchRule], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class InvokeMethod(method: Method, exp: LiftedAst.Expression, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class GetField(field: Field, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class PutField(field: Field, exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class GetStaticField(field: Field, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class PutStaticField(field: Field, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class NewChannel(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class GetChannel(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class PutChannel(exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class SelectChannel(rules: List[LiftedAst.SelectChannelRule], default: Option[LiftedAst.Expression], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Spawn(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Lazy(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class Force(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointConstraintSet(cs: List[LiftedAst.Constraint], tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointCompose(exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointSolve(exp: LiftedAst.Expression, stf: Ast.Stratification, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointEntails(exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class FixpointFold(pred: Name.Pred, exp1: LiftedAst.Expression, exp2: LiftedAst.Expression, exp3: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends LiftedAst.Expression

  }

  sealed trait SelectChannelRule

  object SelectChannelRule {

    case class SelectGet(sym: Symbol.VarSym, chan: LiftedAst.Expression, exp: LiftedAst.Expression) extends SelectChannelRule

    case class SelectPut(chan: LiftedAst.Expression, value: LiftedAst.Expression, exp: LiftedAst.Expression) extends SelectChannelRule

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends LiftedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[LiftedAst.Term.Head], tpe: Type, loc: SourceLocation) extends LiftedAst.Predicate.Head

      case class Union(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Predicate.Head

    }

    sealed trait Body extends LiftedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[LiftedAst.Term.Body], tpe: Type, loc: SourceLocation) extends LiftedAst.Predicate.Body

      case class Guard(exp: LiftedAst.Expression, loc: SourceLocation) extends LiftedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Head

      case class Lit(lit: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Head

      case class App(exp: LiftedAst.Expression, args: List[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Head

    }

    sealed trait Body

    object Body {

      case class Wild(tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Body

      case class Lit(exp: LiftedAst.Expression, tpe: Type, loc: SourceLocation) extends LiftedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: Type)

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: Type, loc: SourceLocation)

  case class Constraint(cparams: List[LiftedAst.ConstraintParam], head: LiftedAst.Predicate.Head, body: List[LiftedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.ConstraintParam

  }

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: LiftedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type)

}

