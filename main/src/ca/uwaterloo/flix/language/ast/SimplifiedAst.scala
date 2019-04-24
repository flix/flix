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

import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy, Source}
import ca.uwaterloo.flix.language.phase.{ClosureConv, LambdaLift, Tailrec}

object SimplifiedAst {

  case class Root(defs: Map[Symbol.DefnSym, SimplifiedAst.Def],
                  effs: Map[Symbol.EffSym, SimplifiedAst.Eff],
                  handlers: Map[Symbol.EffSym, SimplifiedAst.Handler],
                  enums: Map[Symbol.EnumSym, SimplifiedAst.Enum],
                  relations: Map[Symbol.RelSym, SimplifiedAst.Relation],
                  lattices: Map[Symbol.LatSym, SimplifiedAst.Lattice],
                  latticeComponents: Map[Type, SimplifiedAst.LatticeComponents],
                  properties: List[SimplifiedAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation)

  case class Eff(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[SimplifiedAst.FormalParam], tpe: Type, loc: SourceLocation)

  case class Handler(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation)

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[String, SimplifiedAst.Case], tpe: Type, loc: SourceLocation)

  case class Relation(mod: Ast.Modifiers, sym: Symbol.RelSym, attr: List[SimplifiedAst.Attribute], loc: SourceLocation)

  case class Lattice(mod: Ast.Modifiers, sym: Symbol.LatSym, attr: List[SimplifiedAst.Attribute], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: SimplifiedAst.Expression)

  case class LatticeComponents(tpe: Type, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case object Unit extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Unit)
      final val loc = SourceLocation.Unknown
    }

    case object True extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Bool)
      final val loc = SourceLocation.Unknown
    }

    case object False extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Bool)
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Char)
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Float32)
      final val loc = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Float64)
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Int8)
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Int16)
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Int32)
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Int64)
      final val loc = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.BigInt)
      final val loc = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends SimplifiedAst.Expression {
      final val tpe = Type.Cst(TypeConstructor.Str)
      final val loc = SourceLocation.Unknown
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Eff(sym: Symbol.EffSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @EliminatedBy(LambdaLift.getClass)
    case class Lambda(fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @EliminatedBy(ClosureConv.getClass)
    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    @EliminatedBy(LambdaLift.getClass)
    case class LambdaClosure(fparams: List[SimplifiedAst.FormalParam], freeVars: List[FreeVar], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(LambdaLift.getClass)
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyDef(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyEff(sym: Symbol.EffSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(Tailrec.getClass)
    case class ApplyCloTail(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(Tailrec.getClass)
    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(Tailrec.getClass)
    case class ApplyEffTail(sym: Symbol.EffSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(Tailrec.getClass)
    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[SimplifiedAst.FormalParam], actuals: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    // NB: After lambda lifting and closure conversion `exp1` is guaranteed to be a MkClosureDef.
    case class LetRec(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: String, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe: Type = Type.Cst(TypeConstructor.Bool)
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: String, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Index(base: SimplifiedAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordSelect(exp: SimplifiedAst.Expression, label: String, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordExtend(label: String, value: SimplifiedAst.Expression, rest: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordRestrict(label: String, rest: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLit(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayNew(elm: SimplifiedAst.Expression, len: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLoad(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayStore(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, elm: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLength(base: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArraySlice(base: SimplifiedAst.Expression, beginIndex: SimplifiedAst.Expression, endIndex: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Ref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Deref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Assign(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class HandleWith(exp: SimplifiedAst.Expression, bindings: List[SimplifiedAst.HandlerBinding], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Existential(fparam: SimplifiedAst.FormalParam, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)
    }

    case class Universal(fparam: SimplifiedAst.FormalParam, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)
    }

    case class TryCatch(exp: SimplifiedAst.Expression, rules: List[SimplifiedAst.CatchRule], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NativeConstructor(constructor: Constructor[_], args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NativeMethod(method: Method, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NewChannel(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetChannel(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutChannel(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class SelectChannel(rules: List[SimplifiedAst.SelectChannelRule], default: Option[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Spawn(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Sleep(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointConstraint(c: SimplifiedAst.Constraint, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointCompose(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointSolve(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointProject(pred: SimplifiedAst.PredicateWithParam, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class FixpointEntails(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class SwitchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

  }

  case class SelectChannelRule(sym: Symbol.VarSym, chan: SimplifiedAst.Expression, exp: SimplifiedAst.Expression)

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class Atom(pred: SimplifiedAst.PredicateWithParam, terms: List[SimplifiedAst.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }

    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Atom(pred: SimplifiedAst.PredicateWithParam, polarity: Ast.Polarity, terms: List[SimplifiedAst.Term.Body], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[SimplifiedAst.Term.Body], loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Functional(sym: Symbol.VarSym, term: SimplifiedAst.Term.Head, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class Lit(lit: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class App(sym: Symbol.DefnSym, args: List[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

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

  case class Case(sym: Symbol.EnumSym, tag: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Constraint(cparams: List[SimplifiedAst.ConstraintParam], head: SimplifiedAst.Predicate.Head, body: List[SimplifiedAst.Predicate.Body])

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.ConstraintParam

  }

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: SimplifiedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type)

  case class HandlerBinding(sym: Symbol.EffSym, exp: SimplifiedAst.Expression)

  case class PredicateWithParam(sym: Symbol.PredSym, exp: SimplifiedAst.Expression)

}
