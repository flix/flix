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

import ca.uwaterloo.flix.language.ast.Ast.Source

object FinalAst {

  case class Root(defs: Map[Symbol.DefnSym, FinalAst.Def],
                  effs: Map[Symbol.EffSym, FinalAst.Eff],
                  handlers: Map[Symbol.EffSym, FinalAst.Handler],
                  enums: Map[Symbol.EnumSym, FinalAst.Enum],
                  relations: Map[Symbol.RelSym, FinalAst.Relation],
                  lattices: Map[Symbol.LatSym, FinalAst.Lattice],
                  latticeComponents: Map[MonoType, FinalAst.LatticeComponents],
                  properties: List[FinalAst.Property],
                  specialOps: Map[SpecialOperator, Map[MonoType, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) {
    var method: Method = null
  }

  case class Eff(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[FinalAst.FormalParam], tpe: MonoType, loc: SourceLocation)

  case class Handler(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation)

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[String, FinalAst.Case], tpe: MonoType, loc: SourceLocation)

  case class Relation(mod: Ast.Modifiers, sym: Symbol.RelSym, attr: List[FinalAst.Attribute], loc: SourceLocation)

  case class Lattice(mod: Ast.Modifiers, sym: Symbol.LatSym, attr: List[FinalAst.Attribute], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: FinalAst.Expression) {
    def loc: SourceLocation = defn.loc
  }

  case class LatticeComponents(tpe: MonoType, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation)

  sealed trait Expression {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expression {

    case object Unit extends FinalAst.Expression {
      final val tpe = MonoType.Unit
      final val loc = SourceLocation.Unknown
    }

    case object True extends FinalAst.Expression {
      final val tpe = MonoType.Bool
      final val loc = SourceLocation.Unknown
    }

    case object False extends FinalAst.Expression {
      final val tpe = MonoType.Bool
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends FinalAst.Expression {
      final val tpe = MonoType.Char
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends FinalAst.Expression {
      final val tpe = MonoType.Float32
      final val loc = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends FinalAst.Expression {
      final val tpe = MonoType.Float64
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends FinalAst.Expression {
      final val tpe = MonoType.Int8
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends FinalAst.Expression {
      final val tpe = MonoType.Int16
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends FinalAst.Expression {
      final val tpe = MonoType.Int32
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends FinalAst.Expression {
      final val tpe = MonoType.Int64
      final val loc = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends FinalAst.Expression {
      final val tpe = MonoType.BigInt
      final val loc = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends FinalAst.Expression {
      final val tpe = MonoType.Str
      final val loc = SourceLocation.Unknown
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    // TODO: Get rid of the fnMonoType here.
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], fnMonoType: MonoType, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyClo(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyEff(sym: Symbol.EffSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyCloTail(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyEffTail(sym: Symbol.EffSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], actuals: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class IfThenElse(exp1: FinalAst.Expression, exp2: FinalAst.Expression, exp3: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Branch(exp: FinalAst.Expression, branches: Map[Symbol.LabelSym, FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    // NB: After lambda lifting and closure conversion `exp1` is guaranteed to be a MkClosureDef.
    case class LetRec(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Index(base: FinalAst.Expression, offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Tuple(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordSelect(exp: FinalAst.Expression, label: String, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordExtend(label: String, value: FinalAst.Expression, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordRestrict(label: String, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLit(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayNew(elm: FinalAst.Expression, len: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLoad(base: FinalAst.Expression, index: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayStore(base: FinalAst.Expression, index: FinalAst.Expression, elm: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLength(base: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArraySlice(base: FinalAst.Expression, beginIndex: FinalAst.Expression, endIndex: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Ref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Deref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Assign(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class HandleWith(exp: FinalAst.Expression, bindings: List[FinalAst.HandlerBinding], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Existential(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: MonoType = MonoType.Bool
    }

    case class Universal(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: MonoType = MonoType.Bool
    }

    case class TryCatch(exp: FinalAst.Expression, rules: List[FinalAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NativeConstructor(constructor: Constructor[_], args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NativeField(field: Field, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NativeMethod(method: Method, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NewChannel(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetChannel(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutChannel(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class SelectChannel(rules: List[FinalAst.SelectChannelRule], default: Option[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Spawn(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Sleep(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointConstraint(c: FinalAst.Constraint, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointCompose(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointSolve(uid: Ast.UId, exp: FinalAst.Expression, stf: Ast.Stratification, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointProject(pred: FinalAst.PredicateWithParam, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointEntails(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class UserError(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class MatchError(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class SwitchError(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

  }

  case class SelectChannelRule(sym: Symbol.VarSym, chan: FinalAst.Expression, exp: FinalAst.Expression)

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends FinalAst.Predicate

    object Head {

      case class Atom(pred: FinalAst.PredicateWithParam, terms: List[FinalAst.Term.Head], tpe: MonoType, loc: SourceLocation) extends FinalAst.Predicate.Head

    }

    sealed trait Body extends FinalAst.Predicate

    object Body {

      case class Atom(pred: FinalAst.PredicateWithParam, polarity: Ast.Polarity, terms: List[FinalAst.Term.Body], tpe: MonoType, loc: SourceLocation) extends FinalAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[FinalAst.Term.Body], loc: SourceLocation) extends FinalAst.Predicate.Body

      case class Functional(varSym: Symbol.VarSym, defSym: Symbol.DefnSym, terms: List[Symbol.VarSym], loc: SourceLocation) extends FinalAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head

    object Head {

      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Head

      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Head

      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Head

      case class App(sym: Symbol.DefnSym, args: List[Symbol.VarSym], tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Head

    }

    sealed trait Body

    object Body {

      case class Wild(tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Body

      case class QuantVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Body

      case class CapturedVar(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Body

      case class Lit(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: MonoType)

  case class Case(sym: Symbol.EnumSym, tag: Name.Ident, tpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: FinalAst.Expression)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body])

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType)

  case class HandlerBinding(sym: Symbol.EffSym, exp: FinalAst.Expression)

  case class PredicateWithParam(sym: Symbol.PredSym, exp: FinalAst.Expression)

}
