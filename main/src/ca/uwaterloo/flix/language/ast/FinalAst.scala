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

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject

object FinalAst {

  case class Root(defs: Map[Symbol.DefnSym, FinalAst.Def],
                  effs: Map[Symbol.EffSym, FinalAst.Eff],
                  handlers: Map[Symbol.EffSym, FinalAst.Handler],
                  enums: Map[Symbol.EnumSym, FinalAst.Enum],
                  relations: Map[Symbol.RelSym, FinalAst.Relation],
                  lattices: Map[Symbol.LatSym, FinalAst.Lattice],
                  latticeComponents: Map[Type, FinalAst.LatticeComponents],
                  strata: List[FinalAst.Stratum],
                  properties: List[FinalAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym])

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body]) {
    val isFact: Boolean = body.isEmpty
    val isRule: Boolean = !isFact
  }

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) {
    var method: Method = null
  }

  case class Eff(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[FinalAst.FormalParam], tpe: Type, loc: SourceLocation)

  case class Handler(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, fparams: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: Type, loc: SourceLocation)

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[String, FinalAst.Case], tpe: Type, loc: SourceLocation)

  case class Relation(mod: Ast.Modifiers, sym: Symbol.RelSym, attr: List[FinalAst.Attribute], loc: SourceLocation)

  case class Lattice(mod: Ast.Modifiers, sym: Symbol.LatSym, attr: List[FinalAst.Attribute], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: FinalAst.Expression) {
    def loc: SourceLocation = defn.loc
  }

  case class Stratum(constraints: List[FinalAst.Constraint])

  case class LatticeComponents(tpe: Type, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case object Unit extends FinalAst.Expression {
      final val tpe = Type.Unit
      final val loc = SourceLocation.Unknown
    }

    case object True extends FinalAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case object False extends FinalAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends FinalAst.Expression {
      final val tpe = Type.Char
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends FinalAst.Expression {
      final val tpe = Type.Float32
      final val loc = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends FinalAst.Expression {
      final val tpe = Type.Float64
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends FinalAst.Expression {
      final val tpe = Type.Int8
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends FinalAst.Expression {
      final val tpe = Type.Int16
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends FinalAst.Expression {
      final val tpe = Type.Int32
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends FinalAst.Expression {
      final val tpe = Type.Int64
      final val loc = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends FinalAst.Expression {
      final val tpe = Type.BigInt
      final val loc = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends FinalAst.Expression {
      final val tpe = Type.Str
      final val loc = SourceLocation.Unknown
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    // TODO: Get rid of the fnType here.
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], fnType: Type, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyClo(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyEff(sym: Symbol.EffSym, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyCloTail(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyEffTail(sym: Symbol.EffSym, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], actuals: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class IfThenElse(exp1: FinalAst.Expression, exp2: FinalAst.Expression, exp3: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Branch(exp: FinalAst.Expression, branches: Map[Symbol.LabelSym, FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    // NB: After lambda lifting and closure conversion `exp1` is guaranteed to be a MkClosureDef.
    case class LetRec(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: String, exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Index(base: FinalAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Tuple(elms: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLit(elms: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayNew(elm: FinalAst.Expression, len: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLoad(base: FinalAst.Expression, index: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayStore(base: FinalAst.Expression, index: FinalAst.Expression, elm: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLength(base: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ArraySlice(base: FinalAst.Expression, beginIndex: FinalAst.Expression, endIndex: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Ref(exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Deref(exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Assign(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class HandleWith(exp: FinalAst.Expression, bindings: List[FinalAst.HandlerBinding], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Existential(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class TryCatch(exp: FinalAst.Expression, rules: List[FinalAst.CatchRule], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class NativeConstructor(constructor: Constructor[_], args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class NativeMethod(method: Method, args: List[FinalAst.Expression], tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class NewRelation(sym: Symbol.RelSym, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class NewLattice(sym: Symbol.LatSym, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class Constraint(con: FinalAst.Constraint, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class ConstraintUnion(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointSolve(exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class FixpointCheck(exp: FinalAst.Expression, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class UserError(tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends FinalAst.Expression

    case class SwitchError(tpe: Type, loc: SourceLocation) extends FinalAst.Expression

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends FinalAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends FinalAst.Predicate.Head

      case class False(loc: SourceLocation) extends FinalAst.Predicate.Head

      case class RelAtom(base: Option[Symbol.VarSym], sym: Symbol.RelSym, terms: List[FinalAst.Term.Head], loc: SourceLocation) extends FinalAst.Predicate.Head

      case class LatAtom(base: Option[Symbol.VarSym], sym: Symbol.LatSym, terms: List[FinalAst.Term.Head], loc: SourceLocation) extends FinalAst.Predicate.Head

    }

    sealed trait Body extends FinalAst.Predicate

    object Body {

      case class RelAtom(base: Option[Symbol.VarSym], sym: Symbol.RelSym, polarity: Ast.Polarity, terms: List[FinalAst.Term.Body], index2sym: List[Symbol.VarSym], loc: SourceLocation) extends FinalAst.Predicate.Body

      case class LatAtom(base: Option[Symbol.VarSym], sym: Symbol.LatSym, polarity: Ast.Polarity, terms: List[FinalAst.Term.Body], index2sym: List[Symbol.VarSym], loc: SourceLocation) extends FinalAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[FinalAst.Term.Body], loc: SourceLocation) extends FinalAst.Predicate.Body

      case class Functional(varSym: Symbol.VarSym, defSym: Symbol.DefnSym, terms: List[Symbol.VarSym], loc: SourceLocation) extends FinalAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head

    object Head {

      case class FreeVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Head

      case class BoundVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Head

      case class Lit(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Head

      case class App(sym: Symbol.DefnSym, args: List[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends FinalAst.Term.Head

    }

    sealed trait Body

    object Body {

      case class Wild(tpe: Type, loc: SourceLocation) extends FinalAst.Term.Body

      case class FreeVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Body

      case class BoundVar(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Body

      case class Lit(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends FinalAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: Type)

  case class Case(sym: Symbol.EnumSym, tag: Name.Ident, tpe: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: FinalAst.Expression)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends FinalAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: Type)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type)

  case class HandlerBinding(sym: Symbol.EffSym, exp: FinalAst.Expression)

}
