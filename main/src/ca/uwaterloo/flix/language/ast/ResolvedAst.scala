/*
 *  Copyright 2017 Magnus Madsen
 *  
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import java.lang.reflect.{Constructor, Field, Method}

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import scala.collection.immutable.List

object ResolvedAst {

  case class Program(defs: Map[Symbol.DefnSym, ResolvedAst.Def],
                     effs: Map[Symbol.EffSym, ResolvedAst.Eff],
                     handlers: Map[Symbol.EffSym, ResolvedAst.Handler],
                     enums: Map[Symbol.EnumSym, ResolvedAst.Enum],
                     classes: Map[Symbol.ClassSym, ResolvedAst.Class],
                     impls: Map[Symbol.ClassSym, ResolvedAst.Impl],
                     relations: Map[Symbol.RelSym, ResolvedAst.Relation],
                     lattices: Map[Symbol.LatSym, ResolvedAst.Lattice],
                     latticeComponents: Map[Type, ResolvedAst.LatticeComponents],
                     properties: List[ResolvedAst.Property],
                     reachable: Set[Symbol.DefnSym],
                     sources: Map[Source, SourceLocation])

  case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, sc: Scheme, eff: Type, loc: SourceLocation)

  case class Eff(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], sc: Scheme, eff: Type, loc: SourceLocation)

  case class Handler(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, sc: Scheme, eff: Type, loc: SourceLocation)

  // TODO
  case class Law()

  // TODO
  case class Sig()

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[ResolvedAst.TypeParam], cases: Map[String, ResolvedAst.Case], tpe: Type, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ResolvedAst.Expression, loc: SourceLocation)

  case class Class(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, quantifiers: List[Type.Var], head: ResolvedAst.SimpleClass, body: List[ResolvedAst.SimpleClass], sigs: Map[String, ResolvedAst.Sig], laws: List[ResolvedAst.Law], loc: SourceLocation)

  case class Impl(doc: Ast.Doc, mod: Ast.Modifiers, head: ResolvedAst.ComplexClass, body: List[ResolvedAst.ComplexClass], defs: List[ResolvedAst.Def], loc: SourceLocation)

  // TODO: Disallow.

  case class Relation(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.RelSym, tparams: List[ResolvedAst.TypeParam], attr: List[ResolvedAst.Attribute], sc: ast.Scheme, loc: SourceLocation)

  case class Lattice(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.LatSym, tparams: List[ResolvedAst.TypeParam], attr: List[ResolvedAst.Attribute], sc: ast.Scheme, loc: SourceLocation)

  case class LatticeComponents(tpe: Type, bot: ResolvedAst.Expression, top: ResolvedAst.Expression, equ: ResolvedAst.Expression, leq: ResolvedAst.Expression, lub: ResolvedAst.Expression, glb: ResolvedAst.Expression, ns: Name.NName, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression {
      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ResolvedAst.Expression {
      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression {
      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Eff(sym: Symbol.EffSym, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Sig(sym: Symbol.SigSym, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unit(loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Unit)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class True(loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class False(loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Char)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Float32)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Float64)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Int8)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Int16)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Int32)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Int64)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.BigInt)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Str)

      def eff: Type = Type.Cst(TypeConstructor.Pure)
    }

    case class Apply(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lambda(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Binary(op: BinaryOperator, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class IfThenElse(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, exp3: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Stm(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Match(exp: ResolvedAst.Expression, rules: List[ResolvedAst.MatchRule], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Switch(rules: List[(ResolvedAst.Expression, ResolvedAst.Expression)], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordEmpty(tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordSelect(exp: ResolvedAst.Expression, label: String, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordExtend(label: String, value: ResolvedAst.Expression, rest: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordRestrict(label: String, rest: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLit(elms: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayNew(elm: ResolvedAst.Expression, len: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLoad(base: ResolvedAst.Expression, index: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayStore(base: ResolvedAst.Expression, index: ResolvedAst.Expression, elm: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLength(base: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArraySlice(base: ResolvedAst.Expression, beginIndex: ResolvedAst.Expression, endIndex: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorLit(elms: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorNew(elm: ResolvedAst.Expression, len: Int, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorLoad(base: ResolvedAst.Expression, index: Int, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorStore(base: ResolvedAst.Expression, index: Int, elm: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorLength(base: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorSlice(base: ResolvedAst.Expression, startIndex: Int, optEndIndex: Option[Int], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ref(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Deref(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Assign(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class HandleWith(exp: ResolvedAst.Expression, bindings: List[ResolvedAst.HandlerBinding], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Existential(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)
    }

    case class Universal(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression {
      def tpe: Type = Type.Cst(TypeConstructor.Bool)
    }

    case class Ascribe(exp: ResolvedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends ResolvedAst.Expression

    case class Cast(exp: ResolvedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends ResolvedAst.Expression

    case class TryCatch(exp: ResolvedAst.Expression, rules: List[ResolvedAst.CatchRule], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeMethod(method: Method, exp: ResolvedAst.Expression, args: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class GetField(field: Field, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutField(field: Field, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class GetStaticField(field: Field, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutStaticField(field: Field, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class NewChannel(exp: ResolvedAst.Expression, tpe: Type, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class GetChannel(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutChannel(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class SelectChannel(rules: List[ResolvedAst.SelectChannelRule], default: Option[ResolvedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ProcessSpawn(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ProcessSleep(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ProcessPanic(msg: String, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointConstraintSet(cs: List[ResolvedAst.Constraint], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointCompose(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointSolve(exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointProject(sym: Symbol.PredSym, exp: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointEntails(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointFold(sym: Symbol.PredSym, init: ResolvedAst.Expression, f: ResolvedAst.Expression, constraints: ResolvedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Unit(loc: SourceLocation) extends ResolvedAst.Pattern

    case class True(loc: SourceLocation) extends ResolvedAst.Pattern

    case class False(loc: SourceLocation) extends ResolvedAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ResolvedAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Tag(sym: Symbol.EnumSym, tag: String, pat: ResolvedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Tuple(elms: scala.List[ResolvedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Array(elms: scala.List[ResolvedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class ArrayTailSpread(elms: scala.List[ResolvedAst.Pattern], sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[ResolvedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends ResolvedAst.Predicate

    object Head {

      case class Atom(sym: Symbol.PredSym, den: Denotation, terms: List[ResolvedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class Union(exp: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Predicate.Head

    }

    sealed trait Body extends ResolvedAst.Predicate

    object Body {

      case class Atom(sym: Symbol.PredSym, den: Denotation, polarity: Ast.Polarity, terms: List[ResolvedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Guard(exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Predicate.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type)

  case class Constraint(cparams: List[ResolvedAst.ConstraintParam], head: ResolvedAst.Predicate.Head, body: List[ResolvedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

  }


  case class SimpleClass(sym: Symbol.ClassSym, args: List[Type.Var], loc: SourceLocation)

  case class ComplexClass(sym: Symbol.ClassSym, polarity: Ast.Polarity, args: List[Type], loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class HandlerBinding(sym: Symbol.EffSym, exp: ResolvedAst.Expression)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ResolvedAst.Expression)

  case class MatchRule(pat: ResolvedAst.Pattern, guard: ResolvedAst.Expression, exp: ResolvedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: ResolvedAst.Expression, exp: ResolvedAst.Expression)

  case class TypeParam(name: Name.Ident, tpe: Type.Var, loc: SourceLocation)

}
