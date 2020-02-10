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

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.language.debug.{FormatExpression, FormatPattern}

object TypedAst {

  case class Root(defs: Map[Symbol.DefnSym, TypedAst.Def],
                  effs: Map[Symbol.EffSym, TypedAst.Eff],
                  handlers: Map[Symbol.EffSym, TypedAst.Handler],
                  enums: Map[Symbol.EnumSym, TypedAst.Enum],
                  relations: Map[Symbol.RelSym, TypedAst.Relation],
                  lattices: Map[Symbol.LatSym, TypedAst.Lattice],
                  latticeComponents: Map[Type, TypedAst.LatticeComponents],
                  properties: List[TypedAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  // TODO: Remove .tpe from here.
  case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, sc: Scheme, tpe: Type, eff: Type, loc: SourceLocation)

  case class Eff(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], tpe: Type, eff: Type, loc: SourceLocation)

  case class Handler(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation)

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypedAst.TypeParam], cases: Map[String, TypedAst.Case], tpe: Type, loc: SourceLocation)

  case class Relation(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.RelSym, tparams: List[TypedAst.TypeParam], attr: List[TypedAst.Attribute], loc: SourceLocation)

  case class Lattice(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.LatSym, tparams: List[TypedAst.TypeParam], attr: List[TypedAst.Attribute], loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: TypedAst.Expression, loc: SourceLocation)

  case class LatticeComponents(tpe: Type, bot: TypedAst.Expression, top: TypedAst.Expression, equ: TypedAst.Expression, leq: TypedAst.Expression, lub: TypedAst.Expression, glb: TypedAst.Expression, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation

    final override def toString: String = FormatExpression.format(this)
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Unit

      final def eff: Type = Type.Pure
    }

    case class True(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool

      final def eff: Type = Type.Pure
    }

    case class False(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool

      final def eff: Type = Type.Pure
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Char

      final def eff: Type = Type.Pure
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float32

      final def eff: Type = Type.Pure
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float64

      final def eff: Type = Type.Pure
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int8

      final def eff: Type = Type.Pure
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int16

      final def eff: Type = Type.Pure
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Cst(TypeConstructor.Int32)

      final def eff: Type = Type.Pure
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int64

      final def eff: Type = Type.Pure
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Cst(TypeConstructor.BigInt)

      final def eff: Type = Type.Pure
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Cst(TypeConstructor.Str)

      final def eff: Type = Type.Pure
    }

    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      final def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      final def eff: Type = Type.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      final def eff: Type = Type.Pure
    }

    case class Eff(sym: Symbol.EffSym, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Apply(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordEmpty(tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordSelect(exp: TypedAst.Expression, label: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordExtend(label: String, value: TypedAst.Expression, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordRestrict(label: String, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class HandleWith(exp: TypedAst.Expression, bindings: List[TypedAst.HandlerBinding], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: Type, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: Type, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Cast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ProcessSleep(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ProcessPanic(msg: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointProject(sym: Symbol.PredSym, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointFold(sym: Symbol.PredSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation

    final override def toString: String = FormatPattern.format(this)
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Unit(loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Unit
    }

    case class True(loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Bool
    }

    case class False(loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Cst(TypeConstructor.Int32)
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Cst(TypeConstructor.BigInt)
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Cst(TypeConstructor.Str)
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tuple(elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Array(elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class ArrayTailSpread(elms: List[TypedAst.Pattern], sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends TypedAst.Predicate

    object Head {

      case class Atom(sym: Symbol.PredSym, den: Denotation, terms: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Head

      case class Union(exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    sealed trait Body extends TypedAst.Predicate

    object Body {

      case class Atom(sym: Symbol.PredSym, den: Denotation, polarity: Ast.Polarity, terms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Guard(exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  case class Attribute(name: String, tpe: Type, loc: SourceLocation)

  case class Case(sym: Symbol.EnumSym, tag: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Constraint(cparams: List[TypedAst.ConstraintParam], head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class HandlerBinding(sym: Symbol.EffSym, exp: TypedAst.Expression)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: TypedAst.Expression)

  case class MatchRule(pat: TypedAst.Pattern, guard: TypedAst.Expression, exp: TypedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: TypedAst.Expression, exp: TypedAst.Expression)

  case class TypeParam(name: Name.Ident, tpe: Type.Var, loc: SourceLocation)

}
