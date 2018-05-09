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

sealed trait TypedAst

object TypedAst {

  case class Root(defs: Map[Symbol.DefnSym, TypedAst.Def],
                  effs: Map[Symbol.EffSym, TypedAst.Eff],
                  handlers: Map[Symbol.EffSym, TypedAst.Handler],
                  enums: Map[Symbol.EnumSym, TypedAst.Enum],
                  lattices: Map[Type, TypedAst.Lattice],
                  tables: Map[Symbol.TableSym, TypedAst.Table],
                  indexes: Map[Symbol.TableSym, TypedAst.Index],
                  strata: List[TypedAst.Stratum],
                  properties: List[TypedAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  time: Time) extends TypedAst

  case class Constraint(cparams: List[TypedAst.ConstraintParam], head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body], loc: SourceLocation) extends TypedAst

  case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst

  case class Eff(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst

  case class Handler(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[String, TypedAst.Case], tpe: Type, loc: SourceLocation) extends TypedAst

  case class Index(sym: Symbol.TableSym, indexes: List[List[Name.Ident]], loc: SourceLocation) extends TypedAst

  case class Lattice(tpe: Type, bot: TypedAst.Expression, top: TypedAst.Expression, equ: TypedAst.Expression, leq: TypedAst.Expression, lub: TypedAst.Expression, glb: TypedAst.Expression, loc: SourceLocation) extends TypedAst

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst

  case class Stratum(constraints: List[TypedAst.Constraint]) extends TypedAst

  sealed trait Table

  object Table {

    case class Relation(doc: Ast.Doc, sym: Symbol.TableSym, attributes: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Table

    case class Lattice(doc: Ast.Doc, sym: Symbol.TableSym, keys: List[TypedAst.Attribute], value: TypedAst.Attribute, loc: SourceLocation) extends TypedAst.Table {
      // TODO: To be refactored.
      def attributes: List[TypedAst.Attribute] = keys ::: value :: Nil
    }

  }

  sealed trait Expression extends TypedAst {
    def tpe: Type

    def eff: ast.Eff

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Unit

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class True(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class False(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Char

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float32

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float64

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int8

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int16

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int32

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int64

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.BigInt

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Str

      final def eff: ast.Eff = ast.Eff.Pure
    }

    case class Wild(tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Eff(sym: Symbol.EffSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Lambda(fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends  TypedAst.Expression

    case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends  TypedAst.Expression

    case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLoad(exp1: TypedAst.Expression, exp2: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorStore(exp1: TypedAst.Expression, exp2: Int, exp3: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLength(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class VectorSlice(exp1: TypedAst.Expression, exp2: Int, exp3: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Unique(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class HandleWith(exp: TypedAst.Expression, bindings: List[TypedAst.HandlerBinding], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class Cast(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class NativeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class NativeField(field: Field, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class NativeMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    case class UserError(tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

  }

  sealed trait Pattern extends TypedAst {
    def tpe: Type

    def loc: SourceLocation
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
      def tpe: Type = Type.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Pattern {
      def tpe: Type = Type.Str
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tuple(elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern
  }

  sealed trait Predicate extends TypedAst {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends TypedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends TypedAst.Predicate.Head

      case class False(loc: SourceLocation) extends TypedAst.Predicate.Head

      case class Atom(sym: Symbol.TableSym, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    sealed trait Body extends TypedAst.Predicate

    object Body {

      case class Atom(sym: Symbol.TableSym, polarity: Ast.Polarity, terms: List[TypedAst.Pattern], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Loop(sym: Symbol.VarSym, term: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  case class Attribute(name: String, tpe: Type, loc: SourceLocation) extends TypedAst

  case class Case(sym: Symbol.EnumSym, tag: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation) extends TypedAst

  case class HandlerBinding(sym: Symbol.EffSym, exp: TypedAst.Expression) extends TypedAst

  case class MatchRule(pat: TypedAst.Pattern, guard: TypedAst.Expression, exp: TypedAst.Expression) extends TypedAst

  case class TypeParam(name: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst

}
