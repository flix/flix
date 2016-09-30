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

sealed trait TypedAst

object TypedAst {

  case class Root(definitions: Map[Symbol.DefnSym, TypedAst.Declaration.Definition],
                  lattices: Map[Type, TypedAst.Declaration.BoundedLattice],
                  tables: Map[Symbol.TableSym, TypedAst.Table],
                  indexes: Map[Symbol.TableSym, TypedAst.Declaration.Index],
                  facts: List[TypedAst.Declaration.Fact],
                  rules: List[TypedAst.Declaration.Rule],
                  hooks: Map[Symbol.Resolved, Ast.Hook], // TODO:  Symbol.Res
                  properties: List[TypedAst.Property],
                  time: Time) extends TypedAst

  sealed trait Declaration extends TypedAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Definition(ann: Ast.Annotations, sym: Symbol.DefnSym, formals: List[TypedAst.FormalParam], exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Declaration

    case class Index(sym: Symbol.TableSym, indexes: List[List[Name.Ident]], loc: SourceLocation) extends TypedAst.Declaration

    case class Fact(head: TypedAst.Predicate.Head, loc: SourceLocation) extends TypedAst.Declaration

    case class Rule(head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body], loc: SourceLocation) extends TypedAst.Declaration

    case class BoundedLattice(tpe: Type,
                              bot: TypedAst.Expression,
                              top: TypedAst.Expression,
                              leq: TypedAst.Expression,
                              lub: TypedAst.Expression,
                              glb: TypedAst.Expression,
                              loc: SourceLocation) extends TypedAst.Declaration

  }

  sealed trait Table

  object Table {

    case class Relation(sym: Symbol.TableSym, attributes: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[TypedAst.Attribute], value: TypedAst.Attribute, loc: SourceLocation) extends TypedAst.Table

  }

  sealed trait Expression extends TypedAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Unit
    }

    case class True(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool
    }

    case class False(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Str
    }

    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Ref(name: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Hook(hook: Ast.Hook, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Lambda(args: List[TypedAst.FormalParam], body: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Match(exp: TypedAst.Expression, rules: List[(TypedAst.Pattern, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: Name.Ident, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FNone(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FSome(exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FNil(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FList(hd: TypedAst.Expression, tl: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FVec(elms: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FSet(elms: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FMap(elms: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetIndex(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutIndex(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Existential(params: List[TypedAst.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[TypedAst.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class UserError(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

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
      def tpe: Type = Type.Int16
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

    case class Tag(sym: Symbol.EnumSym, tag: Name.Ident, pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tuple(elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FNone(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FSome(pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FNil(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FList(hd: TypedAst.Pattern, tl: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FVec(elms: List[TypedAst.Pattern], rest: Option[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FSet(elms: List[TypedAst.Pattern], rest: Option[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class FMap(elms: List[(TypedAst.Pattern, TypedAst.Pattern)], rest: Option[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

  }

  sealed trait Predicate extends TypedAst {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends TypedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends TypedAst.Predicate.Head

      case class False(loc: SourceLocation) extends TypedAst.Predicate.Head

      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    sealed trait Body extends TypedAst.Predicate

    object Body {

      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class ApplyFilter(name: Symbol.DefnSym, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class ApplyHookFilter(hook: Ast.Hook, terms: List[TypedAst.Expression], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class NotEqual(sym1: Symbol.VarSym, sym2: Symbol.VarSym, loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends TypedAst

  case class FormalParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst

  case class Property(law: Law, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst

}
