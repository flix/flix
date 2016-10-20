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

trait WeededAst

object WeededAst {

  case class Program(roots: List[WeededAst.Root], hooks: Map[Name.NName, Map[String, Ast.Hook]], time: Time) extends WeededAst

  case class Root(decls: List[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration extends WeededAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Namespace(name: Name.NName, decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Definition(doc: Option[Ast.Documentation], ann: Ast.Annotations, ident: Name.Ident, tparams: List[Name.Ident], params: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Declaration

    case class Signature(doc: Option[Ast.Documentation], ident: Name.Ident, params: List[WeededAst.FormalParam], tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Declaration

    case class External(doc: Option[Ast.Documentation], ident: Name.Ident, params: List[WeededAst.FormalParam], tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Declaration

    case class Law(doc: Option[Ast.Documentation], ident: Name.Ident, tparams: List[ParsedAst.ContextBound], params: List[WeededAst.FormalParam], tpe: WeededAst.Type, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

    case class Enum(doc: Option[Ast.Documentation], ident: Name.Ident, tparams: List[Name.Ident], cases: Map[String, WeededAst.Case], loc: SourceLocation) extends WeededAst.Declaration

    case class Class(doc: Option[Ast.Documentation], ident: Name.Ident, tparams: List[WeededAst.Type], /* bounds: List[ContextBound],*/ decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Impl(doc: Option[Ast.Documentation], ident: Name.Ident, tparams: List[WeededAst.Type], /*bounds: List[ContextBound],*/ decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Fact(head: WeededAst.Predicate.Head, loc: SourceLocation) extends WeededAst.Declaration

    case class Rule(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation) extends WeededAst.Declaration

    case class Index(qname: Name.QName, indexes: List[List[Name.Ident]], loc: SourceLocation) extends WeededAst.Declaration

    case class BoundedLattice(tpe: WeededAst.Type, bot: WeededAst.Expression, top: WeededAst.Expression, leq: WeededAst.Expression, lub: WeededAst.Expression, glb: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

  }

  sealed trait Table extends WeededAst.Declaration {
    def ident: Name.Ident

    def loc: SourceLocation
  }

  object Table {

    case class Relation(doc: Option[Ast.Documentation], ident: Name.Ident, attr: List[WeededAst.Attribute], loc: SourceLocation) extends WeededAst.Table

    case class Lattice(doc: Option[Ast.Documentation], ident: Name.Ident, keys: List[WeededAst.Attribute], value: WeededAst.Attribute, loc: SourceLocation) extends WeededAst.Table

  }

  sealed trait Expression extends WeededAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends WeededAst.Expression

    case class VarOrRef(name: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class Unit(loc: SourceLocation) extends WeededAst.Expression

    case class True(loc: SourceLocation) extends WeededAst.Expression

    case class False(loc: SourceLocation) extends WeededAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends WeededAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends WeededAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends WeededAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends WeededAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends WeededAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends WeededAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends WeededAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends WeededAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends WeededAst.Expression

    case class Apply(lambda: WeededAst.Expression, args: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Lambda(params: List[Name.Ident], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Unary(op: UnaryOperator, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Binary(op: BinaryOperator, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class IfThenElse(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Let(ident: Name.Ident, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Match(exp: WeededAst.Expression, rules: List[(WeededAst.Pattern, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class Switch(rules: List[(WeededAst.Expression, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FNil(loc: SourceLocation) extends WeededAst.Expression

    case class FList(hd: WeededAst.Expression, tl: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FVec(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FSet(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FMap(elms: List[(WeededAst.Expression, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class GetIndex(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutIndex(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Existential(params: List[WeededAst.FormalParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Universal(params: List[WeededAst.FormalParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(exp: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class UserError(loc: SourceLocation) extends WeededAst.Expression

  }

  sealed trait Pattern extends WeededAst {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Pattern

    case class Unit(loc: SourceLocation) extends WeededAst.Pattern

    case class True(loc: SourceLocation) extends WeededAst.Pattern

    case class False(loc: SourceLocation) extends WeededAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends WeededAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends WeededAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends WeededAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends WeededAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends WeededAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends WeededAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends WeededAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends WeededAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends WeededAst.Pattern

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, pat: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class Tuple(elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class FNil(loc: SourceLocation) extends WeededAst.Pattern

    case class FList(hd: WeededAst.Pattern, tl: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class FVec(elms: List[WeededAst.Pattern], rest: Option[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class FSet(elms: List[WeededAst.Pattern], rest: Option[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class FMap(elms: List[(WeededAst.Pattern, WeededAst.Pattern)], rest: Option[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

  }

  sealed trait Predicate extends WeededAst

  object Predicate {

    sealed trait Head extends WeededAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends WeededAst.Predicate.Head

      case class False(loc: SourceLocation) extends WeededAst.Predicate.Head

      case class Table(name: Name.QName, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Head

    }

    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class Table(name: Name.QName, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Filter(name: Name.QName, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

    }

  }

  sealed trait Type extends WeededAst

  object Type {

    case class Unit(loc: SourceLocation) extends WeededAst.Type

    case class Var(qname: Name.Ident, loc: SourceLocation) extends WeededAst.Type

    case class Ref(qname: Name.QName, loc: SourceLocation) extends WeededAst.Type

    case class Tuple(elms: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

    case class Arrow(tparams: List[WeededAst.Type], retType: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Apply(base: WeededAst.Type, tparams: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: WeededAst.Type) extends WeededAst

  case class FormalParam(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst

}
