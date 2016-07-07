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

// TODO: Why are there seq here?
import scala.collection.immutable.Seq

trait WeededAst

object WeededAst {

  case class Program(roots: List[WeededAst.Root], hooks: Map[Symbol.Resolved, Ast.Hook], time: Time) extends WeededAst

  case class Root(decls: List[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration extends WeededAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Namespace(name: Name.NName, decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Definition(ann: Ast.Annotations, ident: Name.Ident, params: List[Ast.FormalParam], exp: WeededAst.Expression, tpe: Type, loc: SourceLocation) extends WeededAst.Declaration

    case class Signature(ident: Name.Ident, params: Seq[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends WeededAst.Declaration

    case class External(ident: Name.Ident, params: Seq[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends WeededAst.Declaration

    case class Law(ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], params: Seq[Ast.FormalParam], tpe: Type, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

    case class Enum(ident: Name.Ident, cases: Map[String, WeededAst.Case], loc: SourceLocation) extends WeededAst.Declaration

    case class Class(ident: Name.Ident, tparams: Seq[Type], /* bounds: Seq[ContextBound],*/ decls: Seq[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Impl(ident: Name.Ident, tparams: Seq[Type], /*bounds: Seq[ContextBound],*/ decls: Seq[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Fact(head: WeededAst.Predicate.Head, loc: SourceLocation) extends WeededAst.Declaration

    case class Rule(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation) extends WeededAst.Declaration

    case class Index(ident: Name.Ident, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends WeededAst.Declaration

    @deprecated("Will be replaced by type classes", "0.1.0")
    case class BoundedLattice(tpe: Type, bot: WeededAst.Expression, top: WeededAst.Expression, leq: WeededAst.Expression, lub: WeededAst.Expression, glb: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

  }

  sealed trait Table extends WeededAst.Declaration {
    def ident: Name.Ident

    def loc: SourceLocation
  }

  object Table {

    case class Relation(ident: Name.Ident, attr: List[Ast.Attribute], loc: SourceLocation) extends WeededAst.Table

    case class Lattice(ident: Name.Ident, keys: List[Ast.Attribute], value: Ast.Attribute, loc: SourceLocation) extends WeededAst.Table

  }

  // TODO: Deprecated to be removed once terms are removed.
  sealed trait Literal extends WeededAst {
    def loc: SourceLocation
  }

  object Literal {

    case class Unit(loc: SourceLocation) extends WeededAst.Literal

    case class True(loc: SourceLocation) extends WeededAst.Literal

    case class False(loc: SourceLocation) extends WeededAst.Literal

    case class Char(lit: scala.Char, loc: SourceLocation) extends WeededAst.Literal

    case class Float32(lit: scala.Float, loc: SourceLocation) extends WeededAst.Literal

    case class Float64(lit: scala.Double, loc: SourceLocation) extends WeededAst.Literal

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends WeededAst.Literal

    case class Int16(lit: scala.Short, loc: SourceLocation) extends WeededAst.Literal

    case class Int32(lit: scala.Int, loc: SourceLocation) extends WeededAst.Literal

    case class Int64(lit: scala.Long, loc: SourceLocation) extends WeededAst.Literal

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends WeededAst.Literal

    case class Str(lit: java.lang.String, loc: SourceLocation) extends WeededAst.Literal

  }

  sealed trait Expression extends WeededAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends WeededAst.Expression

    case class Var(name: Name.QName, loc: SourceLocation) extends WeededAst.Expression

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

    case class Tag(enum: Name.QName, tag: Name.Ident, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FNone(loc: SourceLocation) extends WeededAst.Expression

    case class FSome(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FNil(loc: SourceLocation) extends WeededAst.Expression

    case class FList(hd: WeededAst.Expression, tl: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FVec(elms: Seq[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FSet(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class FMap(elms: Seq[(WeededAst.Expression, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class GetIndex(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutIndex(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Existential(params: Seq[Ast.FormalParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Universal(params: Seq[Ast.FormalParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(exp: WeededAst.Expression, tpe: Type, loc: SourceLocation) extends WeededAst.Expression

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

    case class Tag(enum: Name.QName, tag: Name.Ident, pat: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class Tuple(elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class FNone(loc: SourceLocation) extends WeededAst.Pattern

    case class FSome(pat: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

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

      case class Table(name: Name.QName, terms: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Predicate.Head

    }

    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class Ambiguous(name: Name.QName, terms: List[WeededAst.Term.Body], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: WeededAst.Term.Head, loc: SourceLocation) extends WeededAst.Predicate.Body

    }

  }

  // TODO: Deprecated and to be replaced by expressions.
  sealed trait Term extends WeededAst {
    def loc: SourceLocation
  }

  object Term {

    sealed trait Head extends WeededAst.Term

    object Head {

      case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Term.Head

      case class Lit(lit: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Term.Head

      case class Tag(enumName: Name.QName, tagName: Name.Ident, t: WeededAst.Term.Head, loc: SourceLocation) extends WeededAst.Term.Head

      case class Tuple(elms: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Term.Head

      case class Apply(name: Name.QName, args: List[WeededAst.Term.Head], loc: SourceLocation) extends WeededAst.Term.Head

    }

    sealed trait Body extends WeededAst.Term

    object Body {

      case class Wild(loc: SourceLocation) extends WeededAst.Term.Body

      case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Term.Body

      case class Lit(lit: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Term.Body

    }

  }

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type)


}
