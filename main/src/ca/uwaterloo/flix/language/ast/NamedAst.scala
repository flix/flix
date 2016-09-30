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

import ca.uwaterloo.flix.language.ast

import scala.collection.immutable.List

trait NamedAst

object NamedAst {

  case class Program(enums: Map[Name.NName, Map[String, NamedAst.Declaration.Enum]],
                     definitions: Map[Name.NName, Map[String, NamedAst.Declaration.Definition]],
                     classes: Map[Symbol.ClassSym, NamedAst.Declaration.Class],
                     impls: Map[Symbol.ImplSym, NamedAst.Declaration.Impl],
                     lattices: Map[NamedAst.Type, NamedAst.Declaration.BoundedLattice],
                     indexes: Map[Name.NName, Map[String, NamedAst.Declaration.Index]],
                     tables: Map[Name.NName, Map[String, NamedAst.Table]],
                     facts: Map[Name.NName, List[NamedAst.Declaration.Fact]],
                     rules: Map[Name.NName, List[NamedAst.Declaration.Rule]],
                     hooks: Map[Name.NName, Map[String, Ast.Hook]],
                     time: Time) extends NamedAst

  sealed trait Declaration extends NamedAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Definition(sym: Symbol.DefnSym, tparams: List[ast.Type.Var], params: List[NamedAst.FormalParam], exp: NamedAst.Expression, ann: Ast.Annotations, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Signature(ident: Name.Ident, params: List[NamedAst.FormalParam], tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class External(ident: Name.Ident, params: List[NamedAst.FormalParam], tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Law(ident: Name.Ident, tparams: List[ParsedAst.ContextBound], params: List[NamedAst.FormalParam], tpe: NamedAst.Type, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Declaration

    case class Enum(sym: Symbol.EnumSym, cases: Map[String, NamedAst.Case], tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Class(ident: Name.Ident, tparams: List[NamedAst.Type], /* bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Impl(ident: Name.Ident, tparams: List[NamedAst.Type], /*bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Fact(head: NamedAst.Predicate.Head, loc: SourceLocation) extends NamedAst.Declaration

    case class Rule(head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation) extends NamedAst.Declaration

    case class Index(qname: Name.QName, indexes: List[List[Name.Ident]], loc: SourceLocation) extends NamedAst.Declaration

    case class BoundedLattice(tpe: NamedAst.Type, bot: NamedAst.Expression, top: NamedAst.Expression, leq: NamedAst.Expression, lub: NamedAst.Expression, glb: NamedAst.Expression, ns: Name.NName, loc: SourceLocation) extends NamedAst.Declaration

  }

  sealed trait Table extends NamedAst.Declaration {
    def sym: Symbol.TableSym

    def attr: List[NamedAst.Attribute]

    def loc: SourceLocation
  }

  object Table {

    case class Relation(sym: Symbol.TableSym, attr: List[NamedAst.Attribute], loc: SourceLocation) extends NamedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[NamedAst.Attribute], value: NamedAst.Attribute, loc: SourceLocation) extends NamedAst.Table {
      def attr: List[NamedAst.Attribute] = keys ::: value :: Nil
    }

  }

  sealed trait Expression extends NamedAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(ref: Name.QName, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Unit(loc: SourceLocation) extends NamedAst.Expression

    case class True(loc: SourceLocation) extends NamedAst.Expression

    case class False(loc: SourceLocation) extends NamedAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends NamedAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends NamedAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends NamedAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends NamedAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends NamedAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends NamedAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends NamedAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends NamedAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends NamedAst.Expression

    case class Apply(lambda: NamedAst.Expression, args: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Lambda(params: List[Symbol.VarSym], exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Unary(op: UnaryOperator, exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Binary(op: BinaryOperator, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class IfThenElse(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[(NamedAst.Pattern, NamedAst.Expression)], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Switch(rules: List[(NamedAst.Expression, NamedAst.Expression)], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tag(enum: Name.QName, tag: Name.Ident, exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FNone(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FSome(exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FNil(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FList(hd: NamedAst.Expression, tl: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FVec(elms: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FSet(elms: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FMap(elms: List[(NamedAst.Expression, NamedAst.Expression)], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class GetIndex(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class PutIndex(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Existential(params: List[NamedAst.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Universal(params: List[NamedAst.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class UserError(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

  }

  sealed trait Pattern extends NamedAst {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Unit(loc: SourceLocation) extends NamedAst.Pattern

    case class True(loc: SourceLocation) extends NamedAst.Pattern

    case class False(loc: SourceLocation) extends NamedAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends NamedAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends NamedAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends NamedAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends NamedAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends NamedAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends NamedAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends NamedAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends NamedAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends NamedAst.Pattern

    case class Tag(enum: Name.QName, tag: Name.Ident, pat: NamedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: scala.List[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FNone(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FSome(pat: NamedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FNil(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FList(hd: NamedAst.Pattern, tl: NamedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FVec(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FSet(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FMap(elms: List[(NamedAst.Pattern, NamedAst.Pattern)], rest: Option[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

  }

  sealed trait Predicate extends NamedAst

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class False(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class Table(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Table(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Filter(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class NotEqual(sym1: Symbol.VarSym, sym2: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Loop(sym: Symbol.VarSym, term: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  sealed trait Type extends NamedAst

  object Type {

    case class Var(tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Type

    case class Unit(loc: SourceLocation) extends NamedAst.Type

    case class Ref(name: Name.QName, loc: SourceLocation) extends NamedAst.Type

    case class Enum(name: Symbol.EnumSym, cases: Map[String, NamedAst.Type]) extends NamedAst.Type

    case class Tuple(elms: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class Arrow(params: List[NamedAst.Type], ret: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Apply(base: NamedAst.Type, tparams: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class Forall(quantifiers: List[ast.Type.Var], base: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: NamedAst.Type) extends NamedAst

  case class FormalParam(sym: Symbol.VarSym, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst

}
