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

import scala.collection.immutable.List

trait NamedAst

object NamedAst {

  case class Program(enums: Map[Name.NName, Map[String, NamedAst.Declaration.Enum]],
                     definitions: Map[Name.NName, Map[String, NamedAst.Declaration.Definition]],
                     classes: Map[Symbol.ClassSym, NamedAst.Declaration.Class],
                     impls: Map[Symbol.ImplSym, NamedAst.Declaration.Impl],
                     lattices: Map[Type, NamedAst.Declaration.BoundedLattice],
                     indexes: Map[Symbol.TableSym, NamedAst.Declaration.Index],
                     tables: Map[Name.NName, Map[String, NamedAst.Table]],
                     facts: List[NamedAst.Declaration.Fact],
                     rules: List[NamedAst.Declaration.Rule],
                     hooks: Map[Symbol.Resolved, Ast.Hook],
                     time: Time) extends NamedAst

  sealed trait Declaration extends NamedAst {
    def loc: SourceLocation
  }

  object Declaration {

    // TODO: Remove ident, since we have sym?
    case class Definition(sym: Symbol.DefnSym, params: List[NamedAst.FormalParam], exp: NamedAst.Expression, ann: Ast.Annotations, tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Signature(ident: Name.Ident, params: List[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class External(ident: Name.Ident, params: List[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Law(ident: Name.Ident, tparams: List[ParsedAst.ContextBound], params: List[Ast.FormalParam], tpe: Type, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Declaration

    case class Enum(sym: Symbol.EnumSym, cases: Map[String, NamedAst.Case], tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Class(ident: Name.Ident, tparams: List[Type], /* bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Impl(ident: Name.Ident, tparams: List[Type], /*bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Fact(head: NamedAst.Predicate.Head, loc: SourceLocation) extends NamedAst.Declaration

    case class Rule(head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation) extends NamedAst.Declaration

    case class Index(ident: Name.Ident, indexes: List[List[Name.Ident]], loc: SourceLocation) extends NamedAst.Declaration

    @deprecated("Will be replaced by type classes", "0.1.0")
    case class BoundedLattice(tpe: Type, bot: NamedAst.Expression, top: NamedAst.Expression, leq: NamedAst.Expression, lub: NamedAst.Expression, glb: NamedAst.Expression, ns: Name.NName, loc: SourceLocation) extends NamedAst.Declaration

  }

  sealed trait Table extends NamedAst.Declaration {
    def sym: Symbol.TableSym

    def loc: SourceLocation
  }

  object Table {

    case class Relation(sym: Symbol.TableSym, attr: List[Ast.Attribute], loc: SourceLocation) extends NamedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[Ast.Attribute], value: Ast.Attribute, loc: SourceLocation) extends NamedAst.Table

  }

  sealed trait Expression extends NamedAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(ref: Name.QName, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

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

    case class Apply(lambda: NamedAst.Expression, args: List[NamedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Lambda(params: List[Symbol.VarSym], exp: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Unary(op: UnaryOperator, exp: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Binary(op: BinaryOperator, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class IfThenElse(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[(NamedAst.Pattern, NamedAst.Expression)], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Switch(rules: List[(NamedAst.Expression, NamedAst.Expression)], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tag(enum: Name.QName, tag: Name.Ident, exp: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FNone(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FSome(exp: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FNil(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FList(hd: NamedAst.Expression, tl: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FVec(elms: List[NamedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FSet(elms: List[NamedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FMap(elms: List[(NamedAst.Expression, NamedAst.Expression)], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class GetIndex(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class PutIndex(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Existential(params: List[NamedAst.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Universal(params: List[NamedAst.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, tpe: Type, loc: SourceLocation) extends NamedAst.Expression

    case class UserError(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Expression

  }

  sealed trait Pattern extends NamedAst {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

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

    case class Tag(enum: Name.QName, tag: Name.Ident, pat: NamedAst.Pattern, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: scala.List[NamedAst.Pattern], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FNone(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FSome(pat: NamedAst.Pattern, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FNil(tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FList(hd: NamedAst.Pattern, tl: NamedAst.Pattern, tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FVec(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FSet(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class FMap(elms: List[(NamedAst.Pattern, NamedAst.Pattern)], rest: Option[NamedAst.Pattern], tvar: Type.Var, loc: SourceLocation) extends NamedAst.Pattern

  }

  // TODO: Cleanup this stuff:

  sealed trait Predicate extends NamedAst

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class False(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class Table(name: Name.QName, terms: List[WeededAst.Term.Head], loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Ambiguous(name: Name.QName, terms: List[WeededAst.Term.Body], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: WeededAst.Term.Head, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type)

  case class FormalParam(sym: Symbol.VarSym, tpe: Type)

}
