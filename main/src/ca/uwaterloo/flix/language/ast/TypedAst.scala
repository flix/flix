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

  case class Root(constants: Map[Symbol.Resolved, TypedAst.Definition.Constant],
                  lattices: Map[Type, TypedAst.Definition.BoundedLattice],
                  tables: Map[Symbol.TableSym, TypedAst.Table],
                  indexes: Map[Symbol.TableSym, TypedAst.Definition.Index],
                  facts: List[TypedAst.Constraint.Fact],
                  rules: List[TypedAst.Constraint.Rule],
                  hooks: Map[Symbol.Resolved, Ast.Hook],
                  properties: List[TypedAst.Property],
                  time: Time) extends TypedAst

  sealed trait Definition

  object Definition {

    case class Constant(ann: Ast.Annotations, name: Symbol.Resolved, formals: List[TypedAst.FormalArg], exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Definition

    case class BoundedLattice(tpe: Type,
                              bot: TypedAst.Expression,
                              top: TypedAst.Expression,
                              leq: TypedAst.Expression,
                              lub: TypedAst.Expression,
                              glb: TypedAst.Expression,
                              loc: SourceLocation) extends TypedAst.Definition

    case class Index(sym: Symbol.TableSym, indexes: List[List[Name.Ident]], loc: SourceLocation) extends TypedAst.Definition

  }

  sealed trait Table

  object Table {

    case class Relation(sym: Symbol.TableSym, attributes: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[TypedAst.Attribute], value: TypedAst.Attribute, loc: SourceLocation) extends TypedAst.Table

  }

  sealed trait Constraint extends TypedAst

  object Constraint {

    case class Fact(head: TypedAst.Predicate.Head) extends TypedAst.Constraint

    case class Rule(head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body]) extends TypedAst.Constraint

  }

  sealed trait Literal extends TypedAst {
    def tpe: Type
    def loc: SourceLocation
  }

  object Literal {

    case class Unit(loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Unit
    }

    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Str
    }

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

    case class Lit(literal: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Ref(name: Symbol.Resolved, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Hook(hook: Ast.Hook, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Lambda(args: List[TypedAst.FormalArg], body: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Let(ident: Name.Ident, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Match(exp: TypedAst.Expression, rules: List[(TypedAst.Pattern, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tag(name: Symbol.Resolved, ident: Name.Ident, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

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


    case class Existential(params: List[Ast.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[Ast.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Error(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

  }

  sealed trait Pattern extends TypedAst {
    def tpe: Type

    def loc: SourceLocation

    def freeVars: Map[String, Type] = {
      def visit(pat: TypedAst.Pattern, m: Map[String, Type]): Map[String, Type] =
        pat match {
          case TypedAst.Pattern.Wildcard(_, _) => m
          case TypedAst.Pattern.Var(ident, tpe, _) => m + (ident.name -> tpe)
          case TypedAst.Pattern.Lit(_, _, _) => m
          case TypedAst.Pattern.Tag(_, _, pat2, _, _) => visit(pat2, m)
          case TypedAst.Pattern.Tuple(elms, _, _) => elms.foldLeft(m) {
            case (macc, elm) => visit(elm, macc)
          }
        }

      visit(this, Map.empty)
    }
  }

  object Pattern {

    case class Wildcard(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Lit(lit: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tag(name: Symbol.Resolved, ident: Name.Ident, pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

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

      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Term.Head], loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    sealed trait Body extends TypedAst.Predicate

    object Body {

      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Term.Body], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class ApplyFilter(name: Symbol.Resolved, terms: List[TypedAst.Term.Body], loc: SourceLocation) extends TypedAst.Predicate.Body

      case class ApplyHookFilter(hook: Ast.Hook, terms: List[TypedAst.Term.Body],loc: SourceLocation) extends TypedAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: TypedAst.Term.Head, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends TypedAst {
      def tpe: Type
      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class Lit(literal: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class Tag(enumName: Symbol.Resolved, tagName: Name.Ident, t: TypedAst.Term.Head, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class Tuple(elms: List[TypedAst.Term.Head], tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class Apply(name: Symbol.Resolved, args: List[TypedAst.Term.Head], tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class ApplyHook(hook: Ast.Hook, args: List[TypedAst.Term.Head], tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

    }

    sealed trait Body extends TypedAst {
      def tpe: Type
      def loc: SourceLocation
    }

    object Body {
      case class Wildcard(tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body
      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body
      case class Lit(lit: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body
    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends TypedAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends TypedAst

  case class Property(law: Law, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst

}
