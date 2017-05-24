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

import scala.collection.immutable.List

trait NamedAst

object NamedAst {

  case class Program(definitions: Map[Name.NName, Map[String, NamedAst.Declaration.Definition]],
                     enums: Map[Name.NName, Map[String, NamedAst.Declaration.Enum]],
                     lattices: Map[NamedAst.Type, NamedAst.Declaration.BoundedLattice],
                     indexes: Map[Name.NName, Map[String, NamedAst.Declaration.Index]],
                     tables: Map[Name.NName, Map[String, NamedAst.Table]],
                     constraints: Map[Name.NName, List[NamedAst.Constraint]],
                     hooks: Map[Symbol.DefnSym, Ast.Hook],
                     properties: Map[Name.NName, List[NamedAst.Property]],
                     reachable: Set[Symbol.DefnSym],
                     time: Time) extends NamedAst

  case class Constraint(cparams: List[NamedAst.ConstraintParam], head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation) extends NamedAst

  sealed trait Declaration extends NamedAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Definition(doc: Option[Ast.Documentation], ann: Ast.Annotations, sym: Symbol.DefnSym, tparams: List[NamedAst.TypeParam], fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression, sc: NamedAst.Scheme, loc: SourceLocation) extends NamedAst.Declaration

    case class Enum(doc: Option[Ast.Documentation], sym: Symbol.EnumSym, tparams: List[NamedAst.TypeParam], cases: Map[String, NamedAst.Case], tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Index(qname: Name.QName, indexes: List[List[Name.Ident]], loc: SourceLocation) extends NamedAst.Declaration

    case class BoundedLattice(tpe: NamedAst.Type, bot: NamedAst.Expression, top: NamedAst.Expression, leq: NamedAst.Expression, lub: NamedAst.Expression, glb: NamedAst.Expression, ns: Name.NName, loc: SourceLocation) extends NamedAst.Declaration

  }

  sealed trait Table extends NamedAst.Declaration {
    def sym: Symbol.TableSym
    
    def loc: SourceLocation
  }

  object Table {

    case class Relation(doc: Option[Ast.Documentation], sym: Symbol.TableSym, attr: List[NamedAst.Attribute], loc: SourceLocation) extends NamedAst.Table

    case class Lattice(doc: Option[Ast.Documentation], sym: Symbol.TableSym, keys: List[NamedAst.Attribute], value: NamedAst.Attribute, loc: SourceLocation) extends NamedAst.Table

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

    case class LetRec(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[NamedAst.MatchRule], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Switch(rules: List[(NamedAst.Expression, NamedAst.Expression)], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Existential(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Universal(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class NativeConstructor(constructor: Constructor[_], args: List[NamedAst.Expression], tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class NativeField(field: Field, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class NativeMethod(method: Method, args: List[NamedAst.Expression], tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

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

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, pat: NamedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: scala.List[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

  }

  sealed trait Predicate extends NamedAst

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class False(loc: SourceLocation) extends NamedAst.Predicate.Head

      case class Positive(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Head

      case class Negative(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Positive(name: Name.QName, terms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Negative(name: Name.QName, terms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Filter(name: Name.QName, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Loop(pat: NamedAst.Pattern, term: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  sealed trait Type extends NamedAst

  object Type {

    case class Var(tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Type

    case class Unit(loc: SourceLocation) extends NamedAst.Type

    case class Ref(name: Name.QName, loc: SourceLocation) extends NamedAst.Type

    case class Enum(name: Symbol.EnumSym) extends NamedAst.Type

    case class Tuple(elms: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class Arrow(params: List[NamedAst.Type], ret: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Apply(base: NamedAst.Type, tparams: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

  }

  case class Scheme(quantifiers: List[ast.Type.Var], base: NamedAst.Type) extends NamedAst

  case class Attribute(ident: Name.Ident, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: NamedAst.Type) extends NamedAst

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst

  case class MatchRule(pat: NamedAst.Pattern, guard: NamedAst.Expression, exp: NamedAst.Expression) extends NamedAst

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: NamedAst.Expression, loc: SourceLocation) extends Ast.Annotation

  case class TypeParam(name: Name.Ident, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst

}
