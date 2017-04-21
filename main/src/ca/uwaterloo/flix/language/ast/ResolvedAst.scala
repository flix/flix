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

import scala.collection.immutable.List

trait ResolvedAst

object ResolvedAst {

  // TODO: These should be direct maps
  case class Program(
                      definitions2: Map[Symbol.DefnSym, ResolvedAst.Declaration.Definition],
                      enums2: Map[Symbol.EnumSym, ResolvedAst.Declaration.Enum],
                      tables2: Map[Symbol.TableSym, ResolvedAst.Table],

                      // TODO: Refactor these:
                      definitions: Map[Name.NName, Map[String, ResolvedAst.Declaration.Definition]],
                      enums: Map[Name.NName, Map[String, ResolvedAst.Declaration.Enum]],
                      lattices: Map[ast.Type, ResolvedAst.Declaration.BoundedLattice],
                      indexes: Map[Name.NName, Map[String, ResolvedAst.Declaration.Index]],
                      tables: Map[Name.NName, Map[String, ResolvedAst.Table]],
                      constraints: Map[Name.NName, List[ResolvedAst.Constraint]],
                      hooks: Map[Symbol.DefnSym, Ast.Hook],
                      properties: Map[Name.NName, List[ResolvedAst.Property]],
                      reachable: Set[Symbol.DefnSym],
                      time: Time) extends ResolvedAst

  case class Constraint(cparams: List[ResolvedAst.ConstraintParam], head: ResolvedAst.Predicate.Head, body: List[ResolvedAst.Predicate.Body], loc: SourceLocation) extends ResolvedAst

  sealed trait Declaration extends ResolvedAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Definition(doc: Option[Ast.Documentation], ann: Ast.Annotations, sym: Symbol.DefnSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, sc: Scheme, loc: SourceLocation) extends ResolvedAst.Declaration

    case class Enum(doc: Option[Ast.Documentation], sym: Symbol.EnumSym, tparams: List[ResolvedAst.TypeParam], cases: Map[String, ResolvedAst.Case], tpe: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Declaration

    case class Index(sym: Symbol.TableSym, indexes: List[List[Name.Ident]], loc: SourceLocation) extends ResolvedAst.Declaration

    // TODO: Rename BoundedLattice.
    case class BoundedLattice(tpe: ast.Type, bot: ResolvedAst.Expression, top: ResolvedAst.Expression, leq: ResolvedAst.Expression, lub: ResolvedAst.Expression, glb: ResolvedAst.Expression, ns: Name.NName, loc: SourceLocation) extends ResolvedAst.Declaration

  }

  sealed trait Table extends ResolvedAst.Declaration {
    def sym: Symbol.TableSym

    def attr: List[ResolvedAst.Attribute]

    def loc: SourceLocation
  }

  object Table {

    case class Relation(doc: Option[Ast.Documentation], sym: Symbol.TableSym, attr: List[ResolvedAst.Attribute], loc: SourceLocation) extends ResolvedAst.Table

    case class Lattice(doc: Option[Ast.Documentation], sym: Symbol.TableSym, keys: List[ResolvedAst.Attribute], value: ResolvedAst.Attribute, loc: SourceLocation) extends ResolvedAst.Table {
      def attr: List[ResolvedAst.Attribute] = keys ::: value :: Nil
    }

  }

  sealed trait Expression extends ResolvedAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ref(sym: Symbol.DefnSym, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    // TODO: Hook
    case class Hook(hook: Ast.Hook, tpe: Type, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unit(loc: SourceLocation) extends ResolvedAst.Expression

    case class True(loc: SourceLocation) extends ResolvedAst.Expression

    case class False(loc: SourceLocation) extends ResolvedAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends ResolvedAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ResolvedAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ResolvedAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ResolvedAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Expression

    case class Apply(lambda: ResolvedAst.Expression, args: List[ResolvedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lambda(params: List[Symbol.VarSym], exp: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, exp: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Binary(op: BinaryOperator, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class IfThenElse(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, exp3: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Match(exp: ResolvedAst.Expression, rules: List[ResolvedAst.MatchRule], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Switch(rules: List[(ResolvedAst.Expression, ResolvedAst.Expression)], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Existential(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Universal(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ascribe(exp: ResolvedAst.Expression, tpe: ast.Type, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeConstructor(method: Constructor[_], args: List[ResolvedAst.Expression], tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeField(field: Field, tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeMethod(method: Method, args: List[ResolvedAst.Expression], tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class UserError(tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern extends ResolvedAst {
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

  }

  sealed trait Predicate extends ResolvedAst

  object Predicate {

    sealed trait Head extends ResolvedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class False(loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class Positive(sym: Symbol.TableSym, terms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class Negative(sym: Symbol.TableSym, terms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Predicate.Head

    }

    sealed trait Body extends ResolvedAst.Predicate

    object Body {

      case class Positive(sym: Symbol.TableSym, terms: List[ResolvedAst.Pattern], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Negative(sym: Symbol.TableSym, terms: List[ResolvedAst.Pattern], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Loop(pat: ResolvedAst.Pattern, term: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Predicate.Body

    }

  }

  // TODO: Remove and replace every occurence of ast.Type with just Type.
  sealed trait Type extends ResolvedAst

  object Type {

    case class Var(tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Type

    case class Unit(loc: SourceLocation) extends ResolvedAst.Type

    case class Ref(name: Name.QName, loc: SourceLocation) extends ResolvedAst.Type

    case class Enum(name: Symbol.EnumSym) extends ResolvedAst.Type

    case class Tuple(elms: List[ResolvedAst.Type], loc: SourceLocation) extends ResolvedAst.Type

    case class Arrow(params: List[ResolvedAst.Type], ret: ResolvedAst.Type, loc: SourceLocation) extends ResolvedAst.Type

    case class Apply(base: ResolvedAst.Type, tparams: List[ResolvedAst.Type], loc: SourceLocation) extends ResolvedAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: ast.Type, loc: SourceLocation) extends ResolvedAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: ResolvedAst.Type) extends ResolvedAst

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: ast.Type, loc: SourceLocation) extends ResolvedAst

  case class MatchRule(pat: ResolvedAst.Pattern, guard: ResolvedAst.Expression, exp: ResolvedAst.Expression) extends ResolvedAst

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ResolvedAst.Expression, loc: SourceLocation) extends Ast.Annotation

  case class TypeParam(name: Name.Ident, tpe: ast.Type.Var, loc: SourceLocation) extends ResolvedAst

}
