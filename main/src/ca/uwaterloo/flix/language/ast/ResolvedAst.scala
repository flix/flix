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

  case class Program(defs: Map[Symbol.DefnSym, ResolvedAst.Def],
                     effs: Map[Symbol.EffSym, ResolvedAst.Eff],
                     handlers: Map[Symbol.EffSym, ResolvedAst.Handler],
                     enums: Map[Symbol.EnumSym, ResolvedAst.Enum],
                     classes: Map[Symbol.ClassSym, ResolvedAst.Class],
                     impls: Map[Symbol.ClassSym, ResolvedAst.Impl],
                     lattices: Map[Type, ResolvedAst.Lattice],
                     indexes: Map[Symbol.TableSym, ResolvedAst.Index],
                     tables: Map[Symbol.TableSym, ResolvedAst.Table],
                     constraints: List[ResolvedAst.Constraint],
                     properties: List[ResolvedAst.Property],
                     reachable: Set[Symbol.DefnSym],
                     time: Time) extends ResolvedAst

  case class Constraint(cparams: List[ResolvedAst.ConstraintParam], head: ResolvedAst.Predicate.Head, body: List[ResolvedAst.Predicate.Body], loc: SourceLocation) extends ResolvedAst

  case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, sc: Scheme, eff: ast.Eff, loc: SourceLocation) extends ResolvedAst

  case class Eff(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], sc: Scheme, eff: ast.Eff, loc: SourceLocation) extends ResolvedAst

  case class Handler(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffSym, tparams: List[ResolvedAst.TypeParam], fparams: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, sc: Scheme, eff: ast.Eff, loc: SourceLocation) extends ResolvedAst

  // TODO
  case class Law() extends ResolvedAst

  // TODO
  case class Sig() extends ResolvedAst

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[ResolvedAst.TypeParam], cases: Map[String, ResolvedAst.Case], tpe: Type, loc: SourceLocation) extends ResolvedAst

  case class Index(sym: Symbol.TableSym, indexes: List[List[Name.Ident]], loc: SourceLocation) extends ResolvedAst

  case class Lattice(tpe: Type, bot: ResolvedAst.Expression, top: ResolvedAst.Expression, equ: ResolvedAst.Expression, leq: ResolvedAst.Expression, lub: ResolvedAst.Expression, glb: ResolvedAst.Expression, ns: Name.NName, loc: SourceLocation) extends ResolvedAst

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ResolvedAst.Expression, loc: SourceLocation)

  case class Class(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, quantifiers: List[Type.Var], head: ResolvedAst.SimpleClass, body: List[ResolvedAst.SimpleClass], sigs: Map[String, ResolvedAst.Sig], laws: List[ResolvedAst.Law], loc: SourceLocation) extends ResolvedAst

  case class Impl(doc: Ast.Doc, mod: Ast.Modifiers, head: ResolvedAst.ComplexClass, body: List[ResolvedAst.ComplexClass], defs: List[ResolvedAst.Def], loc: SourceLocation) extends NamedAst

  // TODO: Disallow.

  sealed trait Table extends ResolvedAst {
    def sym: Symbol.TableSym

    def attr: List[ResolvedAst.Attribute]

    def loc: SourceLocation
  }

  object Table {

    case class Relation(doc: Ast.Doc, sym: Symbol.TableSym, attr: List[ResolvedAst.Attribute], loc: SourceLocation) extends ResolvedAst.Table

    case class Lattice(doc: Ast.Doc, sym: Symbol.TableSym, keys: List[ResolvedAst.Attribute], value: ResolvedAst.Attribute, loc: SourceLocation) extends ResolvedAst.Table {
      def attr: List[ResolvedAst.Attribute] = keys ::: value :: Nil
    }

  }

  sealed trait Expression extends ResolvedAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends ResolvedAst.Expression

    case class Def(sym: Symbol.DefnSym, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Eff(sym: Symbol.EffSym, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

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

    case class Apply(lambda: ResolvedAst.Expression, args: List[ResolvedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lambda(params: List[ResolvedAst.FormalParam], exp: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, exp: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Binary(op: BinaryOperator, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class IfThenElse(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, exp3: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Match(exp: ResolvedAst.Expression, rules: List[ResolvedAst.MatchRule], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Switch(rules: List[(ResolvedAst.Expression, ResolvedAst.Expression)], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLit(elms: List[ResolvedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayNew(elm: ResolvedAst.Expression, len: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLoad(base: ResolvedAst.Expression, index: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends  ResolvedAst.Expression

    case class ArrayStore(base: ResolvedAst.Expression, index: ResolvedAst.Expression, elm: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends  ResolvedAst.Expression

    case class ArrayLength(base: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArraySlice(base: ResolvedAst.Expression, beginIndex: ResolvedAst.Expression, endIndex: ResolvedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends  ResolvedAst.Expression

    case class VectorLit(elms: List[ResolvedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends  ResolvedAst.Expression

    case class VectorNew(elm: ResolvedAst.Expression, len: Int, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorLoad(base: ResolvedAst.Expression, index: Int, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorStore(base: ResolvedAst.Expression, index: Int, elm: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorLength(base: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class VectorSlice(base: ResolvedAst.Expression, startIndex: Int, optEndIndex: Option[Int], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ref(exp: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Deref(exp: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Assign(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class HandleWith(exp: ResolvedAst.Expression, bindings: List[ResolvedAst.HandlerBinding], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Existential(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Universal(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ascribe(exp: ResolvedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends ResolvedAst.Expression

    case class Cast(exp: ResolvedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeConstructor(method: Constructor[_], args: List[ResolvedAst.Expression], tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeField(field: Field, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class NativeMethod(method: Method, args: List[ResolvedAst.Expression], tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class UserError(tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern extends ResolvedAst {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

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

    case class Tag(sym: Symbol.EnumSym, tag: String, pat: ResolvedAst.Pattern, tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Tuple(elms: scala.List[ResolvedAst.Pattern], tvar: Type.Var, loc: SourceLocation) extends ResolvedAst.Pattern

  }

  sealed trait Predicate extends ResolvedAst

  object Predicate {

    sealed trait Head extends ResolvedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class False(loc: SourceLocation) extends ResolvedAst.Predicate.Head

      case class Atom(sym: Symbol.TableSym, terms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Predicate.Head

    }

    sealed trait Body extends ResolvedAst.Predicate

    object Body {

      case class Atom(sym: Symbol.TableSym, polarity: Ast.Polarity, terms: List[ResolvedAst.Pattern], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Filter(sym: Symbol.DefnSym, terms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Loop(pat: ResolvedAst.Pattern, term: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Predicate.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ResolvedAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type) extends ResolvedAst

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

  }


  case class SimpleClass(sym: Symbol.ClassSym, args: List[Type.Var], loc: SourceLocation) extends NamedAst

  case class ComplexClass(sym: Symbol.ClassSym, polarity: Ast.Polarity, args: List[Type], loc: SourceLocation) extends NamedAst

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation) extends ResolvedAst

  case class HandlerBinding(sym: Symbol.EffSym, exp: ResolvedAst.Expression) extends ResolvedAst

  case class MatchRule(pat: ResolvedAst.Pattern, guard: ResolvedAst.Expression, exp: ResolvedAst.Expression) extends ResolvedAst

  case class TypeParam(name: Name.Ident, tpe: Type.Var, loc: SourceLocation) extends ResolvedAst

}
