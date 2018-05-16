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

trait WeededAst

object WeededAst {

  case class Program(roots: List[WeededAst.Root], named: Map[Symbol.DefnSym, WeededAst.Expression], reachable: Set[Symbol.DefnSym], time: Time) extends WeededAst

  case class Root(decls: List[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration extends WeededAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Namespace(name: Name.NName, decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: ast.Eff, loc: SourceLocation) extends WeededAst.Declaration

    case class Law(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: ast.Eff, loc: SourceLocation) extends WeededAst.Declaration

    case class Eff(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, eff: ast.Eff, loc: SourceLocation) extends WeededAst.Declaration

    case class Handler(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: ast.Eff, loc: SourceLocation) extends WeededAst.Declaration

    case class Sig(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, eff: ast.Eff, loc: SourceLocation) extends WeededAst.Declaration

    case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, tparams: List[Name.Ident], cases: Map[String, WeededAst.Case], loc: SourceLocation) extends WeededAst.Declaration

    case class Property(law: Name.QName, defn: Name.Ident, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

    case class Constraint(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation) extends WeededAst.Declaration

    case class Index(qname: Name.QName, indexes: List[List[Name.Ident]], loc: SourceLocation) extends WeededAst.Declaration

    case class Lattice(tpe: WeededAst.Type, bot: WeededAst.Expression, top: WeededAst.Expression, equ: WeededAst.Expression, leq: WeededAst.Expression, lub: WeededAst.Expression, glb: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Declaration

    case class Class(doc: Ast.Doc, mod: Ast.Modifiers, head: WeededAst.SimpleClass, body: List[WeededAst.SimpleClass], sigs: List[WeededAst.Declaration.Sig], laws: List[WeededAst.Declaration.Law], loc: SourceLocation) extends WeededAst.Declaration

    case class Impl(doc: Ast.Doc, mod: Ast.Modifiers, head: WeededAst.ComplexClass, body: List[WeededAst.ComplexClass], defs: List[WeededAst.Declaration.Def], loc: SourceLocation) extends WeededAst.Declaration

    case class Disallow(doc: Ast.Doc, body: List[WeededAst.ComplexClass], loc: SourceLocation) extends WeededAst.Declaration

  }

  sealed trait Table extends WeededAst.Declaration {
    def ident: Name.Ident

    def loc: SourceLocation
  }

  object Table {

    case class Relation(doc: Ast.Doc, ident: Name.Ident, attr: List[WeededAst.Attribute], loc: SourceLocation) extends WeededAst.Table

    case class Lattice(doc: Ast.Doc, ident: Name.Ident, keys: List[WeededAst.Attribute], value: WeededAst.Attribute, loc: SourceLocation) extends WeededAst.Table

  }

  sealed trait Expression extends WeededAst {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends WeededAst.Expression

    case class VarOrDef(name: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class Hole(name: Name.Ident, loc: SourceLocation) extends WeededAst.Expression

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

    case class Lambda(fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Unary(op: UnaryOperator, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Binary(op: BinaryOperator, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class IfThenElse(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Let(ident: Name.Ident, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class LetRec(ident: Name.Ident, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Match(exp: WeededAst.Expression, rules: List[WeededAst.MatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class Switch(rules: List[(WeededAst.Expression, WeededAst.Expression)], loc: SourceLocation) extends WeededAst.Expression

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, expOpt: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLit(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class ArrayNew(elm: WeededAst.Expression, len: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLoad(base: WeededAst.Expression, index: WeededAst.Expression, loc:SourceLocation) extends  WeededAst.Expression

    case class ArrayLength(base: WeededAst.Expression, loc: SourceLocation) extends  WeededAst.Expression

    case class ArrayStore(base: WeededAst.Expression, index: WeededAst.Expression, elm: WeededAst.Expression, loc: SourceLocation) extends  WeededAst.Expression

    case class ArraySlice(base: WeededAst.Expression, beginIndex: WeededAst.Expression, endIndex: WeededAst.Expression, loc: SourceLocation) extends  WeededAst.Expression

    case class VectorLit(elms: List[WeededAst.Expression], loc: SourceLocation) extends  WeededAst.Expression

    case class VectorNew(elm: WeededAst.Expression, len: Int, loc: SourceLocation) extends WeededAst.Expression

    case class VectorLoad(base: WeededAst.Expression, index: Int, loc: SourceLocation) extends WeededAst.Expression

    case class VectorStore(base: WeededAst.Expression, index: Int, elm: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class VectorLength(base: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class VectorSlice(base: WeededAst.Expression, startIndex: Int, optEndIndex: Option[Int], loc: SourceLocation) extends WeededAst.Expression

    case class Unique(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ref(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Deref(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Assign(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class HandleWith(exp: WeededAst.Expression, bindings: List[WeededAst.HandlerBinding], loc: SourceLocation) extends WeededAst.Expression

    case class Existential(fparam: WeededAst.FormalParam, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Universal(fparam: WeededAst.FormalParam, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(exp: WeededAst.Expression, tpe: WeededAst.Type, eff: Eff, loc: SourceLocation) extends WeededAst.Expression

    case class Cast(exp: WeededAst.Expression, tpe: WeededAst.Type, eff: Eff, loc: SourceLocation) extends WeededAst.Expression

    case class NativeConstructor(className: String, args: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class NativeField(className: String, fieldName: String, loc: SourceLocation) extends WeededAst.Expression

    case class NativeMethod(className: String, methodName: String, args: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

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

  }

  sealed trait Predicate extends WeededAst

  object Predicate {

    sealed trait Head extends WeededAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends WeededAst.Predicate.Head

      case class False(loc: SourceLocation) extends WeededAst.Predicate.Head

      case class Atom(name: Name.QName, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Head

    }

    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class Atom(name: Name.QName, polarity: Ast.Polarity, terms: List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Filter(name: Name.QName, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Loop(pat: WeededAst.Pattern, term: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

    }

  }

  sealed trait Type extends WeededAst

  object Type {

    case class Var(qname: Name.Ident, loc: SourceLocation) extends WeededAst.Type

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends WeededAst.Type

    case class Unit(loc: SourceLocation) extends WeededAst.Type

    case class Tuple(elms: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

    case class Nat(len: Int, loc: SourceLocation) extends WeededAst.Type

    case class Native(fqn: List[String], loc: SourceLocation) extends WeededAst.Type

    case class Arrow(tparams: List[WeededAst.Type], retType: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Apply(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: WeededAst.Type) extends WeededAst

  case class SimpleClass(qname: Name.QName, args: List[Name.Ident], loc: SourceLocation) extends WeededAst

  case class ComplexClass(qname: Name.QName, polarity: Ast.Polarity, args: List[WeededAst.Type], loc: SourceLocation) extends WeededAst

  case class FormalParam(ident: Name.Ident, mod: Ast.Modifiers, tpe: Option[WeededAst.Type], loc: SourceLocation) extends WeededAst

  case class HandlerBinding(qname: Name.QName, exp: WeededAst.Expression) extends WeededAst

  case class MatchRule(pat: WeededAst.Pattern, guard: WeededAst.Expression, exp: WeededAst.Expression) extends WeededAst

}
