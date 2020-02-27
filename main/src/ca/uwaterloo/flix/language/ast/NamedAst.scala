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
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import scala.collection.immutable.List

object NamedAst {

  case class Root(defs: Map[Name.NName, Map[String, NamedAst.Def]],
                  enums: Map[Name.NName, Map[String, NamedAst.Enum]],
                  typealiases: Map[Name.NName, Map[String, NamedAst.TypeAlias]],
                  relations: Map[Name.NName, Map[String, NamedAst.Relation]],
                  lattices: Map[Name.NName, Map[String, NamedAst.Lattice]],
                  latticeComponents: Map[NamedAst.Type, NamedAst.LatticeComponents],
                  named: Map[Symbol.DefnSym, NamedAst.Expression],
                  properties: Map[Name.NName, List[NamedAst.Property]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, tparams: List[NamedAst.TypeParam], fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression, sc: NamedAst.Scheme, eff: NamedAst.Type, loc: SourceLocation)

  // TODO
  case class Law()

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[NamedAst.TypeParam], cases: Map[String, NamedAst.Case], tpe: NamedAst.Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[NamedAst.TypeParam], tpe: NamedAst.Type, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: NamedAst.Expression, loc: SourceLocation) extends Ast.Annotation

  case class Relation(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.RelSym, tparams: List[NamedAst.TypeParam], attr: List[NamedAst.Attribute], loc: SourceLocation)

  case class Lattice(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.LatSym, tparams: List[NamedAst.TypeParam], attr: List[NamedAst.Attribute], loc: SourceLocation)

  case class LatticeComponents(tpe: NamedAst.Type, bot: NamedAst.Expression, top: NamedAst.Expression, equ: NamedAst.Expression, leq: NamedAst.Expression, lub: NamedAst.Expression, glb: NamedAst.Expression, ns: Name.NName, loc: SourceLocation)

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Expression

    case class Def(name: Name.QName, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Hole(name: Option[Name.Ident], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

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

    case class Apply(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Lambda(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Unary(op: UnaryOperator, exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Binary(op: BinaryOperator, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class IfThenElse(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Stm(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[NamedAst.MatchRule], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tag(enum: Option[Name.QName], tag: Name.Ident, expOpt: Option[NamedAst.Expression], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class RecordEmpty(tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class RecordSelect(exp: NamedAst.Expression, label: Name.Ident, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class RecordExtend(label: Name.Ident, value: NamedAst.Expression, rest: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class RecordRestrict(label: Name.Ident, rest: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLit(elms: List[NamedAst.Expression], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayNew(elm: NamedAst.Expression, len: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLoad(base: NamedAst.Expression, index: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayStore(base: NamedAst.Expression, index: NamedAst.Expression, elm: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLength(base: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ArraySlice(base: NamedAst.Expression, beginIndex: NamedAst.Expression, endIndex: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorLit(elms: List[NamedAst.Expression], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorNew(elm: NamedAst.Expression, len: Int, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorLoad(base: NamedAst.Expression, index: Int, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorStore(base: NamedAst.Expression, index: Int, elm: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorLength(base: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class VectorSlice(base: NamedAst.Expression, startIndex: Int, optEndIndex: Option[Int], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Deref(exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Assign(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Existential(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Universal(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, expectedType: Option[NamedAst.Type], expectedEff: Option[NamedAst.Type], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class Cast(exp: NamedAst.Expression, declaredType: Option[NamedAst.Type], declaredEff: Option[NamedAst.Type], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class TryCatch(exp: NamedAst.Expression, rules: List[NamedAst.CatchRule], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeConstructor(className: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeMethod(className: String, methodName: String, exp: NamedAst.Expression, args: List[NamedAst.Expression], sig: List[NamedAst.Type], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeStaticMethod(className: String, methodName: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class GetField(className: String, fieldName: String, exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class PutField(className: String, fieldName: String, exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class GetStaticField(className: String, fieldName: String, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class PutStaticField(className: String, fieldName: String, exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class NewChannel(exp: NamedAst.Expression, tpe: NamedAst.Type, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class GetChannel(exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class PutChannel(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class SelectChannel(rules: List[NamedAst.SelectChannelRule], default: Option[NamedAst.Expression], tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ProcessSpawn(exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class ProcessPanic(msg: String, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointConstraintSet(cs: List[NamedAst.Constraint], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointCompose(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointSolve(exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointProject(qname: Name.QName, exp: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointEntails(exp1: NamedAst.Expression, exp2: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointFold(qname: Name.QName, exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, tvar: ast.Type.Var, evar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Expression

  }

  sealed trait Pattern {
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

    case class Array(elms: scala.List[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class ArrayTailSpread(elms: scala.List[NamedAst.Pattern], sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Pattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class Atom(name: Name.QName, den: Denotation, terms: List[NamedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Predicate.Head

      case class Union(exp: NamedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Atom(name: Name.QName, den: Denotation, polarity: Ast.Polarity, terms: List[NamedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Guard(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  sealed trait Type

  object Type {

    case class Var(tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.Type

    case class Ambiguous(name: Name.QName, loc: SourceLocation) extends NamedAst.Type

    case class Unit(loc: SourceLocation) extends NamedAst.Type

    case class Enum(name: Symbol.EnumSym) extends NamedAst.Type

    case class Tuple(elms: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class RecordEmpty(loc: SourceLocation) extends NamedAst.Type

    case class RecordExtend(label: Name.Ident, field: NamedAst.Type, rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class SchemaEmpty(loc: SourceLocation) extends NamedAst.Type

    case class Schema(ts: List[NamedAst.Type], rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Nat(len: Int, loc: SourceLocation) extends NamedAst.Type

    case class Native(fqn: String, loc: SourceLocation) extends NamedAst.Type

    case class Relation(sym: Symbol.RelSym, loc: SourceLocation) extends NamedAst.Type

    case class Lattice(sym: Symbol.LatSym, loc: SourceLocation) extends NamedAst.Type

    case class Arrow(tparams: List[NamedAst.Type], eff: NamedAst.Type, tresult: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Apply(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Pure(loc: SourceLocation) extends NamedAst.Type

    case class Impure(loc: SourceLocation) extends NamedAst.Type

    case class Not(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class And(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Or(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

  }

  case class Scheme(quantifiers: List[ast.Type.Var], base: NamedAst.Type)

  case class Attribute(ident: Name.Ident, tpe: NamedAst.Type, loc: SourceLocation)

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: NamedAst.Type)

  case class Constraint(cparams: List[NamedAst.ConstraintParam], head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: ast.Type.Var, loc: SourceLocation) extends NamedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: NamedAst.Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: NamedAst.Expression)

  case class MatchRule(pat: NamedAst.Pattern, guard: NamedAst.Expression, exp: NamedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: NamedAst.Expression, exp: NamedAst.Expression)

  case class TypeParam(name: Name.Ident, tpe: ast.Type.Var, loc: SourceLocation)

}
