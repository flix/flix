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

object NamedAst {

  case class Root(classes: Map[Name.NName, Map[String, NamedAst.Class]],
                  instances: Map[Name.NName, Map[String, List[NamedAst.Instance]]],
                  defsAndSigs: Map[Name.NName, Map[String, NamedAst.DefOrSig]],
                  enums: Map[Name.NName, Map[String, NamedAst.Enum]],
                  typeAliases: Map[Name.NName, Map[String, NamedAst.TypeAlias]],
                  effects: Map[Name.NName, Map[String, NamedAst.Effect]],
                  ops: Map[Name.NName, Map[String, NamedAst.Op]],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  // TODO change laws to NamedAst.Law
  case class Class(doc: Ast.Doc, ann: List[NamedAst.Annotation], mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: NamedAst.TypeParam, superClasses: List[NamedAst.TypeConstraint], sigs: List[NamedAst.Sig], laws: List[NamedAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: List[NamedAst.Annotation], mod: Ast.Modifiers, clazz: Name.QName, tpe: NamedAst.Type, tconstrs: List[NamedAst.TypeConstraint], defs: List[NamedAst.Def], loc: SourceLocation)

  sealed trait DefOrSig

  object DefOrSig {
    case class Def(d: NamedAst.Def) extends NamedAst.DefOrSig

    case class Sig(s: NamedAst.Sig) extends NamedAst.DefOrSig
  }

  case class Sig(sym: Symbol.SigSym, spec: NamedAst.Spec, exp: Option[NamedAst.Expression])

  case class Def(sym: Symbol.DefnSym, spec: NamedAst.Spec, exp: NamedAst.Expression)

  case class Spec(doc: Ast.Doc, ann: List[NamedAst.Annotation], mod: Ast.Modifiers, tparams: NamedAst.TypeParams, fparams: List[NamedAst.FormalParam], retTpe: NamedAst.Type, purAndEff: PurityAndEffect, tconstrs: List[NamedAst.TypeConstraint], loc: SourceLocation)

  case class Enum(doc: Ast.Doc, ann: List[NamedAst.Annotation], mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: NamedAst.TypeParams, derives: List[Name.QName], cases: Map[Name.Tag, NamedAst.Case], tpe: NamedAst.Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: NamedAst.TypeParams, tpe: NamedAst.Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: List[NamedAst.Annotation], mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[NamedAst.Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: NamedAst.Spec)

  sealed trait Use

  object Use {

    case class UseDefOrSig(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends NamedAst.Use

    case class UseTypeOrClass(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends NamedAst.Use

    case class UseTag(qname: Name.QName, tag: Name.Tag, alias: Name.Ident, loc: SourceLocation) extends NamedAst.Use

  }

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends NamedAst.Expression

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Expression

    case class DefOrSig(name: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends NamedAst.Expression

    case class Use(use: NamedAst.Use, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Unit(loc: SourceLocation) extends NamedAst.Expression

    case class Null(loc: SourceLocation) extends NamedAst.Expression

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

    case class Default(loc: SourceLocation) extends NamedAst.Expression

    case class Apply(exp: NamedAst.Expression, exps: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Lambda(fparam: NamedAst.FormalParam, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Unary(sop: SemanticOperator, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Binary(sop: SemanticOperator, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class IfThenElse(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Stm(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Discard(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Region(tpe: ca.uwaterloo.flix.language.ast.Type, loc: SourceLocation) extends NamedAst.Expression

    case class Scope(sym: Symbol.VarSym, regionVar: Symbol.UnkindedTypeVarSym, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[NamedAst.MatchRule], loc: SourceLocation) extends NamedAst.Expression

    case class Choose(star: Boolean, exps: List[NamedAst.Expression], rules: List[NamedAst.ChoiceRule], loc: SourceLocation) extends NamedAst.Expression

    case class Tag(qname: Option[Name.QName], tag: Name.Tag, expOpt: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends NamedAst.Expression

    case class RecordSelect(exp: NamedAst.Expression, field: Name.Field, loc: SourceLocation) extends NamedAst.Expression

    case class RecordExtend(field: Name.Field, value: NamedAst.Expression, rest: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class New(qname: Name.QName, exp: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLit(exps: List[NamedAst.Expression], exp: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class ArrayNew(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLoad(base: NamedAst.Expression, index: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayStore(base: NamedAst.Expression, index: NamedAst.Expression, elm: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLength(base: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArraySlice(base: NamedAst.Expression, beginIndex: NamedAst.Expression, endIndex: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(exp1: NamedAst.Expression, exp2: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Deref(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Assign(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, expectedType: Option[NamedAst.Type], expectedEff: NamedAst.PurityAndEffect, loc: SourceLocation) extends NamedAst.Expression

    case class Cast(exp: NamedAst.Expression, declaredType: Option[NamedAst.Type], declaredEff: NamedAst.PurityAndEffect, loc: SourceLocation) extends NamedAst.Expression

    case class Without(exp: NamedAst.Expression, eff: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class TryCatch(exp: NamedAst.Expression, rules: List[NamedAst.CatchRule], loc: SourceLocation) extends NamedAst.Expression

    case class TryWith(exp: NamedAst.Expression, eff: Name.QName, rules: List[NamedAst.HandlerRule], loc: SourceLocation) extends NamedAst.Expression

    case class Do(op: Name.QName, args: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Resume(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeConstructor(className: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class InvokeMethod(className: String, methodName: String, exp: NamedAst.Expression, args: List[NamedAst.Expression], sig: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class InvokeStaticMethod(className: String, methodName: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class GetField(className: String, fieldName: String, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class PutField(className: String, fieldName: String, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class GetStaticField(className: String, fieldName: String, loc: SourceLocation) extends NamedAst.Expression

    case class PutStaticField(className: String, fieldName: String, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class NewObject(className: String, methods: List[JvmMethod], loc: SourceLocation) extends NamedAst.Expression

    case class NewChannel(exp: NamedAst.Expression, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class GetChannel(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class PutChannel(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class SelectChannel(rules: List[NamedAst.SelectChannelRule], default: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Spawn(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Lazy(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Force(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointConstraintSet(cs: List[NamedAst.Constraint], loc: SourceLocation) extends NamedAst.Expression

    case class FixpointLambda(pparams: List[NamedAst.PredicateParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointMerge(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointSolve(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointInject(exp: NamedAst.Expression, pred: Name.Pred, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Reify(t: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class ReifyType(t: NamedAst.Type, k: Kind, loc: SourceLocation) extends NamedAst.Expression

    case class ReifyEff(sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends NamedAst.Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Pattern

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

    case class Tag(qname: Option[Name.QName], tag: Name.Tag, pat: NamedAst.Pattern, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

    case class Array(elms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

    case class ArrayTailSpread(elms: scala.List[NamedAst.Pattern], sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

  }

  sealed trait ChoicePattern

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(sym: Symbol.VarSym, loc: SourceLocation) extends ChoicePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Guard(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Loop(varSyms: List[Symbol.VarSym], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  sealed trait Type {
    val loc: SourceLocation
  }

  object Type {

    case class Var(tvar: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends NamedAst.Type

    case class Ambiguous(name: Name.QName, loc: SourceLocation) extends NamedAst.Type

    case class Unit(loc: SourceLocation) extends NamedAst.Type

    case class Enum(name: Symbol.EnumSym, loc: SourceLocation) extends NamedAst.Type

    case class Tuple(elms: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class RecordRowEmpty(loc: SourceLocation) extends NamedAst.Type

    case class RecordRowExtend(field: Name.Field, tpe: NamedAst.Type, rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Record(row: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowEmpty(loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowExtendWithAlias(qname: Name.QName, targs: List[NamedAst.Type], rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowExtendWithTypes(ident: Name.Ident, den: Ast.Denotation, tpes: List[NamedAst.Type], rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Schema(row: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Native(fqn: String, loc: SourceLocation) extends NamedAst.Type

    case class Relation(tpes: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class Lattice(tpes: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class Arrow(tparams: List[NamedAst.Type], purAndEff: NamedAst.PurityAndEffect, tresult: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Apply(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class True(loc: SourceLocation) extends NamedAst.Type

    case class False(loc: SourceLocation) extends NamedAst.Type

    case class Not(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class And(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Or(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Complement(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Union(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Intersection(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Difference(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Read(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Write(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Ascribe(tpe: NamedAst.Type, kind: Kind, loc: SourceLocation) extends NamedAst.Type

  }

  sealed trait TypeParams {
    val tparams: List[NamedAst.TypeParam]
  }

  object TypeParams {

    case class Kinded(tparams: List[NamedAst.TypeParam.Kinded]) extends TypeParams

    case class Unkinded(tparams: List[NamedAst.TypeParam.Unkinded]) extends TypeParams

  }

  case class Annotation(name: Ast.Annotation, args: List[NamedAst.Expression], loc: SourceLocation)

  case class Attribute(ident: Name.Ident, tpe: NamedAst.Type, loc: SourceLocation)

  case class Case(ident: Name.Ident, tag: Name.Tag, tpe: NamedAst.Type)

  case class ConstrainedType(ident: Name.Ident, classes: List[Name.QName])

  case class Constraint(cparams: List[NamedAst.ConstraintParam], head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: ast.Type.UnkindedVar, loc: SourceLocation) extends NamedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: ast.Type.UnkindedVar, loc: SourceLocation) extends NamedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: NamedAst.Type, loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[NamedAst.Type], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression, tpe: NamedAst.Type, purAndEff: PurityAndEffect, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, className: String, exp: NamedAst.Expression)

  case class HandlerRule(op: Name.Ident, fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression)

  case class ChoiceRule(pat: List[NamedAst.ChoicePattern], exp: NamedAst.Expression)

  case class MatchRule(pat: NamedAst.Pattern, guard: NamedAst.Expression, exp: NamedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: NamedAst.Expression, exp: NamedAst.Expression)

  sealed trait TypeParam {
    def name: Name.Ident

    def sym: Symbol.UnkindedTypeVarSym

    def loc: SourceLocation
  }

  object TypeParam {

    case class Kinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

  }

  case class TypeConstraint(clazz: Name.QName, tpe: NamedAst.Type, loc: SourceLocation)

  case class PurityAndEffect(pur: Option[Type], eff: Option[List[Type]])
}
