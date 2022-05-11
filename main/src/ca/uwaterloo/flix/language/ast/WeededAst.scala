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

import ca.uwaterloo.flix.language.ast.Ast.Denotation

object WeededAst {

  case class Root(units: Map[Ast.Source, WeededAst.CompilationUnit], entryPoint: Option[Symbol.DefnSym], reachable: Set[Symbol.DefnSym])

  case class CompilationUnit(uses: List[WeededAst.Use], decls: List[WeededAst.Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {

    case class Namespace(name: Name.NName, uses: List[WeededAst.Use], decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    // TODO change laws to WeededAst.Law
    case class Class(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, tparam: WeededAst.TypeParam, superClasses: List[WeededAst.TypeConstraint], sigs: List[WeededAst.Declaration.Sig], laws: List[WeededAst.Declaration.Def], loc: SourceLocation) extends WeededAst.Declaration

    case class Instance(doc: Ast.Doc, mod: Ast.Modifiers, clazz: Name.QName, tpe: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], defs: List[WeededAst.Declaration.Def], loc: SourceLocation) extends WeededAst.Declaration

    case class Sig(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: Option[WeededAst.Expression], tpe: WeededAst.Type, retTpe: WeededAst.Type, pur: WeededAst.Type, eff: WeededAst.EffectSet, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation)

    case class Def(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, retTpe: WeededAst.Type, pur: WeededAst.Type, eff: WeededAst.EffectSet, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation) extends WeededAst.Declaration

    case class Law(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, retTpe: WeededAst.Type, pur: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation) extends WeededAst.Declaration

    case class Enum(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.TypeParams, derives: List[Name.QName], cases: Map[Name.Tag, WeededAst.Case], loc: SourceLocation) extends WeededAst.Declaration

    case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.TypeParams, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Declaration

    case class Effect(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, ops: List[WeededAst.Declaration.Op], loc: SourceLocation) extends WeededAst.Declaration

    case class Op(doc: Ast.Doc, ann: List[WeededAst.Annotation], mod: Ast.Modifiers, ident: Name.Ident, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, retTpe: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation)

  }

  sealed trait Use

  object Use {

    case class UseLower(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends WeededAst.Use

    case class UseUpper(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends WeededAst.Use

    case class UseTag(qname: Name.QName, tag: Name.Tag, alias: Name.Ident, loc: SourceLocation) extends WeededAst.Use

  }

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends WeededAst.Expression

    case class VarOrDefOrSig(name: Name.Ident, loc: SourceLocation) extends WeededAst.Expression

    case class DefOrSig(name: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends WeededAst.Expression

    case class Use(uses: List[WeededAst.Use], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Unit(loc: SourceLocation) extends WeededAst.Expression

    case class Null(loc: SourceLocation) extends WeededAst.Expression

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

    case class Default(loc: SourceLocation) extends WeededAst.Expression

    case class Apply(exp: WeededAst.Expression, exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Lambda(fparam: WeededAst.FormalParam, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Unary(sop: SemanticOperator, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Binary(sop: SemanticOperator, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class IfThenElse(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Stm(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Discard(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Let(ident: Name.Ident, mod: Ast.Modifiers, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class LetRec(ident: Name.Ident, mod: Ast.Modifiers, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Region(tpe: ca.uwaterloo.flix.language.ast.Type, loc: SourceLocation) extends WeededAst.Expression

    case class Scope(ident: Name.Ident, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Match(exp: WeededAst.Expression, rules: List[WeededAst.MatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class Choose(star: Boolean, exps: List[WeededAst.Expression], rules: List[WeededAst.ChoiceRule], loc: SourceLocation) extends WeededAst.Expression

    case class Tag(qname: Option[Name.QName], tag: Name.Tag, expOpt: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends WeededAst.Expression

    case class RecordSelect(exp: WeededAst.Expression, field: Name.Field, loc: SourceLocation) extends WeededAst.Expression

    case class RecordExtend(field: Name.Field, value: WeededAst.Expression, rest: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class RecordRestrict(field: Name.Field, rest: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class New(qname: Name.QName, exp: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLit(exps: List[WeededAst.Expression], exp: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class ArrayNew(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLoad(base: WeededAst.Expression, index: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLength(base: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayStore(base: WeededAst.Expression, index: WeededAst.Expression, elm: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArraySlice(base: WeededAst.Expression, beginIndex: WeededAst.Expression, endIndex: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ref(exp1: WeededAst.Expression, exp2: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Deref(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Assign(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(exp: WeededAst.Expression, expectedType: Option[WeededAst.Type], expectedEff: Option[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class Cast(exp: WeededAst.Expression, declaredType: Option[WeededAst.Type], declaredEff: Option[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class TryCatch(exp: WeededAst.Expression, rules: List[WeededAst.CatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class TryWith(exp: WeededAst.Expression, eff: Name.QName, rules: List[WeededAst.HandlerRule], loc: SourceLocation) extends WeededAst.Expression

    case class Do(op: Name.QName, args: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Resume(args: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class InvokeConstructor(className: String, args: List[WeededAst.Expression], sig: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class InvokeMethod(className: String, methodName: String, exp: WeededAst.Expression, args: List[WeededAst.Expression], sig: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class InvokeStaticMethod(className: String, methodName: String, args: List[WeededAst.Expression], sig: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class GetField(className: String, fieldName: String, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutField(className: String, fieldName: String, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class GetStaticField(className: String, fieldName: String, loc: SourceLocation) extends WeededAst.Expression

    case class PutStaticField(className: String, fieldName: String, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class NewChannel(exp: WeededAst.Expression, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class GetChannel(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutChannel(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class SelectChannel(rules: List[WeededAst.SelectChannelRule], default: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Spawn(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Lazy(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Force(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointConstraintSet(cs: List[WeededAst.Constraint], loc: SourceLocation) extends WeededAst.Expression

    case class FixpointLambda(pparams: List[WeededAst.PredicateParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointMerge(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointSolve(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointProjectIn(exp: WeededAst.Expression, pred: Name.Pred, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointProjectOut(pred: Name.Pred, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Reify(t: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class ReifyType(t: WeededAst.Type, k: Kind, loc: SourceLocation) extends WeededAst.Expression

    case class ReifyEff(ident: Name.Ident, exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

  }

  sealed trait Pattern {
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

    case class Tag(qname: Option[Name.QName], tag: Name.Tag, pat: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class Tuple(elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class Array(elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class ArrayTailSpread(elms: scala.List[WeededAst.Pattern], ident: Option[Name.Ident], loc: SourceLocation) extends WeededAst.Pattern

    case class ArrayHeadSpread(ident: Option[Name.Ident], elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

  }

  sealed trait ChoicePattern

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(ident: Name.Ident, loc: SourceLocation) extends ChoicePattern

  }


  sealed trait Predicate

  object Predicate {

    sealed trait Head extends WeededAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Head

    }

    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Guard(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Loop(idents: List[Name.Ident], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

    }

  }

  sealed trait Type

  object Type {

    case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Type

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends WeededAst.Type

    case class Unit(loc: SourceLocation) extends WeededAst.Type

    case class Tuple(elms: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

    case class RecordRowEmpty(loc: SourceLocation) extends WeededAst.Type

    case class RecordRowExtend(field: Name.Field, tpe: WeededAst.Type, rest: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Record(row: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class SchemaRowEmpty(loc: SourceLocation) extends WeededAst.Type

    case class SchemaRowExtendByAlias(qname: Name.QName, targs: List[WeededAst.Type], rest: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class SchemaRowExtendByTypes(name: Name.Ident, den: Ast.Denotation, tpes: List[WeededAst.Type], rest: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Schema(row: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Relation(tpes: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

    case class Lattice(tpes: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Type

    case class Native(fqn: String, loc: SourceLocation) extends WeededAst.Type

    case class Arrow(tparams: List[WeededAst.Type], pur: WeededAst.Type, eff: WeededAst.EffectSet, tresult: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Apply(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class True(loc: SourceLocation) extends WeededAst.Type

    case class False(loc: SourceLocation) extends WeededAst.Type

    case class Not(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class And(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Or(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Effect(tpe: WeededAst.Type, eff: WeededAst.EffectSet, loc: SourceLocation) extends WeededAst.Type

    case class Ascribe(tpe: WeededAst.Type, kind: Kind, loc: SourceLocation) extends WeededAst.Type

  }

  sealed trait EffectSet

  object EffectSet {
    case class Pure(loc: SourceLocation) extends WeededAst.EffectSet

    case class Singleton(eff: WeededAst.EffectSet.Effect, loc: SourceLocation) extends WeededAst.EffectSet

    case class Complement(eff: WeededAst.EffectSet, loc: SourceLocation) extends WeededAst.EffectSet

    case class Union(eff1: WeededAst.EffectSet, eff2: WeededAst.EffectSet, loc: SourceLocation) extends WeededAst.EffectSet

    case class Intersection(eff1: WeededAst.EffectSet, eff2: WeededAst.EffectSet, loc: SourceLocation) extends WeededAst.EffectSet

    case class Difference(eff1: WeededAst.EffectSet, eff2: WeededAst.EffectSet, loc: SourceLocation) extends WeededAst.EffectSet

    sealed trait Effect

    object Effect {
      case class Eff(name: Name.QName, loc: SourceLocation) extends WeededAst.EffectSet.Effect

      case class Var(eff: Name.Ident, loc: SourceLocation) extends WeededAst.EffectSet.Effect

      case class Read(reg: Name.Ident, loc: SourceLocation) extends WeededAst.EffectSet.Effect

      case class Write(reg: Name.Ident, loc: SourceLocation) extends WeededAst.EffectSet.Effect

      case class Impure(loc: SourceLocation) extends WeededAst.EffectSet.Effect
    }
  }


  sealed trait TypeParams

  sealed trait KindedTypeParams extends TypeParams

  object TypeParams {

    case object Elided extends TypeParams with KindedTypeParams

    case class Unkinded(tparams: List[TypeParam.Unkinded]) extends TypeParams

    case class Kinded(tparams: List[TypeParam.Kinded]) extends TypeParams with KindedTypeParams

  }

  case class Annotation(name: Ast.Annotation, args: List[WeededAst.Expression], loc: SourceLocation)

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation)

  case class Case(ident: Name.Ident, tag: Name.Tag, tpe: WeededAst.Type)

  case class FormalParam(ident: Name.Ident, mod: Ast.Modifiers, tpe: Option[WeededAst.Type], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[WeededAst.Type], loc: SourceLocation) extends PredicateParam

  }

  case class CatchRule(ident: Name.Ident, className: String, exp: WeededAst.Expression)

  case class HandlerRule(op: Name.Ident, fparams: Seq[WeededAst.FormalParam], exp: WeededAst.Expression)

  case class ChoiceRule(pat: List[WeededAst.ChoicePattern], exp: WeededAst.Expression)

  case class TypeConstraint(clazz: Name.QName, tpe: WeededAst.Type, loc: SourceLocation)

  case class Constraint(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation)

  case class MatchRule(pat: WeededAst.Pattern, guard: WeededAst.Expression, exp: WeededAst.Expression)

  case class SelectChannelRule(ident: Name.Ident, channel: WeededAst.Expression, exp: WeededAst.Expression)

  sealed trait TypeParam

  object TypeParam {

    case class Unkinded(ident: Name.Ident) extends TypeParam

    case class Kinded(ident: Name.Ident, kind: Kind) extends TypeParam

  }

}
