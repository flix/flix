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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.util.collection.MultiMap

object WeededAst {

  val empty: Root = Root(Map.empty, None, MultiMap.empty)

  case class Root(units: Map[Ast.Source, WeededAst.CompilationUnit], entryPoint: Option[Symbol.DefnSym], names: MultiMap[List[String], String])

  case class CompilationUnit(usesAndImports: List[WeededAst.UseOrImport], decls: List[WeededAst.Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {

    case class Namespace(ident: Name.Ident, usesAndImports: List[WeededAst.UseOrImport], decls: List[WeededAst.Declaration], loc: SourceLocation) extends WeededAst.Declaration

    // TODO change laws to WeededAst.Law
    case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparam: WeededAst.TypeParam, superClasses: List[WeededAst.TypeConstraint], assocs: List[WeededAst.Declaration.AssocTypeSig], sigs: List[WeededAst.Declaration.Sig], laws: List[WeededAst.Declaration.Def], loc: SourceLocation) extends WeededAst.Declaration

    case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Name.QName, tpe: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], assocs: List[WeededAst.Declaration.AssocTypeDef], defs: List[WeededAst.Declaration.Def], loc: SourceLocation) extends WeededAst.Declaration

    case class Sig(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: Option[WeededAst.Expression], tpe: WeededAst.Type, eff: Option[WeededAst.Type], tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation)

    case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: Option[WeededAst.Type], tconstrs: List[WeededAst.TypeConstraint], constrs: List[WeededAst.EqualityConstraint], loc: SourceLocation) extends WeededAst.Declaration

    case class Law(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.KindedTypeParams, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation) extends WeededAst.Declaration

    case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.TypeParams, derives: List[Name.QName], cases: List[WeededAst.Case], loc: SourceLocation) extends WeededAst.Declaration

    case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, index: WeededAst.TypeParam, tparams: WeededAst.TypeParams, derives: List[Name.QName], cases: List[WeededAst.RestrictableCase], loc: SourceLocation) extends WeededAst.Declaration

    case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, tparams: WeededAst.TypeParams, tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Declaration

    case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, tparam: WeededAst.TypeParam, kind: WeededAst.Kind, loc: SourceLocation)

    case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, arg: WeededAst.Type, tpe: WeededAst.Type, loc: SourceLocation)

    case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, ops: List[WeededAst.Declaration.Op], loc: SourceLocation) extends WeededAst.Declaration

    case class Op(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, tconstrs: List[WeededAst.TypeConstraint], loc: SourceLocation)

  }

  sealed trait UseOrImport

  object UseOrImport {

    case class Use(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends WeededAst.UseOrImport

    case class Import(name: Name.JavaName, alias: Name.Ident, loc: SourceLocation) extends WeededAst.UseOrImport
  }


  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class Open(qname: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class OpenAs(qname: Name.QName, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends WeededAst.Expression

    case class HoleWithExp(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Use(uses: List[WeededAst.UseOrImport], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends WeededAst.Expression

    case class Apply(exp: WeededAst.Expression, exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Lambda(fparam: WeededAst.FormalParam, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Unary(sop: SemanticOp, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Binary(sop: SemanticOp, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class IfThenElse(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Stm(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Discard(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Let(ident: Name.Ident, mod: Ast.Modifiers, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class LetRec(ident: Name.Ident, mod: Ast.Modifiers, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Region(tpe: ca.uwaterloo.flix.language.ast.Type, loc: SourceLocation) extends WeededAst.Expression

    case class Scope(ident: Name.Ident, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ScopeExit(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Match(exp: WeededAst.Expression, rules: List[WeededAst.MatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class TypeMatch(exp: WeededAst.Expression, rules: List[WeededAst.TypeMatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class RelationalChoose(star: Boolean, exps: List[WeededAst.Expression], rules: List[WeededAst.RelationalChooseRule], loc: SourceLocation) extends WeededAst.Expression

    case class RestrictableChoose(star: Boolean, exp: WeededAst.Expression, rules: List[WeededAst.RestrictableChooseRule], loc: SourceLocation) extends WeededAst.Expression

    case class Tuple(exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends WeededAst.Expression

    case class RecordSelect(exp: WeededAst.Expression, field: Name.Field, loc: SourceLocation) extends WeededAst.Expression

    case class RecordExtend(field: Name.Field, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class RecordRestrict(field: Name.Field, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLit(exps: List[WeededAst.Expression], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayNew(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLoad(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayLength(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ArrayStore(exp1: WeededAst.Expression, exp2: WeededAst.Expression, exp3: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class VectorLit(exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class VectorLoad(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class VectorLength(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ref(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Deref(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Assign(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Ascribe(exp: WeededAst.Expression, expectedType: Option[WeededAst.Type], expectedEff: Option[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class InstanceOf(exp: WeededAst.Expression, className: String, loc: SourceLocation) extends WeededAst.Expression

    case class CheckedCast(cast: Ast.CheckedCastType, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class UncheckedCast(exp: WeededAst.Expression, declaredType: Option[WeededAst.Type], declaredEff: Option[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class UncheckedMaskingCast(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Without(exp: WeededAst.Expression, eff: Name.QName, loc: SourceLocation) extends WeededAst.Expression

    case class TryCatch(exp: WeededAst.Expression, rules: List[WeededAst.CatchRule], loc: SourceLocation) extends WeededAst.Expression

    case class TryWith(exp: WeededAst.Expression, eff: Name.QName, rules: List[WeededAst.HandlerRule], loc: SourceLocation) extends WeededAst.Expression

    case class Do(op: Name.QName, exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Resume(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class InvokeConstructor(className: String, exps: List[WeededAst.Expression], sig: List[WeededAst.Type], loc: SourceLocation) extends WeededAst.Expression

    case class InvokeMethod(className: String, methodName: String, exp: WeededAst.Expression, exps: List[WeededAst.Expression], sig: List[WeededAst.Type], retTpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class InvokeStaticMethod(className: String, methodName: String, exps: List[WeededAst.Expression], sig: List[WeededAst.Type], retTpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

    case class GetField(className: String, fieldName: String, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutField(className: String, fieldName: String, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class GetStaticField(className: String, fieldName: String, loc: SourceLocation) extends WeededAst.Expression

    case class PutStaticField(className: String, fieldName: String, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class NewObject(tpe: WeededAst.Type, methods: List[JvmMethod], loc: SourceLocation) extends WeededAst.Expression

    case class NewChannel(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class GetChannel(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class PutChannel(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class SelectChannel(rules: List[WeededAst.SelectChannelRule], exp: Option[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Expression

    case class Spawn(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class ParYield(frags: List[WeededAst.ParYieldFragment], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Lazy(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Force(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointConstraintSet(cs: List[WeededAst.Constraint], loc: SourceLocation) extends WeededAst.Expression

    case class FixpointLambda(pparams: List[WeededAst.PredicateParam], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointMerge(exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointSolve(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointInject(exp: WeededAst.Expression, pred: Name.Pred, loc: SourceLocation) extends WeededAst.Expression

    case class FixpointProject(pred: Name.Pred, exp1: WeededAst.Expression, exp2: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Expression

    case class Error(m: CompilationMessage) extends WeededAst.Expression {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: Name.Ident, loc: SourceLocation) extends WeededAst.Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends WeededAst.Pattern

    case class Tag(qname: Name.QName, pat: WeededAst.Pattern, loc: SourceLocation) extends WeededAst.Pattern

    case class Tuple(elms: scala.List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Pattern

    case class Record(pats: List[Record.RecordFieldPattern], pat: Option[Pattern], loc: SourceLocation) extends WeededAst.Pattern

    object Record {
      case class RecordFieldPattern(field: Name.Field, tpe: Option[Type], pat: Option[Pattern], loc: SourceLocation)
    }

  }

  sealed trait RelationalChoosePattern

  object RelationalChoosePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoosePattern

    case class Absent(loc: SourceLocation) extends RelationalChoosePattern

    case class Present(ident: Name.Ident, loc: SourceLocation) extends RelationalChoosePattern

  }

  sealed trait RestrictableChoosePattern {
    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(ident: Name.Ident, loc: SourceLocation) extends VarOrWild

    case class Tag(qname: Name.QName, pat: List[VarOrWild], loc: SourceLocation) extends RestrictableChoosePattern

  }


  sealed trait Predicate

  object Predicate {

    sealed trait Head extends WeededAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, exps: List[WeededAst.Expression], loc: SourceLocation) extends WeededAst.Predicate.Head

    }

    sealed trait Body extends WeededAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[WeededAst.Pattern], loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Functional(idents: List[Name.Ident], exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

      case class Guard(exp: WeededAst.Expression, loc: SourceLocation) extends WeededAst.Predicate.Body

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

    case class Native(fqn: String, loc: SourceLocation) extends WeededAst.Type

    case class Arrow(tparams: List[WeededAst.Type], eff: Option[WeededAst.Type], tresult: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Apply(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class True(loc: SourceLocation) extends WeededAst.Type

    case class False(loc: SourceLocation) extends WeededAst.Type

    case class Not(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class And(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Or(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Complement(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Union(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Intersection(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Pure(loc: SourceLocation) extends WeededAst.Type

    case class CaseSet(cases: List[Name.QName], loc: SourceLocation) extends WeededAst.Type

    case class CaseUnion(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class CaseIntersection(tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class CaseComplement(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Type

    case class Ascribe(tpe: WeededAst.Type, kind: WeededAst.Kind, loc: SourceLocation) extends WeededAst.Type

  }

  sealed trait Kind

  object Kind {
    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends WeededAst.Kind

    case class Arrow(k1: Kind, k2: Kind, loc: SourceLocation) extends WeededAst.Kind
  }

  sealed trait TypeParams

  sealed trait KindedTypeParams extends TypeParams

  object TypeParams {

    case object Elided extends TypeParams with KindedTypeParams

    case class Unkinded(tparams: List[TypeParam.Unkinded]) extends TypeParams

    case class Kinded(tparams: List[TypeParam.Kinded]) extends TypeParams with KindedTypeParams

  }

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation)

  case class Case(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation)

  case class RestrictableCase(ident: Name.Ident, tpe: WeededAst.Type, loc: SourceLocation)

  case class FormalParam(ident: Name.Ident, mod: Ast.Modifiers, tpe: Option[WeededAst.Type], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[WeededAst.Type], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression, tpe: WeededAst.Type, eff: Option[WeededAst.Type], loc: SourceLocation)

  case class CatchRule(ident: Name.Ident, className: String, exp: WeededAst.Expression)

  case class HandlerRule(op: Name.Ident, fparams: List[WeededAst.FormalParam], exp: WeededAst.Expression)

  case class RelationalChooseRule(pat: List[WeededAst.RelationalChoosePattern], exp: WeededAst.Expression)

  case class RestrictableChooseRule(pat: WeededAst.RestrictableChoosePattern, exp: WeededAst.Expression)

  case class TypeConstraint(clazz: Name.QName, tpe: WeededAst.Type, loc: SourceLocation)

  case class EqualityConstraint(qname: Name.QName, tpe1: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation)

  case class Constraint(head: WeededAst.Predicate.Head, body: List[WeededAst.Predicate.Body], loc: SourceLocation)

  case class MatchRule(pat: WeededAst.Pattern, exp1: Option[WeededAst.Expression], exp2: WeededAst.Expression)

  case class TypeMatchRule(ident: Name.Ident, tpe: WeededAst.Type, exp: WeededAst.Expression)

  case class SelectChannelRule(ident: Name.Ident, exp1: WeededAst.Expression, exp2: WeededAst.Expression)

  sealed trait TypeParam

  object TypeParam {

    case class Unkinded(ident: Name.Ident) extends TypeParam

    case class Kinded(ident: Name.Ident, kind: WeededAst.Kind) extends TypeParam

  }

  case class ParYieldFragment(pat: WeededAst.Pattern, exp: WeededAst.Expression, loc: SourceLocation)

}
