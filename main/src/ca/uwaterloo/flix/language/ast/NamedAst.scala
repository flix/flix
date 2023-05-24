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
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.util.collection.MultiMap

object NamedAst {

  case class Root(symbols: Map[Name.NName, Map[String, List[NamedAst.Declaration]]],
                  instances: Map[Name.NName, Map[String, List[NamedAst.Declaration.Instance]]],
                  uses: Map[Name.NName, List[NamedAst.UseOrImport]],
                  units: Map[Ast.Source, NamedAst.CompilationUnit],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  names: MultiMap[List[String], String])

  case class CompilationUnit(usesAndImports: List[NamedAst.UseOrImport], decls: List[NamedAst.Declaration], loc: SourceLocation)
  // TODO change laws to NamedAst.Law

  sealed trait Declaration

  object Declaration {

    case class Namespace(sym: Symbol.ModuleSym, usesAndImports: List[NamedAst.UseOrImport], decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: NamedAst.TypeParam, superClasses: List[NamedAst.TypeConstraint], assocs: List[NamedAst.Declaration.AssocTypeSig], sigs: List[NamedAst.Declaration.Sig], laws: List[NamedAst.Declaration.Def], loc: SourceLocation) extends NamedAst.Declaration

    case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Name.QName, tparams: NamedAst.TypeParams, tpe: NamedAst.Type, tconstrs: List[NamedAst.TypeConstraint], assocs: List[NamedAst.Declaration.AssocTypeDef], defs: List[NamedAst.Declaration.Def], ns: List[String], loc: SourceLocation) extends NamedAst.Declaration

    case class Sig(sym: Symbol.SigSym, spec: NamedAst.Spec, exp: Option[NamedAst.Expression]) extends NamedAst.Declaration

    case class Def(sym: Symbol.DefnSym, spec: NamedAst.Spec, exp: NamedAst.Expression) extends NamedAst.Declaration

    case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: NamedAst.TypeParams, derives: List[Name.QName], cases: List[NamedAst.Declaration.Case], loc: SourceLocation) extends NamedAst.Declaration

    case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: NamedAst.TypeParam, tparams: NamedAst.TypeParams, derives: List[Name.QName], cases: List[NamedAst.Declaration.RestrictableCase], loc: SourceLocation) extends NamedAst.Declaration

    case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: NamedAst.TypeParams, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: NamedAst.TypeParam, kind: NamedAst.Kind, loc: SourceLocation) extends NamedAst.Declaration

    case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, arg: NamedAst.Type, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[NamedAst.Declaration.Op], loc: SourceLocation) extends NamedAst.Declaration

    case class Op(sym: Symbol.OpSym, spec: NamedAst.Spec) extends NamedAst.Declaration

    case class Case(sym: Symbol.CaseSym, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration

    case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Declaration
  }

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: NamedAst.TypeParams, fparams: List[NamedAst.FormalParam], retTpe: NamedAst.Type, pur: Option[NamedAst.Type], tconstrs: List[NamedAst.TypeConstraint], econstrs: List[NamedAst.EqualityConstraint], loc: SourceLocation)


  sealed trait UseOrImport {
    def alias: Name.Ident
    def loc: SourceLocation
  }

  object UseOrImport {

    case class Use(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends NamedAst.UseOrImport

    case class Import(name: Name.JavaName, alias: Name.Ident, loc: SourceLocation) extends NamedAst.UseOrImport
  }

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class Open(qname: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class OpenAs(qname: Name.QName, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends NamedAst.Expression

    case class HoleWithExp(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Use(use: NamedAst.UseOrImport, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends NamedAst.Expression

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

    case class ScopeExit(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Match(exp: NamedAst.Expression, rules: List[NamedAst.MatchRule], loc: SourceLocation) extends NamedAst.Expression

    case class TypeMatch(exp: NamedAst.Expression, rules: List[NamedAst.MatchTypeRule], loc: SourceLocation) extends NamedAst.Expression

    case class RelationalChoose(star: Boolean, exps: List[NamedAst.Expression], rules: List[NamedAst.RelationalChoiceRule], loc: SourceLocation) extends NamedAst.Expression

    case class RestrictableChoose(star: Boolean, exp: NamedAst.Expression, rules: List[NamedAst.RestrictableChoiceRule], loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(elms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends NamedAst.Expression

    case class RecordSelect(exp: NamedAst.Expression, field: Name.Field, loc: SourceLocation) extends NamedAst.Expression

    case class RecordExtend(field: Name.Field, value: NamedAst.Expression, rest: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLit(exps: List[NamedAst.Expression], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayNew(exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLoad(base: NamedAst.Expression, index: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayStore(base: NamedAst.Expression, index: NamedAst.Expression, elm: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ArrayLength(base: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class VectorLit(exps: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class VectorLoad(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class VectorLength(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Deref(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Assign(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(exp: NamedAst.Expression, expectedType: Option[NamedAst.Type], expectedEff: Option[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class InstanceOf(exp: NamedAst.Expression, className: String, loc: SourceLocation) extends NamedAst.Expression

    case class CheckedCast(cast: Ast.CheckedCastType, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class UncheckedCast(exp: NamedAst.Expression, declaredType: Option[NamedAst.Type], declaredEff: Option[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class UncheckedMaskingCast(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Without(exp: NamedAst.Expression, eff: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class TryCatch(exp: NamedAst.Expression, rules: List[NamedAst.CatchRule], loc: SourceLocation) extends NamedAst.Expression

    case class TryWith(exp: NamedAst.Expression, eff: Name.QName, rules: List[NamedAst.HandlerRule], loc: SourceLocation) extends NamedAst.Expression

    case class Do(op: Name.QName, args: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Resume(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeConstructor(className: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Expression

    case class InvokeMethod(className: String, methodName: String, exp: NamedAst.Expression, args: List[NamedAst.Expression], sig: List[NamedAst.Type], retTpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class InvokeStaticMethod(className: String, methodName: String, args: List[NamedAst.Expression], sig: List[NamedAst.Type], retTpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Expression

    case class GetField(className: String, fieldName: String, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class PutField(className: String, fieldName: String, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class GetStaticField(className: String, fieldName: String, loc: SourceLocation) extends NamedAst.Expression

    case class PutStaticField(className: String, fieldName: String, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class NewObject(name: String, tpe: NamedAst.Type, methods: List[JvmMethod], loc: SourceLocation) extends NamedAst.Expression

    case class NewChannel(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class GetChannel(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class PutChannel(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class SelectChannel(rules: List[NamedAst.SelectChannelRule], default: Option[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Spawn(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class ParYield(frags: List[NamedAst.ParYieldFragment], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Lazy(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Force(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointConstraintSet(cs: List[NamedAst.Constraint], loc: SourceLocation) extends NamedAst.Expression

    case class FixpointLambda(pparams: List[NamedAst.PredicateParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointMerge(exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointSolve(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointInject(exp: NamedAst.Expression, pred: Name.Pred, loc: SourceLocation) extends NamedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Error(m: CompilationMessage) extends NamedAst.Expression {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends NamedAst.Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends NamedAst.Pattern

    case class Tag(qname: Name.QName, pat: NamedAst.Pattern, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

  }

  sealed trait RelationalChoicePattern

  object RelationalChoicePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoicePattern

    case class Absent(loc: SourceLocation) extends RelationalChoicePattern

    case class Present(sym: Symbol.VarSym, loc: SourceLocation) extends RelationalChoicePattern

  }

  sealed trait RestrictableChoicePattern

  object RestrictableChoicePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends VarOrWild

    case class Tag(qname: Name.QName, pat: List[VarOrWild], loc: SourceLocation) extends RestrictableChoicePattern

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

      case class Functional(idents: List[Name.Ident], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Guard(exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  sealed trait Type {
    val loc: SourceLocation
  }

  object Type {

    case class Var(ident: Name.Ident, loc: SourceLocation) extends NamedAst.Type

    case class Ambiguous(name: Name.QName, loc: SourceLocation) extends NamedAst.Type

    case class Unit(loc: SourceLocation) extends NamedAst.Type

    case class Tuple(elms: List[NamedAst.Type], loc: SourceLocation) extends NamedAst.Type

    case class RecordRowEmpty(loc: SourceLocation) extends NamedAst.Type

    case class RecordRowExtend(field: Name.Field, tpe: NamedAst.Type, rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Record(row: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowEmpty(loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowExtendWithAlias(qname: Name.QName, targs: List[NamedAst.Type], rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class SchemaRowExtendWithTypes(ident: Name.Ident, den: Ast.Denotation, tpes: List[NamedAst.Type], rest: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Schema(row: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Native(fqn: String, loc: SourceLocation) extends NamedAst.Type

    case class Arrow(tparams: List[NamedAst.Type], pur: Option[NamedAst.Type], tresult: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Apply(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class True(loc: SourceLocation) extends NamedAst.Type

    case class False(loc: SourceLocation) extends NamedAst.Type

    case class Not(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class And(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Or(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Complement(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Union(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Intersection(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class Empty(loc: SourceLocation) extends NamedAst.Type

    case class CaseSet(cases: List[Name.QName], loc: SourceLocation) extends NamedAst.Type

    case class CaseUnion(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class CaseIntersection(tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type

    case class CaseComplement(tpe: NamedAst.Type, loc: SourceLocation) extends NamedAst.Type


    case class Ascribe(tpe: NamedAst.Type, kind: NamedAst.Kind, loc: SourceLocation) extends NamedAst.Type

  }

  sealed trait Kind

  object Kind {
    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends NamedAst.Kind

    case class Arrow(k1: NamedAst.Kind, k2: NamedAst.Kind, loc: SourceLocation) extends NamedAst.Kind
  }

  sealed trait TypeParams {
    val tparams: List[NamedAst.TypeParam]
  }

  object TypeParams {

    case class Kinded(tparams: List[NamedAst.TypeParam.Kinded]) extends TypeParams

    case class Unkinded(tparams: List[NamedAst.TypeParam.Unkinded]) extends TypeParams

    case class Implicit(tparams: List[NamedAst.TypeParam.Implicit]) extends TypeParams

  }

  case class Attribute(ident: Name.Ident, tpe: NamedAst.Type, loc: SourceLocation)

  case class Constraint(cparams: List[NamedAst.ConstraintParam], head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Option[NamedAst.Type], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[NamedAst.Type], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression, tpe: NamedAst.Type, pur: Option[NamedAst.Type], loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, className: String, exp: NamedAst.Expression)

  case class HandlerRule(op: Name.Ident, fparams: List[NamedAst.FormalParam], exp: NamedAst.Expression)

  case class RelationalChoiceRule(pat: List[NamedAst.RelationalChoicePattern], exp: NamedAst.Expression)

  case class RestrictableChoiceRule(pat: NamedAst.RestrictableChoicePattern, exp: NamedAst.Expression)

  case class MatchRule(pat: NamedAst.Pattern, guard: Option[NamedAst.Expression], exp: NamedAst.Expression)

  case class MatchTypeRule(sym: Symbol.VarSym, tpe: NamedAst.Type, exp: NamedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: NamedAst.Expression, exp: NamedAst.Expression)

  sealed trait TypeParam {
    def name: Name.Ident

    def sym: Symbol.UnkindedTypeVarSym

    def loc: SourceLocation
  }

  object TypeParam {

    case class Kinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

    case class Implicit(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

  }

  case class TypeConstraint(clazz: Name.QName, tpe: NamedAst.Type, loc: SourceLocation)

  case class EqualityConstraint(qname: Name.QName, tpe1: NamedAst.Type, tpe2: NamedAst.Type, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expression, loc: SourceLocation)

}
