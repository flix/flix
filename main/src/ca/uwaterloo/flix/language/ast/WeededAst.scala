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

  case class Root(units: Map[Ast.Source, CompilationUnit], entryPoint: Option[Symbol.DefnSym], names: MultiMap[List[String], String])

  case class CompilationUnit(usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation)

  sealed trait Declaration {
    def loc: SourceLocation
  }

  object Declaration {

    case class Namespace(ident: Name.Ident, usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation) extends Declaration

    // TODO change laws to Law
    case class Trait(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparam: TypeParam, superTraits: List[TypeConstraint], assocs: List[Declaration.AssocTypeSig], sigs: List[Declaration.Sig], laws: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Name.QName, tpe: Type, tconstrs: List[TypeConstraint], assocs: List[Declaration.AssocTypeDef], defs: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Sig(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: KindedTypeParams, fparams: List[FormalParam], exp: Option[Expr], tpe: Type, eff: Option[Type], tconstrs: List[TypeConstraint], econstrs: List[EqualityConstraint], loc: SourceLocation)

    case class Def(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: KindedTypeParams, fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Option[Type], tconstrs: List[TypeConstraint], constrs: List[EqualityConstraint], loc: SourceLocation) extends Declaration

    case class Law(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: KindedTypeParams, fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Type, tconstrs: List[TypeConstraint], loc: SourceLocation) extends Declaration

    case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: TypeParams, derives: Derivations, cases: List[Case], loc: SourceLocation) extends Declaration

    case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, index: TypeParam, tparams: TypeParams, derives: Derivations, cases: List[RestrictableCase], loc: SourceLocation) extends Declaration

    case class TypeAlias(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, tparams: TypeParams, tpe: Type, loc: SourceLocation) extends Declaration

    case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

    case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, ident: Name.Ident, arg: Type, tpe: Type, loc: SourceLocation)

    case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, ops: List[Declaration.Op], loc: SourceLocation) extends Declaration

    case class Op(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, ident: Name.Ident, fparams: List[FormalParam], tpe: Type, tconstrs: List[TypeConstraint], loc: SourceLocation)

  }

  sealed trait UseOrImport

  object UseOrImport {

    case class Use(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends UseOrImport

    case class Import(name: Name.JavaName, alias: Name.Ident, loc: SourceLocation) extends UseOrImport
  }


  sealed trait Expr {
    def loc: SourceLocation
  }

  object Expr {

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Expr

    case class Open(qname: Name.QName, loc: SourceLocation) extends Expr

    case class OpenAs(qname: Name.QName, exp: Expr, loc: SourceLocation) extends Expr

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, loc: SourceLocation) extends Expr

    case class Use(uses: List[UseOrImport], exp: Expr, loc: SourceLocation) extends Expr

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Expr

    case class Apply(exp: Expr, exps: List[Expr], loc: SourceLocation) extends Expr

    case class Infix(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Lambda(fparam: FormalParam, exp: Expr, loc: SourceLocation) extends Expr

    case class LambdaMatch(pat: Pattern, exp: Expr, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, loc: SourceLocation) extends Expr

    case class Let(ident: Name.Ident, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LetRec(ident: Name.Ident, ann: Ast.Annotations, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LetImport(op: JvmOp, exp: Expr, loc: SourceLocation) extends Expr

    case class Scope(ident: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr

    case class Match(exp: Expr, rules: List[MatchRule], loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], loc: SourceLocation) extends Expr

    case class ApplicativeFor(frags: List[ForFragment.Generator], exp: Expr, loc: SourceLocation) extends Expr

    case class ForEach(frags: List[ForFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class MonadicFor(frags: List[ForFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class ForEachYield(frags: List[ForFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class LetMatch(pat: Pattern, mod: Ast.Modifiers, tpe: Option[Type], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], loc: SourceLocation) extends Expr

    case class RecordEmpty(loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, label: Name.Label, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class ArrayLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ArrayLength(exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayStore(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr

    case class FCons(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class FAppend(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ListLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class SetLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class MapLit(exps: List[(Expr, Expr)], loc: SourceLocation) extends Expr

    case class Ref(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Deref(exp: Expr, loc: SourceLocation) extends Expr

    case class Assign(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Ascribe(exp: Expr, expectedType: Option[Type], expectedEff: Option[Type], loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, className: String, loc: SourceLocation) extends Expr

    case class CheckedCast(cast: Ast.CheckedCastType, exp: Expr, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], loc: SourceLocation) extends Expr

    case class UncheckedMaskingCast(exp: Expr, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, eff: Name.QName, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, handlers: List[CatchRule], loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, handler: List[WithHandler], loc: SourceLocation) extends Expr

    case class Do(op: Name.QName, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeMethod2(exp: Expr, name: Name.Ident, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeConstructor(className: String, exps: List[Expr], sig: List[Type], loc: SourceLocation) extends Expr

    case class InvokeMethod(className: String, methodName: String, exp: Expr, exps: List[Expr], sig: List[Type], retTpe: Type, loc: SourceLocation) extends Expr

    case class InvokeStaticMethod(className: String, methodName: String, exps: List[Expr], sig: List[Type], retTpe: Type, loc: SourceLocation) extends Expr

    case class GetField(className: String, fieldName: String, exp: Expr, loc: SourceLocation) extends Expr

    case class PutField(className: String, fieldName: String, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetStaticField(className: String, fieldName: String, loc: SourceLocation) extends Expr

    case class PutStaticField(className: String, fieldName: String, exp: Expr, loc: SourceLocation) extends Expr

    case class NewObject(tpe: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class Static(loc: SourceLocation) extends Expr

    case class NewChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], exp: Option[Expr], loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ParYield(frags: List[ParYieldFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class Lazy(exp: Expr, loc: SourceLocation) extends Expr

    case class Force(exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointConstraintSet(cs: List[Constraint], loc: SourceLocation) extends Expr

    case class FixpointLambda(pparams: List[PredicateParam], exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointMerge(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class FixpointSolve(exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointFilter(pred: Name.Pred, exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointInject(exp: Expr, pred: Name.Pred, loc: SourceLocation) extends Expr

    case class FixpointInjectInto(exps: List[Expr], idents: List[Name.Ident], loc: SourceLocation) extends Expr

    case class FixpointSolveWithProject(exps: List[Expr], optIdents: Option[List[Name.Ident]], loc: SourceLocation) extends Expr

    case class FixpointQueryWithSelect(exps: List[Expr], selects: List[Expr], from: List[Predicate.Body], where: List[Expr], loc: SourceLocation) extends Expr

    case class FixpointProject(pred: Name.Pred, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Debug(exp: Expr, kind: DebugKind, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends Pattern

    case class Var(ident: Name.Ident, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Pattern

    case class Tag(qname: Name.QName, pat: Pattern, loc: SourceLocation) extends Pattern

    case class Tuple(elms: scala.List[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, loc: SourceLocation) extends Pattern

    case class RecordEmpty(loc: SourceLocation) extends Pattern

    case class Error(loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Option[Pattern], loc: SourceLocation)
    }

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

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, exps: List[Expr], loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[Pattern], loc: SourceLocation) extends Predicate.Body

      case class Functional(idents: List[Name.Ident], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  sealed trait Type

  object Type {

    case class Var(ident: Name.Ident, loc: SourceLocation) extends Type

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Type

    case class Unit(loc: SourceLocation) extends Type

    case class Tuple(elms: List[Type], loc: SourceLocation) extends Type

    case class RecordRowEmpty(loc: SourceLocation) extends Type

    case class RecordRowExtend(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation) extends Type

    case class Record(row: Type, loc: SourceLocation) extends Type

    case class SchemaRowEmpty(loc: SourceLocation) extends Type

    case class SchemaRowExtendByAlias(qname: Name.QName, targs: List[Type], rest: Type, loc: SourceLocation) extends Type

    case class SchemaRowExtendByTypes(name: Name.Ident, den: Ast.Denotation, tpes: List[Type], rest: Type, loc: SourceLocation) extends Type

    case class Schema(row: Type, loc: SourceLocation) extends Type

    case class Native(fqn: String, loc: SourceLocation) extends Type

    case class Arrow(tparams: List[Type], eff: Option[Type], tresult: Type, loc: SourceLocation) extends Type

    case class Apply(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class True(loc: SourceLocation) extends Type

    case class False(loc: SourceLocation) extends Type

    case class Not(tpe: Type, loc: SourceLocation) extends Type

    case class And(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Or(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Complement(tpe: Type, loc: SourceLocation) extends Type

    case class Union(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Intersection(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Pure(loc: SourceLocation) extends Type

    case class CaseSet(cases: List[Name.QName], loc: SourceLocation) extends Type

    case class CaseUnion(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class CaseIntersection(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class CaseComplement(tpe: Type, loc: SourceLocation) extends Type

    case class Ascribe(tpe: Type, kind: Kind, loc: SourceLocation) extends Type

    case class Error(loc: SourceLocation) extends Type

  }

  sealed trait Kind

  object Kind {
    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Kind

    case class Arrow(k1: Kind, k2: Kind, loc: SourceLocation) extends Kind
  }

  sealed trait TypeParams

  sealed trait KindedTypeParams extends TypeParams

  object TypeParams {

    case object Elided extends TypeParams with KindedTypeParams

    case class Unkinded(tparams: List[TypeParam.Unkinded]) extends TypeParams

    case class Kinded(tparams: List[TypeParam.Kinded]) extends TypeParams with KindedTypeParams

  }

  case class Attribute(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Case(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class FormalParam(ident: Name.Ident, mod: Ast.Modifiers, tpe: Option[Type], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[Type], loc: SourceLocation) extends PredicateParam

  }

  case class JavaClassMember(prefix: String, suffix: List[String], loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Option[Type], loc: SourceLocation)

  case class CatchRule(ident: Name.Ident, className: String, exp: Expr)

  case class HandlerRule(op: Name.Ident, fparams: List[FormalParam], exp: Expr)

  case class WithHandler(eff: Name.QName, rules: List[HandlerRule])

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class TypeConstraint(trt: Name.QName, tpe: Type, loc: SourceLocation)

  case class EqualityConstraint(qname: Name.QName, tpe1: Type, tpe2: Type, loc: SourceLocation)

  case class Constraint(head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class MatchRule(pat: Pattern, exp1: Option[Expr], exp2: Expr)

  case class TypeMatchRule(ident: Name.Ident, tpe: Type, exp: Expr)

  case class SelectChannelRule(ident: Name.Ident, exp1: Expr, exp2: Expr)

  sealed trait TypeParam

  object TypeParam {

    case class Unkinded(ident: Name.Ident) extends TypeParam

    case class Kinded(ident: Name.Ident, kind: Kind) extends TypeParam

  }

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

  case class Derivations(classes: List[Name.QName], loc: SourceLocation)


  sealed trait ForFragment {
    def loc: SourceLocation
  }

  object ForFragment {

    case class Generator(pat: Pattern, exp: Expr, loc: SourceLocation) extends ForFragment

    case class Guard(exp: Expr, loc: SourceLocation) extends ForFragment

    case class Let(pat: Pattern, exp: Expr, loc: SourceLocation) extends ForFragment
  }

  sealed trait DebugKind

  object DebugKind {

    case object Debug extends DebugKind

    case object DebugWithLoc extends DebugKind

    case object DebugWithLocAndSrc extends DebugKind

  }

  sealed trait JvmOp

  object JvmOp {

    case class Constructor(fqn: Name.JavaName, sig: List[WeededAst.Type], tpe: Type, eff: Option[WeededAst.Type], ident: Name.Ident) extends JvmOp

    case class Method(fqn: WeededAst.JavaClassMember, sig: List[WeededAst.Type], tpe: Type, eff: Option[WeededAst.Type], ident: Option[Name.Ident]) extends JvmOp

    case class StaticMethod(fqn: WeededAst.JavaClassMember, sig: List[WeededAst.Type], tpe: Type, eff: Option[WeededAst.Type], ident: Option[Name.Ident]) extends JvmOp

    case class GetField(fqn: WeededAst.JavaClassMember, tpe: Type, eff: Option[WeededAst.Type], ident: Name.Ident) extends JvmOp

    case class PutField(fqn: WeededAst.JavaClassMember, tpe: Type, eff: Option[WeededAst.Type], ident: Name.Ident) extends JvmOp

    case class GetStaticField(fqn: WeededAst.JavaClassMember, tpe: Type, eff: Option[WeededAst.Type], ident: Name.Ident) extends JvmOp

    case class PutStaticField(fqn: WeededAst.JavaClassMember, tpe: Type, eff: Option[WeededAst.Type], ident: Name.Ident) extends JvmOp

  }

}
