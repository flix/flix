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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.util.collection.MultiMap

import java.lang.reflect.{Constructor, Field, Method}

object ResolvedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, List.empty, None, Map.empty, MultiMap.empty)

  case class Root(classes: Map[Symbol.ClassSym, Declaration.Class],
                  instances: Map[Symbol.ClassSym, List[Declaration.Instance]],
                  defs: Map[Symbol.DefnSym, Declaration.Def],
                  enums: Map[Symbol.EnumSym, Declaration.Enum],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, Declaration.RestrictableEnum],
                  effects: Map[Symbol.EffectSym, Declaration.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, Declaration.TypeAlias],
                  uses: Map[Symbol.ModuleSym, List[Ast.UseOrImport]],
                  taOrder: List[Symbol.TypeAliasSym],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  names: MultiMap[List[String], String])

  // TODO use Law for laws
  case class CompilationUnit(usesAndImports: List[Ast.UseOrImport], decls: List[Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {
    case class Namespace(sym: Symbol.ModuleSym, usesAndImports: List[Ast.UseOrImport], decls: List[Declaration], loc: SourceLocation) extends Declaration

    case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: TypeParam, superClasses: List[TypeConstraint], assocs: List[Declaration.AssocTypeSig], sigs: Map[Symbol.SigSym, Declaration.Sig], laws: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Ast.ClassSymUse, tpe: UnkindedType, tconstrs: List[TypeConstraint], assocs: List[Declaration.AssocTypeDef], defs: List[Declaration.Def], ns: Name.NName, loc: SourceLocation) extends Declaration

    case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr]) extends Declaration

    case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr) extends Declaration

    case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: TypeParams, derives: Ast.Derivations, cases: List[Declaration.Case], loc: SourceLocation) extends Declaration

    case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: TypeParams, derives: Ast.Derivations, cases: List[Declaration.RestrictableCase], loc: SourceLocation) extends Declaration

    case class Case(sym: Symbol.CaseSym, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: TypeParams, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, loc: SourceLocation) extends Declaration

    case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: UnkindedType, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Declaration.Op], loc: SourceLocation) extends Declaration

    case class Op(sym: Symbol.OpSym, spec: Spec) extends Declaration
  }

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: TypeParams, fparams: List[FormalParam], tpe: UnkindedType, eff: Option[UnkindedType], tconstrs: List[TypeConstraint], econstrs: List[EqualityConstraint], loc: SourceLocation)

  sealed trait Expr {
    def loc: SourceLocation
  }

  object Expr {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Expr

    case class Def(sym: Symbol.DefnSym, loc: SourceLocation) extends Expr

    case class Sig(sym: Symbol.SigSym, loc: SourceLocation) extends Expr

    case class Hole(sym: Symbol.HoleSym, loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: Ast.RestrictableEnumSymUse, exp: Expr, loc: SourceLocation) extends Expr

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Expr

    case class Apply(exp: Expr, exps: List[Expr], loc: SourceLocation) extends Expr

    case class Lambda(fparam: FormalParam, exp: Expr, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp, exp: Expr, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    // MATT why was this a full type
    case class Region(tpe: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Symbol.UnkindedTypeVarSym, exp: Expr, loc: SourceLocation) extends Expr

    case class ScopeExit(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Match(exp: Expr, rules: List[MatchRule], loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], loc: SourceLocation) extends Expr

    case class RelationalChoose(star: Boolean, exps: List[Expr], rules: List[RelationalChooseRule], loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], loc: SourceLocation) extends Expr

    case class Tag(sym: Ast.CaseSymUse, exp: Expr, loc: SourceLocation) extends Expr

    case class RestrictableTag(sym: Ast.RestrictableCaseSymUse, exp: Expr, isOpen: Boolean, loc: SourceLocation) extends Expr

    case class Tuple(elms: List[Expr], loc: SourceLocation) extends Expr

    case class RecordEmpty(loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, field: Name.Field, loc: SourceLocation) extends Expr

    case class RecordExtend(field: Name.Field, value: Expr, rest: Expr, loc: SourceLocation) extends Expr

    case class RecordRestrict(field: Name.Field, rest: Expr, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class ArrayLoad(base: Expr, index: Expr, loc: SourceLocation) extends Expr

    case class ArrayStore(base: Expr, index: Expr, elm: Expr, loc: SourceLocation) extends Expr

    case class ArrayLength(base: Expr, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr

    case class Ref(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Deref(exp: Expr, loc: SourceLocation) extends Expr

    case class Assign(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Ascribe(exp: Expr, expectedType: Option[UnkindedType], expectedEff: Option[UnkindedType], loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: java.lang.Class[_], loc: SourceLocation) extends Expr

    case class CheckedCast(cast: Ast.CheckedCastType, exp: Expr, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[UnkindedType], declaredEff: Option[UnkindedType], loc: SourceLocation) extends Expr

    case class UncheckedMaskingCast(exp: Expr, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, eff: Ast.EffectSymUse, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, eff: Ast.EffectSymUse, rules: List[HandlerRule], loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, args: List[Expr], loc: SourceLocation) extends Expr

    case class Resume(exp: Expr, loc: SourceLocation) extends Expr

    case class InvokeConstructor(constructor: Constructor[_], args: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeMethod(method: Method, clazz: java.lang.Class[_], exp: Expr, args: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeStaticMethod(method: Method, args: List[Expr], loc: SourceLocation) extends Expr

    case class GetField(field: Field, clazz: java.lang.Class[_], exp: Expr, loc: SourceLocation) extends Expr

    case class PutField(field: Field, clazz: java.lang.Class[_], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetStaticField(field: Field, loc: SourceLocation) extends Expr

    case class PutStaticField(field: Field, exp: Expr, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Expr], loc: SourceLocation) extends Expr

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

    case class FixpointProject(pred: Name.Pred, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, loc: SourceLocation) extends Pattern

    case class Tuple(elms: List[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordFieldPattern], pat: Pattern, loc: SourceLocation) extends Pattern

    case class RecordEmpty(loc: SourceLocation) extends Pattern

    object Record {
      case class RecordFieldPattern(field: Name.Field, pat: Pattern, loc: SourceLocation)
    }

  }

  sealed trait RelationalChoosePattern {
    def loc: SourceLocation
  }

  object RelationalChoosePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoosePattern

    case class Absent(loc: SourceLocation) extends RelationalChoosePattern

    case class Present(sym: Symbol.VarSym, loc: SourceLocation) extends RelationalChoosePattern

  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: Ast.RestrictableCaseSymUse, pat: List[VarOrWild], loc: SourceLocation) extends RestrictableChoosePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[Pattern], loc: SourceLocation) extends Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  sealed trait TypeParams {
    val tparams: List[TypeParam]
  }

  object TypeParams {

    case class Kinded(tparams: List[TypeParam.Kinded]) extends TypeParams

    case class Unkinded(tparams: List[TypeParam.Unkinded]) extends TypeParams

  }

  case class Attribute(ident: Name.Ident, tpe: UnkindedType, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Option[UnkindedType], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Ast.Denotation, tpes: List[UnkindedType], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: Seq[FormalParam], exp: Expr, tpe: UnkindedType, eff: Option[UnkindedType], loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: Seq[FormalParam], exp: Expr)

  case class RelationalChooseRule(pat: List[RelationalChoosePattern], exp: Expr)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: UnkindedType, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  sealed trait TypeParam {
    val name: Name.Ident
    val sym: Symbol.UnkindedTypeVarSym
  }

  object TypeParam {

    case class Kinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

  }

  case class TypeConstraint(head: Ast.TypeConstraint.Head, tpe: UnkindedType, loc: SourceLocation)

  case class EqualityConstraint(cst: Ast.AssocTypeConstructor, tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
