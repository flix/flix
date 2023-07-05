/*
 *  Copyright 2021 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}
import ca.uwaterloo.flix.language.{CompilationMessage, ast}
import ca.uwaterloo.flix.util.collection.MultiMap

import java.lang.reflect.{Constructor, Field, Method}

object KindedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, MultiMap.empty)

  case class Root(classes: Map[Symbol.ClassSym, KindedAst.Class],
                  instances: Map[Symbol.ClassSym, List[KindedAst.Instance]],
                  defs: Map[Symbol.DefnSym, KindedAst.Def],
                  enums: Map[Symbol.EnumSym, KindedAst.Enum],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, KindedAst.RestrictableEnum],
                  effects: Map[Symbol.EffectSym, KindedAst.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias],
                  uses: Map[Symbol.ModuleSym, List[Ast.UseOrImport]],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  names: MultiMap[List[String], String])

  case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: KindedAst.TypeParam, superClasses: List[Ast.TypeConstraint], assocs: List[KindedAst.AssocTypeSig], sigs: Map[Symbol.SigSym, KindedAst.Sig], laws: List[KindedAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Ast.ClassSymUse, tpe: Type, tconstrs: List[Ast.TypeConstraint], assocs: List[KindedAst.AssocTypeDef], defs: List[KindedAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: KindedAst.Spec, exp: Option[KindedAst.Expression])

  case class Def(sym: Symbol.DefnSym, spec: KindedAst.Spec, exp: KindedAst.Expression)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[KindedAst.TypeParam], fparams: List[KindedAst.FormalParam], sc: Scheme, tpe: Type, eff: Type, tconstrs: List[Ast.TypeConstraint], loc: SourceLocation)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[KindedAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.CaseSym, KindedAst.Case], tpe: Type, loc: SourceLocation)

  case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: KindedAst.TypeParam, tparams: List[KindedAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.RestrictableCaseSym, KindedAst.RestrictableCase], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[KindedAst.TypeParam], tpe: Type, loc: SourceLocation)

  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: KindedAst.TypeParam, kind: Kind, loc: SourceLocation)

  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[KindedAst.Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: KindedAst.Spec)

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends KindedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Sig(sym: Symbol.SigSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class HoleWithExp(exp: KindedAst.Expression, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class OpenAs(symUse: Ast.RestrictableEnumSymUse, exp: KindedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Use(sym: Symbol, alias: Name.Ident, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends KindedAst.Expression

    case class Apply(exp: KindedAst.Expression, exps: List[KindedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Lambda(fparam: KindedAst.FormalParam, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Unary(sop: SemanticOp, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Binary(sop: SemanticOp, exp1: KindedAst.Expression, exp2: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class IfThenElse(exp1: KindedAst.Expression, exp2: KindedAst.Expression, exp3: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Stm(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Discard(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends KindedAst.Expression

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp1: KindedAst.Expression, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ScopeExit(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Match(exp: KindedAst.Expression, rules: List[KindedAst.MatchRule], loc: SourceLocation) extends KindedAst.Expression

    case class TypeMatch(exp: KindedAst.Expression, rules: List[KindedAst.TypeMatchRule], loc: SourceLocation) extends KindedAst.Expression

    case class RelationalChoose(star: Boolean, exps: List[KindedAst.Expression], rules: List[KindedAst.RelationalChooseRule], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RestrictableChoose(star: Boolean, exp: KindedAst.Expression, rules: List[KindedAst.RestrictableChooseRule], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Tag(sym: Ast.CaseSymUse, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RestrictableTag(sym: Ast.RestrictableCaseSymUse, exp: KindedAst.Expression, isOpen: Boolean, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Tuple(elms: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends KindedAst.Expression

    case class RecordSelect(exp: KindedAst.Expression, field: Name.Field, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RecordExtend(field: Name.Field, value: KindedAst.Expression, rest: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLit(exps: List[KindedAst.Expression], exp: KindedAst.Expression, tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayNew(exp1: KindedAst.Expression, exp2: KindedAst.Expression, exp3: KindedAst.Expression, tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLoad(base: KindedAst.Expression, index: KindedAst.Expression, tpe: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayStore(base: KindedAst.Expression, index: KindedAst.Expression, elm: KindedAst.Expression, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLength(base: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class VectorLit(exps: List[KindedAst.Expression], tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class VectorLoad(exp1: KindedAst.Expression, exp2: KindedAst.Expression, tpe: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class VectorLength(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Ref(exp1: KindedAst.Expression, exp2: KindedAst.Expression, tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Deref(exp: KindedAst.Expression, tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Assign(exp1: KindedAst.Expression, exp2: KindedAst.Expression, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Ascribe(exp: KindedAst.Expression, expectedType: Option[Type], expectedPur: Option[Type], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class InstanceOf(exp: KindedAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends KindedAst.Expression

    case class CheckedCast(cast: Ast.CheckedCastType, exp: KindedAst.Expression, tvar: Type.Var, pvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class UncheckedCast(exp: KindedAst.Expression, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class UncheckedMaskingCast(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Without(exp: KindedAst.Expression, eff: Ast.EffectSymUse, loc: SourceLocation) extends KindedAst.Expression

    case class TryCatch(exp: KindedAst.Expression, rules: List[KindedAst.CatchRule], loc: SourceLocation) extends KindedAst.Expression

    case class TryWith(exp: KindedAst.Expression, eff: Ast.EffectSymUse, rules: List[KindedAst.HandlerRule], tvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Do(op: Ast.OpSymUse, args: List[KindedAst.Expression], tvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Resume(exp: KindedAst.Expression, argTvar: Type.Var, retTvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class InvokeMethod(method: Method, clazz: java.lang.Class[_], exp: KindedAst.Expression, args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class GetField(field: Field, clazz: java.lang.Class[_], exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class PutField(field: Field, clazz: java.lang.Class[_], exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class GetStaticField(field: Field, loc: SourceLocation) extends KindedAst.Expression

    case class PutStaticField(field: Field, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], methods: List[KindedAst.JvmMethod], loc: SourceLocation) extends KindedAst.Expression

    case class NewChannel(exp1: KindedAst.Expression, exp2: KindedAst.Expression, tvar: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class GetChannel(exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class PutChannel(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class SelectChannel(rules: List[KindedAst.SelectChannelRule], default: Option[KindedAst.Expression], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Spawn(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class ParYield(frags: List[KindedAst.ParYieldFragment], exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Lazy(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Force(exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointConstraintSet(cs: List[KindedAst.Constraint], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointLambda(pparams: List[KindedAst.PredicateParam], exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointMerge(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointSolve(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointInject(exp: KindedAst.Expression, pred: Name.Pred, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp1: KindedAst.Expression, exp2: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Error(m: CompilationMessage, tpe: Type.Var, eff: Type.Var) extends KindedAst.Expression {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends KindedAst.Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: KindedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Tuple(elms: List[KindedAst.Pattern], loc: SourceLocation) extends KindedAst.Pattern

    case class Record(pats: List[Record.RecordFieldPattern], pat: Option[Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    object Record {
      case class RecordFieldPattern(field: Name.Field, tpe: Option[Type], pat: Option[Pattern], loc: SourceLocation)
    }
  }

  sealed trait RelationalChoosePattern {
    def loc: SourceLocation
  }

  object RelationalChoosePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoosePattern

    case class Absent(loc: SourceLocation) extends RelationalChoosePattern

    case class Present(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends RelationalChoosePattern

  }

  sealed trait RestrictableChoosePattern {
    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: Ast.RestrictableCaseSymUse, pat: List[VarOrWild], tvar: Type.Var, loc: SourceLocation) extends RestrictableChoosePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends KindedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[KindedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Predicate.Head

    }

    sealed trait Body extends KindedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[KindedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Predicate.Body

      case class Guard(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Predicate.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[KindedAst.ConstraintParam], head: KindedAst.Predicate.Head, body: List[KindedAst.Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[KindedAst.FormalParam], exp: KindedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: KindedAst.Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[KindedAst.FormalParam], exp: KindedAst.Expression, tvar: Type.Var)

  case class RelationalChooseRule(pat: List[KindedAst.RelationalChoosePattern], exp: KindedAst.Expression)

  case class RestrictableChooseRule(pat: KindedAst.RestrictableChoosePattern, exp: KindedAst.Expression)

  case class MatchRule(pat: KindedAst.Pattern, guard: Option[KindedAst.Expression], exp: KindedAst.Expression)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: KindedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: KindedAst.Expression, exp: KindedAst.Expression)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: KindedAst.Pattern, exp: KindedAst.Expression, loc: SourceLocation)

}
