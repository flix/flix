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
import ca.uwaterloo.flix.util.collection.{ListMap, MultiMap}

import java.lang.reflect.{Constructor, Field, Method}

object TypedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, Map.empty, ListMap.empty, MultiMap.empty)

  case class Root(modules: Map[Symbol.ModuleSym, List[Symbol]],
                  classes: Map[Symbol.ClassSym, TypedAst.Class],
                  instances: Map[Symbol.ClassSym, List[TypedAst.Instance]],
                  sigs: Map[Symbol.SigSym, TypedAst.Sig],
                  defs: Map[Symbol.DefnSym, TypedAst.Def],
                  enums: Map[Symbol.EnumSym, TypedAst.Enum],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum],
                  effects: Map[Symbol.EffectSym, TypedAst.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypedAst.TypeAlias],
                  uses: Map[Symbol.ModuleSym, List[Ast.UseOrImport]],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  classEnv: Map[Symbol.ClassSym, Ast.ClassContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef],
                  names: MultiMap[List[String], String])

  case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: TypedAst.TypeParam, superClasses: List[Ast.TypeConstraint], assocs: List[TypedAst.AssocTypeSig], signatures: List[TypedAst.Sig], laws: List[TypedAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Ast.ClassSymUse, tpe: Type, tconstrs: List[Ast.TypeConstraint], assocs: List[TypedAst.AssocTypeDef], defs: List[TypedAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: TypedAst.Spec, impl: Option[TypedAst.Impl])

  case class Def(sym: Symbol.DefnSym, spec: TypedAst.Spec, impl: TypedAst.Impl)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[Ast.TypeConstraint], loc: SourceLocation)

  case class Impl(exp: TypedAst.Expression, inferredScheme: Scheme)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypedAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.CaseSym, TypedAst.Case], tpe: Type, loc: SourceLocation)

  case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: TypedAst.TypeParam, tparams: List[TypedAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.RestrictableCaseSym, TypedAst.RestrictableCase], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypedAst.TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypedAst.TypeParam, kind: Kind, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[TypedAst.Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: TypedAst.Spec)

  sealed trait Expression extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Hole(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class HoleWithExp(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class OpenAs(symUse: Ast.RestrictableEnumSymUse, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = exp.eff
    }

    case class Use(sym: Symbol, alias: Name.Ident, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = exp.tpe

      def eff: Type = exp.eff
    }

    case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Apply(exp: TypedAst.Expression, exps: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Unary(sop: SemanticOp, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Binary(sop: SemanticOp, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ScopeExit(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Discard(exp: TypedAst.Expression, eff: Type, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class TypeMatch(exp: TypedAst.Expression, rules: List[TypedAst.TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RelationalChoose(exps: List[TypedAst.Expression], rules: List[TypedAst.RelationalChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RestrictableChoose(star: Boolean, exp: TypedAst.Expression, rules: List[TypedAst.RestrictableChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tag(sym: Ast.CaseSymUse, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RestrictableTag(sym: Ast.RestrictableCaseSymUse, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class RecordSelect(exp: TypedAst.Expression, field: Name.Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordExtend(field: Name.Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class RecordRestrict(field: Name.Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLit(exps: List[TypedAst.Expression], exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayNew(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLoad(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ArrayLength(exp: TypedAst.Expression, eff: Type, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Int32
    }

    case class ArrayStore(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, eff: Type, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Unit
    }

    case class VectorLit(exps: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLoad(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class VectorLength(exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ref(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InstanceOf(exp: TypedAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = exp.eff

      def tpe: Type = Type.Bool
    }

    case class CheckedCast(cast: Ast.CheckedCastType, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class UncheckedCast(exp: TypedAst.Expression, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class UncheckedMaskingCast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Without(exp: TypedAst.Expression, effUse: Ast.EffectSymUse, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class TryWith(exp: TypedAst.Expression, effUse: Ast.EffectSymUse, rules: List[TypedAst.HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Do(op: Ast.OpSymUse, exps: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Resume(exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, eff: Type, methods: List[TypedAst.JvmMethod], loc: SourceLocation) extends TypedAst.Expression

    case class NewChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Spawn(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class ParYield(frags: List[TypedAst.ParYieldFragment], exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Lazy(exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Force(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointConstraintSet(cs: List[TypedAst.Constraint], stf: Ast.Stratification, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
      def eff: Type = Type.Pure
    }

    case class FixpointLambda(pparams: List[TypedAst.PredicateParam], exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointMerge(exp1: TypedAst.Expression, exp2: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointInject(exp: TypedAst.Expression, pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case class Error(m: CompilationMessage, tpe: Type, eff: Type) extends TypedAst.Expression {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: TypedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    case class Tuple(elms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

  }

  sealed trait RelationalChoosePattern {
    def loc: SourceLocation
  }

  object RelationalChoosePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoosePattern

    case class Absent(loc: SourceLocation) extends RelationalChoosePattern

    case class Present(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends RelationalChoosePattern

  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: Ast.RestrictableCaseSymUse, pat: List[VarOrWild], tpe: Type, loc: SourceLocation) extends RestrictableChoosePattern

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends TypedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    sealed trait Body extends TypedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[TypedAst.Pattern], tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Predicate.Body

      case class Guard(exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  case class Attribute(name: String, tpe: Type, loc: SourceLocation)

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[TypedAst.ConstraintParam], head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: TypedAst.Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[TypedAst.FormalParam], exp: TypedAst.Expression)

  case class RelationalChooseRule(pat: List[TypedAst.RelationalChoosePattern], exp: TypedAst.Expression)

  case class RestrictableChooseRule(pat: TypedAst.RestrictableChoosePattern, exp: TypedAst.Expression)

  case class MatchRule(pat: TypedAst.Pattern, guard: Option[TypedAst.Expression], exp: TypedAst.Expression)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: TypedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: TypedAst.Expression, exp: TypedAst.Expression)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: TypedAst.Pattern, exp: TypedAst.Expression, loc: SourceLocation)

}
