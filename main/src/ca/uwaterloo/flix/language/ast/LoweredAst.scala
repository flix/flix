/*
 * Copyright 2022 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, EliminatedBy, Source}
import ca.uwaterloo.flix.language.phase.Monomorph
import ca.uwaterloo.flix.util.collection.ListMap

import java.lang.reflect.{Constructor, Field, Method}

object LoweredAst {

  val empty: Root = LoweredAst.Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, Map.empty, ListMap.empty)

  case class Root(classes: Map[Symbol.ClassSym, LoweredAst.Class],
                  instances: Map[Symbol.ClassSym, List[LoweredAst.Instance]],
                  sigs: Map[Symbol.SigSym, LoweredAst.Sig],
                  defs: Map[Symbol.DefnSym, LoweredAst.Def],
                  enums: Map[Symbol.EnumSym, LoweredAst.Enum],
                  effects: Map[Symbol.EffectSym, LoweredAst.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, LoweredAst.TypeAlias],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  classEnv: Map[Symbol.ClassSym, Ast.ClassContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])

  case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: LoweredAst.TypeParam, superClasses: List[Ast.TypeConstraint], assocs: List[LoweredAst.AssocTypeSig], signatures: List[LoweredAst.Sig], laws: List[LoweredAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Ast.ClassSymUse, tpe: Type, tconstrs: List[Ast.TypeConstraint], assocs: List[LoweredAst.AssocTypeDef], defs: List[LoweredAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: LoweredAst.Spec, impl: Option[LoweredAst.Impl])

  case class Def(sym: Symbol.DefnSym, spec: LoweredAst.Spec, impl: LoweredAst.Impl)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[LoweredAst.TypeParam], fparams: List[LoweredAst.FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[Ast.TypeConstraint], loc: SourceLocation)

  case class Impl(exp: LoweredAst.Expression, inferredScheme: Scheme)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[LoweredAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.CaseSym, LoweredAst.Case], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[LoweredAst.TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypedAst.TypeParam, kind: Kind, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[LoweredAst.Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: LoweredAst.Spec)

  sealed trait Expression extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    @EliminatedBy(Monomorph.getClass)
    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Hole(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Lambda(fparam: LoweredAst.FormalParam, exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Apply(exp: LoweredAst.Expression, exps: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Unary(sop: SemanticOp, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Binary(sop: SemanticOp, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ScopeExit(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class IfThenElse(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, exp3: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Stm(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Discard(exp: LoweredAst.Expression, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: LoweredAst.Expression, rules: List[LoweredAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TypeMatch(exp: LoweredAst.Expression, rules: List[LoweredAst.TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RelationalChoose(exps: List[LoweredAst.Expression], rules: List[LoweredAst.RelationalChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Tag(sym: Ast.CaseSymUse, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Tuple(exps: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class RecordSelect(exp: LoweredAst.Expression, field: Name.Field, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordExtend(field: Name.Field, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordRestrict(field: Name.Field, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLit(exps: List[LoweredAst.Expression], exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayNew(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, exp3: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLoad(base: LoweredAst.Expression, index: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLength(base: LoweredAst.Expression, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.Int32
    }

    case class ArrayStore(base: LoweredAst.Expression, index: LoweredAst.Expression, elm: LoweredAst.Expression, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.Unit
    }

    case class VectorLit(exps: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class VectorLoad(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class VectorLength(exp: LoweredAst.Expression, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ref(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Deref(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Assign(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Ascribe(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class InstanceOf(exp: LoweredAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = exp.eff

      def tpe: Type = Type.Bool
    }

    case class Cast(exp: LoweredAst.Expression, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TryCatch(exp: LoweredAst.Expression, rules: List[LoweredAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TryWith(exp: LoweredAst.Expression, effUse: Ast.EffectSymUse, rules: List[LoweredAst.HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Do(op: Ast.OpSymUse, exps: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Resume(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class InvokeConstructor(constructor: Constructor[_], args: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class InvokeMethod(method: Method, exp: LoweredAst.Expression, args: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[LoweredAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class GetField(field: Field, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class PutField(field: Field, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class PutStaticField(field: Field, exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, eff: Type, methods: List[LoweredAst.JvmMethod], loc: SourceLocation) extends LoweredAst.Expression

    case class Spawn(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Lazy(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def eff: Type = Type.Pure
    }

    case class Force(exp: LoweredAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: LoweredAst.Pattern, tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class Tuple(elms: List[LoweredAst.Pattern], tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

  }

  sealed trait RelationalChoosePattern {
    def loc: SourceLocation
  }

  object RelationalChoosePattern {

    case class Wild(loc: SourceLocation) extends RelationalChoosePattern

    case class Absent(loc: SourceLocation) extends RelationalChoosePattern

    case class Present(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends RelationalChoosePattern

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends LoweredAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[LoweredAst.Expression], tpe: Type, loc: SourceLocation) extends LoweredAst.Predicate.Head

    }

    sealed trait Body extends LoweredAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[LoweredAst.Pattern], tpe: Type, loc: SourceLocation) extends LoweredAst.Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: LoweredAst.Expression, loc: SourceLocation) extends LoweredAst.Predicate.Body

      case class Guard(exp: LoweredAst.Expression, loc: SourceLocation) extends LoweredAst.Predicate.Body

    }

  }

  case class Attribute(name: String, tpe: Type, loc: SourceLocation)

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[LoweredAst.ConstraintParam], head: LoweredAst.Predicate.Head, body: List[LoweredAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[LoweredAst.FormalParam], exp: LoweredAst.Expression, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: LoweredAst.Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[LoweredAst.FormalParam], exp: LoweredAst.Expression)

  case class RelationalChooseRule(pat: List[LoweredAst.RelationalChoosePattern], exp: LoweredAst.Expression)

  case class MatchRule(pat: LoweredAst.Pattern, guard: Option[LoweredAst.Expression], exp: LoweredAst.Expression)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: LoweredAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: LoweredAst.Expression, exp: LoweredAst.Expression)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: LoweredAst.Pattern, exp: LoweredAst.Expression, loc: SourceLocation)

}
