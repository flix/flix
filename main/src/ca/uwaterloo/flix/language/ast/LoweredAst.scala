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

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import java.lang.reflect.{Constructor, Field, Method}

object LoweredAst {

  case class Root(classes: Map[Symbol.ClassSym, LoweredAst.Class],
                  instances: Map[Symbol.ClassSym, List[LoweredAst.Instance]],
                  sigs: Map[Symbol.SigSym, LoweredAst.Sig],
                  defs: Map[Symbol.DefnSym, LoweredAst.Def],
                  enums: Map[Symbol.EnumSym, LoweredAst.Enum],
                  effects: Map[Symbol.EffectSym, LoweredAst.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, LoweredAst.TypeAlias],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  classEnv: Map[Symbol.ClassSym, Ast.ClassContext])

  case class Class(doc: Ast.Doc, ann: List[LoweredAst.Annotation], mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: LoweredAst.TypeParam, superClasses: List[Ast.TypeConstraint], signatures: List[LoweredAst.Sig], laws: List[LoweredAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: List[LoweredAst.Annotation], mod: Ast.Modifiers, sym: Symbol.InstanceSym, tpe: Type, tconstrs: List[Ast.TypeConstraint], defs: List[LoweredAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: LoweredAst.Spec, impl: Option[LoweredAst.Impl])

  case class Def(sym: Symbol.DefnSym, spec: LoweredAst.Spec, impl: LoweredAst.Impl)

  case class Spec(doc: Ast.Doc, ann: List[LoweredAst.Annotation], mod: Ast.Modifiers, tparams: List[LoweredAst.TypeParam], fparams: List[LoweredAst.FormalParam], declaredScheme: Scheme, retTpe: Type, pur: Type, eff: Type, tconstrs: List[Ast.TypeConstraint], loc: SourceLocation)

  case class Impl(exp: LoweredAst.Expression, inferredScheme: Scheme)

  case class Enum(doc: Ast.Doc, ann: List[LoweredAst.Annotation], mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[LoweredAst.TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.CaseSym, LoweredAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[LoweredAst.TypeParam], tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: List[LoweredAst.Annotation], mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[LoweredAst.Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: LoweredAst.Spec)

  sealed trait Expression extends Product {
    def tpe: Type

    def pur: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Wild(tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Hole(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Lambda(fparam: LoweredAst.FormalParam, exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Apply(exp: LoweredAst.Expression, exps: List[LoweredAst.Expression], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Unary(sop: SemanticOperator, exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Binary(sop: SemanticOperator, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class IfThenElse(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, exp3: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Stm(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Discard(exp: LoweredAst.Expression, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: LoweredAst.Expression, rules: List[LoweredAst.MatchRule], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TypeMatch(exp: LoweredAst.Expression, rules: List[LoweredAst.MatchTypeRule], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Choose(exps: List[LoweredAst.Expression], rules: List[LoweredAst.ChoiceRule], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Tag(sym: Ast.CaseSymUse, exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Tuple(elms: List[LoweredAst.Expression], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class RecordSelect(exp: LoweredAst.Expression, field: Name.Field, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordExtend(field: Name.Field, value: LoweredAst.Expression, rest: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class RecordRestrict(field: Name.Field, rest: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLit(exps: List[LoweredAst.Expression], exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayNew(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, exp3: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLoad(base: LoweredAst.Expression, index: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class ArrayLength(base: LoweredAst.Expression, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.Int32
    }

    case class ArrayStore(base: LoweredAst.Expression, index: LoweredAst.Expression, elm: LoweredAst.Expression, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.Unit
    }

    case class ArraySlice(base: LoweredAst.Expression, beginIndex: LoweredAst.Expression, endIndex: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Ref(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Deref(exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Assign(exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Ascribe(exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Cast(exp: LoweredAst.Expression, declaredType: Option[Type], declaredPur: Option[Type], declaredEff: Option[Type], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Upcast(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      override def pur: Type = exp.pur

      override def eff: Type = exp.eff
    }

    case class Supercast(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      override def pur: Type = exp.pur

      override def eff: Type = exp.eff
    }

    case class Without(exp: LoweredAst.Expression, effUse: Ast.EffectSymUse, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TryCatch(exp: LoweredAst.Expression, rules: List[LoweredAst.CatchRule], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class TryWith(exp: LoweredAst.Expression, effUse: Ast.EffectSymUse, rules: List[LoweredAst.HandlerRule], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Do(op: Ast.OpSymUse, exps: List[LoweredAst.Expression], pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def tpe: Type = Type.Unit
    }

    case class Resume(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class InvokeConstructor(constructor: Constructor[_], args: List[LoweredAst.Expression], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class InvokeMethod(method: Method, exp: LoweredAst.Expression, args: List[LoweredAst.Expression], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[LoweredAst.Expression], tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class GetField(field: Field, exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class PutField(field: Field, exp1: LoweredAst.Expression, exp2: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class GetStaticField(field: Field, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class PutStaticField(field: Field, exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, pur: Type, eff: Type, methods: List[LoweredAst.JvmMethod], loc: SourceLocation) extends LoweredAst.Expression

    case class Spawn(exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

    case class Lazy(exp: LoweredAst.Expression, tpe: Type, loc: SourceLocation) extends LoweredAst.Expression {
      def pur: Type = Type.Pure

      def eff: Type = Type.Empty
    }

    case class Force(exp: LoweredAst.Expression, tpe: Type, pur: Type, eff: Type, loc: SourceLocation) extends LoweredAst.Expression

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

    case class Array(elms: List[LoweredAst.Pattern], tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class ArrayTailSpread(elms: List[LoweredAst.Pattern], sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: List[LoweredAst.Pattern], tpe: Type, loc: SourceLocation) extends LoweredAst.Pattern

  }

  sealed trait ChoicePattern {
    def loc: SourceLocation
  }

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ChoicePattern

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

      case class Guard(exp: LoweredAst.Expression, loc: SourceLocation) extends LoweredAst.Predicate.Body

      case class Loop(varSyms: List[Symbol.VarSym], exp: LoweredAst.Expression, loc: SourceLocation) extends LoweredAst.Predicate.Body

    }

  }

  case class Annotation(name: Ast.Annotation, args: List[LoweredAst.Expression], loc: SourceLocation)

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

  case class JvmMethod(ident: Name.Ident, fparams: List[LoweredAst.FormalParam], exp: LoweredAst.Expression, retTpe: Type, pur: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: LoweredAst.Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[LoweredAst.FormalParam], exp: LoweredAst.Expression)

  case class ChoiceRule(pat: List[LoweredAst.ChoicePattern], exp: LoweredAst.Expression)

  case class MatchRule(pat: LoweredAst.Pattern, guard: Option[LoweredAst.Expression], exp: LoweredAst.Expression)

  case class MatchTypeRule(sym: Symbol.VarSym, tpe: Type, exp: LoweredAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: LoweredAst.Expression, exp: LoweredAst.Expression)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: LoweredAst.Pattern, exp: LoweredAst.Expression, loc: SourceLocation)

}
