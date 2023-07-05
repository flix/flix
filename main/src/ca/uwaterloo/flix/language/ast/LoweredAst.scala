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

object LoweredAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, Map.empty, ListMap.empty)

  case class Root(classes: Map[Symbol.ClassSym, Class],
                  instances: Map[Symbol.ClassSym, List[Instance]],
                  sigs: Map[Symbol.SigSym, Sig],
                  defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  effects: Map[Symbol.EffectSym, Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypeAlias],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  classEnv: Map[Symbol.ClassSym, Ast.ClassContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])

  case class Class(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: TypeParam, superClasses: List[Ast.TypeConstraint], assocs: List[AssocTypeSig], signatures: List[Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, clazz: Ast.ClassSymUse, tpe: Type, tconstrs: List[Ast.TypeConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: Spec, impl: Option[Impl])

  case class Def(sym: Symbol.DefnSym, spec: Spec, impl: Impl)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[Ast.TypeConstraint], loc: SourceLocation)

  case class Impl(exp: Expression, inferredScheme: Scheme)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: List[Ast.Derivation], cases: Map[Symbol.CaseSym, Case], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypedAst.TypeParam, kind: Kind, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  sealed trait Expression extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    @EliminatedBy(Monomorph.getClass)
    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    case class Lambda(fparam: FormalParam, exp: Expression, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    case class Apply(exp: Expression, exps: List[Expression], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class ApplyAtomic(op: AtomicOp, exps: List[Expression], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expression, exp2: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expression, exp2: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class IfThenElse(exp1: Expression, exp2: Expression, exp3: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Stm(exp1: Expression, exp2: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Discard(exp: Expression, eff: Type, loc: SourceLocation) extends Expression {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expression, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class TypeMatch(exp: Expression, rules: List[TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class RelationalChoose(exps: List[Expression], rules: List[RelationalChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class VectorLit(exps: List[Expression], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class VectorLoad(exp1: Expression, exp2: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class VectorLength(exp: Expression, loc: SourceLocation) extends Expression {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expression, tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Cast(exp: Expression, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class TryCatch(exp: Expression, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class TryWith(exp: Expression, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Do(op: Ast.OpSymUse, exps: List[Expression], tpe: Type, eff: Type, loc: SourceLocation) extends Expression

    case class Resume(exp: Expression, tpe: Type, loc: SourceLocation) extends Expression {
      def eff: Type = Type.Pure
    }

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expression

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(elms: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

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

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expression], tpe: Type, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, fixity: Ast.Fixity, terms: List[Pattern], tpe: Type, loc: SourceLocation) extends Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: Expression, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expression, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Attribute(name: String, tpe: Type, loc: SourceLocation)

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expression, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expression)

  case class RelationalChooseRule(pat: List[RelationalChoosePattern], exp: Expression)

  case class MatchRule(pat: Pattern, guard: Option[Expression], exp: Expression)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expression, exp: Expression)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expression, loc: SourceLocation)

}
