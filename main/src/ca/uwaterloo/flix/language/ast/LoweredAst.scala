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

import ca.uwaterloo.flix.language.ast.Ast.EliminatedBy
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, Denotation, Doc, Fixity, Modifiers, Polarity, Source}
import ca.uwaterloo.flix.language.phase.Monomorpher
import ca.uwaterloo.flix.util.collection.ListMap

object LoweredAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Set.empty, Map.empty, Map.empty, ListMap.empty)

  case class Root(traits: Map[Symbol.TraitSym, Trait],
                  instances: Map[Symbol.TraitSym, List[Instance]],
                  sigs: Map[Symbol.SigSym, Sig],
                  defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffectSym, Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypeAlias],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  traitEnv: Map[Symbol.TraitSym, Ast.TraitContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])

  case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[Ast.TraitConstraint], assocs: List[AssocTypeSig], signatures: List[Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, trt: Ast.TraitSymUse, tpe: Type, tconstrs: List[Ast.TraitConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr)

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[Ast.TraitConstraint], loc: SourceLocation)

  case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Ast.Derivations, cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation)

  case class TypeAlias(doc: Doc, mod: Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Doc, mod: Modifiers, sym: Symbol.AssocTypeSym, tparam: TypedAst.TypeParam, kind: Kind, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Doc, mod: Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  sealed trait Expr extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    @EliminatedBy(Monomorpher.getClass)
    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyClo(exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(sym: Symbol.VarSym, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplySig(sym: Symbol.SigSym, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Cast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tpe: Type, pat: Pattern, loc: SourceLocation)
    }
  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], tpe: Type, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], tpe: Type, loc: SourceLocation) extends Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

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

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
