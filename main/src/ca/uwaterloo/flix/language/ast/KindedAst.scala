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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.util.collection.{ListMap, Nel}

import java.lang.reflect.Field

object KindedAst {

  val empty: Root = Root(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, ListMap.empty, None, Map.empty, AvailableClasses.empty, Map.empty)

  case class Root(traits: Map[Symbol.TraitSym, Trait],
                  instances: ListMap[Symbol.TraitSym, Instance],
                  defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, RestrictableEnum],
                  effects: Map[Symbol.EffSym, Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypeAlias],
                  uses: ListMap[Symbol.ModuleSym, UseOrImport],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  availableClasses: AvailableClasses,
                  tokens: Map[Source, Array[Token]])

  case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[TraitConstraint], assocs: List[AssocTypeSig], sigs: Map[Symbol.SigSym, Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, symUse: TraitSymUse, tparams: List[TypeParam], tpe: Type, tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Exp], loc: SourceLocation)

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Exp, loc: SourceLocation)

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], sc: Scheme, tpe: Type, eff: Type, tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint])

  case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Derivations, cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], sc: Scheme, fields: List[StructField], loc: SourceLocation)

  case class RestrictableEnum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Derivations, cases: Map[Symbol.RestrictableCaseSym, RestrictableCase], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  case class AssocTypeSig(doc: Doc, mod: Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

  case class AssocTypeDef(doc: Doc, mod: Modifiers, symUse: AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffSym, tparams: List[TypeParam], ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec, loc: SourceLocation)

  sealed trait Exp {
    def loc: SourceLocation
  }

  object Exp {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Exp

    case class Hole(sym: Symbol.HoleSym, scp: LocalScope, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class HoleWithExp(exp: Exp, scp: LocalScope, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class OpenAs(symUse: RestrictableEnumSymUse, exp: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class Use(sym: Symbol, alias: Name.Ident, exp: Exp, loc: SourceLocation) extends Exp

    case class Cst(cst: Constant, loc: SourceLocation) extends Exp

    case class ApplyClo(exp1: Exp, exp2: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ApplyDef(symUse: DefSymUse, exps: List[Exp], targs: List[Type.Var], itvar: Type, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ApplyLocalDef(symUse: LocalDefSymUse, exps: List[Exp], arrowTvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ApplyOp(symUse: OpSymUse, exps: List[Exp], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ApplySig(symUse: SigSymUse, exps: List[Exp], targ: Type.Var, targs: List[Type.Var], itvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class Lambda(fparam: FormalParam, exp: Exp, allowSubeffecting: Boolean, loc: SourceLocation) extends Exp

    case class Unary(sop: SemanticOp.UnaryOp, exp: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Exp, exp2: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class IfThenElse(exp1: Exp, exp2: Exp, exp3: Exp, loc: SourceLocation) extends Exp

    case class Stm(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class Discard(exp: Exp, loc: SourceLocation) extends Exp

    case class Let(sym: Symbol.VarSym, exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class Region(sym: Symbol.VarSym, regSym: Symbol.RegionSym, exp1: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class Match(exp: Exp, rules: List[MatchRule], loc: SourceLocation) extends Exp

    case class TypeMatch(exp: Exp, rules: List[TypeMatchRule], loc: SourceLocation) extends Exp

    case class RestrictableChoose(star: Boolean, exp: Exp, rules: List[RestrictableChooseRule], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class ExtMatch(exp: Exp, rules: List[ExtMatchRule], loc: SourceLocation) extends Exp

    case class Tag(symUse: CaseSymUse, exps: List[Exp], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class RestrictableTag(symUse: RestrictableCaseSymUse, exps: List[Exp], isOpen: Boolean, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ExtTag(label: Name.Label, exps: List[Exp], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class Tuple(exps: List[Exp], loc: SourceLocation) extends Exp

    case class RecordSelect(exp: Exp, label: Name.Label, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class RecordExtend(label: Name.Label, value: Exp, rest: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class RecordRestrict(label: Name.Label, rest: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class ArrayLit(exps: List[Exp], exp: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ArrayNew(exp1: Exp, exp2: Exp, exp3: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ArrayLoad(base: Exp, index: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ArrayStore(base: Exp, index: Exp, elm: Exp, evar: Type.Var, loc: SourceLocation) extends Exp

    case class ArrayLength(base: Exp, evar: Type.Var, loc: SourceLocation) extends Exp

    case class StructNew(sym: Symbol.StructSym, fields: List[(StructFieldSymUse, Exp)], region: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class StructGet(exp: Exp, symUse: StructFieldSymUse, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class StructPut(exp1: Exp, symUse: StructFieldSymUse, exp2: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class VectorLit(exps: List[Exp], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class VectorLoad(exp1: Exp, exp2: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class VectorLength(exp: Exp, loc: SourceLocation) extends Exp

    case class Ascribe(exp: Exp, expectedType: Option[Type], expectedPur: Option[Type], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class InstanceOf(exp: Exp, clazz: java.lang.Class[?], loc: SourceLocation) extends Exp

    case class CheckedCast(cast: CheckedCastType, exp: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class UncheckedCast(exp: Exp, declaredType: Option[Type], declaredEff: Option[Type], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class Unsafe(exp: Exp, eff: Type, loc: SourceLocation) extends Exp

    case class Without(exp: Exp, symUse: EffSymUse, loc: SourceLocation) extends Exp

    case class TryCatch(exp: Exp, rules: List[CatchRule], loc: SourceLocation) extends Exp

    case class Throw(exp: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class Handler(symUse: EffSymUse, rules: List[HandlerRule], tvar: Type.Var, evar1: Type.Var, evar2: Type.Var, loc: SourceLocation) extends Exp

    case class RunWith(exp1: Exp, exp2: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class InvokeConstructor(clazz: Class[?], exps: List[Exp], jvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class InvokeMethod(exp: Exp, methodName: Name.Ident, exps: List[Exp], jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class InvokeStaticMethod(clazz: Class[?], methodName: Name.Ident, exps: List[Exp], jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class GetField(exp: Exp, fieldName: Name.Ident, jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class PutField(field: Field, clazz: java.lang.Class[?], exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class GetStaticField(field: Field, loc: SourceLocation) extends Exp

    case class PutStaticField(field: Field, exp: Exp, loc: SourceLocation) extends Exp

    case class NewObject(name: String, clazz: java.lang.Class[?], methods: List[JvmMethod], loc: SourceLocation) extends Exp

    case class NewChannel(exp: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class GetChannel(exp: Exp, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class PutChannel(exp1: Exp, exp2: Exp, evar: Type.Var, loc: SourceLocation) extends Exp

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Exp], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class Spawn(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class ParYield(frags: List[ParYieldFragment], exp: Exp, loc: SourceLocation) extends Exp

    case class Lazy(exp: Exp, loc: SourceLocation) extends Exp

    case class Force(exp: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class FixpointConstraintSet(cs: List[Constraint], tvar: Type.Var, loc: SourceLocation) extends Exp

    case class FixpointLambda(pparams: List[PredicateParam], exp: Exp, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class FixpointMerge(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class FixpointQueryWithProvenance(exps: List[Exp], select: Predicate.Head, withh: List[Name.Pred], tvar: Type, loc: SourceLocation) extends Exp

    case class FixpointQueryWithSelect(exps: List[Exp], queryExp: Exp, selects: List[Exp], from: List[Predicate.Body], where: List[Exp], pred: Name.Pred, tvar: Type, loc: SourceLocation) extends Exp

    case class FixpointSolveWithProject(exps: List[Exp], optPreds: Option[List[Name.Pred]], mode: SolveMode, tvar: Type.Var, loc: SourceLocation) extends Exp

    case class FixpointInjectInto(exps: List[Exp], predsAndArities: List[PredicateAndArity], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Exp

    case class Error(m: CompilationMessage, tvar: Type.Var, evar: Type.Var) extends Exp {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, loc: SourceLocation) extends Pattern

    case class Tag(symUse: CaseSymUse, pats: List[Pattern], tvar: Type.Var, loc: SourceLocation) extends Pattern

    case class Tuple(pats: Nel[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, tvar: Type.Var, loc: SourceLocation) extends Pattern

    case class Error(tvar: Type.Var, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, tvar: Type.Var, loc: SourceLocation)
    }
  }

  sealed trait RestrictableChoosePattern {
    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Tag(symUse: RestrictableCaseSymUse, pat: List[VarOrWild], tvar: Type.Var, loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(tvar: Type.Var, loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

  }

  sealed trait ExtPattern {
    def loc: SourceLocation
  }

  object ExtPattern {

    case class Default(tvar: Type.Var, loc: SourceLocation) extends ExtPattern

    case class Tag(label: Name.Label, pats: List[ExtTagPattern], loc: SourceLocation) extends ExtPattern

    case class Error(tvar: Type.Var, loc: SourceLocation) extends ExtPattern

  }

  sealed trait ExtTagPattern {
    def loc: SourceLocation
  }

  object ExtTagPattern {

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends ExtTagPattern

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends ExtTagPattern

    case class Unit(loc: SourceLocation) extends ExtTagPattern

    case class Error(tvar: Type.Var, loc: SourceLocation) extends ExtTagPattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Exp], tvar: Type.Var, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], tvar: Type.Var, loc: SourceLocation) extends Predicate.Body

      case class Functional(syms: List[Symbol.VarSym], exp: Exp, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Exp, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Case(sym: Symbol.CaseSym, tpes: List[Type], sc: Scheme, loc: SourceLocation)

  case class StructField(mod: Modifiers, sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpes: List[Type], sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, tpe: Type, src: TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Exp, tpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Exp, loc: SourceLocation)

  case class HandlerRule(symUse: OpSymUse, fparams: List[FormalParam], exp: Exp, tvar: Type.Var, loc: SourceLocation)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Exp)

  case class MatchRule(pat: Pattern, guard: Option[Exp], exp: Exp, loc: SourceLocation)

  case class ExtMatchRule(pat: ExtPattern, exp: Exp, loc: SourceLocation)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Exp, loc: SourceLocation)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Exp, exp: Exp, loc: SourceLocation)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Exp, loc: SourceLocation)

}
