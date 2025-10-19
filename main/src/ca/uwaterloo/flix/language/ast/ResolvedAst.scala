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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.util.collection.{ListMap, Nel}

import java.lang.reflect.Field

object ResolvedAst {

  val empty: Root = Root(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, ListMap.empty, List.empty, None, Map.empty, AvailableClasses.empty, Map.empty)

  case class Root(traits: Map[Symbol.TraitSym, Declaration.Trait],
                  instances: ListMap[Symbol.TraitSym, Declaration.Instance],
                  defs: Map[Symbol.DefnSym, Declaration.Def],
                  enums: Map[Symbol.EnumSym, Declaration.Enum],
                  structs: Map[Symbol.StructSym, Declaration.Struct],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, Declaration.RestrictableEnum],
                  effects: Map[Symbol.EffSym, Declaration.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, Declaration.TypeAlias],
                  uses: ListMap[Symbol.ModuleSym, UseOrImport],
                  taOrder: List[Symbol.TypeAliasSym],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  availableClasses: AvailableClasses,
                  tokens: Map[Source, Array[Token]])

  // TODO use Law for laws
  case class CompilationUnit(usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {
    case class Namespace(sym: Symbol.ModuleSym, usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation) extends Declaration

    case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[TraitConstraint], assocs: List[Declaration.AssocTypeSig], sigs: Map[Symbol.SigSym, Declaration.Sig], laws: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, symUse: TraitSymUse, tparams: List[TypeParam], tpe: UnkindedType, tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], assocs: List[Declaration.AssocTypeDef], defs: List[Declaration.Def], ns: Name.NName, loc: SourceLocation) extends Declaration

    case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Exp], loc: SourceLocation) extends Declaration

    case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Exp, loc: SourceLocation) extends Declaration

    case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Derivations, cases: List[Declaration.Case], loc: SourceLocation) extends Declaration

    case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation) extends Declaration

    case class StructField(mod: Modifiers, sym: Symbol.StructFieldSym, tpe: UnkindedType, loc: SourceLocation)

    case class RestrictableEnum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Derivations, cases: List[Declaration.RestrictableCase], loc: SourceLocation) extends Declaration

    case class Case(sym: Symbol.CaseSym, tpes: List[UnkindedType], loc: SourceLocation) extends Declaration

    case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpes: List[UnkindedType], loc: SourceLocation) extends Declaration

    case class TypeAlias(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class AssocTypeSig(doc: Doc, mod: Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[UnkindedType], loc: SourceLocation) extends Declaration

    case class AssocTypeDef(doc: Doc, mod: Modifiers, symUse: AssocTypeSymUse, arg: UnkindedType, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffSym, tparams: List[TypeParam], ops: List[Declaration.Op], loc: SourceLocation) extends Declaration

    case class Op(sym: Symbol.OpSym, spec: Spec, loc: SourceLocation) extends Declaration
  }

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], tpe: UnkindedType, eff: Option[UnkindedType], tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint])

  sealed trait Exp {
    def loc: SourceLocation
  }

  object Exp {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Exp

    case class Hole(sym: Symbol.HoleSym, scp: LocalScope, loc: SourceLocation) extends Exp

    case class HoleWithExp(exp: Exp, scp: LocalScope, loc: SourceLocation) extends Exp

    case class OpenAs(symUse: RestrictableEnumSymUse, exp: Exp, loc: SourceLocation) extends Exp

    case class Use(sym: Symbol, alias: Name.Ident, exp: Exp, loc: SourceLocation) extends Exp

    case class Cst(cst: Constant, loc: SourceLocation) extends Exp

    case class ApplyClo(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class ApplyDef(symUse: DefSymUse, exps: List[Exp], loc: SourceLocation) extends Exp

    case class ApplyLocalDef(symUse: LocalDefSymUse, exps: List[Exp], loc: SourceLocation) extends Exp

    case class ApplyOp(symUse: OpSymUse, exps: List[Exp], loc: SourceLocation) extends Exp

    case class ApplySig(symUse: SigSymUse, exps: List[Exp], loc: SourceLocation) extends Exp

    case class Lambda(fparam: FormalParam, exp: Exp, allowSubeffecting: Boolean, loc: SourceLocation) extends Exp

    case class Unary(sop: SemanticOp.UnaryOp, exp: Exp, loc: SourceLocation) extends Exp

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class IfThenElse(exp1: Exp, exp2: Exp, exp3: Exp, loc: SourceLocation) extends Exp

    case class Stm(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class Discard(exp: Exp, loc: SourceLocation) extends Exp

    case class Let(sym: Symbol.VarSym, exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class Region(sym: Symbol.VarSym, regSym: Symbol.RegionSym, exp: Exp, loc: SourceLocation) extends Exp

    case class Match(exp: Exp, rules: List[MatchRule], loc: SourceLocation) extends Exp

    case class TypeMatch(exp: Exp, rules: List[TypeMatchRule], loc: SourceLocation) extends Exp

    case class RestrictableChoose(star: Boolean, exp: Exp, rules: List[RestrictableChooseRule], loc: SourceLocation) extends Exp

    case class ExtMatch(exp: Exp, rules: List[ExtMatchRule], loc: SourceLocation) extends Exp

    case class Tag(symUse: CaseSymUse, exps: List[Exp], loc: SourceLocation) extends Exp

    case class RestrictableTag(symUse: RestrictableCaseSymUse, exps: List[Exp], isOpen: Boolean, loc: SourceLocation) extends Exp

    case class ExtTag(label: Name.Label, exps: List[Exp], loc: SourceLocation) extends Exp

    case class Tuple(exps: List[Exp], loc: SourceLocation) extends Exp

    case class RecordSelect(exp: Exp, label: Name.Label, loc: SourceLocation) extends Exp

    case class RecordExtend(label: Name.Label, value: Exp, rest: Exp, loc: SourceLocation) extends Exp

    case class RecordRestrict(label: Name.Label, rest: Exp, loc: SourceLocation) extends Exp

    case class ArrayLit(exps: List[Exp], exp: Exp, loc: SourceLocation) extends Exp

    case class ArrayNew(exp1: Exp, exp2: Exp, exp3: Exp, loc: SourceLocation) extends Exp

    case class ArrayLoad(base: Exp, index: Exp, loc: SourceLocation) extends Exp

    case class ArrayStore(base: Exp, index: Exp, elm: Exp, loc: SourceLocation) extends Exp

    case class ArrayLength(base: Exp, loc: SourceLocation) extends Exp

    case class StructNew(sym: Symbol.StructSym, exps: List[(StructFieldSymUse, Exp)], region: Exp, loc: SourceLocation) extends Exp

    case class StructGet(exp: Exp, symUse: StructFieldSymUse, loc: SourceLocation) extends Exp

    case class StructPut(exp1: Exp, symUse: StructFieldSymUse, exp2: Exp, loc: SourceLocation) extends Exp

    case class VectorLit(exps: List[Exp], loc: SourceLocation) extends Exp

    case class VectorLoad(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class VectorLength(exp: Exp, loc: SourceLocation) extends Exp

    case class Ascribe(exp: Exp, expectedType: Option[UnkindedType], expectedEff: Option[UnkindedType], loc: SourceLocation) extends Exp

    case class InstanceOf(exp: Exp, clazz: java.lang.Class[?], loc: SourceLocation) extends Exp

    case class CheckedCast(cast: CheckedCastType, exp: Exp, loc: SourceLocation) extends Exp

    case class UncheckedCast(exp: Exp, declaredType: Option[UnkindedType], declaredEff: Option[UnkindedType], loc: SourceLocation) extends Exp

    case class Unsafe(exp: Exp, eff: UnkindedType, loc: SourceLocation) extends Exp

    case class Without(exp: Exp, symUse: EffSymUse, loc: SourceLocation) extends Exp

    case class TryCatch(exp: Exp, rules: List[CatchRule], loc: SourceLocation) extends Exp

    case class Throw(exp: Exp, loc: SourceLocation) extends Exp

    case class Handler(symUse: EffSymUse, rules: List[HandlerRule], loc: SourceLocation) extends Exp

    case class RunWith(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class InvokeConstructor(clazz: Class[?], exps: List[Exp], loc: SourceLocation) extends Exp

    case class InvokeMethod(exp: Exp, methodName: Name.Ident, exps: List[Exp], loc: SourceLocation) extends Exp

    case class InvokeStaticMethod(clazz: Class[?], methodName: Name.Ident, exps: List[Exp], loc: SourceLocation) extends Exp

    case class GetField(exp: Exp, fieldName: Name.Ident, loc: SourceLocation) extends Exp

    case class PutField(field: Field, clazz: java.lang.Class[?], exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class GetStaticField(field: Field, loc: SourceLocation) extends Exp

    case class PutStaticField(field: Field, exp: Exp, loc: SourceLocation) extends Exp

    case class NewObject(name: String, clazz: java.lang.Class[?], methods: List[JvmMethod], loc: SourceLocation) extends Exp

    case class NewChannel(exp: Exp, loc: SourceLocation) extends Exp

    case class GetChannel(exp: Exp, loc: SourceLocation) extends Exp

    case class PutChannel(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Exp], loc: SourceLocation) extends Exp

    case class Spawn(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class ParYield(frags: List[ParYieldFragment], exp: Exp, loc: SourceLocation) extends Exp

    case class Lazy(exp: Exp, loc: SourceLocation) extends Exp

    case class Force(exp: Exp, loc: SourceLocation) extends Exp

    case class FixpointConstraintSet(cs: List[Constraint], loc: SourceLocation) extends Exp

    case class FixpointLambda(pparams: List[PredicateParam], exp: Exp, loc: SourceLocation) extends Exp

    case class FixpointMerge(exp1: Exp, exp2: Exp, loc: SourceLocation) extends Exp

    case class FixpointQueryWithProvenance(exps: List[Exp], select: Predicate.Head, withh: List[Name.Pred], loc: SourceLocation) extends Exp

    case class FixpointQueryWithSelect(exps: List[Exp], queryExp: Exp, selects: List[Exp], from: List[Predicate.Body], where: List[Exp], pred: Name.Pred, loc: SourceLocation) extends Exp

    case class FixpointSolveWithProject(exps: List[Exp], optPreds: Option[List[Name.Pred]], mode: SolveMode, loc: SourceLocation) extends Exp

    case class FixpointInjectInto(exps: List[Exp], predsAndArities: List[PredicateAndArity], loc: SourceLocation) extends Exp

    case class Error(m: CompilationMessage) extends Exp {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, loc: SourceLocation) extends Pattern

    case class Tag(symUse: CaseSymUse, pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Tuple(pats: Nel[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, loc: SourceLocation) extends Pattern

    case class Error(loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, loc: SourceLocation)
    }

  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends VarOrWild

    case class Tag(symUse: RestrictableCaseSymUse, pats: List[VarOrWild], loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

  }

  sealed trait ExtPattern {
    def loc: SourceLocation
  }

  object ExtPattern {

    case class Default(loc: SourceLocation) extends ExtPattern

    case class Tag(label: Name.Label, pats: List[ExtTagPattern], loc: SourceLocation) extends ExtPattern

    case class Error(loc: SourceLocation) extends ExtPattern

  }

  sealed trait ExtTagPattern {
    def loc: SourceLocation
  }

  object ExtTagPattern {

    case class Wild(loc: SourceLocation) extends ExtTagPattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends ExtTagPattern

    case class Unit(loc: SourceLocation) extends ExtTagPattern

    case class Error(loc: SourceLocation) extends ExtTagPattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Exp], loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], loc: SourceLocation) extends Predicate.Body

      case class Functional(syms: List[Symbol.VarSym], exp: Exp, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Exp, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, tpe: Option[UnkindedType], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Denotation, tpes: List[UnkindedType], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Exp, tpe: UnkindedType, eff: Option[UnkindedType], loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Exp, loc: SourceLocation)

  case class HandlerRule(symUse: OpSymUse, fparams: List[FormalParam], exp: Exp, loc: SourceLocation)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Exp)

  case class MatchRule(pat: Pattern, guard: Option[Exp], exp: Exp, loc: SourceLocation)

  case class ExtMatchRule(pat: ExtPattern, exp: Exp, loc: SourceLocation)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: UnkindedType, exp: Exp, loc: SourceLocation)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Exp, exp: Exp, loc: SourceLocation)

  sealed trait TypeParam {
    val name: Name.Ident
    val sym: Symbol.UnkindedTypeVarSym
  }

  object TypeParam {
    case class Kinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

    case class Implicit(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam
  }

  case class TraitConstraint(symUse: TraitSymUse, tpe: UnkindedType, loc: SourceLocation)

  case class EqualityConstraint(assocTypeSymUse: AssocTypeSymUse, tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Exp, loc: SourceLocation)

}
