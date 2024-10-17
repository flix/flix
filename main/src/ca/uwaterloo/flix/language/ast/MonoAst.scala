/*
  * Copyright 2024 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.shared.SymUse.{CaseSymUse, EffectSymUse, OpSymUse}
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, Denotation, Doc, Fixity, Modifiers, Polarity, Source}
import ca.uwaterloo.flix.language.phase.Monomorpher

object MonoAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, None, Set.empty, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffectSym, Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr, loc: SourceLocation)

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, fparams: List[FormalParam], functionType: Type, retTpe: Type, eff: Type)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec, loc: SourceLocation)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[Symbol.KindedTypeVarSym], fields: List[StructField], loc: SourceLocation)

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

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyClo(exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(sym: Symbol.VarSym, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Cast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

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

    case class Tag(sym: CaseSymUse, pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tpe: Type, pat: Pattern, loc: SourceLocation)
    }
  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
