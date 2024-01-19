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
import ca.uwaterloo.flix.language.phase.MonoDefs
import ca.uwaterloo.flix.util.collection.ListMap

object LoweredNormalAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, None, Set.empty, Map.empty, ListMap.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, LoweredAst.Enum],
                  effects: Map[Symbol.EffectSym, Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, fparams: List[FormalParam], scheme: Scheme, retTpe: NormalType, eff: NormalType, loc: SourceLocation)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, Case], tpe: NormalType, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  sealed trait Expr extends Product {
    def tpe: NormalType

    def eff: NormalType

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: NormalType, loc: SourceLocation) extends Expr {
      def eff: NormalType = NormalType.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: NormalType, loc: SourceLocation) extends Expr {
      def eff: NormalType = NormalType.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: NormalType, loc: SourceLocation) extends Expr {
      def eff: NormalType = NormalType.Pure
    }

    @EliminatedBy(MonoDefs.getClass)
    case class Sig(sym: Symbol.SigSym, tpe: NormalType, loc: SourceLocation) extends Expr {
      def eff: NormalType = NormalType.Pure
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: NormalType, loc: SourceLocation) extends Expr {
      def eff: NormalType = NormalType.Pure
    }

    case class Apply(exp: Expr, exps: List[Expr], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Symbol.KindedTypeVarSym, exp: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: NormalType, loc: SourceLocation) extends Expr {
      def tpe: NormalType = NormalType.Cst(TypeConstructor.Unit)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: NormalType = exp.eff

      def tpe: NormalType = NormalType.Cst(TypeConstructor.Int32)
    }

    case class Ascribe(exp: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Cast(exp: Expr, tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: NormalType, eff: NormalType, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: NormalType, eff: NormalType, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  sealed trait Pattern {
    def tpe: NormalType

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: NormalType, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: NormalType, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, tpe: NormalType, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tpe: NormalType, loc: SourceLocation) extends Pattern

    case class Tuple(elms: List[Pattern], tpe: NormalType, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, tpe: NormalType, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: NormalType, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tpe: NormalType, pat: Pattern, loc: SourceLocation)
    }
  }

  case class Attribute(name: String, tpe: NormalType, loc: SourceLocation)

  case class Case(sym: Symbol.CaseSym, tpe: NormalType, enumType: NormalType, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: NormalType, src: Ast.TypeSource, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: NormalType, eff: NormalType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
