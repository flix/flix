/*
 * Copyright 2020 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.Purity.Pure

object LiftedAst {

  val empty: Root = Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, LiftedAst.Def],
                  enums: Map[Symbol.EnumSym, LiftedAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[LiftedAst.FormalParam], fparams: List[LiftedAst.FormalParam], exp: LiftedAst.Expr, tpe: Type, purity: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, LiftedAst.Case], tpe: Type, loc: SourceLocation)

  sealed trait Expr {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends LiftedAst.Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends LiftedAst.Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[LiftedAst.Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class ApplyClo(exp: LiftedAst.Expr, exps: List[LiftedAst.Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[LiftedAst.Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[LiftedAst.FormalParam], actuals: List[LiftedAst.Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class IfThenElse(exp1: LiftedAst.Expr, exp2: LiftedAst.Expr, exp3: LiftedAst.Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, LiftedAst.Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class Let(sym: Symbol.VarSym, exp1: LiftedAst.Expr, exp2: LiftedAst.Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: LiftedAst.Expr, exp2: LiftedAst.Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class Scope(sym: Symbol.VarSym, exp: LiftedAst.Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class TryCatch(exp: LiftedAst.Expr, rules: List[LiftedAst.CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class TryWith(exp: LiftedAst.Expr, effUse: Ast.EffectSymUse, rules: List[LiftedAst.HandlerRule], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class Do(op: Ast.OpSymUse, exps: List[LiftedAst.Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends LiftedAst.Expr

    case class Resume(exp: LiftedAst.Expr, tpe: Type, loc: SourceLocation) extends LiftedAst.Expr {
      def purity: Purity = Pure
    }

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[LiftedAst.JvmMethod], loc: SourceLocation) extends LiftedAst.Expr

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[LiftedAst.FormalParam], clo: LiftedAst.Expr, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: LiftedAst.Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[LiftedAst.FormalParam], exp: LiftedAst.Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

