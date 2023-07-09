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

import ca.uwaterloo.flix.language.ast.Ast.{IntroducedBy, Source}
import ca.uwaterloo.flix.language.ast.Purity.Pure
import ca.uwaterloo.flix.language.phase.ClosureConv

object SimplifiedAst {

  val empty: Root = SimplifiedAst.Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, SimplifiedAst.Def],
                  enums: Map[Symbol.EnumSym, SimplifiedAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, purity: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, SimplifiedAst.Case], tpe: Type, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Lambda(fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class LambdaClosure(cparams: List[SimplifiedAst.FormalParam], fparams: List[SimplifiedAst.FormalParam], freeVars: List[FreeVar], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: SimplifiedAst.Expression, exps: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyDef(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Scope(sym: Symbol.VarSym, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class TryCatch(exp: SimplifiedAst.Expression, rules: List[SimplifiedAst.CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class TryWith(exp: SimplifiedAst.Expression, effUse: Ast.EffectSymUse, rules: List[SimplifiedAst.HandlerRule], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Do(op: Ast.OpSymUse, exps: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Resume(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[SimplifiedAst.JvmMethod], loc: SourceLocation) extends SimplifiedAst.Expression

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: SimplifiedAst.Expression)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type) extends Ordered[FreeVar] {
    override def compare(that: FreeVar): Int = this.sym.compare(that.sym)
  }

}
