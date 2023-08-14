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

  val empty: Root = Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[FormalParam], exp: Expr, tpe: MonoType, purity: MonoType, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, Case], tpe: MonoType, loc: SourceLocation)

  sealed trait Expr {
    def tpe: MonoType

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Lambda(fparams: List[FormalParam], exp: Expr, tpe: MonoType.Arrow, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Apply(exp: Expr, args: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    @IntroducedBy(ClosureConv.getClass)
    case class LambdaClosure(cparams: List[FormalParam], fparams: List[FormalParam], freeVars: List[FreeVar], exp: Expr, tpe: MonoType.Arrow, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: Expr, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyDef(sym: Symbol.DefnSym, args: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Resume(exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, purity: Purity, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  case class Case(sym: Symbol.CaseSym, tpe: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: MonoType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: MonoType, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType) extends Ordered[FreeVar] {
    override def compare(that: FreeVar): Int = this.sym.compare(that.sym)
  }

}
