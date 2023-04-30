/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Purity.{Impure, Pure}

object ControlAst {

  case class Root(defs: Map[Symbol.DefnSym, ControlAst.Def],
                  enums: Map[Symbol.EnumSym, ControlAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[ControlAst.FormalParam], exp: ControlAst.Expression, tpe: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, ControlAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def purity : Purity

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Pure
    }

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[ControlAst.Expression], tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ApplyClo(exp: ControlAst.Expression, args: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ApplyCloTail(exp: ControlAst.Expression, args: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[ControlAst.FormalParam], actuals: List[ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class IfThenElse(exp1: ControlAst.Expression, exp2: ControlAst.Expression, exp3: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, ControlAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ControlAst.Expression, exp2: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: ControlAst.Expression, exp2: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Pure
    }

    case class Scope(sym: Symbol.VarSym, exp: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class ScopeExit(exp1: ControlAst.Expression, exp2: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class InstanceOf(exp: ControlAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = exp.purity

      def tpe: Type = Type.Bool
    }

    case class Cast(exp: ControlAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class TryCatch(exp: ControlAst.Expression, rules: List[ControlAst.CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends ControlAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[ControlAst.JvmMethod], loc: SourceLocation) extends ControlAst.Expression

    case class Spawn(exp1: ControlAst.Expression, exp2: ControlAst.Expression, tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Impure
    }

    case class Lazy(exp: ControlAst.Expression, tpe: Type, loc: SourceLocation) extends ControlAst.Expression  {
      def purity: Purity = Pure
    }

    case class Force(exp: ControlAst.Expression, tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Pure
    }

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Impure
    }

    case class MatchError(tpe: Type, loc: SourceLocation) extends ControlAst.Expression {
      def purity: Purity = Impure
    }

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[ControlAst.FormalParam], clo: ControlAst.Expression, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ControlAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

