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
import ca.uwaterloo.flix.language.ast.Purity.Pure

object ReducedAst {

  val empty: Root = Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[FormalParam], fparams: List[FormalParam], stmt: Stmt, tpe: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, Case], tpeDeprecated: Type, loc: SourceLocation)

  sealed trait Expr {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[Expr], tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyClo(exp: Expr, exps: List[Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FormalParam], actuals: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: Expr, exp2: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  sealed trait Stmt {
    def tpe: Type

    def loc: SourceLocation
  }

  object Stmt {

    case class Ret(expr: Expr, tpe: Type, loc: SourceLocation) extends Stmt

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], clo: Expr, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

