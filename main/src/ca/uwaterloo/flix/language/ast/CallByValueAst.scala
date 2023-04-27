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
import ca.uwaterloo.flix.language.ast.Purity.{Impure, Pure}

import java.lang.reflect.{Constructor, Field, Method}

object CallByValueAst {

  case class Root(defs: Map[Symbol.DefnSym, CallByValueAst.Def],
                  enums: Map[Symbol.EnumSym, CallByValueAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[CallByValueAst.FormalParam], exp: CallByValueAst.Expr, tpe: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, CallByValueAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  sealed trait Expr {
    def tpe: Type
    def loc: SourceLocation
  }
  object Expr {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Expr
    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, /* require body to be pure */ rules: List[CatchRule], tpe: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, /* no control effects */ methods: List[JvmMethod], loc: SourceLocation) extends Expr

    // TODO: Minus all the applies stuff in Intrinsic.
    case class ApplyPure(op: IntrinsicOperator, exps: List[Expr], tpe: Type, loc: SourceLocation) extends Expr

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[Expr], tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }
    // Introduce run for force/lazy

    // TODO: Enforce spawn e, e must be control pure. Put spawn here. (Wrap into run?)

  }

  sealed trait Stmt

  object Stmt {

    case class Ret(exp: Expr, tpe: Type, loc: SourceLocation) extends Stmt

    case class IfThenElse(exp: Expr, stmt1: Stmt, stmt2: Stmt, tpe: Type, loc: SourceLocation) extends Stmt

    case class Branch(stmt: Stmt, branches: Map[Symbol.LabelSym, Stmt], tpe: Type, loc: SourceLocation) extends Stmt

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends Stmt

    case class LetVal(sym: Symbol.VarSym, stmt1: Stmt, stmt2: Stmt, tpe: Type, loc: SourceLocation) extends Stmt

    case class LetRecVal(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp: Expr, stmt: Stmt, tpe: Type, loc: SourceLocation) extends Stmt

    case class Scope(sym: Symbol.VarSym, stmt: Stmt, tpe: Type, loc: SourceLocation) extends Stmt

    case class ApplyClo(exp: Expr, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyCloTail(exp: Expr, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[LiftedAst.FormalParam], actuals: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class Do(sym: Symbol.OpSym, exps: List[Expr], tpe: Type, loc: SourceLocation) extends Stmt

    case class Handle(sym: Symbol.OpSym, stmt: Stmt, loc: SourceLocation) extends Stmt

  }

  sealed trait IntrinsicOperator

  object IntrinsicOperator {
    case object ArrayLength
    // ...
  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[CallByValueAst.FormalParam], clo: CallByValueAst.Expression, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: CallByValueAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

