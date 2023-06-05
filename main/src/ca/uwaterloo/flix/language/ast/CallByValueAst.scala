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

object CallByValueAst {

  val empty: Root = Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, CallByValueAst.Def],
                  enums: Map[Symbol.EnumSym, CallByValueAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[CallByValueAst.FormalParam], fparams: List[CallByValueAst.FormalParam], stmt: CallByValueAst.Stmt, tpe: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, CallByValueAst.Case], tpeDeprecated: Type, loc: SourceLocation)

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

    /**
      * Conceptually, we want `exp` and `rules` to be control-pure. It is very
      * hard to do this with the structural transformation. So while these can
      * be statements, we expect these statements to be control-pure. This means
      * that
      *
      * `try { (x -> x + 1)(4+5) } catch { case e: Exception => ... }`
      *
      * becomes
      *
      * `try { letval cbv0 = (x -> x + 1); letval cbv1 = (4+5); cbv0(cbv1) } catch { case e: Exception => ... }`
      *
      * and likewise in the handler body
      */
    case class TryCatch(exp: Stmt, rules: List[CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, /* no control effects */ methods: List[JvmMethod], loc: SourceLocation) extends Expr

    // TODO: Minus all the applies stuff in Intrinsic.
    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr


    // Introduce run for force/lazy

    // TODO: Enforce spawn e, e must be control pure. Put spawn here. (Wrap into run?)

  }

  sealed trait Stmt {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Stmt {

    case class Ret(exp: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class IfThenElse(exp: Expr, stmt1: Stmt, stmt2: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class Branch(stmt: Stmt, branches: Map[Symbol.LabelSym, Stmt], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetVal(sym: Symbol.VarSym, stmt1: Stmt, stmt2: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp: Expr, stmt: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class Scope(sym: Symbol.VarSym, stmt: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyClo(exp: Expr, exps: List[Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], ct: Ast.CallType, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[CallByValueAst.FormalParam], actuals: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[CallByValueAst.FormalParam], clo: CallByValueAst.Expr, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: CallByValueAst.Stmt)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

