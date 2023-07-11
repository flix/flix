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

/**
  * We have two kinds of code terms: expressions and statements.
  *
  * Expressions must not have control effects, i.e. expressions do not have unhandled `do`'s.
  *
  * Statements may have control effects.
  *
  * In relation to the current (for this AST) effect system, expressions can be both pure and impure
  * and statements can also be both.
  */
object CallByValueAst {

  val empty: Root = Root(Map.empty, Map.empty, List.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, CallByValueAst.Def],
                  enums: Map[Symbol.EnumSym, CallByValueAst.Enum],
                  anonClasses: List[AnonClass],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[CallByValueAst.FormalParam], fparams: List[CallByValueAst.FormalParam], stmt: CallByValueAst.Stmt, tpe: MonoType, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, CallByValueAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)

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

    /**
      * This is not algebraic effect try, but java exception try.
      *
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
      * and likewise in the catch case body
      */
    case class TryCatch(exp: Stmt, rules: List[CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, purity: Purity, /* no control effects */ methods: List[JvmMethodImpl], loc: SourceLocation) extends Expr

    // TODO: Minus all the applies stuff in Intrinsic.
    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr


    // `stmt` must be control pure. There must not be algebraic effects "across" run.
    // `run` could be omited but is used to limit the time spent in the cps monad.
//    case class Run(stmt: Stmt, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr


    // TODO: force/lazy (using run)

    // TODO: Enforce spawn e, e must be control pure. Put spawn here. Wrap into run

  }

  sealed trait Stmt {
    def tpe: MonoType

    def purity: Purity

    def loc: SourceLocation
  }

  object Stmt {

    case class Ret(exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class IfThenElse(exp: Expr, stmt1: Stmt, stmt2: Stmt, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class Branch(stmt: Stmt, branches: Map[Symbol.LabelSym, Stmt], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetVal(sym: Symbol.VarSym, stmt1: Stmt, stmt2: Stmt, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp: Expr, stmt: Stmt, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class Scope(sym: Symbol.VarSym, stmt: Stmt, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyClo(exp: Expr, exps: List[Expr], ct: Ast.CallType, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], ct: Ast.CallType, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[CallByValueAst.FormalParam], actuals: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Stmt

    // TODO: add DO and Try-With

  }

  case class AnonClass(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[JvmMethodSpec], loc: SourceLocation)

  case class JvmMethodSpec(ident: Name.Ident, fparams: List[FormalParam], tpe: MonoType, purity: Purity, loc: SourceLocation)

  case class JvmMethodImpl(ident: Name.Ident, fparams: List[FormalParam], clo: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation)
  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: CallByValueAst.Stmt)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: MonoType, loc: SourceLocation)

}

