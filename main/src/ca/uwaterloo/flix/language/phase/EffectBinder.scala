/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.{Level, Purity, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.GenExpression
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * This phase transforms the AST such that all effect operations will happen on
  * an empty operand stack in [[GenExpression]]. This means that additional
  * let-bindings are introduced which means that [[Def.lparams]] must be updated
  * accordingly.
  *
  * An effect operation is either a Do or an application with a control effect,
  * i.e. an effect that's not just a region or IO. For now all calls are
  * considered to have an effect.
  *
  * Currently, this phase let-binds everything maximally, simplifying the
  * algorithm.
  */
object EffectBinder {

  /**
    * Transforms the AST such that effect operations will be run without an
    * operand stack.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EffectBinder") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    */
  private case class LocalContext(lparams: mutable.ArrayBuffer[LocalParam])

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = LocalContext(mutable.ArrayBuffer.empty)
  }

  /**
    * Transforms the [[Def]] such that effect operations will be run without an
    * operand stack.
    */
  private def visitDef(defn: Def)(implicit flix: Flix): Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val stmt = visitStmt(defn.stmt)
    defn.copy(stmt = stmt, lparams = defn.lparams ++ lctx.lparams.toList)
  }

  /**
    * Transforms the [[Stmt]] such that effect operations will be run without an
    * operand stack.
    */
  private def visitStmt(stmt: Stmt)(implicit lctx: LocalContext, flix: Flix): Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(visitExpr(expr), tpe, loc)
  }

  /**
    * Transforms the [[Expr]] such that effect operations will be run without an
    * operand stack - binding necessary expressions in the returned [[Expr]].
    */
  private def visitExpr(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = exp match {
    case Expr.Cst(_, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.Var(_, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.ApplyAtomic(_, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.ApplyClo(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.ApplyDef(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.ApplySelfTail(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.IfThenElse(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val branches1 = branches.map {
        case (sym, branchExp) => (sym, visitExpr(branchExp))
      }
      Expr.Branch(e, branches1, tpe, purity, loc)

    case Expr.JumpTo(_, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e = Expr.Let(sym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e = Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rules1 = rules.map {
        case cr => CatchRule(cr.sym, cr.clazz, visitExpr(cr.exp))
      }
      Expr.TryCatch(e, rules1, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rules1 = rules.map {
        case hr => hr.copy(exp = visitExpr(hr.exp))
      }
      Expr.TryWith(e, effUse, rules1, tpe, purity, loc)

    case Expr.Do(_, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.Resume(_, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case Expr.NewObject(_, _, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Expr]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)
  }

  /**
    * Transforms the [[Expr]] such that effect operations will be run without an
    * operand stack. The outer-most expression IS NOT let-bound but all
    * sub-expressions will be. `do E(x, y, z)` might be returned.
    *
    * Necessary bindings are added to binders, where the first binder is the
    * outermost one. Only [[Expr.Let]] and [[Expr.LetRec]] are added.
    */
  private def visitExprInnerWithBinders(binders: mutable.ArrayBuffer[Expr])(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(sym, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExprWithBinders(binders))
      Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = visitExprWithBinders(binders)(exp)
      val es = exps.map(visitExprWithBinders(binders))
      Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExprWithBinders(binders))
      Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExprWithBinders(binders))
      Expr.ApplySelfTail(sym, formals, as, tpe, purity, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches.map {
        case (sym, branchExp) => (sym, visitExpr(branchExp))
      }
      Expr.Branch(e, bs, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, exp1, exp2, _, _, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      binders.addOne(Expr.Let(sym, e1, null, null, null, loc))
      visitExprInnerWithBinders(binders)(exp2)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      binders.addOne(Expr.LetRec(varSym, index, defSym, e1, null, null, null, loc))
      visitExprInnerWithBinders(binders)(exp2)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      Expr.TryCatch(e, rules, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map {
        case HandlerRule(op, fparams, handlerExp) => HandlerRule(op, fparams, visitExpr(handlerExp))
      }
      Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExprWithBinders(binders))
      Expr.Do(op, es, tpe, purity, loc)

    case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
      val es = exps.map(visitExprInnerWithBinders(binders))
      Expr.NewObject(name, clazz, tpe, purity, methods, es, loc)

    case Expr.Resume(exp, tpe, loc) =>
      val e = visitExprWithBinders(binders)(exp)
      Expr.Resume(e, tpe, loc)
  }

  /**
    * Transforms the [[Expr]] such that effect operations will be run without an
    * operand stack. The outer-most expression IS let-bound along with all
    * sub-expressions. A variable or a constant is always returned.
    *
    * Necessary bindings are added to binders, where the first binder is the
    * outermost one. Only [[Expr.Let]] and [[Expr.LetRec]] are added.
    */
  private def visitExprWithBinders(binders: mutable.ArrayBuffer[Expr])(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = {
    /**
      * Let-binds the given expression, unless its a variable or constant.
      * If the given argument is a binder, then the structure is flattened.
      */
    @tailrec
    def bind(e: Expr): Expr = e match {
      // trivial expressions
      case Expr.Cst(_, _, _) => e
      case Expr.Var(_, _, _) => e
      case Expr.JumpTo(_, _, _, _) => e
      case Expr.ApplyAtomic(_, _, _, _, _) => e
      // non-trivial expressions
      case Expr.ApplyClo(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.ApplyDef(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.ApplySelfTail(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.IfThenElse(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.Branch(_, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.Let(sym, exp1, exp2, _, _, loc) =>
        binders.addOne(Expr.Let(sym, exp1, null, null, null, loc))
        bind(exp2)
      case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
        binders.addOne(Expr.LetRec(varSym, index, defSym, exp1, null, null, null, loc))
        bind(exp2)
      case Expr.Scope(_, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.TryCatch(_, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.TryWith(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.Do(_, _, _, _, _) => letBindExpr(binders)(e)
      case Expr.Resume(_, _, _) => letBindExpr(binders)(e)
      case Expr.NewObject(_, _, _, _, _, _, _) => letBindExpr(binders)(e)
    }
    bind(visitExprInnerWithBinders(binders)(exp))
  }

  /**
    * Simply let-binds the given expression, adding a [[Expr.Let]] to binders.
    * The local params of [[LocalContext]] is updated with this new binder.
    */
  private def letBindExpr(binders: mutable.ArrayBuffer[Expr])(e: Expr)(implicit lctx: LocalContext, flix: Flix): Expr.Var = {
    val loc = e.loc.asSynthetic
    val sym = Symbol.freshVarSym("anf", BoundBy.Let, loc)(Level.Default, flix)
    lctx.lparams.addOne(LocalParam(sym, e.tpe))
    binders.addOne(Expr.Let(sym, e, null, null, null, loc))
    Expr.Var(sym, e.tpe, loc)
  }

  /**
    * Returns an [[Expr]] where the given binders is a chained [[Expr.Let]]
    * expression. The first binder will be the outer-most one.
    *
    * The binders are expected to be either [[Expr.Let]] or [[Expr.LetRec]]
    * where the body of the binder is ignored.
    */
  private def bindBinders(binders: mutable.ArrayBuffer[Expr], exp: Expr): Expr = {
    binders.foldRight(exp) {
      case (Expr.Let(sym, exp1, _, _, _, loc), acc) =>
        Expr.Let(sym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
      case (Expr.LetRec(varSym, index, defSym, exp1, _, _, _, loc), acc) =>
        Expr.LetRec(varSym, index, defSym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
      case (other, _) => throw InternalCompilerException(s"Unexpected binder $other", other.loc)
    }
  }

  /**
    * Returns [[Purity.Pure]] if and only if both arguments are [[Purity.Pure]].
    */
  private def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Purity.Pure, Purity.Pure) => Purity.Pure
    case _ => Purity.Impure
  }

}
