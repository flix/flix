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
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, CallType}
import ca.uwaterloo.flix.language.ast.{LiftedAst, ReducedAst}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, VarSym}
import ca.uwaterloo.flix.language.ast.{AtomicOp, Level, Purity, SemanticOp, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.GenExpression
import ca.uwaterloo.flix.util.ParOps

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * This phase transforms the AST such that all effect operations will happen on
  * an empty operand stack in [[GenExpression]].
  *
  * The number of "pc points" must be counted, which is the number of points
  * where a continuation will be used. This includes do operations, all calls
  * except tail recursive self-calls, and try-with expressions.
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
  def run(root: LiftedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("EffectBinder") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    */
  private case class LocalContext(var pcPoints: Int)

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = LocalContext(0)
  }

  private sealed trait Binder

  private case class LetBinder(sym: VarSym, exp: ReducedAst.Expr, loc: SourceLocation) extends Binder

  private case class LetRecBinder(varSym: VarSym, index: Int, defSym: DefnSym, exp: ReducedAst.Expr, loc: SourceLocation) extends Binder

  /**
    * Transforms the [[LiftedAst.Def]] such that effect operations will be run without an
    * operand stack.
    */
  private def visitDef(defn: LiftedAst.Def)(implicit flix: Flix): ReducedAst.Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val exp = visitExpr(defn.exp)
    defn.copy(exp = exp, pcPoints = lctx.pcPoints)
  }

  private def visitJvmMethod(method: LiftedAst.JvmMethod)(implicit lctx: LocalContext, flix: Flix): ReducedAst.JvmMethod = method match {
    case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      // JvmMethods are generated as their own functions so let-binding do not
      // span across
      ReducedAst.JvmMethod(ident, fparams, visitExpr(clo), retTpe, purity, loc)
  }

  /**
    * Transforms the [[LiftedAst.Expr]] such that effect operations will be run without an
    * operand stack - binding necessary expressions in the returned [[ReducedAst.Expr]].
    *
    * Updates [[LocalContext.pcPoints]] only for expressions not given to [[visitExprInnerWithBinders]].
    */
  private def visitExpr(exp: LiftedAst.Expr)(implicit lctx: LocalContext, flix: Flix): ReducedAst.Expr = exp match {
    case LiftedAst.Expr.Cst(_, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.Var(_, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.ApplyAtomic(_, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.ApplyClo(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.ApplyDef(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.ApplySelfTail(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.IfThenElse(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val branches1 = branches.map {
        case (sym, branchExp) => (sym, visitExpr(branchExp))
      }
      ReducedAst.Expr.Branch(e, branches1, tpe, purity, loc)

    case LiftedAst.Expr.JumpTo(_, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e = ReducedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case LiftedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e = ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case LiftedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rules1 = rules.map {
        case cr => ReducedAst.CatchRule(cr.sym, cr.clazz, visitExpr(cr.exp))
      }
      ReducedAst.Expr.TryCatch(e, rules1, tpe, purity, loc)

    case LiftedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      lctx.pcPoints += 1 // added here since visitInner will not see this try-with
      val e = visitExpr(exp)
      val rs = rules.map(visitHandlerRule)
      val rs = rules.map{
        case LiftedAst.HandlerRule(op, fparams, exp) =>
          val fs = fparams.map(visitParam)
          val e = visitExpr(exp)
          ReducedAst.HandlerRule(op, fs, e)
      }
//      val rules1 = rules.map {
//        case hr => hr.copy(exp = visitExpr(hr.exp))
//      }
      ReducedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case LiftedAst.Expr.Do(_, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)

    case LiftedAst.Expr.NewObject(_, _, _, _, _, _) =>
      val binders = mutable.ArrayBuffer.empty[Binder]
      val e = visitExprInnerWithBinders(binders)(exp)
      bindBinders(binders, e)
  }

  /**
    * Transforms the [[LiftedAst.Expr]] such that effect operations will be run without an
    * operand stack. The outer-most expression IS NOT let-bound but all
    * sub-expressions will be. `do E(x, y, z)` might be returned.
    *
    * Necessary bindings are added to binders, where the first binder is the
    * outermost one.
    *
    * Updates [[LocalContext.pcPoints]] as required.
    */
  private def visitExprInnerWithBinders(binders: mutable.ArrayBuffer[Binder])(exp: LiftedAst.Expr)(implicit lctx: LocalContext, flix: Flix): ReducedAst.Expr = exp match {
    case LiftedAst.Expr.Cst(cst, tpe, loc) =>
      ReducedAst.Expr.Cst(cst, tpe, loc)

    case LiftedAst.Expr.Var(sym, tpe, loc) =>
      ReducedAst.Expr.Var(sym, tpe, loc)

    case LiftedAst.Expr.ApplyAtomic(op@AtomicOp.Binary(SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or), exps, tpe, purity, loc) =>
      // And and Or does not leave the first argument on the stack in genExpression.
      val List(exp1, exp2) = exps
      val e1 = visitExprWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExprWithBinders(binders))
      ReducedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      if (ct == CallType.NonTailCall && purity != Purity.Pure) lctx.pcPoints += 1
      val e = visitExprWithBinders(binders)(exp)
      val es = exps.map(visitExprWithBinders(binders))
      ReducedAst.Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      if (ct == CallType.NonTailCall && purity != Purity.Pure) lctx.pcPoints += 1
      val es = exps.map(visitExprWithBinders(binders))
      ReducedAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExprWithBinders(binders))
      val fs = formals.map(visitParam)
      ReducedAst.Expr.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case LiftedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches.map {
        case (sym, branchExp) => (sym, visitExpr(branchExp))
      }
      ReducedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expr.Let(sym, exp1, exp2, _, _, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      binders.addOne(LetBinder(sym, e1, loc))
      visitExprInnerWithBinders(binders)(exp2)

    case LiftedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
      val e1 = visitExprInnerWithBinders(binders)(exp1)
      binders.addOne(LetRecBinder(varSym, index, defSym, e1, loc))
      visitExprInnerWithBinders(binders)(exp2)

    case LiftedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map{
        case LiftedAst.CatchRule(sym, clazz, exp) =>
          ReducedAst.CatchRule(sym, clazz, visitExpr(exp))
      }
      ReducedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      lctx.pcPoints += 1
      val e = visitExpr(exp)
      val rs = rules.map {
        case LiftedAst.HandlerRule(op, fparams, handlerExp) =>
          val fs = fparams.map(visitParam)
          ReducedAst.HandlerRule(op, fs, visitExpr(handlerExp))
      }
      ReducedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case LiftedAst.Expr.Do(op, exps, tpe, purity, loc) =>
      lctx.pcPoints += 1
      val es = exps.map(visitExprWithBinders(binders))
      ReducedAst.Expr.Do(op, es, tpe, purity, loc)

    case LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      ReducedAst.Expr.NewObject(name, clazz, tpe, purity, ms, ???, loc)
  }

  /**
    * Transforms the [[LiftedAst.Expr]] such that effect operations will be run without an
    * operand stack. The outer-most expression IS let-bound along with all
    * sub-expressions. A variable or a constant is always returned.
    *
    * Necessary bindings are added to binders, where the first binder is the
    * outermost one.
    */
  private def visitExprWithBinders(binders: mutable.ArrayBuffer[Binder])(exp: LiftedAst.Expr)(implicit lctx: LocalContext, flix: Flix): ReducedAst.Expr = {
    /**
      * Let-binds the given expression, unless its a variable or constant.
      * If the given argument is a binder, then the structure is flattened.
      */
    @tailrec
    def bind(e: ReducedAst.Expr): ReducedAst.Expr = e match {
      // trivial expressions
      case ReducedAst.Expr.Cst(_, _, _) => e
      case ReducedAst.Expr.Var(_, _, _) => e
      case ReducedAst.Expr.JumpTo(_, _, _, _) => e
      case ReducedAst.Expr.ApplyAtomic(_, _, _, _, _) => e
      // non-trivial expressions
      case ReducedAst.Expr.ApplyClo(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.ApplyDef(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.ApplySelfTail(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.IfThenElse(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.Branch(_, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.Let(sym, exp1, exp2, _, _, loc) =>
        binders.addOne(LetBinder(sym, exp1, loc))
        bind(exp2)
      case ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
        binders.addOne(LetRecBinder(varSym, index, defSym, exp1, loc))
        bind(exp2)
      case ReducedAst.Expr.Scope(_, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.TryCatch(_, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.TryWith(_, _, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.Do(_, _, _, _, _) => letBindExpr(binders)(e)
      case ReducedAst.Expr.NewObject(_, _, _, _, _, _, _) => letBindExpr(binders)(e)
    }

    bind(visitExprInnerWithBinders(binders)(exp))
  }

  /**
    * Simply let-binds the given expression, adding a [[ReducedAst.Expr.Let]] to binders.
    * The local params of [[LocalContext]] is updated with this new binder.
    */
  private def letBindExpr(binders: mutable.ArrayBuffer[Binder])(e: ReducedAst.Expr)(implicit flix: Flix): ReducedAst.Expr.Var = {
    val loc = e.loc.asSynthetic
    val sym = Symbol.freshVarSym("anf", BoundBy.Let, loc)(Level.Default, flix)
    binders.addOne(LetBinder(sym, e, loc))
    ReducedAst.Expr.Var(sym, e.tpe, loc)
  }

  /**
    * Returns an [[ReducedAst.Expr]] where the given binders is a chained [[ReducedAst.Expr.Let]]
    * expression. The first binder will be the outer-most one.
    */
  private def bindBinders(binders: mutable.ArrayBuffer[Binder], exp: ReducedAst.Expr): ReducedAst.Expr = {
    binders.foldRight(exp) {
      case (LetBinder(sym, exp1, loc), acc) =>
        ReducedAst.Expr.Let(sym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
      case (LetRecBinder(varSym, index, defSym, exp1, loc), acc) =>
        ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
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
