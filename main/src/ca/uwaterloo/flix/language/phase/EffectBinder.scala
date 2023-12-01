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
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable


object EffectBinder {

  /**
    * Identity Function.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EffectBinder") {
    val newDefs = ParOps.parMapValues(root.defs)(letBindEffectsDef)
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
    * Identity Function.
    */
  private def letBindEffectsDef(defn: Def)(implicit flix: Flix): Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val stmt = letBindEffectsStmt(defn.stmt)
    defn.copy(stmt = stmt, lparams = defn.lparams ++ lctx.lparams.toList)
  }

  /**
    * Identity Function.
    */
  private def letBindEffectsStmt(stmt: Stmt)(implicit lctx: LocalContext, flix: Flix): Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(letBindEffectsTopLevel(expr), tpe, loc)
  }

  /**
    * Let-binds sub-expressions inside the returned expression.
    */
  private def letBindEffectsTopLevel(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = exp match {
    case Expr.Cst(_, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.Var(_, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.ApplyAtomic(_, _, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.ApplyClo(_, _, _, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.ApplyDef(_, _, _, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.ApplySelfTail(_, _, _, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.IfThenElse(_, _, _, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val branches1 = branches.map {
        case (sym, branchExp) => (sym, letBindEffectsTopLevel(branchExp))
      }
      Expr.Branch(e, branches1, tpe, purity, loc)

    case Expr.JumpTo(_, _, _, _) =>
      bindBinders(letBindEffectsTopLevelAux(exp))

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (binders, e1) = letBindEffectsTopLevelAux(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      val e = Expr.Let(sym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val (binders, e1) = letBindEffectsTopLevelAux(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      val e = Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
      bindBinders(binders, e)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rules1 = rules.map {
        case cr => CatchRule(cr.sym, cr.clazz, letBindEffectsTopLevel(cr.exp))
      }
      Expr.TryCatch(e, rules1, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rules1 = rules.map {
        case hr => hr.copy(exp = letBindEffectsTopLevel(hr.exp))
      }
      Expr.TryWith(e, effUse, rules1, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      bindBinders(letBindEffectsTopLevelAux(Expr.Do(op, exps, tpe, purity, loc)))

    case Expr.Resume(exp, tpe, loc) =>
      bindBinders(letBindEffectsTopLevelAux(Expr.Resume(exp, tpe, loc)))

    case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
      bindBinders(letBindEffectsTopLevelAux(Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc)))
  }

  private def letBindEffectsTopLevelAux(exp: Expr)(implicit lctx: LocalContext, flix: Flix): (mutable.ArrayBuffer[Expr], Expr) = {
    val binders = mutable.ArrayBuffer.empty[Expr]
    val e = letBindInnerEffects(binders)(exp)
    (binders, e)
  }

  private def bindBinders(t: (mutable.ArrayBuffer[Expr], Expr)): Expr=  {
    // the right-most binder is the closest one, so we fold from the right
    val (binders, e) = t
    binders.foldRight(e) {
      case (Expr.Let(sym, exp1, _, _, _, loc), acc) =>
        Expr.Let(sym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
      case (Expr.LetRec(varSym, index, defSym, exp1, _, _, _, loc), acc) =>
        Expr.LetRec(varSym, index, defSym, exp1, acc, acc.tpe, combine(acc.purity, exp1.purity), loc)
      case (other, _) => throw InternalCompilerException(s"Unexpected binder $other", other.loc)
    }
  }

  private def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Purity.Pure, Purity.Pure) => Purity.Pure
    case _ => Purity.Impure
  }

  /**
    * Let-binds all inner expressions, but not the top-most one.
    * Binders are added to the [[LocalContext]].
    */
  private def letBindInnerEffects(binders: mutable.ArrayBuffer[Expr])(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(sym, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(letBindEffects(binders))
      Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = letBindEffects(binders)(exp)
      val es = exps.map(letBindEffects(binders))
      Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(letBindEffects(binders))
      Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(letBindEffects(binders))
      Expr.ApplySelfTail(sym, formals, as, tpe, purity, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = letBindInnerEffects(binders)(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      val e3 = letBindEffectsTopLevel(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val bs = branches.map {
        case (sym, branchExp) => (sym, letBindEffectsTopLevel(branchExp))
      }
      Expr.Branch(e, bs, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, exp1, exp2, _, _, loc) =>
      val e1 = letBindInnerEffects(binders)(exp1)
      binders.addOne(Expr.Let(sym, e1, null, null, null, loc))
      letBindInnerEffects(binders)(exp2)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
      val e1 = letBindInnerEffects(binders)(exp1)
      binders.addOne(Expr.LetRec(varSym, index, defSym, e1, null, null, null, loc))
      letBindInnerEffects(binders)(exp2)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      Expr.TryCatch(e, rules, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rs = rules.map {
        case HandlerRule(op, fparams, handlerExp) => HandlerRule(op, fparams, letBindEffectsTopLevel(handlerExp))
      }
      Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(letBindEffects(binders))
      Expr.Do(op, es, tpe, purity, loc)

    case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
      val es = exps.map(letBindInnerEffects(binders))
      Expr.NewObject(name, clazz, tpe, purity, methods, es, loc)

    case Expr.Resume(exp, tpe, loc) =>
      val e = letBindEffects(binders)(exp)
      Expr.Resume(e, tpe, loc)
  }

  /**
    * Let-binds all sub-expressions including the top-most one.
    * Always returns a variable or a constant.
    */
  private def letBindEffects(binders: mutable.ArrayBuffer[Expr])(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = {
    val expTransformed = letBindInnerEffects(binders)(exp)
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
    bind(expTransformed)
  }

  private def letBindExpr(binders: mutable.ArrayBuffer[Expr])(e: Expr)(implicit lctx: LocalContext, flix: Flix): Expr.Var = {
    val loc = e.loc.asSynthetic
    val sym = Symbol.freshVarSym("anf", BoundBy.Let, loc)(Level.Default, flix)
    lctx.lparams.addOne(LocalParam(sym, e.tpe))
    binders.addOne(Expr.Let(sym, e, null, null, null, loc))
    Expr.Var(sym, e.tpe, loc)
  }

}
