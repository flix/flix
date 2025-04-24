/*
 * Copyright 2025 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.Expr
import ca.uwaterloo.flix.language.ast.{MonoAst, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue

/**
  * Rewrites functions that recursively call themselves in tail-position to
  * non-recursive functions with a recursive local def.
  */
object RecursionRewriter {

  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("RecursionRewriter") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  private def visitDef(defn: MonoAst.Def): MonoAst.Def = {
    // 1. Check that every recursive call is in tail position
    implicit val ctx: LocalContext = LocalContext.mk()
    val isRewritable = checkTailPosition(defn.exp, tailPos = true)(defn.sym, defn.spec.fparams ctx)
    // 2. Return a set of alive parameters, i.e, function parameters that are changed in a recursive call (if it is an Expr.Var with the same symbol, then it is dead).
    // 3. Rewrite eligible functions
    // 3.1 Create a substitution from the function symbol and alive parameters to fresh symbols (maybe this can be created during step 2)
    // 3.2 Copy the function body, visit and apply the substitution and rewrite nodes. Any ApplyDef expr becomes an ApplyLocalDef expr.
    // 3.3 Replace the original function body with a LocalDef declaration that has the body from 3.2, followed by an ApplyLocalDef expr.
    ???
  }

  private def checkTailPosition(exp0: MonoAst.Expr, tailPos: Boolean)(implicit sym0: Symbol.DefnSym, fparams: List[MonoAst.FormalParam], ctx: LocalContext): Boolean = exp0 match {
    case Expr.Cst(_, _, _) =>
      tailPos

    case Expr.Var(_, _, _) =>
      tailPos

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      // The inner expr is always in tailpos, but we care about `sym0` so it should be in tailpos only if the lambda is too
      val expIsTailPos = checkTailPosition(exp, tailPos = tailPos)
      tailPos && expIsTailPos

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val expsInTailPos = exps.forall(checkTailPosition(_, tailPos = false))
      tailPos

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) => ???
    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) => ???
    case Expr.Let(sym, exp1, exp2, tpe, eff, loc) => ???
    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) => ???
    case Expr.Scope(sym, regSym, exp, tpe, eff, loc) => ???
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => ???
    case Expr.Discard(exp, eff, loc) => ???
    case Expr.Match(exp, rules, tpe, eff, loc) => ???
    case Expr.VectorLit(exps, tpe, eff, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ascribe(exp, tpe, eff, loc) => ???
    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) => ???
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => ???
    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) => ???
    case Expr.Do(op, exps, tpe, eff, loc) => ???
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ???
  }

  private object LocalContext {

    def mk(): LocalContext = new LocalContext(new ConcurrentLinkedQueue())

  }

  private case class LocalContext(alive: ConcurrentLinkedQueue[(Symbol.VarSym, Symbol.VarSym)])

}
