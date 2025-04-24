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

import scala.collection.mutable

/**
  * Rewrites functions that recursively call themselves in tail-position to
  * non-recursive functions with a recursive _al def.
  */
object RecursionRewriter {

  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("RecursionRewriter") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  private def visitDef(defn: MonoAst.Def)(implicit flix: Flix): MonoAst.Def = {
    // 1. Check that every recursive call is in tail position
    //    Return a set of alive parameters, i.e, function parameters that are changed in a recursive call (if it is an Expr.Var with the same symbol, then it is dead).
    implicit val ctx: LocalContext = LocalContext.mk()
    val isRewritable = checkTailPosition(defn.exp, tailPos = true)(defn.sym, defn.spec.fparams, ctx, flix)
    if (!isRewritable) {
      return defn
    }

    // 2. Rewrite eligible functions

    // 2.1 Create a substitution from the function symbol and alive parameters to fresh symbols (maybe this can be created during step 2)
    // 2.2 Copy the function body, visit and apply the substitution and rewrite nodes. Any ApplyDef expr becomes an ApplyLocalDef expr.
    // 2.3 Replace the original function body with a LocalDef declaration that has the body from 3.2, followed by an ApplyLocalDef expr.
    ???
  }

  private def checkTailPosition(exp0: MonoAst.Expr, tailPos: Boolean)(implicit sym0: Symbol.DefnSym, fparams: List[MonoAst.FormalParam], ctx: LocalContext, flix: Flix): Boolean = exp0 match {
    case Expr.Cst(_, _, _) =>
      true

    case Expr.Var(sym, _, _) =>
      if (fparams.map(_.sym).contains(sym)) {
        ctx.alive.getOrElseUpdate(sym, Symbol.freshVarSym(sym))
      }
      true

    case Expr.Lambda(_, exp, _, _) =>
      checkTailPosition(exp, tailPos = false)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos) &&
        checkTailPosition(exp2, tailPos = false)

    case Expr.ApplyDef(sym, exps, _, _, _, _) =>
      if (sym != sym0) {
        // Not recursive call
        return exps.forall(checkTailPosition(_, tailPos = false))
      }

      // Recursive Call
      if (!tailPos) {
        // Not tailpos => abort!
        return false
      }

      // Recursive call in tailpos
      // Check alive parameters
      exps.zip(fparams)
        .filter {
          case (exp, fparam) => aliveParam(exp, fparam)
        }.forall {
          case (exp, _) => checkTailPosition(exp, tailPos = false)
        }

    case Expr.ApplyLocalDef(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)


    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)

    case Expr.Scope(_, _, exp, _, _, _) =>
      checkTailPosition(exp, tailPos)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos) &&
        checkTailPosition(exp3, tailPos)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)

    case Expr.Discard(exp, _, _) =>
      checkTailPosition(exp, tailPos)

    case Expr.Match(exp1, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.MatchRule(_, guard, exp2) =>
            guard.forall(checkTailPosition(_, tailPos = false)) &&
              checkTailPosition(exp2, tailPos)
        }

    case Expr.VectorLit(exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos = false)

    case Expr.VectorLength(exp, _) =>
      checkTailPosition(exp, tailPos = false)

    case Expr.Ascribe(exp, _, _, _) => // Is this erased???
      checkTailPosition(exp, tailPos)

    case Expr.Cast(exp, _, _, _, _, _) => // Is this erased???
      checkTailPosition(exp, tailPos)

    case Expr.TryCatch(exp1, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.CatchRule(_, _, exp2) =>
            checkTailPosition(exp2, tailPos)
        }
    case Expr.RunWith(exp1, _, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.HandlerRule(_, _, exp2) =>
            checkTailPosition(exp2, tailPos)
        }

    case Expr.Do(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos))

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.forall {
        case MonoAst.JvmMethod(_, _, exp, _, _, _) =>
          checkTailPosition(exp, tailPos = false)
      }
  }

  /** Returns `false` if `expr` is a var with same symbol as `fparam`. Returns `true` otherwise. */
  private def aliveParam(expr: MonoAst.Expr, fparam: MonoAst.FormalParam): Boolean = expr match {
    case Expr.Var(sym, _, _) => sym != fparam.sym
    case _ => true
  }

  private object LocalContext {

    def mk(): LocalContext = new LocalContext(new mutable.HashMap())

  }

  private case class LocalContext(alive: mutable.HashMap[Symbol.VarSym, Symbol.VarSym])

}
