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
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.ParOps

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
    // 2. Return a set of alive parameters, i.e, function parameters that are changed in a recursive call (if it is an Expr.Var with the same symbol, then it is dead).
    // 3. Rewrite eligible functions
    // 3.1 Create a substitution from the function symbol and alive parameters to fresh symbols (maybe this can be created during step 2)
    // 3.2 Copy the function body, visit and apply the substitution and rewrite nodes. Any ApplyDef expr becomes an ApplyLocalDef expr.
    // 3.3 Replace the original function body with a LocalDef declaration that has the body from 3.2, followed by an ApplyLocalDef expr.
    ???
  }

}
