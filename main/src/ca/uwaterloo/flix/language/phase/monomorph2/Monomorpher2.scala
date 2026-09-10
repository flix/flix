/*
 * Copyright 2026 Flix Authors
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{MonoAst, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

/**
  * Entry point for constraint-based monomorphization, following the approach of "The Simple
  * Essence of Monomorphization" by Matthew Lutze, Philipp Schuster, and Jonathan Immanuel
  * Brachthäuser.
  *
  * At a high level, this pipeline works as follows:
  *
  *   - 1. [[ConstraintGen]] generates flow constraints describing how concrete types and
  *     type shapes propagate into the type-parameter slots of every polymorphic def/enum/struct/
  *     restrictable-enum.
  *   - 2. [[NonMonomorphizableCheck]] rejects programs with no finite solution (e.g. polymorphic
  *     recursion) before solving, so the next step cannot loop forever.
  *   - 3. [[ConstraintSolver]] solves the flow constraints to a fixpoint, producing the set of
  *     concrete instantiations each live symbol must be specialized at. Solving is
  *     demand-driven: it starts from the entry points (plus effect ops, non-parametric
  *     enums/structs, and monomorphic channel/Datalog lowering targets), and a flow only fires
  *     once its origin declaration is live — so declarations that are never demanded produce no
  *     specializations, even if [[TreeShaker1]]'s coarser reachability kept them around.
  *   - 4. [[Specialize]] specializes (and lowers) every live def/enum/struct/
  *     restrictable-enum accordingly.
  *
  * Caution: step 4's lowering can synthesize references to specific stdlib defs/enums that step 1
  * would not otherwise have any reason to see. Any such construct needs its own constraints
  * generated in step 1 (or a seed in step 3, for monomorphic targets), or it won't be in the
  * solution by the time step 4 needs to specialize it. Demand-driven solving makes this stricter:
  * an eager ground flow from an unrelated declaration can no longer paper over a missing demand —
  * a miss surfaces as a strict lookup failure in step 4 and must be fixed by adding the missing
  * constraint in step 1, never by weakening the gating in step 3.
  */
object Monomorpher2 {

  /** Performs constraint-based monomorphization of the given AST `root`. */
  def run(root: TypedAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Monomorpher2") {
    val constraints = ConstraintGen.generate(root)
    NonMonomorphizableCheck.checkMonomorphizable(constraints)
    val solution = ConstraintSolver.solve(constraints, root)
    Specialize.run(root, solution)
  }
}
