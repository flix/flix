/*
 * Copyright 2026 Simon Lykke Andersen
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

/**
  * Entry point for constraint-based monomorphization, following the approach of "The Simple
  * Essence of Monomorphization" by Matthew Lutze, Philipp Schuster, and Jonathan Immanuel
  * Brachthäuser.
  *
  * At a high level, this pipeline works as follows:
  *
  *   - 1. [[ConstraintCollection]] generates flow constraints describing how concrete types and
  *     type shapes propagate into the type-parameter slots of every polymorphic def/enum/struct/
  *     restrictable-enum.
  *   - 2. [[NonMonomorphizableCheck]] rejects programs with no finite solution (e.g. polymorphic
  *     recursion) before solving, so the next step cannot loop forever.
  *   - 3. [[ConstraintSolver]] solves the flow constraints to a fixpoint, producing the set of
  *     concrete type-argument tuples each polymorphic symbol must be specialized at.
  *   - 4. [[SolutionSpecialization]] specializes (and lowers) every def/enum/struct/
  *     restrictable-enum accordingly.
  *
  * Caution: step 4's lowering can synthesize references to specific stdlib defs/enums that step 1
  * would not otherwise have any reason to see. Any such construct needs its own constraints
  * generated in step 1, or it won't be in the solution by the time step 4 needs to specialize it.
  */
object ConstraintMonomorphization {

  /** Performs constraint-based monomorphization of the given AST `root`. */
  def run(root: TypedAst.Root)(implicit flix: Flix): MonoAst.Root = {
    val constraints = ConstraintCollection.generate(root)
    NonMonomorphizableCheck.checkMonomorphizable(constraints)
    val solution = ConstraintSolver.solve(constraints, root)
    SolutionSpecialization.run(root, solution)
  }
}
