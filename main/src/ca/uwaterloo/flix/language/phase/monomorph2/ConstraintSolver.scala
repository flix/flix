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
import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Solves the flow constraints produced by [[ConstraintCollection]] to a fixpoint: starting from
  * the ground (all-constant) flows, each newly solved tuple is substituted into the flows that
  * depend on it until no new tuples appear. Sig destinations are additionally dispatched to their
  * implementing (or default) def. The result is, per polymorphic symbol, exactly the ground
  * type-argument tuples that actually arise.
  */
object ConstraintSolver {

  /**
    * Solves `flows` to a fixpoint and returns the set of required specializations.
    *
    * Callers must run [[NonMonomorphizableCheck.checkMonomorphizable]] first; without it, a
    * non-monomorphizable flow set makes the fixpoint loop grow without bound.
    */
  def solve(flows: Set[Flow], root: TypedAst.Root)(implicit flix: Flix): Solution = ???
}
