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

/**
  * Rejects non-monomorphizable programs (flow sets with no finite solution) before
  * [[ConstraintSolver.solve]]'s fixpoint loop, which would otherwise grow without bound.
  *
  * Following "The Simple Essence of Monomorphization" (§3.3.2), the flow set is reinterpreted as
  * a graph over `(MonoVar, tuple-position)` vertices, and we look for a "growing cycle": a reachable
  * cycle with at least one edge that wraps the flowing type in an additional type constructor
  * (`a` flowing into `List[a]`). Such a cycle arises from polymorphic recursion
  * (e.g. `def f(x: a): List[a] = f(x::Nil)`) or a non-regular recursive enum/struct
  * (`enum T[a] { case Base(a); case Recurse(T[Poly[a]]) }`). A cycle of only direct-copy edges is
  * ordinary, convergent self-recursion and is fine.
  *
  * The check over-approximates in one known way: a non-regular enum whose growing case is declared
  * but never constructed is still rejected once any other case of it is constructed, even though
  * demand-driven specialization would not need the growing case.
  */
object NonMonomorphizableCheck {

  // TODO Make it a proper compiler error-message
  /**
    * Checks whether `flows` contains a reachable growing cycle and throws
    * [[InternalCompilerException]] if so.
    */
  def checkMonomorphizable(flows: Set[Flow]): Unit = ???
}
