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

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Thrown when the flow set contains a growing cycle (see [[NonMonomorphizableCheck]]).
  *
  * Deliberately not an [[ca.uwaterloo.flix.util.InternalCompilerException]]: it indicates a
  * property of the user's program, not a compiler bug. Routing it through the ordinary
  * `CompilationMessage` pipeline is a known follow-up.
  */
case class NonMonomorphizableProgramException(message: String, loc: SourceLocation) extends RuntimeException(s"$message ($loc)")

/**
  * Rejects non-monomorphizable programs — flow sets with no finite solution — before
  * [[ConstraintSolver.solve]]'s fixpoint loop would grow without bound and exhaust the heap.
  *
  * Following "The Simple Essence of Monomorphization" (§3.3.2), the flow set is reinterpreted as
  * a graph over `(MonoVar, tuple-position)` vertices, and we look for a "growing cycle": a reachable
  * cycle with at least one edge that wraps the flowing type in an additional type constructor
  * (`a` flowing into `List[a]`). Such a cycle arises from polymorphic recursion
  * (`def f(x: a): List[a] = ...f(lst)...`) or a non-regular recursive enum/struct
  * (`enum T[a] { case Base(a); case Recurse(T[Poly[a]]) }`). A cycle of only direct-copy edges is
  * ordinary, convergent self-recursion and is fine. (The paper's third case — escaping
  * existentials — cannot arise: Flix has no existential types.)
  *
  * Every `MonoVar` kind participates, including `RestrictableEnum` (which the `Solution` never
  * reports): the solver propagates flows uniformly regardless of destination kind, so a growing
  * cycle through any kind still diverges. The check over-approximates in one known way: a
  * non-regular enum whose growing case is declared but never constructed is still rejected once
  * any other case of it is constructed, even though demand-driven specialization would not need
  * the growing case.
  */
object NonMonomorphizableCheck {

  /**
    * Checks whether `flows` contains a reachable growing cycle and throws
    * [[NonMonomorphizableProgramException]] if so.
    */
  def checkMonomorphizable(flows: Set[Flow]): Unit = ???
}
