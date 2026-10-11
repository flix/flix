/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.typer

/**
  * Per-thread deterministic counters for constraint-solver work, used by the
  * compiler-top profiler to attribute solver effort to each def.
  *
  * The Typer solves each def on a single thread ([[ca.uwaterloo.flix.language.phase.Typer]]
  * uses `ParOps.parMapValues` over `root.defs`), so a snapshot delta taken
  * around one def's typing attributes the work to that def with no
  * cross-thread interference. Counts are deliberately cheap mutable longs in a
  * thread-local holder rather than atomics.
  *
  *   - `reduces` counts calls to [[TypeReduction2.reduce]] (the type-reduction
  *     engine, the hottest method in JFR profiles). It backs the `rpv` column.
  */
object SolverMetrics {

  final class Counters {
    var reduces: Long = 0L
  }

  private val tl: ThreadLocal[Counters] = ThreadLocal.withInitial(() => new Counters)

  /** The calling thread's counters. */
  def counters: Counters = tl.get()

}
