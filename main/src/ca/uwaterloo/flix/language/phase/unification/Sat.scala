/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import scala.collection.immutable.SortedSet

trait Sat[F, V] {

  /**
    * Returns true if the given Boolean formula is satisfiable.
    */
  def satisfiable(f: F)(implicit alg: BoolAlg2[F, V]): Boolean

  /**
    * Returns the set of free variables in `f`.
    */
  def freeVars(f: F)(implicit alg: BoolAlg2[F, V]): SortedSet[V]

  // MATT docs
  def map(f: F)(gn: V => F)(implicit alg: BoolAlg2[F, V]): F
}
