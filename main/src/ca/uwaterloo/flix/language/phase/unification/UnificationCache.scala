/*
 *  Copyright 2022 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import java.util.concurrent.ConcurrentHashMap

object UnificationCache {
  /**
    * A Global (per-JVM) cache of unification queries for BoolFormulas.
    */
  val GlobalBool: UnificationCache[BoolFormula] = new UnificationCache()
}

/**
  * A thread-safe cache of unification queries.
  */
class UnificationCache[F] {
  private val m: ConcurrentHashMap[(F, F, Set[Int]), BoolSubstitution[F]] = new ConcurrentHashMap

  def lookup(f1: F, f2: F, renv: Set[Int]): Option[BoolSubstitution[F]] =
    Option(m.get((f1, f2, renv)))

  def put(f1: F, f2: F, renv: Set[Int], s: BoolSubstitution[F]): Unit =
    m.putIfAbsent((f1, f2, renv), s)

  def clear(): Unit = {
    m.clear()
  }

}
