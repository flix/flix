/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.util.collection.MultiMap

/**
  * Companion object for [[DependencyGraph]].
  */
object DependencyGraph {
  /**
    * The empty dependency graph.
    */
  val empty: DependencyGraph = DependencyGraph(Map.empty, MultiMap.empty)
}

/**
  * Represents a dependency graph.
  *
  * If the graph contains an edge `src -> dst` that means that if `src` changes then `dst` must be recomputed.
  */
case class DependencyGraph(defDeps: Map[DefnSym, SymUse.DefSymUse], deps: MultiMap[Input, Input]) {

  /**
    * Returns all inputs that are transitively dirty (including `i`).
    *
    * We compute a fixpoint such that if `x` is dirty, `x -> {y}` and `y -> {z}` then `{x, y, z}` are dirty.
    */
  def dirty(i: Input): Set[Input] = {
    var current = deps(i) + i
    var changed = true
    while (changed) {
      changed = false
      val next = current.flatMap(i => deps(i))
      if (!next.subsetOf(current)) {
        current = current ++ next
        changed = true
      }
    }
    current
  }

  override def toString: String = {
    val sb = new StringBuilder()
    for ((src, dsts) <- deps.m) {
      for (dst <- dsts) {
        sb.append(f"$src -> $dst\n")
      }
    }
    sb.toString()
  }

}
