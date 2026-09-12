/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.util.collection.MultiMap

/**
  * Companion object for [[DependencyGraph]].
  */
object DependencyGraph {
  /**
    * The empty dependency graph.
    */
  val empty: DependencyGraph = DependencyGraph(MultiMap.empty)
}

/**
  * Represents a dependency graph.
  *
  * If the graph contains an edge `src -> dst` that means that if `src` changes then `dst` must be recomputed.
  */
case class DependencyGraph(deps: MultiMap[Input, Input]) {

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
