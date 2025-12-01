/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Computes the set of reachable public library functions from the source project.
  */
object Reachability {

  case class ReachableSyms(defs: Set[Symbol.DefnSym], sigs: Set[Symbol.SigSym])

  def run(root: TypedAst.Root): ReachableSyms = {
    val initDefnSyms = root.defs.keys.map(ReachableSym.DefnSym.apply).toSet
    val initTraitSyms = root.traits.keys.map(ReachableSym.TraitSym.apply)
    val initSigSym = root.sigs.keys.map(ReachableSym.SigSym.apply)
    val init: Set[ReachableSym] = initDefnSyms ++ initTraitSyms ++ initSigSym

    var reach = init
    var delta = reach

    while (delta.nonEmpty) {
      val newReach = delta.flatMap(visitSym)
      delta = newReach -- reach
      reach = reach ++ delta
    }

    val defnSyms = reach.collect { case x: ReachableSym.DefnSym => x }.map(_.sym)
    val sigSyms = reach.collect { case x: ReachableSym.SigSym => x }.map(_.sym)
    ReachableSyms(defnSyms, sigSyms)

  }

  private def visitSym(sym0: ReachableSym): Set[ReachableSym] = ???

  /** Reachable symbols (defs, traits, sigs). */
  private sealed trait ReachableSym

  private object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }

}
