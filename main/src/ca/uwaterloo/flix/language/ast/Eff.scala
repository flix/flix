/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

object Eff {

  /**
    * Represents a computational effect that is pure.
    */
  val Pure: Eff = Eff(Set.empty, Set.empty)

  def lub(eff1: Eff, eff2: Eff): Eff = ???

}

/**
  * Represents the computational effect of an expression.
  */
case class Eff(may: Set[Effect], must: Set[Effect]) {

  /**
    * Returns `true` if the computational effect is pure.
    */
  val isPure: Boolean = may.isEmpty && must.isEmpty

  /**
    * Returns the latent effects of `this` effect, i.e. the effects
    * that result from an application of the expression.
    */
  def latent: Eff = {
    // Collect the latent effects that *may* occur.
    val latentMay = may.collect {
      case Effect.Latent(effect) => effect
    }
    // Collect the latent effects that *must* occur.
    val latentMust = must.collect {
      case Effect.Latent(effect) => effect
    }
    Eff(latentMay, latentMust)
  }

}
