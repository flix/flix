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

import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Represents the computational effect of an expression.
  */
sealed trait Eff {

  /**
    * Returns the effect set.
    */
  def eff: EffectSet

  /**
    * Returns `true` if `this` effect is pure.
    */
  def isPure: Boolean = this match {
    case Eff.Box(eff) => eff.isPure
    case Eff.Arrow(_, _, _, eff) => eff.isPure
  }

  /**
    * Returns the "arrow" part of an effect.
    */
  def restrict: Eff = this match {
    case Eff.Box(eff) => Eff.Box(EffectSet.Pure)
    case Eff.Arrow(e1, latent, e2, eff) => Eff.Arrow(e1, latent, e2, EffectSet.Pure)
  }

  /**
    * Returns `true` if `this` effect is subsumed by `that` effect.
    *
    * NB: The structure of `this` and `that` must be the same.
    */
  def leq(that: Eff): Boolean = this.eff leq that.eff

  /**
    * Returns the least upper bound of `this` and `that` effect.
    *
    * NB: The structure of `this` and `that` must be the same.
    */
  def lub(that: Eff): Eff = (this, that) match {
    case (Eff.Box(eff1), Eff.Box(eff2)) => Eff.Box(eff1 lub eff2)
    case (Eff.Arrow(e11, latent1, e12, eff1), Eff.Arrow(e21, latent2, e22, eff2)) =>
      val e1 = e11 lub e21
      val e2 = e12 lub e22
      Eff.Arrow(e1, latent1 lub latent2, e2, eff1 lub eff2)
    case _ => throw InternalCompilerException(s"Mismatched effects '$this' and '$that'.")
  }

  /**
    * Returns the effect of executing the effects of `this` before the effects of `that`.
    */
  def seq(that: Eff): Eff = (this, that) match {
    case (Eff.Box(eff1), Eff.Box(eff2)) => Eff.Box(eff1 seq eff2)
    case (Eff.Arrow(e11, latent1, e12, eff1), Eff.Box(eff2)) => Eff.Box(eff1 seq eff2)
    case (Eff.Box(eff1), Eff.Arrow(e21, latent2, e22, eff2)) => Eff.Arrow(e21, latent2, e22, eff1 seq eff2)

    // TODO: Arrow part...
    case _ => throw InternalCompilerException(s"Mismatched effects '$this' and '$that'.")
  }

}

object Eff {

  /**
    * Represents the bot (impossible) effect set.
    */
  val Bot: Eff = Box(EffectSet.Bot)

  /**
    * Represents the top (any) effect set.
    */
  val Top: Eff = Box(EffectSet.Top)

  /**
    * Represents the pure (empty) effect set.
    */
  val Pure: Eff = Box(EffectSet.Pure)

  /**
    * Represents a non-lambda effect.
    */
  case class Box(eff: EffectSet) extends Eff

  /**
    * Represents a lambda effect from `e1` with `latent` effect to `e2` where the expression itself has effect `eff`.
    *
    * As a picture: (e1 ---latent---> e2) @ eff
    */
  case class Arrow(e1: Eff, latent: EffectSet, e2: Eff, eff: EffectSet) extends Eff

}
