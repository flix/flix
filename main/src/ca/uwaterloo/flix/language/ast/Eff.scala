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

/**
  * Represents the computational effect of an expression.
  */
sealed trait Eff {

  /**
    * Returns `true` if the computational effect is pure.
    */
  def isPure: Boolean = this match {
    case Eff.EffectSet(Some(may), None) => may.isEmpty
    case Eff.Arrow(eff1, eff2, eff3) => eff1.isPure && eff2.isPure && eff3.isPure
    case _ => false
  }

  /**
    *
    */
  def leq(that: Eff): Boolean = true // TODO

  def lub(eff1: Eff): Eff = eff1 // TODO

  def seq(eff1: Eff): Eff = eff1

}

object Eff {

  /**
    * Represents a computational effect that is pure.
    */
  val Pure: Eff = EffectSet(Some(Set.empty), None)

  /**
    * Represents any effect.
    */
  // TODO: Name
  val Top: Eff = EffectSet(None, Some(Set.empty)) // TODO

  /**
    */
  def app(eff: Eff): Eff = eff // TODO

  /**
    */
  def seq(eff1: Eff, eff2: Eff): Eff = eff1

  /**
    */
  def seq(effs: List[Eff]): Eff = effs.foldLeft(Eff.Pure)(seq)

  /**
    * Represents a collection of effects.
    */
  case class EffectSet(may: Option[Set[Effect]], must: Option[Set[Effect]]) extends Eff

  /**
    *
    */
  case class Arrow(eff1: Eff, eff: Eff, eff2: Eff) extends Eff

}
