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
  def isPure: Boolean = ??? // TODO

  // TODO: Which ops should be infix?

  /**
    *
    */
  def leq(that: Eff): Boolean = ??? // TODO

}

object Eff {

  /**
    * Represents a computational effect that is pure.
    */
  val Pure: Eff = Coll(Set.empty, Set.empty)

  /**
    * Represents any effect.
    */
  // TODO: Name
  val Top: Eff = Coll(Set.empty, Set.empty) // TODO

  /**
    *
    */
  def leq(eff1: Eff, eff2: Eff): Boolean = true // TODO

  /**
    *
    */
  def lub(eff1: Eff, eff2: Eff): Eff = eff1 // TODO

  /**
    */
  def app(eff: Eff): Eff = eff // TODO

  /**
    */
  def seq(eff1: Eff, eff2: Eff): Eff = eff1

  /**
    */
  def seq(effs: List[Eff]): Eff = effs.head // TODO

  /**
    *
    */
  case object Box extends Eff

  /**
    *
    */
  case class Arrow(eff1: Eff, eff: Eff, eff2: Eff) extends Eff

  /**
    *
    */
  case class Coll(may: Set[Effect], must: Set[Effect]) extends Eff

}
