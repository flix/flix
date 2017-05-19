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
  * Representation of effects.
  */
trait Effect {

  def latent: Effect = ???

}

object Effect {

  /**
    * A pure (empty) effect.
    */
  case object Pure extends Effect

  /**
    * An effect that represents that the expression may throw an exception.
    */
  case object Exception extends Effect

  /**
    * A set of effects.
    */
  case class EffectSet(effs: Set[Effect]) extends Effect

  /**
    * Combines the two given effects `eff1` and `eff2` into a single effect.
    */
  def combine(eff1: Effect, eff2: Effect): Effect = (eff1, eff2) match {
    case (Pure, Pure) => Pure
    case (Exception, Exception) => Exception

    case (EffectSet(es), Pure) => EffectSet(es)
    case (EffectSet(es), Exception) => EffectSet(es + Exception)

    case (Pure, EffectSet(es)) => EffectSet(es)
    case (Exception, EffectSet(es)) => EffectSet(es)

    case (EffectSet(es1), EffectSet(es2)) => EffectSet(es1 ++ es2)
  }

  // TODO: DOC
  def combine(eff1: Effect, eff2: Effect, es: Effect*): Effect =
    es.foldLeft(combine(eff1, eff2))(combine)

  // TODO: DOC
  def combine(eff: Effect, effs: List[Effect]): Effect =
    effs.foldLeft(eff)(combine)


}
