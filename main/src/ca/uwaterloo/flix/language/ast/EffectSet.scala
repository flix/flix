/*
 *  Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

/**
  * Represents a collection of effects.
  */
sealed trait EffectSet {

  /**
    * Alias for [[isBot]].
    */
  def isPure: Boolean = isBot

  /**
    * Returns `true` if `this` effect set is the bottom (pure) effect set.
    */
  def isBot: Boolean = this == EffectSet.Bot

  /**
    * Returns `true` if `this` effect set is the top (any) effect set.
    */
  def isTop: Boolean = this == EffectSet.Top

  /**
    * Returns `true` if `this` effect set is smaller or equal to `that` effect set.
    */
  def leq(that: EffectSet): Boolean = (this, that) match {
    case (EffectSet.Bot, _) => true
    case (_, EffectSet.Top) => true
    case (EffectSet.MayMust(may1, must1), EffectSet.MayMust(may2, must2)) => (may1 subsetOf may2) && (must2 subsetOf must1)
    case _ => false
  }

  /**
    * Returns the least upper bound of `this` effect set and `that` effect set.
    *
    * Note: We union the may effects and intersect the must effects.
    */
  def lub(that: EffectSet): EffectSet = (this, that) match {
    case (EffectSet.Bot, x) => x
    case (x, EffectSet.Bot) => x
    case (EffectSet.MayMust(may1, must1), EffectSet.MayMust(may2, must2)) => EffectSet.MayMust(may1 union may2, must1 intersect must2)
    case (EffectSet.Top, _) => EffectSet.Top
    case (_, EffectSet.Top) => EffectSet.Top
  }

  /**
    * Returns the greatest lower bound of `this` effect set and `that` effect set.
    *
    * Note: We intersect the may effects and union the must effects.
    */
  def glb(that: EffectSet): EffectSet = (this, that) match {
    case (EffectSet.Top, x) => x
    case (x, EffectSet.Top) => x
    case (EffectSet.MayMust(may1, must1), EffectSet.MayMust(may2, must2)) => EffectSet.MayMust(may1 intersect may2, must1 union must2)
    case (EffectSet.Bot, _) => EffectSet.Top
    case (_, EffectSet.Bot) => EffectSet.Top
  }

  /**
    * Returns the result of executing `this` effect set before `that` effect set.
    *
    * Note: We union *both* the may effects and the must effects.
    */
  def seq(that: EffectSet): EffectSet = (this, that) match {
    case (EffectSet.Bot, x) => x
    case (x, EffectSet.Bot) => x
    case (EffectSet.MayMust(may1, must1), EffectSet.MayMust(may2, must2)) => EffectSet.MayMust(may1 union may2, must1 union must2)
    case (EffectSet.Top, _) => EffectSet.Top
    case (_, EffectSet.Top) => EffectSet.Top
  }

}

object EffectSet {

  /**
    * The bot (pure) effect set.
    */
  case object Bot extends EffectSet

  /**
    * The top (any) effect set.
    */
  case object Top extends EffectSet

  /**
    * Represents a collection of effects that an expression may and must cause.
    *
    * @param may  the set of effects the expression may cause.
    * @param must the set of effects the expression must cause.
    */
  case class MayMust(may: Set[Effect], must: Set[Effect]) extends EffectSet

}



