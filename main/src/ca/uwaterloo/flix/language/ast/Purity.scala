/*
 * Copyright 2022 Christian Bonde, Patrick Lundvig, Anna Krogh
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

sealed trait Purity

/**
 * Represents the purity (or impurity) of an expression.
 */
object Purity {

  /**
   * Represents a pure expression (i.e. an expression that cannot have side-effects).
   */
  case object Pure extends Purity

  /**
   * Represents an impure expression (i.e. an expression that could potentially have side-effects).
   */
  case object Impure extends Purity

  /**
    * Returns true if `p` is is a purity that does not allows algebraic effects.
    */
  def isControlPure(p: Purity): Boolean = p match {
    case Pure => true
    case Impure => false
  }

  /**
    * Returns true if `p` is is a purity that allows algebraic effects.
    */
  def isControlImpure(p: Purity): Boolean = p match {
    case Pure => false
    case Impure => true
  }

  /**
    * Returns the max purity of `p1` and `p2` according to this ordering:
    * Pure < Impure
    */
  def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Pure, Pure) => Pure
    case (Pure, Impure) => Impure
    case (Impure, Pure) => Impure
    case (Impure, Impure) => Impure
  }

  /**
    * Returns the max purity of `p1`, `p2`, and `p3` according to this ordering:
    * Pure < Impure
    */
  def combine3(p1: Purity, p2: Purity, p3: Purity): Purity = {
    combine(combine(p1, p2), p3)
  }

  /**
    * Returns the max purity of `p` according to this ordering:
    * Pure < Impure
    *
    * Returns [[Pure]] if empty.
    */
  def combineAll(p: List[Purity]): Purity = {
    p.foldLeft(Pure: Purity)(combine)
  }

  /**
    * Returns the purity of the given formula `eff`. Returns [[Pure]] for the
    * effect constant [[TypeConstructor.Pure]] and [[Impure]] otherwise.
    */
  def fromType(eff: Type): Purity = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => Pure
    case _ => Impure
  }

}
