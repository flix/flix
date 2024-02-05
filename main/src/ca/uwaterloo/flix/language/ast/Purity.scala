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
  * Represents the purity of an expression.
  *
  * - Pure expressions are treated as mathematical functions.
  * - Impure expressions can use mutation or interact with the system.
  * - Control-Impure expressions can do all the above but also use unhandled
  *   algebraic effects.
  *
  * In terms of the set of expressions that is allowed under each effect the following holds:
  * `e_pure ⊆ e_impure ⊆ e_control-impure`.
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
    * Represents a control-impure expression (i.e. an expression that could potentially use effects like `do Print.print()`).
    */
  case object ControlImpure extends Purity

  /**
    * Returns the max effect of `p1` and `p2` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Pure, Pure) => Pure
    case (ControlImpure, _) => ControlImpure
    case (_, ControlImpure) => ControlImpure
    case (Impure, _) => Impure
    case (_, Impure) => Impure
  }

  /**
    * Returns the max effect of `p1`, `p2`, and `p3` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def combine(p1: Purity, p2: Purity, p3: Purity): Purity = combine(combine(p1, p2), p3)

  /**
    * Returns the max effect of `p` according to this ordering:
    * Pure < Impure < ControlImpure
    *
    * Returns [[Pure]] if empty.
    */
  def combine(p: List[Purity]): Purity = p.foldLeft(Pure: Purity)(combine)

  /**
    * Returns the min effect of `p1` and `p2` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def min(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Pure, _) => Pure
    case (_, Pure) => Pure
    case (Impure, _) => Impure
    case (_, Impure) => Impure
    case (_, _) => ControlImpure
  }

  /**
    * Returns the min effect of `p1`, `p2`, and `p3` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def min(p1: Purity, p2: Purity, p3: Purity): Purity = min(min(p1, p2), p3)

  /**
    * Returns the min effect of `p` according to this ordering:
    * Pure < Impure < ControlImpure
    *
    * Returns [[Pure]] if empty.
    */
  def min(p: List[Purity]): Purity = p.foldLeft(Pure: Purity)(min)

  sealed trait I

  object I {
    case object Top extends I
    case class TopM(set: Set[Symbol.EffectSym]) extends I
    case class S(set: Set[Symbol.EffectSym]) extends I

    def union(i1: I, i2: I): I = (i1, i2) match {
      case (Top, _) => Top
      case (_, Top) => Top
      case (S(s1), S(s2)) => S(s1.union(s2))
      case (S(s1), TopM(s2)) =>
        // s1 + (T - s2) = T - (s2 - s1)
        TopM(s2.diff(s1))
      case (TopM(s1), S(s2)) =>
        // (T - s1) + s2 = T - (s1 - s2)
        TopM(s1.diff(s2))
      case (TopM(s1), TopM(s2)) =>
        // (T - s1) + (T - s2) = T - (s1 & s2)
        TopM(s1.intersect(s2))
    }

    def intersect(i1: I, i2: I): I = (i1, i2) match {
      case (Top, other) => other
      case (other, Top) => other
      case (S(s1), S(s2)) => S(s1.intersect(s2))
      case (S(s1), TopM(s2)) =>
        // s1 & (T - s2) = s1 - ({} + !s2) =
        TopM(s2.diff(s1))
      case (TopM(s1), S(s2)) =>
        // (T - s1) + s2 = T - (s1 - s2)
        TopM(s1.diff(s2))
      case (TopM(s1), TopM(s2)) =>
        // (T - s1) + (T - s2) = T - (s1 & s2)
        TopM(s1.intersect(s2))
    }
  }

  def fromType(tpe0: Type): I = tpe0 match {
    case Type.Cst(TypeConstructor.Effect(sym), _) => I.S(Set(sym))
    case Type.Cst(TypeConstructor.Pure, _) => I.S(Set.empty)
    case Type.Cst(TypeConstructor.Univ, _) => I.Top
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => ???
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => ???
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe, _) => ???
    case Type.Var(sym, loc) => ???
    case Type.Alias(cst, args, tpe, loc) => ???
    case Type.AssocType(cst, arg, kind, loc) => ???
  }

}
