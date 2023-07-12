/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.util.collection.Bimap

/**
  * A type class for Boolean Formulas.
  */
trait BoolAlg2[F, V] {

  /**
    * Returns `true` if `f` represents TRUE.
    */
  def isTrue(f: F): Boolean

  /**
    * Returns `true` if `f` represents FALSE.
    */
  def isFalse(f: F): Boolean

  /**
    * Returns `true` if `f` represents a variable.
    */
  def isVar(f: F): Boolean

  /**
    * Returns a representation of TRUE.
    */
  def mkTrue: F

  /**
    * Returns a representation of FALSE.
    */
  def mkFalse: F

  /**
    * Returns a representation of the variable with the given `id`.
    */
  def mkVar(id: V): F

  /**
    * Returns a representation of the complement of `f`.
    */
  def mkNot(f: F): F

  /**
    * Returns a representation of the disjunction of `f1` and `f2`.
    */
  def mkOr(f1: F, f2: F): F

  /**
    * Returns a representation of the conjunction of `f1` and `f2`.
    */
  def mkAnd(f1: F, f2: F): F

  /**
    * Returns a representation of the formula `f1 xor f2`.
    */
  def mkXor(f1: F, f2: F): F = mkOr(mkAnd(f1, mkNot(f2)), mkAnd(mkNot(f1), f2))

  /**
    * Converts the given formula f into another Boolean formula.
    */
  def convert[G, W](f: F, env: Bimap[V, W])(implicit otherAlg: BoolAlg2[G, W]): G
}

