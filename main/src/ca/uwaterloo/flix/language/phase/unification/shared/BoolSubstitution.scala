/*
 *  Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.shared

import ca.uwaterloo.flix.language.phase.unification.BoolAlg

/**
 * Companion object for the [[BoolSubstitution]] class.
 */
object BoolSubstitution {
  /**
   * Returns the empty substitution.
   */
  def empty[F]: BoolSubstitution[F] = BoolSubstitution(Map.empty)

  /**
   * Returns the singleton substitution mapping the type variable `x` to `tpe`.
   */
  def singleton[F](x: Int, f: F)(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    // Ensure that we do not add any `x -> x` mappings.
    if (f == alg.mkVar(x))
      empty
    else
      BoolSubstitution(Map(x -> f))
  }

}

/**
 * A substitution is a map from type variables to types.
 */
case class BoolSubstitution[F](m: Map[Int, F]) {

  /**
   * Returns `true` if `this` is the empty substitution.
   */
  val isEmpty: Boolean = m.isEmpty

  /**
   * Applies `this` substitution to the given type `tpe0`.
   */
  def apply(f: F)(implicit alg: BoolAlg[F]): F = {
    // Optimization: Return the type if the substitution is empty. Otherwise, visit the type.
    if (isEmpty) {
      f
    } else {
      alg.map(f) {
        i => m.getOrElse(i, alg.mkVar(i))
      }
    }
  }

  /**
   * Applies `this` substitution to the given types `ts`.
   */
  def apply(ts: List[F])(implicit alg: BoolAlg[F]): List[F] = if (isEmpty) ts else ts map apply

  /**
   * Returns the left-biased composition of `this` substitution with `that` substitution.
   */
  def ++(that: BoolSubstitution[F]): BoolSubstitution[F] = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      BoolSubstitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  /**
   * Returns the composition of `this` substitution with `that` substitution.
   */
  def @@(that: BoolSubstitution[F])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // NB: Use of mutability improve performance.
    import scala.collection.mutable
    val newMap = mutable.Map.empty[Int, F]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      newMap.update(x, this.apply(t))
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newMap.update(x, t)
      }
    }

    BoolSubstitution(newMap.toMap)
  }

}
