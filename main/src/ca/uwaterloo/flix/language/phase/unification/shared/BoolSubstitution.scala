/*
 *  Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

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
