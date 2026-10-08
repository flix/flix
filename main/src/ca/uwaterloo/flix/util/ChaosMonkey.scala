/*
 * Copyright 2025 Cade Lueker
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix

import scala.util.Random

/**
  * A testing utility that introduces controlled randomness into the compiler.
  *
  * When enabled the ChaosMonkey randomly reorders collections to help expose
  * order-dependent bugs that might otherwise go unnoticed due to deterministic
  * iteration order.
  *
  * As a general rule, the earlier the ChaosMonkey is used, the better.
  */
object ChaosMonkey {

  /**
    * Probability that the ChaosMonkey makes chaos.
    */
  private val P: Double = 0.10

  /**
    * Randomly permutes the given list if the ChaosMonkey is enabled.
    */
  def chaos[A](l: List[A])(implicit flix: Flix): List[A] = {
    if (flix.options.xchaosMonkey && Random.nextDouble() <= P) {
      Random.shuffle(l)
    } else {
      l
    }
  }

}
