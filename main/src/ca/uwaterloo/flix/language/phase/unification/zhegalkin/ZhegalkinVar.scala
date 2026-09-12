/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

/**
  * Represents a flexibile or rigid variable. (A rigid variable is also known as constant).
  *
  * Importantly equality and ordering is defined on *both* the integer `id` and the Boolean `flexible`.
  *
  * This is necessary because we want to cache Zhegalkin expressions that involves variables,
  * but the flexibility/rigidity of a variable may change between different invocations of Boolean unification.
  *
  * A Zhegalkin expression should _never_ contain the same variable that is both flexible and rigid,
  * i.e. we assume that the two domains are disjoint.
  */
case class ZhegalkinVar(id: Int, flexible: Boolean) extends Ordered[ZhegalkinVar] {
  override def compare(that: ZhegalkinVar): Int = {
    val cmp = this.id - that.id
    if (cmp != 0) {
      return cmp
    }
    val x = if (this.flexible) 0 else 1
    val y = if (that.flexible) 0 else 1
    x - y
  }

  /** Returns a human-readable string representation of `this` Zhegalkin variable. Must only be used for debugging. */
  override def toString: String = if (flexible) s"x$id" else s"x!$id"
}
