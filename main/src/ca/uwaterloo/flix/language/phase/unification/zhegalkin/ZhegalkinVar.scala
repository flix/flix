/*
 * Copyright 2024 Magnus Madsen
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
