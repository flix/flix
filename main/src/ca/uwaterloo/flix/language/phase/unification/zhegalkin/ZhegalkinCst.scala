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

import ca.uwaterloo.flix.util.CofiniteIntSet

/** Companion object for [[ZhegalkinCst]] */
object ZhegalkinCst {
  /** A Zhegalkin constant that represents the empty set. */
  val empty: ZhegalkinCst = ZhegalkinCst(CofiniteIntSet.empty)

  /** A Zhegalkin constant that represents the universe. */
  val universe: ZhegalkinCst = ZhegalkinCst(CofiniteIntSet.universe)

  /**
    * A smart constructor for Zhegalkin constants.
    *
    * Ensures that the empty and the universe has a unique representation.
    */
  def mkCst(s: CofiniteIntSet): ZhegalkinCst = {
    if (s.isEmpty)
      empty
    else if (s.isUniverse)
      universe
    else
      ZhegalkinCst(s)
  }

  /** Returns the xor of the two given Zhegalkin constants `c1` and `c2`. */
  def mkXor(c1: ZhegalkinCst, c2: ZhegalkinCst): ZhegalkinCst = {
    // Note: use of union, inter, compl ensures a canonical representation.
    // a ⊕ b = (a ∪ b) - (a ∩ b) = (a ∪ b) ∩ ¬(a ∩ b)
    c1.union(c2).inter(c1.inter(c2).compl)
  }
}

/** Represents a set Zhegalkin constant (i.e. a set or co-set). A thin wrapper around [[CofiniteIntSet]]. */
case class ZhegalkinCst(s: CofiniteIntSet) {
  /** Returns the complement of `this` Zhegalkin constant. */
  def compl: ZhegalkinCst = ZhegalkinCst.mkCst(CofiniteIntSet.complement(s))

  /** Returns the union of `this` Zhegalkin constant with `that`/ */
  def union(that: ZhegalkinCst): ZhegalkinCst = ZhegalkinCst.mkCst(CofiniteIntSet.union(s, that.s))

  /** Returns the intersection of `this` Zhegalkin constant with `that`. */
  def inter(that: ZhegalkinCst): ZhegalkinCst = ZhegalkinCst.mkCst(CofiniteIntSet.intersection(s, that.s))

  /** Returns a human-readable string representation of `this` Zhegalkin constant. Must only be used for debugging. */
  override def toString: String = {
    if (s.isEmpty) "Ø"
    else if (s.isUniverse) "𝓤"
    else s match {
      case CofiniteIntSet.Set(xs) => s"{${xs.mkString(", ")}}"
      case CofiniteIntSet.Compl(xs) => s"¬{${xs.mkString(", ")}}"
    }
  }
}
