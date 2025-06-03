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

import ca.uwaterloo.flix.util.collection.CofiniteIntSet

/** Companion object for [[ZhegalkinCst]] */
object ZhegalkinCst {

  /**
    * A smart constructor for Zhegalkin constants.
    *
    * Ensures that the empty and the universe has a unique representation.
    */
  def mkCst[T](s: CofiniteIntSet)(implicit alg: ZhegalkinAlgebra[T]): ZhegalkinCst[T] = {
    if (s.isEmpty)
      alg.empty
    else if (s.isUniverse)
      alg.universe
    else
      ZhegalkinCst(s)
  }

  /** Returns the xor of the two given Zhegalkin constants `c1` and `c2`. */
  def mkXor[T](c1: ZhegalkinCst[T], c2: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T]): ZhegalkinCst[T] = {
    // Note: use of union, inter, compl ensures a canonical representation.
    // a ⊕ b = (a ∪ b) - (a ∩ b) = (a ∪ b) ∩ ¬(a ∩ b)
    c1.union(c2).inter(c1.inter(c2).compl()(alg))
  }
}

/** Represents a set Zhegalkin constant (i.e. a set or co-set). A thin wrapper around [[CofiniteIntSet]]. */
case class ZhegalkinCst[T](s: CofiniteIntSet) {
  /** Returns `true` if `this` Zhegalkin constant is empty. */
  def isEmpty: Boolean = s.isEmpty

  /** Returns the complement of `this` Zhegalkin constant. */
  def compl()(implicit alg: ZhegalkinAlgebra[T]): ZhegalkinCst[T] = ZhegalkinCst.mkCst(CofiniteIntSet.complement(s))

  /** Returns the union of `this` Zhegalkin constant with `that`/ */
  def union(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T]): ZhegalkinCst[T] = ZhegalkinCst.mkCst(CofiniteIntSet.union(s, that.s))

  /** Returns the intersection of `this` Zhegalkin constant with `that`. */
  def inter(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T]): ZhegalkinCst[T] = ZhegalkinCst.mkCst(CofiniteIntSet.intersection(s, that.s))

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
