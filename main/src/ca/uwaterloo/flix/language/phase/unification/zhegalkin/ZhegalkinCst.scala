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

/** Represents a set Zhegalkin constant (i.e. a set or co-set). A thin wrapper around [[CofiniteIntSet]]. */
case class ZhegalkinCst[T](s: CofiniteIntSet) {
  /** Returns `true` if `this` Zhegalkin constant is empty. */
  def isEmpty: Boolean = s.isEmpty

  /** Returns the complement of `this` Zhegalkin constant. */
  def compl()(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(CofiniteIntSet.complement(s))

  /** Returns the union of `this` Zhegalkin constant with `that`/ */
  def union(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(CofiniteIntSet.union(s, that.s))

  /** Returns the intersection of `this` Zhegalkin constant with `that`. */
  def inter(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(CofiniteIntSet.intersection(s, that.s))
}
