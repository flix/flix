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

/** Represents a set Zhegalkin constant of type T */
case class ZhegalkinCst[T](t: T) {
  /** Returns `true` if `this` Zhegalkin constant is empty. */
  def isEmpty()(implicit dom: Domain[T]): Boolean = dom.isEmpty(t)

  /** Returns the complement of `this` Zhegalkin constant. */
  def compl()(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(dom.complement(t))

  /** Returns the union of `this` Zhegalkin constant with `that`/ */
  def union(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(dom.union(this.t, that.t))

  /** Returns the intersection of `this` Zhegalkin constant with `that`. */
  def inter(that: ZhegalkinCst[T])(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinCst[T] = Zhegalkin.mkCst(dom.intersection(this.t, that.t))
}
