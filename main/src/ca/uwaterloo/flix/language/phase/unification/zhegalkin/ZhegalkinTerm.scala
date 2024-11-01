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

import scala.collection.immutable.SortedSet

/** Represents a Zhegalkin term: c ∩ x1 ∩ x2 ∩ ... ∩ xn */
case class ZhegalkinTerm(cst: ZhegalkinCst, vars: SortedSet[ZhegalkinVar]) {

  /**
    * Returns the free (i.e. flexible) variables in `this` Zhegalkin term.
    */
  def freeVars: SortedSet[ZhegalkinVar] = vars.filter(x => x.flexible)

  /**
    * Maps the given function `f` over the variables in `this` Zhegalkin term.
    *
    * {{{
    *   map(f, c ∩ x1 ∩ x2 ∩ ... ∩ xn) = c ∩ map(f, x1) ∩ map(f, x2) ∩ ... ∩ map(f, xn)
    * }}}
    *
    */
  def map(f: Int => ZhegalkinExpr): ZhegalkinExpr = {
    vars.foldLeft(ZhegalkinExpr(cst, Nil)) {
      case (acc, x) => ZhegalkinExpr.zmkInter(f(x.id), acc)
    }
  }

  /** Returns a human-readable string representation of `this` Zhegalkin term. Must only be used for debugging. */
  override def toString: String =
    if (vars.isEmpty)
      cst.toString
    else
      s"$cst ∩ ${vars.mkString(" ∩ ")}"

}
