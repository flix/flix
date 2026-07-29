/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.util.collection.ListOps

import scala.collection.immutable.IntMap

object SetSubstitution {

  /** The empty substitution (`[]`). */
  val empty: SetSubstitution = SetSubstitution(IntMap.empty)

  /** Returns the singleton substitution of `x` mapped to `t` (`[x -> t]`). */
  def singleton(x: Int, t: SetFormula): SetSubstitution = SetSubstitution(IntMap(x -> t))

}

/**
  * Represents a substitution from [[SetFormula.Var]] (represented as integers) to [[SetFormula]].
  *
  * A substitution contains a partial map from variables to formulas. This partial map implies a
  * total map by leaving un-mapped variables as they are (the identity mapping).
  *
  * Substitutions are written `[x -> t1, y -> t2, ..]`.
  * Any individual pair, `x -> t1`, is called a binding or a mapping.
  */
case class SetSubstitution(m: IntMap[SetFormula]) {

  /** Returns `true` if `this` is the empty substitution (`[]`). */
  def isEmpty: Boolean = m.isEmpty

  /** Applies `this` substitution to `f`. Replaces [[SetFormula.Var]] according to `this`. */
  def apply(f: SetFormula): SetFormula = {
    if (m.isEmpty) {
      f
    } else {
      visit(f)
    }
  }

  private def visit(f0: SetFormula): SetFormula = f0 match {
    // Maintain and exploit reference equality for performance.
    case SetFormula.Univ => SetFormula.Univ

    case SetFormula.Empty => SetFormula.Empty

    case SetFormula.Cst(_) => f0

    case SetFormula.ElemSet(_) => f0

    case SetFormula.Var(x) =>
      // Performance: A non-capturing default avoids a thunk allocation per lookup.
      val t = m.getOrElse(x, null)
      if (t == null) f0 else t

    case SetFormula.Compl(f1) =>
      val inner = visit(f1)
      if (inner eq f1) f0 else SetFormula.mkCompl(inner)

    case SetFormula.Inter(l) =>
      val inner = l.mapWithReuse(visit)
      if (inner eq l) f0 else SetFormula.Inter(inner)

    case SetFormula.Union(l) =>
      val inner = l.mapWithReuse(visit)
      if (inner eq l) f0 else SetFormula.Union(inner)

    case SetFormula.Xor(l) =>
      val inner = ListOps.mapWithReuse(l)(visit)
      if (inner eq l) f0 else SetFormula.mkXorAll(inner)
  }

  /** Applies `this` to both sides of `eq`. */
  def apply(eq: Equation): Equation = {
    val Equation(f1, f2, status, loc) = eq
    val app1 = apply(f1)
    val app2 = apply(f2)
    // Maintain and exploit reference equality for performance.
    if ((app1 eq f1) && (app2 eq f2)) eq else Equation.mk(app1, app2, loc, status)
  }

  /** Applies `this` to each [[Equation]] in `eqs`. */
  def apply(eqs: List[Equation]): List[Equation] =
    if (m.isEmpty) eqs else eqs.map(apply)

  /** Returns the number of bindings in `this`. */
  def numberOfBindings: Int = m.size

  /** Returns a combined substitution, first applying `that` and then applying `this`. */
  def @@(that: SetSubstitution): SetSubstitution = {
    if (this.m.isEmpty)
      that
    else if (that.m.isEmpty)
      this
    else {
      // Add all bindings in `that` with `this` applied to the result.
      // Performance: Start from `that.m` and only update changed bindings.
      var result = that.m
      for ((x, f) <- that.m) {
        val t = this.apply(f)
        if (!(t eq f)) {
          result = result.updated(x, t)
        }
      }

      // Add all bindings in `this` that are not in `that`.
      for ((x, f) <- this.m) {
        if (!that.m.contains(x)) {
          result = result.updated(x, f)
        }
      }

      SetSubstitution(result)
    }
  }

  /** Returns a multi-line, string representation of `this`. */
  override def toString: String = {
    val indentation = 4
    val indentString = " ".repeat(indentation)

    val sb = new StringBuilder()
    sb.append("SetSubstitution{\n")
    val sorted = m.toList.sortBy[(Int, Int)] { case (x, t) => (t.size, x) }
    for ((x, f) <- sorted) {
      sb.append(indentString)
      sb.append("x")
      sb.append(x.toString)
      sb.append(" -> ")
      sb.append(f)
      sb.append("\n")
    }
    sb.append("}\n")
    sb.toString()
  }

}
