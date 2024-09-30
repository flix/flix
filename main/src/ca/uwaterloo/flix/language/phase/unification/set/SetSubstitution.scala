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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private object SetSubstitution {

  /** The empty substitution (`[]`). */
  val empty: SetSubstitution = SetSubstitution(Map.empty)

  /** Returns the singleton substitution of `x` mapped to `t` (`[x -> t]`). */
  def singleton(x: Int, t: SetFormula): SetSubstitution = SetSubstitution(Map(x -> t))

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
case class SetSubstitution(m: Map[Int, SetFormula]) {

  /** Returns `true` if `this` is the empty substitution (`[]`). */
  def isEmpty: Boolean = m.isEmpty

  /** Applies `this` substitution to `f`. Replaces [[SetFormula.Var]] according to `this`. */
  def apply(f: SetFormula): SetFormula =
    if (m.isEmpty) f else applyInternal(f)

  /** Applies `this` substitution to `f`. */
  private def applyInternal(f: SetFormula): SetFormula = f match {
    // Maintain and exploit reference equality for performance.
    case SetFormula.Univ => SetFormula.Univ
    case SetFormula.Empty => SetFormula.Empty
    case cst@SetFormula.Cst(_) => cst
    case elemSet@SetFormula.ElemSet(_) => elemSet

    case variable@SetFormula.Var(x) => m.getOrElse(x, variable)

    case compl@SetFormula.Compl(t1) =>
      val t1Applied = applyInternal(t1)
      if (t1Applied eq t1) compl else SetFormula.mkCompl(t1Applied)

    case inter@SetFormula.Inter(_, _, posVars, _, _, negVars, Nil)
      if posVars.forall(v => !this.m.contains(v.x)) && negVars.forall(v => !this.m.contains(v.x)) =>
      inter

    case SetFormula.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = ListBuffer.empty[SetFormula]
      for (x <- posVars) {
        val x1 = applyInternal(x)
        if (x1 == SetFormula.Empty) return SetFormula.Empty
        ts += x1
      }
      for (x <- negVars) {
        val x1 = applyInternal(x)
        if (x1 == SetFormula.Univ) return SetFormula.Empty
        ts += SetFormula.mkCompl(x1)
      }
      for (t <- rest) {
        val t1 = applyInternal(t)
        if (t1 == SetFormula.Empty) return SetFormula.Empty
        ts += t1
      }
      val simpleInter = SetFormula.Inter(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, Nil)
      SetFormula.mkInterAll(simpleInter :: ts.toList)

    case union@SetFormula.Union(_, _, posVars, _, _, negVars, Nil)
      if posVars.forall(v => !this.m.contains(v.x)) && negVars.forall(v => !this.m.contains(v.x)) =>
      union

    case SetFormula.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = ListBuffer.empty[SetFormula]
      for (x <- posVars) {
        val x1 = applyInternal(x)
        if (x1 == SetFormula.Univ) return SetFormula.Univ
        ts += x1
      }
      for (x <- negVars) {
        val x1 = applyInternal(x)
        if (x1 == SetFormula.Empty) return SetFormula.Univ
        ts += SetFormula.mkCompl(x1)
      }
      for (t <- rest) {
        val t1 = applyInternal(t)
        if (t1 == SetFormula.Univ) return SetFormula.Univ
        ts += t1
      }
      val simpleUnion = SetFormula.Union(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, Nil)
      SetFormula.mkUnionAll(simpleUnion :: ts.toList)
  }

  /** Applies `this` to both sides of `eq`. */
  def apply(eq: Equation): Equation = {
    val Equation(t1, t2, loc) = eq
    val app1 = apply(t1)
    val app2 = apply(t2)
    // Maintain and exploit reference equality for performance.
    if ((app1 eq t1) && (app2 eq t2)) eq else Equation.mk(app1, app2, loc)
  }

  /** Applies `this` to each [[Equation]] in `eqs`. */
  def apply(eqs: List[Equation]): List[Equation] =
    if (m.isEmpty) eqs else eqs.map(apply)

  /** Returns the number of bindings in `this`. */
  def numberOfBindings: Int = m.size

  /** Returns the sum of the sizes of the formulas in `this`. */
  def totalFormulaSize: Int = m.values.map(_.size).sum

  /** Returns a combined substitution, first applying `that` and then applying `this`. */
  def @@(that: SetSubstitution): SetSubstitution = {
    if (this.m.isEmpty) that
    else if (that.m.isEmpty) this
    else {
      val result = mutable.Map.empty[Int, SetFormula]
      // Add all bindings in `that` with `this` applied to the result.
      for ((x, t) <- that.m) result.update(x, this.apply(t))
      // Add all bindings in `this` that are not in `that`.
      for ((x, t) <- this.m) if (!that.m.contains(x)) result.update(x, t)
      SetSubstitution(result.toMap)
    }
  }

  /** Returns a multi-line, string representation of `this`. */
  override def toString: String = {
    val indentation = 4
    val indentString = " ".repeat(indentation)

    val sb = new StringBuilder()
    sb.append("SetSubstitution{\n")
    val sorted = m.toList.sortBy[(Int, Int)] { case (x, t) => (t.size, x) }
    for ((x, t) <- sorted) {
      sb.append(indentString)
      sb.append("x")
      sb.append(x.toString)
      sb.append(" -> ")
      sb.append(t)
      sb.append("\n")
    }
    sb.append("}")
    sb.toString()
  }

}
