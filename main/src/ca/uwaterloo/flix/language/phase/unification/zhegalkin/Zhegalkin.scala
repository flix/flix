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

import ca.uwaterloo.flix.language.phase.unification.set.SetFormula
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.util.CofiniteIntSet

import scala.collection.immutable.SortedSet

object Zhegalkin {

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    */
  def toZhegalkin(f: SetFormula): ZhegalkinExpr = f match {
    case SetFormula.Univ => ZhegalkinExpr.one
    case SetFormula.Empty => ZhegalkinExpr.zero
    case Cst(c) => ZhegalkinExpr(ZhegalkinCst.empty, List(ZhegalkinTerm(ZhegalkinCst.universe, SortedSet(ZhegalkinVar(c, flexible = false)))))
    case Var(x) => ZhegalkinExpr(ZhegalkinCst.empty, List(ZhegalkinTerm(ZhegalkinCst.universe, SortedSet(ZhegalkinVar(x, flexible = true)))))
    case ElemSet(s) =>
      ZhegalkinExpr(ZhegalkinCst.mkCst(CofiniteIntSet.mkSet(s)), Nil)
    case Compl(f) => ZhegalkinExpr.zmkNot(toZhegalkin(f))
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(ZhegalkinExpr.mkInter)
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(ZhegalkinExpr.zmkUnion)
  }

  /** Returns the given Zhegalkin expression: `c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn` as a SetFormula. */
  def toSetFormula(z: ZhegalkinExpr): SetFormula = {
    /**
      * Returns *ALL* variables (both flexible and rigid) in the given Zhegalkin expression `e`.
      */
    def allVars(e0: ZhegalkinExpr): SortedSet[ZhegalkinVar] =
      e0.terms.foldLeft(SortedSet.empty[ZhegalkinVar]) {
        case (s, t) => s ++ t.vars
      }

    val variables = allVars(z)
    val disjs = variables.subsets().map(pos => {
      val insts = variables.iterator.map {
        case zv@ZhegalkinVar(i, isFlexible) =>
          val v = if (isFlexible) Var(i) else Cst(i)
          if (pos.contains(zv)) v else mkCompl(v)
      }.toList
      mkInterAll(fromCofiniteIntSet(evaluate(z, pos)) :: insts)
    })
    mkUnionAll(disjs.toList)
  }

  /** Evaluates `z` where all variables in `pos` are universe and all others are empty. */
  private def evaluate(z: ZhegalkinExpr, pos: SortedSet[ZhegalkinVar]): CofiniteIntSet = {
    val ZhegalkinExpr(cst, terms) = z
    (cst.s :: terms.map(evaluate(_, pos))).reduce(CofiniteIntSet.xor)
  }

  /** Evaluates `z` where all variables in `pos` are universe and all others are empty. */
  private def evaluate(z: ZhegalkinTerm, pos: SortedSet[ZhegalkinVar]): CofiniteIntSet = {
    val ZhegalkinTerm(cst, vars) = z

    def instVar(v: ZhegalkinVar): CofiniteIntSet = if (pos.contains(v)) CofiniteIntSet.universe else CofiniteIntSet.empty

    (cst.s :: vars.toList.map(instVar)).reduce(CofiniteIntSet.intersection(_, _: CofiniteIntSet))
  }

  /** Returns the [[SetFormula]] representation of `s`. */
  private def fromCofiniteIntSet(s: CofiniteIntSet): SetFormula = s match {
    case CofiniteIntSet.Set(s) => mkElemSet(s)
    case CofiniteIntSet.Compl(s) => mkCompl(mkElemSet(s))
  }

}
