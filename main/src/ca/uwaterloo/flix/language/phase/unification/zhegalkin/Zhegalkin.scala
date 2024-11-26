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
    case Compl(f) => ZhegalkinExpr.mkCompl(toZhegalkin(f))
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(ZhegalkinExpr.mkInter)
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(ZhegalkinExpr.mkUnion)
    case Xor(other) =>
      val polys = other.map(toZhegalkin)
      polys.reduce(ZhegalkinExpr.mkXor)
  }

  /** Returns the given Zhegalkin expression as a SetFormula. */
  def toSetFormula(z: ZhegalkinExpr): SetFormula = {
    def visitCst(cst: ZhegalkinCst): SetFormula = cst match {
      case ZhegalkinCst(s) => s match {
        case CofiniteIntSet.Set(s) => SetFormula.mkElemSet(s)
        case CofiniteIntSet.Compl(s) => SetFormula.mkCompl(SetFormula.mkElemSet(s))
      }
    }

    def visitTerm(term: ZhegalkinTerm): SetFormula = term match {
      case ZhegalkinTerm(cst, vars) =>
        // c ∩ x1 ∩ x2 ∩ ... ∩ xn
        val flexVars = vars.toList.flatMap{
          case zvar if zvar.flexible => Some(SetFormula.Var(zvar.id))
          case _ => None
        }
        val rigidVars = vars.toList.flatMap{
          case zvar if !zvar.flexible => Some(SetFormula.Cst(zvar.id))
          case _ => None
        }
        SetFormula.mkInterAll(visitCst(cst) :: flexVars ::: rigidVars)
    }

    z match {
      case ZhegalkinExpr(cst, terms) =>
        // `c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn`
        SetFormula.mkXorDirectAll(visitCst(cst) :: terms.map(visitTerm))
    }
  }

}
