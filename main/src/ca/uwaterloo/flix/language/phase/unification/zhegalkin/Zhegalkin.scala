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
import ca.uwaterloo.flix.util.collection.CofiniteIntSet

import scala.collection.immutable.SortedSet

object Zhegalkin {

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

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    */
  def toZhegalkin[T](f: SetFormula)(implicit alg: ZhegalkinAlgebra[T], dom: Domain[T]): ZhegalkinExpr[T] = f match {
    case SetFormula.Univ => alg.one
    case SetFormula.Empty => alg.zero
    case Cst(c) => ZhegalkinExpr(alg.empty, List(ZhegalkinTerm(alg.universe, SortedSet(ZhegalkinVar(c, flexible = false)))))
    case Var(x) => ZhegalkinExpr(alg.empty, List(ZhegalkinTerm(alg.universe, SortedSet(ZhegalkinVar(x, flexible = true)))))
    case ElemSet(s) =>
      ZhegalkinExpr(mkCst(CofiniteIntSet.mkSet(s)), Nil)
    case Compl(f1) => ZhegalkinExpr.mkCompl(toZhegalkin(f1))
    case Inter(l) =>
      val polys = l.toList.map(x => toZhegalkin(x)(alg, dom))
      polys.reduce[ZhegalkinExpr[T]] {
        case (x, y) =>  ZhegalkinExpr.mkInter(x, y)(alg, dom)
      }
    case Union(l) =>
      val polys = l.toList.map(x => toZhegalkin(x)(alg, dom))
      polys.reduce[ZhegalkinExpr[T]] {
        case (x, y) =>  ZhegalkinExpr.mkUnion(x, y)(alg, dom)
      }
    case Xor(l) =>
      val polys = l.map(x => toZhegalkin(x)(alg, dom))
      polys.reduce[ZhegalkinExpr[T]] {
        case (x, y) =>  ZhegalkinExpr.mkXor(x, y)(alg, dom)
      }
  }

  /** Returns the given Zhegalkin expression as a SetFormula. */
  def toSetFormula[T](z: ZhegalkinExpr[T]): SetFormula = {
    def visitCst(cst: ZhegalkinCst[T]): SetFormula = cst match {
      case ZhegalkinCst(c) => c match {
        case CofiniteIntSet.Set(s) => SetFormula.mkElemSet(s)
        case CofiniteIntSet.Compl(s) => SetFormula.mkCompl(SetFormula.mkElemSet(s))
      }
    }

    def visitTerm(term: ZhegalkinTerm[T]): SetFormula = term match {
      case ZhegalkinTerm(cst, vars) =>
        // c ∩ x1 ∩ x2 ∩ ... ∩ xn
        val flexVars = vars.foldLeft(SetFormula.Univ: SetFormula) {
          case (acc, zvar) if zvar.flexible => SetFormula.mkInter(acc, SetFormula.Var(zvar.id))
          case (acc, _) => acc
        }
        val rigidVars = vars.foldLeft(SetFormula.Univ: SetFormula) {
          case (acc, zvar) if !zvar.flexible => SetFormula.mkInter(acc, SetFormula.Cst(zvar.id))
          case (acc, _) => acc
        }
        SetFormula.mkInter3(visitCst(cst), flexVars, rigidVars)
    }

    z match {
      case ZhegalkinExpr(cst, terms) =>
        // `c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn`
        terms.foldLeft(visitCst(cst): SetFormula) {
          case (acc, term) => SetFormula.mkXor2(acc, visitTerm(term))
        }
    }
  }

}
