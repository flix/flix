/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.set.SetFormula
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolLattice, CofiniteIntSet}

import scala.collection.immutable.SortedSet

object Zhegalkin {

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    */
  def toZhegalkin(f: SetFormula)(implicit alg: ZhegalkinAlgebra[CofiniteIntSet], lat: BoolLattice[CofiniteIntSet]): ZhegalkinExpr[CofiniteIntSet] = f match {
    case SetFormula.Univ => alg.one
    case SetFormula.Empty => alg.zero
    case Cst(c) => ZhegalkinExpr(lat.Bot, List(ZhegalkinTerm(lat.Top, SortedSet(ZhegalkinVar(c, flexible = false)))))
    case Var(x) => ZhegalkinExpr(lat.Bot, List(ZhegalkinTerm(lat.Top, SortedSet(ZhegalkinVar(x, flexible = true)))))
    case ElemSet(s) =>
      ZhegalkinExpr(CofiniteIntSet.mkSet(s), Nil)
    case Compl(f1) => ZhegalkinExpr.mkCompl(toZhegalkin(f1))
    case Inter(l) =>
      val polys = l.toList.map(x => toZhegalkin(x)(alg, lat))
      polys.reduce[ZhegalkinExpr[CofiniteIntSet]] {
        case (x, y) =>  ZhegalkinExpr.mkInter(x, y)(alg, lat)
      }
    case Union(l) =>
      val polys = l.toList.map(x => toZhegalkin(x)(alg, lat))
      polys.reduce[ZhegalkinExpr[CofiniteIntSet]] {
        case (x, y) =>  ZhegalkinExpr.mkUnion(x, y)(alg, lat)
      }
    case Xor(l) =>
      val polys = l.map(x => toZhegalkin(x)(alg, lat))
      polys.reduce[ZhegalkinExpr[CofiniteIntSet]] {
        case (x, y) =>  ZhegalkinExpr.mkXor(x, y)(alg, lat)
      }
  }

  /** Returns the given Zhegalkin expression as a SetFormula. */
  def toSetFormula(z: ZhegalkinExpr[CofiniteIntSet]): SetFormula = {
    def visitCst(cst: CofiniteIntSet): SetFormula = cst match {
      case CofiniteIntSet.Set(s) => SetFormula.mkElemSet(s)
      case CofiniteIntSet.Compl(s) => SetFormula.mkCompl(SetFormula.mkElemSet(s))
    }

    def visitTerm(term: ZhegalkinTerm[CofiniteIntSet]): SetFormula = term match {
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
