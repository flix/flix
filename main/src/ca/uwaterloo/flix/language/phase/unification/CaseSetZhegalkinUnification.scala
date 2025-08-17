/*
 *  Copyright 2025 Matthew Lutze, Alex Asafov
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.FiniteSet.{FiniteSet, LatticeOps}
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolLattice, SveAlgorithm}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{ZhegalkinAlgebra, ZhegalkinExpr, ZhegalkinTerm, ZhegalkinVar}

import scala.collection.immutable.{IntMap, SortedSet}

object CaseSetZhegalkinUnification {

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv, cases: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym)(implicit scope: Scope): Option[Substitution] = {
    if (tpe1 eq tpe2) {
      return Some(Substitution.empty)
    }

    (tpe1, tpe2) match {
      case (t1@Type.Var(x, _), t2) if renv0.isFlexible(x) && !t2.typeVars.contains(t1) =>
        return Some(Substitution.singleton(x, t2))

      case (t1, t2@Type.Var(x, _)) if renv0.isFlexible(x) && !t1.typeVars.contains(t2) =>
        return Some(Substitution.singleton(x, t1))

      case _ => // nop
    }

    val (env, univ) = mkEnv(List(tpe1, tpe2), cases)
    val input1 = fromCaseType(tpe1, env, univ)
    val input2 = fromCaseType(tpe2, env, univ)
    val renv = liftRigidityEnv(renv0, env)

    val lat: LatticeOps[Int] = new LatticeOps(univ)
    val alg: ZhegalkinAlgebra[FiniteSet[Int]] = new ZhegalkinAlgebra[FiniteSet[Int]](lat)

    val f1 = toZhegalkin(input1)(renv, alg, lat)
    val f2 = toZhegalkin(input2)(renv, alg, lat)
    val res = SveAlgorithm.unify(f1, f2, renv)(alg)
    res.map(subst => subst.m.foldLeft(IntMap.empty[SetFormula]) {
      case (acc, (x1, e)) => acc.updated(x1, toSetFormula(e)(univ))
    }).map {
      CaseSetSubstitution(_).toTypeSubstitution(enumSym, env)
    }
  }

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    * The universe of the Zhegalkin algebra must be a finite set of cases of restrictable enum.
    */
  private def toZhegalkin(f: SetFormula)(implicit renv: Set[Int], alg: ZhegalkinAlgebra[FiniteSet[Int]], lat: BoolLattice[FiniteSet[Int]]): ZhegalkinExpr[FiniteSet[Int]] = f match {
    case SetFormula.Cst(s) => ZhegalkinExpr(FiniteSet(s), Nil)
    case SetFormula.Var(x) => ZhegalkinExpr(lat.Bot, List(ZhegalkinTerm(lat.Top, SortedSet(ZhegalkinVar(x, flexible = !renv.contains(x))))))
    case SetFormula.Not(f1) => ZhegalkinExpr.mkCompl(toZhegalkin(f1))
    case SetFormula.And(x, y) => ZhegalkinExpr.mkInter(toZhegalkin(x), toZhegalkin(y))(alg, lat)
    case SetFormula.Or(x, y) => ZhegalkinExpr.mkUnion(toZhegalkin(x), toZhegalkin(y))(alg, lat)
    case SetFormula.Xor(x, y) => ZhegalkinExpr.mkXor(toZhegalkin(x), toZhegalkin(y))(alg, lat)
  }

  /** Returns the given Zhegalkin expression as a SetFormula. */
  private def toSetFormula(z: ZhegalkinExpr[FiniteSet[Int]])(implicit univ: Set[Int]): SetFormula = {
    def visitCst(cst: FiniteSet[Int]): SetFormula = cst match {
      case FiniteSet(s) => SetFormula.mkCst(s)
    }

    def visitTerm(term: ZhegalkinTerm[FiniteSet[Int]]): SetFormula = term match {
      case ZhegalkinTerm(cst, vars) =>
        // `c ∩ v1 ∩ v2 ∩ ... ∩ vn`
        val varsInter = vars.foldLeft(SetFormula.mkUni(): SetFormula) {
          case (acc, zvar) => SetFormula.mkAnd(acc, SetFormula.Var(zvar.id))
        }
        SetFormula.mkAnd(visitCst(cst), varsInter)
    }

    z match {
      case ZhegalkinExpr(cst, terms) =>
        // `c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn`
        terms.foldLeft(visitCst(cst): SetFormula) {
          case (acc, term) =>
            SetFormula.mkXor(acc, visitTerm(term))
        }
    }
  }
}
