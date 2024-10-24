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

  /** Represents a variable. */
  case class ZhegalkinVar(v: Int) extends Ordered[ZhegalkinVar] {
    override def toString: String = s"x$v"

    override def compare(that: ZhegalkinVar): Int = this.v.compare(that.v)
  }

  /** Represents a set Zhegalkin constant (i.e. a set or co-set) */
  case class ZhegalkinConstant(s: CofiniteIntSet) {
    def compl: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.complement(s))
    def union(that: ZhegalkinConstant): ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.union(s, that.s))
    def inter(that: ZhegalkinConstant): ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.intersection(s, that.s))

    override def toString: String = {
      if (s.isEmpty) "Ø"
      else if (s.isUniverse) "𝓤"
      else s match {
        case CofiniteIntSet.Set(xs) => s"{${xs.mkString(", ")}}"
        case CofiniteIntSet.Compl(xs) => s"¬{${xs.mkString(", ")}}"
      }
    }
  }

  private val ZEmpty: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.empty)
  private val ZUniverse: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.universe)

  /** Represents a Zhegalkin term: c ∩ x1 ∩ x2 ∩ ... ∩ xn */
  case class ZhegalkinTerm(cst: ZhegalkinConstant, vars: SortedSet[ZhegalkinVar]) {
    override def toString: String =
      if (vars.isEmpty)
        cst.toString
      else
        s"$cst ∩ ${vars.mkString(" ∩ ")}"
  }

  /** Represents a Zhegalkin expr: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn */
  case class ZhegalkinExpr(cst: ZhegalkinConstant, terms: List[ZhegalkinTerm]) {
    override def toString: String =
      if (terms.isEmpty)
        cst.toString
      else
        s"$cst ⊕ ${terms.map(t => s"($t)").mkString(" ⊕ ")}"
  }

  /**
    * A smart constructor to filter empty intersections.
    */
  private def mkZhegalkinExpr(cst: ZhegalkinConstant, terms: List[ZhegalkinTerm]): ZhegalkinExpr =
    ZhegalkinExpr(cst, terms.filter(t => !t.cst.s.isEmpty))

  /** Returns the xor of the two Zhegalkin constants */
  private def mkXor(c1: ZhegalkinConstant, c2: ZhegalkinConstant): ZhegalkinConstant = {
    // a ⊕ b = (a ⊕ b) - (a ∩ b) = (a ⊕ b) ∩ ¬(a ∩ b)
    c1.union(c2).inter(c1.inter(c2).compl)
  }

  /** Returns the xor of the two Zhegalkin expressions. */
  private def mkXor(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = (z1, z2) match {
    case (ZhegalkinExpr(c1, ts1), ZhegalkinExpr(c2, ts2)) =>
      val c = mkXor(c1, c2)
      val ts = (ts1 ++ ts2).groupBy(identity).collect { case (k, v) if v.size % 2 != 0 => k }.toList
      mkZhegalkinExpr(c, ts)
  }

  /** Returns the complement of the Zhegalkin expr. */
  private def zmkCompl(a: ZhegalkinExpr): ZhegalkinExpr =
    // ¬a = 1 ⊕ a
    mkXor(ZhegalkinExpr(ZUniverse, Nil), a)

  //
  // (c1 ⊕ t11 ⊕ t12 ⊕ ... ⊕ t1n) ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //   =   (c1  ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t11 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t12 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //
  private def mkInter(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = z1 match {
    case ZhegalkinExpr(c1, ts1) =>
      val zero = mkInterConstantExpr(c1, z2)
      ts1.foldLeft(zero) {
        case (acc, z) => mkXor(acc, mkInterTermExpr(z, z2))
      }
  }

  //
  // c ∩ (c2 ∩ x1 ∩ x2 ∩ ... ∩ xn) = (c ∩ c2) ∩ x1 ∩ x2 ∩ ... ∩ xn)
  //
  private def mkInterConstantTerm(c: ZhegalkinConstant, t: ZhegalkinTerm): ZhegalkinTerm = t match {
    case ZhegalkinTerm(c2, vars) =>
      ZhegalkinTerm(c.inter(c2), vars)
  }

  //
  // c ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m) = (c ∩ c2) ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m
  //
  private def mkInterConstantExpr(c: ZhegalkinConstant, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val ts = terms.map(t => mkInterConstantTerm(c, t))
      mkZhegalkinExpr(c.inter(c2), ts)
  }

  //
  // t ∩ (c2 ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = (t ∩ c2) ⊕ (t ∩ t1) ⊕ (t ∩ t2) ⊕ ... ⊕ (t ∩ tn)
  //
  private def mkInterTermExpr(t: ZhegalkinTerm, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val zero: ZhegalkinExpr = mkZhegalkinExpr(ZEmpty, List(mkInterConstantTerm(c2, t)))
      terms.foldLeft(zero) {
        case (acc, t2) => mkXor(acc, mkZhegalkinExpr(ZEmpty, List(mkInterTermTerm(t, t2))))
      }
  }

  // (c1 ∩ x11 ∩ x12 ∩ ... ∩ x1n) ∩ (c2 ∩ x21 ∩ x22 ∩ ... ∩ x2m)
  private def mkInterTermTerm(t1: ZhegalkinTerm, t2: ZhegalkinTerm): ZhegalkinTerm = (t1, t2) match {
    case (ZhegalkinTerm(c1, vars1), ZhegalkinTerm(c2, vars2)) =>
      ZhegalkinTerm(c1.inter(c2), vars1 ++ vars2)
  }

  /** Returns the union of the two Zhegalkin expressions. */
  private def mkUnion(a: ZhegalkinExpr, b: ZhegalkinExpr): ZhegalkinExpr = {
    /** a ⊕ b = a ⊕ b ⊕ (a ∩ b) */
    mkXor(mkXor(a, b), mkInter(a, b))
  }

  def map(f: Int => ZhegalkinExpr, z: ZhegalkinExpr): ZhegalkinExpr = mapExpr(f, z)

  //
  // map(f, c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = c ⊕ map(f, t1) ⊕ map(f, t2) ⊕ ... ⊕ map(f, tn)
  //
  private def mapExpr(f: Int => ZhegalkinExpr, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(_, Nil) => z

    case ZhegalkinExpr(cst, terms) => terms.foldLeft(ZhegalkinExpr(cst, Nil)) {
      case (acc, term) => mkXor(acc, mapTerm(f, term))
    }
  }

  //
  // map(f, c ∩ x1 ∩ x2 ∩ ... ∩ xn) = c ∩ map(f, x1) ∩ map(f, x2) ∩ ... ∩ map(f, xn)
  //
  private def mapTerm(f: Int => ZhegalkinExpr, t: ZhegalkinTerm): ZhegalkinExpr = t match {
    case ZhegalkinTerm(cst, vars) => vars.foldLeft(ZhegalkinExpr(cst, Nil)) {
      case (acc, x) => mkInter(f(x.v), acc)
    }
  }

  private def isEmpty(z: ZhegalkinExpr): Boolean = z match {
    case ZhegalkinExpr(cst, Nil) => cst == ZEmpty
    case _ => false
  }

  private def isSatisfiable(z: ZhegalkinExpr): Boolean = !isEmpty(z)

  // TODO: Need to distinguish free and rigid variables.
  private def freeVars(z: ZhegalkinExpr): SortedSet[Int] = ???

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    */
  def toZhegalkin(f: SetFormula): ZhegalkinExpr = f match {
    case SetFormula.Univ => ZhegalkinExpr(ZUniverse, Nil)
    case SetFormula.Empty => ZhegalkinExpr(ZEmpty, Nil)
    case Cst(c) => ZhegalkinExpr(ZEmpty, List(ZhegalkinTerm(ZUniverse, SortedSet(ZhegalkinVar(c))))) // We treat uninterpreted constants as vars.
    case Var(x) => ZhegalkinExpr(ZEmpty, List(ZhegalkinTerm(ZUniverse, SortedSet(ZhegalkinVar(x)))))
    case ElemSet(s) =>
      ZhegalkinExpr(ZhegalkinConstant(CofiniteIntSet.mkSet(s)), Nil)
    case Compl(f) => zmkCompl(toZhegalkin(f))
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(mkInter)
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(mkUnion)
  }

}

