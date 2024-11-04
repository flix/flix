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

/** Companion object for [[ZhegalkinExpr]] */
object ZhegalkinExpr {

  /** A Zhegalkin expression that represents the empty set, i.e. the zero element of the algebra. */
  val zero: ZhegalkinExpr = ZhegalkinExpr(ZhegalkinCst.empty, Nil)

  /** A Zhegalkin expression that represents the universe, i.e. the one element of the algebra. */
  val one: ZhegalkinExpr = ZhegalkinExpr(ZhegalkinCst.universe, Nil)

  /**
    * A smart constructor for Zhegalkin expressions that filters empty intersections.
    *
    * A Zhegalkin expression is of the form: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn
    */
  def mkZhegalkinExpr(cst: ZhegalkinCst, terms: List[ZhegalkinTerm]): ZhegalkinExpr = (cst, terms) match {
    case (ZhegalkinCst.empty, Nil) => ZhegalkinExpr.zero
    case (ZhegalkinCst.universe, Nil) => ZhegalkinExpr.one
    case _ =>
      // Construct a new polynomial.

      // Compute non-empty terms (i.e. terms where the coefficient is non-empty).
      val ts = terms.filter(t => !t.cst.s.isEmpty)

      // Special case: If ts is empty then this could be 0 or 1.
      if (ts.isEmpty) {
        if (cst eq ZhegalkinCst.empty) {
          return ZhegalkinExpr.zero
        }
        if (cst eq ZhegalkinCst.universe) {
          return ZhegalkinExpr.one
        }
      }

      // General case:
      ZhegalkinExpr(cst, ts)
  }

  /** Returns a Zhegalkin expression that represents a single variable, i.e. x ~~ Ø ⊕ (𝓤 ∩ x) */
  def mkVar(x: ZhegalkinVar): ZhegalkinExpr = ZhegalkinExpr(ZhegalkinCst.empty, List(ZhegalkinTerm(ZhegalkinCst.universe, SortedSet(x))))

  /**
    * Returns `true` if the given Zhegalkin expression `e` represents the empty set.
    *
    * Note: The representation of Zhegalkin polynomials is unique, hence we can use a simple equality check here.
    */
  def isEmpty(e: ZhegalkinExpr): Boolean = e == ZhegalkinExpr.zero

  /**
    * Returns the complement of the given Zhegalkin expression `e`.
    *
    * Uses identity laws to speed up the computation.
    */
  def mkCompl(e: ZhegalkinExpr): ZhegalkinExpr = {
    // ¬Ø = 𝓤
    if (e eq ZhegalkinExpr.zero) {
      return ZhegalkinExpr.one
    }

    // Performance: A common case.
    // Ø ⊕ (𝓤 ∩ x1 ∩ ...) ⊕ (𝓤 ∩ x2 ∩ ...) --> 𝓤 ⊕ (𝓤 ∩ x1 ∩ ...) ⊕ (𝓤 ∩ x2 ∩ ...)
    if ((e.cst eq ZhegalkinCst.empty) && e.terms.forall(t => t.cst eq ZhegalkinCst.universe)) {
      return e.copy(cst = ZhegalkinCst.universe)
    }

    // ¬a = 1 ⊕ a
    mkXor(ZhegalkinExpr.one, e)
  }

  /**
    * Returns the xor of the two Zhegalkin expressions.
    *
    * Uses identity laws and caching to speed up the computation.
    */
  def mkXor(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = {
    // 0 ⊕ a = a
    if (z1 eq ZhegalkinExpr.zero) {
      return z2
    }
    // a ⊕ 0 = a
    if (z2 eq ZhegalkinExpr.zero) {
      return z1
    }

    // Perform a cache lookup or an actual computation.
    ZhegalkinCache.lookupOrComputeXor(z1, z2, computeXor)
  }

  /**
    * Computes and returns the xor of the given two Zhegalkin expressions `e1` and `e2`.
    *
    * Does not use any simplification rules nor any cache.
    */
  private def computeXor(e1: ZhegalkinExpr, e2: ZhegalkinExpr): ZhegalkinExpr = (e1, e2) match {
    case (ZhegalkinExpr(c1, ts1), ZhegalkinExpr(c2, ts2)) =>
      val c = ZhegalkinCst.mkXor(c1, c2)
      // Eliminate duplicates: t ⊕ t = 0
      val tsr1 = (ts1 ++ ts2).groupBy(identity).collect { case (k, v) if v.size % 2 != 0 => k }.toList

      // Merge coefficients: (c1 ∩ x1 ∩ x2) ⊕ (c2 ∩ x1 ∩ x2)
      val grouped = tsr1.groupBy(_.vars).toList
      val resTerms = grouped.map {
        case (vars, l) =>
          val mergedCst: ZhegalkinCst = l.foldLeft(ZhegalkinCst.empty) { // Neutral element for Xor
            case (acc, t) => ZhegalkinCst.mkXor(acc, t.cst) // Distributive law: (c1 ∩ A) ⊕ (c2 ∩ A) = (c1 ⊕ c2) ∩ A
          }
          ZhegalkinTerm(mergedCst, vars)
      }
      mkZhegalkinExpr(c, resTerms)
  }

  /**
    * Returns the union of the given two Zhegalkin expressions `e1` and `e2`.
    *
    * Uses identity laws to speed up the computation.
    */
  def mkUnion(e1: ZhegalkinExpr, e2: ZhegalkinExpr): ZhegalkinExpr = {
    // Ø ∪ a = a
    if (e1 eq ZhegalkinExpr.zero) {
      return e2
    }
    // a ∪ Ø = a
    if (e2 eq ZhegalkinExpr.zero) {
      return e1
    }

    ZhegalkinCache.lookupOrComputeUnion(e1, e2, computeUnion)
  }

  /**
    * Computes the union of the given two Zhegalkin expressions `e1` and `e2`.
    */
  private def computeUnion(e1: ZhegalkinExpr, e2: ZhegalkinExpr): ZhegalkinExpr = {
    // a ∪ b = 1 ⊕ (1 ⊕ a)(1 ⊕ b)
    //mkXor(ZhegalkinExpr.one, mkInter(mkXor(ZhegalkinExpr.one, e1), mkXor(ZhegalkinExpr.one, e2)))

    // a ∪ b = a ⊕ b ⊕ (a ∩ b)
    mkXor(mkXor(e1, e2), mkInter(e1, e2))
  }

  /**
    * Returns the intersection of the given two Zhegalkin expressions `e1` and `e2`.
    *
    * Uses identity laws to speed up the computation.
    */
  def mkInter(e1: ZhegalkinExpr, e2: ZhegalkinExpr): ZhegalkinExpr = {
    // Ø ∩ a = Ø
    if (e1 eq ZhegalkinExpr.zero) {
      return ZhegalkinExpr.zero
    }
    // a ∩ Ø = Ø
    if (e2 eq ZhegalkinExpr.zero) {
      return ZhegalkinExpr.zero
    }
    // 𝓤 ∩ a = a
    if (e1 eq ZhegalkinExpr.one) {
      return e2
    }
    // a ∩ 𝓤 = a
    if (e2 eq ZhegalkinExpr.one) {
      return e1
    }

    // Perform a cache lookup or an actual computation.
    ZhegalkinCache.lookupOrComputeInter(e1, e2, computeInter)
  }

  //
  // (c1 ⊕ t11 ⊕ t12 ⊕ ... ⊕ t1n) ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //   =   (c1  ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t11 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t12 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //
  // TODO: Docs
  private def computeInter(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = z1 match {
    case ZhegalkinExpr(c1, ts1) =>
      val zero = mkInterConstantExpr(c1, z2)
      ts1.foldLeft(zero) {
        case (acc, z) => mkXor(acc, mkInterTermExpr(z, z2))
      }
  }

  //
  // c ∩ (c2 ∩ x1 ∩ x2 ∩ ... ∩ xn) = (c ∩ c2) ∩ x1 ∩ x2 ∩ ... ∩ xn)
  //
  // TODO: Docs
  private def mkInterConstantTerm(c: ZhegalkinCst, t: ZhegalkinTerm): ZhegalkinTerm = t match {
    case ZhegalkinTerm(c2, vars) =>
      ZhegalkinTerm(c.inter(c2), vars)
  }

  //
  // c ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m) = (c ∩ c2) ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m
  //
  // TODO: Docs
  private def mkInterConstantExpr(c: ZhegalkinCst, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val ts = terms.map(t => mkInterConstantTerm(c, t))
      mkZhegalkinExpr(c.inter(c2), ts)
  }

  //
  // t ∩ (c2 ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = (t ∩ c2) ⊕ (t ∩ t1) ⊕ (t ∩ t2) ⊕ ... ⊕ (t ∩ tn)
  //
  // TODO: Docs
  private def mkInterTermExpr(t: ZhegalkinTerm, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val zero: ZhegalkinExpr = mkZhegalkinExpr(ZhegalkinCst.empty, List(mkInterConstantTerm(c2, t)))
      terms.foldLeft(zero) {
        case (acc, t2) => mkXor(acc, mkZhegalkinExpr(ZhegalkinCst.empty, List(mkInterTermTerm(t, t2))))
      }
  }

  /**
    * Computes the intersection of the two given Zhegalkin terms `t1` and `t2`.
    *
    * {{{
    *   (c1 ∩ x11 ∩ ... ∩ x1n) ∩ (c2 ∩ x21 ∩ ... ∩ x2m) = (c1 ∩ c2) ∩ x11 ∩ ... ∩ x1n ∩ x21 ∩ ... ∩ x2m
    * }}}
    */
  //
  private def mkInterTermTerm(t1: ZhegalkinTerm, t2: ZhegalkinTerm): ZhegalkinTerm = (t1, t2) match {
    case (ZhegalkinTerm(c1, vars1), ZhegalkinTerm(c2, vars2)) =>
      ZhegalkinTerm(c1.inter(c2), vars1 ++ vars2)
  }


}

/** Represents a Zhegalkin expr: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn */
case class ZhegalkinExpr(cst: ZhegalkinCst, terms: List[ZhegalkinTerm]) {

  // Representation Invariants:
  //  if (this == ZhegalkinExpr.zero) {
  //    assert(this eq ZhegalkinExpr.zero)
  //  }
  //  if (this == ZhegalkinExpr.one) {
  //    assert(this eq ZhegalkinExpr.one)
  //  }

  /**
    * Returns all flexible variables in the given Zhegalkin expression `e`.
    */
  def freeVars: SortedSet[ZhegalkinVar] = terms.foldLeft(SortedSet.empty[ZhegalkinVar]) {
    case (acc, term) => acc ++ term.freeVars
  }

  /**
    * Maps the given function `f` over the variables in `this` Zhegalkin expression.
    *
    * {{{
    *   map(f, c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = c ⊕ map(f, t1) ⊕ map(f, t2) ⊕ ... ⊕ map(f, tn)
    * }}}
    *
    */
  def map(f: Int => ZhegalkinExpr): ZhegalkinExpr = {
    if (terms == Nil) {
      return this
    }

    terms.foldLeft(ZhegalkinExpr.mkZhegalkinExpr(cst, Nil)) {
      case (acc, term) => ZhegalkinExpr.mkXor(acc, term.map(f))
    }
  }

  /** Returns a human-readable string representation of `this` Zhegalkin expression. Must only be used for debugging. */
  override def toString: String =
    if (terms.isEmpty)
      cst.toString
    else
      s"$cst ⊕ ${terms.map(t => s"($t)").mkString(" ⊕ ")}"

}
