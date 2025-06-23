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

import ca.uwaterloo.flix.language.phase.unification.shared.BoolLattice

import java.util.Objects
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/** Companion object for [[ZhegalkinExpr]] */
object ZhegalkinExpr {

  /**
    * A smart constructor for Zhegalkin expressions that filters empty intersections.
    *
    * A Zhegalkin expression is of the form: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn
    */
  def mkZhegalkinExpr[T](cst: T, terms: List[ZhegalkinTerm[T]])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    if (cst == lat.Bot && terms.isEmpty) {
      return alg.zero
    }
    if (cst == lat.Top && terms.isEmpty) {
      return alg.one
    }

    // Construct a new polynomial.

    // Compute non-empty terms (i.e. terms where the coefficient is non-empty).
    val ts = terms.filter(t => !lat.isBot(t.cst))

    // Special case: If ts is empty then this could be 0 or 1.
    if (ts.isEmpty) {
      if (lat.isBot(cst)) {
        return alg.zero
      }
      if (lat.isTop(cst)) {
        return alg.one
      }
    }

    // General case:
    ZhegalkinExpr(cst, ts.sortBy(t => t.vars.size))
  }

  /** Returns a Zhegalkin expression that represents a single variable, i.e. x ~~ Ø ⊕ (𝓤 ∩ x) */
  def mkVar[T](x: ZhegalkinVar)(implicit lat: BoolLattice[T]): ZhegalkinExpr[T] =
    ZhegalkinExpr(lat.Bot, List(ZhegalkinTerm(lat.Top, SortedSet(x))))

  /**
    * Returns `true` if the given Zhegalkin expression `e` represents the empty set.
    *
    * Note: The representation of Zhegalkin polynomials is unique, hence we can use a simple equality check here.
    */
  def isEmpty[T](e: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T]): Boolean = e == alg.zero

  /**
    * Returns the complement of the given Zhegalkin expression `e`.
    *
    * Uses identity laws to speed up the computation.
    */
  def mkCompl[T](e: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    // ¬Ø = 𝓤
    if (e eq alg.zero) {
      return alg.one
    }

    // Performance: A common case.
    // Ø ⊕ (𝓤 ∩ x1 ∩ ...) ⊕ (𝓤 ∩ x2 ∩ ...) --> 𝓤 ⊕ (𝓤 ∩ x1 ∩ ...) ⊕ (𝓤 ∩ x2 ∩ ...)
    if (lat.isBot(e.cst) && e.terms.forall(t => lat.isTop(t.cst))) {
      return e.copy(cst = lat.Top)
    }

    // ¬a = 1 ⊕ a
    mkXor(alg.one, e)
  }

  /**
    * Returns the xor of the two given Zhegalkin constants `c1` and `c2`.
    * */
  def mkXor[T](c1: T, c2: T)(implicit lat: BoolLattice[T]): T = {
    // Note: use of union, inter, compl ensures a canonical representation.
    // a ⊕ b = (a ∪ b) - (a ∩ b) = (a ∪ b) ∩ ¬(a ∩ b)
    lat.meet(lat.join(c1, c2), lat.comp(lat.meet(c1, c2)))
  }

  /**
    * Returns the xor of the two Zhegalkin expressions.
    *
    * Uses identity laws and caching to speed up the computation.
    */
  def mkXor[T](z1: ZhegalkinExpr[T], z2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    // 0 ⊕ a = a
    if (z1 eq alg.zero) {
      return z2
    }
    // a ⊕ 0 = a
    if (z2 eq alg.zero) {
      return z1
    }

    // Perform a cache lookup or an actual computation.
    alg.Cache.lookupOrComputeXor(z1, z2, computeXor)
  }

  /**
    * Computes and returns the xor of the given two Zhegalkin expressions `e1` and `e2`.
    *
    * Does not use any simplification rules nor any cache.
    */
  private def computeXor[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = (e1, e2) match {
    case (ZhegalkinExpr(c1, ts1), ZhegalkinExpr(c2, ts2)) =>
      val c = mkXor(c1, c2)
      // Eliminate duplicates: a ⊕ a = 0

      // We want to retain all terms that occur an odd number of times.
      val seen = mutable.Set.empty[ZhegalkinTerm[T]]
      for (t <- ts1.iterator ++ ts2.iterator) {
        if (seen.contains(t)) {
          // The term is already there, so we remove it, since we have seen it an even number of times.
          seen.remove(t)
        } else {
          seen.add(t)
        }
      }
      val allTermsNonDup = seen.toList

      // Merge coefficients: (c1 ∩ x1 ∩ x2) ⊕ (c2 ∩ x1 ∩ x2) = (c1 ∩ c2) ∩ x1 ∩ x2
      val termsGroupedByVarSet = allTermsNonDup.groupBy(_.vars).toList
      val mergedTerms = termsGroupedByVarSet.map {
        case (vars, l) =>
          val mergedCst: T = l.foldLeft(lat.Bot) { // Neutral element for Xor.
            case (acc, t) => mkXor(acc, t.cst) // Distributive law: (c1 ∩ A) ⊕ (c2 ∩ A) = (c1 ⊕ c2) ∩ A.
          }
          ZhegalkinTerm(mergedCst, vars)
      }
      mkZhegalkinExpr(c, mergedTerms)
  }

  /**
    * Returns the union of the given two Zhegalkin expressions `e1` and `e2`.
    *
    * Uses identity laws to speed up the computation.
    */
  def mkUnion[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    // Ø ∪ a = a
    if (e1 eq alg.zero) {
      return e2
    }
    // a ∪ Ø = a
    if (e2 eq alg.zero) {
      return e1
    }

    alg.Cache.lookupOrComputeUnion(e1, e2, computeUnion)
  }

  /**
    * Computes the union of the given two Zhegalkin expressions `e1` and `e2`.
    */
  private def computeUnion[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
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
  def mkInter[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    // Ø ∩ a = Ø
    if (e1 eq alg.zero) {
      return alg.zero
    }
    // a ∩ Ø = Ø
    if (e2 eq alg.zero) {
      return alg.zero
    }
    // 𝓤 ∩ a = a
    if (e1 eq alg.one) {
      return e2
    }
    // a ∩ 𝓤 = a
    if (e2 eq alg.one) {
      return e1
    }

    // Perform a cache lookup or an actual computation.
    alg.Cache.lookupOrComputeInter(e1, e2, computeInter)
  }

  /**
    * Computes the intersection of the given Zhegalkin expressions `e1` and `e2`.
    *
    * {{{
    *   (c1 ⊕ t11 ⊕ t12 ⊕ ... ⊕ t1n) ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
    *     =   (c1  ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
    *       ⊕ (t11 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
    *       ⊕ (t12 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
    *       ⊕ ...
    * }}}
    */
  private def computeInter[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = e1 match {
    case ZhegalkinExpr(c1, ts1) =>
      val zero = mkInterConstantExpr(c1, e2)
      ts1.foldLeft(zero) {
        case (acc, z) => mkXor(acc, mkInterTermExpr(z, e2))
      }
  }

  /**
    * Computes the intersection of the given Zhegalkin constant `c` and the given Zhegalkin expression `e`.
    */
  private def mkInterConstantExpr[T](c: T, e: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    // Perform a cache lookup or an actual computation.
    alg.Cache.lookupOrComputeInterCst(c, e, computeInterConstantExpr)
  }

  /**
    * Computes the intersection of the given Zhegalkin constant `c` and the given Zhegalkin expression `e`.
    *
    * {{{
    *   c ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m) = (c ∩ c2) ⊕ (c ∩ t21) ⊕ (c ∩ t22) ⊕ ... ⊕ (c ∩ t2m)
    * }}}
    */
  private def computeInterConstantExpr[T](c: T, e: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = e match {
    case ZhegalkinExpr(c2, terms) =>
      val ts = terms.map(t => mkInterConstantTerm(c, t))
      mkZhegalkinExpr(lat.meet(c, c2), ts)
  }

  /**
    * Computes the intersection of the given Zhegalkin constant `c` and the given Zhegalkin term `t`.
    *
    * {{{
    *   c ∩ (c2 ∩ x1 ∩ x2 ∩ ... ∩ xn) = (c ∩ c2) ∩ x1 ∩ x2 ∩ ... ∩ xn)
    * }}}
    */
  private def mkInterConstantTerm[T](c: T, t: ZhegalkinTerm[T])(implicit lat: BoolLattice[T]): ZhegalkinTerm[T] = t match {
    case ZhegalkinTerm(c2, vars) =>
      if (c == c2) {
        return t
      }

      ZhegalkinTerm(lat.meet(c, c2), vars)
  }

  /**
    * Computes the intersection of the given Zhegalkin term `t` and the given Zhegalkin expression `e`.
    *
    * {{{
    *   t ∩ (c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = (t ∩ c) ⊕ (t ∩ t1) ⊕ (t ∩ t2) ⊕ ... ⊕ (t ∩ tn)
    * }}}
    *
    */
  private def mkInterTermExpr[T](t: ZhegalkinTerm[T], e: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = e match {
    case ZhegalkinExpr(c2, terms) =>
      val zero: ZhegalkinExpr[T] = mkZhegalkinExpr(lat.Bot, List(mkInterConstantTerm(c2, t)))
      terms.foldLeft(zero) {
        case (acc, t2) => mkXor(acc, mkZhegalkinExpr(lat.Bot, List(mkInterTermTerm(t, t2))))
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
  private def mkInterTermTerm[T](t1: ZhegalkinTerm[T], t2: ZhegalkinTerm[T])(implicit lat: BoolLattice[T]): ZhegalkinTerm[T] = {
    // a ∩ a = a
    if (t1 eq t2) {
      return t1
    }

    (t1, t2) match {
      case (ZhegalkinTerm(c1, vars1), ZhegalkinTerm(c2, vars2)) =>
        if (c1 == c2) {
          // Order of cases determined by profiling.
          if (vars1.subsetOf(vars2)) { // We have that t1 is fully contained within t2.
            return t2
          }

          if (vars2.subsetOf(vars1)) { // We have that t2 is fully contained within t1.
            return t1
          }
        }
        // General case:
        ZhegalkinTerm(lat.meet(c1, c2), vars1 ++ vars2)
    }
  }

}

/** Represents a Zhegalkin expr: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn */
case class ZhegalkinExpr[T](cst: T, terms: List[ZhegalkinTerm[T]]) {

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
  def map(f: Int => ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    if (terms == Nil) {
      return this
    }

    terms.foldLeft(ZhegalkinExpr.mkZhegalkinExpr(cst, Nil)) {
      case (acc, term) => ZhegalkinExpr.mkXor(acc, term.map(f))
    }
  }

  /**
    * Returns `true` if `this` is equal to `that`.
    */
  @annotation.nowarn // Disable warning about erasure of T. Safe because all objects have equals.
  override def equals(obj: Any): Boolean = obj match {
    case that: ZhegalkinExpr[T] => (this eq that) || (this.cst == that.cst && this.terms == that.terms)
    case _ => false
  }

  /**
    * Returns the hashCode of `this` Zhegalkin expression.
    *
    * Caching the hashCode leads to speed-ups.
    */
  override val hashCode: Int = Objects.hash(cst, terms)

  /** Returns a human-readable string representation of `this` Zhegalkin expression. Must only be used for debugging. */
  override def toString: String =
    if (terms.isEmpty)
      cst.toString
    else
      s"$cst ⊕ ${terms.map(t => s"($t)").mkString(" ⊕ ")}"

}
