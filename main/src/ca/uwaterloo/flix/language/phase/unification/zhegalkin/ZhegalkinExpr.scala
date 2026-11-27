/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.shared.BoolLattice

import java.util.Objects
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/** Companion object for [[ZhegalkinExpr]] */
object ZhegalkinExpr {

  /**
    * A smart constructor for Zhegalkin expressions.
    *
    * A Zhegalkin expression is of the form: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn
    *
    * The terms may come in any order and may repeat variables. They are sorted, the terms with
    * the same variables are combined, and the terms with an empty coefficient are dropped.
    */
  def mkZhegalkinExpr[T](cst: T, terms: List[ZhegalkinTerm[T]])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    if (terms.isEmpty) {
      return mkOrdered(cst, Nil)
    }
    mkXorOfTerms(cst, mutable.ArrayBuffer.from(terms))
  }

  /**
    * Returns the Zhegalkin expression `cst ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn` where `terms` is in the order
    * of [[compareVars]], without repeated variables and without empty coefficients.
    */
  private def mkOrdered[T](cst: T, terms: List[ZhegalkinTerm[T]])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    if (terms.isEmpty) {
      // The zero and the one are unique.
      if (lat.isBot(cst)) {
        return alg.zero
      }
      if (lat.isTop(cst)) {
        return alg.one
      }
    }
    ZhegalkinExpr(cst, terms)
  }

  /**
    * Returns the Zhegalkin expression `cst ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn` for `terms` in any order.
    *
    * This is xor in a single pass over any number of operands: the terms are sorted, which puts
    * the terms with the same variables next to each other, and every such run is combined with
    *
    * {{{
    *   (c1 ∩ A) ⊕ (c2 ∩ A) = (c1 ⊕ c2) ∩ A
    * }}}
    *
    * The buffer `terms` is sorted in place.
    */
  private def mkXorOfTerms[T](cst: T, terms: mutable.ArrayBuffer[ZhegalkinTerm[T]])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    terms.sortInPlaceWith((t1, t2) => compareVars(t1.vars, t2.vars) < 0)
    val result = mutable.ListBuffer.empty[ZhegalkinTerm[T]]
    val n = terms.length
    var i = 0
    while (i < n) {
      val t = terms(i)
      var c = t.cst
      var j = i + 1
      while (j < n && compareVars(terms(j).vars, t.vars) == 0) {
        c = mkXor(c, terms(j).cst)
        j += 1
      }
      if (!lat.isBot(c)) {
        result += (if (j == i + 1) t else ZhegalkinTerm(c, t.vars))
      }
      i = j
    }
    mkOrdered(cst, result.toList)
  }

  /**
    * Compares two sets of variables: the smaller set is less, and sets of the same size are
    * compared lexicographically.
    *
    * The terms of a Zhegalkin expression are kept in this order, with distinct variables. This
    * makes xor a merge (see [[computeXor]]) and the representation of a polynomial unique.
    */
  private def compareVars(vars1: SortedSet[ZhegalkinVar], vars2: SortedSet[ZhegalkinVar]): Int = {
    if (vars1 eq vars2) {
      return 0
    }
    val bySize = Integer.compare(vars1.size, vars2.size)
    if (bySize != 0) {
      return bySize
    }
    val i1 = vars1.iterator
    val i2 = vars2.iterator
    while (i1.hasNext) {
      val cmp = i1.next().compare(i2.next())
      if (cmp != 0) {
        return cmp
      }
    }
    0
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
    * Does not use any cache.
    *
    * The terms of both expressions are in the order of [[compareVars]], so the xor is a merge:
    * a term whose variables occur in one expression is kept, and two terms with the same
    * variables are combined with `(c1 ∩ A) ⊕ (c2 ∩ A) = (c1 ⊕ c2) ∩ A`, and dropped if their
    * coefficients cancel.
    */
  private def computeXor[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    var ts1 = e1.terms
    var ts2 = e2.terms
    val merged = mutable.ListBuffer.empty[ZhegalkinTerm[T]]
    while (ts1.nonEmpty && ts2.nonEmpty) {
      val t1 = ts1.head
      val t2 = ts2.head
      val cmp = compareVars(t1.vars, t2.vars)
      if (cmp < 0) {
        merged += t1
        ts1 = ts1.tail
      } else if (cmp > 0) {
        merged += t2
        ts2 = ts2.tail
      } else {
        val c = mkXor(t1.cst, t2.cst)
        if (!lat.isBot(c)) {
          merged += ZhegalkinTerm(c, t1.vars)
        }
        ts1 = ts1.tail
        ts2 = ts2.tail
      }
    }
    // The rest of the longer list is shared with the operand.
    mkOrdered(mkXor(e1.cst, e2.cst), merged.prependToList(if (ts1.nonEmpty) ts1 else ts2))
  }

  /**
    * Returns the xor of `cst` and the given Zhegalkin expressions `es`.
    *
    * Collects the terms of all operands and combines them in a single pass, where a fold of
    * [[mkXor]] would rebuild the accumulated expression once per operand.
    */
  def mkXorAll[T](cst: T, es: List[ZhegalkinExpr[T]])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    var c = cst
    val terms = mutable.ArrayBuffer.empty[ZhegalkinTerm[T]]
    for (e <- es) {
      c = mkXor(c, e.cst)
      terms ++= e.terms
    }
    mkXorOfTerms(c, terms)
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
    *
    * The products are collected and combined in a single pass (see [[mkXorOfTerms]]).
    */
  private def computeInter[T](e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = (e1, e2) match {
    case (ZhegalkinExpr(c1, ts1), ZhegalkinExpr(c2, ts2)) =>
      // c1 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
      val zero = mkInterConstantExpr(c1, e2)
      val terms = mutable.ArrayBuffer.from(zero.terms)
      for (t1 <- ts1) {
        // t1 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m) = (t1 ∩ c2) ⊕ (t1 ∩ t21) ⊕ (t1 ∩ t22) ⊕ ... ⊕ (t1 ∩ t2m)
        terms += mkInterConstantTerm(c2, t1)
        for (t2 <- ts2) {
          terms += mkInterTermTerm(t1, t2)
        }
      }
      mkXorOfTerms(zero.cst, terms)
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
  //  The terms have non-empty coefficients and distinct variables, and are sorted by their
  //  variables (see ZhegalkinExpr.compareVars).
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

    ZhegalkinExpr.mkXorAll(cst, terms.map(_.map(f)))
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
    * Caching the hashCode leads to speed-ups. It is computed on demand, because most expressions
    * are intermediate results that are never used as the key of a cache.
    */
  override lazy val hashCode: Int = Objects.hash(cst, terms)

  /** Returns a human-readable string representation of `this` Zhegalkin expression. Must only be used for debugging. */
  override def toString: String =
    if (terms.isEmpty)
      cst.toString
    else
      s"$cst ⊕ ${terms.map(t => s"($t)").mkString(" ⊕ ")}"

}
