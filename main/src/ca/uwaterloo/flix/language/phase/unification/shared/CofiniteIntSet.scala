/*
 * Copyright 2024 Jonathan Lindegaard Starup
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.shared

import scala.collection.immutable.SortedSet

/**
  * Represents a finite or co-finite set of integers with an infinite universe.
  *
  * No finite set is ever equivalent to the universe.
  */
sealed trait CofiniteIntSet

object CofiniteIntSet {

  /** Represents a finite set of integers. */
  case class Set(s: SortedSet[Int]) extends CofiniteIntSet {
    override def toString: String = if (s.isEmpty) "Ø" else s"{${s.mkString(", ")}}"
  }

  /** Represents a co-finite set of integers. */
  case class Compl(s: SortedSet[Int]) extends CofiniteIntSet {
    override def toString: String = if (s.isEmpty) "𝓤" else s"¬{${s.mkString(", ")}}"
  }

  /** THE empty set. */
  private val Empty: CofiniteIntSet = Set(SortedSet.empty)

  /** THE universe set. */
  private val Universe: CofiniteIntSet = Compl(SortedSet.empty)

  /** Returns the set of `s`. */
  def mkSet(s: SortedSet[Int]): CofiniteIntSet = {
    // Note: We must check whether `s` is empty to ensure correctness.
    if (s.isEmpty) Empty else Set(s)
  }

  object LatticeOps extends BoolLattice[CofiniteIntSet] {
    override def Bot: CofiniteIntSet = CofiniteIntSet.Empty

    override def Top: CofiniteIntSet = CofiniteIntSet.Universe

    override def isBot(t: CofiniteIntSet): Boolean = t match {
      case CofiniteIntSet.Set(s) => s.isEmpty
      case CofiniteIntSet.Compl(_) => false
    }

    override def isTop(t: CofiniteIntSet): Boolean = t match {
      case CofiniteIntSet.Set(_) => false
      case CofiniteIntSet.Compl(s) => s.isEmpty
    }

    override def comp(t: CofiniteIntSet): CofiniteIntSet = {
      t match {
        case Set(s) =>
          // !s
          Compl(s)
        case Compl(s) =>
          // !!s
          // = s             (double negation)
          Set(s)
      }
    }

    override def join(t1: CofiniteIntSet, t2: CofiniteIntSet): CofiniteIntSet = {
      (t1, t2) match {
        case (Set(x), Set(y)) =>
          // x ∪ y
          Set(x.union(y))
        case (Set(x), Compl(y)) =>
          // x ∪ !y
          // = !!(x ∪ !y)    (double complement)
          // = !(!x ∩ y)     (complement distribution)
          // = !(y ∩ !x)     (intersection symmetry)
          // = !(y - x)      (difference definition)
          Compl(y.diff(x))
        case (Compl(x), Set(y)) =>
          // !x ∪ y
          // = !!(!x ∪ y)    (double complement)
          // = !(x ∩ !y)     (complement distribution)
          // = !(x - y)      (difference definition)
          Compl(x.diff(y))
        case (Compl(x), Compl(y)) =>
          // !x ∪ !y
          // = !!(!x ∪ !y)   (double complement)
          // = !(!!x ∩ !!y)  (complement distribution)
          // = !(x ∩ y)      (double complement)
          Compl(x.intersect(y))
      }
    }

    override def meet(t1: CofiniteIntSet, t2: CofiniteIntSet): CofiniteIntSet = {
      (t1, t2) match {
        case (Set(x), Set(y)) =>
          // x ∩ y
          Set(x.intersect(y))
        case (Set(x), Compl(y)) =>
          // x ∩ !y
          // = x - y         (difference definition)
          Set(x.diff(y))
        case (Compl(x), Set(y)) =>
          // !x ∩ y
          // = y ∩ !x        (intersection symmetry)
          // = y - x         (difference definition)
          Set(y.diff(x))
        case (Compl(x), Compl(y)) =>
          // !x ∩ !y
          // = !!(!x ∩ !y)   (double complement)
          // = !(x ∪ y)      (complement distribution)
          Compl(x.union(y))
      }
    }
  }

}
