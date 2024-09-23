/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import scala.annotation.nowarn
import scala.collection.immutable.SortedSet

/**
  * A common super-type for set formulas `f`, like `x1 ∩ x2 ∪ (e4 ∪ !c17)`.
  *
  * A set formula can contain elements, constants (fixed unknown sets), variables (unknown sets),
  * complements, unions, and intersections.
  *
  * Variables and constants are collectively referred to as unknowns.
  * A formula without unknowns is called ground.
  *
  * A set without unknowns can be evaluated to get a finite or a co-finite set of elements. The
  * universe of elements is infinite so no finite set is equivalent to universe.
  *
  * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use
  * disjoint integers (see [[unknowns]]).
  */
sealed trait SetFormula {

  import SetFormula.*

  /** The set of [[Var]] in `this`. */
  final def variables: SortedSet[Int] = this match {
    case Univ => SortedSet.empty
    case Empty => SortedSet.empty
    case Cst(_) => SortedSet.empty
    case Var(x) => SortedSet(x)
    case ElemSet(_) => SortedSet.empty
    case Compl(t) => t.variables
    case Inter(_, _, varsPos, _, _, varsNeg, other) =>
      SortedSet.from(varsPos.map(_.x)) ++ varsNeg.map(_.x) ++ other.flatMap(_.variables)
    case Union(_, _, varsPos, _, _, varsNeg, other) =>
      SortedSet.from(varsPos.map(_.x)) ++ varsNeg.map(_.x) ++ other.flatMap(_.variables)
  }

  /** `true` if `this` contains neither [[Var]] nor [[Cst]]. */
  final def isGround: Boolean = this match {
    case Univ => true
    case Empty => true
    case Cst(_) => false
    case Var(_) => false
    case ElemSet(_) => true
    case Compl(t) => t.isGround
    case Inter(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
      cstsPos.isEmpty &&
        varsPos.isEmpty &&
        cstsNeg.isEmpty &&
        varsNeg.isEmpty &&
        other.forall(_.isGround)
    case Union(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
      cstsPos.isEmpty &&
        varsPos.isEmpty &&
        cstsNeg.isEmpty &&
        varsNeg.isEmpty &&
        other.forall(_.isGround)
  }

  /**
    * Returns all [[Cst]] and [[Var]] that occur in `this`.
    *
    * Invariant: [[Cst]], and [[Var]] must use disjoint integers.
    */
  final def unknowns: SortedSet[Int] = this match {
    case Univ => SortedSet.empty
    case Empty => SortedSet.empty
    case Cst(c) => SortedSet(c)
    case ElemSet(_) => SortedSet.empty
    case Var(x) => SortedSet(x)
    case Compl(t) => t.unknowns
    case Inter(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
      SortedSet.from(
        cstsPos.iterator.map(_.c) ++
          varsPos.iterator.map(_.x) ++
          cstsNeg.iterator.map(_.c) ++
          varsNeg.iterator.map(_.x) ++
          other.iterator.flatMap(_.unknowns)
      )
    case Union(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
      SortedSet.from(
        cstsPos.iterator.map(_.c) ++
          varsPos.iterator.map(_.x) ++
          cstsNeg.iterator.map(_.c) ++
          varsNeg.iterator.map(_.x) ++
          other.iterator.flatMap(_.unknowns)
      )
  }

  /**
    * Returns the number of connectives in the unary/binary representation of `this`.
    *
    * [[Compl]], [[Union]], and [[Inter]] are connectives.
    */
  final def size: Int = {
    // This is a worklist algorithm (instead of a recursive algorithm) because:
    // - It doesn't use the stack: error reporting will not be overridden by stack overflow.
    // - The complexity is self-contained so maintenance cost is lower.
    // - The cost of bugs is low since size is not used for correctness.
    var workList = List(this)
    var counter = 0

    /** Updates `counter` and `workList` given intersection or union subformulas. */
    @inline
    def countSetFormulas(
                          elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var],
                          elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var],
                          other: List[SetFormula]
                        ): Unit = {
      val negElemSize = elemNeg.size
      val negCstsSize = cstsNeg.size
      val negVarsSize = varsNeg.size
      val subformulas = elemPos.size + cstsPos.size + varsPos.size +
        negElemSize + negCstsSize + negVarsSize + other.size
      val connectives = negElemSize + negCstsSize + negVarsSize
      counter += (subformulas - 1) + connectives
      workList = other ++ workList
    }

    while (workList.nonEmpty) {
      val t :: next = workList
      workList = next
      t match {
        case Univ => ()
        case Empty => ()
        case Cst(_) => ()
        case Var(_) => ()
        case ElemSet(_) => ()
        case Compl(t) =>
          counter += 1
          workList = t :: workList
        case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
          countSetFormulas(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
        case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
          countSetFormulas(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
      }
    }
    counter
  }

  /** Returns a human-readable string of `this`. */
  override final def toString: String = this match {
    case Univ => "univ"
    case Empty => "empty"
    case Cst(c) => s"c$c"
    case ElemSet(s) if s.sizeIs == 1 => s"e${s.head}"
    case ElemSet(s) => s"e${s.mkString("+")}"
    case Var(x) => s"x$x"
    case Compl(f) => f match {
      case Univ | Empty | Cst(_) | ElemSet(_) | Var(_) | Compl(_) => s"!$f"
      case Inter(_, _, _, _, _, _, _) | Union(_, _, _, _, _, _, _) => s"!($f)"
    }
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val subformulas = subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
      s"(${subformulas.mkString(" ∩ ")})"
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val subformulas = subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
      s"(${subformulas.mkString(" ∪ ")})"
  }

}

object SetFormula {

  /** Skip invariant checks if `false`. */
  private val CHECK_INVARIANTS: Boolean = true

  /** A trait used to refer to [[Univ]] and [[Empty]] collectively. */
  sealed trait UnivOrEmpty extends SetFormula

  /** The full universe set (`univ`). */
  final case object Univ extends SetFormula with UnivOrEmpty

  /** The empty set (`empty`). */
  final case object Empty extends SetFormula with UnivOrEmpty

  /**
    * An uninterpreted constant set (`c42`), i.e. a set that we do not know and cannot make
    * assumptions about.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  final case class Cst(c: Int) extends SetFormula

  /**
    * A set variable (`x42`), i.e. a set that we do not know but should instantiate to its most
    * general form via unification.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  final case class Var(x: Int) extends SetFormula

  /**
    * A concrete, non-empty set/union of elements (`e42` or `e42+43`).
    *
    * Remember that the universe is infinite so an element set is never equivalent to universe.
    *
    * Invariant: `s` is non-empty.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    *
    * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
    * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
    * still public. However, we do not use the `copy` method anywhere for [[ElemSet]], so this is
    * fine.
    */
  @nowarn
  final case class ElemSet private(s: SortedSet[Int]) extends SetFormula {
    if (CHECK_INVARIANTS) assert(s.nonEmpty)
  }

  /**
    * A complement of a formula (`!f`).
    *
    * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
    * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
    * still public. However, we do not use the `copy` method anywhere for [[Compl]], so this is
    * fine.
    */
  @nowarn
  final case class Compl private(f: SetFormula) extends SetFormula

  /**
    * An intersection of formulas (`f1 ∩ f2`).
    *
    * The intersection
    * {{{
    * elemPos = Some({e1, e2}),
    * cstsPos =  Set( c1, c2),
    * varsPos =  Set( x1, x2),
    * elemNeg = Some({e3, e4}),
    * cstsNeg =  Set( c3, c4),
    * varsNeg =  Set( x3, x4),
    * other    = List( e1 ∪ x9)
    * }}}
    * represents the formula
    * {{{ (e1 ∪ e2) ∩ c1 ∩ c2 ∩ x1 ∩ x2 ∩ !(e3 ∪ e4) ∩ !c3 ∩ !c4 ∩ !x3 ∩ !x4 ∩ (e1 ∪ x9) }}}
    *
    * Property: An empty intersection is [[Univ]].
    *
    * Invariant: `elemPos` and `elemNeg` are disjoint.
    *
    * Invariant: There is at least two formulas in the intersection.
    *
    * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
    * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
    * still public. However, we do not use the `copy` method anywhere for [[Inter]], so this is
    * fine.
    */
  @nowarn
  final case class Inter private(
                                  elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var],
                                  elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var],
                                  other: List[SetFormula]
                                ) extends SetFormula {
    if (CHECK_INVARIANTS) {
      // `varsPos` and `varsNeg` are disjoint.
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      // `cstsPos` and `cstsNeg` are disjoint.
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
      // There is always at least two subformulas.
      assert(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList.size == 2, message = this.toString)
    }
  }

  /**
    * A union of formulas (`f1 ∪ f2`).
    *
    * The union
    * {{{
    * elemPos = Some({e1, e2}),
    * cstsPos =  Set( c1, c2),
    * varsPos =  Set( x1, x2),
    * elemNeg = Some({e3, e4}),
    * cstsNeg =  Set( c3, c4),
    * varsNeg =  Set( x3, x4),
    * other    = List( e1 ∩ x9)
    * }}}
    * represents the formula
    * {{{ (e1 ∪ e2) ∪ c1 ∪ c2 ∪ x1 ∪ x2 ∪ !(e3 ∪ e4) ∪ !c3 ∪ !c4 ∪ !x3 ∪ !x4 ∪ (e1 ∩ x9) }}}
    *
    * Property: An empty union is [[Empty]].
    *
    * Invariant: `elemPos` and `elemNeg` are disjoint.
    *
    * Invariant: There is at least two formulas in the union.
    *
    * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
    * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
    * still public. However, we do not use the `copy` method anywhere for [[Union]], so this is
    * fine.
    */
  @nowarn
  final case class Union private(
                                  elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var],
                                  elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var],
                                  other: List[SetFormula]
                                ) extends SetFormula {
    if (CHECK_INVARIANTS) {
      // `varsPos` and `varsNeg` are disjoint.
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      // `cstsPos` and `cstsNeg` are disjoint.
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
      // There is always at least two subformulas.
      assert(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList.sizeIs >= 2, message = this.toString)
    }
  }

  /** Returns an iterator of the subformulas of the union or intersection. */
  private def subformulasOf(
                             elemPos: Iterable[ElemSet], cstsPos: Iterable[Cst], varsPos: Iterable[Var],
                             elemNeg: Iterable[ElemSet], cstsNeg: Iterable[Cst], varsNeg: Iterable[Var],
                             other: Iterable[SetFormula]
                           ): Iterator[SetFormula] = {
    elemPos.iterator ++
      cstsPos.iterator ++
      varsPos.iterator ++
      elemNeg.iterator.map(Compl(_)) ++
      cstsNeg.iterator.map(Compl(_)) ++
      varsNeg.iterator.map(Compl(_)) ++
      other.iterator
  }

}
