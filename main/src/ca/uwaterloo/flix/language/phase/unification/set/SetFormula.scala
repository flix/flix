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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{CofiniteIntSet, InternalCompilerException}

import scala.annotation.nowarn
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

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
  * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use disjoint
  * integers (see [[SetFormula.unknowns]]).
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
    case Compl(f) => f.variables
    case Inter(_, _, varsPos, _, _, varsNeg, other) =>
      SortedSet.from(varsPos.map(_.x)) ++ varsNeg.map(_.x) ++ other.flatMap(_.variables)
    case Union(_, _, varsPos, _, _, varsNeg, other) =>
      SortedSet.from(varsPos.map(_.x)) ++ varsNeg.map(_.x) ++ other.flatMap(_.variables)
  }

  /** Faster alternative to `this.variables.contains(v)`. */
  final def contains(v: Var): Boolean = this match {
    case Univ => false
    case Empty => false
    case Cst(_) => false
    case Var(x) => x == v.x
    case ElemSet(_) => false
    case Compl(f) => f.contains(v)
    case Inter(_, _, varsPos, _, _, varsNeg, other) =>
      varsPos.contains(v) || varsNeg.contains(v) || other.exists(_.contains(v))
    case Union(_, _, varsPos, _, _, varsNeg, other) =>
      varsPos.contains(v) || varsNeg.contains(v) || other.exists(_.contains(v))
  }

  /** `true` if `this` contains neither [[Var]] nor [[Cst]]. */
  final def isGround: Boolean = this match {
    case Univ => true
    case Empty => true
    case Cst(_) => false
    case Var(_) => false
    case ElemSet(_) => true
    case Compl(f) => f.isGround
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
    case Compl(f) => f.unknowns
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
      val f0 :: next = workList
      workList = next
      f0 match {
        case Univ => ()
        case Empty => ()
        case Cst(_) => ()
        case Var(_) => ()
        case ElemSet(_) => ()
        case Compl(f) =>
          counter += 1
          workList = f :: workList
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

    /** Applies `f` to the subformulas of `this`. */
    def mapSubformulas[T](f: SetFormula => T): List[T] = {
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).map(f).toList
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

    /** Applies `f` to the subformulas of `this`. */
    def mapSubformulas[T](f: SetFormula => T): List[T] = {
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).map(f).toList
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

  //
  // Smart Constructors
  //

  /** Returns a singleton [[ElemSet]]. */
  def mkElemSet(e: Int): ElemSet = {
    // Maintains that ElemSet is non-empty.
    ElemSet(SortedSet(e))
  }

  /**
    * Returns [[ElemSet]] if `s` is non-empty.
    *
    * Returns [[Empty]] if `s` is empty.
    */
  def mkElemSet(s: SortedSet[Int]): SetFormula = {
    // Maintains that ElemSet is non-empty.
    if (s.isEmpty) Empty else ElemSet(s)
  }

  /**
    * Returns the complement of `f` (`!f`).
    *
    * Complements are pushed to the bottom of the formula.
    *
    * Example
    *   - `mkCompl(x1 ∩ (!x2 ∪ x3)) = !x1 ∪ (x2 ∩ !x3)`
    */
  def mkCompl(f: SetFormula): SetFormula = f match {
    case Univ => Empty
    case Empty => Univ
    case cst@Cst(_) => Compl(cst)
    case v@Var(_) => Compl(v)
    case e@ElemSet(_) => Compl(e)
    case Compl(f1) => f1
    case inter@Inter(_, _, _, _, _, _, _) =>
      mkUnionAll(inter.mapSubformulas(mkCompl))
    case union@Union(_, _, _, _, _, _, _) =>
      mkInterAll(union.mapSubformulas(mkCompl))
  }

  /**
    * Returns the intersection of `f1` and `f2` (`f1 ∩ f2`).
    *
    * Nested intersections are put into a single intersection.
    *
    * Example
    *   - `mkInter((x ∩ y), (z ∩ q)) = x ∩ y ∩ z ∩ q`
    */
  def mkInter(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Univ, _) => f2
    case (_, Univ) => f1
    case _ => mkInterAll(List(f1, f2))
  }

  /**
    * Returns the intersection of `fs` (`fs1 ∩ fs2 ∩ ..`).
    *
    * Nested intersections are put into a single intersection.
    */
  def mkInterAll(fs: List[SetFormula]): SetFormula = {
    // We need to do two things:
    // - Separate subformulas into specific buckets of pos/neg elements/variables/constants and other.
    // - Flatten nested intersections.

    // es1 ∩ !es2 ∩ es3 ∩ !es4 ∩ ..
    // univ ∩ es1 ∩ !es2 ∩ es3 ∩ !es4 ∩ ..       (neutral intersection formula)
    // = (univ ∩ es1 ∩ es3 ∩ !es2 ∩ !es4) ∩ ..   (intersection associativity)
    // = (univ ∩ es1 ∩ es3 - es2 - es4) ∩ ..
    // So we keep one CofiniteIntSet that represents both element sets -
    // intersecting positive elems and differencing negative elements.

    var elemPos0 = CofiniteIntSet.universe
    val cstsPos = mutable.Set.empty[Cst]
    val varsPos = mutable.Set.empty[Var]
    val cstsNeg = mutable.Set.empty[Cst]
    val varsNeg = mutable.Set.empty[Var]
    val other = mutable.ListBuffer.empty[SetFormula]

    var workList = fs
    while (workList.nonEmpty) {
      val f0 :: next = workList
      workList = next
      f0 match {
        case Univ =>
          ()
        case Empty =>
          return Empty
        case c@Cst(_) =>
          if (cstsNeg.contains(c)) return Empty
          cstsPos += c
        case Compl(c@Cst(_)) =>
          if (cstsPos.contains(c)) return Empty
          cstsNeg += c
        case ElemSet(s) =>
          elemPos0 = CofiniteIntSet.intersection(elemPos0, s)
          if (elemPos0.isEmpty) return Empty
        case Compl(ElemSet(s)) =>
          elemPos0 = CofiniteIntSet.difference(elemPos0, s)
          if (elemPos0.isEmpty) return Empty
        case x@Var(_) =>
          if (varsNeg.contains(x)) return Empty
          varsPos += x
        case Compl(x@Var(_)) =>
          if (varsPos.contains(x)) return Empty
          varsNeg += x
        case Inter(elemPos1, cstsPos1, varsPos1, elemNeg1, cstsNeg1, varsNeg1, other1) =>
          // To avoid wrapping negated subformulas, we process them inline
          for (e <- elemNeg1) {
            elemPos0 = CofiniteIntSet.difference(elemPos0, e.s)
            if (elemPos0.isEmpty) return Empty
          }
          for (x <- cstsNeg1) {
            if (cstsPos.contains(x)) return Empty
            cstsNeg += x
          }
          for (x <- varsNeg1) {
            if (varsPos.contains(x)) return Empty
            varsNeg += x
          }
          // Add the positive subformulas to the work list
          workList = elemPos1.toList ++ cstsPos1 ++ varsPos1 ++ other1 ++ workList
        case union@Union(_, _, _, _, _, _, _) =>
          other += union
        case compl@Compl(_) =>
          other += compl
      }
    }
    // Split into pos/neg elements.
    val (elemPos, elemNeg) = elemPos0 match {
      case CofiniteIntSet.Set(s) =>
        if (s.isEmpty) {
          // empty ∩ ..
          // = empty       (idempotent intersection formula)
          // Could return Empty, but unreachable since we check for empty at each operation.
          throw InternalCompilerException(s"Impossible empty ElemSet $s", SourceLocation.Unknown)
        } else {
          // s.nonEmpty
          // s ∩ ..
          (Some(ElemSet(s)), None)
        }
      case CofiniteIntSet.Compl(s) =>
        if (s.isEmpty) {
          // !empty ∩ ..
          // = univ ∩ ..   (complement distribution)
          // = ..          (neutral intersection formula)
          (None, None)
        } else {
          // s.nonEmpty
          // !s ∩ ..
          (None, Some(ElemSet(s)))
        }
    }

    // Avoid intersections with zero or one subformula.
    subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
      case Nil => return Univ
      case one :: Nil => return one
      case _ => ()
    }
    Inter(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
  }

  /**
    * Returns the union of `f1` and `f2` (`f1 ∪ f2`).
    *
    * Nested unions are put into a single union.
    *
    * Example
    *   - `mkUnion((x ∪ y), (z ∪ q)) = x ∪ y ∪ z ∪ q`
    */
  def mkUnion(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Univ, _) => Univ
    case (_, Univ) => Univ
    case (Empty, _) => f2
    case (_, Empty) => f1
    case _ => mkUnionAll(List(f1, f2))
  }

  /**
    * Returns the union of `fs` (`fs1 ∪ fs2 ∪ ..`).
    *
    * Nested unions are put into a single union.
    */
  def mkUnionAll(fs: List[SetFormula]): SetFormula = {
    // We need to do two things:
    // - Separate subformulas into specific buckets of pos/neg elements/variables/constants and other.
    // - Flatten nested unions.

    // es1 ∪ !es2 ∪ es3 ∪ !es4 ∪ ..
    // empty ∪ es1 ∪ !es2 ∪ es3 ∪ !es4 ∪ ..       (neutral union formula)
    // = (empty ∪ es1 ∪ es3 ∪ !es2 ∪ !es4) ∪ ..   (intersection associativity)
    // = !!(empty ∪ es1 ∪ es3 ∪ !es2 ∪ !es4) ∪ .. (double complement)
    // = !(univ ∩ !es1 ∩ !es3 ∩ es2 ∩ es4) ∪ .. (double complement)
    // So we keep one CofiniteIntSet that represents both element sets -
    // differencing positive elems and intersecting negative elements.

    val cstsPos = mutable.Set.empty[Cst]
    val varsPos = mutable.Set.empty[Var]
    var elemNeg0 = CofiniteIntSet.universe
    val cstsNeg = mutable.Set.empty[Cst]
    val varsNeg = mutable.Set.empty[Var]
    val other = mutable.ListBuffer.empty[SetFormula]

    // Variables and constants are checked for overlap immediately.
    // Elements are checked at the end.

    var workList = fs
    while (workList.nonEmpty) {
      val f0 :: next = workList
      workList = next
      f0 match {
        case Empty =>
          ()
        case Univ =>
          return Univ
        case x@Cst(_) =>
          if (cstsNeg.contains(x)) return Univ
          cstsPos += x
        case Compl(x@Cst(_)) =>
          if (cstsPos.contains(x)) return Univ
          cstsNeg += x
        case ElemSet(s) =>
          elemNeg0 = CofiniteIntSet.difference(elemNeg0, s)
          if (elemNeg0.isEmpty) return Univ
        case Compl(ElemSet(s)) =>
          elemNeg0 = CofiniteIntSet.intersection(elemNeg0, s)
          if (elemNeg0.isEmpty) return Univ
        case x@Var(_) =>
          if (varsNeg.contains(x)) return Univ
          varsPos += x
        case Compl(x@Var(_)) =>
          if (varsPos.contains(x)) return Univ
          varsNeg += x
        case Union(elemPos1, cstsPos1, varsPos1, elemNeg1, cstsNeg1, varsNeg1, other1) =>
          // To avoid wrapping negated subformulas, we process them inline
          for (e <- elemNeg1) {
            elemNeg0 = CofiniteIntSet.intersection(elemNeg0, e.s)
            if (elemNeg0.isEmpty) return Univ
          }
          for (x <- cstsNeg1) {
            if (cstsPos.contains(x)) return Univ
            cstsNeg += x
          }
          for (x <- varsNeg1) {
            if (varsPos.contains(x)) return Univ
            varsNeg += x
          }
          // Add the positive subformulas to the work list
          workList = elemPos1.toList ++ cstsPos1 ++ varsPos1 ++ other1 ++ workList
        case inter@Inter(_, _, _, _, _, _, _) =>
          other += inter
        case compl@Compl(_) =>
          other += compl
      }
    }
    // Split into pos/neg elements.
    val (elemPos, elemNeg) = elemNeg0 match {
      case CofiniteIntSet.Set(s) =>
        if (s.isEmpty) {
          // !empty ∪ ..
          // = univ ∪ ..   (complement distribution)
          // = univ        (idempotent union formula)
          // Could return Univ, but unreachable since we check for empty at each operation.
          throw InternalCompilerException(s"Impossible empty ElemSet $s", SourceLocation.Unknown)
        } else {
          // s.nonEmpty
          // !s ∪ ..
          (None, Some(ElemSet(s)))
        }
      case CofiniteIntSet.Compl(s) =>
        if (s.isEmpty) {
          // !!empty ∪ ..
          // = empty ∪ ..  (double complement)
          // = ..          (neutral union formula)
          (None, None)
        } else {
          // s.nonEmpty
          // !!s ∪ ..
          // = s ∪ ..      (double complement)
          (Some(ElemSet(s)), None)
        }
    }

    // Avoid unions with zero or one subformula.
    subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
      case Nil => return Empty
      case one :: Nil => return one
      case _ => ()
    }
    Union(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
  }

  /** Returns the Xor of `f1` and `f2` with the formula `(f1 - f2) ∪ (f2 - f1)`. */
  def mkXor(f1: SetFormula, f2: SetFormula): SetFormula =
    mkUnion(mkDifference(f1, f2), mkDifference(f2, f1))

  /** Returns the difference of `f1` and `f2` (`-`) with the formula `f1 ∩ !f2`. */
  def mkDifference(f1: SetFormula, f2: SetFormula): SetFormula =
    mkInter(f1, mkCompl(f2))

  /**
    * Returns a formula that is equivalent to [[Empty]] if `f1` is equivalent to `f2`.
    */
  def mkEmptyQuery(f1: SetFormula, f2: SetFormula): SetFormula = {
    if (f1 == Empty) f2
    else if (f2 == Empty) f1
    else mkXor(f1, f2)
  }

  //
  // Other Functions
  //

  /**
    * Propagation uses the following rewrites:
    *   - `x ∩ f == x ∩ f[x -> Univ]`
    *   - `!x ∩ f == !x ∩ f[x -> Empty]`
    *   - `x ∪ f == x ∪ f[x -> Empty]`
    *   - `!x ∪ f == !x ∪ f[x -> Univ]`
    *
    * where `x` is [[Cst]], [[Var]], or [[ElemSet]].
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  def propagation(f: SetFormula): SetFormula = debug(f, SortedMap.empty)

  /**
    * Applies set propagation to `f` where `insts` keep track of running instantiations for
    * [[ElemSet]], [[Cst]], and [[Var]] (using the identity mapping if not present).
    */
  private def propagationWithInsts(f: SetFormula, insts: SortedMap[Int, UnivOrEmpty]): SetFormula = f match {
    case Univ => f
    case Empty => f
    case Cst(c) => insts.getOrElse(c, f)
    case Var(x) => insts.getOrElse(x, f)
    case e@ElemSet(_) => instElemSet(e, insts)
    case compl@Compl(f1) =>
      val f1Prop = debug(f1, insts)
      // Maintain and exploit reference equality for performance.
      if (f1Prop eq f1) compl else mkCompl(f1Prop)

    case Inter(elemPos0, cstsPos0, varsPos0, elemNeg0, cstsNeg0, varsNeg0, other0) =>
      // Compute element sets and check for early exits.
      val elemPos = elemPos0.map(instElemSet(_, insts)) match {
        case Some(e) => e match {
          case Univ => None
          case Empty => return Empty
          case e@ElemSet(_) => Some(e)
          case other =>
            // Unreachable since `instElemSet` only returns univ, empty, or elem set.
            throw InternalCompilerException(s"Unexpected formula: '$other'", SourceLocation.Unknown)
        }
        case None => None
      }
      if (cstsPos0.exists(c => insts.get(c.c).contains(Empty))) return Empty
      if (varsPos0.exists(x => insts.get(x.x).contains(Empty))) return Empty
      val elemNeg = elemNeg0.map(instElemSet(_, insts)) match {
        case Some(e) => e match {
          case Univ => return Empty
          case Empty => None
          case e@ElemSet(_) => Some(e)
          case other =>
            // Unreachable since `instElemSet` only returns univ, empty, or elem set.
            throw InternalCompilerException(s"Unexpected formula: '$other'", SourceLocation.Unknown)
        }
        case None => None
      }
      if (cstsNeg0.exists(c => insts.get(c.c).contains(Univ))) return Empty
      if (varsNeg0.exists(x => insts.get(x.x).contains(Univ))) return Empty

      // Compute constants and variables by removing constants and variables that are redundant.
      // We know from previous checks that none are mapped to the respective short-circuit element.
      val cstsPos = cstsPos0.filterNot(c => insts.get(c.c).contains(Univ))
      val varsPos = varsPos0.filterNot(x => insts.get(x.x).contains(Univ))
      val cstsNeg = cstsNeg0.filterNot(c => insts.get(c.c).contains(Empty))
      val varsNeg = varsNeg0.filterNot(x => insts.get(x.x).contains(Empty))

      // Add new instantiations.
      var currentInsts = insts ++
        setElemOneOpt(elemPos, Univ) ++
        setElemOneOpt(elemNeg, Empty) ++
        cstsPos.map(_.c -> Univ) ++
        cstsNeg.map(_.c -> Empty) ++
        varsPos.map(_.x -> Univ) ++
        varsNeg.map(_.x -> Empty)

      // Recursively instantiate `other` while collecting further instantiations as we go.
      val other = mutable.ListBuffer.empty[SetFormula]
      for (f <- other0) {
        val fProp = debug(f, currentInsts)
        // Add elements, constants, and variables to the instantiations.
        fProp match {
          case Cst(c) => currentInsts += (c -> Univ)
          case Compl(Cst(c)) => currentInsts += (c -> Empty)
          case Var(x) => currentInsts += (x -> Univ)
          case Compl(Var(x)) => currentInsts += (x -> Empty)
          case e@ElemSet(_) => currentInsts ++= setElemOne(e, Univ)
          case Compl(e@ElemSet(_)) => currentInsts ++= setElemOne(e, Empty)
          case Univ => ()
          case Empty => ()
          case Compl(_) => ()
          case Inter(_, _, _, _, _, _, _) => ()
          case Union(_, _, _, _, _, _, _) => ()
        }
        other.append(fProp)
      }
      mkInterAll(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList)

    case Union(elemPos0, cstsPos0, varsPos0, elemNeg0, cstsNeg0, varsNeg0, other0) =>
      // Compute element sets and check for early exits.
      val elemPos = elemPos0.map(instElemSet(_, insts)) match {
        case Some(e) => e match {
          case Univ => return Univ
          case Empty => None
          case e@ElemSet(_) => Some(e)
          case other =>
            // Unreachable since `instElemSet` only returns univ, empty, or elem set.
            throw InternalCompilerException(s"Unexpected formula: '$other'", SourceLocation.Unknown)
        }
        case None => None
      }
      if (cstsPos0.exists(c => insts.get(c.c).contains(Univ))) return Univ
      if (varsPos0.exists(x => insts.get(x.x).contains(Univ))) return Univ
      val elemNeg = elemNeg0.map(instElemSet(_, insts)) match {
        case Some(e) => e match {
          case Univ => None
          case Empty => return Univ
          case e@ElemSet(_) => Some(e)
          case other =>
            // Unreachable since `instElemSet` only returns univ, empty, or elem set.
            throw InternalCompilerException(s"Unexpected formula: '$other'", SourceLocation.Unknown)
        }
        case None => None
      }
      if (cstsNeg0.exists(c => insts.get(c.c).contains(Empty))) return Univ
      if (varsNeg0.exists(x => insts.get(x.x).contains(Empty))) return Univ

      // Compute constants and variables by removing constants and variables that are redundant.
      // We know from previous checks that none are mapped to the respective short-circuit element.
      val cstsPos = cstsPos0.filterNot(c => insts.get(c.c).contains(Empty))
      val varsPos = varsPos0.filterNot(x => insts.get(x.x).contains(Empty))
      val cstsNeg = cstsNeg0.filterNot(c => insts.get(c.c).contains(Univ))
      val varsNeg = varsNeg0.filterNot(x => insts.get(x.x).contains(Univ))

      // Add new instantiations.
      var currentInsts = insts ++
        setElemOneOpt(elemPos, Empty) ++
        setElemOneOpt(elemNeg, Univ) ++
        cstsPos.map(_.c -> Empty) ++
        cstsNeg.map(_.c -> Univ) ++
        varsPos.map(_.x -> Empty) ++
        varsNeg.map(_.x -> Univ)

      // Recursively instantiate `other` while collecting further instantiations as we go.
      val other = mutable.ListBuffer.empty[SetFormula]
      for (f <- other0) {
        val fProp = debug(f, currentInsts)
        // Add elements, constants, and variables to the instantiations.
        fProp match {
          case Cst(c) => currentInsts += (c -> Univ)
          case Compl(Cst(c)) => currentInsts += (c -> Empty)
          case Var(x) => currentInsts += (x -> Univ)
          case Compl(Var(x)) => currentInsts += (x -> Empty)
          case e@ElemSet(_) => currentInsts ++= setElemOne(e, Univ)
          case Compl(e@ElemSet(_)) => currentInsts ++= setElemOne(e, Empty)
          case Univ => ()
          case Empty => ()
          case Compl(_) => ()
          case Inter(_, _, _, _, _, _, _) => ()
          case Union(_, _, _, _, _, _, _) => ()
        }
        other.append(fProp)
      }
      mkUnionAll(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList)
  }

  private def debug(f: SetFormula, insts: SortedMap[Int, UnivOrEmpty]): SetFormula = {
    val res = propagationWithInsts(f, insts)
    println()
    println("  f: " + f.toString)
    println(s"  insts: ${insts.mkString(", ")}" )
    println("  res: " + res)
    res
  }

  /**
    * Instantiates [[ElemSet]], returning either [[Univ]], [[Empty]], or [[ElemSet]].
    *
    * If something is not present in `insts`, then it is left as it is.
    */
  private def instElemSet(e: ElemSet, insts: SortedMap[Int, UnivOrEmpty]): SetFormula = {
    if (e.s.exists(i => insts.get(i).contains(Univ))) {
      // (e1 ∪ e2 ∪ ..)[e2 -> univ, ..]
      // = (e1 ∪ univ ∪ ..)[..]           (apply map partially)
      // = (univ ∪ (e1 ∪ ..))[..]         (intersection associativity)
      // = univ[..]                       (idempotent intersection formula)
      // = univ                           (mapping on univ)
      Univ
    } else {
      // We know that all mapping are `ei -> empty`.
      // (e1 ∪ e2 ∪ ..)[e2 -> empty, ..]
      // = (e1 ∪ empty ∪ ..)[..]          (apply map partially)
      // = (e1 ∪ ..)[..]                  (union neutral formula)
      // (repeat until the empty mapping)
      val s1 = e.s.filterNot(i => insts.get(i).contains(Empty))
      // Maintain and exploit reference equality for performance.
      if (s1 eq e.s) e else mkElemSet(s1)
    }
  }

  /**
    * Returns a point-wise map for `[e1 ∪ e2 ∪ .. -> f]` if possible, otherwise an empty map.
    *   - `setElemOne(e1, f) = Map(e1 -> f)`
    *   - `setElemOne(e1 ∪ e2 ∪ .., empty) = Map(e1 -> empty, e2 -> empty, ..)`
    *   - otherwise `Map.empty`
    */
  private def setElemOne[T <: SetFormula](e: ElemSet, f: T): SortedMap[Int, T] = {
    if (e.s.sizeIs == 1) {
      SortedMap(e.s.head -> f)
    } else if (f == Empty) {
      e.s.foldLeft(SortedMap.empty[Int, T]) { case (acc, i) => acc + (i -> f) }
    } else SortedMap.empty[Int, T]
  }

  /** Calls [[setElemOne]] if `e != None` */
  private def setElemOneOpt[T <: SetFormula](e: Option[ElemSet], f: T): SortedMap[Int, T] = {
    e match {
      case Some(e) => setElemOne(e, f)
      case _ => SortedMap.empty[Int, T]
    }
  }

  /**
    * Returns `true` if `f` is equivalent to [[Empty]].
    * Exponential time in the number of unknowns.
    */
  def isEmptyEquivalent(f: SetFormula): Boolean = f match {
    case Univ => false
    case Cst(_) => false
    case Var(_) => false
    case ElemSet(_) => false
    case Empty => true
    // If there are unknowns in a union, then it will clearly be non-empty for some instantiation.
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, _) if elemPos.nonEmpty ||
      cstsPos.nonEmpty || varsPos.nonEmpty || elemNeg.nonEmpty || cstsNeg.nonEmpty ||
      varsNeg.nonEmpty => false
    case _ =>
      isEmptyEquivalentExhaustive(f)
  }

  /**
    * Returns `true` if `f1` and `f2` are equivalent.
    * Exponential time in the number of unknowns.
    */
  def isEquivalent(f1: SetFormula, f2: SetFormula): Boolean =
    isEmptyEquivalent(mkEmptyQuery(f1, f2))

  /**
    * Helper function of [[isEmptyEquivalent]], should not be called directly since
    * [[isEmptyEquivalent]] can be faster.
    *
    * Returns `true` if `f` is equivalent to [[Empty]].
    * Exponential time in the number of unknowns.
    */
  private def isEmptyEquivalentExhaustive(f: SetFormula): Boolean = {
    /**
      * Checks that all possible instantiations of unknowns result in [[Empty]].
      * The unknowns not present in either set is implicitly instantiated to [[Empty]].
      *
      * @param unknowns     the unknowns in `f` that has not yet been instantiated
      * @param univUnknowns the unknowns that are instantiated to [[Univ]]
      */
    def loop(unknowns: List[Int], univUnknowns: SortedSet[Int]): Boolean = unknowns match {
      case Nil =>
        // All unknowns are bound. Evaluate the set and check.
        evaluate(f, univUnknowns).isEmpty
      case x :: xs =>
        // `f` is equivalent to `empty` if and only if both `f[x -> univ]` and `f[x -> empty]` are
        // equivalent to `empty`.
        loop(xs, univUnknowns + x) && loop(xs, univUnknowns)
    }

    loop(f.unknowns.toList, SortedSet.empty)
  }

  /**
    * Returns the [[CofiniteIntSet]] evaluation of `f`, interpreting unknowns in `univUnknowns` as
    * [[Univ]] and the rest as [[Empty]].
    */
  private def evaluate(t: SetFormula, univUnknowns: SortedSet[Int]): CofiniteIntSet = {
    import ca.uwaterloo.flix.util.CofiniteIntSet as CISet
    t match {
      case Univ => CISet.universe
      case Empty => CISet.empty
      case Cst(c) => if (univUnknowns.contains(c)) CISet.universe else CISet.empty
      case ElemSet(s) => CISet.mkSet(s)
      case Var(x) => if (univUnknowns.contains(x)) CISet.universe else CISet.empty
      case Compl(t) => CISet.complement(evaluate(t, univUnknowns))

      case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        // Evaluate the subformulas, exiting early in case the running set is `empty`.
        var running = CISet.universe
        for (t <- elemPos.iterator ++ cstsPos.iterator ++ varsPos.iterator) {
          running = CISet.intersection(running, evaluate(t, univUnknowns))
          if (running.isEmpty) return CISet.empty
        }
        for (t <- elemNeg.iterator ++ cstsNeg.iterator ++ varsNeg.iterator) {
          running = CISet.intersection(running, CISet.complement(evaluate(t, univUnknowns)))
          if (running.isEmpty) return CISet.empty
        }
        for (t <- other) {
          running = CISet.intersection(running, evaluate(t, univUnknowns))
          if (running.isEmpty) return CISet.empty
        }
        running

      case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        // Evaluate the subformulas, exiting early in case the running set is `univ`.
        var running = CISet.empty
        for (t <- elemPos.iterator ++ cstsPos.iterator ++ varsPos.iterator) {
          running = CISet.union(running, evaluate(t, univUnknowns))
          if (running.isUniverse) return CISet.universe
        }
        for (t <- elemNeg.iterator ++ cstsNeg.iterator ++ varsNeg.iterator) {
          running = CISet.union(running, CISet.complement(evaluate(t, univUnknowns)))
          if (running.isUniverse) return CISet.universe
        }
        for (t <- other) {
          running = CISet.union(running, evaluate(t, univUnknowns))
          if (running.isUniverse) return CISet.universe
        }
        running
    }
  }

}
