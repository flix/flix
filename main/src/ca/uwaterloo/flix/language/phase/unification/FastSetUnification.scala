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

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.nowarn
import scala.collection.immutable.SortedSet
import scala.collection.mutable.{ListBuffer, Set as MutSet}

object FastSetUnification {

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

    /**
      * The set of [[Var]] in `this`.
      *
      * This is computed on construction.
      */
    final val variables: SortedSet[Int] = this match {
      case Univ => SortedSet.empty
      case Empty => SortedSet.empty
      case Cst(_) => SortedSet.empty
      case Var(x) => SortedSet(x)
      case ElemSet(_) => SortedSet.empty
      case Compl(t) => t.variables
      case Inter(_, _, varsPos, _, _, varsNeg, other) =>
        SortedSet.from(
          varsPos.iterator.map(_.x) ++
            varsNeg.iterator.map(_.x) ++
            other.iterator.flatMap(_.variables)
        )
      case Union(_, _, varsPos, _, _, varsNeg, other) =>
        SortedSet.from(
          varsPos.iterator.map(_.x) ++
            varsNeg.iterator.map(_.x) ++
            other.iterator.flatMap(_.variables)
        )
    }

    /**
      * `true` if `this` contains neither [[Var]] nor [[Cst]].
      *
      * This is computed on construction.
      */
    final val ground: Boolean = this match {
      case Univ => true
      case Empty => true
      case Cst(_) => false
      case Var(_) => false
      case ElemSet(_) => true
      case Compl(t) => t.ground
      case Inter(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
        cstsPos.isEmpty &&
          varsPos.isEmpty &&
          cstsNeg.isEmpty &&
          varsNeg.isEmpty &&
          other.forall(_.ground)
      case Union(_, cstsPos, varsPos, _, cstsNeg, varsNeg, other) =>
        cstsPos.isEmpty &&
          varsPos.isEmpty &&
          cstsNeg.isEmpty &&
          varsNeg.isEmpty &&
          other.forall(_.ground)
    }

    /**
      * Returns all [[Var]] and [[Cst]] that occur in `this`.
      *
      * This is not computed on construction so it traverses `this`.
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
      var workList = List(this)
      var counter = 0

      /** Updates `counter` and `workList` given intersection or union subformulas. */
      @inline
      def countSetFormulas(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var], other: List[SetFormula]): Unit = {
        val negElemSize = elemNeg.size
        val negCstsSize = cstsNeg.size
        val negVarsSize = varsNeg.size
        val subformulas = elemPos.size + cstsPos.size + varsPos.size + negElemSize + negCstsSize + negVarsSize + other.size
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
    override final def toString: String = {
      /** Returns the string of subformulas joined by `sepString`. */
      @inline
      def stringOfSubformulas(sepString: String,
                              elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var],
                              elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var],
                              other: List[SetFormula]): String = {
        val subformulas = elemPos.iterator ++
          elemNeg.iterator.map(mkCompl(_)) ++
          cstsPos.iterator ++
          cstsNeg.iterator.map(mkCompl(_)) ++
          varsPos.iterator ++
          varsNeg.iterator.map(mkCompl(_)) ++
          other.iterator
        s"(${subformulas.mkString(sepString)})"
      }

      this match {
        case Univ => "univ"
        case Empty => "empty"
        case Cst(c) => s"c$c"
        case ElemSet(s) if s.sizeIs == 1 => s"e${s.head}"
        case ElemSet(s) => s"e${s.mkString("+")}"
        case Var(x) => s"x$x"
        case Compl(f) => f match {
          case Univ => s"!$f"
          case Empty => s"!$f"
          case Cst(_) => s"!$f"
          case ElemSet(_) => s"!$f"
          case Var(_) => s"!$f"
          case _ => s"!($f)"
        }
        case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
          stringOfSubformulas(" ∩ ", elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
        case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
          stringOfSubformulas(" ∪ ", elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
      }
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
      * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use
      * disjoint integers.
      */
    final case class Cst(c: Int) extends SetFormula

    /**
      * A set variable (`x42`), i.e. a set that we do not know but should learn assumptions about.
      *
      * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use
      * disjoint integers.
      */
    final case class Var(x: Int) extends SetFormula

    /**
      * A concrete, non-empty set/union of elements (`e42` or `e42+43`).
      *
      * Remember that the universe is infinite so an element set is never equivalent to universe.
      *
      * Invariant: `s` is non-empty.
      *
      * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use
      * disjoint integers.
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
        assert(!varsPos.exists(varsNeg.contains), message = this.toString)
        assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
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
        assert(!varsPos.exists(varsNeg.contains), message = this.toString)
        assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
        assert(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList.sizeIs >= 2, message = this.toString)
      }
    }

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
      * Returns [[ElemSet]] if `s` is non-empty.
      *
      * Returns `None` if `s` is empty.
      */
    def mkElemSetOpt(s: SortedSet[Int]): Option[ElemSet] = {
      // Maintains that ElemSet is non-empty.
      if (s.isEmpty) None else Some(ElemSet(s))
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
      case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        val simpleCompl = mkUnsafeUnion(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos)
        mkUnionAll(simpleCompl :: other.map(mkCompl))
      case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        val simpleCompl = mkUnsafeInter(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos)
        mkInterAll(simpleCompl :: other.map(mkCompl))
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
      * Returns the intersection of `fs` (`fs1 ∩ fs2 ∩ ..`).
      *
      * Nested intersections are put into a single intersection.
      */
    def mkInterAll(fs: List[SetFormula]): SetFormula = {
      // We need to do two things:
      // - Separate subformulas into specific buckets of (neg) elements/variables/constants.
      // - Flatten nested intersections.

      // To avoid set computing intersections of `Option[ElemSet]`, use `CofiniteIntSet`.
      var elemPos0 = CofiniteIntSet.universe
      val cstsPos = MutSet.empty[Cst]
      val varsPos = MutSet.empty[Var]
      var elemNeg0 = SortedSet.empty[Int]
      val cstsNeg = MutSet.empty[Cst]
      val varsNeg = MutSet.empty[Var]
      val other = ListBuffer.empty[SetFormula]

      // Variables and constants are checked for overlap immediately.
      // Elements are checked at the end.

      var workList = fs
      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
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
            elemPos0 = CofiniteIntSet.intersection(elemPos0, CofiniteIntSet.mkSet(s))
            if (elemPos0.isEmpty) return Empty
          case Compl(ElemSet(s)) =>
            elemNeg0 = elemNeg0.union(s)
          case x@Var(_) =>
            if (varsNeg.contains(x)) return Empty
            varsPos += x
          case Compl(x@Var(_)) =>
            if (varsPos.contains(x)) return Empty
            varsNeg += x
          case Inter(elemPos1, cstsPos1, varsPos1, elemNeg1, cstsNeg1, varsNeg1, other1) =>
            for (e <- elemPos1) {
              elemPos0 = CofiniteIntSet.intersection(elemPos0, CofiniteIntSet.mkSet(e.s))
              if (elemPos0.isEmpty) return Empty
            }
            for (x <- cstsPos1) {
              if (cstsNeg.contains(x)) return Empty
              cstsPos += x
            }
            for (x <- varsPos1) {
              if (varsNeg.contains(x)) return Empty
              varsPos += x
            }
            for (e <- elemNeg1) {
              elemNeg0 = elemNeg0.union(e.s)
            }
            for (x <- cstsNeg1) {
              if (cstsPos.contains(x)) return Empty
              cstsNeg += x
            }
            for (x <- varsNeg1) {
              if (varsPos.contains(x)) return Empty
              varsNeg += x
            }
            workList = other1 ++ workList
          case union@Union(_, _, _, _, _, _, _) =>
            other += union
          case compl@Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets (could be done inline).
      val (elemPos, elemNeg) = elemPos0 match {
        case _ if elemPos0.isUniverse =>
          // univ ∩ !elemNeg0 ∩ ..
          // = !elemNeg0 ∩ ..
          (None, mkElemSetOpt(elemNeg0))
        case CofiniteIntSet.Set(s) =>
          // s ∩ !elemNeg0 ∩ ..
          // = (s ∩ !elemNeg0) ∩ ..      (intersection associativity)
          // = (s - elemNeg0) ∩ ..       (difference definition)
          mkElemSetOpt(s.diff(elemNeg0)) match {
            case Some(e) => (Some(e), None)
            case None =>
              // empty ∩ ..
              // = empty
              return Empty
          }
        case compl@CofiniteIntSet.Compl(_) =>
          // Unreachable since we never only intersect finite sets with universe.
          throw InternalCompilerException(s"Impossible co-finite set $compl", SourceLocation.Unknown)
      }

      // Reduce intersections with zero or one subformula.
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
        case Nil => return Univ
        case one :: Nil => return one
        case _ => ()
      }
      Inter(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
    }

    /**
      * Returns the intersection of subformulas from another union or intersection (`∩`).
      *
      * More efficient than the other constructors when applicable.
      *
      * OBS: Must be called with collections from existing intersections or unions.
      */
    def mkUnsafeInter(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var]): SetFormula =
      Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil)

    /** Returns the union of `ts` (`ts1 ∪ ts2 ∪ ..`).
      *
      * Nested unions are put into a single union.
      */
    def mkUnionAll(ts: List[SetFormula]): SetFormula = {
      // We need to do two things:
      // - Separate subformulas into specific buckets of elements/variables/constants/etc.
      // - Flatten nested unions.

      var elemPos0 = SortedSet.empty[Int]
      val cstsPos = MutSet.empty[Cst]
      val varsPos = MutSet.empty[Var]
      // To avoid set computing intersections of `Option[ElemSet]`, use `CofiniteIntSet`.
      var elemNeg0 = CofiniteIntSet.universe
      val cstsNeg = MutSet.empty[Cst]
      val varsNeg = MutSet.empty[Var]
      val other = ListBuffer.empty[SetFormula]

      // Variables and constants are checked for overlap immediately.
      // Elements are checked at the end.

      var workList = ts
      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
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
            elemPos0 = elemPos0.union(s)
          case Compl(ElemSet(s)) =>
            elemNeg0 = CofiniteIntSet.intersection(elemNeg0, CofiniteIntSet.mkSet(s))
            if (elemNeg0.isEmpty) return Univ
          case x@Var(_) =>
            if (varsNeg.contains(x)) return Univ
            varsPos += x
          case Compl(x@Var(_)) =>
            if (varsPos.contains(x)) return Univ
            varsNeg += x
          case Union(elemPos1, cstsPos1, varsPos1, elemNeg1, cstsNeg1, varsNeg1, other1) =>
            for (x <- elemPos1) {
              elemPos0 = elemPos0.union(x.s)
            }
            for (x <- cstsPos1) {
              if (cstsNeg.contains(x)) return Univ
              cstsPos += x
            }
            for (x <- varsPos1) {
              if (varsNeg.contains(x)) return Univ
              varsPos += x
            }
            for (e <- elemNeg1) {
              elemNeg0 = CofiniteIntSet.intersection(elemNeg0, CofiniteIntSet.mkSet(e.s))
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
            workList = other1 ++ workList
          case inter@Inter(_, _, _, _, _, _, _) =>
            other += inter
          case compl@Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets.
      val (elemPos, elemNeg) = elemNeg0 match {
        case _ if elemNeg0.isUniverse =>
          // elemPos0 ∪ !univ ∪ ..
          // = elemPos0 ∪ empty ∪ ..      (complement of univ)
          // = elemPos ∪ ..               (neutral union formula)
          (mkElemSetOpt(elemPos0), None)
        case CofiniteIntSet.Set(s) =>
          // elemPos0 ∪ !s ∪ ..
          // = (elemPos0 ∪ !s) ∪ ..       (union associativity)
          // = !!(elemPos0 ∪ !s) ∪ ..     (double complement)
          // = !(!elemPos0 ∩ s) ∪ ..      (complement distribution)
          // = !(s ∩ !elemPos0) ∪ ..      (intersection symmetry)
          // = !(s - elemPos0) ∪ ..       (difference definition)
          mkElemSetOpt(s.diff(elemPos0)) match {
            case Some(e) => (None, Some(e))
            case None =>
              // !empty ∪ ..
              // = univ ∪ ..              (complement of empty)
              // = univ
              return Univ
          }
        case compl@CofiniteIntSet.Compl(_) =>
          // Unreachable since we never only intersect finite sets with universe.
          throw InternalCompilerException(s"Impossible co-finite set $compl", SourceLocation.Unknown)
      }

      // Reduce unions with zero or one subformula.
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
        case Nil => return Empty
        case one :: Nil => return one
        case _ => ()
      }
      Union(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
    }

    /**
      * Returns the union of subformulas from another union or intersection (`∪`).
      *
      * More efficient than the other constructors when applicable.
      *
      * OBS: Must be called with collections from existing intersections or unions.
      */
    def mkUnsafeUnion(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var]): Union =
      Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil)

    /** Returns the Xor of `f1` and `f2` with the formula `(f1 - f2) ∪ (f2 - f1)`. */
    def mkXor(f1: SetFormula, f2: SetFormula): SetFormula =
      mkUnion(mkDifference(f1, f2), mkDifference(f2, f1))

    /** Returns the difference of `f1` and `f2` (`-`) with the formula `f1 ∩ !f2`. */
    def mkDifference(f1: SetFormula, f2: SetFormula): SetFormula =
      mkInter(f1, mkCompl(f2))

    /** Returns an iterator of the subformulas of the union or intersection. */
    private def subformulasOf(
                               elemPos: Iterable[ElemSet], cstsPos: Iterable[Cst], varsPos: Iterable[Var],
                               elemNeg: Iterable[ElemSet], cstsNeg: Iterable[Cst], varsNeg: Iterable[Var],
                               other: Iterable[SetFormula]
                             ): Iterator[SetFormula] = {
      elemPos.iterator ++
        cstsPos.iterator ++
        varsPos.iterator ++
        elemNeg.iterator.map(mkCompl(_)) ++
        cstsNeg.iterator.map(mkCompl(_)) ++
        varsNeg.iterator.map(mkCompl(_)) ++
        other.iterator
    }

  }

}
