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

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.{SortedMap, SortedSet}
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
    *            disjoint integers (see [[unknowns]]).
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

      @inline
      def countSetFormulas(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var], other: List[SetFormula]): Unit = {
        val negElemSize = elemNeg.size
        val negCstsSize = cstsNeg.size
        val negVarsSize = varsNeg.size
        val terms = elemPos.size + cstsPos.size + varsPos.size + negElemSize + negCstsSize + negVarsSize + other.size
        val connectives = negElemSize + negCstsSize + negVarsSize
        counter += (terms - 1) + connectives
        workList = other ++ workList
      }

      // variables and constants are immediately checked for overlap
      // elements are checked at the end

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
        case Univ => s"!$f"
        case Empty => s"!$f"
        case Cst(_) => s"!$f"
        case ElemSet(_) => s"!$f"
        case Var(_) => s"!$f"
        case _ => s"!($f)"
      }
      case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        val terms = elemPos.iterator ++
          elemNeg.iterator.map(mkCompl(_)) ++
          cstsPos.iterator ++
          cstsNeg.iterator.map(mkCompl(_)) ++
          varsPos.iterator ++
          varsNeg.iterator.map(mkCompl(_)) ++
          other.iterator
        s"(${terms.mkString(" ∩ ")})"
      case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        val terms = elemPos.iterator ++
          elemNeg.iterator.map(mkCompl(_)) ++
          cstsPos.iterator ++
          cstsNeg.iterator.map(mkCompl(_)) ++
          varsPos.iterator ++
          varsNeg.iterator.map(mkCompl(_)) ++
          other.iterator
        s"(${terms.mkString(" ∪ ")})"
    }

  }

  object SetFormula {

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
      *            disjoint integers.
      */
    final case class Cst(c: Int) extends SetFormula

    /**
      * A set variable (`x42`), i.e. a set that we do not know but should learn assumptions about.
      *
      * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use
      *            disjoint integers.
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
      *            disjoint integers.
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[ElemSet]], so this is
      * fine.
      */
    @nowarn
    final case class ElemSet private(s: SortedSet[Int]) extends SetFormula {
      assert(s.nonEmpty)
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
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
      assert(iteratorSizeGreaterEq(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other), 2), message = this.toString)
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
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
      assert(iteratorSizeGreaterEq(subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other), 2), message = this.toString)
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
      if (s.isEmpty) Empty
      else ElemSet(s)
    }

    /**
      * Returns [[ElemSet]] if `s` is non-empty.
      *
      * Returns `None` if `s` is empty.
      */
    def mkElemSetOpt(s: SortedSet[Int]): Option[ElemSet] =
    // Maintains that ElemSet is non-empty.
      if (s.isEmpty) None
      else Some(ElemSet(s))

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
      // Optimized case for simple intersections.
      case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil) =>
        Union(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos, Nil)
      // General intersection case.
      case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        reconstructUnion(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos, other.map(mkCompl))
      // Optimized case for simple unions.
      case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil) =>
        Inter(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos, Nil)
      // General union case.
      case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
        reconstructInter(elemNeg, cstsNeg, varsNeg, elemPos, cstsPos, varsPos, other.map(mkCompl))
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
      // - Separate terms into specific buckets of elements/variables/constants/etc.
      // - Flatten nested intersections

      /** Returns the intersection, interpreting `None` as `univ`. */
      @inline
      def intersectionSet(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      /** Returns the intersection, interpreting `None` as `univ`. */
      @inline
      def intersectionOptSet(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0: Option[SortedSet[Int]] = None // The neutral element `univ` is not a set, so we use `None`.
      val cstsPos = MutSet.empty[Cst]
      val varsPos = MutSet.empty[Var]
      var negElem0 = SortedSet.empty[Int]
      val cstsNeg = MutSet.empty[Cst]
      val varsNeg = MutSet.empty[Var]
      val other = ListBuffer.empty[SetFormula]

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
            val inter = intersectionSet(posElem0, s)
            if (inter.isEmpty) return Empty
            posElem0 = Some(inter)
          case Compl(ElemSet(s)) =>
            negElem0 = negElem0.union(s)
          case x@Var(_) =>
            if (varsNeg.contains(x)) return Empty
            varsPos += x
          case Compl(x@Var(_)) =>
            if (varsPos.contains(x)) return Empty
            varsNeg += x
          case Inter(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            posElem0 = intersectionOptSet(posElem1.map(_.s), posElem0)
            if (posElem0.contains(SortedSet.empty[Int])) return Empty
            for (x <- posCsts1) {
              if (cstsNeg.contains(x)) return Empty
              cstsPos += x
            }
            for (x <- posVars1) {
              if (varsNeg.contains(x)) return Empty
              varsPos += x
            }
            for (e <- negElem1) {
              negElem0 = negElem0.union(e.s)
            }
            for (x <- negCsts1) {
              if (cstsPos.contains(x)) return Empty
              cstsNeg += x
            }
            for (x <- negVars1) {
              if (varsPos.contains(x)) return Empty
              varsNeg += x
            }
            workList = rest1 ++ workList
          case union@Union(_, _, _, _, _, _, _) =>
            other += union
          case compl@Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets (could be done inline)
      val elemPos = posElem0 match {
        case Some(s) =>
          // s ∩ !neg ∩ .. = (s - neg) ∩ !neg ∩ ..
          val posElemSet = s.diff(negElem0)
          if (posElemSet.isEmpty) return Empty
          else Some(ElemSet(posElemSet))
        case None => None
      }
      val elemNeg = mkElemSetOpt(negElem0)

      // Reduce intersections with zero or one subformula
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
        case Nil => return Univ
        case one :: Nil => return one
        case _ => ()
      }
      Inter(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
    }

    /**
      * Re-constructor for intersection (`∩`).
      *
      * More efficient than the other constructors when building an intersection
      * based on an existing intersection/union.
      *
      * OBS: Must be called with collections from existing intersections/unions.
      */
    def reconstructInter(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var], other: List[SetFormula]): SetFormula = {
      val maintain = Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil)
      mkInterAll(maintain :: other)
    }

    /** Returns the union of `ts` (`ts1 ∪ ts2 ∪ ..`).
      *
      * Nested unions are put into a single union.
      */
    def mkUnionAll(ts: List[SetFormula]): SetFormula = {
      // We need to do two things:
      // - Separate terms into specific buckets of elements/variables/constants/etc.
      // - Flatten nested unions

      /** Returns the intersection, interpreting `None` as `univ`. */
      @inline
      def intersectionSet(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      /** Returns the intersection, interpreting `None` as `univ`. */
      @inline
      def intersectionOptSet(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0 = SortedSet.empty[Int]
      val cstsPos = MutSet.empty[Cst]
      val varsPos = MutSet.empty[Var]
      var negElem0: Option[SortedSet[Int]] = None // The neutral element `!univ` is not a set, so we use `None`.
      val cstsNeg = MutSet.empty[Cst]
      val varsNeg = MutSet.empty[Var]
      val other = ListBuffer.empty[SetFormula]

      // variables and constants are immediately checked for overlap
      // elements are checked at the end

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
            posElem0 = posElem0.union(s)
          case Compl(ElemSet(s)) =>
            // !negElem0 ∪ !s
            // = !!(!negElem0 ∪ !s)      (double complement)
            // = !(negElem0 ∩ s)         (complement distribution)
            val intersection = intersectionSet(negElem0, s)
            if (intersection.isEmpty) return Univ
            negElem0 = Some(intersection)
          case x@Var(_) =>
            if (varsNeg.contains(x)) return Univ
            varsPos += x
          case Compl(x@Var(_)) =>
            if (varsPos.contains(x)) return Univ
            varsNeg += x
          case Union(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            for (x <- posElem1) {
              posElem0 = posElem0.union(x.s)
            }
            for (x <- posCsts1) {
              if (cstsNeg.contains(x)) return Univ
              cstsPos += x
            }
            for (x <- posVars1) {
              if (varsNeg.contains(x)) return Univ
              varsPos += x
            }
            negElem0 = intersectionOptSet(negElem1.map(_.s), negElem0)
            if (negElem0.contains(SortedSet.empty[Int])) return Univ
            for (x <- negCsts1) {
              if (cstsPos.contains(x)) return Univ
              cstsNeg += x
            }
            for (x <- negVars1) {
              if (varsPos.contains(x)) return Univ
              varsNeg += x
            }
            workList = rest1 ++ workList
          case inter@Inter(_, _, _, _, _, _, _) =>
            other += inter
          case compl@Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets
      val (elemPos, elemNeg) = negElem0 match {
        case Some(s) =>
          // posElem0 ∪ !s ∪ ..
          // = (posElem0 ∪ !s) ∪ ..       (union associativity)
          // = !!(posElem0 ∪ !s) ∪ ..     (double complement)
          // = !(!posElem0 ∩ s) ∪ ..      (complement distribution)
          // = !(s ∩ !posElem0) ∪ ..      (intersection symmetry)
          // = !(s - posElem0) ∪ ..       (difference definition)
          val negElemSet = s.diff(posElem0)
          if (negElemSet.isEmpty) return Univ
          else (None, mkElemSetOpt(negElemSet))
        case None =>
          // posElem0 ∪ empty ∪ ..
          (mkElemSetOpt(posElem0), None)
      }

      // Reduce unions with zero or one subformula
      subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).take(2).toList match {
        case Nil => return Empty
        case one :: Nil => return one
        case _ => ()
      }
      Union(elemPos, cstsPos.toSet, varsPos.toSet, elemNeg, cstsNeg.toSet, varsNeg.toSet, other.toList)
    }

    /**
      * Re-constructor for union (`∪`).
      *
      * More efficient than the other constructors when building an union
      * based on an existing intersection/union.
      *
      * OBS: Must be called with collections from existing intersections/unions.
      */
    def reconstructUnion(elemPos: Option[ElemSet], cstsPos: Set[Cst], varsPos: Set[Var], elemNeg: Option[ElemSet], cstsNeg: Set[Cst], varsNeg: Set[Var], other: List[SetFormula]): SetFormula = {
      val maintain = Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, Nil)
      mkUnionAll(maintain :: other)
    }

    /** Returns the Xor of `f1` and `f2` with the formula `(f1 - f2) ∪ (f2 - f1)`. */
    def mkXor(f1: SetFormula, f2: SetFormula): SetFormula =
      mkUnion(mkMinus(f1, f2), mkMinus(f2, f1))

    /** Returns the Minus of `f1` and `f2` (`-`) with the formula `f1 ∩ !f2`. */
    final private def mkMinus(f1: SetFormula, f2: SetFormula): SetFormula =
      mkInter(f1, mkCompl(f2))

    /**
      * Returns a point-wise substitution for `{e1 ∪ e2 ∪ .. -> target}` if possible, otherwise an empty map.
      *
      *   - `{e1 -> target}` becomes `Map(e1 -> target)`
      *   - `{e1 ∪ e2 ∪ .. -> empty` becomes `Map(e1 -> empty, e2 -> empty, ..)`
      *   - otherwise `empty`
      */
    private def setElemOne[T <: SetFormula](elem: ElemSet, target: T): SortedMap[Int, T] = {
      if (elem.s.sizeIs == 1) SortedMap(elem.s.head -> target)
      else if (target == Empty) setElemPointwise(elem, target)
      else SortedMap.empty[Int, T]
    }

    /** Returns a map with `e -> target` for each `e` in `elem`. */
    private def setElemPointwise[T <: SetFormula](elem: ElemSet, target: T): SortedMap[Int, T] = {
      elem.s.foldLeft(SortedMap.empty[Int, T]) { case (acc, i) => acc + (i -> target) }
    }

    /** Returns an iterator of the subterms of the union or intersection parts. */
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

    private def iteratorSizeGreaterEq[T](i: Iterator[T], size: Int): Boolean = {
      @tailrec
      def helper(accSize: Int): Boolean = {
        if (accSize >= size) true
        else if (i.hasNext) {
          i.next()
          helper(accSize + 1)
        } else {
          false
        }
      }
      helper(0)
    }
  }
}
