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

import scala.annotation.nowarn
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.{ListBuffer, Set as MutSet}

object FastSetUnification {

  /**
    * A common super-type for set formulas like `x1 ∩ x2 ∪ (e4 ∪ !c17)`.
    *
    * A set formula can contain elements, constants (unknown sets), variables (unknown sets),
    * complements of sets, unions of sets, and intersections of sets.
    *
    * Variables and constants are collectively referred to as unknowns.
    * A formula without unknowns is called ground.
    *
    * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use
    *            disjoint integers (see [[SetFormula.unknowns]]).
    */
  sealed trait SetFormula {

    /**
      * The set of variables in `this`.
      *
      * This set is computed on construction.
      */
    final val variables: SortedSet[Int] = this match {
      case SetFormula.Univ => SortedSet.empty
      case SetFormula.Empty => SortedSet.empty
      case SetFormula.Cst(_) => SortedSet.empty
      case SetFormula.Var(x) => SortedSet(x)
      case SetFormula.ElemSet(_) => SortedSet.empty
      case SetFormula.Compl(t) => t.variables
      case SetFormula.Inter(_, _, posVars, _, _, negVars, other) =>
        SortedSet.from(posVars.iterator.map(_.x)) ++ negVars.iterator.map(_.x) ++ other.iterator.flatMap(_.variables)
      case SetFormula.Union(_, _, posVars, _, _, negVars, other) =>
        SortedSet.from(posVars.iterator.map(_.x)) ++ negVars.iterator.map(_.x) ++ other.iterator.flatMap(_.variables)
    }

    /**
      * `true` if `this` term contains neither [[SetFormula.Var]] nor [[SetFormula.Cst]].
      *
      * This value is computed on construction.
      */
    final val ground: Boolean = this match {
      case SetFormula.Univ => true
      case SetFormula.Empty => true
      case SetFormula.Cst(_) => false
      case SetFormula.Var(_) => false
      case SetFormula.ElemSet(_) => true
      case SetFormula.Compl(t) => t.ground
      case SetFormula.Inter(_, posCsts, posVars, _, negCsts, negVars, other) =>
        posCsts.isEmpty && posVars.isEmpty && negCsts.isEmpty && negVars.isEmpty && other.forall(_.ground)
      case SetFormula.Union(_, posCsts, posVars, _, negCsts, negVars, other) =>
        posCsts.isEmpty && posVars.isEmpty && negCsts.isEmpty && negVars.isEmpty && other.forall(_.ground)
    }

    /**
      * Returns all [[SetFormula.Var]] and [[SetFormula.Cst]] and constants that occur in `this`.
      *
      * This set is not computed on construction so it traverses `this`.
      */
    final def unknowns: SortedSet[Int] = this match {
      case SetFormula.Univ => SortedSet.empty
      case SetFormula.Empty => SortedSet.empty
      case SetFormula.Cst(c) => SortedSet(c)
      case SetFormula.ElemSet(_) => SortedSet.empty
      case SetFormula.Var(x) => SortedSet(x)
      case SetFormula.Compl(t) => t.unknowns
      case SetFormula.Inter(_, posCsts, posVars, _, negCsts, negVars, other) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ other.flatMap(_.unknowns)
      case SetFormula.Union(_, posCsts, posVars, _, negCsts, negVars, other) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ other.flatMap(_.unknowns)
    }

    /**
      * Returns the number of unary/binary connectives in `this` term.
      *
      * This metric is not influenced by the specific representation.
      */
    final def size: Int = this match {
      case SetFormula.Univ => 0
      case SetFormula.Empty => 0
      case SetFormula.Cst(_) => 0
      case SetFormula.Var(_) => 0
      case SetFormula.ElemSet(_) => 0
      case SetFormula.Compl(t) => t.size + 1
      case SetFormula.Inter(_, _, _, _, _, _, _) => SetFormula.sizes(List(this))
      case SetFormula.Union(_, _, _, _, _, _, _) => SetFormula.sizes(List(this))
    }

    /** Returns a human-readable representation of `this`. */
    override final def toString: String = this match {
      case SetFormula.Univ => "univ"
      case SetFormula.Empty => "empty"
      case SetFormula.Cst(c) => s"c$c"
      case SetFormula.ElemSet(s) if s.sizeIs == 1 => s"e${s.head}"
      case SetFormula.ElemSet(s) => s"e${s.mkString("+")}"
      case SetFormula.Var(x) => s"x$x"
      case SetFormula.Compl(f) => f match {
        case SetFormula.Univ => s"!$f"
        case SetFormula.Empty => s"!$f"
        case SetFormula.Cst(_) => s"!$f"
        case SetFormula.ElemSet(_) => s"!$f"
        case SetFormula.Var(_) => s"!$f"
        case _ => s"!($f)"
      }
      case SetFormula.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
        val terms = posElem.toList ++ negElem.map(SetFormula.mkCompl(_)) ++ posCsts ++ negCsts.map(SetFormula.mkCompl(_)) ++ posVars ++ negVars.map(SetFormula.mkCompl(_)) ++ other
        s"(${terms.mkString(" ∩ ")})"
      case SetFormula.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
        val terms = posElem.toList ++ negElem.map(SetFormula.mkCompl(_)) ++ posCsts ++ negCsts.map(SetFormula.mkCompl(_)) ++ posVars ++ negVars.map(SetFormula.mkCompl(_)) ++ other
        s"(${terms.mkString(" ∪ ")})"
    }
  }

  object SetFormula {

    /** A trait to refer to [[Univ]] and [[Empty]] collectively. */
    sealed trait UniverseOrEmpty extends SetFormula

    /** The full universe set (`univ`). */
    final case object Univ extends SetFormula with UniverseOrEmpty

    /** The empty set (`empty`). */
    final case object Empty extends SetFormula with UniverseOrEmpty

    /**
      * An uninterpreted constant set (`c42`), i.e. a set that we do not know and cannot make
      * assumptions about.
      *
      * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use
      *            disjoint integers.
      */
    final case class Cst(c: Int) extends SetFormula

    /**
      * A set variable (`x42`), i.e. a set that we do not know but should learn assumptions about.
      *
      * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use
      *            disjoint integers.
      */
    final case class Var(x: Int) extends SetFormula

    /**
      * A concrete, non-empty set/union of elements (`e42` or `e42+43`).
      *
      * Invariant: `s` is non-empty.
      *
      * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use
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
      * elemsPos = Some({e1, e2}),
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
      * Property: An empty intersection is [[SetFormula.Univ]].
      *
      * Invariant: `elemsPos` and `elemNeg` are disjoint.
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[Inter]], so this is
      * fine.
      */
    @nowarn
    final case class Inter private(
                                    elemsPos: Option[SetFormula.ElemSet], cstsPos: Set[SetFormula.Cst], varsPos: Set[SetFormula.Var],
                                    elemNeg: Option[SetFormula.ElemSet], cstsNeg: Set[SetFormula.Cst], varsNeg: Set[SetFormula.Var],
                                    other: List[SetFormula]
                                  ) extends SetFormula {
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
    }

    /**
      * A union of formulas (`f1 ∪ f2`).
      *
      * The union
      * {{{
      * elemsPos = Some({e1, e2}),
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
      * Property: An empty union is [[SetFormula.Empty]].
      *
      * Invariant: `elemsPos` and `elemNeg` are disjoint.
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[Union]], so this is
      * fine.
      */
    @nowarn
    final case class Union private(
                                    elemPos: Option[SetFormula.ElemSet], cstsPos: Set[SetFormula.Cst], varsPos: Set[SetFormula.Var],
                                    elemNeg: Option[SetFormula.ElemSet], cstsNeg: Set[SetFormula.Cst], varsNeg: Set[SetFormula.Var],
                                    other: List[SetFormula]
                                  ) extends SetFormula {
      assert(!varsPos.exists(varsNeg.contains), message = this.toString)
      assert(!cstsPos.exists(cstsNeg.contains), message = this.toString)
    }

    /** Returns a singleton [[SetFormula.ElemSet]]. */
    def mkElemSet(e: Int): SetFormula.ElemSet = {
      // Maintains that ElemSet is non-empty.
      SetFormula.ElemSet(SortedSet(e))
    }

    /**
      * Returns [[SetFormula.ElemSet]] if `s` is non-empty.
      *
      * Returns [[SetFormula.Empty]] if `s` is empty.
      */
    def mkElemSet(s: SortedSet[Int]): SetFormula = {
      // Maintains that ElemSet is non-empty.
      if (s.isEmpty) SetFormula.Empty
      else SetFormula.ElemSet(s)
    }

    /**
      * Returns [[SetFormula.ElemSet]] if `s` is non-empty.
      *
      * Returns `None` if `s` is empty.
      */
    def mkElemSetOpt(s: SortedSet[Int]): Option[ElemSet] =
      // Maintains that ElemSet is non-empty.
      if (s.isEmpty) None
      else Some(SetFormula.ElemSet(s))

    /**
      * Returns the complement of `f` (`!f`).
      *
      * Complements are pushed to the bottom of the formula.
      *
      * Example
      *   - `mkCompl(x ∩ y) = !x ∪ !y`
      */
    def mkCompl(f: SetFormula): SetFormula = f match {
      case Univ => Empty
      case Empty => Univ
      case cst@Cst(_) => Compl(cst)
      case v@Var(_) => Compl(v)
      case e@ElemSet(_) => Compl(e)
      case Compl(f1) => f1
      // Optimized case for simple intersections.
      case Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil) =>
        SetFormula.Union(negElem, negCsts, negVars, posElem, posCsts, posVars, Nil)
      // General intersection case.
      case Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
        reconstructUnion(negElem, negCsts, negVars, posElem, posCsts, posVars, other.map(mkCompl))
      // Optimized case for simple unions.
      case Union(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil) =>
        SetFormula.Inter(negElem, negCsts, negVars, posElem, posCsts, posVars, Nil)
      // General union case.
      case Union(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
        reconstructInter(negElem, negCsts, negVars, posElem, posCsts, posVars, other.map(mkCompl))
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
      // - Separate terms into specific buckets
      // - Flatten nested intersections

      @inline
      def interOpt(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      @inline
      def interOptOpt(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0: Option[SortedSet[Int]] = None // `None` represents `univ`
      val posCsts = MutSet.empty[SetFormula.Cst]
      val posVars = MutSet.empty[SetFormula.Var]
      var negElem0 = SortedSet.empty[Int]
      val negCsts = MutSet.empty[SetFormula.Cst]
      val negVars = MutSet.empty[SetFormula.Var]
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
          case c@SetFormula.Cst(_) =>
            if (negCsts.contains(c)) return Empty
            posCsts += c
          case SetFormula.Compl(c@SetFormula.Cst(_)) =>
            if (posCsts.contains(c)) return Empty
            negCsts += c
          case SetFormula.ElemSet(s) =>
            val inter = interOpt(posElem0, s)
            if (inter.isEmpty) return Empty
            posElem0 = Some(inter)
          case SetFormula.Compl(SetFormula.ElemSet(s)) =>
            negElem0 = negElem0.union(s)
          case x@SetFormula.Var(_) =>
            if (negVars.contains(x)) return Empty
            posVars += x
          case SetFormula.Compl(x@SetFormula.Var(_)) =>
            if (posVars.contains(x)) return Empty
            negVars += x
          case Inter(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            posElem0 = interOptOpt(posElem1.map(_.s), posElem0)
            if (posElem0.contains(SortedSet.empty[Int])) return Empty
            for (x <- posCsts1) {
              if (negCsts.contains(x)) return Empty
              posCsts += x
            }
            for (x <- posVars1) {
              if (negVars.contains(x)) return Empty
              posVars += x
            }
            for (e <- negElem1) {
              negElem0 = negElem0.union(e.s)
            }
            for (x <- negCsts1) {
              if (posCsts.contains(x)) return Empty
              negCsts += x
            }
            for (x <- negVars1) {
              if (posVars.contains(x)) return Empty
              negVars += x
            }
            workList = rest1 ++ workList
          case union@SetFormula.Union(_, _, _, _, _, _, _) =>
            other += union
          case compl@SetFormula.Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets (could be done inline)
      val posElem = posElem0 match {
        case Some(s) =>
          // s ∩ !neg ∩ .. = (s - neg) ∩ !neg ∩ ..
          val posElemSet = s.diff(negElem0)
          if (posElemSet.isEmpty) return Empty
          else Some(SetFormula.ElemSet(posElemSet))
        case None => None
      }
      val negElem = SetFormula.mkElemSetOpt(negElem0)

      // Eliminate intersections with zero or one subterm
      val elms = posElem.size + posCsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + other.size
      if (elms == 0) {
        return SetFormula.Univ
      } else if (elms == 1) {
        for (any <- posElem) return any
        for (any <- posCsts) return any
        for (any <- posVars) return any
        for (any <- negElem) return SetFormula.mkCompl(any)
        for (any <- negCsts) return SetFormula.mkCompl(any)
        for (any <- negVars) return SetFormula.mkCompl(any)
        for (any <- other) return any
      }
      Inter(posElem, posCsts.toSet, posVars.toSet, negElem, negCsts.toSet, negVars.toSet, other.toList)
    }

    /**
      * Re-constructor for intersection (`∩`).
      *
      * More efficient than the other constructors when building an intersection
      * based on an existing intersection/union.
      *
      * OBS: Must be called with collections from existing intersections/unions.
      */
    def reconstructInter(posElem: Option[SetFormula.ElemSet], posCsts: Set[SetFormula.Cst], posVars: Set[SetFormula.Var], negElem: Option[SetFormula.ElemSet], negCsts: Set[SetFormula.Cst], negVars: Set[SetFormula.Var], other: List[SetFormula]): SetFormula = {
      val maintain = Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
      mkInterAll(maintain :: other)
    }

    /** Returns the union of `ts` (`ts1 ∪ ts2 ∪ ..`).
      *
      * Nested unions are put into a single union.
      */
    def mkUnionAll(ts: List[SetFormula]): SetFormula = {
      // We need to do two things:
      // - Separate terms into specific buckets
      // - Flatten nested unions

      @inline
      def unionNegOpt(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      @inline
      def unionNegOptOpt(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0 = SortedSet.empty[Int]
      val poscsts = MutSet.empty[SetFormula.Cst]
      val posVars = MutSet.empty[SetFormula.Var]
      var negElem0: Option[SortedSet[Int]] = None // `None` represents `empty`
      val negCsts = MutSet.empty[SetFormula.Cst]
      val negVars = MutSet.empty[SetFormula.Var]
      val other = ListBuffer.empty[SetFormula]

      var workList = ts
      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
          case Empty =>
            ()
          case Univ =>
            return Univ
          case x@SetFormula.Cst(_) =>
            if (negCsts.contains(x)) return Univ
            poscsts += x
          case SetFormula.Compl(x@SetFormula.Cst(_)) =>
            if (poscsts.contains(x)) return Univ
            negCsts += x
          case SetFormula.ElemSet(s) =>
            posElem0 = posElem0.union(s)
          case SetFormula.Compl(SetFormula.ElemSet(s)) =>
            val union = unionNegOpt(negElem0, s)
            if (union.isEmpty) return Univ
            negElem0 = Some(union)
          case x@SetFormula.Var(_) =>
            if (negVars.contains(x)) return Univ
            posVars += x
          case SetFormula.Compl(x@SetFormula.Var(_)) =>
            if (posVars.contains(x)) return Univ
            negVars += x
          case Union(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            for (x <- posElem1) {
              posElem0 = posElem0.union(x.s)
            }
            for (x <- posCsts1) {
              if (negCsts.contains(x)) return Univ
              poscsts += x
            }
            for (x <- posVars1) {
              if (negVars.contains(x)) return Univ
              posVars += x
            }
            negElem0 = unionNegOptOpt(negElem1.map(_.s), negElem0)
            if (negElem0.contains(SortedSet.empty[Int])) return Univ
            for (x <- negCsts1) {
              if (poscsts.contains(x)) return Univ
              negCsts += x
            }
            for (x <- negVars1) {
              if (posVars.contains(x)) return Univ
              negVars += x
            }
            workList = rest1 ++ workList
          case inter@SetFormula.Inter(_, _, _, _, _, _, _) =>
            other += inter
          case compl@SetFormula.Compl(_) =>
            other += compl
        }
      }
      // Check overlaps in the element sets (could be done inline)
      val (posElem, negElem) = negElem0 match {
        case Some(s) =>
          // pos ∪ !s ∪ .. = !(!pos ∩ s) ∪ .. = !(s - pos) ∪ ..
          val negElemSet = s.diff(posElem0)
          if (negElemSet.isEmpty) return SetFormula.Univ
          else (None, SetFormula.mkElemSetOpt(negElemSet))
        case None =>
          (SetFormula.mkElemSetOpt(posElem0), None)
      }

      // Eliminate unions with zero or one subterm
      val elms = posElem.size + poscsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + other.size
      if (elms == 0) {
        return SetFormula.Empty
      } else if (elms == 1) {
        for (any <- posElem) return any
        for (any <- poscsts) return any
        for (any <- posVars) return any
        for (any <- negElem) return SetFormula.mkCompl(any)
        for (any <- negCsts) return SetFormula.mkCompl(any)
        for (any <- negVars) return SetFormula.mkCompl(any)
        for (any <- other) return any
      }
      Union(posElem, poscsts.toSet, posVars.toSet, negElem, negCsts.toSet, negVars.toSet, other.toList)
    }

    /**
      * Re-constructor for union (`∪`).
      *
      * More efficient than the other constructors when building an union
      * based on an existing intersection/union.
      *
      * OBS: Must be called with collections from existing intersections/unions.
      */
    def reconstructUnion(posElem: Option[SetFormula.ElemSet], posCsts: Set[SetFormula.Cst], posVars: Set[SetFormula.Var], negElem: Option[SetFormula.ElemSet], negCsts: Set[SetFormula.Cst], negVars: Set[SetFormula.Var], other: List[SetFormula]): SetFormula = {
      val maintain = Union(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
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
    private def setElemOne[T <: SetFormula](elem: SetFormula.ElemSet, target: T): SortedMap[Int, T] = {
      if (elem.s.sizeIs == 1) SortedMap(elem.s.head -> target)
      else if (target == SetFormula.Empty) setElemPointwise(elem, target)
      else SortedMap.empty[Int, T]
    }

    /** Returns a map with `e -> target` for each `e` in `elem`. */
    private def setElemPointwise[T <: SetFormula](elem: SetFormula.ElemSet, target: T): SortedMap[Int, T] = {
      elem.s.foldLeft(SortedMap.empty[Int, T]) { case (acc, i) => acc + (i -> target) }
    }

    /** Returns the number of connectives in the terms `ts`. */
    def sizes(ts: List[SetFormula]): Int = {
      var workList = ts
      var counter = 0

      @inline
      def countSetFormulas(posElem: Option[SetFormula.ElemSet], posCsts: Set[SetFormula.Cst], posVars: Set[SetFormula.Var], negElem: Option[SetFormula.ElemSet], negCsts: Set[SetFormula.Cst], negVars: Set[SetFormula.Var], other: List[SetFormula]): Unit = {
        val negElemSize = negElem.size
        val negCstsSize = negCsts.size
        val negVarsSize = negVars.size
        val terms = posElem.size + posCsts.size + posVars.size + negElemSize + negCstsSize + negVarsSize + other.size
        val connectives = negElemSize + negCstsSize + negVarsSize
        counter += (terms - 1) + connectives
        workList = other ++ workList
      }

      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
          case SetFormula.Univ => ()
          case SetFormula.Empty => ()
          case SetFormula.Cst(_) => ()
          case SetFormula.Var(_) => ()
          case SetFormula.ElemSet(_) => ()
          case SetFormula.Compl(t) =>
            counter += 1
            workList = t :: workList
          case SetFormula.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
            countSetFormulas(posElem, posCsts, posVars, negElem, negCsts, negVars, other)
          case SetFormula.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, other) =>
            countSetFormulas(posElem, posCsts, posVars, negElem, negCsts, negVars, other)
        }
      }
      counter
    }
  }

}
