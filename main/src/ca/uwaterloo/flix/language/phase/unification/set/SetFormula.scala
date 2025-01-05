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
import ca.uwaterloo.flix.util.{CofiniteIntSet, InternalCompilerException, TwoList}

import scala.annotation.nowarn
import scala.collection.immutable.SortedSet
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
  * integers.
  */
sealed trait SetFormula {

  import SetFormula.*

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
    case Union(l) => l.exists(f => f.contains(v))
    case Xor(other) =>
      other.exists(_.contains(v))
  }

  /** `true` if `this` contains neither [[Var]] nor [[Cst]]. */
  final lazy val isGround: Boolean = this match {
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
    case Union(l) => l.forall(_.isGround)
    case Xor(other) =>
        other.forall(_.isGround)
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
                          elemPos: Option[ElemSet], cstsPos: SortedSet[Cst], varsPos: SortedSet[Var],
                          elemNeg: Option[ElemSet], cstsNeg: SortedSet[Cst], varsNeg: SortedSet[Var],
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
        case Union(l) =>
          counter += l.length - 1
          workList = l.toList ::: workList
        case Xor(other) =>
          counter +=1
          workList = other ::: workList
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
      case Inter(_, _, _, _, _, _, _) | Union(_) | Xor(_) => s"!($f)"
    }
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val subformulas = subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other)
      s"(${subformulas.mkString(" ∩ ")})"
    case Union(l) =>
      s"(${l.toList.mkString(" ∪ ")})"
    case Xor(other) =>
      s"(${other.mkString(" ⊕ ")})"
  }

}

object SetFormula {

  implicit val ordCst: Ordering[Cst] = Ordering.by(_.c)
  implicit val ordVar: Ordering[Var] = Ordering.by(_.x)

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
                                  elemPos: Option[ElemSet], cstsPos: SortedSet[Cst], varsPos: SortedSet[Var],
                                  elemNeg: Option[ElemSet], cstsNeg: SortedSet[Cst], varsNeg: SortedSet[Var],
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
  final case class Union(l: TwoList[SetFormula]) extends SetFormula {
    /** Applies `f` to the subformulas of `this`. */
    def mapSubformulas[T](f: SetFormula => T): List[T] = l.toList.map(f)
  }

  /**
    * A xor (symmetric difference) of formulas (`f1 ⊕ f2`).
    *
    * The xor
    * {{{
    * other    = List(e1 ∩ x9, x10)
    * }}}
    * represents the formula
    * {{{ (e1 ∩ x9) ⊕ x10 }}}
    *
    * Property: An empty xor is ???.
    *
    * Invariant: There is at least two formulas in the xor.
    *
    * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
    * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
    * still public. However, we do not use the `copy` method anywhere for [[Xor]], so this is
    * fine.
    */
  @nowarn
  final case class Xor private(
                              other: List[SetFormula]
                              ) extends SetFormula {
    if (CHECK_INVARIANTS) {
      // There is always at least two subformulas.
      assert(other.sizeIs >= 2, message = this.toString)
    }
  }

  /** Returns an iterator of the subformulas of the union or intersection. */
  def subformulasOf(
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
      Compl(inter)
    case union@Union(_) =>
      Compl(union)
    case xor@Xor(_) =>
      Compl(xor)
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
        case union@Union(_) =>
          other += union
        case xor@Xor(_) =>
          other += xor
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
    Inter(
      elemPos,
      SortedSet.from(cstsPos),
      SortedSet.from(varsPos),
      elemNeg,
      SortedSet.from(cstsNeg),
      SortedSet.from(varsNeg),
      other.toList
    )
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
    def visit(l: List[SetFormula], elmAcc: SortedSet[Int], seenCsts: SortedSet[Int], seenVars: SortedSet[Int]): List[SetFormula] = l match {
      case Nil => if (elmAcc.isEmpty) Nil else ElemSet(elmAcc) :: Nil

      case ElemSet(s) :: rs => visit(rs, elmAcc ++ s, seenCsts, seenVars)

      case (f@Cst(c)) :: rs =>
        if (seenCsts.contains(c))
          visit(rs, elmAcc, seenCsts, seenVars)
        else
          f :: visit(rs, elmAcc, seenCsts + c, seenVars)

      case (f@Var(x)) :: rs =>
        if (seenVars.contains(x))
          visit(rs, elmAcc, seenCsts, seenVars)
        else
          f :: visit(rs, elmAcc, seenCsts, seenVars + x)

      case Union(l2) :: rs =>
        visit(l2.toList ::: rs, elmAcc, seenCsts, seenVars)

      case f :: rs => f :: visit(rs, elmAcc, seenCsts, seenVars)
    }

    visit(fs, SortedSet.empty, SortedSet.empty, SortedSet.empty) match {
      case Nil => Empty
      case f :: Nil => f
      case f1 :: f2 :: rest => Union(TwoList(f1, f2, rest))
    }
  }

  /** Returns the Xor of `f1` and `f2` (`f1 ⊕ f2`). */
  def mkXorDirect(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Univ, _) => mkCompl(f2)
    case (_, Univ) => mkCompl(f1)
    case (Empty, _) => f2
    case (_, Empty) => f1
    case _ => Xor(List(f1, f2))
  }

  /**
    * Returns the xor of `fs` (`fs1 ⊕ fs2 ⊕ ..`).
    *
    * Nested xors are put into a single xor.
    */
  def mkXorDirectAll(fs: List[SetFormula]): SetFormula = fs match {
    case Nil => Empty
    case List(one) => one
    case _ => Xor(fs)
  }

  /** Returns the Xor of `f1` and `f2` with the formula `(f1 - f2) ∪ (f2 - f1)`. */
  def mkXor(f1: SetFormula, f2: SetFormula): SetFormula =
    mkUnion(mkDifference(f1, f2), mkDifference(f2, f1))

  /** Returns the difference of `f1` and `f2` (`-`) with the formula `f1 ∩ !f2`. */
  def mkDifference(f1: SetFormula, f2: SetFormula): SetFormula =
    mkInter(f1, mkCompl(f2))

}
