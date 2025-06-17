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

import ca.uwaterloo.flix.util.collection.TwoList
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
  * Invariant: [[SetFormula.ElemSet]], [[SetFormula.Cst]], and [[SetFormula.Var]] must use disjoint
  * integers.
  */
sealed trait SetFormula {

  import SetFormula.*

  /** Faster alternative to `this.varsOf.contains(v)`. */
  final def contains(v: Var): Boolean = this match {
    case Univ => false
    case Empty => false
    case Cst(_) => false
    case Var(x) => x == v.x
    case ElemSet(_) => false
    case Compl(f) => f.contains(v)
    case Inter(l) => l.exists(f => f.contains(v))
    case Union(l) => l.exists(f => f.contains(v))
    case Xor(other) => other.exists(_.contains(v))
  }

  /** `true` if `this` contains no variables. */
  final def isGround: Boolean = this match {
    case Univ => true
    case Empty => true
    case Cst(_) => true
    case Var(_) => false
    case ElemSet(_) => true
    case Compl(f) => f.isGround
    case Inter(l) => l.forall(_.isGround)
    case Union(l) => l.forall(_.isGround)
    case Xor(l) => l.forall(_.isGround)
  }

  /**
    * Returns the constants (i.e. "rigid variables") in `this` set formula.
    */
  final def cstsOf: SortedSet[Int] = this match {
    case SetFormula.Univ => SortedSet.empty
    case SetFormula.Empty => SortedSet.empty
    case Cst(x) => SortedSet(x)
    case Var(_) => SortedSet.empty
    case ElemSet(_) => SortedSet.empty
    case Compl(f) => f.cstsOf
    case Inter(l) => l.toList.map(_.cstsOf).reduce(_ ++ _)
    case Union(l) => l.toList.map(_.cstsOf).reduce(_ ++ _)
    case Xor(l) => l.foldLeft(SortedSet.empty[Int]) {
      case (acc, f) => acc ++ f.cstsOf
    }
  }

  /**
    * Returns the variables (i.e. "flexible variables") in `this` set formula.
    */
  final def varsOf: SortedSet[Int] = this match {
    case SetFormula.Univ => SortedSet.empty
    case SetFormula.Empty => SortedSet.empty
    case Cst(_) => SortedSet.empty
    case Var(x) => SortedSet(x)
    case ElemSet(_) => SortedSet.empty
    case Compl(f) => f.varsOf
    case Inter(l) => l.toList.map(_.varsOf).reduce(_ ++ _)
    case Union(l) => l.toList.map(_.varsOf).reduce(_ ++ _)
    case Xor(other) => other.foldLeft(SortedSet.empty[Int]) {
      case (acc, f) => acc ++ f.varsOf
    }
  }

  /**
    * Returns the elements (i.e. "concrete members") in `this` set formula.
    */
  final def elmsOf: SortedSet[Int] = this match {
    case SetFormula.Univ => SortedSet.empty
    case SetFormula.Empty => SortedSet.empty
    case Cst(_) => SortedSet.empty
    case Var(_) => SortedSet.empty
    case ElemSet(e) => e
    case Compl(f) => f.cstsOf
    case Inter(l) => l.toList.map(_.cstsOf).reduce(_ ++ _)
    case Union(l) => l.toList.map(_.cstsOf).reduce(_ ++ _)
    case Xor(other) => other.foldLeft(SortedSet.empty[Int]) {
      case (acc, f) => acc ++ f.cstsOf
    }
  }

  /**
    * Returns the number of connectives in `this` set formula.
    */
  final def size: Int = this match {
    case SetFormula.Univ => 0
    case SetFormula.Empty => 0
    case Cst(_) => 0
    case Var(_) => 0
    case ElemSet(_) => 0
    case Compl(f) => 1 + f.size
    case Inter(l) => l.length + l.toList.map(_.size).sum
    case Union(l) => l.length + l.toList.map(_.size).sum
    case Xor(l) => l.length + l.map(_.size).sum
  }

  /** Returns a human-readable string of `this`. */
  override final def toString: String = this match {
    case Univ => "univ"
    case Empty => "empty"
    case Cst(c) => s"c$c"
    case ElemSet(s) if s.sizeIs == 1 => s"e${s.head}"
    case ElemSet(s) => s"{${s.map(x => s"e$x").mkString(", ")}}"
    case Var(x) => s"x$x"
    case Compl(f) => f match {
      case Univ | Empty | Cst(_) | ElemSet(_) | Var(_) | Compl(_) => s"!$f"
      case Inter(_) | Union(_) | Xor(_) => s"!($f)"
    }
    case Inter(l) =>
      s"(${l.toList.mkString(" ∩ ")})"
    case Union(l) =>
      s"(${l.toList.mkString(" ∪ ")})"
    case Xor(other) =>
      s"(${other.mkString(" ⊕ ")})"
  }

}

object SetFormula {

  implicit val ordCst: Ordering[Cst] = Ordering.by(_.c)
  implicit val ordVar: Ordering[Var] = Ordering.by(_.x)

  /** A trait used to refer to [[Univ]] and [[Empty]] collectively. */
  sealed trait UnivOrEmpty extends SetFormula

  /** The full universe set (`univ`). */
  case object Univ extends SetFormula with UnivOrEmpty

  /** The empty set (`empty`). */
  case object Empty extends SetFormula with UnivOrEmpty

  /**
    * An uninterpreted constant set (`c42`), i.e. a set that we do not know and cannot make
    * assumptions about.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  case class Cst(c: Int) extends SetFormula

  /**
    * A set variable (`x42`), i.e. a set that we do not know but should instantiate to its most
    * general form via unification.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  case class Var(x: Int) extends SetFormula

  /**
    * A concrete, non-empty set/union of elements (`e42` or `e42+43`).
    *
    * Remember that the universe is infinite so an element set is never equivalent to universe.
    *
    * Invariant: `s` is non-empty.
    *
    * Invariant: [[ElemSet]], [[Cst]], and [[Var]] must use disjoint integers.
    */
  case class ElemSet(s: SortedSet[Int]) extends SetFormula {
    assert(s.nonEmpty)
  }

  /**
    * A complement of a formula (`!f`).
    */
  case class Compl(f: SetFormula) extends SetFormula

  /**
    * An intersection of at least two formulas (`f1 ∩ f2 ∩ ...`).
    */
  case class Inter(l: TwoList[SetFormula]) extends SetFormula

  /**
    * A union of at least two formulas (`f1 ∪ f2 ∪ ...`).
    */
  case class Union(l: TwoList[SetFormula]) extends SetFormula

  /**
    * A xor (symmetric difference) of formulas (`f1 ⊕ f2`).
    */
  case class Xor(other: List[SetFormula]) extends SetFormula { // TODO: Use TwoList
    other match {
      case _ :: _ :: _ => // ok
      case _ => assert(false)
    }
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
    case inter@Inter(_) =>
      Compl(inter)
    case union@Union(_) =>
      Compl(union)
    case xor@Xor(_) =>
      Compl(xor)
  }

  /**
    * Returns the intersection of `f1` and `f2`.
    */
  def mkInter(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Univ, _) => f2
    case (_, Univ) => f1
    case (Inter(TwoList(x, y, rs)), f) => Inter(TwoList(x, y, f :: rs))
    case (f, Inter(TwoList(x, y, rs))) => Inter(TwoList(x, y, f :: rs))
    case _ => Inter(TwoList(f1, f2, Nil))
  }

  /**
    * Returns the intersection of `f1`, `f2`, and `f3`.
    */
  def mkInter3(f1: SetFormula, f2: SetFormula, f3: SetFormula): SetFormula = (f1, f2, f3) match {
    // The following cases were determined by profiling.
    case (Univ, _, Univ) => f2
    case (Univ, _, _) => Inter(TwoList(f2, f3, Nil))
    case (_, _, Univ) => Inter(TwoList(f1, f2, Nil))
    case _ => Inter(TwoList(f1, f2, List(f3)))
  }

  /**
    * Returns the union of `f1` and `f2` (`f1 ∪ f2`).
    */
  def mkUnion2(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Univ, _) => Univ
    case (_, Univ) => Univ
    case (Empty, _) => f2
    case (_, Empty) => f1
    case (Union(TwoList(x, y, rs)), f) => Union(TwoList(x, y, f :: rs))
    case (f, Union(TwoList(x, y, rs))) => Union(TwoList(x, y, f :: rs))
    case _ => Union(TwoList(f1, f2, Nil))
  }

  /** Returns the Xor of `f1` and `f2` (`f1 ⊕ f2`). */
  def mkXor2(f1: SetFormula, f2: SetFormula): SetFormula = (f1, f2) match {
    case (Univ, _) => mkCompl(f2)
    case (_, Univ) => mkCompl(f1)
    case (Empty, _) => f2
    case (_, Empty) => f1
    case _ => Xor(List(f1, f2))
  }

  /**
    * Returns the xor of `fs` (`fs1 ⊕ fs2 ⊕ ...`).
    */
  def mkXorAll(fs: List[SetFormula]): SetFormula = fs match {
    case Nil => Empty
    case List(one) => one
    case _ => Xor(fs)
  }

  /** Returns the difference of `f1` and `f2` (`-`) with the formula `f1 ∩ !f2`. */
  def mkDiff2(f1: SetFormula, f2: SetFormula): SetFormula =
    mkInter(f1, mkCompl(f2))

}
