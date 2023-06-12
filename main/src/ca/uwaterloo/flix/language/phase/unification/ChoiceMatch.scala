/*
 *  Copyright 2021 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.KindedAst.RelationalChoosePattern
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec

object ChoiceMatch {

  /**
    * Returns `true` if `pat1` is less than or equal to `pat2` according to the partial order on choice patterns.
    *
    * The partial order states that one pattern is smaller than another if it is more specific (i.e. less liberal).
    *
    * The top element is the wildcard.
    *
    * Thus:
    *
    * A <= A    P <= P    x <= W for any x (where W is a wildcard).
    */
  private def leq(pat1: RelationalChoosePattern, pat2: RelationalChoosePattern): Boolean = (pat1, pat2) match {
    case (RelationalChoosePattern.Wild(_), RelationalChoosePattern.Wild(_)) => true
    case (RelationalChoosePattern.Absent(_), RelationalChoosePattern.Absent(_)) => true
    case (RelationalChoosePattern.Present(_, _, _), RelationalChoosePattern.Present(_, _, _)) => true
    case (_, RelationalChoosePattern.Wild(_)) => true
    case _ => false
  }

  /**
    * Returns `true` if the row of choice patterns `r1` is less than or equal to `r2`.
    *
    * A row is less than or equal to another row if every element of the first row is
    * pair-wise less than or equal to the corresponding element of the second row.
    *
    * Note: The rows must have the same length.
    */
  @tailrec
  private def leq(r1: List[RelationalChoosePattern], r2: List[RelationalChoosePattern]): Boolean = (r1, r2) match {
    case (Nil, Nil) => true
    case (x :: xs, y :: ys) => leq(x, y) && leq(xs, ys)
    case (xs, ys) => throw InternalCompilerException(s"Mismatched rows: '$xs' and '$ys'.", SourceLocation.Unknown)
  }

  /**
    * Returns true if the row `r` is subsumed by a row in the choice pattern match matrix `m`.
    */
  private def subsumed(r: List[RelationalChoosePattern], m: List[List[RelationalChoosePattern]]): Boolean = m.exists(r2 => leq(r, r2))

  /**
    * Computes an anti-chain on the given choice pattern match matrix `m`.
    *
    * Every element (i.e. row) in the anti-chain is incomparable to every other element.
    */
  private def antiChain(m: List[List[RelationalChoosePattern]]): List[List[RelationalChoosePattern]] = {
    @tailrec
    def visit(acc: List[List[RelationalChoosePattern]], rest: List[List[RelationalChoosePattern]]): List[List[RelationalChoosePattern]] = rest match {
      case Nil => acc.reverse
      case r :: rs =>
        if (subsumed(r, acc) || subsumed(r, rs))
          visit(acc, rs)
        else
          visit(r :: acc, rs)
    }

    visit(Nil, m)
  }

  /**
    * Attempts to combine the rows `r1` and `r2` into a generalized row.
    *
    * Returns `None` if the rows cannot be combined.
    *
    * The length of rows `r1` and `r2` must be the same.
    * The length of the (optionally) returned row is the same as `r1` and `r2`.
    */
  private def generalize(r1: List[RelationalChoosePattern], r2: List[RelationalChoosePattern]): Option[List[RelationalChoosePattern]] = {

    @tailrec
    def before(acc: List[RelationalChoosePattern], row1: List[RelationalChoosePattern], row2: List[RelationalChoosePattern]): Option[List[RelationalChoosePattern]] =
      (row1, row2) match {
        case (Nil, Nil) => None
        case (x :: xs, y :: ys) if leq(x, y) => before(x :: acc, xs, ys) // We choose x because its the cap.
        case (x :: xs, y :: ys) if leq(y, x) => before(y :: acc, xs, ys) // We choose y because its the cap.
        case (x :: xs, y :: ys) =>
          // We know that x and y are incomparable, consequent they are either A and P (or vise versa).
          // Thus we can combine them with a wildcard.
          after(RelationalChoosePattern.Wild(x.loc) :: acc, xs, ys)
        case (xs, ys) => throw InternalCompilerException(s"Mismatched lists: '$xs' and '$ys'.", SourceLocation.Unknown)
      }

    @tailrec
    def after(acc: List[RelationalChoosePattern], row1: List[RelationalChoosePattern], row2: List[RelationalChoosePattern]): Option[List[RelationalChoosePattern]] =
      (row1, row2) match {
        case (Nil, Nil) => Some(acc.reverse)
        case (x :: xs, y :: ys) if leq(x, y) => after(x :: acc, xs, ys) // We choose x because its the cap.
        case (x :: xs, y :: ys) if leq(y, x) => after(y :: acc, xs, ys) // We choose y because its the cap.
        case (x :: xs, y :: ys) =>
          // We know that x and y are incomparable. However we have "already spent" our wildcard.
          // Thus we cannot generalize the pattern.
          None
        case (xs, ys) => throw InternalCompilerException(s"Mismatched lists: '$xs' and '$ys'.", SourceLocation.Unknown)
      }

    before(Nil, r1, r2)
  }

  /**
    * Performs generalization on the given choice pattern matrix `m`.
    */
  private def generalizeAll(m: List[List[RelationalChoosePattern]]): List[List[RelationalChoosePattern]] = {
    filterMap(allDiagonalPairs(m))(p => generalize(p._1, p._2))
  }

  /**
    * Saturates the given choice pattern matrix `m`.
    */
  @tailrec
  def saturate(m: List[List[RelationalChoosePattern]]): List[List[RelationalChoosePattern]] = {
    // Computes the fixpoint on generalizeAll.
    val m1 = antiChain(m ::: generalizeAll(m))
    if (eq(m, m1)) m else saturate(m1)
  }

  /**
    * Returns `true` if the two given pattern match matrices `m1` and `m2` are equal.
    */
  private def eq(m1: List[List[RelationalChoosePattern]], m2: List[List[RelationalChoosePattern]]): Boolean = {
    def eqPat(p1: RelationalChoosePattern, p2: RelationalChoosePattern): Boolean = (p1, p2) match {
      case (RelationalChoosePattern.Wild(_), RelationalChoosePattern.Wild(_)) => true
      case (RelationalChoosePattern.Absent(_), RelationalChoosePattern.Absent(_)) => true
      case (RelationalChoosePattern.Present(_, _, _), RelationalChoosePattern.Present(_, _, _)) => true
      case _ => false
    }

    @tailrec
    def eqRow(r1: List[RelationalChoosePattern], r2: List[RelationalChoosePattern]): Boolean = (r1, r2) match {
      case (Nil, Nil) => true
      case (x :: xs, y :: ys) => eqPat(x, y) && eqRow(xs, ys)
      case _ => false
    }

    val isSubsetL = m1.forall(r1 => m2.exists(r2 => eqRow(r1, r2)))
    val isSubsetR = m2.forall(r2 => m1.exists(r1 => eqRow(r2, r1)))

    isSubsetL && isSubsetR
  }

  /**
    * Collects the result of applying the partial function `f` to every element in `l`.
    */
  private def filterMap[A, B](l: List[A])(f: A => Option[B]): List[B] = l match {
    case Nil => Nil
    case x :: xs => f(x) match {
      case None => filterMap(xs)(f)
      case Some(b) => b :: filterMap(xs)(f)
    }
  }

  /**
    * Given a list `l` returns all unordered pairs.
    *
    * E.g. 1 :: 2 :: 3 :: Nil => (1, 1) :: (1, 2) :: (1, 3) :: (2, 3) :: Nil
    */
  private def allDiagonalPairs[T](l: List[T]): List[(T, T)] = l match {
    case Nil => Nil
    case x :: xs => xs.map(y => (x, y)) ::: allDiagonalPairs(xs)
  }

  /**
    * Converts the given choice pattern match matrix `m` into a readable form.
    */
  def toPrettyString(m: List[List[RelationalChoosePattern]]): String = {
    def toPrettyString(p: RelationalChoosePattern): String = p match {
      case RelationalChoosePattern.Wild(_) => "W"
      case RelationalChoosePattern.Absent(_) => "A"
      case RelationalChoosePattern.Present(_, _, _) => "P"
    }

    m.map(l => l.map(toPrettyString).mkString(" ")).mkString("\n")
  }

}
