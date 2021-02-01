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

import ca.uwaterloo.flix.language.ast.TypedAst.ChoicePattern
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec

object ChoiceMatch {

  /**
    * Returns `true` if the `pat1` is less than or equal to `pat2` according to the partial order on choice patterns.
    *
    * The partial order states that one pattern is smaller than another if it is more specific (i.e. less liberal).
    *
    * Thus:
    *
    * A <= A    P <= P    x <= W for any x (where W is a wildcard).
    */
  def leq(pat1: ChoicePattern, pat2: ChoicePattern): Boolean = (pat1, pat2) match {
    // Reflexive.
    case (ChoicePattern.Wild(_), ChoicePattern.Wild(_)) => true
    case (ChoicePattern.Absent(_), ChoicePattern.Absent(_)) => true
    case (ChoicePattern.Present(_, _, _), ChoicePattern.Present(_, _, _)) => true
    case (_, ChoicePattern.Wild(_)) => true
    case _ => false
  }

  /**
    * Returns `true` if the list of choice patterns `l1` is less than or equal to `l2`.
    *
    * The partial order on lists of choice patterns states that one list is smaller than
    * or equal to another list if every element of the first list is pair-wise smaller
    * than or equal to the corresponding element of the second list.
    *
    * Note: The lists must have the same length.
    */
  @tailrec
  def leq(l1: List[ChoicePattern], l2: List[ChoicePattern]): Boolean = (l1, l2) match {
    case (Nil, Nil) => true
    case (x :: xs, y :: ys) => leq(x, y) && leq(xs, ys)
    case (xs, ys) => throw InternalCompilerException(s"Mismatched lists: '$xs' and '$ys'.")
  }


  //
  //  // remove redundant patterns
  //  def antichain(patterns:List[List[Int]]):List[List[Int]] = aux(Nil,patterns)
  //
  //  def aux(acc:List[List[Int]],rest:List[List[Int]]):List[List[Int]] =
  //    let subsumed = (p,ps) -> List.exists(x->le_pattern(p,x),ps);
  //  match rest {
  //    case Nil   => List.reverse(acc)
  //    case p::ps => if (subsumed(p,acc) or subsumed(p,ps)) aux(acc,ps) else aux(p::acc,ps)
  //  }
  //
  //  // add a generalized pattern
  //  def generalize(p1:List[Int],p2:List[Int]):Option[List[Int]] = before(Nil,p1,p2)
  //
  //  def before(acc:List[Int],p1:List[Int],p2:List[Int]):Option[List[Int]] =
  //  match (p1,p2) {
  //    case (Nil,Nil) => None
  //    case ((a1::p1s),(a2::p2s)) =>
  //      if      (le(a1,a2)) before(a1::acc,p1s,p2s)
  //      else if (le(a2,a1)) before(a2::acc,p1s,p2s)
  //      else                after(8::acc,p1s,p2s)
  //    case _ => None
  //  }
  //
  //  def after(acc:List[Int],p1:List[Int],p2:List[Int]):Option[List[Int]] =
  //  match (p1,p2) {
  //    case (Nil,Nil) => Some(List.reverse(acc))
  //    case ((a1::p1s),(a2::p2s)) =>
  //      if      (le(a1,a2)) after(a1::acc,p1s,p2s)
  //      else if (le(a2,a1)) after(a2::acc,p1s,p2s)
  //      else                None
  //    case _ => None
  //  }
  //
  //
  //  // add all generalized pairs of rules once
  //  def generalizeAll(p:List[List[Int]]):List[List[Int]] =
  //    optlistlist(List.map(my_uncurry(generalize),allpairs(p)))
  //
  //  def my_uncurry(f: (a,b) -> c) : ((a, b)) -> c = match (x, y) -> f(x,y)
  //
  //  def allpairs(x:List[a]):List[(a,a)] =
  //  match x {
  //    case Nil => Nil
  //    case a::xs => List.map(x->(a,x),xs) ::: allpairs(xs)
  //  }
  //
  //  def optlistlist(x:List[Option[a]]):List[a] =
  //  match x {
  //    case Nil => Nil
  //    case None::xs => optlistlist(xs)
  //    case Some(y)::xs => y::optlistlist(xs)
  //  }
  //
  //  // Normalize a list of patterns (main function)
  //  def normalize(p:List[List[Int]]):List[List[Int]] =
  //    let p1 = antichain(p ::: generalizeAll(p));
  //  if (List.toSet(p)==List.toSet(p1)) p1 else normalize(p1)


}
