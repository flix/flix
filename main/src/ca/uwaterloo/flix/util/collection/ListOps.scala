/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.util.collection

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec

/**
  * Operations on lists.
  */
object ListOps {

  /** An alternative to [[List.zip]] that crashed for different length lists. */
  def zip[T1, T2](list1: List[T1], list2: List[T2]): List[(T1, T2)] = {
    @tailrec
    def loop(l1: List[T1], l2: List[T2], acc: List[(T1, T2)]): List[(T1, T2)] = (l1, l2) match {
      case (x :: xs, y :: ys) => loop(xs, ys, (x, y) :: acc)
      case (Nil, Nil) => acc.reverse
      case _ => throw InternalCompilerException(s"Zipped lists of length ${list1.length} and ${list2.length}.", SourceLocation.Unknown)
    }

    loop(list1, list2, Nil)
  }

  /** An alternative to [[List.zip]] that crashed for different length lists. */
  def zipOption[T1, T2](list1: List[T1], list2: List[T2]): Option[List[(T1, T2)]] = {
    @tailrec
    def loop(l1: List[T1], l2: List[T2], acc: List[(T1, T2)]): Option[List[(T1, T2)]] = (l1, l2) match {
      case (x :: xs, y :: ys) => loop(xs, ys, (x, y) :: acc)
      case (Nil, Nil) => Some(acc.reverse)
      case _ => None
    }

    loop(list1, list2, Nil)
  }

  /**
    * Zips the given lists, truncating the longer one so that the lengths match.
    *
    * This is the same as [[List.zip]], but with a clearer name to express intent.
    */
  def zipTruncate[T1, T2](list1: List[T1], list2: List[T2]): List[(T1, T2)] = {
    list1.zip(list2)
  }


  /**
    * Unzips the given list of 4-tuples into 4 lists.
    */
  def unzip4[T1, T2, T3, T4](list: List[(T1, T2, T3, T4)]): (List[T1], List[T2], List[T3], List[T4]) = {
    list.foldRight((Nil: List[T1], Nil: List[T2], Nil: List[T3], Nil: List[T4])) {
      case ((h1, h2, h3, h4), (t1, t2, t3, t4)) => (h1 :: t1, h2 :: t2, h3 :: t3, h4 :: t4)
    }
  }

  /**
    * Lazily executes the function `f` on the elements of `list`.
    *
    * Returns the first `Some` value we get, or `None` if all results are `None`.
    */
  def findMap[A, B](list: List[A])(f: A => Option[B]): Option[B] = {
    list.iterator.map(f).collectFirst {
      case Some(value) => value
    }
  }
}
