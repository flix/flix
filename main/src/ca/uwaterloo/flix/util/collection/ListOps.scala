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

/**
  * Operations on lists.
  */
object ListOps {

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
