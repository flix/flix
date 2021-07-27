/*
 * Copyright 2021 Matthew Lutze
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

object ListOps {

  /**
    * Unzips the list of 4-tuples into 4 lists
    */
  def unzip4[A, B, C, D](list: List[(A, B, C, D)]): (List[A], List[B], List[C], List[D]) = list match {
    case Nil => (Nil, Nil, Nil, Nil)
    case (a, b, c, d) :: tail =>
      val (listA, listB, listC, listD) = unzip4(tail)
      (a :: listA, b :: listB, c :: listC, d :: listD)
  }
}
