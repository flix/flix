/*
 * Copyright 2025 Matthew Lutze, Jakob Schneider Villumsen
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

import scala.collection.mutable

object SeqOps {

  /**
    * Returns a list of pairs `(first, duplicate)` for each item that shares a key with an earlier item.
    * What constitutes a "duplicate" is abstracted into the `groupBy` argument.
    */
  def getDuplicates[A, K](items: Seq[A], groupBy: A => K): List[(A, A)] = {
    val seen = mutable.Map.empty[K, A]
    val duplicates = List.newBuilder[(A, A)]
    for (item <- items) {
      val key = groupBy(item)
      seen.get(key) match {
        case Some(first) => duplicates += ((first, item))
        case None => seen(key) = item
      }
    }
    duplicates.result()
  }

}
