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

object SeqOps {

  /**
    * Gets duplicate pairs from a list of items.
    * This is used to generate a list of pairs that can be mapped into Duplicate errors.
    * What constitutes a "duplicate" is abstracted into the groupBy argument.
    * But for enum variants, two variants are duplicates if they share names.
    */
  def getDuplicates[A, K](items: Seq[A], groupBy: A => K): List[(A, A)] = {
    val groups = items.groupBy(groupBy)
    for {
      (_, group) <- groups.toList
      // if a group has a nonempty tail, then everything in the tail is a duplicate of the head
      duplicate <- group.tail
    } yield (group.head, duplicate)
  }

}
