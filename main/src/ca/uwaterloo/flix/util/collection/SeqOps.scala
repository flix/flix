/*
 * Copyright 2025 Matthew Lutze, Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
