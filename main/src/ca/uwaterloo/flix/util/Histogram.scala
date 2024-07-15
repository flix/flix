/*
 * Copyright 2022 Magnus Madsen
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

package ca.uwaterloo.flix.util

import scala.collection.mutable

/**
  * A small utility class used to compute histograms.
  */
class Histogram[T] {

  /**
    * An internal map that records the number of occurrences of its keys.
    */
  private val m: mutable.Map[T, Int] = mutable.Map.empty[T, Int]

  /**
    * Observe an occurrence. Thread-safe.
    */
  def observe(x: T): Unit = synchronized {
    m.put(x, m.getOrElse(x, 0) + 1)
  }

  override def toString: String = {
    val sb = new mutable.StringBuilder
    sb.append("Entry, Count").append(System.lineSeparator())
    for ((k, count) <- m.toList.sortBy(_._2)) {
      sb.append(s"$k, $count").append(System.lineSeparator())
    }
    sb.append(System.lineSeparator())
    val entries = m.size
    val counts = m.values.sum
    sb.append(s"Unique Entries: $entries, Total Counts: $counts.")
    sb.toString()
  }
}
