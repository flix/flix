/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.datastore

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import ca.uwaterloo.flix.util.misc.ThreadSafe

@ThreadSafe
class KeyCache {

  // TODO: Relies on Object.equals which is not necessarily correct! Must use Value.equal.

  /**
    * An atomic integer holding the next available integer.
    */
  private val counter = new AtomicInteger(1)

  /**
    * A concurrent map from integers to values.
    */
  private val i2v = new ConcurrentHashMap[Int, AnyRef]()

  /**
    * A concurrent map from values to integers.
    */
  private val v2i = new ConcurrentHashMap[AnyRef, Int]()

  /**
    * Returns the value that the integer `i` is mapped to.
    */
  @ThreadSafe
  def get(i: Int): AnyRef = {
    i2v.get(i)
  }

  /**
    * Maps the value `v` to a fresh integer in the cache and returns it.
    */
  @ThreadSafe
  def put(v: AnyRef): Int = {
    v2i.computeIfAbsent(v, new java.util.function.Function[AnyRef, Int] {
      def apply(k: scala.AnyRef): Int = {
        val i = counter.getAndIncrement()
        i2v.put(i, k)
        i
      }
    })
  }

}
