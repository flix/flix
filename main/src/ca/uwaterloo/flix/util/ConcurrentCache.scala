/*
 * Copyright 2025 Magnus Madsen
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

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

/**
  * A concurrent cache for values of type `V`.
  */
class ConcurrentCache[V] {

  private val Cache: ConcurrentMap[V, V] = new ConcurrentHashMap[V, V]()

  /**
    * Returns the canonical representation of the given value `v`.
    *
    * May return `v` itself or some other value equal to `v`.
    */
  def getOrPut(v: V): V = {
    Cache.computeIfAbsent(v, (_: V) => v)
  }

}
