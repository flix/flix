/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

object ZhegalkinCache {

  /**
    * A cache that represents the xor of the two given expressions.
    */
  private val cachedXor: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * Returns the xor of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupXor(e1: ZhegalkinExpr, e2: ZhegalkinExpr, xor: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    cachedXor.computeIfAbsent((e1, e2), _ => xor(e1, e2))
  }

  /**
    * Clears all caches.
    */
  def clearCaches(): Unit = {
    cachedXor.clear()
  }

}
