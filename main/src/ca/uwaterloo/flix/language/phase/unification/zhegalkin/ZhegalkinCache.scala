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
    * Controls what caches are enabled.
    */
  private val EnableUnionCache: Boolean = false
  private val EnableInterCache: Boolean = false // Experiments suggest: Not worth it.
  private val EnableXorCache: Boolean = true

  /**
    * A cache that represents the union of the two given Zhegalkin expressions.
    */
  private val cachedUnion: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * A cache that represents the intersection of the two given Zhegalkin expressions.
    */
  private val cachedInter: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * A cache that represents the exclusive-or of the two given Zhegalkin expressions.
    */
  private val cachedXor: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * Returns the union of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupOrComputeUnion(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkUnion: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableUnionCache) {
      return mkUnion(e1, e2)
    }
    cachedUnion.computeIfAbsent((e1, e2), _ => mkUnion(e1, e2))
  }

  /**
    * Returns the intersection of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupOrComputeInter(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkInter: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableInterCache) {
      return mkInter(e1, e2)
    }
    cachedInter.computeIfAbsent((e1, e2), _ => mkInter(e1, e2))
  }

  /**
    * Returns the exclusive-or of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupOrComputeXor(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkXor: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableXorCache) {
      return mkXor(e1, e2)
    }

    cachedXor.computeIfAbsent((e1, e2), _ => mkXor(e1, e2))
  }

  /**
    * Clears all caches.
    */
  def clearCaches(): Unit = {
    cachedUnion.clear()
    cachedInter.clear()
    cachedXor.clear()
  }

}
