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

import ca.uwaterloo.flix.language.phase.unification.shared.BoolSubstitution

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

class ZhegalkinCache[T] {

  /**
    * Controls what caches are enabled.
    */
  var EnableInterCstCache: Boolean = false
  var EnableUnionCache: Boolean = false
  var EnableInterCache: Boolean = true
  var EnableXorCache: Boolean = false
  var EnableSVECache: Boolean = false

  /**
   * A cache that represents the intersection of the given Zhegalkin constant and expression.
   *
   * Note: initial size and load factor determined by profiling.
   */
  private val cachedInterCst: ConcurrentMap[(T, ZhegalkinExpr[T]), ZhegalkinExpr[T]] = new ConcurrentHashMap(1024, 0.5f)

  /**
    * A cache that represents the union of the two given Zhegalkin expressions.
    *
    * Note: initial size and load factor determined by profiling.
    */
  private val cachedUnion: ConcurrentMap[(ZhegalkinExpr[T], ZhegalkinExpr[T]), ZhegalkinExpr[T]] = new ConcurrentHashMap(1024, 0.5f)

  /**
    * A cache that represents the intersection of the two given Zhegalkin expressions.
    *
    * Note: initial size and load factor determined by profiling.
    */
  private val cachedInter: ConcurrentMap[(ZhegalkinExpr[T], ZhegalkinExpr[T]), ZhegalkinExpr[T]] = new ConcurrentHashMap(2048, 0.5f)

  /**
    * A cache that represents the exclusive-or of the two given Zhegalkin expressions.
    *
    * Note: initial size and load factor determined by profiling.
    */
  private val cachedXor: ConcurrentMap[(ZhegalkinExpr[T], ZhegalkinExpr[T]), ZhegalkinExpr[T]] = new ConcurrentHashMap(8192, 0.5f)

  /**
    * A cache of SVE queries: a map from the query to its MGU (if it exists).
    */
  private val cachedSVE: ConcurrentMap[ZhegalkinExpr[T], BoolSubstitution[ZhegalkinExpr[T]]] = new ConcurrentHashMap()

  /**
   * Returns the intersection of the given Zhegalkin constant `c` and the expression `e`.
   *
   * Performs a lookup in the cache or computes the result.
   */
  def lookupOrComputeInterCst(c: T, e: ZhegalkinExpr[T], mkInter: (T, ZhegalkinExpr[T]) => ZhegalkinExpr[T]): ZhegalkinExpr[T] = {
    if (!EnableInterCstCache) {
      return mkInter(c, e)
    }
    cachedInterCst.computeIfAbsent((c, e), _ => mkInter(c, e))
  }

  /**
    * Returns the union of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupOrComputeUnion(e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T], mkUnion: (ZhegalkinExpr[T], ZhegalkinExpr[T]) => ZhegalkinExpr[T]): ZhegalkinExpr[T] = {
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
  def lookupOrComputeInter(e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T], mkInter: (ZhegalkinExpr[T], ZhegalkinExpr[T]) => ZhegalkinExpr[T]): ZhegalkinExpr[T] = {
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
  def lookupOrComputeXor(e1: ZhegalkinExpr[T], e2: ZhegalkinExpr[T], mkXor: (ZhegalkinExpr[T], ZhegalkinExpr[T]) => ZhegalkinExpr[T]): ZhegalkinExpr[T] = {
    if (!EnableXorCache) {
      return mkXor(e1, e2)
    }

    cachedXor.computeIfAbsent((e1, e2), _ => mkXor(e1, e2))
  }

  /**
    * Returns the MGU of the given Zhegalkin query `q`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  def lookupOrComputeSVE(q: ZhegalkinExpr[T], sve: ZhegalkinExpr[T] => BoolSubstitution[ZhegalkinExpr[T]]): BoolSubstitution[ZhegalkinExpr[T]] = {
    if (!EnableSVECache) {
      return sve(q)
    }

    cachedSVE.computeIfAbsent(q, _ => sve(q))
  }

  /**
    * Clears all caches.
    */
  def clearCaches(): Unit = {
    cachedInterCst.clear()
    cachedUnion.clear()
    cachedInter.clear()
    cachedXor.clear()
    cachedSVE.clear()
  }

}
