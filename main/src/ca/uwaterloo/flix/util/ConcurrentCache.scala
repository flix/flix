/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

/**
  * A concurrent cache for values of type `V`.
  */
class ConcurrentCache[V] {

  private val cache: ConcurrentMap[V, V] = new ConcurrentHashMap[V, V]()

  /**
    * Returns the canonical representation of the given value `v`.
    *
    * May return `v` itself or some other value equal to `v`.
    *
    * The value `v` must correctly implement the `equals` and `hashCode` methods.
    */
  def getCanonicalValue[T <: V](v: T): T = {
    cache.computeIfAbsent(v, (_: V) => v).asInstanceOf[T]
  }

}
