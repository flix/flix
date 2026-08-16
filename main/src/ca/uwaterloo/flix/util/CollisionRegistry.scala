/*
 * Copyright 2026 Werner Stein
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

import ca.uwaterloo.flix.language.ast.SourceLocation

import java.util.concurrent.ConcurrentHashMap

/**
  * A thread-safe registry that claims each `key: K` for exactly one `value: V`.
  *
  * This is the mechanism behind every collision check on a content-addressed generated
  * symbol in this compiler: `key` is the generated symbol (or id) and `value` is whatever
  * identifies what minted it -- an originating symbol and type, an enclosing def and an
  * index, a source key string, and so on. Two claims for the same key are only a real
  * collision if they also disagree on the value: the same origin claiming its own key
  * twice (e.g. because a cache lookup ran more than once) is not an error.
  *
  * Claiming by the full value, not by the key alone, matters. A registry keyed only on a
  * bare hash would flag two semantically distinct origins as colliding whenever their ids
  * merely coincide, even when whatever a generated name is actually built from -- namespace
  * and text, say -- differs between them. That was a real bug once, in the id-only
  * registries this type replaces; it exists so every call site shares one correct
  * implementation instead of several hand-rolled ones.
  */
final class CollisionRegistry[K, V] {

  private val claimed: ConcurrentHashMap[K, V] = new ConcurrentHashMap()

  /**
    * Claims `key` for `value`.
    *
    * Throws an [[InternalCompilerException]], built by `describe`, if `key` is already
    * claimed by a different value. `describe` receives the previously-claimed value and
    * the newly-claimed one, in that order, and is only invoked when there is a genuine
    * collision to report. Claiming the same `(key, value)` pair more than once is not an
    * error.
    */
  def claim(key: K, value: V, loc: SourceLocation)(describe: (V, V) => String): Unit = {
    claimed.merge(key, value, (existing, incoming) =>
      if (existing == incoming) existing
      else throw InternalCompilerException(describe(existing, incoming), loc)
    )
  }

}
