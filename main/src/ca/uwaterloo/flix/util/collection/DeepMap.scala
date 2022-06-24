/*
 * Copyright 2022 Matthew Lutze
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

// MATT docs everywhere
object DeepMap {
  def empty[K1, K2, V]: DeepMap[K1, K2, V] = DeepMap(Map.empty)
}

case class DeepMap[K1, K2, V](m: Map[K1, Map[K2, V]]) {
  def getShallow(k1: K1): Option[Map[K2, V]] = m.get(k1)

  def getDeep(k1: K1, k2: K2): Option[V] = getShallow(k1).flatMap(_.get(k2))

  def putDeep(k1: K1, k2: K2, v: V): DeepMap[K1, K2, V] = {
    val inner0 = getShallow(k1).getOrElse(Map.empty)
    val inner = inner0 + (k2 -> v)
    val outer = m + (k1 -> inner)
    DeepMap(outer)
  }

  def entries: Iterable[(K1, K2, V)] = {
    for {
      (k1, inner) <- m
      (k2, v) <- inner
    } yield (k1, k2, v)
  }
}
