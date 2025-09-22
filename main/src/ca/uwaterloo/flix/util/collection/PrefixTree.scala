/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

import scala.collection.mutable

/** An immutable map with string keys that is step-wise searchable by prefixes. */
object PrefixTree {

  /** Returns the root of a prefix tree of the mappings in `mappings`. */
  def mk[T](mappings: IterableOnce[(String, T)]): Node[T] =
    Node.mk(mappings)

  object Node {

    /** Returns the root of a prefix tree of the mappings in `mappings`. */
    def mk[T](mappings: IterableOnce[(String, T)]): Node[T] = {
      val node = new Node[T]()
      for ((k, v) <- mappings.iterator) {
        node.put(k, v)
      }
      node
    }

  }

  class Node[T] {

    // The node is implemented as a mutable tree but none of the mutation is exposed to the outside.

    /** Node value, if present. */
    private var value: Option[T] = None

    /** Node children. */
    private val children: mutable.HashMap[Char, Node[T]] = new mutable.HashMap()

    /** Returns the value of `this`. */
    def getValue: Option[T] =
      value

    /** Returns the `c` child if present. */
    def getNode(c: Char): Option[Node[T]] =
      children.get(c)

    /**
      * Returns the child with prefix `s` if present.
      *
      * If `s` is empty, `this` is returned.
      */
    def getNode(s: String): Option[Node[T]] = {
      var curr = this
      var i = 0
      while (i < s.length) {
        curr.getNode(s.charAt(i)) match {
          case Some(node) =>
            curr = node
            i = i + 1
          case None =>
            return None
        }
      }
      Some(curr)
    }

    /**
      * Returns the value of `s` if present.
      *
      * If `s` is empty, [[getValue]] of `this` is returned.
      */
    def get(s: String): Option[T] =
      getNode(s).flatMap(_.getValue)

    /** Assigns `v` as the value of `this`, overriding any existing value. */
    private def put(v: T): Unit =
      this.value = Some(v)

    /** Inserts `s` with value `v`, overriding any existing value. */
    private def put(s: String, v: T): Unit = {
      var curr = this
      var i = 0
      while (i < s.length) {
        curr = curr.getNodeOrCreateIfAbsent(s.charAt(i))
        i = i + 1
      }
      curr.put(v)
    }

    /** Returns child `c`, creating a new node if necessary. */
    private def getNodeOrCreateIfAbsent(c: Char): Node[T] = {
      children.get(c) match {
        case Some(node) => node
        case None =>
          val node = new Node[T]()
          children.put(c, node)
          node
      }
    }

  }

}

