/*
 * Copyright 2025 Jakob Schneider Villumsen
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

import ca.uwaterloo.flix.util.CyclicalGraph.Vertex

/**
  *
  * @param vertices the list of vertices that form the graph.
  */
case class CyclicalGraph[T](vertices: List[Vertex[T]])

object CyclicalGraph {

  sealed trait Vertex[T] {

    def out: List[T]
  }

  /**
    * Represents a single vertex.
    *
    * @param out the list of outgoing edges.
    */
  case class Singleton[T](out: List[T]) extends Vertex[T]

  /**
    * Represents a strongly connected component.
    *
    * @param cycle the list of vertices that form the cycle.
    * @param out   the list of outgoing edges.
    */
  case class SCC[T](cycle: List[T], out: List[T]) extends Vertex[T]

}
