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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.CyclicalGraph.Vertex

import scala.collection.mutable

/**
  * Represents a graph that may contain cycles.
  *
  * @param vertices the list of vertices that form the graph.
  */
case class CyclicalGraph[T](vertices: List[Vertex[T]])

object CyclicalGraph {

  sealed trait Vertex[T] {

    /**
      * Returns the list of outgoing edges.
      */
    def out: List[T]

  }

  /**
    * Represents a single vertex.
    *
    * @param out the list of outgoing edges.
    */
  case class Singleton[T](value: T, out: List[T]) extends Vertex[T]

  /**
    * Represents a strongly connected component.
    *
    * @param cycle the list of vertices that form the cycle.
    * @param out   the list of outgoing edges of the component.
    */
  case class SCC[T](cycle: List[Singleton[T]]) extends Vertex[T] {
    def out: List[T] = {
      cycle.flatMap(singleton => singleton.out.filterNot(cycle.contains))
    }
  }

  def from[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    val vertices = graph.map { case (k, vs) => Singleton(k, vs) }.toList
    CyclicalGraph(vertices)
  }

  def invert[T](graph: CyclicalGraph[T]): CyclicalGraph[T] = {
    from(invert(toMap(graph)))
  }

  def scc[T](graph: CyclicalGraph[T]): CyclicalGraph[T] = {
    scc(toMap(graph))
  }

  def toMap[T](graph: CyclicalGraph[T]): Map[T, List[T]] = {
    graph.vertices.flatMap {
      case Singleton(value, out) => Map(value -> out)
      case SCC(cycle) => cycle.map(v => v.value -> v.out)
    }.toMap
  }

  private def invert[T](graph: Map[T, List[T]]): Map[T, List[T]] = {
    val result: mutable.Map[T, List[T]] = mutable.Map.empty
    ???
  }

  private def scc[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    if (graph.isEmpty) {
      throw InternalCompilerException("unexpected empty graph", SourceLocation.Unknown)
    }

    val visited: mutable.ArrayBuffer[T] = mutable.ArrayBuffer.empty
    val discoveryTimes: mutable.Map[T, Int] = mutable.Map.empty
    val finishingTimes: mutable.Map[T, Int] = mutable.Map.empty
    var time = 0

    def visit(u: T, g: Map[T, List[T]]): Unit = {
      time += 1
      discoveryTimes.put(u, time)
      visited.addOne(u)
      g(u).foreach(v => if (!visited.contains(v)) visit(v, g))
      time += 1
      finishingTimes.put(u, time)
    }

    graph.keys.foreach(u => if (!visited.contains(u)) visit(u, graph))
    visited.clear()
    time = 0

    val inverted = invert(graph)
    val sortedByFinishingTimes = inverted.keys.map(u => (u, discoveryTimes(u), finishingTimes(u)))
      .toArray.sortInPlaceBy { case (_, _, f) => f }
      .reverse

    discoveryTimes.clear()
    finishingTimes.clear()

    sortedByFinishingTimes.foreach { case (u, _, _) => if (!visited.contains(u)) visit(u, inverted) }

    val timesStack = mutable.Stack.from(sortedByFinishingTimes)
    var result = List.empty[List[T]]
    var (u, d, f) = timesStack.pop()
    var cycle = List(u)
    while (timesStack.nonEmpty) {
      val (v, d1, f1) = timesStack.pop()
      if (d < d1 && f1 < f) {
        cycle = v :: cycle
      } else {
        result = cycle :: result
        cycle = List(v)
      }
      u = v
      d = d1
      f = f1
    }

    CyclicalGraph(result.map {
      case value :: Nil => Singleton(value, graph(value))
      case l => SCC(l.reverse.map(value => Singleton(value, graph(value))))
    })
  }
}
