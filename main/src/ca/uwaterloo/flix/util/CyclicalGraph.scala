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
import ca.uwaterloo.flix.util.Graph.TopologicalSort

import scala.collection.mutable

/**
  * Represents a graph that may contain cycles.
  *
  * @param vertices the set of vertices and edges that form the graph.
  */
case class CyclicalGraph[T](vertices: Set[Vertex[T]])

object CyclicalGraph {

  sealed trait Vertex[T] {

    /**
      * Returns the list of outgoing edges of the component.
      */
    def outgoing: Set[T] = this match {
      case Singleton(value, edges) => edges - value
      case SCC(cycle) => cycle.flatMap(_.outgoing) -- cycle.map(_.value)
    }

    def equiv(v: T): Boolean = this match {
      case Singleton(u, _) => u == v
      case SCC(cycle) => cycle.exists(_.value == v)
    }

  }

  /**
    * Represents a single vertex.
    *
    * @param value the value / label of the vertex. Must be unique in the graph.
    * @param edges the list of edges (including edges to itself).
    */
  case class Singleton[T](value: T, edges: Set[T]) extends Vertex[T]

  /**
    * Represents a strongly connected component.
    *
    * @param cycle the list of vertices that form the cycle.
    */
  case class SCC[T](cycle: Set[Singleton[T]]) extends Vertex[T]

  def from[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    val vertices: Set[Vertex[T]] = graph.map { case (k, vs) => Singleton(k, vs.toSet) }.toSet
    CyclicalGraph(vertices)
  }

  def scc[T](graph: CyclicalGraph[T]): CyclicalGraph[T] = {
    scc(toMap(graph))
  }

  def toMap[T](graph: CyclicalGraph[T]): Map[T, List[T]] = {
    graph.vertices.flatMap {
      case Singleton(value, edges) => Map(value -> edges.toList)
      case SCC(cycle) => cycle.map(v => v.value -> v.edges.toList)
    }.toMap
  }

  def invert[T](graph: Map[T, List[T]]): Map[T, List[T]] = {
    val result: mutable.Map[T, List[T]] = mutable.Map.empty
    graph.foreach {
      case (u, edges) => edges.foreach {
        v =>
          result.get(v) match {
            case Some(es) =>
              result.put(v, u :: es)
            case None =>
              result.put(v, List(u))
          }
      }
    }
    result.map {
      case (u, edges) => u -> edges.reverse
    }.toMap
  }

  def topologicalSort[T](graph: CyclicalGraph[T]): List[Vertex[T]] = {
    val topSort = Graph.topologicalSort(graph.vertices, (v0: Vertex[T]) => v0.outgoing.flatMap(v => graph.vertices.filter {
      case Singleton(u, _) => v == u
      case SCC(cycle) => cycle.exists(_.value == v)
    }).toList)
    topSort match {
      case TopologicalSort.Cycle(_) => ???
      case TopologicalSort.Sorted(sorted) => sorted
    }
  }

  def computeLayers[T](sortedGraph: List[Vertex[T]]): List[List[Vertex[T]]] = {
    if (sortedGraph.isEmpty) {
      return List.empty
    }

    val seen = mutable.ArrayBuffer.empty[(Int, Vertex[T])]
    for (u <- sortedGraph) {
      val outgoing = u.outgoing
      if (outgoing.isEmpty) {
        // `u` is a leaf vertex
        seen.addOne((0, u))
      } else {
        val max = seen.filter { case (_, v1) => outgoing.exists(v2 => v1.equiv(v2)) }
          .map { case (layer, _) => layer }
          .max
        seen.addOne((max + 1, u))
      }
    }

    val bufSize = seen.map { case (layer, _) => layer }.max + 1
    val result = new mutable.ArrayBuffer[mutable.ArrayBuffer[Vertex[T]]](bufSize)
    for (_ <- 0 until bufSize) {
      result.addOne(mutable.ArrayBuffer.empty)
    }
    for ((layer, u) <- seen) {
      val buf = result(layer)
      buf.addOne(u)
    }
    result.map(_.toList).toList
  }

  private def scc[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    if (graph.isEmpty) {
      return CyclicalGraph(Set.empty)
    }

    val visited: mutable.Set[T] = mutable.Set.empty
    val discoveryTimes: mutable.Map[T, Int] = mutable.Map.empty
    val finishingTimes: mutable.Map[T, Int] = mutable.Map.empty
    var time = 0

    def visit(u: T, g: Map[T, List[T]]): Unit = {
      time += 1
      discoveryTimes.put(u, time)
      visited.addOne(u)
      g.get(u).toList.flatten.foreach(v => if (!visited.contains(v)) visit(v, g))
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

    val sortedByFinishingTimes2 = inverted.keys.map(u => (u, discoveryTimes(u), finishingTimes(u)))
      .toArray.sortInPlaceBy { case (_, _, f) => f }
      .reverse

    val timesStack = mutable.Stack.from(sortedByFinishingTimes2)
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
    result = cycle :: result

    CyclicalGraph(result.map {
      case value :: Nil => Singleton(value, graph(value).toSet)
      case l => SCC(l.reverse.map(value => Singleton(value, graph(value).toSet)).toSet)
    }.toSet)
  }
}
