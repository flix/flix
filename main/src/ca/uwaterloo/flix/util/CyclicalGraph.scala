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
      * Returns the list of outgoing edges of the component.
      */
    def outgoing: Set[T]

  }

  /**
    * Represents a single vertex.
    *
    * @param value the value / label of the vertex. Must be unique in the graph.
    * @param outgoing the list of outgoing edges.
    */
  case class Singleton[T](value: T, outgoing: Set[T]) extends Vertex[T]

  /**
    * Represents a strongly connected component.
    *
    * @param cycle the list of vertices that form the cycle.
    */
  case class SCC[T](cycle: Set[Singleton[T]]) extends Vertex[T] {
    def outgoing: Set[T] = {
      cycle.flatMap(singleton => singleton.outgoing -- cycle.map(_.value))
    }
  }

  def from[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    val vertices = graph.map { case (k, vs) => Singleton(k, vs.toSet) }.toList
    CyclicalGraph(vertices)
  }

  def scc[T](graph: CyclicalGraph[T]): CyclicalGraph[T] = {
    scc(toMap(graph))
  }

  def toMap[T](graph: CyclicalGraph[T]): Map[T, List[T]] = {
    graph.vertices.flatMap {
      case Singleton(value, edges) => Map(value -> edges.toList)
      case SCC(cycle) => cycle.map(v => v.value -> v.outgoing.toList)
    }.toMap
  }

  private def invert[T](graph: Map[T, List[T]]): Map[T, List[T]] = {
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

  private def scc[T](graph: Map[T, List[T]]): CyclicalGraph[T] = {
    if (graph.isEmpty) {
      return CyclicalGraph(List.empty)
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
      case value :: Nil => Singleton(value, graph(value).toSet)
      case l => SCC(l.reverse.map(value => Singleton(value, graph(value).toSet)).toSet)
    })
  }
}
