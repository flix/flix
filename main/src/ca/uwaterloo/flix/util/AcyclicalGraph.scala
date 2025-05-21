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

import ca.uwaterloo.flix.util.Graph.TopologicalSort

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/**
  * Represents a graph that may contain strongly connected components.
  *
  * @param graph          the set of vertices and edges that form the original graph.
  * @param cycles         the set of cycles in [[graph]].
  * @param acyclicalGraph the original graph without cycles, i.e. {{{ graph -- cycles }}}
  */
case class AcyclicalGraph[T](graph: HashMap[T, List[T]], cycles: HashMap[T, HashSet[T]], acyclicalGraph: HashMap[T, List[T]])

object AcyclicalGraph {

  def empty[T]: AcyclicalGraph[T] = AcyclicalGraph(HashMap.empty, HashMap.empty, HashMap.empty)

  def invert[T](graph: Map[T, List[T]]): HashMap[T, List[T]] = {
    graph.foldLeft(HashMap.empty[T, List[T]]) {
      case (acc, (u, edges)) => edges.foldLeft(acc) {
        case (acc1, v) => acc1.get(v) match {
          case Some(es) => acc1 + (v -> (u :: es))
          case None => acc1 + (v -> List(u))
        }
      }
    }
  }

  def topologicalSort[T](graph: AcyclicalGraph[T]): List[T] = {
    val next = (v: T) => graph.acyclicalGraph(v)
    val vertices = graph.graph.keys
    Graph.topologicalSort(vertices, next) match {
      case TopologicalSort.Cycle(_) => ???
      case TopologicalSort.Sorted(sorted) => sorted
    }
  }

  def layers[T](sortedGraph: List[T], graph: AcyclicalGraph[T]): List[List[T]] = {
    if (sortedGraph.isEmpty) {
      return List.empty
    }

    val seen = mutable.HashMap.empty[T, Int]
    for (u <- sortedGraph) {
      val outgoingEdges = graph.acyclicalGraph(u)
      if (outgoingEdges.isEmpty) {
        // `u` is a leaf vertex
        seen.put(u, 0)
      } else {
        val max = seen.filter { case (v, _) => outgoingEdges.contains(v) }
          .map { case (_, layer) => layer }
          .max
        seen.put(u, max + 1)
      }
    }

    val bufSize = seen.values.max + 1
    val result = new mutable.ArrayBuffer[mutable.ArrayBuffer[T]](bufSize)
    for (_ <- 0 until bufSize) {
      result.addOne(mutable.ArrayBuffer.empty)
    }
    for ((u, layer) <- seen) {
      val buf = result(layer)
      buf.addOne(u)
    }
    result.map(_.toList).toList
  }

  def scc[T](graph: Map[T, List[T]]): AcyclicalGraph[T] = {
    if (graph.isEmpty) {
      return AcyclicalGraph(HashMap.empty, HashMap.empty, HashMap.empty)
    }

    val hashGraph = HashMap.from(graph)
    val inverted = invert(graph)
    if (inverted.isEmpty) {
      return AcyclicalGraph(hashGraph, HashMap.empty, hashGraph)
    }

    val visited: mutable.Set[T] = mutable.HashSet.empty
    val discoveryTimes: mutable.HashMap[T, Int] = mutable.HashMap.empty
    val finishingTimes: mutable.HashMap[T, Int] = mutable.HashMap.empty
    var time = 0

    def visit(u: T, g: HashMap[T, List[T]]): Unit = {
      time += 1
      discoveryTimes.put(u, time)
      visited.addOne(u)
      g.get(u).toList.flatten.foreach(v => if (!visited.contains(v)) visit(v, g))
      time += 1
      finishingTimes.put(u, time)
    }

    hashGraph.keys.foreach(u => if (!visited.contains(u)) visit(u, hashGraph))
    visited.clear()
    time = 0


    val sortedByFinishingTimes = inverted.keys.map(u => (u, discoveryTimes(u), finishingTimes(u)))
      .toArray.sortInPlaceBy { case (_, _, f) => f }
      .reverse

    discoveryTimes.clear()
    finishingTimes.clear()

    sortedByFinishingTimes.foreach { case (u, _, _) => if (!visited.contains(u)) visit(u, inverted) }

    val sortedByFinishingTimes2 = inverted.keys.map(u => (u, discoveryTimes(u), finishingTimes(u)))
      .toArray.sortInPlaceBy { case (_, _, f) => f }
      .reverse

    var cycles = HashMap.empty[T, HashSet[T]]
    val diff = hashGraph -- inverted.keys
    diff.keys.foreach { k =>
      cycles = cycles + (k -> HashSet(k))
    }
    var acyclicalGraph = HashMap.empty[T, List[T]]

    val timesStack = mutable.Stack.from(sortedByFinishingTimes2)
    var (u, d, f) = timesStack.pop()
    var cycle = List(u)
    while (timesStack.nonEmpty) {
      val (v, d1, f1) = timesStack.pop()
      if (d < d1 && f1 < f) {
        cycle = v :: cycle
      } else {
        // Strongly Connected Component
        val cycleAsSet = HashSet.from(cycle)
        val cmap = HashMap.from(cycle.map(c => c -> cycleAsSet))
        cycles = cycles ++ cmap
        cycle = List(v)
      }
      u = v
      d = d1
      f = f1
    }
    // Strongly Connected Component
    val cycleAsSet = HashSet.from(cycle)
    val cmap = HashMap.from(cycle.map(c => c -> cycleAsSet))
    cycles = cycles ++ cmap

    cycles.foreach {
      case (k, cycle1) =>
        val edgesInCycle = cycle1.flatMap(hashGraph.apply)
        val outgoing = edgesInCycle.diff(cycle1)
        val outgoingWithLambdaStep = outgoing ++ outgoing.flatMap(cycles.apply)
        acyclicalGraph = acyclicalGraph + (k -> outgoingWithLambdaStep.toList)
    }

    cycles.foreach {
      case (x, cycle) if cycle.size == 1 && cycle.contains(x) =>
        cycles = cycles - x
      case _ => ()
    }

    AcyclicalGraph(hashGraph, cycles, acyclicalGraph)
  }
}
