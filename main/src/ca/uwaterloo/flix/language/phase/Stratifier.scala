/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The stratification phase computes the strongly-connected components (SCCs)
  * of the constraints and computes a topological sort of the SCCs.
  *
  * Reports a [[StratificationError]] if the constraint graph contains negative cycles.
  */
object Stratifier extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {

    val startTime = System.nanoTime()

    // We start off with only a single set of constraints
    val constraints = root.strata.head.constraints
    val graph = createGraph(constraints)

    // Not sure if need to do from just head of program?
    val negativeCycles = hasNegativeCycles(graph)
    val duration = System.nanoTime() - startTime


    if (negativeCycles) {
      StratificationError(constraints).toFailure
    } else {
      root.copy(time = root.time.copy(stratifier = duration)).toSuccess
    }
  }

  /**
    * A graph is an adjacency list of Constraint -> (Constraint, Weight)
    * where weight is either -1 or 0
    */
  class Graph {
    var head: Symbol.TableSym = _
    var vertices = Set.empty[Symbol.TableSym]

    // An adjacency list of from - to edges with weights
    var edges = Map.empty[Symbol.TableSym, Map[Symbol.TableSym, Int]]

    /**
      * Insert and edge from-to with weight into the graph
      */
    def insert(from: Symbol.TableSym, to: Symbol.TableSym, weight: Int): Stratifier.Graph = {
      edges = edges.get(from) match {
        case Some(m) => edges + (from -> (m + (to -> weight)))
        case None => edges + (from -> Map(to -> weight))
      }
      vertices += from
      vertices += to
      this
    }

    /**
      * Get the list of edges that a vertex has edges to
      */
    def lookup(from: Symbol.TableSym): Option[Map[Symbol.TableSym, Int]] = {
      edges.get(from)
    }
  }

  /**
    * Use the contraints to create an adjacency matrix
    *
    * A constraint of:
    *   P :- Q creates an edge Q -> P of weight 0
    *   P :- !Q creates an edge Q -> P of weight -1
    */
  def createGraph(constraints: List[TypedAst.Constraint]): Graph = {
    constraints.foldRight(new Graph)((constraint: TypedAst.Constraint, graph: Graph) => constraint.head match {
      case TypedAst.Predicate.Head.Positive(headSym, _, _) =>
        if (graph.head == null ) {
          graph.head = headSym
        }
        constraint.body.foldRight(graph)((pred: TypedAst.Predicate.Body, graph: Graph) => pred match {
          case TypedAst.Predicate.Body.Negative(predSym, _, _) =>
            graph.insert(headSym, predSym, -1)
          case TypedAst.Predicate.Body.Positive(predSym, _, _) =>
            graph.insert(headSym, predSym, 0)
          case _:TypedAst.Predicate.Body.Filter => graph
          case _:TypedAst.Predicate.Body.Loop => graph
        })
      case TypedAst.Predicate.Head.Negative(headSym, _, _) => ??? // Can't have negative heads
      case TypedAst.Predicate.Head.True(_) => graph
      case TypedAst.Predicate.Head.False(_) => graph
    })
  }

  /**
    * Returns true iff a graph has a negative cycle
    */
  def hasNegativeCycles(g: Graph): Boolean = {
    // The Bellman Ford algorithm, we can use it to find negative cycles

    // Initialize the distance and predecessor arrays to default values
    var distPred: Map[Symbol.TableSym, (Int, Symbol.TableSym)] = g.vertices.toList.zip(List.fill(g.vertices.size)((1, null))).toMap
    // The source has 0 weight
    distPred = distPred.updated(g.head, (0, null))

    // Repeatedly relax the edges
    g.vertices.foreach(_ => {
      g.edges.foreach((fromEntry: (Symbol.TableSym, Map[Symbol.TableSym, Int])) => {
        fromEntry._2.foreach(toEntry => {
          val from = fromEntry._1
          val to = toEntry._1
          val weight = toEntry._2
          // Relax the edge from-to
          if (distPred.get(from).get._1 + weight < distPred.get(to).get._1) {
            distPred = distPred.updated(to, (distPred.get(from).get._1 + weight, from))
          }
        })
      })
    })

    // At this point, we must have found the shortest path, so if there is a
    // shorter path, then there is a negative cycle
    g.edges.foreach((fromEntry: (Symbol.TableSym, Map[Symbol.TableSym, Int])) => {
      fromEntry._2.foreach(toEntry => {
        val from = fromEntry._1
        val to = toEntry._1
        val weight = toEntry._2
        if (distPred.get(from).get._1 + weight < distPred.get(to).get._1) {
          return true
        }
      })
    })
    false
  }

  def stronglyConnectedComponents() = ???


}
