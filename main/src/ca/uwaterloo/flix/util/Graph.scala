package ca.uwaterloo.flix.util

import scala.collection.mutable

object Graph {

  /** A result of a topological sort. */
  sealed trait TopologicalSort[N]
  object TopologicalSort {

    /**
      * A cycle found during topological sorting.
      *
      * Adjacency is from left to right:
      * `[n1, n2, n3]` indicates a cycle `n1 -> n2 -> n3 -> n1`
      */
    case class Cycle[N](path: List[N]) extends TopologicalSort[N]

    /**
      * A topologically sorted result.
      *
      * Fewest incoming edges on the left.
      * The node at the head of the list has no incoming edges.
      */
    case class Sorted[N](sorted: List[N]) extends TopologicalSort[N]
  }

  /**
    * Topologically sort the nodes, using the `getAdj` function to find adjacent nodes.
    *
    * `N` must have a well-defined equality and hashcode.
    */
  def topologicalSort[N](nodes: Iterable[N], getAdj: (N => List[N])): TopologicalSort[N] = {
    val sorted = mutable.LinkedHashSet.empty[N]


    /** Optionally returns a list of nodes forming a cycle. */
    def visit(node: N, path: List[N]): Option[List[N]] = {
      if (sorted.contains(node)) {
        // Case 1: We've visited this node and it's acyclic.
        None
      } else if (path.contains(node)) {
        // Case 2: This node is in the path. We have a cycle.
        val cycle = node :: path.takeWhile(_ != node).reverse
        Some(cycle)
      } else {
        // Case 3: New node. Check all its adjacent nodes.
        val cycles = for {
          adj <- getAdj(node)
          cycle <- visit(adj, node :: path)
        } yield cycle
        val result = cycles.headOption

        if (result.isEmpty) {
          // Add this node to the list of sorted nodes if there is no cycle.
          sorted += node
        }
        result
      }
    }

    // Visit all the nodes, and report if there is a cycle.
    // Otherwise return the sorted list.
    nodes.map(visit(_, Nil)).collectFirst {
      case Some(cycle) => TopologicalSort.Cycle(cycle)
    }.getOrElse(TopologicalSort.Sorted(sorted.toList))
  }

}
