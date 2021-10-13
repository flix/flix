package ca.uwaterloo.flix.util

import scala.collection.mutable

object Graph {

  /**
    * A result of a topological sort.
    */
  sealed trait TopSortResult[N]
  object TopSortResult {

    /**
      * A cycle found during topological sorting.
      *
      * Adjacency is from left to right:
      * `[n1, n2, n3]` indicates a cycle `n1 -> n2 -> n3 -> n1`
      */
    case class Cycle[N](path: List[N]) extends TopSortResult[N]

    /**
      * A topologically sorted result.
      *
      * Fewest neighbors on the left.
      * The node at the head of the list has no neighbors.
      */
    case class Sorted[N](sorted: List[N]) extends TopSortResult[N]
  }

  /**
    * Topologically sort the nodes, using the `getAdj` function to find adjacent nodes.
    */
  def topSort[N](nodes: Iterable[N], getAdj: (N => List[N])): TopSortResult[N] = {
    val sorted = mutable.LinkedHashSet.empty[N]


    /**
      * Optionally returns a list of nodes forming a cycle.
      */
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
        val result = getAdj(node).flatMap {
          adj => visit(adj, node :: path)
        }.headOption

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
      case Some(cycle) => TopSortResult.Cycle(cycle)
    }.getOrElse(TopSortResult.Sorted(sorted.toList))
  }

}
