package ca.uwaterloo.flix.util

import scala.collection.mutable

object Graph {

  /**
    * A result of a topological sort.
    */
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
      * Fewest outgoing edges on the left.
      * The node at the head of the list has no outgoing edges.
      * The last element has no incoming edges.
      * In other words, the edges point from right to left:
      * `leaf <- n1 <- n2 <- n3`
      */
    case class Sorted[N](sorted: List[N]) extends TopologicalSort[N]
  }

  /**
    * Topologically sort the nodes, using the `getAdj` function to find adjacent nodes,
    * i.e., the outgoing edges of a node `n`.
    *
    * `N` must have a well-defined equality and hashcode.
    */
  def topologicalSort[N](nodes: Iterable[N], getAdj: (N => List[N])): TopologicalSort[N] = {
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

  /**
    * Get strongly-connected-components, using Tarjan's algorithm.
    *
    * Returns a map from each node to an arbitrary but consistent integer id, shared by every node in its SCC.
    *
    * The `getAdj` function returns the adjacent nodes, i.e, the outgoing edges of a node `n`.
    *
    * `N` must have a well-defined equality and hashcode.
    */
  def stronglyConnectedComponents[N](nodes: Iterable[N], getAdj: (N => List[N])): Map[N, Int] = {
    var nextIndex = 0
    val index    = mutable.Map.empty[N, Int]
    val lowlink  = mutable.Map.empty[N, Int]
    val onStack  = mutable.Set.empty[N]
    val stack    = mutable.ArrayDeque.empty[N]
    val sccId    = mutable.Map.empty[N, Int]
    var nextScc  = 0

    def strongConnect(v: N): Unit = {
      // Assign v the next unused index, and seed its lowlink (the smallest index
      // reachable from v) with its own index.
      index(v) = nextIndex
      lowlink(v) = nextIndex
      nextIndex += 1
      stack.append(v)
      onStack += v

      for (w <- getAdj(v)) {
        if (!index.contains(w)) {
          // Case 1: w is unvisited. Recurse, then pull v's lowlink down to w's.
          strongConnect(w)
          lowlink(v) = math.min(lowlink(v), lowlink(w))
        } else if (onStack(w)) {
          // Case 2: w is on the stack, so it's in the current SCC (a back edge).
          // Pull v's lowlink down to w's index.
          lowlink(v) = math.min(lowlink(v), index(w))
        }
        // Case 3 (implicit): w is visited but not on the stack, i.e. it belongs
        // to an already-completed SCC. Ignore it.
      }

      if (lowlink(v) == index(v)) {
        // v is the root of its SCC: pop the stack down to and including v,
        // assigning every popped node the same fresh SCC id.
        var w = stack.removeLast()
        onStack -= w
        sccId(w) = nextScc
        while (w != v) {
          w = stack.removeLast()
          onStack -= w
          sccId(w) = nextScc
        }
        nextScc += 1
      }
    }

    for (v <- nodes if !index.contains(v)) {
      strongConnect(v)
    }

    sccId.toMap
  }
}
