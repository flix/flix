package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestGraph extends AnyFunSuite {

  test("topSort.Cycle.01") {
    val graph = Map(1 -> List(1))
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    val expected = Graph.TopologicalSort.Cycle(List(1))

    assert(result == expected)
  }

  test("topSort.Cycle.02") {
    val graph = Map(
      1 -> List(2),
      2 -> List(1)
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    val expected = Graph.TopologicalSort.Cycle(List(1, 2))

    assert(result == expected)
  }

  test("topSort.Cycle.03") {
    val graph = Map(
      1 -> List(2),
      2 -> List(3),
      3 -> List(1),
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    val expected = Graph.TopologicalSort.Cycle(List(1, 2, 3))

    assert(result == expected)
  }

  test("topSort.Cycle.04") {
    val graph = Map(
      1 -> List(2),
      2 -> List(2)
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    val expected = Graph.TopologicalSort.Cycle(List(2))

    assert(result == expected)
  }

  test("topSort.Sorted.01") {
    val graph = Map(
      1 -> List(),
      2 -> List(1)
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    // expected:
    //     1 <- 2
    val expected = Graph.TopologicalSort.Sorted(List(1, 2))

    assert(result == expected)
  }

  test("topSort.Sorted.02") {
    val graph = Map(
      1 -> List(),
      2 -> List(1),
      3 -> List(2)
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    // expected:
    //     1 <- 2 <- 3
    val expected = Graph.TopologicalSort.Sorted(List(1, 2, 3))

    assert(result == expected)
  }

  test("topSort.Sorted.03") {
    val graph = Map(
      1 -> List(),
      2 -> List(1),
      3 -> List(1)
    )
    val result = Graph.topologicalSort(graph.keys, graph.apply)
    // expected:
    //     1 <- 2
    //     1 <- 3
    val expected = Graph.TopologicalSort.Sorted(List(1, 2, 3))

    assert(result == expected)
  }

  /**
    * `stronglyConnectedComponents` only guarantees that the returned ids are consistent within
    * an SCC, not what the ids themselves are, so tests compare the partition of nodes into SCCs
    * rather than the raw `Map[N, Int]`.
    */
  private def sccGroups[N](result: Map[N, Int]): Set[Set[N]] =
    result.groupBy { case (_, id) => id }.values.map(_.keySet).toSet

  test("scc.Singleton.01") {
    val graph = Map(1 -> List())
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1)))
  }

  test("scc.Singleton.02") {
    // A self-loop is a trivial cycle, but still just one SCC of size one.
    val graph = Map(1 -> List(1))
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1)))
  }

  test("scc.Singleton.03") {
    val graph = Map(1 -> List(), 2 -> List(), 3 -> List())
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1), Set(2), Set(3)))
  }

  test("scc.Empty.01") {
    val graph = Map.empty[Int, List[Int]]
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(result == Map.empty)
  }

  test("scc.Cycle.01") {
    val graph = Map(
      1 -> List(2),
      2 -> List(1)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2)))
  }

  test("scc.Cycle.02") {
    val graph = Map(
      1 -> List(2),
      2 -> List(3),
      3 -> List(1)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2, 3)))
  }

  test("scc.Cycle.03") {
    val graph = Map(
      1 -> List(2),
      2 -> List(3),
      3 -> List(4),
      4 -> List(1)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2, 3, 4)))
  }

  test("scc.Cycle.04") {
    // A cycle with a tail: 3 is reachable from the cycle but the cycle isn't reachable from 3.
    val graph = Map(
      1 -> List(2),
      2 -> List(1, 3),
      3 -> List()
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2), Set(3)))
  }

  test("scc.Cycle.05") {
    // A node pointing into a cycle, without being part of it.
    val graph = Map(
      1 -> List(2),
      2 -> List(1),
      3 -> List(1)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2), Set(3)))
  }

  test("scc.DAG.01") {
    val graph = Map(
      1 -> List(),
      2 -> List(1),
      3 -> List(2)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1), Set(2), Set(3)))
  }

  test("scc.DAG.02") {
    // Diamond: 4 -> {2, 3} -> 1. No cycles, so every node is its own SCC.
    val graph = Map(
      1 -> List(),
      2 -> List(1),
      3 -> List(1),
      4 -> List(2, 3)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1), Set(2), Set(3), Set(4)))
  }

  test("scc.Disconnected.01") {
    // Two separate cycles with no edges between them.
    val graph = Map(
      1 -> List(2),
      2 -> List(1),
      3 -> List(4),
      4 -> List(3)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2), Set(3, 4)))
  }

  test("scc.Mixed.01") {
    // A chain of three cycles, joined by one-way bridge edges: {1,2,3} -> {4,5,6} -> {7,8}.
    val graph = Map(
      1 -> List(2),
      2 -> List(3),
      3 -> List(1, 4),
      4 -> List(5),
      5 -> List(6),
      6 -> List(4, 7),
      7 -> List(8),
      8 -> List(7)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2, 3), Set(4, 5, 6), Set(7, 8)))
  }

  test("scc.Mixed.02") {
    // Two 2-cycles joined by a bridge, plus an isolated node.
    val graph = Map(
      1 -> List(2),
      2 -> List(1, 3),
      3 -> List(4),
      4 -> List(3),
      5 -> List()
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2), Set(3, 4), Set(5)))
  }

  test("scc.Overlap.01") {
    // Two cycles sharing a bridge in both directions: 1<->2, 2->3->4->2.
    // Every node can reach every other node, so they all collapse into one SCC.
    val graph = Map(
      1 -> List(2),
      2 -> List(1, 3),
      3 -> List(4),
      4 -> List(2)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2, 3, 4)))
  }

  test("scc.Overlap.02") {
    // As scc.Overlap.01, but with an extra node hanging off the merged cycle that
    // doesn't loop back, and another node feeding into it that isn't reachable from it.
    val graph = Map(
      1 -> List(2),
      2 -> List(1, 3),
      3 -> List(4),
      4 -> List(2, 5),
      5 -> List(),
      6 -> List(1)
    )
    val result = Graph.stronglyConnectedComponents(graph.keys, graph.apply)

    assert(sccGroups(result) == Set(Set(1, 2, 3, 4), Set(5), Set(6)))
  }
}
