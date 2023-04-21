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
    val expected = Graph.TopologicalSort.Sorted(List(1, 2, 3))

    assert(result == expected)
  }
}
