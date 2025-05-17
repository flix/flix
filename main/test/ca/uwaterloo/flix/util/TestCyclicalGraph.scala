package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestCyclicalGraph extends AnyFunSuite {

  test("Empty.01") {
    val graph = Map.empty[Int, List[Int]]
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(List.empty)
    assert(result == expected)
  }

  test("ToMap.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.toMap(cyclicalGraph)
    val expected = graph
    assert(result == expected)
  }

  test("Invert.01") {
    val graph = Map(1 -> List(2, 3, 4), 2 -> List.empty, 3 -> List(2), 4 -> List(1))
    val result = CyclicalGraph.invert(graph)
    val expected = Map(1 -> List(4), 2 -> List(1, 3), 3 -> List(1), 4 -> List(1))
    assert(result == expected)
  }

  test("Singleton.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(List(CyclicalGraph.Singleton(1, Set(1))))
    assert(result == expected)
  }

  test("Singleton.02") {
    val graph = Map(1 -> List(1), 2 -> List.empty)
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph))
    val expected = CyclicalGraph(List(
      CyclicalGraph.Singleton(1, Set(1)),
      CyclicalGraph.Singleton(2, Set.empty),
    ))
    assert(result == expected)
  }

}
