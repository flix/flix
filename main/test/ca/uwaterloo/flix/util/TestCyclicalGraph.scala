package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestCyclicalGraph extends AnyFunSuite {

  test("Empty.01") {
    val graph = Map.empty[Int, List[Int]]
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(Set.empty[CyclicalGraph.Vertex[Int]])
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

  test("Invert.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = CyclicalGraph.invert(graph)
    val expected = Map(1 -> List(1), 2 -> List(1))
    assert(result == expected)
  }

  test("SCC.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](CyclicalGraph.Singleton(1, Set(1))))
    assert(result == expected)
  }

  test("SCC.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph))
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](
      CyclicalGraph.Singleton(1, Set(1, 2)),
      CyclicalGraph.Singleton(2, Set.empty),
    ))
    assert(result == expected)
  }

  test("SCC.03") {
    val graph = Map(1 -> List(2, 3, 4), 2 -> List.empty, 3 -> List(2), 4 -> List(1))
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph))
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(1, Set(2, 3, 4)),
        CyclicalGraph.Singleton(4, Set(1)))),
      CyclicalGraph.Singleton(2, Set.empty),
      CyclicalGraph.Singleton(3, Set(2))
    ))
    assert(result == expected)
  }

  test("SCC.04") {
    val graph = Map(1 -> List(2), 2 -> List(3, 5, 6), 3 -> List(4, 7), 4 -> List(3, 8), 5 -> List(1, 6), 6 -> List(7), 7 -> List(6, 8), 8 -> List(8))
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph))
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(1, Set(2)),
        CyclicalGraph.Singleton(2, Set(3, 5, 6)),
        CyclicalGraph.Singleton(5, Set(1, 6)),
      )),
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(3, Set(4, 7)),
        CyclicalGraph.Singleton(4, Set(3, 8)),
      )),
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(6, Set(7)),
        CyclicalGraph.Singleton(7, Set(6, 8)),
      )),
      CyclicalGraph.Singleton(8, Set(8))
    ))
    assert(result == expected)
  }

  test("SCC.OutGoing.01") {
    val graph = Map(1 -> List(2), 2 -> List(3, 5, 6), 3 -> List(4, 7), 4 -> List(3, 8), 5 -> List(1, 6), 6 -> List(7), 7 -> List(6, 8), 8 -> List(8))
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph)).vertices.map(_.outgoing).filter(_.nonEmpty)
    val expected = Set(
      Set(3, 6),
      Set(7, 8),
      Set(8),
    )
    assert(result == expected)
  }

  test("SCC.TopologicalSort.01") {
    val graph = Map(1 -> List(2), 2 -> List(3, 5, 6), 3 -> List(4, 7), 4 -> List(3, 8), 5 -> List(1, 6), 6 -> List(7), 7 -> List(6, 8), 8 -> List(8))
    val result = CyclicalGraph.topologicalSort(CyclicalGraph.scc(CyclicalGraph.from(graph)))
    val expected = List(
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(1, Set(2)),
        CyclicalGraph.Singleton(2, Set(3, 5, 6)),
        CyclicalGraph.Singleton(5, Set(1, 6)),
      )),
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(3, Set(4, 7)),
        CyclicalGraph.Singleton(4, Set(3, 8)),
      )),
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(6, Set(7)),
        CyclicalGraph.Singleton(7, Set(6, 8)),
      )),
      CyclicalGraph.Singleton(8, Set(8))
    )
    assert(result == expected)
  }
}
