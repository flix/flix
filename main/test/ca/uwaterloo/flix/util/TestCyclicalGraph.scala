package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestCyclicalGraph extends AnyFunSuite {

  private val nontrivialGraph1: Map[Int, List[Int]] = Map(
    1 -> List(2, 3, 4),
    2 -> List.empty,
    3 -> List(2),
    4 -> List(1)
  )

  private val nontrivialGraph2: Map[Int, List[Int]] = Map(
    1 -> List(2),
    2 -> List(3, 5, 6),
    3 -> List(4, 7),
    4 -> List(3, 8),
    5 -> List(1, 6),
    6 -> List(7),
    7 -> List(6, 8),
    8 -> List(8)
  )

  test("CyclicalGraph.Empty.01") {
    val graph = Map.empty[Int, List[Int]]
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(Set.empty[CyclicalGraph.Vertex[Int]])
    assert(result == expected)
  }

  test("CyclicalGraph.ToMap.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.toMap(cyclicalGraph)
    val expected = graph
    assert(result == expected)
  }

  test("CyclicalGraph.Invert.01") {
    val graph = nontrivialGraph1
    val result = CyclicalGraph.invert(graph)
    val expected = Map(1 -> List(4), 2 -> List(1, 3), 3 -> List(1), 4 -> List(1))
    assert(result == expected)
  }

  test("CyclicalGraph.Invert.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = CyclicalGraph.invert(graph)
    val expected = Map(1 -> List(1), 2 -> List(1))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = CyclicalGraph.from(graph)
    val result = CyclicalGraph.scc(cyclicalGraph)
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](CyclicalGraph.Singleton(1, Set(1))))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph))
    val expected = CyclicalGraph(Set[CyclicalGraph.Vertex[Int]](
      CyclicalGraph.Singleton(1, Set(1, 2)),
      CyclicalGraph.Singleton(2, Set.empty),
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.03") {
    val graph = nontrivialGraph1
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

  test("CyclicalGraph.SCC.04") {
    val graph = nontrivialGraph2
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

  test("CyclicalGraph.OutGoing.01") {
    val graph = nontrivialGraph1
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph)).vertices.map(_.outgoing).filter(_.nonEmpty)
    val expected = Set(
      Set(2, 3),
      Set(2)
    )
    assert(result == expected)
  }

  test("CyclicalGraph.OutGoing.02") {
    val graph = nontrivialGraph2
    val result = CyclicalGraph.scc(CyclicalGraph.from(graph)).vertices.map(_.outgoing).filter(_.nonEmpty)
    val expected = Set(
      Set(3, 6),
      Set(7, 8),
      Set(8),
    )
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.01") {
    val graph = nontrivialGraph1
    val result = CyclicalGraph.topologicalSort(CyclicalGraph.scc(CyclicalGraph.from(graph)))
    val expected = List(
      CyclicalGraph.SCC(Set(
        CyclicalGraph.Singleton(1, Set(2, 3, 4)),
        CyclicalGraph.Singleton(4, Set(1)))),
      CyclicalGraph.Singleton(3, Set(2)),
      CyclicalGraph.Singleton(2, Set.empty)
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.02") {
    val graph = nontrivialGraph2
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
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.03") {
    val graph = CyclicalGraph.from(
      Map(
        "A" -> List("B", "C"),
        "B" -> List("D"),
        "C" -> List("B", "D"),
        "D" -> List.empty
      )
    )
    val result = CyclicalGraph.topologicalSort(graph)
    val expected = List(
      CyclicalGraph.Singleton("A", Set("B", "C")),
      CyclicalGraph.Singleton("C", Set("B", "D")),
      CyclicalGraph.Singleton("B", Set("D")),
      CyclicalGraph.Singleton("D", Set()),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.04") {
    val graph = CyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = CyclicalGraph.topologicalSort(graph)
    val expected = List(
      CyclicalGraph.Singleton("A", Set("B", "C", "D")),
      CyclicalGraph.Singleton("B", Set("C", "E")),
      CyclicalGraph.Singleton("D", Set("E")),
      CyclicalGraph.Singleton("C", Set("E")),
      CyclicalGraph.Singleton("E", Set()),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.01") {
    val graph = nontrivialGraph1
    val result = CyclicalGraph.layers(CyclicalGraph.topologicalSort(CyclicalGraph.scc(CyclicalGraph.from(graph))))
    val expected = List(
      List(
        CyclicalGraph.SCC(Set(
          CyclicalGraph.Singleton(1, Set(2, 3, 4)),
          CyclicalGraph.Singleton(4, Set(1))))),
      List(
        CyclicalGraph.Singleton(3, Set(2))),
      List(
        CyclicalGraph.Singleton(2, Set.empty))
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.02") {
    val graph = nontrivialGraph2
    val result = CyclicalGraph.layers(CyclicalGraph.topologicalSort(CyclicalGraph.scc(CyclicalGraph.from(graph))))
    val expected = List(
      List(
        CyclicalGraph.SCC(Set(
          CyclicalGraph.Singleton(1, Set(2)),
          CyclicalGraph.Singleton(2, Set(3, 5, 6)),
          CyclicalGraph.Singleton(5, Set(1, 6)),
        ))),
      List(
        CyclicalGraph.SCC(Set(
          CyclicalGraph.Singleton(3, Set(4, 7)),
          CyclicalGraph.Singleton(4, Set(3, 8)),
        ))),
      List(
        CyclicalGraph.SCC(Set(
          CyclicalGraph.Singleton(6, Set(7)),
          CyclicalGraph.Singleton(7, Set(6, 8)),
        ))),
      List(CyclicalGraph.Singleton(8, Set(8)))
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.03") {
    val graph = CyclicalGraph.from(
      Map(
        "A" -> List("B", "C"),
        "B" -> List("D"),
        "C" -> List("B", "D"),
        "D" -> List.empty
      )
    )
    val result = CyclicalGraph.layers(CyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        CyclicalGraph.Singleton("A", Set("B", "C"))),
      List(
        CyclicalGraph.Singleton("C", Set("B", "D"))),
      List(
        CyclicalGraph.Singleton("B", Set("D"))),
      List(
        CyclicalGraph.Singleton("D", Set())),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.04") {
    val graph = CyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = CyclicalGraph.layers(CyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        CyclicalGraph.Singleton("A", Set("B", "C", "D"))),
      List(
        CyclicalGraph.Singleton("B", Set("C", "E"))),
      List(
        CyclicalGraph.Singleton("C", Set("E")),
        CyclicalGraph.Singleton("D", Set("E"))),
      List(
        CyclicalGraph.Singleton("E", Set())),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.05") {
    val graph = CyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = CyclicalGraph.layers(CyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        CyclicalGraph.Singleton("A", Set("B", "C", "D"))),
      List(
        CyclicalGraph.Singleton("B", Set("C", "E"))),
      List(
        CyclicalGraph.Singleton("C", Set("E")),
        CyclicalGraph.Singleton("D", Set("E"))),
      List(
        CyclicalGraph.Singleton("E", Set())),
    ).reverse
    assert(result == expected)
  }
}
