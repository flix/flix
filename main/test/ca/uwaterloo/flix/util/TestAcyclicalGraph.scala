package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestAcyclicalGraph extends AnyFunSuite {

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
    val cyclicalGraph = AcyclicalGraph.from(graph)
    val result = AcyclicalGraph.scc(cyclicalGraph)
    val expected = AcyclicalGraph(Set.empty[AcyclicalGraph.Vertex[Int]])
    assert(result == expected)
  }

  test("CyclicalGraph.ToMap.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = AcyclicalGraph.from(graph)
    val result = AcyclicalGraph.toMap(cyclicalGraph)
    val expected = graph
    assert(result == expected)
  }

  test("CyclicalGraph.Invert.01") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.invert(graph)
    val expected = Map(1 -> List(4), 2 -> List(1, 3), 3 -> List(1), 4 -> List(1))
    assert(result == expected)
  }

  test("CyclicalGraph.Invert.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = AcyclicalGraph.invert(graph)
    val expected = Map(1 -> List(1), 2 -> List(1))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.01") {
    val graph = Map(1 -> List(1))
    val cyclicalGraph = AcyclicalGraph.from(graph)
    val result = AcyclicalGraph.scc(cyclicalGraph)
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[Int]](AcyclicalGraph.Singleton(1, Set(1))))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph))
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[Int]](
      AcyclicalGraph.Singleton(1, Set(1, 2)),
      AcyclicalGraph.Singleton(2, Set.empty),
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.03") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph))
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[Int]](
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(1, Set(2, 3, 4)),
        AcyclicalGraph.Singleton(4, Set(1)))),
      AcyclicalGraph.Singleton(2, Set.empty),
      AcyclicalGraph.Singleton(3, Set(2))
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.04") {
    val graph = nontrivialGraph2
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph))
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[Int]](
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(1, Set(2)),
        AcyclicalGraph.Singleton(2, Set(3, 5, 6)),
        AcyclicalGraph.Singleton(5, Set(1, 6)),
      )),
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(3, Set(4, 7)),
        AcyclicalGraph.Singleton(4, Set(3, 8)),
      )),
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(6, Set(7)),
        AcyclicalGraph.Singleton(7, Set(6, 8)),
      )),
      AcyclicalGraph.Singleton(8, Set(8))
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.05") {
    val graph = Map(
      1 -> List.empty,
      2 -> List.empty,
      3 -> List.empty,
      4 -> List.empty,
    )
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph))
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[Int]](
      AcyclicalGraph.Singleton(1, Set.empty),
      AcyclicalGraph.Singleton(2, Set.empty),
      AcyclicalGraph.Singleton(3, Set.empty),
      AcyclicalGraph.Singleton(4, Set.empty),
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.06") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph(Set[AcyclicalGraph.Vertex[String]](
      AcyclicalGraph.Singleton("A", Set("B", "C", "D")),
      AcyclicalGraph.Singleton("B", Set("C", "E")),
      AcyclicalGraph.Singleton("C", Set("E")),
      AcyclicalGraph.Singleton("D", Set("E")),
      AcyclicalGraph.Singleton("E", Set.empty),
    ))
    assert(result == expected)
  }

  test("CyclicalGraph.OutGoing.01") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph)).vertices.map(_.outgoing).filter(_.nonEmpty)
    val expected = Set(
      Set(2, 3),
      Set(2)
    )
    assert(result == expected)
  }

  test("CyclicalGraph.OutGoing.02") {
    val graph = nontrivialGraph2
    val result = AcyclicalGraph.scc(AcyclicalGraph.from(graph)).vertices.map(_.outgoing).filter(_.nonEmpty)
    val expected = Set(
      Set(3, 6),
      Set(7, 8),
      Set(8),
    )
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.01") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(AcyclicalGraph.from(graph)))
    val expected = List(
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(1, Set(2, 3, 4)),
        AcyclicalGraph.Singleton(4, Set(1)))),
      AcyclicalGraph.Singleton(3, Set(2)),
      AcyclicalGraph.Singleton(2, Set.empty)
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.02") {
    val graph = nontrivialGraph2
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(AcyclicalGraph.from(graph)))
    val expected = List(
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(1, Set(2)),
        AcyclicalGraph.Singleton(2, Set(3, 5, 6)),
        AcyclicalGraph.Singleton(5, Set(1, 6)),
      )),
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(3, Set(4, 7)),
        AcyclicalGraph.Singleton(4, Set(3, 8)),
      )),
      AcyclicalGraph.SCC(Set(
        AcyclicalGraph.Singleton(6, Set(7)),
        AcyclicalGraph.Singleton(7, Set(6, 8)),
      )),
      AcyclicalGraph.Singleton(8, Set(8))
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.03") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C"),
        "B" -> List("D"),
        "C" -> List("B", "D"),
        "D" -> List.empty
      )
    )
    val result = AcyclicalGraph.topologicalSort(graph)
    val expected = List(
      AcyclicalGraph.Singleton("A", Set("B", "C")),
      AcyclicalGraph.Singleton("C", Set("B", "D")),
      AcyclicalGraph.Singleton("B", Set("D")),
      AcyclicalGraph.Singleton("D", Set()),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.04") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.topologicalSort(graph)
    val expected = List(
      AcyclicalGraph.Singleton("A", Set("B", "C", "D")),
      AcyclicalGraph.Singleton("B", Set("C", "E")),
      AcyclicalGraph.Singleton("D", Set("E")),
      AcyclicalGraph.Singleton("C", Set("E")),
      AcyclicalGraph.Singleton("E", Set()),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.01") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(AcyclicalGraph.from(graph))))
    val expected = List(
      List(
        AcyclicalGraph.SCC(Set(
          AcyclicalGraph.Singleton(1, Set(2, 3, 4)),
          AcyclicalGraph.Singleton(4, Set(1))))),
      List(
        AcyclicalGraph.Singleton(3, Set(2))),
      List(
        AcyclicalGraph.Singleton(2, Set.empty))
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.02") {
    val graph = nontrivialGraph2
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(AcyclicalGraph.from(graph))))
    val expected = List(
      List(
        AcyclicalGraph.SCC(Set(
          AcyclicalGraph.Singleton(1, Set(2)),
          AcyclicalGraph.Singleton(2, Set(3, 5, 6)),
          AcyclicalGraph.Singleton(5, Set(1, 6)),
        ))),
      List(
        AcyclicalGraph.SCC(Set(
          AcyclicalGraph.Singleton(3, Set(4, 7)),
          AcyclicalGraph.Singleton(4, Set(3, 8)),
        ))),
      List(
        AcyclicalGraph.SCC(Set(
          AcyclicalGraph.Singleton(6, Set(7)),
          AcyclicalGraph.Singleton(7, Set(6, 8)),
        ))),
      List(AcyclicalGraph.Singleton(8, Set(8)))
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.03") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C"),
        "B" -> List("D"),
        "C" -> List("B", "D"),
        "D" -> List.empty
      )
    )
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        AcyclicalGraph.Singleton("A", Set("B", "C"))),
      List(
        AcyclicalGraph.Singleton("C", Set("B", "D"))),
      List(
        AcyclicalGraph.Singleton("B", Set("D"))),
      List(
        AcyclicalGraph.Singleton("D", Set())),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.04") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        AcyclicalGraph.Singleton("A", Set("B", "C", "D"))),
      List(
        AcyclicalGraph.Singleton("B", Set("C", "E"))),
      List(
        AcyclicalGraph.Singleton("C", Set("E")),
        AcyclicalGraph.Singleton("D", Set("E"))),
      List(
        AcyclicalGraph.Singleton("E", Set())),
    ).reverse
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.05") {
    val graph = AcyclicalGraph.from(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph))
    val expected = List(
      List(
        AcyclicalGraph.Singleton("A", Set("B", "C", "D"))),
      List(
        AcyclicalGraph.Singleton("B", Set("C", "E"))),
      List(
        AcyclicalGraph.Singleton("C", Set("E")),
        AcyclicalGraph.Singleton("D", Set("E"))),
      List(
        AcyclicalGraph.Singleton("E", Set())),
    ).reverse
    assert(result == expected)
  }
}
