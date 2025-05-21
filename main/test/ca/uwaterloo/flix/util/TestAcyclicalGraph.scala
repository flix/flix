package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.{HashMap, HashSet}

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
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph.empty
    assert(result == expected)
  }

  test("CyclicalGraph.Invert.01") {
    val result = AcyclicalGraph.invert(nontrivialGraph1)
    val expected = HashMap(1 -> List(4), 2 -> List(3, 1), 3 -> List(1), 4 -> List(1))
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
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph(HashMap(1 -> List(1)), HashMap.empty[Int, HashSet[Int]], HashMap(1 -> List.empty))
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.02") {
    val graph = Map(1 -> List(1, 2), 2 -> List.empty)
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph(
      HashMap(1 -> List(1, 2), 2 -> List.empty),
      HashMap.empty[Int, HashSet[Int]],
      HashMap(1 -> List(2), 2 -> List.empty)
    )
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.03") {
    val result = AcyclicalGraph.scc(nontrivialGraph1)
    val expected = AcyclicalGraph(
      HashMap.from(nontrivialGraph1),
      HashMap(
        1 -> HashSet(4, 1),
        4 -> HashSet(4, 1)
      ),
      HashMap(
        1 -> List(2, 3),
        2 -> List.empty,
        3 -> List(2),
        4 -> List(2, 3)
      )
    )
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.04") {
    val result = AcyclicalGraph.scc(nontrivialGraph2)
    val expected = AcyclicalGraph(
      HashMap.from(nontrivialGraph2),
      HashMap(
        1 -> HashSet(1, 2, 5),
        2 -> HashSet(1, 2, 5),
        3 -> HashSet(4, 3),
        4 -> HashSet(4, 3),
        5 -> HashSet(1, 2, 5),
        6 -> HashSet(6, 7),
        7 -> HashSet(6, 7),
      ),
      HashMap(
        1 -> List(6, 7, 3, 4),
        2 -> List(6, 7, 3, 4),
        3 -> List(6, 7, 8),
        4 -> List(6, 7, 8),
        5 -> List(6, 7, 3, 4),
        6 -> List(8),
        7 -> List(8),
        8 -> List.empty
      )
    )
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.05") {
    val graph = Map(
      1 -> List.empty,
      2 -> List.empty,
      3 -> List.empty,
      4 -> List.empty,
    )
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph(
      HashMap.from(graph),
      HashMap.empty[Int, HashSet[Int]],
      HashMap.from(graph)
    )
    assert(result == expected)
  }

  test("CyclicalGraph.SCC.06") {
    val graph = Map(
      "A" -> List("B", "C", "D"),
      "B" -> List("C", "E"),
      "C" -> List("E"),
      "D" -> List("E"),
      "E" -> List.empty
    )
    val result = AcyclicalGraph.scc(graph)
    val expected = AcyclicalGraph(
      HashMap.from(graph),
      HashMap.empty[String, HashSet[String]],
      HashMap(
        "A" -> List("B", "C", "D"),
        "B" -> List("E", "C"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    assert(result == expected)
  }

  test("CyclicalGraph.OutGoing.01") {
    val result = AcyclicalGraph.scc(nontrivialGraph1).acyclicalGraph
    val expected = HashMap(
      1 -> List(2, 3),
      2 -> List.empty,
      3 -> List(2),
      4 -> List(2, 3)
    )
    assert(result == expected)
  }

  test("CyclicalGraph.OutGoing.02") {
    val result = AcyclicalGraph.scc(nontrivialGraph2).acyclicalGraph
    val expected = HashMap(
      1 -> List(6, 7, 3, 4),
      2 -> List(6, 7, 3, 4),
      3 -> List(6, 7, 8),
      4 -> List(6, 7, 8),
      5 -> List(6, 7, 3, 4),
      6 -> List(8),
      7 -> List(8),
      8 -> List.empty,
    )
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.01") {
    val graph = nontrivialGraph1
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(graph))
    val expected = List(2, 3, 1, 4)
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.02") {
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(nontrivialGraph2))
    val expected = List(8, 6, 7, 3, 4, 5, 1, 2)
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.03") {
    val graph = Map(
      "A" -> List("B", "C"),
      "B" -> List("D"),
      "C" -> List("B", "D"),
      "D" -> List.empty
    )
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(graph))
    val expected = List("D", "B", "C", "A")
    assert(result == expected)
  }

  test("CyclicalGraph.TopologicalSort.04") {
    val graph = Map(
      "A" -> List("B", "C", "D"),
      "B" -> List("C", "E"),
      "C" -> List("E"),
      "D" -> List("E"),
      "E" -> List.empty
    )
    val result = AcyclicalGraph.topologicalSort(AcyclicalGraph.scc(graph))
    val expected = List("E", "C", "B", "D", "A")
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.01") {
    val graph = AcyclicalGraph.scc(nontrivialGraph1)
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph), graph)
    val expected = List(List(2), List(3), List(1, 4))
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.02") {
    val graph = AcyclicalGraph.scc(nontrivialGraph2)
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph), graph)
    val expected = List(List(8), List(6, 7), List(3, 4), List(1, 2, 5))
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.03") {
    val graph = AcyclicalGraph.scc(Map(
      "A" -> List("B", "C"),
      "B" -> List("D"),
      "C" -> List("B", "D"),
      "D" -> List.empty
    ))
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph), graph)
    val expected = List(List("D"), List("B"), List("C"), List("A"))
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.04") {
    val graph = AcyclicalGraph.scc(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph), graph)
    val expected = List(List("E"), List("C", "D"), List("B"), List("A"))
    assert(result == expected)
  }

  test("CyclicalGraph.Layers.05") {
    val graph = AcyclicalGraph.scc(
      Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "E"),
        "C" -> List("E"),
        "D" -> List("E"),
        "E" -> List.empty
      )
    )
    val result = AcyclicalGraph.layers(AcyclicalGraph.topologicalSort(graph), graph)
    val expected = List(List("E"), List("C", "D"), List("B"), List("A"))
    assert(result == expected)
  }
}
