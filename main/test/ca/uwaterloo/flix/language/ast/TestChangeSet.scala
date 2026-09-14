package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.SecurityContext.Unrestricted
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Origin, Source, SourceName}
import ca.uwaterloo.flix.util.collection.{ListMap, MultiMap}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class TestChangeSet extends AnyFunSuite {
  case class MySourceable(name: SourceName) extends Sourceable {
    override def src: Source = Source.empty(name, Origin.User, Unrestricted)
  }

  private def mkName(name: String): SourceName = SourceName.PathName(Path.of(name))

  private val name1 = mkName("name1")
  private val name2 = mkName("name2")
  private val name3 = mkName("name3")
  private val name4 = mkName("name4")
  private val name5 = mkName("name5")

  private val src1 = MySourceable(name1)
  private val src2 = MySourceable(name2)
  private val src3 = MySourceable(name3)
  private val src4 = MySourceable(name4)
  private val src5 = MySourceable(name5)

  private val dg1 = DependencyGraph.empty
  private val dg2 = DependencyGraph(MultiMap(Map(
    name1 -> Set(name2, name3), name2 -> Set(name4), name3 -> Set(name5)
  )))

  test("ChangeSet.Everything.partition should make everything stale") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src3 -> 3, src4 -> 4)

    val cs = ChangeSet.Everything
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == newMap)
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partition should put anything changed into staleMap.01") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src1 -> 3, src2 -> 2)

    val cs = ChangeSet.Everything.markChanged(name1, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src1 -> 3))
    assert(freshMap == Map(src2 -> 2))
  }

  test("ChangeSet.partition should put anything changed into staleMap.02") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5)
    val newMap = Map(src1 -> 3, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5)

    val cs = ChangeSet.Everything.markChanged(name1, dg2)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src1 -> 3, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5))
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partition should put anything new into staleMap") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)

    val cs = ChangeSet.Everything.markChanged(name3, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src3 -> 3))
    assert(freshMap == Map(src1 -> 1, src2 -> 2))
  }

  test("ChangeSet.partition should ignore anything deleted") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)
    val newMap = Map(src1 -> 1, src2 -> 2)

    val cs = ChangeSet.Everything.markChanged(name3, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map.empty)
    assert(freshMap == Map(src1 -> 1, src2 -> 2))
  }

  test("ChangeSet.partition test with both newed and deleted input") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)
    val newMap = Map(src2 -> 2, src3 -> 3, src4 -> 4)

    val cs = ChangeSet.Everything.markChanged(name1, dg1).markChanged(name4, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src4 -> 4))
    assert(freshMap == Map(src2 -> 2, src3 -> 3))
  }

  test("ChangeSet.partition test with new, deleted and changed input") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3, src5 -> 5)
    val newMap = Map(src2 -> 2, src3 -> 30, src4 -> 4, src5 -> 5)

    val cs = ChangeSet.Everything.markChanged(name1, dg1).markChanged(name3, dg2).markChanged(name4, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src3 -> 30, src4 -> 4, src5 -> 5))
    assert(freshMap == Map(src2 -> 2))
  }

  test("ChangeSet.Everything.partitionOnValues should make everything stale") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = ListMap(3 -> List(src3), 4 -> List(src4, src5))

    val cs = ChangeSet.Everything
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == newMap)
    assert(freshMap == ListMap.empty)
  }

  test("ChangeSet.partitionOnValues should put anything changed into staleMap.01") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2))

    val cs = ChangeSet.Everything.markChanged(name1, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap(1 -> List(src1)))
    assert(freshMap == ListMap(1 -> List(src2), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues should put anything changed into staleMap.02") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5))

    val cs = ChangeSet.Everything.markChanged(name1, dg2)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap(1 -> List(src2, src1), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5)))
    assert(freshMap == ListMap.empty)
  }

  test("ChangeSet.partitionOnValues should put anything new into staleMap") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))

    val cs = ChangeSet.Everything.markChanged(name3, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap(2 -> List(src3), 3 -> List(src4)))
    assert(freshMap == ListMap(1 -> List(src2, src1), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues should ignore anything deleted") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2))

    val cs = ChangeSet.Everything.markChanged(name3, dg1).markChanged(name4, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap.empty)
    assert(freshMap == ListMap(1 -> List(src2, src1), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues test with both newed and deleted input") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src4, src5))

    val cs = ChangeSet.Everything.markChanged(name3, dg1).markChanged(name5, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap(3 -> List(src5)))
    assert(freshMap == ListMap(1 -> List(src2, src1), 2 -> List(src2), 3 -> List(src4)))
  }

  test("ChangeSet.partitionOnValues test with new, deleted and changed input") {
    val oldMap = ListMap(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src5))
    val newMap = ListMap(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src4, src5))

    val cs = ChangeSet.Everything.markChanged(name1, dg1).markChanged(name3, dg1).markChanged(name4, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap, (v1: MySourceable, v2: MySourceable) => v1 == v2)

    assert(staleMap == ListMap(1 -> List(src1), 3 -> List(src4)))
    assert(freshMap == ListMap(1 -> List(src2), 2 -> List(src2), 3 -> List(src5)))
  }
}
