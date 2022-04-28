package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.Packager.PathComparator
import org.scalatest.FunSuite

import java.nio.file.Paths

class TestPathComparator extends FunSuite {
  test("order by file name") {
    val comparator = new PathComparator()
    val list = Array(
      Paths.get("b.txt"),
      Paths.get("a.txt"),
    )
    assert(list.sorted(comparator).sameElements(Array(
      Paths.get("a.txt"),
      Paths.get("b.txt")
    )))
  }
  test("order by folder name") {
    val comparator = new PathComparator()
    val list = Array(
      Paths.get("b", "child.txt"),
      Paths.get("a", "child.txt")
    )
    assert(list.sorted(comparator).sameElements(Array(
      Paths.get("a", "child.txt"),
      Paths.get("b", "child.txt")
    )))
  }
  test("order by child file name") {
    val comparator = new PathComparator()
    val list = Array(
      Paths.get("parent", "b.txt"),
      Paths.get("parent", "a.txt"),
      Paths.get("parent", "c.txt")
    )
    assert(list.sorted(comparator).sameElements(Array(
      Paths.get("parent", "a.txt"),
      Paths.get("parent", "b.txt"),
      Paths.get("parent", "c.txt")
    )))
  }
  test("complex case") {
    val comparator = new PathComparator()
    val list = Array(
      Paths.get("2", "d.txt"),
      Paths.get("1", "c.txt"),
      Paths.get("1", "b.txt"),
      Paths.get("2", "a.txt"),
      Paths.get("2", "3", "c.txt")
    )
    assert(list.sorted(comparator).sameElements(Array(
      Paths.get("1", "b.txt"),
      Paths.get("1", "c.txt"),
      Paths.get("2", "3", "c.txt"),
      Paths.get("2", "a.txt"),
      Paths.get("2", "d.txt")
    )))
  }
}
