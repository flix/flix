package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.collection.Nel
import org.scalatest.funsuite.AnyFunSuite

class TestNel extends AnyFunSuite {

  test("of.01") {
    assert(Nel.of(1) == Nel(1, Nil))
  }

  test("of.02") {
    assert(Nel.of(1, 2, 3) == Nel(1, List(2, 3)))
  }

  test("length.01") {
    assert(Nel.of(1).length == 1)
  }

  test("length.02") {
    assert(Nel.of(1, 2).length == 2)
  }

  test("length.03") {
    assert(Nel.of('a', 'b', 'c').length == 3)
  }

  test("map.01") {
    assert(Nel.of("a", "bb", "ccc").map(_.length) == Nel.of(1, 2, 3))
  }

  test("map.02") {
    assert(Nel.of("asd").map(_.length) == Nel.of(3))
  }

  test("map.03") {
    assert(Nel.of(1, 2, 3, 4).map(x => -x) == Nel.of(-1, -2, -3, -4))
  }

  test("unzip.01") {
    assert(Nel.of((1, 2), (3, 4), (5, 6)).unzip == (Nel.of(1, 3, 5), Nel.of(2, 4, 6)))
  }

  test("unzip.02") {
    assert(Nel.of(("1", "2")).unzip == (Nel.of("1"), Nel.of("2")))
  }

  test("toString.01") {
    assert(Nel.of(1, 2, 3, 4).toString == "Nel(1, 2, 3, 4)")
  }

  test("toString.02") {
    assert(Nel.of(1).toString == "Nel(1)")
  }

  test("iterator.01") {
    assert(Nel.of(1, 2, 3, 4).iterator.sum == 10)
  }

  test("iterator.02") {
    assert(Nel.of(1, 2, 3, 4).iterator.toList == List(1, 2, 3 ,4))
  }

  test("toList.01") {
    assert(Nel.of(1, 2, 3, 4).toList.sum == 10)
  }

  test("toList.02") {
    assert(Nel.of(1, 2, 3, 4).toList == List(1, 2, 3, 4))
  }

}
