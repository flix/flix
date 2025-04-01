package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.collection.Nel
import org.scalatest.funsuite.AnyFunSuite

class TestNel extends AnyFunSuite {

  private def mkNel[T](x: T, xs: T*): Nel[T] = Nel(x, xs.toList)

  test("length.01") {
    assert(mkNel(1).length == 1)
  }

  test("length.02") {
    assert(mkNel(1, 2).length == 2)
  }

  test("length.03") {
    assert(mkNel('a', 'b', 'c').length == 3)
  }

  test("map.01") {
    assert(Nel("a", List("bb", "ccc")).map(_.length) == mkNel(1, 2, 3))
  }

  test("map.02") {
    assert(Nel("asd", Nil).map(_.length) == mkNel(3))
  }

  test("map.03") {
    assert(mkNel(1, 2, 3, 4).map(x => -x) == mkNel(-1, -2, -3, -4))
  }

  test("toString.01") {
    assert(mkNel(1, 2, 3, 4).toString == "Nel(1, 2, 3, 4)")
  }

  test("toString.02") {
    assert(mkNel(1, Nil).toString == "Nel(1)")
  }

  test("iterator.01") {
    assert(mkNel(1, 2, 3, 4).iterator.sum == 10)
  }

  test("iterator.02") {
    assert(mkNel(1, 2, 3, 4).iterator.toList == List(1, 2, 3 ,4))
  }

  test("toList.01") {
    assert(mkNel(1, 2, 3, 4).toList.sum == 10)
  }

  test("toList.02") {
    assert(mkNel(1, 2, 3, 4).toList == List(1, 2, 3, 4))
  }

}
