package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.Validation._

import org.scalatest.FunSuite

import scala.RuntimeException

class TestValidation extends FunSuite {

  test("map01") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Success("FOO", Vector.empty))(result)
  }

  test("map02") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }
    assertResult(Success("OOF", Vector.empty))(result)
  }

  test("map03") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }.map {
      case z => z + z
    }
    assertResult(Success("OOFOOF", Vector.empty))(result)
  }

  test("map04") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(Success(true, Vector.empty))(result)
  }

  test("map05") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(Success("e", Vector.empty))(result)
  }

  test("map06") {
    val ex = new RuntimeException()
    val result = ex.toFailure[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Failure(Vector(ex)))(result)
  }

  test("@@(List)01") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      "c".toSuccess,
      "d".toSuccess,
      "e".toSuccess
    ))

    assertResult(Success(List("a", "b", "c", "d", "e"), Vector.empty))(result)
  }

  test("@@(List)02") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      "c".toFailure,
      "d".toSuccess,
      "e".toFailure
    ))

    assertResult(Failure(Vector("c", "e")))(result)
  }

  test("@@(List)03") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      Success("c", Vector("x", "y")),
      "d".toSuccess,
      Success("e", Vector("z"))
    ))

    assertResult(Success(List("a", "b", "c", "d", "e"), Vector("x", "y", "z")))(result)
  }

  // TODO: Rest

}
