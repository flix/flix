/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.Validation._

import org.scalatest.FunSuite


class TestValidation extends FunSuite {

  test("map01") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Success("FOO", Stream.empty))(result)
  }

  test("map02") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }
    assertResult(Success("OOF", Stream.empty))(result)
  }

  test("map03") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }.map {
      case z => z + z
    }
    assertResult(Success("OOFOOF", Stream.empty))(result)
  }

  test("map04") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(Success(true, Stream.empty))(result)
  }

  test("map05") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(Success("e", Stream.empty))(result)
  }

  test("map06") {
    val ex = new RuntimeException()
    val result = ex.toFailure[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Failure(Stream(ex)))(result)
  }

  test("flatMap01") {
    val result = "foo".toSuccess[String, Exception].flatMap {
      case x => x.toUpperCase.toSuccess
    }
    assertResult(Success("FOO", Stream.empty))(result)
  }

  test("flatMap02") {
    val result = "foo".toSuccess[String, Exception].flatMap {
      case x => x.toUpperCase.toSuccess
    }.flatMap {
      case y => y.reverse.toSuccess
    }.flatMap {
      case z => (z + z).toSuccess
    }
    assertResult(Success("OOFOOF", Stream.empty))(result)
  }

  test("flatMap03") {
    val ex = new RuntimeException()
    val result = "foo".toSuccess[String, Exception].flatMap {
      case x => ex.toFailure
    }
    assertResult(Failure(Stream(ex)))(result)
  }

  test("flatMap04") {
    val result = "foo".toSuccess[String, Int].flatMap {
      case x => Success(x.toUpperCase, Stream(1, 2, 3))
    }.flatMap {
      case y => Success(y.reverse, Stream(4, 5, 6))
    }.flatMap {
      case z => Success(z + z, Stream(7, 8, 9))
    }
    assertResult(Success("OOFOOF", Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)))(result)
  }

  test("flatMap05") {
    val result = "foo".toSuccess[String, Int].flatMap {
      case x => Success(x.toUpperCase, Stream(1, 2, 3))
    }.flatMap {
      case y => Failure(Stream(4, 5, 6))
    }.flatMap {
      case z => Failure(Stream(7, 8, 9))
    }
    assertResult(Failure(Stream(1, 2, 3, 4, 5, 6)))(result)
  }

  test("@@(List)01") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      "c".toSuccess,
      "d".toSuccess,
      "e".toSuccess
    ))

    assertResult(Success(List("a", "b", "c", "d", "e"), Stream.empty))(result)
  }

  test("@@(List)02") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      "c".toFailure,
      "d".toSuccess,
      "e".toFailure
    ))

    assertResult(Failure(Stream("c", "e")))(result)
  }

  test("@@(List)03") {
    val result = @@(List(
      "a".toSuccess,
      "b".toSuccess,
      Success("c", Stream("x", "y")),
      "d".toSuccess,
      Success("e", Stream("z"))
    ))

    assertResult(Success(List("a", "b", "c", "d", "e"), Stream("x", "y", "z")))(result)
  }

  test("Collect01") {
    val result = collect(List(
      "a".toSuccess,
      "b".toSuccess,
      "c".toFailure,
      "d".toSuccess,
      "e".toFailure
    ))

    assertResult(Success(List("a", "b", "d"), Stream("c", "e")))(result)
  }

  // TODO: Rest

}
