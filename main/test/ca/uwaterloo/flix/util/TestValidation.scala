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
    assertResult(Success("FOO"))(result)
  }

  test("map02") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }
    assertResult(Success("OOF"))(result)
  }

  test("map03") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }.map {
      case z => z + z
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("map04") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(Success(true))(result)
  }

  test("map05") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(Success("e"))(result)
  }

  test("map06") {
    val ex = new RuntimeException()
    val result = ex.toFailure[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("flatMapN01") {
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => x.toUpperCase.toSuccess
    }
    assertResult(Success("FOO"))(result)
  }

  test("flatMapN02") {
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(x.toUpperCase.toSuccess) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("andThen03") {
    val ex = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => ex.toFailure
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("andThen04") {
    val result = flatMapN("foo".toSuccess[String, Int]) {
      case x => flatMapN(Success(x.toUpperCase)) {
        case y => flatMapN(Success(y.reverse)) {
          case z => Success(z + z)
        }
      }
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("andThen05") {
    val result = flatMapN("foo".toSuccess[String, Int]) {
      case x => flatMapN(Success(x.toUpperCase)) {
        case y => flatMapN(Failure(LazyList(4, 5, 6))) {
          case z => Failure(LazyList(7, 8, 9))
        }
      }
    }
    assertResult(Failure(LazyList(4, 5, 6)))(result)
  }

    test("traverse01") {
      val result = traverse(List(1, 2, 3)) {
        case x => Success(x + 1)
      }

      assertResult(Success(List(2, 3, 4)))(result)
    }

    test("traverse02") {
      val result = traverse(List(1, 2, 3)) {
        case x => Failure(LazyList(42))
      }

      assertResult(Failure(LazyList(42, 42, 42)))(result)
    }

    test("traverse03") {
      val result = traverse(List(1, 2, 3)) {
        case x => if (x % 2 == 1) Success(x) else Failure(LazyList(x))
      }

      assertResult(Failure(LazyList(2)))(result)
    }

    test("foldRight01") {
      val result = foldRight(List(1, 1, 1))(Success(10)) {
        case (x, acc) => (acc - x).toSuccess
      }

      assertResult(Success(7))(result)
    }

}
