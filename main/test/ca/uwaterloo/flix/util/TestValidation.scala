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

import ca.uwaterloo.flix.language.errors.Recoverable
import ca.uwaterloo.flix.util.Validation.*
import ca.uwaterloo.flix.util.collection.Chain
import org.scalatest.funsuite.AnyFunSuite


class TestValidation extends AnyFunSuite {

  test("map01") {
    val result = mapN(Validation.Success("foo")) {
      case x => x.toUpperCase
    }
    assertResult(Validation.Success("FOO"))(result)
  }

  test("map02") {
    val one = mapN(Validation.Success("foo")) {
      case x => x.toUpperCase
    }
    val result = mapN(one) {
      case y => y.reverse
    }
    assertResult(Validation.Success("OOF"))(result)
  }

  test("map03") {
    val one = mapN(Validation.Success("foo")) {
      case x => x.toUpperCase
    }
    val two = mapN(one) {
      case y => y.reverse
    }
    val result = mapN(two) {
      case z => z + z
    }
    assertResult(Validation.Success("OOFOOF"))(result)
  }

  test("map04") {
    val one = mapN(Validation.Success("abc")) {
      case x => x.length
    }
    val result = mapN(one) {
      case y => y < 5
    }
    assertResult(Validation.Success(true))(result)
  }

  test("map05") {
    val one = mapN(Validation.Success[String, Exception]("abc")) {
      case x => x.charAt(1)
    }
    val two = mapN(one) {
      case y => y + 3
    }
    val result = mapN(two) {
      case z => z.toChar.toString
    }
    assertResult(Validation.Success("e"))(result)
  }

  test("mapN01") {
    val result = mapN(Validation.Success("foo"), Validation.Success("foo")) {
      case (x, y) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Validation.Success("OOFOOF"))(result)
  }

  test("mapN02") {
    val ex = new RuntimeException()
    val result = mapN(Failure(Chain(ex)): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Failure(Chain(ex)))(result)
  }

  test("mapN03") {
    val result = mapN(Validation.Success("foo"): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse + x.toUpperCase.reverse
    }
    assertResult(Validation.Success("OOFOOF"))(result)
  }

  test("flatMapN01") {
    val result = flatMapN(Validation.Success("foo")) {
      case x => Validation.Success(x.toUpperCase)
    }
    assertResult(Validation.Success("FOO"))(result)
  }

  test("flatMapN02") {
    val result = flatMapN(Validation.Success("foo")) {
      case x => flatMapN(Validation.Success(x.toUpperCase)) {
        case y => flatMapN(Validation.Success(y.reverse)) {
          case z => Validation.Success(z + z)
        }
      }
    }
    assertResult(Validation.Success("OOFOOF"))(result)
  }

  test("flatMapN03") {
    val ex = DummyError(1)
    val result = flatMapN(Validation.Success[String, DummyError]("foo")) {
      case _ => Validation.Failure(ex)
    }
    assertResult(Failure(Chain(ex)))(result)
  }

  test("flatMapN04") {
    val result = flatMapN(Validation.Success("foo")) {
      case x => flatMapN(Validation.Success(x.toUpperCase)) {
        case y => flatMapN(Validation.Success(y.reverse)) {
          case z => Validation.Success(z + z)
        }
      }
    }
    assertResult(Validation.Success("OOFOOF"))(result)
  }

  test("flatMapN05") {
    val result = flatMapN(Validation.Success[String, Int]("foo")) {
      case x => flatMapN(Validation.Success(x.toUpperCase)) {
        case _ => flatMapN(Failure(Chain(4, 5, 6))) {
          case _ => Failure(Chain(7, 8, 9))
        }
      }
    }
    assertResult(Failure(Chain(4, 5, 6)))(result)
  }

  test("traverse01") {
    val result = traverse(List(1, 2, 3)) {
      case x => Validation.Success(x + 1)
    }
    assertResult(Validation.Success(List(2, 3, 4)))(result)
  }

  test("traverse02") {
    val result = traverse(List(1, 2, 3)) {
      case _ => Failure(Chain(42))
    }
    assertResult(Failure(Chain(42, 42, 42)))(result)
  }

  test("traverse03") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Validation.Success(x) else Failure(Chain(x))
    }
    assertResult(Failure(Chain(2)))(result)
  }

  test("foldRight01") {
    val result = foldRight(List(1, 1, 1))(Validation.Success(10)) {
      case (x, acc) => Validation.Success(acc - x)
    }
    assertResult(Validation.Success(7))(result)
  }

  test("toResult01") {
    val t = Validation.Success[String, DummyError]("abc")
    val result = t.toResult
    assertResult(Result.Ok("abc"))(result)
  }

  test("toResult02") {
    val e = DummyError(1)
    val t = Validation.Failure[String, DummyError](e)
    val result = t.toResult
    assertResult(Result.Err(Chain(e)))(result)
  }

  case class DummyError(n: Int)

}
