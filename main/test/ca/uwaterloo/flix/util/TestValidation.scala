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
    val result = mapN(Validation.success("foo")) {
      case x => x.toUpperCase
    }
    assertResult(Validation.success("FOO"))(result)
  }

  test("map02") {
    val one = mapN(Validation.success("foo")) {
      case x => x.toUpperCase
    }
    val result = mapN(one) {
      case y => y.reverse
    }
    assertResult(Validation.success("OOF"))(result)
  }

  test("map03") {
    val one = mapN(Validation.success("foo")) {
      case x => x.toUpperCase
    }
    val two = mapN(one) {
      case y => y.reverse
    }
    val result = mapN(two) {
      case z => z + z
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("map04") {
    val one = mapN(Validation.success("abc")) {
      case x => x.length
    }
    val result = mapN(one) {
      case y => y < 5
    }
    assertResult(Validation.success(true))(result)
  }

  test("map05") {
    val one = mapN(Validation.success[String, Exception]("abc")) {
      case x => x.charAt(1)
    }
    val two = mapN(one) {
      case y => y + 3
    }
    val result = mapN(two) {
      case z => z.toChar.toString
    }
    assertResult(Validation.success("e"))(result)
  }

  test("mapN01") {
    val result = mapN(Validation.success("foo"), Validation.success("foo")) {
      case (x, y) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("mapN02") {
    val ex = new RuntimeException()
    val result = mapN(Failure(Chain(ex)): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Failure(Chain(ex)))(result)
  }

  test("mapN03") {
    val result = mapN(Validation.success("foo"): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse + x.toUpperCase.reverse
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("flatMapN01") {
    val result = flatMapN(Validation.success("foo")) {
      case x => Validation.success(x.toUpperCase)
    }
    assertResult(Validation.success("FOO"))(result)
  }

  test("flatMapN02") {
    val result = flatMapN(Validation.success("foo")) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("flatMapN03") {
    val ex = DummyError(1)
    val result = flatMapN(Validation.success[String, DummyError]("foo")) {
      case _ => Validation.toFailure(ex)
    }
    assertResult(Failure(Chain(ex)))(result)
  }

  test("flatMapN04") {
    val result = flatMapN(Validation.success("foo")) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("flatMapN05") {
    val result = flatMapN(Validation.success[String, Int]("foo")) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case _ => flatMapN(Failure(Chain(4, 5, 6))) {
          case _ => Failure(Chain(7, 8, 9))
        }
      }
    }
    assertResult(Failure(Chain(4, 5, 6)))(result)
  }

  test("traverse01") {
    val result = traverse(List(1, 2, 3)) {
      case x => Validation.success(x + 1)
    }
    assertResult(Validation.success(List(2, 3, 4)))(result)
  }

  test("traverse02") {
    val result = traverse(List(1, 2, 3)) {
      case _ => Failure(Chain(42))
    }
    assertResult(Failure(Chain(42, 42, 42)))(result)
  }

  test("traverse03") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Validation.success(x) else Failure(Chain(x))
    }
    assertResult(Failure(Chain(2)))(result)
  }

  test("foldRight01") {
    val result = foldRight(List(1, 1, 1))(Validation.success(10)) {
      case (x, acc) => Validation.success(acc - x)
    }
    assertResult(Validation.success(7))(result)
  }

  test("toResult01") {
    val t = Validation.success[String, DummyError]("abc")
    val result = t.toResult
    assertResult(Result.Ok("abc"))(result)
  }

  test("toResult02") {
    val e = DummyError(1)
    val t = Validation.toFailure[String, DummyError](e)
    val result = t.toResult
    assertResult(Result.Err(Chain(e)))(result)
  }

  case class DummyError(n: Int)

}
