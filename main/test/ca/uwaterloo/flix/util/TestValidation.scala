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
import ca.uwaterloo.flix.util.Validation._
import org.scalatest.funsuite.AnyFunSuite


class TestValidation extends AnyFunSuite {

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
    val result = SoftFailure("abc", LazyList.empty[Exception]).map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(SoftFailure(true, LazyList.empty[Exception]))(result)
  }

  test("map07") {
    val result = SoftFailure("abc", LazyList.empty[Exception]).map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(SoftFailure("e", LazyList.empty[Exception]))(result)
  }

  test("map08") {
    val ex = new RuntimeException()
    val result = SoftFailure("abc", LazyList(ex)).map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(SoftFailure(true, LazyList(ex)))(result)
  }

  test("map09") {
    val ex = new RuntimeException()
    val result = SoftFailure("abc", LazyList(ex)).map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(SoftFailure("e", LazyList(ex)))(result)
  }

  test("map10") {
    val ex = new RuntimeException()
    val result = ex.toFailure[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("mapN01") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception]) {
      case (x, y) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("mapN02") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SoftFailure("abc", LazyList.empty)) {
      case (x, y, _) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(SoftFailure("OOFOOF", LazyList.empty))(result)
  }

  test("mapN03") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SoftFailure("abc", LazyList.empty)) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(SoftFailure("OOFOOFCBA", LazyList.empty))(result)
  }

  test("mapN04") {
    val ex = new RuntimeException()
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SoftFailure("abc", LazyList(ex))) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(SoftFailure("OOFOOFCBA", LazyList(ex)))(result)
  }

  test("mapN05") {
    val result = mapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => x.toUpperCase.reverse
    }
    assertResult(SoftFailure("OOF", LazyList.empty))(result)
  }

  test("mapN06") {
    val ex = new RuntimeException()
    val result = mapN(SoftFailure("foo", LazyList(ex))) {
      case x => x.toUpperCase.reverse
    }
    assertResult(SoftFailure("OOF", LazyList(ex)))(result)
  }

  test("mapN07") {
    val ex = new RuntimeException()
    val result = mapN(Failure(LazyList(ex)): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("mapN08") {
    val result = mapN("foo".toSuccess: Validation[String, Exception]) {
      case x => x.toUpperCase.reverse + x.toUpperCase.reverse
    }
    assertResult("OOFOOF".toSuccess)(result)
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

  test("flatMapN03") {
    val result = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => flatMapN(x.toUpperCase.toSuccess) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SoftFailure("OOFOOF", LazyList.empty[Exception]))(result)
  }

  test("flatMapN04") {
    val ex = new RuntimeException()
    val result = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => flatMapN(SoftFailure(x.toUpperCase, LazyList(ex))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SoftFailure("OOFOOF", LazyList(ex)))(result)
  }

  test("flatMapN05") {
    val ex = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(SoftFailure(x.toUpperCase, LazyList(ex))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SoftFailure("OOFOOF", LazyList(ex)))(result)
  }

  test("flatMapN06") {
    val ex1 = new RuntimeException()
    val ex2 = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(SoftFailure(x.toUpperCase, LazyList(ex1))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case _ => ex2.toFailure
        }
      }
    }
    assertResult(Failure(LazyList(ex1, ex2)))(result)
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

  test("flatMap01") {
    val val1 = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => x.toUpperCase.toSuccess
    }
    val val2 = flatMapN(val1) {
      case y => y.reverse.toSuccess
    }
    val result = flatMapN(val2) {
      case z => Success(z + z)
    }
    assertResult(SoftFailure("OOFOOF", LazyList.empty))(result)
  }

  test("flatMap02") {
    val result = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => flatMapN(x.toUpperCase.toSuccess[String, Exception]) {
        case y => flatMapN(y.reverse.toSuccess[String, Exception]) {
          case z => Success[String, Exception](z + z)
        }
      }
    }
    assertResult(SoftFailure("OOFOOF", LazyList.empty))(result)
  }

  test("flatMap03") {
    val ex = new RuntimeException()
    val result = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => flatMapN(x.toUpperCase.toSuccess[String, Exception]) {
        case y => flatMapN(y.reverse.toSuccess[String, Exception]) {
          case z => SoftFailure[String, Exception](z + z, LazyList(ex))
        }
      }
    }
    assertResult(SoftFailure("OOFOOF", LazyList(ex)))(result)
  }

  test("flatMap04") {
    val ex = new RuntimeException()
    val result = flatMapN(SoftFailure("foo", LazyList.empty[Exception])) {
      case x => flatMapN(x.toUpperCase.toSuccess[String, Exception]) {
        case y => flatMapN(y.reverse.toSuccess[String, Exception]) {
          case _ => Failure[String, Exception](LazyList(ex))
        }
      }
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("flatMap05") {
    val ex1: Exception = new RuntimeException()
    val ex2: Exception = new RuntimeException()
    val result = flatMapN(SoftFailure("abc", LazyList(ex1))) {
      case x => flatMapN(x.toUpperCase.toSuccess[String, Exception]) {
        case y => flatMapN(SoftFailure[String, Exception](y.reverse, LazyList(ex2))) {
          case z => Success[String, Exception](z + z)
        }
      }
    }
    assertResult(SoftFailure("CBACBA", LazyList(ex1, ex2)))(result)
  }

  test("flatMap06") {
    val ex1: Exception = new RuntimeException()
    val ex2: Exception = new RuntimeException()
    val result = flatMapN("abc".toSuccess[String, Exception]) {
      case x => flatMapN(SoftFailure(x.toUpperCase, LazyList(ex2))) {
        case y => flatMapN(SoftFailure(y.reverse, LazyList(ex1))) {
          case z => Success[String, Exception](z + z)
        }
      }
    }
    assertResult(SoftFailure("CBACBA", LazyList(ex2, ex1)))(result)
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

  test("traverse04") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) SoftFailure(x, LazyList(-1)) else SoftFailure(-1, LazyList(x))
    }
    assertResult(SoftFailure(List(1, -1, 3), LazyList(-1, 2, -1)))(result)
  }

  test("traverse05") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Success(x) else SoftFailure(-1, LazyList(x))
    }
    assertResult(SoftFailure(List(1, -1, 3), LazyList(2)))(result)
  }

  test("traverse06") {
    val result = traverse(List(1, 2, 3, 4, 5)) {
      case x => if (x % 2 == 1) SoftFailure(x, LazyList(-x)) else Failure(LazyList(x))
    }
    assertResult(Failure(LazyList(-1, 2, -3, 4, -5)))(result)
  }

  test("foldRight01") {
    val result = foldRight(List(1, 1, 1))(Success(10)) {
      case (x, acc) => (acc - x).toSuccess
    }
    assertResult(Success(7))(result)
  }

  test("toSoftFailure01") {
    val e = DummyError()
    val v = Validation.softFailure("abc", e)
    assertResult(SoftFailure("abc", LazyList(e)))(v)
  }

  test("toSoftFailure02") {
    val e = DummyError()
    val v = Validation.softFailure("abc", e)
    val result = mapN(v) {
      case s => s.reverse
    }
    assertResult(SoftFailure("cba", LazyList(e)))(result)
  }

  test("toSoftFailure03") {
    val e = DummyError()
    val v = mapN(Validation.softFailure("abc", e)) {
      case s => s.reverse
    }
    assertResult(SoftFailure("cba", LazyList(e)))(v)
  }

  test("recoverOne01") {
    val e = DummyError()
    val f: PartialFunction[DummyError, String] = (e: DummyError) => e.toString
    val v = e.toFailure.recoverOne(f)
    assertResult(Validation.softFailure(e.toString, e))(v)
  }

  test("recoverOne02") {
    val f: PartialFunction[DummyError, String] = (e: DummyError) => e.toString
    val v = "abc".toSuccess.recoverOne(f)
    assertResult("abc".toSuccess)(v)
  }

  test("recoverOne03") {
    val e = DummyError()
    val f: PartialFunction[DummyError, String] = (e: DummyError) => e.toString
    val r = Validation.softFailure(e.toString, e).recoverOne(f)
    assertResult(Validation.softFailure(e.toString, e))(r)
  }

  test("recoverOne04") {
    val ex = new RuntimeException()
    val f: PartialFunction[Exception, String] = (e: Exception) => e.toString
    val result = Validation.Failure(LazyList(ex, ex)).recoverOne(f)
    assertResult(Validation.Failure(LazyList(ex, ex)))(result)
  }

  case class DummyError() extends Recoverable

}
