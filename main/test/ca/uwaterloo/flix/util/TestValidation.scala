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

import ca.uwaterloo.flix.language.errors.{Recoverable, Unrecoverable}
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

  test("map06") {
    val one = mapN(Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case x => x.length
    }
    val result = mapN(one) {
      case y => y < 5
    }
    assertResult(Validation.toSoftFailure(true, DummyRecoverable(1)))(result)
  }

  test("map07") {
    val one = mapN(Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case x => x.charAt(1)
    }
    val two = mapN(one) {
      case y => y + 3
    }
    val result = mapN(two) {
      case z => z.toChar.toString
    }
    assertResult(Validation.toSoftFailure("e", DummyRecoverable(1)))(result)
  }

  test("map08") {
    val one = mapN(Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case x => x.length
    }
    val result = mapN(one) {
      case y => y < 5
    }
    assertResult(Validation.toSoftFailure(true, DummyRecoverable(1)))(result)
  }

  test("map09") {
    val one = mapN(Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case x => x.charAt(1)
    }
    val two = mapN(one) {
      case y => y + 3
    }
    val result = mapN(two) {
      case z => z.toChar.toString
    }
    assertResult(Validation.toSoftFailure("e", DummyRecoverable(1)))(result)
  }

  test("mapN01") {
    val result = mapN(Validation.success("foo"), Validation.success("foo")) {
      case (x, y) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("mapN02") {
    val result = mapN(Validation.success("foo"), Validation.success("foo"), Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case (x, y, _) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Validation.toSoftFailure("OOFOOF", DummyRecoverable(1)))(result)
  }

  test("mapN03") {
    val result = mapN(Validation.success("foo"), Validation.success("foo"), Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(Validation.toSoftFailure("OOFOOFCBA", DummyRecoverable(1)))(result)
  }

  test("mapN04") {
    val result = mapN(Validation.success("foo"), Validation.success("foo"), Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(Validation.toSoftFailure("OOFOOFCBA", DummyRecoverable(1)))(result)
  }

  test("mapN05") {
    val result = mapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Validation.toSoftFailure("OOF", DummyRecoverable(1)))(result)
  }

  test("mapN06") {
    val result = mapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Validation.toSoftFailure("OOF", DummyRecoverable(1)))(result)
  }

  test("mapN07") {
    val ex = new RuntimeException()
    val result = mapN(HardFailure(Chain(ex)): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse
    }
    assertResult(HardFailure(Chain(ex)))(result)
  }

  test("mapN08") {
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
    val result = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success((z + z))
        }
      }
    }
    assertResult(Validation.toSoftFailure("OOFOOF", DummyRecoverable(1)))(result)
  }

  test("flatMapN04") {
    val ex = new RuntimeException()
    val result = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => flatMapN(Validation.toSoftFailure(x.toUpperCase, DummyRecoverable(1))) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.toSuccessOrSoftFailure("OOFOOF", Seq(DummyRecoverable(1), DummyRecoverable(1))))(result)
  }

  test("flatMapN05") {
    val ex = DummyRecoverable(1)
    val result = flatMapN(Validation.success[String, DummyError]("foo")) {
      case x => flatMapN(Validation.toSoftFailure(x.toUpperCase, ex)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.toSoftFailure("OOFOOF", ex))(result)
  }

  test("andThen03") {
    val ex = DummyUnrecoverable(1)
    val result = flatMapN(Validation.success[String, DummyUnrecoverable]("foo")) {
      case _ => Validation.toHardFailure(ex)
    }
    assertResult(HardFailure(Chain(ex)))(result)
  }

  test("andThen04") {
    val result = flatMapN(Validation.success("foo")) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.success("OOFOOF"))(result)
  }

  test("andThen05") {
    val result = flatMapN(Validation.success[String, Int]("foo")) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case _ => flatMapN(HardFailure(Chain(4, 5, 6))) {
          case _ => HardFailure(Chain(7, 8, 9))
        }
      }
    }
    assertResult(HardFailure(Chain(4, 5, 6)))(result)
  }

  test("flatMap01") {
    val val1 = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => Validation.success(x.toUpperCase)
    }
    val val2 = flatMapN(val1) {
      case y => Validation.success(y.reverse)
    }
    val result = flatMapN(val2) {
      case z => Validation.success(z + z)
    }
    assertResult(Validation.toSoftFailure("OOFOOF", DummyRecoverable(1)))(result)
  }

  test("flatMap02") {
    val result = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.toSoftFailure("OOFOOF", DummyRecoverable(1)))(result)
  }

  test("flatMap03") {
    val result = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case z => Validation.toSoftFailure(z + z, DummyRecoverable(1))
        }
      }
    }
    assertResult(Validation.toSuccessOrSoftFailure("OOFOOF", Seq(DummyRecoverable(1), DummyRecoverable(1))))(result)
  }

  test("flatMap04") {
    val result = flatMapN(Validation.toSoftFailure("foo", DummyRecoverable(1))) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.success(y.reverse)) {
          case _ => HardFailure(Chain(DummyRecoverable(1)))
        }
      }
    }
    assertResult(HardFailure(Chain(DummyRecoverable(1), DummyRecoverable(1))))(result)
  }

  test("flatMap05") {
    val result = flatMapN(Validation.toSoftFailure("abc", DummyRecoverable(1))) {
      case x => flatMapN(Validation.success(x.toUpperCase)) {
        case y => flatMapN(Validation.toSoftFailure(y.reverse, DummyRecoverable(1))) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.toSuccessOrSoftFailure("CBACBA", Seq(DummyRecoverable(1), DummyRecoverable(1))))(result)
  }

  test("flatMap06") {
    val result = flatMapN(Validation.success("abc")) {
      case x => flatMapN(Validation.toSoftFailure(x.toUpperCase, DummyRecoverable(1))) {
        case y => flatMapN(Validation.toSoftFailure(y.reverse, DummyRecoverable(2))) {
          case z => Validation.success(z + z)
        }
      }
    }
    assertResult(Validation.toSuccessOrSoftFailure("CBACBA", Seq(DummyRecoverable(1), DummyRecoverable(2))))(result)
  }

  test("traverse01") {
    val result = traverse(List(1, 2, 3)) {
      case x => Validation.success(x + 1)
    }
    assertResult(Validation.success(List(2, 3, 4)))(result)
  }

  test("traverse02") {
    val result = traverse(List(1, 2, 3)) {
      case x => HardFailure(Chain(42))
    }
    assertResult(HardFailure(Chain(42, 42, 42)))(result)
  }

  test("traverse03") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Validation.success(x) else HardFailure(Chain(x))
    }
    assertResult(HardFailure(Chain(2)))(result)
  }

  test("traverse04") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Validation.toSoftFailure(x, DummyRecoverable(-1)) else Validation.toSoftFailure(-1, DummyRecoverable(x))
    }
    assertResult(Validation.toSuccessOrSoftFailure(List(1, -1, 3), Seq(DummyRecoverable(-1), DummyRecoverable(2), DummyRecoverable(-1))))(result)
  }

  test("traverse05") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Validation.success(x) else Validation.toSoftFailure(-1, DummyRecoverable(x))
    }
    assertResult(Validation.toSuccessOrSoftFailure(List(1, -1, 3), Seq(DummyRecoverable(2))))(result)
  }

  test("traverse06") {
    val result = traverse(List(1, 2, 3, 4, 5)) {
      case x => if (x % 2 == 1) Validation.toSoftFailure(x, DummyRecoverable(-x)) else HardFailure(Chain(DummyRecoverable(x)))
    }
    assertResult(HardFailure(Chain(DummyRecoverable(-1), DummyRecoverable(2), DummyRecoverable(-3), DummyRecoverable(4), DummyRecoverable(-5))))(result)
  }

  test("foldRight01") {
    val result = foldRight(List(1, 1, 1))(Validation.success(10)) {
      case (x, acc) => Validation.success(acc - x)
    }
    assertResult(Validation.success(7))(result)
  }

  test("recoverOne01") {
    val f: PartialFunction[DummyRecoverable, String] = (e: DummyRecoverable) => e.toString
    val v = Validation.success("abc").recoverOne(f)
    assertResult(Validation.success("abc"))(v)
  }

  test("recoverOne02") {
    val e = DummyRecoverable(1)
    val f: PartialFunction[DummyRecoverable, String] = (e: DummyRecoverable) => e.toString
    val r = Validation.toSoftFailure(e.toString, e).recoverOne(f)
    assertResult(Validation.toSoftFailure(e.toString, e))(r)
  }

  test("recoverOne03") {
    val ex = new RuntimeException()
    val f: PartialFunction[Exception, String] = (e: Exception) => e.toString
    val result = Validation.HardFailure(Chain(ex, ex)).recoverOne(f)
    assertResult(Validation.HardFailure(Chain(ex, ex)))(result)
  }

  test("toSoftResult01") {
    val t = Validation.success[String, DummyError]("abc")
    val result = t.toSoftResult
    assertResult(Result.Ok(("abc", Chain.empty)))(result)
  }

  test("toSoftResult02") {
    val e = DummyRecoverable(1)
    val t = Validation.toSoftFailure("xyz", e)
    val result = t.toSoftResult
    assertResult(Result.Ok(("xyz", Chain(e))))(result)
  }

  test("toSoftResult03") {
    val e = DummyUnrecoverable(1)
    val t = Validation.toHardFailure[String, DummyUnrecoverable](e)
    val result = t.toSoftResult
    assertResult(Result.Err(Chain(e)))(result)
  }

  test("toHardResult01") {
    val t = Validation.success[String, DummyError]("abc")
    val result = t.toHardResult
    assertResult(Result.Ok("abc"))(result)
  }

  test("toHardResult02") {
    val e = DummyRecoverable(1)
    val t = Validation.toSoftFailure("xyz", e)
    val result = t.toHardResult
    assertResult(Result.Err(Chain(e)))(result)
  }

  test("toHardResult03") {
    val e = DummyUnrecoverable(1)
    val t = Validation.toHardFailure[String, DummyUnrecoverable](e)
    val result = t.toHardResult
    assertResult(Result.Err(Chain(e)))(result)
  }

  trait DummyError

  case class DummyRecoverable(n: Int) extends DummyError with Recoverable

  case class DummyUnrecoverable(n: Int) extends DummyError with Unrecoverable
}
