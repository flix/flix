/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.util

import org.scalatest.FunSuite

class TestResult extends FunSuite {

  test("isOk") {
    assert(Result.Ok(42).isOk)
  }

  test("isErr") {
    assert(Result.Err(42).isErr)
  }

  test("get01") {
    assertResult(42)(Result.Ok(42).get)
  }

  test("get02") {
    intercept[IllegalStateException] {
      Result.Err(42).get
    }
  }

  test("map01") {
    assertResult(Result.Ok(43))(Result.Ok(42).map(_ + 1))
  }

  test("map02") {
    assertResult(Result.Err(42))(Result.Err[Int, Int](42).map(_ + 1))
  }

  test("map03") {
    assertResult(Result.Ok(45))(Result.Ok(42).map(_ + 1).map(_ + 1).map(_ + 1))
  }

  test("flatMap01") {
    assertResult(Result.Ok[Int, Int](43))(Result.Ok[Int, Int](42).flatMap(x => Result.Ok(x + 1)))
  }

  test("flatMap02") {
    assertResult(Result.Err[Int, Int](42))(Result.Err[Int, Int](42).flatMap(x => Result.Ok(x + 1)))
  }

  test("flatMap03") {
    assertResult(Result.Ok[Int, Int](44))(Result.Ok[Int, Int](42).flatMap(x => Result.Ok(x + 1)).flatMap(x => Result.Ok(x + 1)))
  }

  test("for01") {
    val r = for (
      a <- Result.Ok[Int, Int](42)
    ) yield a
    assertResult(Result.Ok(42))(r)
  }

  test("for02") {
    val r = for (
      a <- Result.Ok[Int, Int](42);
      b <- Result.Ok(21);
      c <- Result.Ok(11)
    ) yield a + b + c
    assertResult(Result.Ok(42 + 21 + 11))(r)
  }

  test("for03") {
    val r = for (
      a <- Result.Ok[Int, Int](42);
      b <- Result.Err[Int, Int](82);
      c <- Result.Ok(11)
    ) yield a + b
    assertResult(Result.Err(82))(r)
  }

}
