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

import ca.uwaterloo.flix.util.Result.*
import org.scalatest.funsuite.AnyFunSuite

class TestResult extends AnyFunSuite {

  test("get01") {
    assertResult(42)(Ok(42).get)
  }

  test("get02") {
    intercept[IllegalStateException] {
      Err(42).get
    }
  }

  test("map01") {
    assertResult(Ok(43))(Ok(42).map(_ + 1))
  }

  test("map02") {
    assertResult(Err(42))(Err[Int, Int](42).map(_ + 1))
  }

  test("map03") {
    assertResult(Ok(45))(Ok(42).map(_ + 1).map(_ + 1).map(_ + 1))
  }

  test("flatMap01") {
    assertResult(Ok[Int, Int](43))(Ok[Int, Int](42).flatMap(x => Ok(x + 1)))
  }

  test("flatMap02") {
    assertResult(Err[Int, Int](42))(Err[Int, Int](42).flatMap(x => Ok(x + 1)))
  }

  test("flatMap03") {
    assertResult(Ok[Int, Int](44))(Ok[Int, Int](42).flatMap(x => Ok(x + 1)).flatMap(x => Ok(x + 1)))
  }

  test("for01") {
    val r = for (
      a <- Ok[Int, Int](42)
    ) yield a
    assertResult(Ok(42))(r)
  }

  test("for02") {
    val r = for (
      a <- Ok[Int, Int](42);
      b <- Ok(21);
      c <- Ok(11)
    ) yield a + b + c
    assertResult(Ok(42 + 21 + 11))(r)
  }

  test("for03") {
    val r = for (
      a <- Ok[Int, Int](42);
      b <- Err[Int, Int](82);
      c <- Ok(11)
    ) yield a + b
    assertResult(Err(82))(r)
  }

  test("seqM01") {
    assertResult(Ok(Nil))(sequence(Nil))
  }

  test("seqM02") {
    val a = Ok(1): Result[Int, String]
    val b = Ok(2): Result[Int, String]
    assertResult(Ok(List(1, 2)))(sequence(List(a, b)))
  }

  test("seqM03") {
    val a = Ok(1): Result[Int, String]
    val b = Ok(2): Result[Int, String]
    val c = Ok(3): Result[Int, String]
    assertResult(Ok(List(1, 2, 3)))(sequence(List(a, b, c)))
  }

  test("seqM04") {
    val a = Ok(1): Result[Int, String]
    val b = Ok(2): Result[Int, String]
    val c = Ok(3): Result[Int, String]
    val d = Ok(4): Result[Int, String]
    assertResult(Ok(List(1, 2, 3, 4)))(sequence(List(a, b, c, d)))
  }

  test("traverse01") {
    val actual = Result.traverse(List.empty[Int])(x => Ok(x + 1))
    assertResult(actual)(Ok(List.empty))
  }

  test("traverse02") {
    val actual = Result.traverse(List(1))(x => Ok(x + 1))
    assertResult(actual)(Ok(List(2)))
  }

  test("traverse03") {
    val actual = Result.traverse(List(1, 2))(x => Ok(x + 1))
    assertResult(actual)(Ok(List(2, 3)))
  }

  test("traverse04") {
    val actual = Result.traverse(List(1, 2, 3))(x => Ok(x + 1))
    assertResult(actual)(Ok(List(2, 3, 4)))
  }

  test("traverse05") {
    val actual = Result.traverse(List(1, 2, 3))(x => if (x == 1) Err("one") else Ok(x))
    assertResult(actual)(Err("one"))
  }

  test("traverse06") {
    val actual = Result.traverse(List(1, 2, 3))(x => if (x == 2) Err("two") else Ok(x))
    assertResult(actual)(Err("two"))
  }

  test("traverse07") {
    val actual = Result.traverse(List(1, 2, 3))(x => if (x == 3) Err("three") else Ok(x))
    assertResult(actual)(Err("three"))
  }

  test("traverse08") {
    val actual = Result.traverse(List(1, 2, 3))(_ => Ok(42))
    assertResult(actual)(Ok(List(42, 42, 42)))
  }
}
