/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation}
import org.scalatest.FunSuite

import scala.concurrent.duration.{Duration, _}

class TestDeltaSolver extends FunSuite {

  //
  // Flix Exceptions.
  //
  test("SameException.MatchException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new MatchException("test", loc)
    val ex2 = new MatchException("test", loc)
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.RuleException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new RuleException("test", loc)
    val ex2 = new RuleException("test", loc)
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.SwitchException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new SwitchException("test", loc)
    val ex2 = new SwitchException("test", loc)
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.TimeoutException.01") {
    val ex1 = new TimeoutException(Duration(1, SECONDS), Duration(3, SECONDS))
    val ex2 = new TimeoutException(Duration(2, SECONDS), Duration(4, SECONDS))
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.UserException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new UserException("test", loc)
    val ex2 = new UserException("test", loc)
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  //
  // Java Exceptions.
  //
  test("SameException.ArithmeticException.01") {
    val ex1 = new ArithmeticException()
    val ex2 = new ArithmeticException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.ArithmeticException.02") {
    val ex1 = new ArithmeticException("test")
    val ex2 = new ArithmeticException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.IllegalArgumentException.01") {
    val ex1 = new IllegalArgumentException()
    val ex2 = new IllegalArgumentException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.IllegalArgumentException.02") {
    val ex1 = new IllegalArgumentException("test")
    val ex2 = new IllegalArgumentException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.IndexOutOfBoundsException.01") {
    val ex1 = new IndexOutOfBoundsException()
    val ex2 = new IndexOutOfBoundsException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.IndexOutOfBoundsException.02") {
    val ex1 = new IndexOutOfBoundsException("test")
    val ex2 = new IndexOutOfBoundsException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NoSuchElementException.01") {
    val ex1 = new NoSuchElementException()
    val ex2 = new NoSuchElementException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NoSuchElementException.02") {
    val ex1 = new NoSuchElementException("test")
    val ex2 = new NoSuchElementException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NullPointerException.01") {
    val ex1 = new NullPointerException()
    val ex2 = new NullPointerException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NullPointerException.02") {
    val ex1 = new NullPointerException("test")
    val ex2 = new NullPointerException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.UnsupportedOperationException.01") {
    val ex1 = new UnsupportedOperationException()
    val ex2 = new UnsupportedOperationException()
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.UnsupportedOperationException.02") {
    val ex1 = new UnsupportedOperationException("test")
    val ex2 = new UnsupportedOperationException("test")
    assert(DeltaSolver.sameException(ex1, ex2))
  }

  //
  // Unequal Exceptions.
  //
  test("SameException.NotEqual01") {
    val ex00 = new MatchException("test", SourceLocation.Unknown)
    val ex01 = new RuleException("test", SourceLocation.Unknown)
    val ex02 = new SwitchException("test", SourceLocation.Unknown)
    val ex03 = new TimeoutException(Duration(1, SECONDS), Duration(2, SECONDS))
    val ex04 = new UserException("test", SourceLocation.Unknown)
    val ex05 = new ArithmeticException()
    val ex06 = new IllegalArgumentException()
    val ex07 = new IndexOutOfBoundsException()
    val ex08 = new NoSuchElementException()
    val ex09 = new NullPointerException()
    val ex10 = new UnsupportedOperationException()

    assert(!DeltaSolver.sameException(ex00, ex01))
    assert(!DeltaSolver.sameException(ex01, ex02))
    assert(!DeltaSolver.sameException(ex02, ex03))
    assert(!DeltaSolver.sameException(ex03, ex04))
    assert(!DeltaSolver.sameException(ex04, ex05))
    assert(!DeltaSolver.sameException(ex05, ex06))
    assert(!DeltaSolver.sameException(ex06, ex07))
    assert(!DeltaSolver.sameException(ex07, ex08))
    assert(!DeltaSolver.sameException(ex08, ex09))
    assert(!DeltaSolver.sameException(ex09, ex10))
    assert(!DeltaSolver.sameException(ex10, ex00))
  }

  test("SameException.NotEqual.MatchException01") {
    val sl1 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 21, () => "test")
    val sl2 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 42, () => "test")
    val ex1 = new MatchException("test", sl1)
    val ex2 = new MatchException("test", sl2)
    assert(!DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NotEqual.RuleException") {
    val sl1 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 21, () => "test")
    val sl2 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 42, () => "test")
    val ex1 = new RuleException("test", sl1)
    val ex2 = new RuleException("test", sl2)
    assert(!DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NotEqual.SwitchException") {
    val sl1 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 21, () => "test")
    val sl2 = SourceLocation(SourceInput.Str("test"), 1, 0, 1, 42, () => "test")
    val ex1 = new SwitchException("test", sl1)
    val ex2 = new SwitchException("test", sl2)
    assert(!DeltaSolver.sameException(ex1, ex2))
  }

  test("SameException.NotEqual.UserException") {
    val ex1 = new UserException("ONE", SourceLocation.Unknown)
    val ex2 = new UserException("TWO", SourceLocation.Unknown)
    assert(!DeltaSolver.sameException(ex1, ex2))
  }

}
