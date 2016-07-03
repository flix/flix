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

import ca.uwaterloo.flix.api.{MatchException, RuleException, SwitchException}
import ca.uwaterloo.flix.language.ast.SourceLocation
import org.scalatest.FunSuite

class TestDeltaDebugger extends FunSuite {

  //
  // Flix Exceptions.
  //

  test("SameException.MatchException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new MatchException("test", loc)
    val ex2 = new MatchException("test", loc)
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.RuleException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new RuleException(loc)
    val ex2 = new RuleException(loc)
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.SwitchException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = new SwitchException("test", loc)
    val ex2 = new SwitchException("test", loc)
    assert(DeltaDebugger.sameException(ex1, ex2))
  }


  test("SameException.TimeoutException.01") {
    val loc = SourceLocation.Unknown
    val ex1 = ???
    val ex2 = new SwitchException("test", loc)
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  //
  // Java Exceptions.
  //

  test("SameException.ArithmeticException.01") {
    val ex1 = new ArithmeticException()
    val ex2 = new ArithmeticException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.ArithmeticException.02") {
    val ex1 = new ArithmeticException("test")
    val ex2 = new ArithmeticException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.IllegalArgumentException.01") {
    val ex1 = new IllegalArgumentException()
    val ex2 = new IllegalArgumentException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.IllegalArgumentException.02") {
    val ex1 = new IllegalArgumentException("test")
    val ex2 = new IllegalArgumentException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.IndexOutOfBoundsException.01") {
    val ex1 = new IndexOutOfBoundsException()
    val ex2 = new IndexOutOfBoundsException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.IndexOutOfBoundsException.02") {
    val ex1 = new IndexOutOfBoundsException("test")
    val ex2 = new IndexOutOfBoundsException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.NoSuchElementException.01") {
    val ex1 = new NoSuchElementException()
    val ex2 = new NoSuchElementException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.NoSuchElementException.02") {
    val ex1 = new NoSuchElementException("test")
    val ex2 = new NoSuchElementException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.NullPointerException.01") {
    val ex1 = new NullPointerException()
    val ex2 = new NullPointerException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.NullPointerException.02") {
    val ex1 = new NullPointerException("test")
    val ex2 = new NullPointerException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.UnsupportedOperationException.01") {
    val ex1 = new UnsupportedOperationException()
    val ex2 = new UnsupportedOperationException()
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  test("SameException.UnsupportedOperationException.02") {
    val ex1 = new UnsupportedOperationException("test")
    val ex2 = new UnsupportedOperationException("test")
    assert(DeltaDebugger.sameException(ex1, ex2))
  }

  //
  // Unequal Exceptions.
  //
  test("SameException.NotEqual") {
    val ex1 = new ArithmeticException()
    val ex2 = new UnsupportedOperationException("test")


    assert(!DeltaDebugger.sameException(ex1, ex2))
  }

}
