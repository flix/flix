/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.util.collection.Chain
import org.scalatest.funsuite.AnyFunSuite

class TestChain extends AnyFunSuite {

  test("TestChain.01") {
    val chain = Chain.empty
    assert(chain.isEmpty)
  }

  test("TestChain.02") {
    val chain = Chain(1, 2, 3, 4)
    assert(chain.toList == List(1, 2, 3, 4))
  }

  test("TestChain.03") {
    val chain = Chain(1, 2) ++ Chain(3, 4)
    assert(chain.toList == List(1, 2, 3, 4))
  }

  test("TestChain.04") {
    val chain = Chain.empty ++ Chain(1, 2, 3, 4)
    assert(chain.toList == List(1, 2, 3, 4))
  }

  test("TestChain.05") {
    val chain = Chain(1, 2, 3, 4) ++ Chain.empty
    assert(chain.toList == List(1, 2, 3, 4))
  }

  test("TestChain.06") {
    val chains = List(
      Chain.empty,
      Chain(1, 2),
      Chain.empty ++ Chain(3, 4)
    )
    val chain = Chain.concat(chains)
    assert(chain.toList == List(1, 2, 3, 4))
  }

  test("TestEq.01") {
    assertResult(Chain.empty)(Chain.empty)
  }

  test("TestEq.02") {
    val c1 = Chain(1, 2, 3, 4, 5)
    assertResult(c1)(c1)
  }

  test("TestEq.03") {
    val c1 = Chain(1, 2, 3, 4, 5)
    val c2 = Chain(1, 2, 3, 4, 5)
    assertResult(c1)(c2)
  }

  test("TestEq.04") {
    val c1 = Chain(1) ++ Chain(2)
    val c2 = Chain(1, 2)
    assertResult(c1)(c2)
  }

  test("TestEq.05") {
    val c1 = Chain.concat(Seq(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5)))
    val c2 = Chain(1, 2, 3, 4, 5)
    assertResult(c1)(c2)
  }

  test("TestEq.06") {
    val c1 = Chain(1, 2, 3, 4, 5)
    val c2 = Chain(1, 1, 3, 4, 5)
    assert(c1 != c2)
  }

  test("TestEq.07") {
    val c1 = Chain(1) ++ Chain(3)
    val c2 = Chain(1, 2)
    assert(c1 != c2)
  }

  test("TestEq.08") {
    val c1 = Chain.concat(Seq(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5)))
    val c2 = Chain(1, 2, 3, 4, 6)
    assert(c1 != c2)
  }

  test("TestEq.09") {
    val c1 = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    val c2 = Chain(1, 2, 3, 4, 6)
    assert(c1 == c2)
  }

  test("TestEq.09") {
    val c1 = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    val c2 = Chain(2, 2, 3, 4, 6)
    assert(c1 != c2)
  }
}
