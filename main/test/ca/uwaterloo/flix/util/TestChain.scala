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

  test("TestToList.01") {
    assertResult(Chain.empty.toList)(List.empty)
  }

  test("TestToList.02") {
    assertResult(Chain(1, 2, 3, 4).toList)(List(1, 2, 3, 4))
  }

  test("TestToList.03") {
    val chain = Chain(1, 2) ++ Chain(3, 4)
    assertResult(chain.toList)(List(1, 2, 3, 4))
  }

  test("TestToList.04") {
    val chain = Chain.empty ++ Chain(1, 2, 3, 4)
    assertResult(chain.toList)(List(1, 2, 3, 4))
  }

  test("TestToList.05") {
    val chain = Chain(1, 2, 3, 4) ++ Chain.empty
    assertResult(chain.toList)(List(1, 2, 3, 4))
  }

  test("TestToList.06") {
    val chains = List(
      Chain.empty,
      Chain(1, 2),
      Chain.empty ++ Chain(3, 4)
    )
    val chain = chains.fold(Chain.empty)(_ ++ _)
    assertResult(chain.toList)(List(1, 2, 3, 4))
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
    val c1 = Seq(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5)).fold(Chain.empty)(_ ++ _)
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
    val c1 = Seq(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5)).fold(Chain.empty)(_ ++ _)
    val c2 = Chain(1, 2, 3, 4, 6)
    assert(c1 != c2)
  }

  test("TestEq.09") {
    val c1 = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    val c2 = Chain(1, 2, 3, 4, 5)
    assert(c1 == c2)
  }

  test("TestEq.10") {
    val c1 = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    val c2 = Chain(2, 2, 3, 4, 6)
    assert(c1 != c2)
  }

  test("TestIsEmpty.01") {
    assert(Chain.empty.isEmpty)
  }

  test("TestIsEmpty.02") {
    assert(!Chain(1).isEmpty)
  }

  test("TestIsEmpty.03") {
    assert(!Chain(1, 2).isEmpty)
  }

  test("TestHead.01") {
    assertResult(Chain.empty.head)(None)
  }

  test("TestHead.02") {
    assertResult(Chain(1).head)(Some(1))
  }

  test("TestHead.03") {
    assertResult(Chain(2, 1).head)(Some(2))
  }

  test("TestHead.04") {
    assertResult(Chain(3, 2, 1).head)(Some(3))
  }

  test("TestLength.01") {
    assertResult(Chain.empty.length)(0)
  }

  test("TestLength.02") {
    assertResult(Chain(1).length)(1)
  }

  test("TestLength.03") {
    assertResult(Chain(1, 2).length)(2)
  }

  test("TestLength.04") {
    assertResult(Chain(1, 2, 3).length)(3)
  }

  test("TestMap.01") {
    assertResult(Chain.empty.map((i: Int) => i > 2))(Chain.empty)
  }

  test("TestMap.02") {
    assertResult(Chain(1).map(i => i > 2))(Chain(false))
  }

  test("TestMap.03") {
    assertResult(Chain(3).map(i => i > 2))(Chain(true))
  }

  test("TestMap.04") {
    assertResult(Chain(1, 2).map(i => i > 2))(Chain(false, false))
  }

  test("TestMap.05") {
    assertResult(Chain(1, 8).map(i => i > 2))(Chain(false, true))
  }

  test("TestMap.06") {
    assertResult(Chain(8, 1).map(i => i > 2))(Chain(true, false))
  }

  test("TestMap.07") {
    assertResult(Chain(7, 8).map(i => i > 2))(Chain(true, true))
  }

  test("TestForeach.01") {
    var r = 21
    Chain.empty.foreach(x => r = x)
    assertResult(r)(21)
  }

  test("TestForeach.02") {
    var r = 21
    Chain(1, 2, 3).foreach(x => r = x)
    assertResult(r)(3)
  }

  test("TestToSeq.01") {
    assertResult(Chain.empty.toSeq)(Seq.empty)
  }

  test("TestToSeq.02") {
    assertResult(Chain(1, 2, 3, 4).toSeq)(Seq(1, 2, 3, 4))
  }

  test("TestToSeq.03") {
    assertResult((Chain(1, 2) ++ Chain(3, 4)).toSeq)(Seq(1, 2, 3, 4))
  }

  test("TestToSeq.04") {
    assertResult((Chain.empty ++ Chain(10, 20, 30, 50)).toSeq)(Seq(10, 20, 30, 50))
  }

  test("TestToSeq.05") {
    assertResult((Chain(1, 2, 3, 4) ++ Chain.empty).toSeq)(Seq(1, 2, 3, 4))
  }


  test("TestExists.01") {
    assertResult(Chain.empty.exists((i: Int) => i > 3))(false)
  }

  test("TestExists.02") {
    assertResult(Chain(1).exists(i => i > 3))(false)
  }

  test("TestExists.03") {
    assertResult(Chain(5).exists(i => i > 3))(true)
  }

  test("TestExists.04") {
    assertResult(Chain(1, 2).exists(i => i > 3))(false)
  }

  test("TestExists.05") {
    assertResult(Chain(1, 6).exists(i => i > 3))(true)
  }

  test("TestExists.06") {
    assertResult(Chain(6, 1).exists(i => i > 3))(true)
  }

  test("TestExists.07") {
    assertResult(Chain(16, 6).exists(i => i > 3))(true)
  }

  test("TestExists.08") {
    assertResult(Chain(1, -9, 3).exists(i => i > 3))(false)
  }

  test("TestExists.09") {
    assertResult(Chain(1, 9, 3).exists(i => i > 3))(true)
  }

  test("TestMkString.01") {
    assertResult(Chain.empty.mkString("+"))("")
  }

  test("TestMkString.02") {
    assertResult(Chain(1, 2, 3).mkString("+"))("1+2+3")
  }

  test("TestMkString.03") {
    val chain = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    assertResult(chain.mkString("-"))("1-2-3-4-5")
  }

  test("TestToString.01") {
    val chain = Chain.empty
    assertResult(chain.toString)("Chain()")
  }

  test("TestToString.02") {
    val chain = Chain.from(Seq.empty)
    assertResult(chain.toString)("Chain()")
  }

  test("TestToString.03") {
    val chain = Chain(1, 2, 3)
    assertResult(chain.toString)("Chain(1, 2, 3)")
  }

  test("TestToString.04") {
    val chain = Seq(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5)).fold(Chain.empty)(_ ++ _)
    assertResult(chain.toString)("Chain(1, 2, 3, 4, 5)")
  }

  test("TestToString.05") {
    val chain = Chain(1) ++ Chain(2) ++ Chain(3) ++ Chain(4) ++ Chain(5)
    assertResult(chain.toString)("Chain(1, 2, 3, 4, 5)")
  }

  test("TestToString.06") {
    val chain = Chain.empty ++ Chain(1) ++ Chain.from(Seq.empty) ++ Chain.from(Seq(2, 3)) ++ Chain.from(Seq(4, 5))
    assertResult(chain.toString)("Chain(1, 2, 3, 4, 5)")
  }
}
