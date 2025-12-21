/*
 *  Copyright 2025 Chenhao Gao
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

import ca.uwaterloo.flix.util.collection.ListMap
import org.scalatest.funsuite.AnyFunSuite

class TestListMap extends AnyFunSuite {

  test("ListMap.apply.01") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    assert(lm.m == Map(1 -> List(1, 2), 2 -> List(3, 4)))
  }

  test("ListMap.apply.02") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4), 1 -> List(3, 4))
    assert(lm.m == Map(1 -> List(3, 4, 1, 2), 2 -> List(3, 4)))
  }

  test("ListMap.from.01") {
    val lm = ListMap.from((1, 2) :: (2, 3) :: Nil)
    assert(lm.m == Map(1 -> List(2), 2 -> List(3)))
  }

  test("ListMap.from.02") {
    val lm = ListMap.from((1, 2) :: (2, 3) :: (1, 3) :: Nil)
    assert(lm.m == Map(1 -> List(3, 2), 2 -> List(3)))
  }

  test("ListMap.map.01") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm.map {
      case (k, v) => k + v
    }
    assert(res == Iterable(2, 3, 5, 6))
  }

  test("ListMap.flatMap.01") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm.flatMap {
      case (k, v) => List(k + v)
    }
    assert(res == Iterable(2, 3, 5, 6))
  }

  test("ListMap.foldLeft.01") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm.foldLeft(0) {
      case (acc, (k, v)) => acc + k + v
    }
    assert(res == 16)
  }

  test("ListMap.+.01") {
    val lm1 = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm1 + (3, 5)
    assert(res == ListMap(1 -> List(1, 2), 2 -> List(3, 4), 3 -> List(5)))
  }

  test("ListMap.+.02") {
    val lm1 = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm1 + (1, 1) + (1, 2) + (1, 3)
    assert(res == ListMap(1 -> List(3, 2, 1, 1, 2), 2 -> List(3, 4)))
  }

  test("ListMap.++.01") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm ++ (1 -> List(5, 6))
    assert(res == ListMap(1 -> List(5, 6, 1, 2), 2 -> List(3, 4)))
  }

  test("ListMap.++.02") {
    val lm1 = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val lm2 = ListMap(1 -> List(5, 6), 2 -> List(7, 8))
    val res = lm1 ++ lm2
    assert(res == ListMap(1 -> List(5, 6, 1, 2), 2 -> List(7, 8, 3, 4)))
  }

  test("ListMap.-") {
    val lm = ListMap(1 -> List(1, 2), 2 -> List(3, 4))
    val res = lm - 1
    assert(res == ListMap(2 -> List(3, 4)))
  }

  test("ListMap.--") {
    val lm1 = ListMap(1 -> List(1, 2), 2 -> List(3, 4), 3 -> List(5, 6))
    val ks = 1 :: 2 :: Nil;
    val res = lm1 -- ks
    assert(res == ListMap(3 -> List(5, 6)))
  }
}
