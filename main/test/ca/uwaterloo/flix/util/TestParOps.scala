/*
 * Copyright 2023 Holger Dal Mogensen
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

import ca.uwaterloo.flix.api.Flix
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.Executors

class TestParOps extends AnyFunSuite {

  implicit var flix: Flix = new Flix()
  flix.threadPool = Executors.newFixedThreadPool(8)

  test("parMap01") {
    val list = List(1, 2, 3, 4)
    val result = ParOps.parMap(list)(x => x * 2)
    assert(result.toList == List(2, 4, 6, 8))
  }

  test("parMap02") {
    val list = List(List(1, 2), List(3, 4))
    val result = ParOps.parMap(list) {
      subList => ParOps.parMap(subList)(x => x * 2)
    }
    assert(result.toList == List(List(2, 4), List(6, 8)))
  }

  test("parAgg01") {
    val list = List(1, 2, 3, 4)
    val result = ParOps.parAgg(list, List.empty[Int])(
      (acc, x) => acc :+ x * 2,
      (l1, l2) => l1 ++ l2,
    )
    assert(result == List(2, 4, 6, 8))
  }

  test("parAgg02") {
    val range = 1 to 100
    val result = ParOps.parAgg(range, List.empty[Int])(
      (acc, x) => acc :+ x * 2,
      (l1, l2) => l1 ++ l2,
    )
    val doubledRange = range.map(_ * 2)
    assert(result == doubledRange)
  }
}
