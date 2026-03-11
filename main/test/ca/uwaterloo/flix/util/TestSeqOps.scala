/*
 * Copyright 2025 Jakob Schneider Villumsen
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

import ca.uwaterloo.flix.util.collection.SeqOps
import org.scalatest.funsuite.AnyFunSuite

class TestSeqOps extends AnyFunSuite {

  test("getDuplicates.01") {
    val s = Seq.empty[Int]
    val expected = List.empty[Int].sorted
    val actual = SeqOps.getDuplicates(s, (e: Int) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.02") {
    val s = Seq(1, 2, 3)
    val expected = List.empty[Int].sorted
    val actual = SeqOps.getDuplicates(s, (e: Int) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.03") {
    val s = Seq(1, 1, 2, 3)
    val expected = List((1, 1)).sorted
    val actual = SeqOps.getDuplicates(s, (e: Int) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.04") {
    val s = Seq(1, 1, 2, 2, 3)
    val expected = List((1, 1), (2, 2)).sorted
    val actual = SeqOps.getDuplicates(s, (e: Int) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.05") {
    val s = Seq(1, 1, 2, 2, 2, 3)
    val expected = List((1, 1), (2, 2), (2, 2)).sorted
    val actual = SeqOps.getDuplicates(s, (e: Int) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.06") {
    val s = Seq((1, 2), (1, 2), (2, 3), (2, 3), (3, 4), (3, 4), (3, 5))
    val expected = List(((1, 2), (1, 2)), ((2, 3), (2, 3)), ((3, 4), (3, 4))).sorted
    val actual = SeqOps.getDuplicates(s, (e: (Int, Int)) => e).sorted
    assert(expected == actual)
  }

  test("getDuplicates.07") {
    val s = Seq((1, 2), (1, 2), (2, 3), (2, 3), (3, 4), (3, 4), (3, 5))
    val expected = List(((1, 2), (1, 2)), ((2, 3), (2, 3)), ((3, 4), (3, 4)), ((3, 4), (3, 5))).sorted
    val actual = SeqOps.getDuplicates(s, (e: (Int, Int)) => e._1).sorted
    assert(expected == actual)
  }

  test("getDuplicates.08") {
    val s = Seq((1, 2), (1, 2), (2, 3), (2, 3), (3, 4), (3, 4), (3, 5))
    val expected = List(((1, 2), (1, 2)), ((2, 3), (2, 3)), ((3, 4), (3, 4))).sorted
    val actual = SeqOps.getDuplicates(s, (e: (Int, Int)) => e._2).sorted
    assert(expected == actual)
  }

  test("getDuplicates.09") {
    val s = Seq((1, 2), (1, 2), (2, 3), (2, 3), (3, 4), (3, 4), (5, 4))
    val expected = List(((1, 2), (1, 2)), ((2, 3), (2, 3)), ((3, 4), (3, 4)), ((3, 4), (5, 4))).sorted
    val actual = SeqOps.getDuplicates(s, (e: (Int, Int)) => e._2).sorted
    assert(expected == actual)
  }

}
