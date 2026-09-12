/*
 * Copyright 2026 Matthew Lutze
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

import ca.uwaterloo.flix.util.collection.MapOps
import org.scalatest.funsuite.AnyFunSuite

class TestMapOps extends AnyFunSuite {

  test("filterMapValues.Empty.01") {
    val actual = MapOps.filterMapValues(Map.empty[String, Int])(Some(_))
    assert(actual == Map.empty[String, Int])
  }

  test("filterMapValues.KeepAll.01") {
    val m = Map("a" -> 1, "b" -> 2)
    val actual = MapOps.filterMapValues(m)(v => Some(v * 10))
    assert(actual == Map("a" -> 10, "b" -> 20))
  }

  test("filterMapValues.DropAll.01") {
    val m = Map("a" -> 1, "b" -> 2)
    val actual = MapOps.filterMapValues(m)(_ => None: Option[Int])
    assert(actual == Map.empty[String, Int])
  }

  test("filterMapValues.Mixed.01") {
    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val actual = MapOps.filterMapValues(m)(v => if (v % 2 == 1) Some(v) else None)
    assert(actual == Map("a" -> 1, "c" -> 3))
  }

  test("filterMapValues.ChangesValueType.01") {
    val m = Map(1 -> "a", 2 -> "bb")
    val actual = MapOps.filterMapValues(m)(v => Some(v.length))
    assert(actual == Map(1 -> 1, 2 -> 2))
  }

  test("filterMapValues.OutOfBoundsIndex.01") {
    // The use site in Safety: indices that fall outside the type argument list are dropped
    // rather than throwing.
    val targs = List("X", "Y")
    val m = Map("T" -> 0, "U" -> 1, "V" -> 5)
    val actual = MapOps.filterMapValues(m)(targs.lift)
    assert(actual == Map("T" -> "X", "U" -> "Y"))
  }

}
