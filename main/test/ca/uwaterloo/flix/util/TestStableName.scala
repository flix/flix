/*
 * Copyright 2026 Werner Stein
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

import org.scalatest.funsuite.AnyFunSuite

class TestStableName extends AnyFunSuite {

  //
  // Golden vectors.
  //
  // These pin the encoding: SHA-256 of the UTF-8 bytes of the key, leading 64 bits read
  // big-endian, rendered in base 36 and left-padded to 13 digits. A change to any part of
  // that pipeline renames every generated class in every Flix program, which is an ABI
  // break with no reviewable diff. These tests are what turn that into a failing build.
  //

  test("golden.01") {
    assert(StableName.suffix("") == "3gng7kheu33tg")
  }

  test("golden.02") {
    assert(StableName.suffix("a") == "32wshvyj5voay")
  }

  test("golden.03") {
    assert(StableName.suffix("List.map|Int32,String|Pure") == "1gm6bet6k2adr")
  }

  test("golden.04") {
    assert(StableName.suffix("List.map|Int32,String|IO") == "2qjs842unsp0z")
  }

  test("golden.05") {
    assert(StableName.suffix("RedBlackTree.insert|Int32,Obj|Pure") == "3vzugeh99noiz")
  }

  test("golden.06") {
    // Non-ASCII keys must hash as UTF-8, not as the platform default charset. If this
    // breaks, generated names depend on the locale of whoever ran the compiler.
    assert(StableName.suffix("üñîçødé") == "15xhdhbvcjkmh")
  }

  //
  // Shape.
  //

  test("width.01") {
    assert(StableName.Width == 13)
  }

  test("width.02") {
    // Thirteen digits are required to hold 64 bits, and twelve are not enough.
    assert(math.pow(36, 12) < math.pow(2, StableName.Bits))
    assert(math.pow(36, 13) > math.pow(2, StableName.Bits))
  }

  test("width.03") {
    // Every suffix is exactly Width digits, including the ones that need padding.
    val suffixes = (0 until 2000).map(i => StableName.suffix(s"key$i"))
    assert(suffixes.forall(_.length == StableName.Width))
  }

  test("alphabet.01") {
    // Lowercase only, so that two suffixes cannot collide on a case-insensitive filesystem.
    val suffixes = (0 until 2000).map(i => StableName.suffix(s"key$i"))
    assert(suffixes.forall(_.forall(c => c.isDigit || (c >= 'a' && c <= 'z'))))
  }

  //
  // Behaviour.
  //

  test("deterministic.01") {
    assert(StableName.suffix("List.map|Int32") == StableName.suffix("List.map|Int32"))
  }

  test("deterministic.02") {
    // The suffix depends on the key alone, never on call order or on how many suffixes
    // have been computed before it. This is the property the GenSym counter lacked.
    val first = (0 until 100).map(i => StableName.suffix(s"key$i"))
    val second = (99 to 0 by -1).map(i => StableName.suffix(s"key$i")).reverse
    assert(first == second)
  }

  test("deterministic.03") {
    // Safe to compute from the parallel specialization phases: MessageDigest is not
    // thread-safe, so a shared instance would silently corrupt suffixes under load.
    val keys = (0 until 500).map(i => s"key$i").toList
    val sequential = keys.map(k => StableName.suffix(k))

    val results = new Array[List[String]](8)
    val threads = (0 until 8).map { t =>
      new Thread(() => results(t) = keys.map(k => StableName.suffix(k)))
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(results.forall(_ == sequential))
  }

  test("distinct.01") {
    // Keys differing only in their effect must not collide.
    assert(StableName.suffix("f|Int32|Pure") != StableName.suffix("f|Int32|IO"))
  }

  test("distinct.02") {
    val suffixes = (0 until 5000).map(i => StableName.suffix(s"key$i")).toSet
    assert(suffixes.size == 5000)
  }

}
