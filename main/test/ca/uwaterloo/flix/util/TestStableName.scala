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

  test("bitsFor.Default.01") {
    assert(StableName.bitsFor(StableName.DefaultWidth) == 62)
  }

  test("bitsFor.Min.01") {
    assert(StableName.bitsFor(1) == 5)
  }

  test("bitsFor.Max.01") {
    assert(StableName.bitsFor(StableName.MaxWidth) == 129)
  }

  test("suffix.Golden.DefaultWidth.01") {
    assert(StableName.suffix("") == "7ivixqqhkd3p")
  }

  test("suffix.Golden.DefaultWidth.02") {
    assert(StableName.suffix("hello") == "yeq239y1gobo")
  }

  test("suffix.Golden.DefaultWidth.03") {
    assert(StableName.suffix("Foo.bar") == "vy7st014gcwm")
  }

  test("suffix.Golden.DefaultWidth.04") {
    assert(StableName.suffix("a") == "yob7506ge857")
  }

  test("suffix.Golden.MinWidth.01") {
    assert(StableName.suffix("", 1) == "p")
  }

  test("suffix.Golden.MinWidth.02") {
    assert(StableName.suffix("hello", 1) == "o")
  }

  test("suffix.Golden.MinWidth.03") {
    assert(StableName.suffix("a", 1) == "7")
  }

  test("suffix.Golden.MaxWidth.01") {
    assert(StableName.suffix("", StableName.MaxWidth) == "q7qlhc63je4gw7ivixqqhkd3p")
  }

  test("suffix.Golden.MaxWidth.02") {
    assert(StableName.suffix("hello", StableName.MaxWidth) == "v6vem8ualnohxyeq239y1gobo")
  }

  test("suffix.Golden.MaxWidth.03") {
    assert(StableName.suffix("a", StableName.MaxWidth) == "s890t8stu899xyob7506ge857")
  }

  test("suffix.LengthBound.01") {
    // Not left-padded: length is at most `width`, and can be shorter when the
    // reduced value happens to have a base-36 leading zero digit.
    for (width <- 1 to StableName.MaxWidth) {
      assert(StableName.suffix("some-key", width).length <= width)
    }
  }

  test("suffix.NotEmpty.01") {
    for (width <- 1 to StableName.MaxWidth) {
      assert(StableName.suffix("some-key", width).nonEmpty)
    }
  }

  test("suffix.Deterministic.01") {
    assert(StableName.suffix("a-stable-key") == StableName.suffix("a-stable-key"))
  }

  test("suffix.DifferentKeys.01") {
    assert(StableName.suffix("key-one") != StableName.suffix("key-two"))
  }

  test("suffix.Lowercase.01") {
    val s = StableName.suffix("some other key", StableName.MaxWidth)
    assert(s == s.toLowerCase)
  }

  test("suffix.WidensByNesting.01") {
    // Modulo truncation nests: reducing modulo 36^width2 (width2 > width1) and
    // then modulo 36^width1 again agrees with reducing modulo 36^width1
    // directly, so the narrower render is always a suffix of the wider one.
    assert(StableName.suffix("hello", StableName.MaxWidth).endsWith(StableName.suffix("hello", StableName.DefaultWidth)))
  }

  test("of.UsesFullAlphabet.01") {
    // Digit-based (modulo) truncation, unlike bit-flooring, can reach every
    // one of the 36 possible digit values at width 1 -- including the top of
    // the alphabet, which a floor(width * log2(36))-bit window would exclude.
    val values = (0 until 200).map(i => StableName.suffix(s"key-$i", 1))
    assert(values.exists(_ == "z") || values.exists(_ == "y") || values.exists(_ == "x"))
  }

  test("of.Width.01") {
    assert(StableName.of("", 1) < BigInt(36))
  }

  test("of.RejectsNonPositiveWidth.01") {
    assertThrows[IllegalArgumentException] {
      StableName.of("key", 0)
    }
  }

  test("of.RejectsTooWideWidth.01") {
    assertThrows[IllegalArgumentException] {
      StableName.of("key", StableName.MaxWidth + 1)
    }
  }

}
