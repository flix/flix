/*
 * Copyright 2024 Magnus Madsen
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
package flix.fuzzers

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class FuzzPrefixes extends AnyFunSuite with TestUtils {

  /**
    * The number of prefixes to compile for each program.
    */
  private val N: Int = 100

  test("Prefixes.simple-card-game") {
    val input = Files.readString(Paths.get("examples/simple-card-game.flix"))
    compilePrefixes(input)
  }

  test("Prefixes.the-ast-typing-problem-with-polymorphic-records") {
    val input = Files.readString(Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix"))
    compilePrefixes(input)
  }

  test("Prefixes.using-channels-and-select") {
    val input = Files.readString(Paths.get("examples/using-channels-and-select.flix"))
    compilePrefixes(input)
  }

  /**
    * We break the given string `input` down into N prefixes and compile each of them.
    *
    * For example, if N is 100 and the input has length 300 then we create prefixes of length 3, 6, 9, ...
    *
    * A prefix might not be a valid program: What we care about is that the compiler must not crash.
    */
  private def compilePrefixes(input: String): Unit = {
    val length = input.length
    val step = length / N

    val flix = new Flix()
    flix.compile()
    for (i <- 1 until N) {
      val e = Math.min(i * step, length)
      val prefix = input.substring(0, e)
      flix.addSourceCode("<input>", prefix)
      flix.compile() // We simply care that this does not crash.
    }
  }

}
