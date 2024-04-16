/*
 * Copyright 2024 Magnus Madsen, Herluf Baggesen
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
import scala.jdk.CollectionConverters._

class FuzzDuplicateLines extends AnyFunSuite with TestUtils {

  /**
    * Number of lines to duplicate from each file.
    */
  private val N = 10

  test("simple-card-game") {
    val lines = Files.lines(Paths.get("examples/simple-card-game.flix"))
    compileWithDuplicateLine("simple-card-game", lines)
  }

  test("using-channels-and-select") {
    val lines = Files.lines(Paths.get("examples/using-channels-and-select.flix"))
    compileWithDuplicateLine("using-channels-and-select", lines)
  }

  test("the-ast-typing-problem-with-polymorphic-records") {
    val lines = Files.lines(Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix"))
    compileWithDuplicateLine("the-ast-typing-problem-with-polymorphic-records", lines)
  }

  /**
    * We compile all variants of the given program where we duplicate a single line.
    * The program may not be valid: We just care that it does not crash the compiler.
    */
  private def compileWithDuplicateLine(name: String, stream: java.util.stream.Stream[String]): Unit = {
    val lines = stream.iterator().asScala.toList
    val numLines = lines.length
    val NFixed = N.min(numLines)
    val step = numLines / NFixed

    val flix = new Flix()
    flix.compile()
    for (i_ <- 0 until NFixed) {
      val i = Math.min(i_ * step, numLines)
      val (before, after) = lines.splitAt(i)
      val src = (before ::: after.head :: after).mkString("\n")
      flix.addSourceCode(s"$name-duplicate-line-$i", src)
      flix.compile() // We simply care that this does not crash.
    }
  }

}
