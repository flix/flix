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
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class FuzzSwapLines extends AnyFunSuite with TestUtils {

  // Swap lines takes a _long_ time to run.
  // In a file with just 100 lines, there is (100 * 99) / 2 unique non-equal swaps.
  // That's 4950 compiles if we try them all.
  // Assuming each take 1sec to run that ends up at 1.3 hours.
  // Instead we select numSwapLines and try to swap those with each-other.
  // For instance numSwapLines = 10 gives a total of 330 swaps per file.
  private val numSwapLines = 5

  test("simple-card-game") {
    val filepath = Paths.get("examples/simple-card-game.flix")
    val lines = Files.lines(filepath)
    compileWithSwappedLines(filepath.getFileName.toString, lines)
  }

  test("using-channels-and-select") {
    val filepath = Paths.get("examples/using-channels-and-select.flix")
    val lines = Files.lines(filepath)
    compileWithSwappedLines(filepath.getFileName.toString, lines)
  }

  test("the-ast-typing-problem-with-polymorphic-records") {
    val filepath = Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix")
    val lines = Files.lines(filepath)
    compileWithSwappedLines(filepath.getFileName.toString, lines)
  }

  ignore("ford-fulkerson") {
    val filepath = Paths.get("examples/larger-examples/datalog/ford-fulkerson.flix")
    val lines = Files.lines(filepath)
    compileWithSwappedLines(filepath.getFileName.toString, lines)
  }

  /**
    * We compile variants of the given program where we swap [[numSwapLines]] lines.
    * For example, in a file with 100 lines and numSwapLines = 10, we try all swaps with 10 of the lines.
    * line 0 with line 10, 20, 30, 40, 50, 60, 70, 80, 90 and 100.
    * The program may not be valid: We just care that it does not crash the compiler.
    */
  private def compileWithSwappedLines(name: String, stream: java.util.stream.Stream[String]): Unit = {
    val lines = stream.iterator().asScala.toList
    val numLines = lines.length
    val numSwapLinesFixed = numLines.min(numSwapLines)
    val step = numLines / numSwapLinesFixed

    val flix = new Flix()
    flix.compile()
    for (i <- 0 until numSwapLinesFixed) {
      val iStepped = Math.min(i * step, numLines)
      for (j <- i + 1 until numSwapLinesFixed) {
        val jStepped = Math.min(j * step, numLines)
        val src = lines.updated(iStepped, lines(jStepped)).updated(jStepped, lines(iStepped)).mkString("\n")
        flix.addSourceCode(s"$name-swap-lines-$iStepped-and-$jStepped", src)(SecurityContext.AllPermissions)
        flix.compile() // We simply care that this does not crash.
      }
    }
  }
}
