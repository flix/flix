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

class FuzzSwapLines extends AnyFunSuite with TestUtils {

  private val testFiles = List(
    "simple-card-game" -> Files.lines(Paths.get("examples/simple-card-game.flix")),
    "the-ast-typing-problem-with-polymorphic-records" -> Files.lines(Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix")),
    "using-channels-and-select" -> Files.lines(Paths.get("examples/using-channels-and-select.flix")),
  )

  testFiles.foreach {
    case (name, input) => test(s"$name-swap-lines")(compileWithSwappedLines(name, input))
  }

  /**
    * We compile all variants of the given program where we swap lines (i, j) where i < j.
    *
    * For example, we omit line 1 and compile the program. Then we omit line 2 and compile the program. And so forth.
    *
    * The program may not be valid: We just care that it does not crash the compiler.
    */
  private def compileWithSwappedLines(name: String, stream: java.util.stream.Stream[String]): Unit = {
    val lines = stream.iterator().asScala.toList
    val numberOfLines = lines.length

    val flix = new Flix()
    flix.compile()
    for (i <- 0 until numberOfLines - 1) {
      for (j <- i + 1 until numberOfLines) {
        val src = lines.updated(i, lines(j)).updated(j, lines(i)).mkString("\n")
        flix.addSourceCode(s"$name-swap-lines-$i-and-$j", src)
        flix.compile() // We simply care that this does not crash.
      }
    }
  }

}
