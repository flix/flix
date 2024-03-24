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
import scala.jdk.CollectionConverters._
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class FuzzLines extends AnyFunSuite with TestUtils {

  test("Prefixes.simple-card-game") {
    val input = Files.lines(Paths.get("examples/simple-card-game.flix"))
    compileAllLinesLessOne(input)
  }

  test("Prefixes.the-ast-typing-problem-with-polymorphic-records") {
    val input = Files.lines(Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix"))
    compileAllLinesLessOne(input)
  }

  test("Prefixes.using-channels-and-select") {
    val input = Files.lines(Paths.get("examples/using-channels-and-select.flix"))
    compileAllLinesLessOne(input)
  }

  /**
    * We compile the given program omitting a single line.
    *
    * For example, we compile the program without the 1st, 2nd, 3rd and so forth line.
    *
    * The program may not be valid: We just care that it does not crash the compiler.
    */
  private def compileAllLinesLessOne(stream: java.util.stream.Stream[String]): Unit = {
    val lines = stream.iterator().asScala.toList
    val numberOfLines = lines.length

    val flix = new Flix()
    flix.compile()
    for (i <- 0 until numberOfLines) {
      val (before, after) = lines.splitAt(i)
      val src = (before ::: after.drop(1)).mkString("\n")
      flix.addSourceCode("<input>", src)
      flix.compile() // We simply care that this does not crash.
    }
  }

}
