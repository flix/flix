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

class FuzzDuplicateLines extends AnyFunSuite with TestUtils {

  /**
    * Number of variants to make of each file. Each variant has a single line duplicated.
    */
  private val N = 20

  test("simple-card-game") {
    val filepath = Paths.get("examples/simple-card-game.flix")
    val lines = Files.lines(filepath)
    compileWithDuplicateLine(filepath.getFileName.toString, lines)
  }

  test("using-channels-and-select") {
    val filepath = Paths.get("examples/using-channels-and-select.flix")
    val lines = Files.lines(filepath)
    compileWithDuplicateLine(filepath.getFileName.toString, lines)
  }

  test("the-ast-typing-problem-with-polymorphic-records") {
    val filepath = Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix")
    val lines = Files.lines(filepath)
    compileWithDuplicateLine(filepath.getFileName.toString, lines)
  }

  test("ford-fulkerson") {
    val filepath = Paths.get("examples/larger-examples/datalog/ford-fulkerson.flix")
    val lines = Files.lines(filepath)
    compileWithDuplicateLine(filepath.getFileName.toString, lines)
  }

  /**
    * Compile N variants of the given program with a single line duplicated.
    * The program may not be valid: We just care that it does not crash the compiler.
    */
  private def compileWithDuplicateLine(name: String, stream: java.util.stream.Stream[String]): Unit = {
    val lines = stream.iterator().asScala.toList
    val numLines = lines.length
    val NFixed = N.min(numLines)
    val step = numLines / NFixed

    val flix = new Flix()
    flix.compile()
    for (i <- 0 until NFixed) {
      val iStepped = Math.min(i * step, numLines)
      val (before, after) = lines.splitAt(iStepped)
      val src = (before ::: after.head :: after).mkString("\n")
      flix.addSourceCode(s"$name-duplicate-line-$iStepped", src)(SecurityContext.AllPermissions)
      flix.compile() // We simply care that this does not crash.
    }
  }

}
