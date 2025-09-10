/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.{CompilationResult, TestFn}
import ca.uwaterloo.flix.util.{FileOps, Result}
import org.scalatest.funsuite.AnyFunSuite

class TestStandardLibrary extends AnyFunSuite {

  /** The path to the library tests. */
  val Path = "main/test/ca/uwaterloo/flix/library/"

  private def init(): Unit = {
    // Create a new Flix compiler.
    val flix = new Flix

    // Find and add all test suites.
    val paths = FileOps.getFlixFilesIn(Path, 1).sorted.take(50)
    for (p <- paths) {
      implicit val sctx: SecurityContext = SecurityContext.AllPermissions
      flix.addFlix(p)
    }

    // Compile the program with all the test suites.
    flix.compile().toResult match {
      case Result.Ok(compilationResult) =>
        runTests(compilationResult)
      case Result.Err(errors) =>
        val es = errors.map(_.messageWithLoc(flix.getFormatter)).mkString("\n")
        fail(s"Unable to compile. Failed with: ${errors.length} errors.\n\n$es")
    }
  }

  private def runTests(r: CompilationResult): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = r.getTests.groupBy(_._1.namespace)
    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespace) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Evaluate each test.
      for ((sym, TestFn(_, skip, run)) <- testsByName; if !skip) {
        // Dynamically create a test using the function name.
        test(sym.toString) {
          run()
        }
      }
    }
  }

  init()
}
