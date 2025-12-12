/*
 * Copyright 2025 Magnus Madsen
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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.{CompilationResult, TestFn}
import ca.uwaterloo.flix.util.{FileOps, Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class StandardLibrarySuite extends AnyFunSuite {

  /** The path to the library tests. */
  private val Path = "main/test/ca/uwaterloo/flix/library/"

  /** The default options. */
  private val Opts = Options.DefaultTest.copy(incremental = false, outputJvm = false)

  private def init(): Unit = try {
    // Create a new Flix compiler.
    val flix = new Flix
    flix.setOptions(Opts)

    // Find and add all test suites.
    val paths = FileOps.getFlixFilesIn(Paths.get(Path), 1)
    for (p <- paths) {
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      flix.addFile(p)
    }

    // Compile the program with all test suites.
    flix.compile().toResult match {
      case Result.Ok(compilationResult) =>
        runTests(compilationResult)
      case Result.Err(errors) =>
        val es = errors.map(_.messageWithLoc(flix.getFormatter)).mkString("\n")
        fail(s"Unable to compile. Failed with: ${errors.length} errors.\n\n$es")
    }
  } catch {
    case ex: Throwable =>
      // We create a fictitious test to ensure that something shows up.
      test("StandardLibrary -- COMPILATION FAILED.") {
        fail(ex)
      }
  }

  private def runTests(r: CompilationResult): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = r.getTests.groupBy {
      case (sym, _) => sym.namespace
    }

    // Sort the namespaces.
    val testsByNamespaceSorted = testsByNamespace.toList.sortBy {
      case (ns, _) => ns.mkString(".")
    }

    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespaceSorted) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Dynamically create a ScalaTest unit test for each @Test function.
      for ((sym, TestFn(_, skip, run)) <- testsByName) {
        val testName = sym.toString
        if (skip){
          ignore(testName) {}
        } else {
          test(testName) {
            run()
          }
        }
      }
    }
  }

  init()
}
