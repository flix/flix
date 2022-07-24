/*
 *  Copyright 2022 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import org.scalatest.FunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

class FlixSuite(incremental: Boolean) extends FunSuite {

  /**
    * A global Flix instance that is used if incremental compilation is enabled.
    */
  var flix = new Flix()

  def mkTestDir(path: String)(implicit options: Options): Unit = {
    val iter = Files.walk(Paths.get(path), 1)
      .iterator().asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".flix"))
      .toList.sorted

    for (p <- iter) {
      mkTest(p.toString)
    }
  }

  def mkTest(path: String)(implicit options: Options): Unit = {
    val p = Paths.get(path)
    val n = p.getFileName.toString
    test(n)(compileAndRun(n, p))
  }

  private def compileAndRun(name: String, path: Path)(implicit options: Options): Unit = {
    // Construct a new fresh Flix object if incremental compilation is disabled.
    if (!incremental) {
      flix = new Flix()
    }

    // Set options.
    flix.setOptions(options.copy(xeffects = false))

    // Add the given path.
    flix.addSourcePath(path)

    try {
      // Compile and Evaluate the program to obtain the compilationResult.
      flix.compile() match {
        case Success(compilationResult) =>
          runTests(name, compilationResult)
        case Failure(errors) =>
          val es = errors.map(_.message(flix.getFormatter)).mkString("\n")
          fail(s"Unable to compile. Failed with: ${errors.length} errors.\n\n$es")
      }
    } finally {
      // Remove the source path.
      flix.remSourcePath(path)
    }
  }

  private def runTests(name: String, compilationResult: CompilationResult): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = compilationResult.getTests.groupBy(_._1.namespace)

    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespace) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Evaluate each tests with a clue of its source location.
      for ((sym, defn) <- testsByName) {
        withClue(sym.loc.format) {
          // Evaluate the function.
          val result = defn()
          // Expect the true value, if boolean.
          if (result.isInstanceOf[java.lang.Boolean]) {
            if (result != true) {
              fail("Expected true, but got false.")
            }
          }
        }
      }
    }
  }
}
