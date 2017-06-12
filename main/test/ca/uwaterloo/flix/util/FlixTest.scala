/*
 *  Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.runtime.Model
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import org.scalatest.FunSuite

class FlixTest(name: String, path: String) extends FunSuite {

  /**
    * Returns the name of the test suite.
    */
  override def suiteName: String = name

  /**
    * Attempts to initialize all the tests.
    */
  private def init(): Unit = {
    // Options and Flix object.
    val opts = Options.DefaultTest.copy(core = false)
    val flix = new Flix().setOptions(opts)

    // Add the given path.
    flix.addPath(path)

    // Compile and Evaluate the program to obtain the model.
    flix.solve() match {
      case Success(model, _) => runTests(model)
      case Failure(errors) =>
        // Create a single test that always fails.
        test("Aborted.") {
          for (e <- errors) {
            println(e.message.fmt(TerminalContext.AnsiTerminal))
          }
          fail(s"Unable to compile FlixTest for test suite: '$name'. Failed with: ${errors.length} errors.")
        }
    }
  }

  /**
    * Runs all tests in the given `model`.
    */
  private def runTests(model: Model): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = model.getTests.groupBy(_._1.namespace)

    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespace) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Evaluate each tests with a clue of its source location.
      for ((sym, defn) <- testsByName) {
        test(sym.name) {
          withClue(sym.loc.format) {
            // Evaluate the function.
            val result = defn()
            // Expect the true value, if boolean.
            if (result.isInstanceOf[java.lang.Boolean]) {
              assertResult(true)(result)
            }
          }
        }

      }
    }
  }

  init()

}
