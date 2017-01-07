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
import org.scalatest.FunSuite

class FlixTest(name: String, path: String) extends FunSuite {

  override def suiteName: String = name

  /*
   * Include the standard library.
   */
  val Library: List[String] = List()

  {
    // Use the default Flix test options, but run in interpreted mode.
    val opts = Options.DefaultTest.copy(evaluation = Evaluation.Interpreted)
    val flix = new Flix().setOptions(opts)

    // Add the given path.
    flix.addPath(path)
    // ... and the library paths.
    for (path <- Library)
      flix.addPath(path)

    // Evaluate the program to obtain the model.
    val model = flix.solve().get

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
            // Expect the true value.
            assertResult(true)(result)
          }
        }

      }
    }
  }

}
