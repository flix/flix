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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.FlixException
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.Highlight.{Blue, Green, Magenta, Red}
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import scala.collection.mutable

object Tester {

  /**
    * Evaluates all tests in the given model.
    *
    * Returns a pair of (successful, failed)-tests.
    */
  def test(model: Model): (List[Symbol.DefnSym], List[Symbol.DefnSym]) = {

    /*
     * Collect successful and failed tests.
     */
    val testSuccess = mutable.ListBuffer.empty[Symbol.DefnSym]
    val testFailure = mutable.ListBuffer.empty[Symbol.DefnSym]

    /*
      * Group tests by namespace.
      */
    val testsByNamespace = model.getTests.groupBy(_._1.namespace)

    /*
     * Iterate through each namespace and evaluate tests.
     */
    for ((ns, tests) <- testsByNamespace) {

      Console.println()
      if (ns.isEmpty) {
        Console.println(s"-- Unit Tests for ${Magenta("root")} -- ")
      } else {
        Console.println(s"-- Unit Tests for '${Magenta(ns.mkString("."))}' -- ")
      }

      /*
       * Sort tests by name.
       */
      val testsByName = tests.toList.sortBy(_._1.name)

      /*
       * Evaluate each test.
       */
      for ((sym, defn) <- testsByName) {

        // Evaluate the test function and catch any potential exception.
        val outcome = try {
          // NB: IntellijIDEA warns about unrelated types. This is not a problem.
          val result = defn()
          if (result == java.lang.Boolean.TRUE)
            Ok("Returned true.")
          else if (result == java.lang.Boolean.FALSE)
            Err("Returned false.")
          else
            Err(s"Returned non-boolean value: '${Value.pretty(result)}'.")
        } catch {
          case ex: FlixException => Err(ex.getMessage)
        }

        // Print test information.
        outcome match {
          case Ok(_) =>
            Console.println(s"  ${Green("✓")} ${sym.name}")
          case Err(msg) =>
            Console.println(s"  ${Red("✗")} ${Red(sym.name)}: $msg (at ${Blue(sym.loc.format)})")
        }
      }

    }

    // Returns the successful and failed tests.
    (testSuccess.toList, testFailure.toList)
  }

}
