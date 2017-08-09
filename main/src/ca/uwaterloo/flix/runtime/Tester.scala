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
import ca.uwaterloo.flix.language.ast.{Source, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString.{Blue, Dedent, Green, Indent, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object Tester {

  /**
    * Represents the outcome of a single test.
    */
  sealed trait TestResult {
    def sym: Symbol.DefnSym
  }

  object TestResult {

    case class Success(sym: Symbol.DefnSym, msg: String) extends TestResult

    case class Failure(sym: Symbol.DefnSym, msg: String) extends TestResult

  }

  /**
    * Represents the results of running all the tests in a given model.
    */
  case class TestResults(results: List[TestResult]) {
    def getMessage: VirtualTerminal = {
      val vt = new VirtualTerminal()
      for ((ns, tests) <- results.groupBy(_.sym.namespace)) {
        val namespace = if (ns.isEmpty) "root" else ns.mkString("/")
        vt << Line("Tests", namespace)
        vt << Indent << NewLine
        for (test <- tests.sortBy(_.sym.loc)) {
          test match {
            case TestResult.Success(sym, msg) =>
              vt << Green("✓") << " " << sym.name << NewLine
            case TestResult.Failure(sym, msg) =>
              vt << Red("✗") << " " << sym.name << ": " << msg << " (" << Blue(sym.loc.format) << ")" << NewLine
          }
        }
        vt << Dedent << NewLine
      }
      vt
    }
  }

  /**
    * Evaluates all tests in the given model.
    *
    * Returns a pair of (successful, failed)-tests.
    */
  def test(model: Model): TestResults = {
    val results = model.getTests.toList.map {
      case (sym, defn) =>
        try {
          val result = defn()
          // NB: IntellijIDEA warns about unrelated types. This is not a problem.
          if (result.isInstanceOf[java.lang.Boolean] && result == java.lang.Boolean.TRUE)
            TestResult.Success(sym, "Returned true.")
          else if (result.isInstanceOf[java.lang.Boolean] && result == java.lang.Boolean.FALSE)
            TestResult.Failure(sym, "Returned false.")
          else
            TestResult.Success(sym, "Returned non-false boolean value.")
        } catch {
          case ex: FlixException =>
            TestResult.Failure(sym, ex.getMessage)
        }
    }
    TestResults(results)
  }

}
