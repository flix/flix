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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Flix, RuleException}
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTutorials extends FunSuite {

  val opts = Options.DefaultTest

  test("introduction.flix") {
    val path = "main/src/tutorials/introduction.flix"
    new Flix().setOptions(opts.copy(core = false)).addPath(path).solve().get
  }

  test("interpreter.flix") {
    val path = "main/src/tutorials/interpreter.flix"
    new Flix().setOptions(opts.copy(core = false)).addPath(path).solve().get
  }

  test("lambda-calculus.flix") {
    val path = "main/src/tutorials/lambda-calculus.flix"
    new Flix().setOptions(opts.copy(core = false)).addPath(path).solve().get
  }

  test("delta-debugging.flix") {
    intercept[RuleException] {
      val path = "main/src/tutorials/delta-debugging.flix"
      new Flix().setOptions(opts.copy(core = false)).addPath(path).solve().get
    }
  }

}
