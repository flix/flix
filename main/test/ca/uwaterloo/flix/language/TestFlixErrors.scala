/*
 * Copyright 2022 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

class TestFlixErrors extends FunSuite with TestUtils {

  test("HoleError.01") {
    val input = "def main(): Unit = ???"
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
    result match {
      case Validation.Success(t) => t.getMain match {
        case Some(main) => try {
          main.apply(Array.empty)
        } catch {
          case _: java.lang.Error => ()
        }
        case None => fail("Could not find main")
      }
      case Validation.Failure(_) => fail("Impossible")
    }
  }

  test("HoleError.02") {
    val input = "def main(): Unit = ?namedHole"
    val result = compile(input, Options.TestWithLibNix)
  }

}
