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
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Options, Result, Validation}
import org.scalatest.funsuite.AnyFunSuite

class TestFlixErrors extends AnyFunSuite with TestUtils {

  def expectRuntimeError(v: Validation[CompilationResult, CompilationMessage], name: String): Unit = {
    expectSuccess(v)
    v.toHardResult match {
      case Result.Ok(t) => t.getMain match {
        case Some(main) => try {
          main.apply(Array.empty)
          fail("No runtime error thrown")
        } catch {
          case e: java.lang.Throwable if e.getClass.getSimpleName == name =>
            ()
          case e: java.lang.Throwable => fail(e)
        }
        case None => fail("Could not find main")
      }
      case Result.Err(_) => fail("Impossible")
    }
  }

  test("HoleError.01") {
    val input = "def main(): Unit = ???"
    val result = compile(input, Options.TestWithLibMin)
    expectRuntimeError(result, BackendObjType.HoleError.jvmName.name)
  }

  test("HoleError.02") {
    val input = "def main(): Unit = ?namedHole"
    val result = compile(input, Options.TestWithLibMin)
    expectRuntimeError(result, BackendObjType.HoleError.jvmName.name)
  }

  test("SpawnedThreadError.01") {
    val input =
      """
        |def main(): Unit \ IO = region rc {
        |    spawn { bug!("Something bad happened") } @ rc;
        |    Thread.sleep(Time.Duration.fromSeconds(1))
        |}
      """.stripMargin
    val result = compile(input, Options.DefaultTestTest)
    expectRuntimeError(result, BackendObjType.HoleError.jvmName.name)
  }

  test("SpawnedThreadError.02") {
    val input =
      """
        |def main(): Unit \ IO = region rc {
        |    spawn {
        |        spawn { bug!("Something bad happened")  } @ rc
        |    } @ rc;
        |    Thread.sleep(Time.Duration.fromSeconds(1))
        |}
      """.stripMargin
    val result = compile(input, Options.DefaultTestTest)
    expectRuntimeError(result, BackendObjType.HoleError.jvmName.name)
  }

  test("SpawnedThreadError.03") {
    val input =
      """
        |def main(): Unit \ IO = region rc {
        |    spawn {
        |        spawn { String.concat(checked_cast(null), "foo") } @ rc
        |    } @ rc;
        |    Thread.sleep(Time.Duration.fromSeconds(1))
        |}
      """.stripMargin
    val result = compile(input, Options.DefaultTestTest)
    expectRuntimeError(result, "NullPointerException")
  }

  test("SpawnedThreadError.04") {
    val input =
      """
        |def main(): Unit \ IO = region rc {
        |    let (_tx, rx) = Channel.unbuffered(rc);
        |    spawn {
        |        spawn { String.concat(checked_cast(null), "foo") } @ rc
        |    } @ rc;
        |    discard Channel.recv(rx)
        |}
      """.stripMargin
    val result = compile(input, Options.DefaultTestTest)
    expectRuntimeError(result, "NullPointerException")
  }

}
