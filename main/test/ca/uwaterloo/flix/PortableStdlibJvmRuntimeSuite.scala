/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.{CompilationResult, TestFn}
import ca.uwaterloo.flix.util.{FileOps, Options, Result, StdlibProfile}
import ca.uwaterloo.flix.verifier.{EffectVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

/**
  * End-to-end runtime smoke tests for the portable conformance suite on the JVM backend.
  *
  * This ensures portable programs both compile and run correctly with `StdlibProfile.Portable`,
  * and provides a JVM baseline to keep LLVM-native behavior aligned.
  */
class PortableStdlibJvmRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")
  private val testDir = Paths.get("main/test/flix/portable")

  test("portable-stdlib-jvm-runtime") {
    val flix = mkFlix()
    flix.setOptions(TestOptions)
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    flix.addFile(preludeFile)
    for (p <- FileOps.getFlixFilesIn(testDir, 1)) flix.addFile(p)

    flix.compile().toResult match {
      case Result.Ok(compilationResult) =>
        runTests(compilationResult)
      case Result.Err(errors) =>
        fail(CompilationMessage.formatAll(errors.toList)(flix.getFormatter, None))
    }
  }

  /**
    * Returns a fresh Flix instance wired with the standard verifier hooks.
    */
  private def mkFlix(): Flix = {
    val flix = new Flix()

    flix.addListener {
      case FlixEvent.AfterTailPos(root) =>
        TypeVerifier.verify(root)(flix)
      case _ => // nop
    }

    flix.addListener {
      case FlixEvent.AfterTyper(root) =>
        EffectVerifier.verify(root)(flix)
      case _ => // nop
    }

    flix
  }

  /**
    * Runs all `@Test` functions in the compilation result.
    */
  private def runTests(compilationResult: CompilationResult): Unit = {
    val testsByNamespace = compilationResult.getTests.groupBy(_._1.namespace)

    for ((_, tests) <- testsByNamespace) {
      val testsByName = tests.toList.sortBy(_._1.name)
      for ((sym, TestFn(_, skip, run)) <- testsByName) {
        if (!skip) {
          withClue(sym.loc.format) {
            val result = run()
            result match {
              case res: java.lang.Boolean =>
                if (!res.booleanValue()) {
                  fail("Expected true, but got false.")
                }
              case _ => // nop
            }
          }
        }
      }
    }
  }
}

