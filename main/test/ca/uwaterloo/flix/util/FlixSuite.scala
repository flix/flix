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

import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.{CompilationResult, TestFn}
import ca.uwaterloo.flix.verifier.{EffectVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Path, Paths}

class FlixSuite(incremental: Boolean) extends AnyFunSuite {

  /**
    * A global Flix instance that is used if incremental compilation is enabled.
    */
  var Flix: Flix = mkFlix()

  /**
    * Returns a new fresh Flix instance with default options.
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
    * Runs all tests in all files in the directory located at `path`.
    *
    * In contrast to [[mkTestDir]], this function compiles all files
    * together so files can depend on each other.
    *
    * Subdirectories are excluded.
    *
    */
  def mkTestDirCollected(path: String, name: String)(implicit options: Options): Unit = {
    val files = FileOps.getFlixFilesIn(Paths.get(path), 1)
    test(name)(compileAndRun(files))
  }

  /**
    * Runs all tests in all files in the directory located at `path`.
    *
    * This function compiles each file separately, so files cannot depend
    * each other. If that is a requirement use [[mkTestDirCollected]] instead.
    *
    * If `prelude` is specified, it is always included.
    *
    * Subdirectories are excluded.
    *
    */
  def mkTestDir(path: String, prelude: Option[String] = None)(implicit options: Options): Unit = {
    val files = FileOps.getFlixFilesIn(Paths.get(path), 1)
    for (p <- files) {
      mkTest(p.toString, prelude)
    }
  }

  /**
    * Runs all the tests in the file located at `path`.
    */
  def mkTest(path: String, prelude: Option[String])(implicit options: Options): Unit = {
    val p = Paths.get(path)
    val n = p.getFileName.toString
    val ps = prelude match {
      case None => List(p)
      case Some(p2) => List(p, Paths.get(p2))
    }
    test(n)(compileAndRun(ps))
  }

  private def compileAndRun(paths: List[Path])(implicit options: Options): Unit = {
    // Construct a new fresh Flix object if incremental compilation is disabled.
    if (!incremental) {
      Flix = mkFlix()
    }

    // Set options.
    Flix.setOptions(options)

    // Default security context.
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    // Add the given path.
    for (p <- paths) {
      Flix.addFile(p)
    }

    try {
      // Compile and Evaluate the program to obtain the compilationResult.
      Flix.compile().toResult match {
        case Result.Ok(compilationResult) =>
          runTests(compilationResult)
        case Result.Err(errors) =>
          val es = errors.map(_.messageWithLoc(Flix.getFormatter)).mkString("\n")
          fail(s"Unable to compile. Failed with: ${errors.length} errors.\n\n$es")
      }
    } finally {
      // Remove the source path.
      for (p <- paths) {
        Flix.remFile(p)
      }
    }
  }

  private def runTests(compilationResult: CompilationResult): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = compilationResult.getTests.groupBy(_._1.namespace)

    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespace) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Evaluate each tests with a clue of its source location.
      for ((sym, TestFn(_, skip, run)) <- testsByName) {
        if (!skip) {
          withClue(sym.loc.format) {
            // Evaluate the function.
            val result = run()
            // Expect the true value, if boolean.
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
