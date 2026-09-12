/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.{JvmLoader, LoadedProgram, TestFn}
import ca.uwaterloo.flix.util.{FileOps, Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class StandardLibrarySuite extends AnyFunSuite {

  /** The path to the library tests. */
  private val Path = "main/test/ca/uwaterloo/flix/library/"

  /** The default options. */
  private val Opts = Options.DefaultTest.copy(incremental = false)

  private def init(): Unit = try {
    // Create a new Flix compiler.
    val flix = new Flix
    flix.setOptions(Opts)

    // Find and add all test suites.
    val paths = FileOps.getFlixFilesIn(Paths.get(Path), 1)
    for (p <- paths) {
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      flix.addFile(p)
    }

    // Compile the program with all test suites.
    flix.compile() match {
      case Result.Ok(compilationResult) =>
        runTests(JvmLoader.load(compilationResult))
      case Result.Err(errors) =>
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, None))
    }
  } catch {
    case ex: Throwable =>
      // We create a fictitious test to ensure that something shows up.
      test("StandardLibrary -- COMPILATION FAILED.") {
        ex.printStackTrace()
        fail(ex)
      }
  }

  private def runTests(program: LoadedProgram): Unit = {
    // Group the tests by namespace.
    val testsByNamespace = program.tests.groupBy {
      case (sym, _) => sym.namespace
    }

    // Sort the namespaces.
    val testsByNamespaceSorted = testsByNamespace.toList.sortBy {
      case (ns, _) => ns.mkString(".")
    }

    // Iterate through each namespace.
    for ((_, tests) <- testsByNamespaceSorted) {
      // Sort the tests by name.
      val testsByName = tests.toList.sortBy(_._1.name)

      // Dynamically create a ScalaTest unit test for each @Test function.
      for ((sym, TestFn(_, skip, run)) <- testsByName) {
        val testName = sym.toString
        if (skip){
          ignore(testName) {}
        } else {
          test(testName) {
            run()
          }
        }
      }
    }
  }

  init()
}
