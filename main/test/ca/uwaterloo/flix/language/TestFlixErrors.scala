/*
 * Copyright 2022 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.jvm.classes.GenHoleError
import ca.uwaterloo.flix.runtime.{CompilationResult, JvmLoader}
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

class TestFlixErrors extends AnyFunSuite {

  private implicit val sctx: SecurityContext = SecurityContext.Unrestricted

  test("HoleError.01") {
    val input = "def main(): Unit = ???"
    val result = new Flix()
      .setOptions(Options.TestWithLibMin)
      .addVirtualPath(CompilerConstants.VirtualTestFile, input)
      .compile()
    expectRuntimeError(result, GenHoleError.Desc.displayName())
  }

  test("HoleError.02") {
    val input = "def main(): Unit = ?namedHole"
    val result = new Flix()
      .setOptions(Options.TestWithLibMin)
      .addVirtualPath(CompilerConstants.VirtualTestFile, input)
      .compile()
    expectRuntimeError(result, GenHoleError.Desc.displayName())
  }

  def expectRuntimeError(v: Result[CompilationResult, List[CompilationMessage]], name: String): Unit = {
    v match {
      case Result.Ok(t) => JvmLoader.load(t).main match {
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

}
