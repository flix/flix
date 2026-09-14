/*
 * Copyright 2022 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.runtime.JvmLoader
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

class TestProgramArgs extends AnyFunSuite {

  private implicit val sctx: SecurityContext = SecurityContext.Unrestricted

  test("ProgramArgs.01") {
    val arg = "Correct"
    val input =
      s"""
         |use Sys.Env
         |def main(): Unit \\ Env = match Env.getArgs() {
         |    case "$arg" :: Nil => ()
         |    case _ :: Nil => ?wrongArgumentValue
         |    case _ => ?incorrectNumberOfArgs
         |}
      """.stripMargin

    val result = new Flix()
      .setOptions(Options.TestWithLibAll)
      .addVirtualPath(CompilerConstants.VirtualTestFile, input)
      .compile()
    result match {
      case Result.Ok(r) => JvmLoader.load(r).main match {
        case Some(main) => try {
          main.apply(Array(arg))
        } catch {
          case e: java.lang.Throwable => fail(e)
        }
        case None => fail("No entrypoint")
      }
      case Result.Err(errors) =>
        val actuals = errors.map(_.getClass)
        fail(s"Expected success, but found errors ${actuals.mkString(", ")}.")
    }

  }

}
