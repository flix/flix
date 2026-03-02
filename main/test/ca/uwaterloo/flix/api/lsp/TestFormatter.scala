package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class TestFormatter extends AnyFunSuite {
  /**
    * A list of program paths to test invariants on.
    *
    * Note: files from large-examples and package-manager are not included in this list
    */
  private val ProgramPathList = List(
    "examples/concurrency-and-parallelism/using-par-yield.flix",
  )

  /**
    * The contents of the programs in the list.
    *
    * We read the files from the disk and cache them in this list.
    */
  private val Programs: List[String] = ProgramPathList.map { programPath =>
    Files.readString(Paths.get(programPath))
  }

  /**
    * The Flix object used across all the tests.
    *
    * We first compile the stdlib so that every further compilation will be incremental.
    */
  private val sharedFlix: Flix = {
    val flix = new Flix().setOptions(Options.Default.copy(incremental = true))
    flix.check()
    flix
  }

  /////////////////////////////////////////////////////////////////////////////
  // Idempotence: formatting once must result in the same as formatting twice
  /////////////////////////////////////////////////////////////////////////////
  test("Idempotence: formatting once must result in the same as formatting twice.") {
    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxRoot = compileWithSuccess(program, programPath)
      val formatTextEditsOnce = Formatter.format(syntaxRoot, programPath)
      val formattedStringOnce = Formatter.applyTextEditsToString(program, formatTextEditsOnce)

      // We must compile the formatted string to get the new syntax tree!
      val syntaxRootAfterOnce = compileWithSuccess(formattedStringOnce, programPath)

      val formatTextEditsTwice = Formatter.format(syntaxRootAfterOnce, programPath)
      val formattedStringTwice = Formatter.applyTextEditsToString(formattedStringOnce, formatTextEditsTwice)
      assert(formattedStringOnce == formattedStringTwice, s"Formatter not idempotent for $programPath: got edits on second pass: $formatTextEditsTwice")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Invariance: formatting must not change the semantics of the program
  /////////////////////////////////////////////////////////////////////////////
  test("Invariance: formatting must not change the semantics of the program.") {
    throw new NotImplementedError("Invariance test not implemented yet.")
  }

  /**
    * Successfully compiles the given input string `s` with the given compilation options `o`.
    *
    * The program must compile without any errors.
    *
    * @throws RuntimeException if the program cannot be compiled without errors.
    */
  private def compileWithSuccess(program: String, programPath: String): SyntaxTree.Root = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    // Add the program under the same path string that the formatter will use to look it up.
    sharedFlix.addVirtualPath(Paths.get(programPath), program)
    sharedFlix.check() match {
      case (_, Nil) => sharedFlix.getParsedAst
      case (optRoot, errors) =>
        fail(CompilationMessage.formatAll(errors)(NoFormatter, optRoot))
    }
  }
}
