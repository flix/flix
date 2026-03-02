package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class TestFormatter extends AnyFunSuite {
  /**
    * A list of program paths to test invariants on.
    * This list is the same list as in `TestCompletionProvider`.
    *
    * Note: files from large-examples and package-manager are not included in this list
    */
  private val ProgramPathList = List(
    "examples/concurrency-and-parallelism/spawning-threads.flix",
    "examples/concurrency-and-parallelism/using-par-yield.flix",
    "examples/concurrency-and-parallelism/using-par-yield-recursively.flix",
    "examples/concurrency-and-parallelism/using-select-with-default.flix",
    "examples/concurrency-and-parallelism/using-select.flix",
    "examples/concurrency-and-parallelism/using-channels-for-message-passing.flix",
    "examples/concurrency-and-parallelism/using-select-with-timeout.flix",
    "examples/effects-and-handlers/advanced/collatz.flix",
    "examples/effects-and-handlers/advanced/nqueens.flix",
    "examples/effects-and-handlers/advanced/backtracking.flix",
    "examples/effects-and-handlers/using-Random.flix",
    "examples/effects-and-handlers/using-HttpWithResult.flix",
    "examples/effects-and-handlers/using-ProcessWithResult.flix",
    "examples/effects-and-handlers/using-FileWriteWithResult.flix",
    "examples/effects-and-handlers/using-Console.flix",
    "examples/effects-and-handlers/using-Logger.flix",
    "examples/effects-and-handlers/running-multiple-effects.flix",
    "examples/effects-and-handlers/using-Clock.flix",
    "examples/datalog/compiler-puzzle.flix",
    "examples/datalog/railroad-network.flix",
    "examples/datalog/train-schedule.flix",
    "examples/functional-style/lists-and-list-processing.flix",
    "examples/functional-style/pure-and-impure-functions.flix",
    "examples/functional-style/mutual-recursion-with-full-tail-call-elimination.flix",
    "examples/functional-style/higher-order-functions.flix",
    "examples/functional-style/effect-polymorphic-functions.flix",
    "examples/functional-style/enums-and-parametric-polymorphism.flix",
    "examples/functional-style/function-composition-pipelines-and-currying.flix",
    "examples/functional-style/algebraic-data-types-and-pattern-matching.flix",
    "examples/imperative-style/copying-characters-into-array-with-foreach.flix",
    "examples/imperative-style/imperative-style-foreach-loops.flix",
    "examples/imperative-style/internal-mutability-with-regions.flix",
    "examples/imperative-style/iterating-over-lists-with-foreach.flix",
    "examples/modules/use-from-a-module-locally.flix",
    "examples/modules/declaring-a-module.flix",
    "examples/modules/use-from-a-module.flix",
    "examples/modules/companion-module-effect.flix",
    "examples/modules/companion-module-struct.flix",
    "examples/modules/companion-module-trait.flix",
    "examples/modules/companion-module-enum.flix",
    "examples/records/the-ast-typing-problem-with-polymorphic-records.flix",
    "examples/records/polymorphic-record-update.flix",
    "examples/records/polymorphic-record-extension-and-restriction.flix",
    "examples/records/record-construction-and-use.flix",
    "examples/structs/structs-and-parametric-polymorphism.flix",
    "examples/structs/struct-person.flix",
    "examples/structs/struct-tree-monadic.flix",
    "examples/structs/struct-tree.flix",
    "examples/traits/trait-with-higher-kinded-type.flix",
    "examples/traits/trait-with-associated-effect.flix",
    "examples/traits/deriving-traits-automatically.flix",
    "examples/traits/trait-with-associated-type.flix",
    "examples/traits/declaring-a-trait-with-instances.flix",
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
    val flix = new Flix().setOptions(Options.Default)
    flix.check()
    flix
  }

  /////////////////////////////////////////////////////////////////////////////
  // Parsability–Formattability Implication: When parsable, the formatter must be able to format the program without errors.
  /////////////////////////////////////////////////////////////////////////////
  test("Parsability–Formattability Implication: When parsable, the formatter must be able to format the program without errors.") {
    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxRoot = compileAndGetSyntaxTree(program, programPath)
      clean(programPath)
      Formatter.format(syntaxRoot, programPath)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Idempotence: formatting once must result in the same as formatting twice
  /////////////////////////////////////////////////////////////////////////////
  test("Idempotence: formatting once must result in the same as formatting twice.") {
    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxRoot = compileAndGetSyntaxTree(program, programPath)
      val formatTextEditsOnce = Formatter.format(syntaxRoot, programPath)
      val formattedStringOnce = Formatter.applyTextEditsToString(program, formatTextEditsOnce)

      // We must compile the formatted string to get the new syntax tree, which includes the formatting changes.
      // Especially, the positions in the String/File may have changed, which will affect the text edits produced by the second formatting pass.
      val syntaxRootAfterOnce = compileAndGetSyntaxTree(formattedStringOnce, programPath)

      val formatTextEditsTwice = Formatter.format(syntaxRootAfterOnce, programPath)
      val formattedStringTwice = Formatter.applyTextEditsToString(formattedStringOnce, formatTextEditsTwice)
      clean(programPath)
      assert(formattedStringOnce == formattedStringTwice, s"Formatter not idempotent for $programPath")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // AST Invariance: formatting must not change the semantics of the program
  /////////////////////////////////////////////////////////////////////////////
  test("AST Invariance: formatting must not change the semantics of the program.") {
    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTree = compileAndGetSyntaxTree(program, programPath)
      val formatTextEdits = Formatter.format(syntaxTree, programPath)
      val formattedProgram = Formatter.applyTextEditsToString(program, formatTextEdits)

      val syntaxTreeAfterFormatting = compileAndGetSyntaxTree(formattedProgram, programPath)
      clean(programPath)
      assert(computeSemanticHash(syntaxTree) == computeSemanticHash(syntaxTreeAfterFormatting), s"Formatter changed the semantics of the program for $programPath")
    }
  }

  /**
    * Successfully compiles the given input string `s` with the given compilation options `o`.
    *
    * The program must compile without any errors.
    *
    * @throws RuntimeException if the program cannot be compiled without errors.
    */
  private def compileAndGetSyntaxTree(program: String, programPath: String): SyntaxTree.Root = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    // We add the program as VirtualPath to the shared Flix instance,
    // if not the shared Flix instance will not be able to find the syntax tree corresponding to the program.
    sharedFlix.addVirtualPath(Paths.get(programPath), program)
    sharedFlix.check() match {
      case (_, Nil) => sharedFlix.getParsedAst
      case (optRoot, errors) =>
        fail(CompilationMessage.formatAll(errors)(NoFormatter, optRoot))
    }
  }

  private def compileAndGetTypedAstAndSyntaxTree(program: String, programPath: String): (Root, SyntaxTree.Root) = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    // We add the program as VirtualPath to the shared Flix instance,
    // if not the shared Flix instance will not be able to find the syntax tree corresponding to the program.
    sharedFlix.addVirtualPath(Paths.get(programPath), program)
    sharedFlix.check() match {
      case (Some(root), Nil) => (root, sharedFlix.getParsedAst)
      case (optRoot, errors) =>
        fail(CompilationMessage.formatAll(errors)(NoFormatter, optRoot))
    }
  }

  /**
    * Computes a semantic hash for the given SyntaxTree.Root.
    *
    * @param root the Root to hash.
    * @return the semantic hash as an Int.
    */
  private def computeSemanticHash(root: SyntaxTree.Root): Int = {
    import java.util.Objects

    Objects.hash(
      root.units.keySet,
      root.tokens.keySet
    )
  }

  /**
    * Removes the virtual path from the shared Flix instance to clean up after a test.
    *
    * @param path the path to remove from the shared Flix instance
    */
  private def clean(path: String): Unit = {
    val virtualPath = Paths.get(path)
    sharedFlix.remVirtualPath(virtualPath)
  }
}
