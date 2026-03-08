package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import java.util.Objects

class TestFormatter extends AnyFunSuite {
  /**
    * A list of program paths to test invariants on.
    * This list is the same list as in `TestCompletionProvider`.
    *
    * Note: files from large-examples and package-manager are not included in this list
    */
  private val ProgramPathList = List(
    // Small examples
    "examples/concurrency-and-parallelism/spawning-threads.flix",
    "examples/modules/use-from-a-module-locally.flix",
    "examples/records/the-ast-typing-problem-with-polymorphic-records.flix",
    "examples/structs/struct-person.flix",
    "examples/traits/trait-with-higher-kinded-type.flix",

    // Larger examples
    "examples/larger-examples/lambda-calculus.flix",
    "examples/larger-examples/program-analysis/IDE.flix",
    "examples/larger-examples/restrictable-variants/sequences.flix",

    // Functional style
    "examples/functional-style/algebraic-data-types-and-pattern-matching.flix",
    "examples/functional-style/enums-and-parametric-polymorphism.flix",
    "examples/functional-style/lists-and-list-processing.flix",
    "examples/functional-style/pure-and-impure-functions.flix",
    "examples/functional-style/mutual-recursion-with-full-tail-call-elimination.flix",
    "examples/functional-style/higher-order-functions.flix",
    "examples/functional-style/effect-polymorphic-functions.flix",
    "examples/functional-style/function-composition-pipelines-and-currying.flix",

    // Imperative style
    "examples/imperative-style/iterating-over-lists-with-foreach.flix",
    "examples/imperative-style/internal-mutability-with-regions.flix",
    "examples/imperative-style/copying-characters-into-array-with-foreach.flix",
    "examples/imperative-style/imperative-style-foreach-loops.flix",
    "examples/imperative-style/internal-mutability-with-regions.flix",
    "examples/imperative-style/iterating-over-lists-with-foreach.flix",

    // Declarative style
    "examples/larger-examples/datalog/connect-graph.flix",
    "examples/larger-examples/datalog/ford-fulkerson.flix",
    "examples/datalog/compiler-puzzle.flix",
    "examples/datalog/railroad-network.flix",
    "examples/datalog/train-schedule.flix",

    // Effects and handlers
    "examples/effects-and-handlers/advanced/backtracking.flix",
    "examples/effects-and-handlers/advanced/collatz.flix",
    "examples/effects-and-handlers/advanced/nqueens.flix",
    "examples/effects-and-handlers/using-Random.flix",
    "examples/effects-and-handlers/using-ProcessWithResult.flix",
    "examples/effects-and-handlers/using-FileWriteWithResult.flix",
    "examples/effects-and-handlers/using-Console.flix",
    "examples/effects-and-handlers/using-Logger.flix",
    "examples/effects-and-handlers/running-multiple-effects.flix",
    "examples/effects-and-handlers/using-Clock.flix",
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
      resetSharedFlixInstance(programPath)
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
      resetSharedFlixInstance(programPath)
      assert(formattedStringOnce == formattedStringTwice, s"Formatter not idempotent for $programPath")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // AST Invariance: formatting must not change the semantics of the program
  /////////////////////////////////////////////////////////////////////////////
  test("AST Invariance: formatting must not change the semantics of the program.") {
    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree = findTreeBasedOnUri(syntaxTreeRoot, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }
      val formatTextEdits = Formatter.format(syntaxTreeRoot, programPath)
      val formattedProgram = Formatter.applyTextEditsToString(program, formatTextEdits)

      val syntaxTreeRootAfterFormatting = compileAndGetSyntaxTree(formattedProgram, programPath)
      val syntaxTreeAfterFormatting = findTreeBasedOnUri(syntaxTreeRootAfterFormatting, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath after formatting")
      }
      resetSharedFlixInstance(programPath)
      val semanticHashBefore = computeSemanticHash(syntaxTree)
      val semanticHashAfter = computeSemanticHash(syntaxTreeAfterFormatting)
      assert(semanticHashBefore == semanticHashAfter, s"Formatter changed the semantics of the program for $programPath")
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

  /**
    * Computes a semantic hash for the given SyntaxTree.Tree.
    * The semantic hash is computed by traversing the syntax tree and hashing the relevant information.
    *
    * @param tree the Root to hash.
    * @return the semantic hash as an Int.
    */
  private def computeSemanticHash(tree: SyntaxTree.Tree): Int = {
    def traverseTree(tree: SyntaxTree.Tree): List[Int] = {
      val hashTokenKind = List(Objects.hash(tree.kind))

      val hashFromChild = tree.children.flatMap {
        case childTree: SyntaxTree.Tree => traverseTree(childTree)
        case _ => Nil
      }
      hashTokenKind ++ hashFromChild
    }
    traverseTree(tree).sum
  }

  /**
    * Finds the syntax tree corresponding to the given URI.
    *
    * @param root the syntax tree root
    * @param uri  the file path of the syntax tree
    * @return an option containing the syntax tree if found
    */
  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    root.units.find {
      case (path, _) => path.toString == uri
    }.map {
      case (_, tree) => tree
    }
  }

  /**
    * Resets the shared Flix instance by removing the virtual path corresponding to the given program path.
    * This is necessary to ensure that the shared Flix instance does not accumulate the virtual paths and has conflicts
    * between the different tests.
    *
    * @param path the path to remove from the shared Flix instance
    */
  @inline
  private def resetSharedFlixInstance(path: String): Unit = {
    val virtualPath = Paths.get(path)
    sharedFlix.remVirtualPath(virtualPath)
  }
}
