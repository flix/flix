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
    // Concurrency and parallelism
    "examples/concurrency-and-parallelism/using-select.flix",
    "examples/concurrency-and-parallelism/using-select-with-timeout.flix",

    // Datalog
    "examples/datalog/ford-fulkerson.flix",
    "examples/datalog/class-hierarchy-analysis.flix",
    "examples/datalog/single-source-shortest-paths.flix",

    // Effects and handlers
    "examples/effects-and-handlers/advanced/nqueens.flix",
    "examples/effects-and-handlers/http/middleware-rate-limiting.flix",
    "examples/effects-and-handlers/advanced/backtracking.flix",

    // Functional style
    "examples/functional-style/enums-and-parametric-polymorphism.flix",
    "examples/functional-style/pure-and-impure-functions.flix",

    // Imperative style
    "examples/imperative-style/imperative-style-foreach-loops.flix",
    "examples/imperative-style/copying-characters-into-array-with-foreach.flix",

    // Interoperability
    "examples/interoperability/swing/swing-dial.flix",
    "examples/interoperability/swing/swing-dialog.flix",

    // Modules
    "examples/modules/use-from-a-module-locally.flix",
    "examples/modules/use-from-a-module.flix",

    // Records
    "examples/records/the-ast-typing-problem-with-polymorphic-records.flix",

    // Structs
    "examples/structs/structs-and-parametric-polymorphism.flix",

    // Tail recursion and termination
    "examples/tail-recursion-and-termination/tail-recursion-with-accumulator.flix",

    // Traits
    "examples/traits/trait-with-associated-effect.flix",
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

  /**
    * Cached results for each program, shared across all 3 tests.
    *
    * Each program is compiled exactly twice (original + formatted).
    * The results are computed lazily on first access.
    */
  private case class FormatterTestResult(
    programPath: String,
    formattedStringOnce: String,
    formattedStringTwice: String,
    semanticHashBefore: Int,
    semanticHashAfter: Int,
  )

  private lazy val cachedResults: List[FormatterTestResult] = {
    Programs.zip(ProgramPathList).map { case (program, programPath) =>
      // Compilation 1: compile the original program
      val syntaxRoot = compileAndGetSyntaxTree(program, programPath)

      // Format the original (Parsability: this must not throw)
      val formatEditsOnce = Formatter.format(syntaxRoot, programPath)
      val formattedStringOnce = Formatter.applyTextEditsToString(program, formatEditsOnce)

      // AST Invariance: compute semantic hash before formatting
      val syntaxTree = findTreeBasedOnUri(syntaxRoot, programPath).getOrElse {
        throw new RuntimeException(s"Could not find syntax tree for $programPath")
      }
      val semanticHashBefore = computeSemanticHash(syntaxTree)

      // Compilation 2: compile the formatted string
      val syntaxRootAfterFormat = compileAndGetSyntaxTree(formattedStringOnce, programPath)

      // Format again (for Idempotence check)
      val formatEditsTwice = Formatter.format(syntaxRootAfterFormat, programPath)
      val formattedStringTwice = Formatter.applyTextEditsToString(formattedStringOnce, formatEditsTwice)

      // AST Invariance: compute semantic hash after formatting
      val syntaxTreeAfterFormatting = findTreeBasedOnUri(syntaxRootAfterFormat, programPath).getOrElse {
        throw new RuntimeException(s"Could not find syntax tree for $programPath after formatting")
      }
      val semanticHashAfter = computeSemanticHash(syntaxTreeAfterFormatting)

      resetSharedFlixInstance(programPath)

      FormatterTestResult(
        programPath = programPath,
        formattedStringOnce = formattedStringOnce,
        formattedStringTwice = formattedStringTwice,
        semanticHashBefore = semanticHashBefore,
        semanticHashAfter = semanticHashAfter,
      )
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Parsability–Formattability Implication: When parsable, the formatter must be able to format the program without errors.
  /////////////////////////////////////////////////////////////////////////////
  test("Parsability–Formattability Implication: When parsable, the formatter must be able to format the program without errors.") {
    // If any program failed to compile or format, cachedResults will throw on access.
    cachedResults
  }

  /////////////////////////////////////////////////////////////////////////////
  // Idempotence: formatting once must result in the same as formatting twice
  /////////////////////////////////////////////////////////////////////////////
  test("Idempotence: formatting once must result in the same as formatting twice.") {
    for (result <- cachedResults) {
      assert(result.formattedStringOnce == result.formattedStringTwice,
        s"Formatter not idempotent for ${result.programPath}")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // AST Invariance: formatting must not change the semantics of the program
  /////////////////////////////////////////////////////////////////////////////
  test("AST Invariance: formatting must not change the semantics of the program.") {
    for (result <- cachedResults) {
      assert(result.semanticHashBefore == result.semanticHashAfter,
        s"Formatter changed the semantics of the program for ${result.programPath}")
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
    val normalizedUri = Paths.get(uri).normalize().toString
    root.units.find {
      case (path, _) => path.toString == normalizedUri
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
