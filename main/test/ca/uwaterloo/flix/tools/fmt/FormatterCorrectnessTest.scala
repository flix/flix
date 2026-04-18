package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SyntaxTree.Tree
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class FormatterCorrectnessTest extends AnyFunSuite {

  /**
    * A list of program paths to test invariants on.
    *
    */
  private val ProgramPathList: List[String] = {
    val root = Paths.get("examples")
    if (!Files.exists(root)) {
      List.empty[String]
    } else {
      val stream = Files.walk(root)
      try {
        stream
          .iterator()
          .asScala
          .filter(Files.isRegularFile(_))
          .map(_.toString)
          .filter(_.endsWith(".flix"))
          .filterNot(p => p.contains("apps") || p.contains("package-manager"))
          .map(p => p.replace("\\", "/"))
          .toList
          .sorted
      } finally {
        stream.close()
      }
    }
  }

  /**
    * The contents of the programs in the list.
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

  private val CommentKinds: Set[TokenKind] = Set(
    TokenKind.CommentLine,
    TokenKind.CommentBlock,
    TokenKind.CommentDoc
  )

  /**
    * Collects all tokens from a syntax tree in source order.
    */
  private def collectTokens(tree: SyntaxTree.Tree): List[Token] = {
    tree.children.flatMap {
      case token: Token => List(token)
      case childTree: SyntaxTree.Tree => collectTokens(childTree)
    }.toList
  }

  /**
    * Extracts comment texts from a syntax tree in source order.
    */
  private def commentSequence(tree: SyntaxTree.Tree): List[String] = {
    collectTokens(tree)
      .filter(t => CommentKinds.contains(t.kind))
      .map(_.text)
  }

  /**
    * Finds the first line where two strings diverge.
    *
    * TODO: This is just for debugging to understand the mistakes the formatter does. Possible to be removed!
    */
  private def firstDivergence(a: String, b: String): String = {
    val linesA = a.linesIterator.toArray
    val linesB = b.linesIterator.toArray

    val minLen = math.min(linesA.length, linesB.length)
    var i = 0
    while (i < minLen && linesA(i) == linesB(i)) { i += 1 }

    if (i < minLen) {
      val context = (math.max(0, i - 2) until math.min(minLen, i + 3)).map { j =>
        val marker = if (j == i) ">>>" else "   "
        f"$marker L${j + 1}%4d| ${linesA(j)}"
      }.mkString("\n")

      val contextB = (math.max(0, i - 2) until math.min(linesB.length, i + 3)).map { j =>
        val marker = if (j == i) ">>>" else "   "
        f"$marker L${j + 1}%4d| ${linesB(j)}"
      }.mkString("\n")

      s"""First divergence at line ${i + 1}:
         |--- first pass ---
         |$context
         |--- second pass ---
         |$contextB""".stripMargin
    } else if (linesA.length != linesB.length) {
      s"Same content up to line $minLen, but different lengths: ${linesA.length} vs ${linesB.length} lines"
    } else {
      "No divergence found (strings are equal)"
    }
  }

  test("PrettyPrinter: formatting must not crash on any example.") {
    import ca.uwaterloo.flix.tools.fmt.PrettyPrinter

    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree = findTreeBasedOnUri(syntaxTreeRoot, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }
      resetSharedFlixInstance(programPath)

      val formattedText = PrettyPrinter.format(syntaxTree)
      assert(formattedText.nonEmpty, s"PrettyPrinter produced empty output for $programPath")
    }
  }

  test("PrettyPrinter: formatted output must still compile.") {
    import ca.uwaterloo.flix.tools.fmt.PrettyPrinter

    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree = findTreeBasedOnUri(syntaxTreeRoot, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }
      resetSharedFlixInstance(programPath)

      val formattedText = PrettyPrinter.format(syntaxTree)
      println(s"Testing compilation of formatted output for $programPath")
      println(s"Formatted output:\n$formattedText")

      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      sharedFlix.addVirtualPath(Paths.get(programPath), formattedText)
      sharedFlix.check() match {
        case (_, Nil) => ()
        case (optRoot, errors) =>
          val errorMsg = CompilationMessage.formatAll(errors)(NoFormatter, optRoot)
          resetSharedFlixInstance(programPath)
          fail(s"PrettyPrinter output does not compile for $programPath:\n$errorMsg")
      }
      resetSharedFlixInstance(programPath)
    }
  }

  // ---------------------------------------------------------------------------
  // Property 1: Idempotency = format(format(src)) == format(src)
  // ---------------------------------------------------------------------------

  test("PrettyPrinter: formatting must be idempotent.") {
    import ca.uwaterloo.flix.tools.fmt.PrettyPrinter

    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot1 = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree1 = findTreeBasedOnUri(syntaxTreeRoot1, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }
      resetSharedFlixInstance(programPath)
      val formattedOnce = PrettyPrinter.format(syntaxTree1)

      val syntaxTreeRoot2 = compileAndGetSyntaxTree(formattedOnce, programPath)
      val syntaxTree2 = findTreeBasedOnUri(syntaxTreeRoot2, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath after first formatting")
      }
      resetSharedFlixInstance(programPath)

      val formattedTwice = PrettyPrinter.format(syntaxTree2)

      assert(formattedOnce == formattedTwice,
        s"PrettyPrinter not idempotent for $programPath:\n${firstDivergence(formattedOnce, formattedTwice)}")
    }
  }

  // ---------------------------------------------------------------------------
  // Property 2: Nondestructive — token sequence is preserved
  //
  // TODO: Decicde if this is a propery as we sometimes add keywords for formatting and remove commas
  // Yes the test will fail for now...
  // ---------------------------------------------------------------------------

  /*
  test("PrettyPrinter: formatting must not change the token sequence.") {
    import ca.uwaterloo.flix.tools.fmt.PrettyPrinter

    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree = findTreeBasedOnUri(syntaxTreeRoot, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }

      val tokensBefore = semanticTokenSequence(syntaxTree)

      resetSharedFlixInstance(programPath)
      val formattedText = PrettyPrinter.format(syntaxTree)

      val syntaxTreeRoot2 = compileAndGetSyntaxTree(formattedText, programPath)
      val syntaxTree2 = findTreeBasedOnUri(syntaxTreeRoot2, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath after formatting")
      }
      resetSharedFlixInstance(programPath)

      val tokensAfter = semanticTokenSequence(syntaxTree2)

      assert(tokensBefore == tokensAfter,
        s"PrettyPrinter changed tokens for $programPath:\n${tokenDivergence(tokensBefore, tokensAfter)}")
    }
  }
  */
  // ---------------------------------------------------------------------------
  // Property 3: Comment order preservation
  // ---------------------------------------------------------------------------
  test("PrettyPrinter: formatting must preserve comment order.") {
    import ca.uwaterloo.flix.tools.fmt.PrettyPrinter

    for ((program, programPath) <- Programs.zip(ProgramPathList)) {
      val syntaxTreeRoot = compileAndGetSyntaxTree(program, programPath)
      val syntaxTree = findTreeBasedOnUri(syntaxTreeRoot, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath")
      }

      val commentsBefore = commentSequence(syntaxTree)

      resetSharedFlixInstance(programPath)
      val formattedText = PrettyPrinter.format(syntaxTree)

      val syntaxTreeRoot2 = compileAndGetSyntaxTree(formattedText, programPath)
      val syntaxTree2 = findTreeBasedOnUri(syntaxTreeRoot2, programPath).getOrElse {
        fail(s"Could not find syntax tree for $programPath after formatting")
      }
      resetSharedFlixInstance(programPath)

      val commentsAfter = commentSequence(syntaxTree2)

      assert(commentsBefore == commentsAfter,
        s"PrettyPrinter changed comment order for $programPath:\n" +
          s"  before: ${commentsBefore.length} comments\n" +
          s"  after:  ${commentsAfter.length} comments")
    }
  }

  private def compileAndGetSyntaxTree(program: String, programPath: String): SyntaxTree.Root = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    sharedFlix.addVirtualPath(Paths.get(programPath), program)
    sharedFlix.check() match {
      case (_, Nil) => sharedFlix.getParsedAst
      case (optRoot, errors) =>
        fail(CompilationMessage.formatAll(errors)(NoFormatter, optRoot))
    }
  }

  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    val normalizedUri = Paths.get(uri).normalize().toString
    root.units.find {
      case (path, _) => path.toString == normalizedUri
    }.map {
      case (_, tree) => tree
    }
  }

  @inline
  private def resetSharedFlixInstance(path: String): Unit = {
    val virtualPath = Paths.get(path)
    sharedFlix.remVirtualPath(virtualPath)
  }
}
