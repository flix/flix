/*
 * Copyright 2026 Din Jakupi
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
package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.api.{Flix, Library}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{SyntaxTree, WeededAst}
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.{FileOps, LibLevel, Options}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}

/**
  * Shared fixtures and helpers for the Flix code formatter test suites.
  *
  * The formatter is tested against several properties.
  * Letting
  *   p : S -> C  (parser)
  *   f : C -> S  (formatter)
  *   w : C -> A  (weeder)
  * where `S` is the set of Flix programs accepted by the parser, `C` the set
  * of concrete syntax trees [[SyntaxTree]], and `A` the set of abstract syntax
  * trees [[WeededAst]], the properties are:
  *
  *   1. Can format:          forall s in S,  f(p(s)) is defined
  *   2. Idempotency:         forall c in C,  f(p(f(c))) = f(c)
  *   3. Non-destructiveness: forall c in C,  w(c) = w(p(f(c)))
  *   4. Stability:           forall l in stdlib ++ examples.  f(p(l)) = l
  *
  * Properties 1-3 are *correctness* properties tested by [[TestFormatterCorrectness]].
  * Property 4 is an *aesthetic* property tested by [[TestFormatterStability]].
  *
  * This trait provides [[ExampleSamples]], [[StdlibSamples]] and the
  * parsing ([[reparseAt]]) shared by both tests.
  */
trait TestFormatterCommon extends AnyFunSuite {

  /**
    * The result of reparsing a source both the [[SyntaxTree.Tree]] and the
    * [[WeededAst.CompilationUnit]] for it.
    *
    * A single reparse yields both, so `Sample.reparse` can be one function
    * regardless of whether a test needs the syntax tree or the weeded unit.
    */
  protected case class Parsed(tree: SyntaxTree.Tree, weeded: WeededAst.CompilationUnit)

  /**
    * A sample program for testing
    *
    * @param path    the path to the sample file, used for error messages and as the virtual path in the Flix instance
    * @param content the original source code of the sample, used as the input for the first parse and for stability checks
    * @param reparse a function that takes a source string and returns the parsed SyntaxTree and WeededAst
    *                after substituting the source for the samples path in the Flix instance and running `check`.
    *                The function is responsible for restoring the Flix instance to its original state after parsing.
    */
  protected case class Sample(
    path: String,
    content: String,
    reparse: String => Parsed
  )

  /** All stdlib files */
  private val StdlibFiles: List[(String, String)] =
    Library.CoreLibrary ++ Library.StandardLibrary

  /** Flix instance used to compile example programs. The standard library is loaded by default. */
  private val exampleFlix: Flix = {
    val flix = new Flix().setOptions(Options.Default)
    flix.check()
    flix
  }

  /** Flix instance used to compile stdlib files. */
  private val stdlibFlix: Flix = {
    val flix = new Flix().setOptions(Options.Default.copy(lib = LibLevel.Nix))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    for ((name, content) <- StdlibFiles) {
      flix.addVirtualPath(Paths.get(name), content)
    }
    flix.check()
    flix
  }

  /**
    * Every `.flix` file under `examples/`, used as a corpus for all properties.
    * `apps` and `package-manager` and `datalog/train-schedule.flix` are excluded.
    */
  protected val ExampleSamples: List[Sample] =
    findFlixFiles(
      Paths.get("examples"),
      exclude = Set("apps", "package-manager", "datalog/train-schedule.flix")
    ).map { p =>
      val content = Files.readString(Paths.get(p))
      Sample(p, content, src => reparseAt(exampleFlix, p, src, restoreTo = None))
    }

  /**
    * Every `.flix` file in the standard library, used as a corpus for all properties.
    */
  protected val StdlibSamples: List[Sample] =
    StdlibFiles.map { case (p, content) =>
      Sample(p, content, src => reparseAt(stdlibFlix, p, src, restoreTo = Some(content)))
    }

  /**
    * Find all `.flix` files under `root` excluding the ones that are in the `exclude` set.
    *
    * Returns full paths (including `root`) as forward-slash strings, sorted
    * platform-independently. The `exclude` entries are matched as substrings.
    */
  private def findFlixFiles(root: Path, exclude: Set[String]): List[String] = {
    val files = FileOps.getFlixFilesIn(root, depth = Int.MaxValue)
    FileOps.sortPlatformIndependently(root, files)
      .map { case (path, _) => path.normalize().toString() }
      .filterNot(str => exclude.exists(str.contains))
  }

  /**
    * Substitutes `src` for `path` in `flix`, runs `check`, and returns the parsed syntax
    * tree for `path`. The instance is restored afterward via the
    * `finally` block:
    *   - `Some(orig)` re-binds the path to its original content (stdlib case).
    *   - `None` removes the path entirely (example case).
    *
    * Fails the test if `src` does not compile cleanly.
    */
  protected def reparseAt(
    flix: Flix,
    path: String,
    src: String,
    restoreTo: Option[String]
  ): Parsed = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    val vpath = Paths.get(path)
    flix.addVirtualPath(vpath, src)
    try {
      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        val msg = CompilationMessage.formatAll(errors)(NoFormatter, optRoot)
        fail(s"Failed to compile $path:\n$msg")
      }
      val tree = findTreeAt(flix.getParsedAst, path)
        .getOrElse(fail(s"No syntax tree found for $path"))
      val weeded = findWeededUnit(flix.getWeededAst, path)
        .getOrElse(fail(s"No weeded unit found for $path"))
      Parsed(tree, weeded)
    } finally {
      restoreTo match {
        case Some(orig) =>
          flix.addVirtualPath(vpath, orig)
          flix.check()
        case None =>
          flix.remVirtualPath(vpath)
      }
    }
  }

  /**
    * Finds the syntaxTree for the given URI in the root if it exists.
    *
    * @param root the syntax tree root to search
    * @param uri  the file URI to find the syntax tree for
    * @return an option containing the syntax tree if found, or None if not found
    */
  private def findTreeAt(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    val normalized = Paths.get(uri).normalize().toString
    root.units.collectFirst { case (p, t) if p.toString == normalized => t }
  }

  /**
    * Finds the weeded compilation unit for the given URI in the root if it exists.
    *
    * @param root the weeded AST root to search
    * @param uri  the file URI to find the weeded compilation unit for
    * @return an option containing the weeded compilation unit if found, or None if not found
    */
  private def findWeededUnit(root: WeededAst.Root, uri: String): Option[WeededAst.CompilationUnit] = {
    val normalized = Paths.get(uri).normalize().toString
    root.units.collectFirst { case (s, u) if s.toString == normalized => u }
  }

  /**
    * Formats `tree`, failing the test if the formatter refuses to.
    * The formatter only refuses when the tree contains a [[SyntaxTree.TreeKind.ErrorTree]].
    */
  protected def formatOrFail(tree: SyntaxTree.Tree): String =
    PrettyPrinter.format(tree).getOrElse(fail("Formatter refused to format a valid syntax tree"))

  /**
    * Finds the first line where two strings diverge and reports surrounding context.
    * This helps debugging idempotency and stability failures by showing the first point the outputs differ.
    */
  protected def firstDivergence(a: String, b: String): String = {
    val linesA = a.linesIterator.toArray
    val linesB = b.linesIterator.toArray
    val minLen = math.min(linesA.length, linesB.length)

    var i = 0
    while (i < minLen && linesA(i) == linesB(i)) i += 1

    if (i < minLen) {
      val contextFirstPass = (math.max(0, i - 2) until math.min(minLen, i + 3)).map { j =>
        val marker = if (j == i) ">>>" else "   "
        f"$marker L${j + 1}%4d| ${linesA(j)}"
      }.mkString("\n")
      val contextSecondPass = (math.max(0, i - 2) until math.min(linesB.length, i + 3)).map { j =>
        val marker = if (j == i) ">>>" else "   "
        f"$marker L${j + 1}%4d| ${linesB(j)}"
      }.mkString("\n")
      s"""First divergence at line ${i + 1}:
         |--- first pass ---
         |$contextFirstPass
         |--- second pass ---
         |$contextSecondPass""".stripMargin
    } else if (linesA.length != linesB.length) {
      s"Same content up to line $minLen, but different lengths: ${linesA.length} vs ${linesB.length} lines"
    } else {
      "No divergence found (strings are equal)"
    }
  }
}
