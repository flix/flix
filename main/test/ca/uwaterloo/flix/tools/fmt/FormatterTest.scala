package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.{Flix, Library}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{SyntaxTree, WeededAst}
import ca.uwaterloo.flix.tools.fmt.PrettyPrinter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.{LibLevel, Options}
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
  * Correctness tests for the Flix code formatter.
  *
  * The correctness properties checked here are part of the canonical set
  * required of a source code formatter.
  *
  * Letting
  *   p : S -> C  (parser)
  *   f : C -> S  (formatter)
  *   w : C -> A  (weeder)
  * where `S` is the set of Flix programs accepted by the parser, `C` the set
  * of concrete syntax trees [[SyntaxTree]], and `A` the set of abstract syntax
  * trees [[WeededAst]], the universal correctness properties are:
  *
  *   1. Can format:          forall s in S,  f(p(s)) is defined
  *   2. Idempotency:         forall c in C,  f(p(f(c))) = f(c)
  *   3. Non-destructiveness: forall c in C,  w(c) = w(p(f(c)))
  *
  * Beyond the correctness properties, the suite also checks one *aesthetic* property.
  *
  *   4. Stability:   forall l in stdlib ++ examples.  f(p(l)) = l
  *
  * The standard library and examples are maintained in a formatted form
  * and are expected to be the canonical style. This property ensures that the formatter preserves the formatting rules.
  * Files that are not yet formatted may be listed in [[KnownUnstable]] as exceptions.
  */
@Ignore
class FormatterTest extends AnyFunSuite {

  // ---------------------------------------------------------------------------
  // Sample.
  //
  // A program in the test corpus, paired with a function that re-parses an
  // alternative source under the same path. The `reparse` function is responsible
  // for restoring the host Flix instance afterward. Stdlib and example samples
  // differ only in what their `reparse` does; the tests don't care which kind
  // they're iterating over.
  // ---------------------------------------------------------------------------

  /**
    * A reparse either yields a SyntaxTree or WeededAst for the given source.
    * This union type is used to define the `reparse` function in `Sample` as a single function that can be used for both
    * the SyntaxTree and WeededAst.
    */
  private case class Parsed(tree: SyntaxTree.Tree, weeded: WeededAst.CompilationUnit)

  /**
    * A sample program for testing
    *
    * @param path the path to the sample file, used for error messages and as the virtual path in the Flix instance
    * @param content the original source code of the sample, used as the input for the first parse and for stability checks
    * @param reparse a function that takes a source string and returns the parsed SyntaxTree and WeededAst
    *                after substituting the source for the samples path in the Flix instance and running `check`.
    *                The function is responsible for restoring the Flix instance to its original state after parsing.
    */
  private case class Sample(
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
    * Every `.flix` file under `examples/`, used as a corpus for all three properties.
    * `apps` and `package-manager` and `datalog/train-schedule.flix` are excluded.
    */
  private val ExampleSamples: List[Sample] =
    findFlixFiles(
      Paths.get("examples"),
      exclude = Set("apps", "package-manager", "datalog/train-schedule.flix")
    ).map { p =>
      val content = Files.readString(Paths.get(p))
      Sample(p, content, src => reparseAt(exampleFlix, p, src, restoreTo = None))
    }

  private val StdlibSamples: List[Sample] =
    StdlibFiles.map { case (p, content) =>
      Sample(p, content, src => reparseAt(stdlibFlix, p, src, restoreTo = Some(content)))
    }

  /**
    * Returns every `.flix` file under `root`.
    * Returns the empty list if `root` does not exist.
    */
  private def findFlixFiles(root: Path, exclude: Set[String]): List[String] = {
    if (!Files.exists(root)) Nil
    else {
      val stream = Files.walk(root)
      try stream.iterator().asScala
        .filter(Files.isRegularFile(_))
        .map(_.toString.replace("\\", "/"))
        .filter(_.endsWith(".flix"))
        .filterNot(p => exclude.exists(p.contains))
        .toList.sorted
      finally stream.close()
    }
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
  private def reparseAt(
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
    * Property 1 -- Can format: `forall s in S, f(p(s)) is defined`.
    *
    * The formatter produces a non-empty string for every [[SyntaxTree]] tree the parser produces.
    */
  private def checkCanFormat(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val tree = sample.reparse(sample.content).tree
      val formatted = PrettyPrinter.format(tree)
      assert(formatted.nonEmpty, s"Formatter produced empty output for ${sample.path}")
    }
  }

  /**
    * Property 2 -- Idempotency: `forall c in C, f(p(f(c))) = f(c)`.
    *
    * Applying the formatter to its own output yields the same output.
    * The inner `p` accounts for the fact that the formatter produces a string.
    */
  private def checkIdempotency(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val tree1 = sample.reparse(sample.content).tree
      val once = PrettyPrinter.format(tree1)
      val tree2 = sample.reparse(once).tree
      val twice = PrettyPrinter.format(tree2)
      assert(once == twice,
        s"Formatter is not idempotent for ${sample.path}:\n${firstDivergence(once, twice)}")
    }
  }

  /**
    * Property 3 -- Non-destructiveness: `forall c in C. shape(w(c)) = shape(w(p(f(c))))`.
    *
    * The formatter must not change the shape of the [[WeededAst]].
    * This is  checked by comparing the weeded compilation unit of the original source
    * against that of the reparsed formatted output, at the kind level only.
    */
  private def checkNonDestructive(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val before = sample.reparse(sample.content)
      val formatted = PrettyPrinter.format(before.tree)
      val after = sample.reparse(formatted)
      assert(sameShape(before.weeded, after.weeded),
        s"Formatter changed the AST shape for ${sample.path}")
    }
  }


  /**
    * Property 3 -- Stability: `forall l in stdlib ++ examples. f(p(l)) = l`.
    *
    * Aesthetic both the
    * standard library and the bundled `examples` programs are kept in formatted
    * form, so formatting any of those files must reproduce its exact source.
    */
  private def checkStability(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val formatted = PrettyPrinter.format(sample.reparse(sample.content).tree)
      val isFixedPoint = formatted == sample.content
      assert(isFixedPoint,
        s"Standard library is not preserved by the formatter (f(p(l)) != l) " +
        s"for ${sample.path}:\n${firstDivergence(sample.content, formatted)}")
    }
  }

  // All three properties run on both test corpus, standard library and examples.

  test("PrettyPrinter: can format (examples)") {
    checkCanFormat(ExampleSamples)
  }
  test("PrettyPrinter: can format (stdlib)") {
    checkCanFormat(StdlibSamples)
  }

  test("PrettyPrinter: idempotency (examples)") {
    checkIdempotency(ExampleSamples)
  }
  test("PrettyPrinter: idempotency (stdlib)") {
    checkIdempotency(StdlibSamples)
  }

  test("PrettyPrinter: non-destructiveness (examples)") {
    checkNonDestructive(ExampleSamples)
  }
  test("PrettyPrinter: non-destructiveness (stdlib)") {
    checkNonDestructive(StdlibSamples)
  }

  test("PrettyPrinter: stability (examples)") {
    checkStability(ExampleSamples)
  }
  test("PrettyPrinter: stability (stdlib)") {
    checkStability(StdlibSamples)
  }

  /**
    * Finds the syntaxTree for the given URI in the root if it exists.
    *
    * @param root the syntax tree root to search
    * @param uri the file URI to find the syntax tree for
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
    * @param uri the file URI to find the weeded compilation unit for
    * @return an option containing the weeded compilation unit if found, or None if not found
    */
  private def findWeededUnit(root: WeededAst.Root, uri: String): Option[WeededAst.CompilationUnit] = {
    val normalized = Paths.get(uri).normalize().toString
    root.units.collectFirst { case (s, u) if s.toString == normalized => u }
  }

  /**
    * Checks if two [[WeededAst]] have the same shape.
    * Meaning that they have the same structure of nodes, but not necessarily the same content.
    * This is used to check the non-destructiveness property of the formatter.
    *
    * TODO: This is a very simple check, therefore, find a more robust way to check for the AST integrity.
    */
  private def sameShape(a: Any, b: Any): Boolean = (a, b) match {
    case (x: Iterable[_], y: Iterable[_]) =>
      // Two collections have the same shape if they have the same length
      x.size == y.size && x.iterator.zip(y.iterator).forall { case (p, q) => sameShape(p, q) }
    case (x: Product, y: Product) =>
      // Two case classes have the same shape if they have the same string prefix.
      x.productPrefix == y.productPrefix &&
        x.productIterator.zip(y.productIterator).forall { case (p, q) => sameShape(p, q) }
    case _ => true
  }

  /**
    * Finds the first line where two strings diverge and reports surrounding context.
    * This helps debugging idempotency failures by showing the first point the formatters output differ.
    */
  private def firstDivergence(a: String, b: String): String = {
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
