/*
 * Copyright 2025 Chenhao Gao
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

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.acceptors.FileAcceptor
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source, SymUse}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, Token, TypedAst}
import ca.uwaterloo.flix.language.phase.Lexer
import ca.uwaterloo.flix.util.Formatter.NoFormatter.code
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.util.Random

class TestCompletionProvider extends AnyFunSuite {

  /**
    * A list of program paths to test invariants on.
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
    "examples/fixpoints/railroad-network.flix",
    "examples/fixpoints/pipelines-of-fixpoint-computations.flix",
    "examples/fixpoints/compiler-puzzle.flix",
    "examples/fixpoints/polymorphic-first-class-constraints.flix",
    "examples/fixpoints/first-class-constraints-and-fixpoints.flix",
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
  private val Flix: Flix = {
    val flix = new Flix().setOptions(Options.Default)
    flix.check()
    flix
  }

  /**
    * The uri of the test source.
    *
    * Every test will use the same uri so that adding a new source with this uri will replace the old one.
    */
  private val Uri = "<test>"

  /**
    * A limit on the maximum number of inputs tested by each property.
    *
    * We use the limit to ensure that property tests terminate within a reasonable time.
    */
  private val Limit: Int = 100

  /////////////////////////////////////////////////////////////////////////////
  // General Properties                                                      //
  /////////////////////////////////////////////////////////////////////////////

  test("Autocomplete is well-defined") {
    forAll(Programs) { prg =>
        val root = compileWithSuccess(prg)
        forAll(allPositions(prg)) { pos =>
          autoComplete(pos, root)
        }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // General Properties                                                      //
  /////////////////////////////////////////////////////////////////////////////

  test("No completions after complete keyword") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      val keywordTokens = root.tokens(source).toList.filter(_.kind.isKeyword)
      keywordTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.getCompletions(Uri, pos, errors)(root, Flix).map(_.toCompletionItem(Flix))
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions after complete literal") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val literalTokens = root.tokens(source).toList.filter(_.kind.isLiteral)
      literalTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.getCompletions(Uri, pos, errors)(root, Flix).map(_.toCompletionItem(Flix))
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions inside comment") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val commentTokens = root.tokens(source).toList.filter(_.kind.isComment)
      commentTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.getCompletions(Uri, pos, errors)(root, Flix).map(_.toCompletionItem(Flix))
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions when defining the name for defs") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.defs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for enums") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.enums.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for sigs") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.sigs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for traits") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.traits.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for effects") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.effects.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for structs") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.structs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for type aliases") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.typeAliases.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No duplicated completions for defs") {
    // Exhaustively generate all tests.
    val tests = Programs.flatMap(program => {
      val (root1, _) = compile(program)
      val defSymUses = getDefSymUseOccurs()(root1).toList
      for {
        defSymUse <- defSymUses
        loc = mkLocForName(defSymUse)
        charsLeft <- listValidCharsLeft(defSymUse.sym.name, loc)
      } yield (program, defSymUse, loc, charsLeft)
    })

    // Randomly sample the generated tests.
    val samples = Random.shuffle(tests).take(Limit)

    // Run the selected tests.
    samples.foreach {
      case (program, defSymUse, loc, charsLeft) =>
        val alteredProgram = alterLocationInCode(program, loc, charsLeft)
        val triggerPosition = Position(loc.sp1.lineOneIndexed, loc.sp1.colOneIndexed + charsLeft)
        val (root, errors) = compile(alteredProgram)
        val completions = CompletionProvider.getCompletions(Uri, triggerPosition, errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertNoDuplicatedCompletions(completions, defSymUse.sym.toString, loc, program, charsLeft)
    }

  }

  test("No duplicated completions for vars") {
    // Exhaustively generate all tests.
    val tests = Programs.flatMap(program => {
      val (root1, _) = compile(program)
      val varOccurs = getVarSymOccurs()(root1)
      for {
        (varSym, loc0) <- varOccurs
        loc = mkLocForName(varSym, loc0)
        charsLeft <- listValidCharsLeft(varSym.text, loc)
      } yield (program, varSym, loc, charsLeft)
    })

    // Randomly sample the generated tests.
    val samples = Random.shuffle(tests).take(Limit)

    // Run the selected tests.
    samples.foreach {
      case (program, varSym, loc, charsLeft) =>
        val alteredProgram = alterLocationInCode(program, loc, charsLeft)
        val triggerPosition = Position(loc.sp1.lineOneIndexed, loc.sp1.colOneIndexed + charsLeft)
        val (root, errors) = compile(alteredProgram)
        val completions = CompletionProvider.getCompletions(Uri, triggerPosition, errors)(root, Flix).map(_.toCompletionItem(Flix))
        assertNoDuplicatedCompletions(completions, varSym.text, loc, program, charsLeft)
    }
  }

  /**
    * Returns all autocomplete suggestions at the given position `pos` for the given AST `root`.
    */
  private def autoComplete(pos: Position, root: Root): List[Completion] = CompletionProvider.getCompletions(Uri, pos, Nil)(root, Flix)

  // TODO: What should f return? Maybe Assertion = True | Cond? | Failed?
  def forAll[A, B](l: List[A])(f: A => B): Unit = l.foreach(f)

  /**
    * Asserts that the given completion list is empty at the given position.
    */
  private def assertEmpty(completions: List[CompletionItem], sourceLocation: SourceLocation, pos: Position): Unit = {
    if (completions.nonEmpty) {
      println(code(sourceLocation, s"Unexpected completions at $pos"))
      println(s"Found completions: ${completions.map(_.label)}")
      fail(s"Expected no completions at position $pos, but found ${completions.length} completions.")
    }
  }

  /**
    * Asserts that there are no duplicated completions in the given completion list.
    *
    * @param completions The CompletionItem list to check.
    * @param codeLoc     The source location of the code that we are changing.
    * @param codeText    The text of the code that we are changing.
    * @param program     The original program string.
    * @param charsLeft   The number of characters left after trimming the code text.
    */
  private def assertNoDuplicatedCompletions(completions: List[CompletionItem], codeText: String, codeLoc: SourceLocation, program: String, charsLeft: Int): Unit = {
    // Two completion items are identical if all the immediately visible fields are identical.
    completions.groupBy(item => (item.label, item.kind, item.labelDetails)).foreach {
      case (completion, duplicates) if duplicates.size > 1 =>
        println(s"Duplicated completions when selecting var \"$codeText\" at $codeLoc for program:\n$program")
        println(code(codeLoc, s"""Duplicated completion with label "${completion._1}" after trimming to "${codeText.take(charsLeft)}" here"""))
        fail("Duplicated completions")
      case _ => ()
    }
  }

  /**
    * The absolute character offset into the source, zero-indexed.
    */
  private def calcOffset(loc: SourcePosition): Int = {
    var offset = 0
    for (i <- 1 until loc.lineOneIndexed) {
      offset += loc.source.getLine(i).length + 1 // +1 for the newline
    }
    offset + loc.colOneIndexed - 1
  }

  /**
    * Trims the given program string after the given location `loc` by `n` characters.
    */
  private def alterLocationInCode(program: String, loc: SourceLocation, charsLeft: Int): String = {
    val target = program.substring(calcOffset(loc.sp1), calcOffset(loc.sp2))
    program.substring(0, calcOffset(loc.sp1)) + target.take(charsLeft) + program.substring(calcOffset(loc.sp2))
  }

  /**
    * Returns a new source location for the given DefSymUse that only contains the name of the symbol, namespace excluded.
    */
  private def mkLocForName(defSymUse: DefSymUse): SourceLocation = {
    val name = defSymUse.sym.name
    val sp2 = defSymUse.loc.sp2
    defSymUse.loc.copy(sp1 = SourcePosition.mkFromOneIndexed(sp2.source, sp2.lineOneIndexed, sp2.colOneIndexed - name.length))
  }

  /**
    * Returns a new source location for the given VarSym that only contains the name of the symbol, namespace excluded.
    */
  private def mkLocForName(varSym: Symbol.VarSym, loc0: SourceLocation): SourceLocation = {
    val name = varSym.text
    val sp2 = loc0.sp2
    loc0.copy(sp1 = SourcePosition.mkFromOneIndexed(sp2.source, sp2.lineOneIndexed, sp2.colOneIndexed - name.length))
  }

  /**
    * Returns a list of all valid numbers of characters left after trimming the given code text.
    *
    * A number of characters is valid if:
    * - The code text is not synthetic.
    * - The code text left is not a keyword.
    * - All trimmed characters are valid identifier characters.
    */
  private def listValidCharsLeft(codeText: String, codeLoc: SourceLocation): List[Int] = {
    (1 until codeText.length).collect {
      case splitPosition if {
        val (left, right) = codeText.splitAt(splitPosition)
        !codeLoc.isSynthetic && right.forall(isValidCharToTrim) && !isKeyword(left)
      } => splitPosition
    }.toList
  }

  /**
    * Returns `true` if the given character is a valid identifier character.
    */
  private def isValidCharToTrim(char: Char): Boolean = {
    char.isLetterOrDigit || char == '_' || char == '-'
  }

  /**
    * Returns `true` if the given code is a keyword.
    */
  private def isKeyword(code: String): Boolean = Lexer.lex(mkSource(code))._1.exists(_.kind.isKeyword)

  /**
    * Returns the set of variable symbols that occur in the given root.
    */
  private def getVarSymOccurs()(implicit root: Root): Set[(Symbol.VarSym, SourceLocation)] = {
    var occurs: Set[(Symbol.VarSym, SourceLocation)] = Set.empty

    object VarConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.Var(sym, _, loc) if sym.loc.isReal => occurs += ((sym, loc))
        case _ =>
      }
    }

    Visitor.visitRoot(root, VarConsumer, FileAcceptor(Uri))

    occurs
  }

  private def getDefSymUseOccurs()(implicit root: Root): Set[SymUse.DefSymUse] = {
    var occurs: Set[SymUse.DefSymUse] = Set.empty

    object DefSymUseConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.ApplyDef(symUse, _, _, _, _, _) => occurs += symUse
        case _ =>
      }
    }

    Visitor.visitRoot(root, DefSymUseConsumer, FileAcceptor(Uri))

    occurs
  }

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  private def compile(program: String): (Root, List[CompilationMessage]) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    Flix.addSourceCode(Uri, program)
    Flix.check() match {
      case (Some(root), errors) => (root, errors)
      case (None, _) => fail("Compilation failed: a root is expected.")
    }
  }

  /**
    * Successfully compiles the given input string `s` with the given compilation options `o`.
    *
    * The program must compile without any errors.
    *
    * @throws RuntimeException if the program cannot be compiled without errors.
    */
  private def compileWithSuccess(program: String): Root = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    Flix.addSourceCode(Uri, program)
    Flix.check() match {
      case (Some(root), Nil) => root
      case _ => fail("Compilation failed: a root is expected.")
    }
  }

  /**
    * Returns all positions within the given token.
    *
    * For example, give a token "def", we will return a list of positions:
    * - |def
    * - d|ef
    * - de|f
    * - def|
    *
    * If the token spans multiple lines, we will return all the positions in all the lines, both sides inclusive.
    */
  private def getAllPositionsWithinToken(token: Token): List[Position] = {
    val initialLine = token.sp1.lineOneIndexed
    val initialCol = token.sp1.colOneIndexed.toInt

    token.text
      .scanLeft((initialLine, initialCol)) { case ((line, col), char) =>
        if (char == '\n') (line + 1, 1)
        else (line, col + 1)
      }
      .map { case (line, col) => Position(line, col) }
      .toList
  }

  /**
    * Returns all valid positions in the given string `s`.
    */
  private def allPositions(p: String): List[Position] = {
    p.scanLeft(Position(1, 1)) {
      case (Position(line, _), '\n') => Position(line + 1, 1)
      case (Position(line, col), _) => Position(line, col + 1)
    }.toList
  }

  /**
    * Creates a source object from the given string `content`.
    */
  private def mkSource(content: String): Source = {
    val sctx = SecurityContext.AllPermissions
    val input = Input.Text(Uri, content, sctx)
    Source(input, content.toCharArray)
  }

}
