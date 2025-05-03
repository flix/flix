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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, Token, TypedAst}
import ca.uwaterloo.flix.language.phase.Lexer
import ca.uwaterloo.flix.util.Formatter.NoFormatter.code
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

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
    "examples/interoperability/calling-methods/calling-java-varargs-methods.flix",
    "examples/interoperability/calling-methods/calling-java-static-methods.flix",
    "examples/interoperability/anonymous-classes/implementing-java-closeable.flix",
    "examples/interoperability/anonymous-classes/implementing-java-runnable.flix",
    "examples/interoperability/swing/swing-dial.flix",
    "examples/interoperability/swing/simple-swing-app.flix",
    "examples/interoperability/swing/swing-dialog.flix",
    "examples/interoperability/exceptions/catching-java-exceptions.flix",
    "examples/interoperability/files/reading-a-file-with-java.flix",
    "examples/interoperability/files/writing-a-file-with-java.flix",
    "examples/interoperability/files/checking-if-file-exists-with-java.flix",
    "examples/misc/type-level-programming/track-list-emptiness-with-type-level-booleans.flix",
    "examples/misc/type-level-programming/type-level-programming-string-sanitization.flix",
    "examples/misc/type-level-programming/type-level-programming-4bit-adder.flix",
    "examples/misc/type-level-programming/type-level-programming-demorgan.flix",
    "examples/misc/type-level-programming/type-level-programming-even-odd-list.flix",
    "examples/misc/type-level-programming/type-level-programming-eager-lazy-list.flix",
    "examples/misc/type-aliases.flix",
    "examples/misc/named-arguments.flix",
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

  test("No crashes when calling autoComplete anywhere") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      program.scanLeft(Position(1, 1))({
        case (Position(line, _), '\n') => Position(line + 1, 1)
        case (Position(line, col), _) => Position(line, col + 1)
      }).foreach(pos => CompletionProvider.autoComplete(Uri, pos, errors)(root, Flix))
    })
  }

  test("No completions after complete keyword") {
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      val keywordTokens = root.tokens(source).toList.filter(_.kind.isKeyword)
      keywordTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, Flix)
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions after complete literal") {
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val literalTokens = root.tokens(source).toList.filter(_.kind.isLiteral)
      literalTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, Flix)
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions inside comment") {
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val commentTokens = root.tokens(source).toList.filter(_.kind.isComment)
      commentTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, Flix)
          assertEmpty(completions, token.mkSourceLocation(), pos)
        }
      }
    })
  }

  test("No completions when defining the name for defs"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.defs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for enums"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.enums.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for sigs"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.sigs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for traits"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.traits.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for effects"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.effects.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for structs"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.structs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for type aliases"){
    Programs.foreach( program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.typeAliases.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, Flix)
        assertEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  /**
    * Asserts that the given completion list is empty at the given position.
    */
  private def assertEmpty(completions: CompletionList, sourceLocation: SourceLocation, pos: Position): Unit = {
    if (completions.items.nonEmpty) {
      println(code(sourceLocation, s"Unexpected completions at $pos"))
      println(s"Found completions: ${completions.items.map(_.label)}")
      fail(s"Expected no completions at position $pos, but found ${completions.items.toList.length} completions.")
    }
  }


  test("No duplicated completions") {
    val charToTrim = 1
    Programs.foreach( program => {
      val (root1, _) = compile(program)
      val varOccurs = getVarSymOccurs()(root1)
      varOccurs.foreach{
        case varOccur if isValidVar(varOccur, charToTrim) =>
          val alteredProgram = trimAfter(program, varOccur.loc, charToTrim)
          val triggerPosition = Position(varOccur.loc.sp2.lineOneIndexed, varOccur.loc.sp2.colOneIndexed - charToTrim)
          val (root, errors) = compile(alteredProgram)
          val completions = CompletionProvider.autoComplete(Uri, triggerPosition, errors)(root, Flix)
          assertNoDuplicatedCompletions(completions, varOccur, program, charToTrim)
        case _ => ()
      }
    })
  }

  /**
    * Asserts that there are no duplicated completions in the given completion list.
    *
    * @param completions The completion list to check.
    * @param varOccur    The variable occurrence where we are checking for completions.
    * @param program     The original program string.
    */
  private def assertNoDuplicatedCompletions(completions: CompletionList, varOccur: Symbol.VarSym, program: String, charToTrim: Int): Unit = {
    // Two completion items are identical if all the immediately visible fields are identical.
    completions.items.groupBy(item => (item.label, item.kind, item.labelDetails)).foreach {
      case (completion, duplicates) if duplicates.size > 1 =>
        println(s"Duplicated completions when selecting var \"${varOccur.text}\" at ${varOccur.loc} for program:\n $program")
        println(code(varOccur.loc, s"""Duplicated completion with label "${completion._1}" after deleting $charToTrim char here"""))
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
  private def trimAfter(program: String, loc: SourceLocation, n: Int): String = {
    val target = program.substring(calcOffset(loc.sp1), calcOffset(loc.sp2))
    program.substring(0, calcOffset(loc.sp1)) + target.dropRight(n) + program.substring(calcOffset(loc.sp2))
  }

  /**
    * A Var is valid for the test if:
    * - It's not empty after trimming
    * - It's not synthetic
    * - It's not a keyword after trimming
    */
  private def isValidVar(varOccur: Symbol.VarSym, charToTrim: Int) = {
    val text = varOccur.text
    val trimmedText = text.dropRight(charToTrim)
    text.length > charToTrim && !varOccur.loc.isSynthetic && !isKeyword(trimmedText)
  }

  /**
    * Returns `true` if the given code is a keyword.
    */
  private def isKeyword(code: String): Boolean = Lexer.lex(mkSource(code))._1.exists(_.kind.isKeyword)

  /**
    * Returns the set of variable symbols that occur in the given root.
    */
  private def getVarSymOccurs()(implicit root: Root): Set[Symbol.VarSym] = {
    var occurs: Set[Symbol.VarSym] = Set.empty

    object VarConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
          case TypedAst.Expr.Var(sym, _, _) => occurs += sym
          case _ =>
        }
    }

    Visitor.visitRoot(root, VarConsumer, FileAcceptor(Uri))

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
    * Creates a source object from the given string `content`.
    */
  private def mkSource(content: String): Source = {
    val sctx = SecurityContext.AllPermissions
    val input = Input.Text(Uri, content, sctx)
    Source(input, content.toCharArray)
  }
}
