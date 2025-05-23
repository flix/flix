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
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, Token, TokenKind, TypedAst}
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
  // General Properties
  /////////////////////////////////////////////////////////////////////////////

  test("Autocomplete is well-defined") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(allPositions(prg)) { pos =>
        Assert.cond(autoComplete(pos, root).length >= 0)
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // No Completions: Comments
  /////////////////////////////////////////////////////////////////////////////

  test("NoCompletions.inBlockComments") {
    // Note: Block comments are exclusive.
    forAll(Programs)(prg => {
      val root = compileWithSuccess(prg)
      forAll(blockCommentsOf(prg, root)) { tok =>
        forAll(rangeOfExclusive(tok)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    })
  }

  test("NoCompletions.onLineComments") {
    // Note: Line (and doc) comments are inclusive.
    forAll(Programs)(prg => {
      val root = compileWithSuccess(prg)
      forAll(lineCommentsOf(prg, root)) { tok =>
        forAll(rangeOfInclusive(tok)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    })
  }

  /////////////////////////////////////////////////////////////////////////////
  // No Completions: Keywords and Literals
  /////////////////////////////////////////////////////////////////////////////

  test("NoCompletions.onKeywords") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(keywordsOf(prg, root)) { tok =>
        forAll(rangeOfInclusive(tok)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onLiterals") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(literalsOf(prg, root)) { tok =>
        forAll(rangeOfInclusive(tok)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // No Completions: Names
  /////////////////////////////////////////////////////////////////////////////

  test("NoCompletions.onDefs") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(getDefSyms(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onEnums") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(getEnumSyms(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }







  test("No completions when defining the name for sigs") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.sigs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix)
        Assert.isEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for traits") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.traits.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix)
        Assert.isEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for effects") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.effects.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix)
        Assert.isEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for structs") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.structs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix)
        Assert.isEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  test("No completions when defining the name for type aliases") {
    Programs.foreach(program => {
      val (root, errors) = compile(program)
      val allNameDefLocs = root.typeAliases.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach { loc =>
        val completions = CompletionProvider.getCompletions(Uri, Position.from(loc.sp2), errors)(root, Flix)
        Assert.isEmpty(completions, loc, Position.from(loc.sp2))
      }
    })
  }

  /////////////////////////////////////////////////////////////////////////////
  // No Duplicate Completions
  /////////////////////////////////////////////////////////////////////////////

  // TODO
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
        completions.foreach(println)
        println("---")
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

  // TODO: More properties.

  /**
    * Returns all autocomplete suggestions at the given position `pos` for the given AST `root`.
    */
  private def autoComplete(pos: Position, root: Root): List[Completion] = CompletionProvider.getCompletions(Uri, pos, Nil)(root, Flix)

  // TODO: DOC
  def forAll[A](l: List[A])(f: A => Assert): Assert = {
    for (x <- l) {
      f(x) match {
        case Assert.Ok =>
        case Assert.Fail => return Assert.Fail
      }
    }
    Assert.Ok
  }

  /**
    * Returns all *block comment* tokens in the given program `prg` associated with the given AST `root`.
    *
    * That is, comments surrounded by `/* */`.
    */
  private def blockCommentsOf(prg: String, root: Root): List[Token] = {
    def isBlockComment(tok: Token): Boolean = tok.kind match {
      case TokenKind.CommentBlock => true
      case _ => false
    }

    getTokens(prg, root).filter(isBlockComment)
  }

  /**
    * Returns all *line comment* and *doc comment* tokens in the given program `prg` associated with the given AST `root`.
    *
    * Returns both regular line comments and documentation comments. That is, comments starting with `//` or `///`.
    */
  private def lineCommentsOf(prg: String, root: Root): List[Token] = {
    def isLineOrDocComment(tok: Token): Boolean = tok.kind match {
      case TokenKind.CommentLine => true
      case TokenKind.CommentDoc => true
      case _ => false
    }

    getTokens(prg, root).filter(isLineOrDocComment)
  }

  /**
    * Returns all *keyword* tokens in the given program `prg` associated with the given AST `root`.
    */
  private def keywordsOf(prg: String, root: Root): List[Token] =
    getTokens(prg, root).filter(_.kind.isKeyword)

  /**
    * Returns all *literal* tokens in the given program `prg` associated with the given AST `root`.
    */
  private def literalsOf(prg: String, root: Root): List[Token] =
    getTokens(prg, root).filter(_.kind.isLiteral)


  /**
    * Returns all tokens in the given program `prg` associated with the given AST `root`.
    */
  private def getTokens(prg: String, root: Root): List[Token] =
    // TODO: Can we get rid of the need for mkSource?
    root.tokens(mkSource(prg)).toList

  sealed trait Assert

  private object Assert {

    case object Ok extends Assert

    case object Fail extends Assert

    /**
      * Returns `Assert.Ok` if `c` is `true`. Returns `Assert.Fail` otherwise.
      */
    def cond(c: Boolean): Assert = if (c) Ok else Fail

    /**
      * Asserts that the given list of completion `l` for position `pos` is empty.
      */
    def isEmpty(l: List[Completion], pos: Position): Assert = {
      if (l.isEmpty) {
        Assert.Ok
      } else {
        println(s"Found completions: ${l.map(_.toCompletionItem(Flix)).map(_.label)}")
        fail(s"Expected no completions at position $pos, but found ${l.length} completions.")
      }
    }

    /**
      * Asserts that the given completion list is empty at the given position.
      */
    // TODO: Deprecated
    def isEmpty(completions: List[Completion], sourceLocation: SourceLocation, pos: Position): Unit = {
      if (completions.nonEmpty) {
        println(code(sourceLocation, s"Unexpected completions at $pos"))
        println(s"Found completions: ${completions.map(_.toCompletionItem(Flix)).map(_.label)}")
        fail(s"Expected no completions at position $pos, but found ${completions.length} completions.")
      }
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
    * Returns all def symbols in the given AST `root` for the program.
    */
  private def getDefSyms(root: Root): List[Symbol.DefnSym] =
    root.defs.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all def symbols in the given AST `root` for the program.
    */
  private def getEnumSyms(root: Root): List[Symbol.EnumSym] =
    root.enums.keys.filter(_.src.name.startsWith(Uri)).toList

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
    * Returns all positions within a token, i.e., excluding the position where the token ends.
    *
    * For example, given the token `def`, we return the positions corresponding to:
    *
    * - |def
    * - d|ef
    * - de|f
    *
    * If the token spans multiple lines, we will return all the positions on all the lines.
    */
  private def rangeOfExclusive(tok: Token): List[Position] = rangeOfInclusive(tok).init

  /**
    * Returns all positions on a token, i.e., including the position where the token ends.
    *
    * For example, given the token `def`, we return the positions corresponding to:
    *
    * - |def
    * - d|ef
    * - de|f
    *
    * If the token spans multiple lines, we will return all the positions on all the lines.
    */
  private def rangeOfInclusive(tok: Token): List[Position] = {
    val initLine = tok.sp1.lineOneIndexed
    val initCol = tok.sp1.colOneIndexed.toInt

    tok.text
      .scanLeft((initLine, initCol)) {
        case ((line, col), char) =>
          if (char == '\n')
            (line + 1, 1)
          else
            (line, col + 1)
      }
      .map { case (line, col) => Position(line, col) }
      .toList
  }

  // TODO: DOC
  private def rangeOfInclusive(loc: SourceLocation): List[Position] = {
    assert(loc.isSingleLine) // TODO: Support multiline
    (loc.beginCol to loc.endCol).map {
      case col => Position(loc.beginLine, col)
    }.toList
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
