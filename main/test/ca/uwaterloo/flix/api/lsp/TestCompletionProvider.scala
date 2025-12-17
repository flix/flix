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

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.api.lsp.acceptors.FileAcceptor
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source, SymUse}

import java.nio.file.Path
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, Token, TokenKind, TypedAst}
import ca.uwaterloo.flix.language.phase.Lexer
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.collection.mutable
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
  private val Uri = CompilerConstants.VirtualTestFile.toString

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

  test("NoCompletions.onDefSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(defSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onEnumSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(enumSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onEffSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(effectSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onSigSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(sigSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onStructSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(structSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onTraitSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(traitSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  test("NoCompletions.onTypeAliasSyms") {
    forAll(Programs) { prg =>
      val root = compileWithSuccess(prg)
      forAll(typeAliasSymsOf(root)) { sym =>
        forAll(rangeOfInclusive(sym.loc)) { pos =>
          Assert.isEmpty(autoComplete(pos, root), pos)
        }
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // No Duplicate Completions
  /////////////////////////////////////////////////////////////////////////////

  test("NoDuplicateCompletions.Defs") {
    forAll(sample(mkProgramsWithDefUseHoles())) {
      case ProgramWithHole(prg, _, pos) =>
        val (root, errors) = compile(prg)
        val l = autoComplete(pos, root, errors)

        //println(prg)
        //l.foreach(println)
        //println("--")
        Assert.noDuplicateCompletions(l, pos)
    }
  }

  test("NoDuplicateCompletions.Vars") { // TODO: Merge with above
    forAll(sample(mkProgramsWithVarHoles())) {
      case ProgramWithHole(prg, _, pos) =>
        val (root, errors) = compile(prg)
        val l = autoComplete(pos, root, errors)

        //println(prg)
        //l.foreach(println)
        //println("--")
        Assert.noDuplicateCompletions(l, pos)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ranking: Keyword Completions Should Come After Other Completions
  /////////////////////////////////////////////////////////////////////////////
  test("Ranking.KeywordsLast") {
    forAll(sample(mkProgramsWithHoles())) {
      case ProgramWithHole(prg, _, pos) =>
        val (root, errors) = compile(prg)
        val l = autoComplete(pos, root, errors)
        Assert.keywordsLast(l, pos)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Infrastructure
  /////////////////////////////////////////////////////////////////////////////

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
    * Returns a list of programs with holes in them.
    */
  def mkProgramsWithHoles(): List[ProgramWithHole] = {
    mkProgramsWithDefUseHoles() ++ mkProgramsWithVarHoles()
  }

  /**
    * Returns a list of programs with holes where DefUse expressions occur.
    */
  def mkProgramsWithDefUseHoles(): List[ProgramWithHole] = {
    Programs.flatMap(prg => {
      val root = compileWithSuccess(prg)
      val uses = getDefSymUseOccurs(root)
      mkProgramHoles(prg, uses.map(_.loc))
    })
  }

  /**
    * Returns a list of programs with holes where variable expressions occur.
    */
  def mkProgramsWithVarHoles(): List[ProgramWithHole] = {
    Programs.flatMap(prg => {
      val root = compileWithSuccess(prg)
      val uses = getVarSymOccurs(root)
      mkProgramHoles(prg, uses.map(_._2))
    })
  }

  /**
    * Cuts holes in the given program `prg` at all the given source locations `locs`.
    */
  private def mkProgramHoles(prg: String, locs: List[SourceLocation]): List[ProgramWithHole] = {
    locs.flatMap { use =>
      // We cut `n` holes into the program where `n` is length of `loc`.
      val prgsWithHoles = cutHoles(prg, use)

      // We filter some malformed programs.
      prgsWithHoles.filter(p => isCutEligible(p.cut))
    }
  }

  /**
    * Returns `true` if the given string is a meaningful cut-- that is if its omission will not lead to parse errors.
    */
  private def isCutEligible(cut: String): Boolean = {
    cut.nonEmpty && !isLexerKeyword(cut) && cut.forall(c => c.isLetterOrDigit || c == '.')
  }

  /**
    * Given a program `prg` and a source location `loc` within that program returns a list programs with holes cut at the given location.
    *
    * For example, if the program is:
    *
    * {{{
    *   def foo(): Int32 = let bar = 1; bar
    * }}}
    *
    * and we give the source location of the `bar` expression then we return the programs and positions:
    *
    * {{{
    *   def foo(): Int32 = let bar = 1; |
    *   def foo(): Int32 = let bar = 1; b|
    *   def foo(): Int32 = let bar = 1; ba|
    * }}}
    */
  private def cutHoles(prg: String, loc: SourceLocation): List[ProgramWithHole] = {
    assert(loc.isSingleLine)

    val SourcePosition(beginLine, beginCol) = loc.startPosition
    val result = mutable.ListBuffer.empty[ProgramWithHole]
    val length = loc.end - loc.start
    for (i <- 0 until length) {
      val o = loc.start + i
      val prefix = prg.substring(0, o)
      val cut = prg.substring(loc.start, loc.start + i)
      val suffix = prg.substring(loc.end, prg.length)
      val withHole = prefix + suffix
      val pos = Position(beginLine, beginCol + i)
      result += ProgramWithHole(withHole, cut, pos)
    }

    result.toList
  }

  /**
    * Returns all autocomplete suggestions at the given position `pos` for the given AST `root` under the assumption that there are no errors.
    */
  private def autoComplete(pos: Position, root: Root): List[Completion] = autoComplete(pos, root, Nil)

  /**
    * Returns all autocomplete suggestions at the given position `pos` for the given AST `root` with the given `errors`.
    */
  private def autoComplete(pos: Position, root: Root, errors: List[CompilationMessage]): List[Completion] = CompletionProvider.getCompletions(Uri, pos, errors)(root, Flix)

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
    root.tokens(mkSource(prg)).toList // TODO: Can we get rid of the need for mkSource?

  /**
    * Returns a random subset of the given list `l`.
    */
  def sample[A](l: List[A]): List[A] = {
    Random.shuffle(l).take(Limit)
  }

  sealed trait Assert

  private object Assert {

    case object Ok extends Assert

    case object Fail extends Assert

    /**
      * Returns `Assert.Ok` if `c` is `true`. Returns `Assert.Fail` otherwise.
      */
    def cond(c: Boolean): Assert = if (c) Ok else Fail

    /**
      * Asserts that the given list of completions `l` at position `pos` is empty.
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
      * Asserts that the given list of completions `l` at position `pos` contains no duplicates.
      *
      * A completion is considered a duplicate if it:
      * - has the same label
      * - has the same kind
      * - had the same label details
      */
    def noDuplicateCompletions(l: List[Completion], pos: Position): Assert = {
      val xs = l.map(_.toCompletionItem(Flix))
      for (c1 <- xs; c2 <- xs) {
        if (c1.label == c2.label && c1.kind == c2.kind && c1.labelDetails == c2.labelDetails && c1 != c2) {
          println(s"Duplicate completions at $pos:")
          println(s"  $c1")
          println(s"  $c2")
          fail(s"Duplicate completions: ${c1.getClass.getName} and ${c2.getClass.getName}.")
        }
      }
      Assert.Ok
    }

    /**
      * Asserts that in the given list of completions `l` at position `pos` there
      * is no keyword completion ranked higher than a non-keyword completion.
      */
    def keywordsLast(l: List[Completion], pos: Position): Assert = {
      val withIndex = l.zipWithIndex
      for ((c1, i1) <- withIndex; (c2, i2) <- withIndex; if i1 != i2) {
        (c1, c2) match {
          case (_: Completion.KeywordCompletion, _: Completion.KeywordCompletion) => // OK
          case (_: Completion.KeywordCompletion, _) =>
            if (c1.priority > c2.priority) {
              println(s"Keyword completion with higher priority than non-keyword completion at $pos:")
              println(s"  $c1")
              println(s"  $c2")
              fail(s"Keyword completion '$c1' with higher priority than non-keyword completion '$c2'.")
            }
          case (_, _) => // OK
        }
      }
      Assert.Ok
    }

  }

  /**
    * Returns `true` if the given string `s` is a keyword.
    */
  private def isLexerKeyword(s: String): Boolean = {
    // Use the lexer to determine if `s` is a keyword.
    val (tokens, _) = Lexer.lex(mkSource(s))
    tokens.exists(_.kind.isKeyword)
  }

  /**
    * Returns all def symbols in the given AST `root` for the program.
    */
  private def defSymsOf(root: Root): List[Symbol.DefnSym] =
    root.defs.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all effects symbols in the given AST `root` for the program.
    */
  private def effectSymsOf(root: Root): List[Symbol.EffSym] =
    root.effects.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all def symbols in the given AST `root` for the program.
    */
  private def enumSymsOf(root: Root): List[Symbol.EnumSym] =
    root.enums.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all sig symbols in the given AST `root` for the program.
    */
  private def sigSymsOf(root: Root): List[Symbol.SigSym] =
    root.sigs.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all struct symbols in the given AST `root` for the program.
    */
  private def structSymsOf(root: Root): List[Symbol.StructSym] =
    root.structs.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all trait symbols in the given AST `root` for the program.
    */
  private def traitSymsOf(root: Root): List[Symbol.TraitSym] =
    root.traits.keys.filter(_.src.name.startsWith(Uri)).toList

  /**
    * Returns all type alias symbols in the given AST `root` for the program.
    */
  private def typeAliasSymsOf(root: Root): List[Symbol.TypeAliasSym] =
    root.typeAliases.keys.filter(_.src.name.startsWith(Uri)).toList


  /**
    * Returns all real uses of all defs for the given AST `root`.
    */
  private def getDefSymUseOccurs(root: Root): List[SymUse.DefSymUse] = {
    var occurs: Set[SymUse.DefSymUse] = Set.empty

    object DefSymUseConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.ApplyDef(symUse, _, _, _, _, _, _) if symUse.loc.isReal =>
          occurs += symUse
        case _ =>
      }
    }

    Visitor.visitRoot(root, DefSymUseConsumer, FileAcceptor(Uri))

    occurs.toList
  }

  /**
    * Returns all real uses of all vars for the given AST `root`.
    */
  private def getVarSymOccurs(root: Root): List[(Symbol.VarSym, SourceLocation)] = {
    var occurs: Set[(Symbol.VarSym, SourceLocation)] = Set.empty

    object VarConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.Var(sym, _, loc) if sym.loc.isReal => occurs += ((sym, loc))
        case _ =>
      }
    }

    Visitor.visitRoot(root, VarConsumer, FileAcceptor(Uri))

    occurs.toList
  }

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  private def compile(program: String): (Root, List[CompilationMessage]) = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    Flix.addVirtualPath(CompilerConstants.VirtualTestFile, program)
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
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    Flix.addVirtualPath(CompilerConstants.VirtualTestFile, program)
    Flix.check() match {
      case (Some(root), Nil) => root
      case (_, errors) =>
        for (error <- errors) {
          val msg = error.message(NoFormatter)
          println(msg)
        }
        fail("Compilation failed.")
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
    val SourcePosition(initLine, initCol) = tok.startPosition

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
    val SourcePosition(beginLine, beginCol) = loc.startPosition
    val SourcePosition(endLine, endCol) = loc.endPosition
    assert(beginLine == endLine) // TODO: Support multiline
    (beginCol to endCol).map {
      case col => Position(beginLine, col)
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
    val sctx = SecurityContext.Unrestricted
    val input = Input.VirtualFile(CompilerConstants.VirtualTestFile, content, sctx)
    Source(input, content.toCharArray)
  }

  /**
    * A program `prg` with a hole - the cut - at the specified position `pos`.
    */
  case class ProgramWithHole(prg: String, cut: String, pos: Position)

}
