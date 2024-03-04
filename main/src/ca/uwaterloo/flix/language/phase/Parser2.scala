/*
 * Copyright 2023 Herluf Baggesen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Ast, ChangeSet, ParsedAst, ReadAst, SourceKind, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.errors.Parser2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{InternalCompilerException, LibLevel, ParOps, Validation}
import org.parboiled2.ParserInput

import scala.collection.mutable.ArrayBuffer

// TODO: Add change set support

/**
 * Errors reside both within the produced `Tree` but are also kept in an array in `state.errors`
 * to make it easy to return them as a `CompilationMessage` after parsing.
 */
object Parser2 {

  private sealed trait Event

  private object Event {
    // TODO: Make `openBefore` links
    case class Open(kind: TreeKind) extends Event

    case object Close extends Event

    case object Advance extends Event
  }

  private class State(val tokens: Array[Token], val src: Ast.Source) {
    var position: Int = 0
    var fuel: Int = 256
    var events: ArrayBuffer[Event] = ArrayBuffer.empty
    val errors: ArrayBuffer[Parser2Error] = ArrayBuffer.empty
    // Compute a `ParserInput` when initializing a state for lexing a source.
    // This is necessary to display source code in error messages.
    // See `sourceLocationAtStart` for usage and `SourceLocation` for more information.
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  private sealed trait Mark

  private object Mark {
    case class Opened(index: Int) extends Mark

    case class Closed(index: Int) extends Mark
  }

  // A helper for development that will run the new parsing pipeline on each source
  // and fall back on the old parsing pipeline if there is a failure.
  def runWithFallback(
                       afterReader: ReadAst.Root,
                       afterLexer: Map[Ast.Source, Array[Token]],
                       entryPoint: Option[Symbol.DefnSym],
                       changeSet: ChangeSet
                     )(implicit flix: Flix): Validation[WeededAst.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.HardFailure(Chain.empty)
    }

    flix.phase("Parser2") {
      // The file that is the current focus of development
      val DEBUG_FOCUS = flix.options.lib match {
        case LibLevel.Nix => "main/foo.flix"
        case LibLevel.Min => ""
        case LibLevel.All => ""
      }

      // For each file: If the parse was successful run Weeder2 on it.
      // If either the parse or the weed was bad pluck the WeededAst from the previous pipeline and use that.
      val afterParser = Parser.run(afterReader, entryPoint, ParsedAst.empty, changeSet)
      val afterWeeder = flatMapN(afterParser)(parsedAst => Weeder.run(parsedAst, WeededAst.empty, changeSet))

      def diffWeededAsts(src: Ast.Source, newAst: WeededAst.CompilationUnit): Boolean = {
        // Unsafely get old WeededAst
        val oldAst = afterWeeder.unsafeGet.units(src)
        formatWeededAst(oldAst, matchingWithOldParser = true) == formatWeededAst(newAst, matchingWithOldParser = true)
      }

      val results = ParOps.parMap(afterLexer) {
        case (src, tokens) =>
          val afterParser2 = parse(src, tokens)
          val p = afterParser2.toSoftResult.toOption

          // Print syntax tree if available
          if (src.name == DEBUG_FOCUS && p.isDefined) {
            val (tree, _) = p.get
            println(syntaxTreeToDebugString(tree))
          }

          val afterWeeder2 = flatMapN(afterParser2)(Weeder2.weed(src, _))
          val w = afterWeeder2.toSoftResult.toOption

          // Log weeded asts if available
          if (src.name == DEBUG_FOCUS && w.isDefined) {
            val (newAst, _) = w.get
            val oldAst = afterWeeder.unsafeGet.units(src) // Assume old weeder succeeded
            println("[[[ OLD PARSER ]]]")
            println(formatWeededAst(oldAst, matchingWithOldParser = true))
            println("[[[ NEW PARSER ]]]")
            println(formatWeededAst(newAst, matchingWithOldParser = true))
            println("[[[ END ]]]")
          }

          // Log status if there were failures
          val weededAstsMatch = w.forall(t => diffWeededAsts(src, t._1))
          // We are all good if the parser and weeder had no errors and the weeded asts match
          val allGood = p.exists(_._2.isEmpty) && w.exists(_._2.isEmpty) && weededAstsMatch
          if (!allGood) {
            val parserPart = if (p.isEmpty) s"${Console.RED}✘ "
            else if (p.exists(!_._2.isEmpty)) s"${Console.YELLOW}✘ "
            else s"${Console.GREEN}✔︎ "

            val weederPart = if (p.isEmpty) "- "
            else if (!weededAstsMatch) s"${Console.YELLOW}!="
            else s"${Console.RED}✘ "
            println(s"$parserPart\t$weederPart\t${Console.RESET}${src.name}")
          }

          // Fallback on old pipeline if necessary
          val weededAst = if (w.isEmpty)
            mapN(afterWeeder)(_.units(src)).withSoftFailures(afterWeeder2.errors.toSeq)
          else
            afterWeeder2

          mapN(weededAst)(src -> _)
      }
      val resultMap = mapN(sequence(results))(_.toMap)
      mapN(resultMap)(m => WeededAst.Root(m, entryPoint, afterReader.names))
    }
  }

  def run(root: Map[Ast.Source, Array[Token]])(implicit flix: Flix): Validation[Map[Ast.Source, SyntaxTree.Tree], CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.HardFailure(Chain.empty)
    }

    flix.phase("Parser2") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(root) {
        case (src, tokens) => mapN(parse(src, tokens))(trees => src -> trees)
      }

      mapN(sequence(results))(_.toMap)
    }
  }

  private def parse(src: Ast.Source, ts: Array[Token]): Validation[SyntaxTree.Tree, CompilationMessage] = {
    implicit val s: State = new State(ts, src)
    source()
    val tree = buildTree()
    if (s.errors.length > 0) {
      Validation.SoftFailure(tree, Chain.from(s.errors))
    } else {
      Validation.success(tree)
    }
  }

  private def buildTree()(implicit s: State): SyntaxTree.Tree = {
    val tokens = s.tokens.iterator.buffered
    var stack: ArrayBuffer[SyntaxTree.Tree] = ArrayBuffer.empty
    var locationStack: ArrayBuffer[Token] = ArrayBuffer.empty

    // Pop the last event, which must be a Close,
    // to ensure that the stack is not empty when handling event below.
    val lastEvent = s.events.last
    s.events = s.events.dropRight(1)
    assert(lastEvent match {
      case Event.Close => true
      case _ => false
    })

    // NB, make a synthetic token to begin with, to make the SourceLocations
    // generated below be correct.
    var lastAdvance = Token(TokenKind.Eof, s.src.data, 0, 0, 0, 0, 0, 0)
    for (event <- s.events) {
      event match {
        case Event.Open(kind) =>
          locationStack.append(tokens.head)
          stack.append(SyntaxTree.Tree(kind, Array.empty, SourceLocation.Unknown))

        case Event.Close =>
          val child = SyntaxTree.Child.TreeChild(stack.last)
          val openToken = locationStack.last
          stack.last.loc = if (stack.last.children.length == 0)
            // If the subtree has no children, give it a zero length position just after the last token
            SourceLocation.mk(
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput)),
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput))
            )
          else
            // Otherwise the source location can span from the first to the last token in the sub tree
            SourceLocation.mk(
              openToken.mkSourcePosition(s.src, Some(s.parserInput)),
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput))
            )
          locationStack = locationStack.dropRight(1)
          stack = stack.dropRight(1)
          stack.last.children = stack.last.children :+ child

        case Event.Advance =>
          val token = tokens.next()
          lastAdvance = token
          stack.last.children = stack.last.children :+ SyntaxTree.Child.TokenChild(token)
      }
    }

    // Set source location of the root
    val openToken = locationStack.last
    stack.last.loc = SourceLocation.mk(
      openToken.mkSourcePosition(s.src, Some(s.parserInput)),
      tokens.head.mkSourcePositionEnd(s.src, Some(s.parserInput))
    )

    // The stack should now contain a single Source tree,
    // and there should only be an <eof> token left.
    assert(stack.length == 1)
    assert(tokens.next().kind == TokenKind.Eof)
    stack.head
  }

  /**
   * A helper function for turning the current position of the parser as a `SourceLocation`
   */
  private def currentSourceLocation()(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val token = s.tokens(s.position)
    val line = token.beginLine + 1
    val column = token.beginCol + 1
    SourceLocation(Some(s.parserInput), s.src, SourceKind.Real, line, column, line, column + token.text.length)
  }

  private def comments()(implicit s: State): Unit = {
    // Note: [[atAny]] is deliberately __not__ used here, since comments is called with every open.
    // Using [[atAny]] calls [[nth]] many times which depletes the parsers fuel unnecessarily.
    val atComment = s.tokens.lift(s.position) match {
      case Some(Token(k, _, _, _, _, _, _, _)) => k == TokenKind.CommentLine || k == TokenKind.CommentBlock
      case None => false
    }
    if (atComment) {
      val mark = Mark.Opened(s.events.length)
      s.events.append(
        Event.Open(TreeKind.ErrorTree(Parser2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))
      )
      while (atAny(List(TokenKind.CommentLine, TokenKind.CommentBlock)) && !eof()) {
        advance()
      }
      close(mark, TreeKind.CommentList)
    }
  }

  private def open()(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    s.events.append(
      Event.Open(TreeKind.ErrorTree(Parser2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))
    )
    // Consume any comments just before opening a new mark
    comments()
    mark
  }

  private def close(mark: Mark.Opened, kind: TreeKind)(implicit s: State): Mark.Closed = {
    s.events(mark.index) = Event.Open(kind)
    // Consume any comments just before closing a mark
    comments()
    s.events.append(Event.Close)
    Mark.Closed(mark.index)
  }

  private def openBefore(before: Mark.Closed)(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(before.index)
    s.events.insert(
      before.index,
      Event.Open(TreeKind.ErrorTree(Parser2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))
    )
    mark
  }

  private def closeWithError(mark: Mark.Opened, error: Parser2Error)(implicit s: State): Mark.Closed = {
    s.errors.append(error)
    close(mark, TreeKind.ErrorTree(error))
  }

  private def advance()(implicit s: State): Unit = {
    if (eof()) {
      return
    }
    s.fuel = 256
    s.events.append(Event.Advance)
    s.position += 1
  }

  private def advanceWithError(error: Parser2Error)(implicit s: State): Mark.Closed = {
    val mark = open()
    advance()
    closeWithError(mark, error)
  }

  private def eof()(implicit s: State): Boolean = {
    s.position == s.tokens.length - 1
  }

  private def nth(lookahead: Int)(implicit s: State): TokenKind = {
    if (s.fuel == 0) {
      throw InternalCompilerException(s"[${currentSourceLocation()}] Parser is stuck", currentSourceLocation())
    }

    s.fuel -= 1
    s.tokens.lift(s.position + lookahead) match {
      case Some(t) => t.kind
      case None => TokenKind.Eof
    }
  }

  private def findBefore(needle: TokenKind, before: List[TokenKind])(implicit s: State): Boolean = {
    var lookahead = 1
    while (!eof()) {
      nth(lookahead) match {
        case t if t == needle => return true
        case t if before.contains(t) => return false
        case TokenKind.Eof => return false
        case _ => lookahead += 1
      }
    }
    false
  }

  private def at(kind: TokenKind)(implicit s: State): Boolean = {
    // Consume any comments before checking for the expected [[Tokenkind]]
    comments()
    nth(0) == kind
  }

  private def atAny(kinds: List[TokenKind])(implicit s: State): Boolean = {
    kinds.contains(nth(0))
  }

  private def eat(kind: TokenKind)(implicit s: State): Boolean = {
    if (at(kind)) {
      advance()
      true
    } else {
      false
    }
  }

  private def eatAny(kinds: List[TokenKind])(implicit s: State): Boolean = {
    if (atAny(kinds)) {
      advance()
      true
    } else {
      false
    }
  }

  private def expect(kind: TokenKind)(implicit s: State): Unit = {
    if (!eat(kind)) {
      val mark = open()
      val error = Parser2Error.DevErr(currentSourceLocation(), s"Expected $kind, but found ${nth(0)}")
      closeWithError(mark, error)
    }
  }

  private def expectAny(kinds: List[TokenKind])(implicit s: State): Unit = {
    if (!eatAny(kinds)) {
      val mark = open()
      val error = Parser2Error.DevErr(currentSourceLocation(), s"Expected one of ${kinds.mkString(", ")}, , but found ${nth(0)}")
      closeWithError(mark, error)
    }
  }

  private class Separated(
                           val getItem: () => Mark.Closed,
                           val checkForItem: () => Boolean,
                           var separator: TokenKind,
                           var optionalSeparator: Boolean,
                           var leftDelim: TokenKind,
                           var rightDelim: TokenKind
                         ) {
    def within(left: TokenKind, right: TokenKind): Separated = {
      this.leftDelim = left
      this.rightDelim = right
      this
    }

    def by(separator: TokenKind, optional: Boolean = false): Separated = {
      this.separator = separator
      this.optionalSeparator = optional
      this
    }

    def zeroOrMore(followedBy: Option[(TokenKind, () => Unit)] = None)(implicit s: State): Unit = {
      def isAtEnd(): Boolean = at(rightDelim) || followedBy.exists { case (delim, _) => at(delim) }

      assert(at(leftDelim))
      expect(leftDelim)
      var continue = true
      while (continue && !isAtEnd() && !eof()) {
        if (checkForItem()) {
          getItem()
          if (!isAtEnd()) {
            if (optionalSeparator) eat(separator) else expect(separator)
          }
        } else {
          continue = false
        }
      }
      if (at(separator)) {
        advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Trailing $separator"))
      }

      followedBy match {
        case Some((delim, rule)) => if (eat(delim)) {
          rule()
        }
        case None =>
      }
      expect(rightDelim)
    }
  }

  private def separated(getItem: () => Mark.Closed, checkForItem: () => Boolean = () => true): Separated = {
    new Separated(getItem, checkForItem, TokenKind.Comma, false, TokenKind.ParenL, TokenKind.ParenR)
  }

  private val NAME_DEFINITION = List(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.UserDefinedOperator)
  private val NAME_PARAMETER = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_VARIABLE = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_JAVA = List(TokenKind.NameJava, TokenKind.NameLowerCase, TokenKind.NameUpperCase)
  private val NAME_QNAME = List(TokenKind.NameLowerCase, TokenKind.NameUpperCase)
  private val NAME_USE = List(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.UserDefinedOperator)
  private val NAME_FIELD = List(TokenKind.NameLowerCase)
  // TODO: Static is used as a type in Prelude.flix. Should we allow this?
  private val NAME_TYPE = List(TokenKind.NameUpperCase, TokenKind.KeywordStaticUppercase)
  private val NAME_KIND = List(TokenKind.NameUpperCase)
  private val NAME_EFFECT = List(TokenKind.NameUpperCase)
  private val NAME_MODULE = List(TokenKind.NameUpperCase)
  // TODO: Pure and Impure are used in enums as tags in Prelude.flix. Should we allow this?
  private val NAME_TAG = List(TokenKind.NameUpperCase, TokenKind.KeywordPure, TokenKind.KeywordImpure)
  private val NAME_PREDICATE = List(TokenKind.NameUpperCase)

  private def name(kinds: List[TokenKind], allowQualified: Boolean = false)(implicit s: State): Mark.Closed = {
    val mark = open()
    expectAny(kinds)
    val first = close(mark, TreeKind.Ident)
    if (!allowQualified) {
      return first
    }
    while (at(TokenKind.Dot) && kinds.contains(nth(1)) && !eof()) {
      eat(TokenKind.Dot)
      val mark = open()
      expectAny(kinds)
      close(mark, TreeKind.Ident)
    }
    if (allowQualified) {
      val mark = openBefore(first)
      close(mark, TreeKind.QName)
    } else {
      first
    }
  }

  /**
   * source -> declaration*
   */
  private def source()(implicit s: State): Unit = {
    val mark = open()
    usesOrImports()
    while (!eof()) {
      Decl.declaration()
    }
    close(mark, TreeKind.Root)
  }

  private def usesOrImports()(implicit s: State): Mark.Closed = {
    val mark = open()
    var continue = true
    while (continue && !eof()) {
      nth(0) match {
        case TokenKind.KeywordUse =>
          use()
          eat(TokenKind.Semi)
        case TokenKind.KeywordImport =>
          iimport()
          eat(TokenKind.Semi)
        case _ => continue = false
      }
    }
    close(mark, TreeKind.UsesOrImports.UseOrImportList)
  }

  private def use()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.KeywordUse))
    val mark = open()
    expect(TokenKind.KeywordUse)
    name(NAME_USE, allowQualified = true)

    // handle use many case
    if (at(TokenKind.DotCurlyL)) {
      val mark = open()
      separated(() => aliasedName(NAME_USE), checkForItem = () => atAny(NAME_USE))
        .within(TokenKind.DotCurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.UsesOrImports.UseMany)
    }

    close(mark, TreeKind.UsesOrImports.Use)
  }

  private def iimport()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.KeywordImport))
    val mark = open()
    expect(TokenKind.KeywordImport)
    name(NAME_JAVA, allowQualified = true)
    // handle import many case
    if (at(TokenKind.DotCurlyL)) {
      val mark = open()
      separated(() => aliasedName(NAME_JAVA))
        .within(TokenKind.DotCurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.UsesOrImports.ImportMany)
    }

    close(mark, TreeKind.UsesOrImports.Import)
  }

  private def aliasedName(names: List[TokenKind])(implicit s: State): Mark.Closed = {
    var lhs = name(names)
    if (eat(TokenKind.ArrowThickR)) {
      name(names)
      lhs = close(openBefore(lhs), TreeKind.UsesOrImports.Alias)
    }
    lhs
  }

  private object Decl {
    def declaration()(implicit s: State): Mark.Closed = {
      val mark = open()
      // TODO: Find better way to handle "DocComment LineComment DocComment"
      while (at(TokenKind.CommentDoc) && !eof()) {
        docComment()
      }
      if (at(TokenKind.KeywordMod)) {
        return module(mark)
      }

      annotations()
      modifiers()
      nth(0) match {
        case TokenKind.KeywordDef => definition(mark)
        case TokenKind.KeywordTrait => typeClass(mark)
        case TokenKind.KeywordInstance => instance(mark)
        case TokenKind.KeywordType => typeAlias(mark)
        case TokenKind.KeywordEff => effect(mark)
        case TokenKind.KeywordEnum | TokenKind.KeywordRestrictable => enumeration(mark)
        case _ =>
          advance()
          closeWithError(mark, Parser2Error.DevErr(currentSourceLocation(), s"Expected declaration but found ${nth(0)}"))
      }
    }

    private def effect(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordEff))
      expect(TokenKind.KeywordEff)
      name(NAME_EFFECT)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }

      if (eat(TokenKind.CurlyL)) {
        while (!at(TokenKind.CurlyR) && !eof()) {
          operation()
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Effect)
    }

    private def operation()(implicit s: State): Mark.Closed = {
      val mark = open()
      docComment()
      annotations()
      modifiers()
      expect(TokenKind.KeywordDef)
      name(NAME_DEFINITION)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      parameters()
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      close(mark, TreeKind.Decl.Op)
    }

    private def typeAlias(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordType))
      expect(TokenKind.KeywordType)
      expect(TokenKind.KeywordAlias)
      name(NAME_TYPE)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }

      if (eat(TokenKind.Equal)) {
        Type.ttype()
      }
      close(mark, TreeKind.Decl.TypeAlias)
    }

    /**
     * definition -> docComment? annotations? 'def' name typeParameters parameters ':' ttype '=' expression (';' expression)*
     */
    private def definition(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordDef)
      // TODO: Remove this special handling. Something goes very wrong when I do!
      if (s.src.name == "Concurrent/Channel.flix") {
        name(NAME_DEFINITION ++ List(TokenKind.KeywordGet))
      } else {
        name(NAME_DEFINITION)
      }

      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.ParenL)) {
        parameters()
      }
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      if (at(TokenKind.KeywordWhere)) {
        equalityConstraints(TokenKind.Equal)
      }
      expect(TokenKind.Equal)
      Expr.statement()
      close(mark, TreeKind.Decl.Def)
    }

    private def module(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordMod))
      expect(TokenKind.KeywordMod)
      name(NAME_MODULE, allowQualified = true)
      if (at(TokenKind.CurlyL)) {
        expect(TokenKind.CurlyL)
        usesOrImports()
        while (!at(TokenKind.CurlyR) && !eof()) {
          declaration()
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Module)
    }

    private def enumeration(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.KeywordRestrictable, TokenKind.KeywordEnum)))
      val isRestrictable = eat(TokenKind.KeywordRestrictable)
      expect(TokenKind.KeywordEnum)
      name(NAME_TYPE)
      if (isRestrictable) {
        expect(TokenKind.BracketL)
        val markParam = open()
        name(NAME_VARIABLE)
        close(markParam, TreeKind.Parameter)
        expect(TokenKind.BracketR)
      }
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      // Singleton short-hand
      if (at(TokenKind.ParenL)) {
        val markType = open()
        Type.tuple()
        close(markType, TreeKind.Type.Type)
      }
      // derivations
      if (at(TokenKind.KeywordWith)) {
        Type.derivations()
      }
      // enum body
      if (eat(TokenKind.CurlyL)) {
        enumCases()
        expect(TokenKind.CurlyR)
      }
      close(mark, if (isRestrictable) TreeKind.Decl.RestrictableEnum else TreeKind.Decl.Enum)
    }

    private def enumCases()(implicit s: State): Unit = {
      while (!eof() && atAny(List(TokenKind.CommentDoc, TokenKind.KeywordCase, TokenKind.Comma))) {
        val mark = open()
        docComment()
        if (at(TokenKind.KeywordCase)) {
          expect(TokenKind.KeywordCase)
        } else {
          expect(TokenKind.Comma)
          // Handle comma followed by case keyword
          docComment()
          eat(TokenKind.KeywordCase)
        }
        name(NAME_TAG)
        if (at(TokenKind.ParenL)) {
          val mark = open()
          Type.tuple()
          close(mark, TreeKind.Type.Type)
        }
        close(mark, TreeKind.Case)
      }
    }


    /**
     * instance -> docComment? annotations? modifiers? 'instance' qname typeParams '{' associatedTypeDef* definition*'}'
     */
    private def instance(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordInstance)
      name(NAME_DEFINITION, allowQualified = true)
      if (eat(TokenKind.BracketL)) {
        Type.ttype()
        expect(TokenKind.BracketR)
      }
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      if (at(TokenKind.CurlyL)) {
        expect(TokenKind.CurlyL)
        while (!at(TokenKind.CurlyR) && !eof()) {
          val mark = open()
          docComment()
          annotations() // TODO: associated types cant have annotations
          modifiers()
          nth(0) match {
            case TokenKind.KeywordDef => definition(mark)
            case TokenKind.KeywordType => associatedTypeDef(mark)
            case _ =>
              advance()
              closeWithError(mark, Parser2Error.DevErr(currentSourceLocation(), s"Expected associated type or definition"))
          }
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Instance)
    }

    /**
     * typeClass -> docComment? annotations? name typeParameters '{' (signature | associatedType)* '}'
     */
    private def typeClass(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordTrait)
      name(NAME_DEFINITION)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      if (at(TokenKind.CurlyL)) {
        expect(TokenKind.CurlyL)
        while (!at(TokenKind.CurlyR) && !eof()) {
          val mark = open()

          // TODO: Find better way to handle "DocComment LineComment DocComment"
          while (at(TokenKind.CommentDoc) && !eof()) {
            docComment()
          }
          annotations() // TODO: associated types cant have annotations
          modifiers()
          nth(0) match {
            case TokenKind.KeywordLaw => law(mark)
            case TokenKind.KeywordDef => signature(mark)
            case TokenKind.KeywordType => associatedTypeSig(mark)
            case _ =>
              advance()
              closeWithError(mark, Parser2Error.DevErr(currentSourceLocation(), s"Expected associated type, signature or law"))
          }
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Class)
    }

    private def law(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordLaw))
      expect(TokenKind.KeywordLaw)
      name(NAME_DEFINITION)
      expect(TokenKind.Colon)
      expect(TokenKind.KeywordForall)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.ParenL)) {
        parameters()
      }
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      expect(TokenKind.Dot)
      Expr.expression()
      close(mark, TreeKind.Decl.Law)
    }

    private def associatedTypeDef(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordType)
      name(NAME_TYPE)
      if (at(TokenKind.BracketL)) {
        Type.arguments()
      }
      expect(TokenKind.Equal)
      Type.ttype()
      close(mark, TreeKind.Decl.AssociatedTypeDef)
    }

    private def associatedTypeSig(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordType)
      name(NAME_TYPE)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.Colon)) {
        expect(TokenKind.Colon)
        Type.kind()
      }
      close(mark, TreeKind.Decl.AssociatedTypeSig)
    }

    /**
     * signature -> 'def' name typeParameters parameters ':' ttype ('=' expression (';' expression)*)?
     */
    private def signature(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordDef)
      name(NAME_DEFINITION)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.ParenL)) {
        parameters()
      }
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      if (at(TokenKind.KeywordWhere)) {
        equalityConstraints(TokenKind.Equal)
      }
      if (at(TokenKind.Equal)) {
        expect(TokenKind.Equal)
        Expr.statement()
      }
      close(mark, TreeKind.Decl.Signature)
    }

    ///////////// SHARED DECL CONCEPTS ////////////
    private val MODIFIERS = List(
      TokenKind.KeywordSealed,
      TokenKind.KeywordLawful,
      TokenKind.KeywordPub,
      TokenKind.KeywordInline,
      TokenKind.KeywordOverride)

    private def modifiers()(implicit s: State): Mark.Closed = {
      val mark = open()
      while (atAny(MODIFIERS) && !eof()) {
        advance()
      }
      close(mark, TreeKind.ModifierList)
    }

    def annotations()(implicit s: State): Mark.Closed = {
      val mark = open()
      while (at(TokenKind.Annotation) && !eof()) {
        advance()
      }
      close(mark, TreeKind.AnnotationList)
    }

    def docComment()(implicit s: State): Mark.Closed = {
      val mark = open()
      while (at(TokenKind.CommentDoc) && !eof()) {
        advance()
      }
      close(mark, TreeKind.Doc)
    }

    /**
     * parameters -> '(' (parameter (',' parameter)* )? ')'
     */
    def parameters()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(parameter, checkForItem = () => atAny(NAME_PARAMETER)).zeroOrMore()
      close(mark, TreeKind.ParameterList)
    }

    def parameter()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PARAMETER)
      if (eat(TokenKind.Colon)) {
        Type.ttype()
      }
      close(mark, TreeKind.Parameter)
    }

    def equalityConstraints(terminator: TokenKind)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordWhere))
      val mark = open()
      expect(TokenKind.KeywordWhere)
      while (!at(terminator) && !eof()) {
        val markConstraint = open()
        Type.ttype()
        expect(TokenKind.Tilde)
        Type.ttype()
        eat(TokenKind.Comma)
        close(markConstraint, TreeKind.Decl.EqualityConstraintFragment)
      }
      close(mark, TreeKind.Decl.EqualityConstraintList)
    }
  }

  private object Expr {

    def statement()(implicit s: State): Mark.Closed = {
      var lhs = expression()
      if (eat(TokenKind.Semi)) {
        statement()
        lhs = close(openBefore(lhs), TreeKind.Expr.Statement)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }
      lhs
    }

    /**
     * expression -> TODO
     */
    def expression(left: TokenKind = TokenKind.Eof, leftIsUnary: Boolean = false, allowQualified: Boolean = true)(implicit s: State): Mark.Closed = {
      var lhs = exprDelimited()

      // Handle calls
      while (at(TokenKind.ParenL)) {
        val mark = openBefore(lhs)
        arguments()
        lhs = close(mark, TreeKind.Expr.Apply)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }

      // Handle record select
      if (at(TokenKind.Dot) && nth(1) == TokenKind.NameLowerCase) {
        val mark = openBefore(lhs)
        eat(TokenKind.Dot)
        name(NAME_FIELD)
        while (eat(TokenKind.Dot)) {
          name(NAME_FIELD)
        }
        lhs = close(mark, TreeKind.Expr.RecordSelect)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }

      // Handle type ascriptions
      if (!leftIsUnary && eat(TokenKind.Colon)) {
        Type.ttype()
        lhs = close(openBefore(lhs), TreeKind.Expr.Ascribe)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }

      // Handle binary operators
      var continue = true
      while (continue) {
        val right = nth(0)
        if (rightBindsTighter(left, right, leftIsUnary)) {
          val mark = openBefore(lhs)
          val markOp = open()
          advance()
          close(markOp, TreeKind.Operator)
          expression(right, allowQualified = allowQualified)
          lhs = close(mark, TreeKind.Expr.Binary)
          lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
        } else {
          continue = false
        }
      }

      // Handle without expressions
      if (eat(TokenKind.KeywordWithout)) {
        if (at(TokenKind.CurlyL)) {
          val mark = open()
          separated(() => name(NAME_EFFECT, allowQualified = true))
            .within(TokenKind.CurlyL, TokenKind.CurlyR)
            .zeroOrMore()
          close(mark, TreeKind.Type.EffectSet)
        } else {
          val mark = open()
          name(NAME_EFFECT, allowQualified = true)
          close(mark, TreeKind.Type.EffectSet)
        }
        lhs = close(openBefore(lhs), TreeKind.Expr.Without)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }

      lhs
    }

    sealed trait OpKind

    private object OpKind {

      case object Unary extends OpKind

      case object Binary extends OpKind
    }

    // A precedence table for operators, lower is higher precedence.
    // Note that [[OpKind]] is necessary for the cases where the same token kind can be both unary and binary. IE. Plus or Minus
    private def PRECEDENCE: List[(OpKind, List[TokenKind])] = List(
      (OpKind.Binary, List(TokenKind.ColonEqual, TokenKind.KeywordInstanceOf)), // :=, instanceof
      (OpKind.Binary, List(TokenKind.KeywordOr)),
      (OpKind.Binary, List(TokenKind.KeywordAnd)),
      (OpKind.Binary, List(TokenKind.TripleBar)), // |||
      (OpKind.Binary, List(TokenKind.TripleCaret)), // ^^^
      (OpKind.Binary, List(TokenKind.TripleAmpersand)), // &&&
      (OpKind.Binary, List(TokenKind.EqualEqual, TokenKind.AngledEqual, TokenKind.BangEqual)), // ==, <=>, !=
      (OpKind.Binary, List(TokenKind.AngleL, TokenKind.AngleR, TokenKind.AngleLEqual, TokenKind.AngleREqual)), // <, >, <=, >=
      (OpKind.Binary, List(TokenKind.ColonColon, TokenKind.TripleColon)), // ::
      (OpKind.Binary, List(TokenKind.TripleAngleL, TokenKind.TripleAngleR)), // <<<, >>>
      (OpKind.Binary, List(TokenKind.Plus, TokenKind.Minus)), // +, -
      (OpKind.Binary, List(TokenKind.Star, TokenKind.StarStar, TokenKind.Slash)), // *, **, /
      (OpKind.Binary, List(TokenKind.AngledPlus)), // <+>
      (OpKind.Unary, List(TokenKind.KeywordDiscard)), // discard
      (OpKind.Binary, List(TokenKind.InfixFunction)), // `my_function`
      (OpKind.Binary, List(TokenKind.UserDefinedOperator, TokenKind.NameMath)), // +=+ user defined op like '+=+' or '++'
      (OpKind.Unary, List(TokenKind.KeywordLazy, TokenKind.KeywordForce, TokenKind.KeywordDeref)), // lazy, force, deref
      (OpKind.Unary, List(TokenKind.Plus, TokenKind.Minus, TokenKind.TripleTilde)), // +, -, ~~~
      (OpKind.Unary, List(TokenKind.KeywordNot))
    )

    // These operators are right associative, meaning for instance that "x :: y :: z" becomes "x :: (y :: z)" rather than "(x :: y) :: z"
    private val rightAssoc: List[TokenKind] = List(TokenKind.ColonColon, TokenKind.TripleColon) // FCons, FAppend

    private def rightBindsTighter(left: TokenKind, right: TokenKind, leftIsUnary: Boolean): Boolean = {
      def tightness(kind: TokenKind, opKind: OpKind = OpKind.Binary): Int = {
        PRECEDENCE.indexWhere { case (k, l) => k == opKind && l.contains(kind) }
      }

      val rt = tightness(right)
      if (rt == -1) {
        return false
      }
      val lt = tightness(left, if (leftIsUnary) OpKind.Unary else OpKind.Binary)
      if (lt == -1) {
        assert(left == TokenKind.Eof)
        return true
      }

      if (lt == rt && rightAssoc.contains(left)) true else rt > lt
    }

    /**
     * arguments -> '(' (argument (',' argument)* )? ')'
     */
    private def arguments()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(argument).zeroOrMore()
      close(mark, TreeKind.ArgumentList)
    }

    private def argument()(implicit s: State): Mark.Closed = {
      val mark = open()
      expression()
      if (eat(TokenKind.Equal)) {
        expression()
        close(mark, TreeKind.ArgumentNamed)
      } else {
        close(mark, TreeKind.Argument)
      }
    }

    private def exprDelimited(allowQualified: Boolean = true)(implicit s: State): Mark.Closed = {
      val mark = open()
      // Handle clearly delimited expressions
      nth(0) match {
        case TokenKind.ParenL => parenOrTupleOrLambda()
        case TokenKind.CurlyL => blockOrRecord()
        case TokenKind.KeywordIf => ifThenElse()
        case TokenKind.KeywordImport => letImport()
        case TokenKind.BuiltIn => intrinsic()
        case TokenKind.KeywordUse => exprUse()
        case TokenKind.KeywordRegion => region()
        case TokenKind.KeywordLet => letMatch()
        case TokenKind.KeywordSpawn => spawn()
        case TokenKind.KeywordPar => parYield()
        case TokenKind.KeywordDo => exprDo()
        case TokenKind.KeywordOpenVariant => exprOpen()
        case TokenKind.KeywordOpenVariantAs => openAs()
        case TokenKind.LiteralStringInterpolationL
             | TokenKind.LiteralDebugStringL => interpolatedString()
        case TokenKind.KeywordTypeMatch => typematch()
        case TokenKind.KeywordMatch => matchOrMatchLambda()
        case TokenKind.KeywordMaskedCast => uncheckedMaskingCast()
        case TokenKind.KeywordUncheckedCast => uncheckedCast()
        case TokenKind.KeywordCheckedECast => checkedEffectCast()
        case TokenKind.KeywordCheckedCast => checkedTypeCast()
        case TokenKind.KeywordChoose
             | TokenKind.KeywordChooseStar => restrictableChoose()
        case TokenKind.KeywordForeach => foreach()
        case TokenKind.KeywordForM => forM()
        case TokenKind.KeywordForA => forA()
        case TokenKind.KeywordRef => reference()
        case TokenKind.KeywordNew => newObject()
        case TokenKind.KeywordTry => exprTry()
        case TokenKind.KeywordSelect => select()
        case TokenKind.KeywordDebug
             | TokenKind.KeywordDebugBang
             | TokenKind.KeywordDebugBangBang => debug()
        case TokenKind.ListHash => listLiteral()
        case TokenKind.SetHash => setLiteral()
        case TokenKind.VectorHash => vectorLiteral()
        case TokenKind.ArrayHash => arrayLiteral()
        case TokenKind.MapHash => mapLiteral()
        case TokenKind.Annotation | TokenKind.KeywordDef | TokenKind.CommentDoc => letRecDef()
        case TokenKind.LiteralString
             | TokenKind.LiteralChar
             | TokenKind.LiteralFloat32
             | TokenKind.LiteralFloat64
             | TokenKind.LiteralBigDecimal
             | TokenKind.LiteralInt8
             | TokenKind.LiteralInt16
             | TokenKind.LiteralInt32
             | TokenKind.LiteralInt64
             | TokenKind.LiteralBigInt
             | TokenKind.KeywordTrue
             | TokenKind.KeywordFalse
             | TokenKind.KeywordNull
             | TokenKind.LiteralRegex => literal()
        case TokenKind.Underscore => if (nth(1) == TokenKind.ArrowThinR) unaryLambda() else name(NAME_VARIABLE)
        case TokenKind.KeywordStaticUppercase => static()
        case TokenKind.NameJava => name(NAME_JAVA, allowQualified = true)
        case TokenKind.NameLowerCase => if (nth(1) == TokenKind.ArrowThinR) unaryLambda() else name(NAME_DEFINITION)
        case TokenKind.NameUpperCase
             | TokenKind.NameMath
             | TokenKind.NameGreek => if (nth(1) == TokenKind.ArrowThinR) unaryLambda() else name(NAME_DEFINITION, allowQualified)
        case TokenKind.HashParenL => fixpointLambda()
        case TokenKind.KeywordInject => fixpointInject()
        case TokenKind.KeywordQuery => fixpointQuery()
        case TokenKind.KeywordSolve => fixpointSolve()
        case TokenKind.HashCurlyL => fixpointConstraintSet()
        case TokenKind.Minus
             | TokenKind.KeywordNot
             | TokenKind.Plus
             | TokenKind.TripleTilde
             | TokenKind.KeywordLazy
             | TokenKind.KeywordForce
             | TokenKind.KeywordDiscard
             | TokenKind.KeywordDeref => unary()
        case TokenKind.HoleVariable => holeVariable()
        case TokenKind.HoleNamed
             | TokenKind.HoleAnonymous => hole()
        case t => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected expression, found $t"))
      }
      close(mark, TreeKind.Expr.Expr)
    }

    private def debug()(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.KeywordDebug, TokenKind.KeywordDebugBang, TokenKind.KeywordDebugBangBang)))
      val mark = open()
      expectAny(List(TokenKind.KeywordDebug, TokenKind.KeywordDebugBang, TokenKind.KeywordDebugBangBang))
      expect(TokenKind.ParenL)
      expression()
      expect(TokenKind.ParenR)
      close(mark, TreeKind.Expr.Debug)
    }

    private def exprOpen()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordOpenVariant))
      val mark = open()
      expect(TokenKind.KeywordOpenVariant)
      name(NAME_QNAME, allowQualified = true)
      close(mark, TreeKind.Expr.OpenVariant)
    }

    private def openAs()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordOpenVariantAs))
      val mark = open()
      expect(TokenKind.KeywordOpenVariantAs)
      name(NAME_QNAME, allowQualified = true)
      expression()
      close(mark, TreeKind.Expr.OpenVariantAs)
    }

    private def exprDo()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordDo))
      val mark = open()
      expect(TokenKind.KeywordDo)
      name(NAME_QNAME, allowQualified = true)
      arguments()
      close(mark, TreeKind.Expr.Do)
    }

    private def fixpointLambda()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashParenL))
      val mark = open()
      Predicate.params()
      expect(TokenKind.ArrowThinR)
      expression()
      close(mark, TreeKind.Expr.FixpointLambda)
    }

    private def fixpointInject()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordInject))
      val mark = open()
      expect(TokenKind.KeywordInject)
      expression()
      while (eat(TokenKind.Comma) && !eof()) {
        expression()
      }
      expect(TokenKind.KeywordInto)
      name(NAME_PREDICATE)
      while (eat(TokenKind.Comma) && !eof()) {
        name(NAME_PREDICATE)
      }
      close(mark, TreeKind.Expr.FixpointInject)
    }

    private def fixpointSolve()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSolve))
      val mark = open()
      expect(TokenKind.KeywordSolve)
      expression()
      while (eat(TokenKind.Comma) && !eof()) {
        expression()
      }

      if (eat(TokenKind.KeywordProject)) {
        name(NAME_PREDICATE)
        while (eat(TokenKind.Comma) && !eof()) {
          name(NAME_PREDICATE)
        }
      }
      close(mark, TreeKind.Expr.FixpointSolveWithProject)
    }

    private def fixpointQuery()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordQuery))
      val mark = open()
      expect(TokenKind.KeywordQuery)
      expression()
      while (eat(TokenKind.Comma) && !eof()) {
        expression()
      }
      fixpointQuerySelect()
      fixpointQueryFrom()
      if (at(TokenKind.KeywordWhere)) {
        fixpointQueryWhere()
      }
      close(mark, TreeKind.Expr.FixpointQuery)
    }

    private def fixpointQuerySelect()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSelect))
      val mark = open()
      expect(TokenKind.KeywordSelect)
      (nth(0), nth(1)) match {
        case (TokenKind.ParenL, TokenKind.ParenR) => expression()
        case (TokenKind.ParenL, _) => separated(() => expression()).zeroOrMore()
        case _ => expression()
      }
      close(mark, TreeKind.Expr.FixpointSelect)
    }

    private def fixpointQueryFrom()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordFrom))
      val mark = open()
      expect(TokenKind.KeywordFrom)
      Predicate.atom()
      while (eat(TokenKind.Comma) && !eof()) {
        Predicate.atom()
      }
      close(mark, TreeKind.Expr.FixpointFromFragment)
    }

    private def fixpointQueryWhere()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordWhere))
      val mark = open()
      expect(TokenKind.KeywordWhere)
      expression()
      close(mark, TreeKind.Expr.FixpointWhere)
    }

    private def fixpointConstraintSet()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashCurlyL))
      val mark = open()
      expect(TokenKind.HashCurlyL)
      while (!at(TokenKind.CurlyR) && !eof()) {
        fixpointConstraint()
      }
      expect(TokenKind.CurlyR)
      close(mark, TreeKind.Expr.FixpointConstraintSet)
    }

    private def fixpointConstraint()(implicit s: State): Mark.Closed = {
      val mark = open()
      Predicate.head()
      if (eat(TokenKind.ColonMinus)) {
        Predicate.body()
        while (eat(TokenKind.Comma) && !eof()) {
          Predicate.body()
        }
      }
      expect(TokenKind.Dot)
      close(mark, TreeKind.Expr.FixpointConstraint)
    }

    private def newObject()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordNew))
      val mark = open()
      expect(TokenKind.KeywordNew)
      Type.ttype()
      expect(TokenKind.CurlyL)
      while (at(TokenKind.KeywordDef) && !eof()) {
        jvmMethod()
      }
      expect(TokenKind.CurlyR)
      close(mark, TreeKind.Expr.NewObject)
    }

    private def jvmMethod()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordDef))
      val mark = open()
      expect(TokenKind.KeywordDef)
      name(NAME_JAVA)
      Decl.parameters()
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      expect(TokenKind.Equal)
      Expr.statement()
      close(mark, TreeKind.Expr.JvmMethod)
    }

    private def parYield()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordPar))
      val mark = open()
      expect(TokenKind.KeywordPar)
      separated(parYieldFragment)
        .by(TokenKind.Semi)
        .zeroOrMore()
      expect(TokenKind.KeywordYield)
      expression()
      close(mark, TreeKind.Expr.ParYield)
    }

    private def parYieldFragment()(implicit s: State): Mark.Closed = {
      val mark = open()
      Pattern.pattern()
      expect(TokenKind.ArrowThinL)
      expression()
      close(mark, TreeKind.Expr.ParYieldFragment)
    }

    private def spawn()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSpawn))
      val mark = open()
      expect(TokenKind.KeywordSpawn)
      expression()
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.Spawn)
    }

    private def foreach()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordForeach))
      val mark = open()
      var kind: TreeKind = TreeKind.Expr.Foreach
      expect(TokenKind.KeywordForeach)
      if (at(TokenKind.ParenL)) {
        forFragments()
      }
      if (eat(TokenKind.KeywordYield)) {
        kind = TreeKind.Expr.ForeachYield
      }
      expression()
      close(mark, kind)
    }

    private def forA()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordForA))
      val mark = open()
      expect(TokenKind.KeywordForA)

      separated(() => {
        val mark = open()
        Pattern.pattern()
        expect(TokenKind.ArrowThinL)
        expression()
        close(mark, TreeKind.Expr.ForFragmentGenerator)
      }).by(TokenKind.Semi).zeroOrMore()

      expect(TokenKind.KeywordYield)
      expression()
      close(mark, TreeKind.Expr.ForApplicative)
    }

    private def forM()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordForM))
      val mark = open()
      expect(TokenKind.KeywordForM)
      if (at(TokenKind.ParenL)) {
        forFragments()
      }
      if (eat(TokenKind.KeywordYield)) {
        expression()
      }
      close(mark, TreeKind.Expr.ForMonadic)
    }

    private def forFragments()(implicit s: State): Unit = {
      assert(at(TokenKind.ParenL))
      expect(TokenKind.ParenL)
      while (!at(TokenKind.ParenR) && !eof()) {
        if (at(TokenKind.KeywordIf)) {
          guardFragment()
        } else {
          generatorOrLetFragment()
        }
        if (!at(TokenKind.ParenR)) {
          expect(TokenKind.Semi)
        }
      }
      expect(TokenKind.ParenR)
    }

    private def guardFragment()(implicit s: State): Unit = {
      assert(at(TokenKind.KeywordIf))
      val mark = open()
      expect(TokenKind.KeywordIf)
      expression()
      close(mark, TreeKind.Expr.ForFragmentGuard)
    }

    private def generatorOrLetFragment()(implicit s: State): Unit = {
      val mark = open()
      Pattern.pattern()
      val isGenerator = eat(TokenKind.ArrowThinL)
      if (!isGenerator) {
        expect(TokenKind.Equal)
      }
      expression()
      close(mark, if (isGenerator) {
        TreeKind.Expr.ForFragmentGenerator
      } else {
        TreeKind.Expr.ForFragmentLet
      })
    }

    private def select()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSelect))
      val mark = open()
      expect(TokenKind.KeywordSelect)

      expect(TokenKind.CurlyL)
      var continue = true
      while (continue && at(TokenKind.KeywordCase) && !eof()) {
        val ruleMark = open()
        expect(TokenKind.KeywordCase)
        val isDefault = findBefore(TokenKind.ArrowThickR, List(TokenKind.ArrowThinL))
        if (isDefault) {
          // Only the last rule can be a wildcard rule so stop after this one
          continue = false
          expect(TokenKind.Underscore)
          expect(TokenKind.ArrowThickR)
          statement()
          close(ruleMark, TreeKind.Expr.SelectRuleDefaultFragment)
        } else {
          name(NAME_VARIABLE)
          expect(TokenKind.ArrowThinL)
          // NB. Only "Channel.recv" and "recv" are allowed for this name.
          // We don't want to reserve "Channel" and "recv" as keywords,
          // so parsing it as a qname seems fine for now.
          name(NAME_QNAME, allowQualified = true)
          expect(TokenKind.ParenL)
          expression()
          expect(TokenKind.ParenR)
          expect(TokenKind.ArrowThickR)
          statement()
          close(ruleMark, TreeKind.Expr.SelectRuleFragment)
        }
      }
      expect(TokenKind.CurlyR)

      close(mark, TreeKind.Expr.Select)
    }

    private def exprTry()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordTry))
      val mark = open()
      expect(TokenKind.KeywordTry)
      expression()
      if (at(TokenKind.KeywordCatch)) {
        while (at(TokenKind.KeywordCatch)) {
          catchBody()
        }
      } else if (at(TokenKind.KeywordWith)) {
        while (at(TokenKind.KeywordWith)) {
          withBody()
        }
      }

      close(mark, TreeKind.Expr.Try)
    }

    private def catchBody()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCatch))
      val mark = open()
      expect(TokenKind.KeywordCatch)
      separated(catchRule)
        .by(TokenKind.Comma, optional = true)
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.TryCatchBodyFragment)
    }

    private def catchRule()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCase))
      val mark = open()
      expect(TokenKind.KeywordCase)
      name(NAME_VARIABLE)
      if (eat(TokenKind.Colon)) {
        name(NAME_JAVA, allowQualified = true)
      }
      if (eat(TokenKind.ArrowThickR)) {
        expression()
      }
      close(mark, TreeKind.Expr.TryCatchRuleFragment)
    }

    private def withBody()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordWith))
      val mark = open()
      expect(TokenKind.KeywordWith)
      name(NAME_EFFECT, allowQualified = true)
      separated(withRule)
        .by(TokenKind.Comma, optional = true)
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.TryWithBodyFragment)
    }

    private def withRule()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordDef))
      val mark = open()
      expect(TokenKind.KeywordDef)
      name(List(TokenKind.NameLowerCase))
      Decl.parameters()
      if (eat(TokenKind.Equal)) {
        expression()
      }
      close(mark, TreeKind.Expr.TryWithRuleFragment)
    }

    private def ifThenElse()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordIf))
      val mark = open()
      expect(TokenKind.KeywordIf)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      expression()
      if (at(TokenKind.KeywordElse)) {
        expect(TokenKind.KeywordElse)
        expression()
      }
      close(mark, TreeKind.Expr.IfThenElse)
    }

    private def listLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ListHash))
      val mark = open()
      expect(TokenKind.ListHash)
      separated(() => expression())
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.LiteralList)
    }

    private def static()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStaticUppercase))
      val mark = open()
      expect(TokenKind.KeywordStaticUppercase)
      close(mark, TreeKind.Expr.Static)
    }

    private def setLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.SetHash))
      val mark = open()
      expect(TokenKind.SetHash)
      separated(() => expression())
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.LiteralSet)
    }

    private def vectorLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.VectorHash))
      val mark = open()
      expect(TokenKind.VectorHash)
      separated(() => expression())
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.LiteralVector)
    }

    private def arrayLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ArrayHash))
      val mark = open()
      expect(TokenKind.ArrayHash)
      separated(() => expression())
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.LiteralArray)
    }

    private def mapLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.MapHash))
      val mark = open()
      expect(TokenKind.MapHash)
      separated(mapLiteralValue)
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.LiteralMap)
    }

    private def mapLiteralValue()(implicit s: State): Mark.Closed = {
      val mark = open()
      expression()
      if (eat(TokenKind.ArrowThickR)) {
        expression()
      }
      close(mark, TreeKind.Expr.LiteralMapKeyValueFragment)
    }

    private def matchOrMatchLambda()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordMatch))
      val mark = open()
      expect(TokenKind.KeywordMatch)

      // Detect match lambda
      // TODO: This is probably not a good way to do this.
      val isLambda = {
        var lookAhead = 0
        var isLambda = false
        var continue = true
        // We need to track the parenthesis nesting level to handle match-expressions
        // that include lambdas. IE. "match f(x -> g(x)) { case ... }".
        // In these cases the ArrowThin __does not__ indicate that the expression being parsed is a match lambda.
        var parenNestingLevel = 0
        while (continue && !eof()) {
          nth(lookAhead) match {
            // match expr { case ... }
            case TokenKind.KeywordCase => continue = false
            // match pattern -> expr
            case TokenKind.ArrowThinR if parenNestingLevel == 0 => isLambda = true; continue = false
            case TokenKind.ParenL => parenNestingLevel += 1; lookAhead += 1
            case TokenKind.ParenR => parenNestingLevel -= 1; lookAhead += 1
            case _ => lookAhead += 1
          }
        }
        isLambda
      }

      if (isLambda) {
        Pattern.pattern()
        expect(TokenKind.ArrowThinR)
        expression()
        close(mark, TreeKind.Expr.LambdaMatch)
      } else {
        expression()
        if (eat(TokenKind.CurlyL)) {
          while (at(TokenKind.KeywordCase) && !eof()) {
            matchRule()
            // TODO: These two calls should be removed
            comments()
          }
          expect(TokenKind.CurlyR)
        }
        close(mark, TreeKind.Expr.Match)
      }
    }

    private def matchRule()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCase))
      val mark = open()
      expect(TokenKind.KeywordCase)
      Pattern.pattern()
      if (eat(TokenKind.KeywordIf)) {
        expression()
      }
      if (eat(TokenKind.ArrowThickR)) {
        statement()
      }
      close(mark, TreeKind.Expr.MatchRuleFragment)
    }

    private def letMatch()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordLet))
      val mark = open()
      expect(TokenKind.KeywordLet)
      Pattern.pattern()
      if (eat(TokenKind.Colon)) {
        Type.ttype()
      }
      expect(TokenKind.Equal)
      statement()
      close(mark, TreeKind.Expr.LetMatch)
    }

    private def reference()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordRef))
      val mark = open()
      expect(TokenKind.KeywordRef)
      expression()
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.Ref)
    }

    private def scopeName()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.At))
      val mark = open()
      expect(TokenKind.At)
      expression()
      close(mark, TreeKind.Expr.ScopeName)
    }

    private def letRecDef()(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.Annotation, TokenKind.KeywordDef, TokenKind.CommentDoc)))
      val mark = open()
      Decl.annotations()
      Decl.docComment()
      expect(TokenKind.KeywordDef)
      name(NAME_DEFINITION)
      val markParams = open()
      separated(() => {
        val mark = open()
        name(NAME_PARAMETER)
        if (eat(TokenKind.Colon)) {
          Type.ttype()
        }
        close(mark, TreeKind.Parameter)
      }, checkForItem = () => atAny(NAME_PARAMETER))
        .zeroOrMore()
      close(markParams, TreeKind.ParameterList)

      if (eat(TokenKind.Colon)) {
        Type.typeAndEffect()
      }
      expect(TokenKind.Equal)
      statement()
      close(mark, TreeKind.Expr.LetRecDef)
    }

    private def region()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordRegion))
      val mark = open()
      expect(TokenKind.KeywordRegion)
      name(NAME_VARIABLE)
      block()
      close(mark, TreeKind.Expr.Scope)
    }

    private def typematch()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordTypeMatch))
      val mark = open()
      expect(TokenKind.KeywordTypeMatch)
      expression()
      if (eat(TokenKind.CurlyL)) {
        while (at(TokenKind.KeywordCase) && !eof()) {
          typematchRule()
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Expr.TypeMatch)
    }

    private def typematchRule()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCase))
      val mark = open()
      expect(TokenKind.KeywordCase)
      name(NAME_VARIABLE)
      if (eat(TokenKind.Colon)) {
        Type.ttype()
      }
      if (eat(TokenKind.ArrowThickR)) {
        statement()
      }
      close(mark, TreeKind.Expr.TypeMatchRuleFragment)
    }

    private def checkedTypeCast()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCheckedCast))
      val mark = open()
      expect(TokenKind.KeywordCheckedCast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.CheckedTypeCast)
    }

    private def restrictableChoose()(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.KeywordChoose, TokenKind.KeywordChooseStar)))
      val mark = open()
      val isStar = eat(TokenKind.KeywordChooseStar)
      if (!isStar) {
        expect(TokenKind.KeywordChoose)
      }
      expression()
      expect(TokenKind.CurlyL)
      while (at(TokenKind.KeywordCase) && !eof()) {
        matchRule()
      }
      expect(TokenKind.CurlyR)
      close(mark, if (isStar) TreeKind.Expr.RestrictableChooseStar else TreeKind.Expr.RestrictableChoose)
    }

    private def checkedEffectCast()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCheckedECast))
      val mark = open()
      expect(TokenKind.KeywordCheckedECast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.CheckedEffectCast)
    }

    private def uncheckedCast()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordUncheckedCast))
      val mark = open()
      expect(TokenKind.KeywordUncheckedCast)
      if (eat(TokenKind.ParenL)) {
        expression()
        if (eat(TokenKind.KeywordAs)) {
          Type.typeAndEffect()
        }
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.UncheckedCast)
    }

    private def uncheckedMaskingCast()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordMaskedCast))
      val mark = open()
      expect(TokenKind.KeywordMaskedCast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.UncheckedMaskingCast)
    }

    private def intrinsic()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Expr.Intrinsic)
    }

    private def interpolatedString()(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.LiteralStringInterpolationL, TokenKind.LiteralDebugStringL)))
      val mark = open()
      var continue = eatAny(List(TokenKind.LiteralStringInterpolationL, TokenKind.LiteralDebugStringL))
      while (continue && !eof()) {
        expression()
        continue = eatAny(List(TokenKind.LiteralStringInterpolationL, TokenKind.LiteralDebugStringL))
      }
      expectAny(List(TokenKind.LiteralStringInterpolationR, TokenKind.LiteralDebugStringR))
      close(mark, TreeKind.Expr.StringInterpolation)
    }

    private def exprUse()(implicit s: State): Mark.Closed = {
      val mark = open()
      use()
      expect(TokenKind.Semi)
      statement()
      close(mark, TreeKind.Expr.Use)
    }

    /**
     * literal -> integer | float | boolean | string
     */
    private def literal()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Expr.Literal)
    }

    /**
     * exprParen -> '(' expression? (',' expression)* ')'
     */
    private def parenOrTupleOrLambda()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      (nth(0), nth(1)) match {
        // Detect unit tuple
        case (TokenKind.ParenL, TokenKind.ParenR) =>
          // Detect unit lambda: () -> expr
          if (nth(2) == TokenKind.ArrowThinR) {
            lambda()
          } else {
            val mark = open()
            advance()
            advance()
            close(mark, TreeKind.Expr.Tuple)
          }

        case (TokenKind.ParenL, _) =>
          // Detect lambda function declaration
          val isLambda = {
            var level = 1
            var lookAhead = 0
            while (level > 0 && !eof()) {
              lookAhead += 1
              nth(lookAhead) match {
                case TokenKind.ParenL => level += 1
                case TokenKind.ParenR => level -= 1
                case _ =>
              }
            }
            nth(lookAhead + 1) == TokenKind.ArrowThinR
          }

          if (isLambda) {
            lambda()
          } else {
            // Distinguish between expression in parenthesis and tuples
            var kind: TreeKind = TreeKind.Expr.Paren
            val mark = open()
            expect(TokenKind.ParenL)
            expression()
            if (at(TokenKind.Comma)) {
              kind = TreeKind.Expr.Tuple
              while (!at(TokenKind.ParenR) && !eof()) {
                expect(TokenKind.Comma)
                expression()
              }
            }
            expect(TokenKind.ParenR)
            close(mark, kind)
          }

        case (t1, _) => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected ParenL, found '$t1'"))
      }
    }

    private def unaryLambda()(implicit s: State): Mark.Closed = {
      val mark = open()
      val markParams = open()
      val markParam = open()
      name(NAME_PARAMETER)
      close(markParam, TreeKind.Parameter)
      close(markParams, TreeKind.ParameterList)
      expect(TokenKind.ArrowThinR)
      expression()
      close(mark, TreeKind.Expr.Lambda)
    }

    private def lambda()(implicit s: State): Mark.Closed = {
      val mark = open()
      val markParams = open()
      separated(() => {
        val mark = open()
        name(NAME_PARAMETER)
        if (eat(TokenKind.Colon)) {
          Type.ttype()
        }
        close(mark, TreeKind.Parameter)
      }, checkForItem = () => atAny(NAME_PARAMETER))
        .zeroOrMore()

      close(markParams, TreeKind.ParameterList)
      expect(TokenKind.ArrowThinR)
      expression()
      close(mark, TreeKind.Expr.Lambda)
    }

    private def blockOrRecord()(implicit s: State): Mark.Closed = {
      // Detemines if a '{' is opening a block, a record literal or a record operation.
      assert(at(TokenKind.CurlyL))
      // If a '|' occurs before '}' or '{' then we are dealing with a record operation.
      if (findBefore(TokenKind.Bar, before = List(TokenKind.CurlyL, TokenKind.CurlyR))) {
        recordOperation()
      } else {
        // Otherwise a record literal can be detected by looking at the two next tokens.
        (nth(1), nth(2)) match {
          case (TokenKind.CurlyR, _)
               | (TokenKind.NameLowerCase, TokenKind.Equal) => recordLiteral()
          case _ => block()
        }
      }
    }

    private def recordLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      separated(recordLiteralField)
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore()
      close(mark, TreeKind.Expr.LiteralRecord)
    }

    private def recordLiteralField()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_FIELD)
      expect(TokenKind.Equal)
      expression()
      close(mark, TreeKind.Expr.LiteralRecordFieldFragment)
    }

    private def recordOperation()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      separated(recordOp)
        .within(TokenKind.CurlyL, TokenKind.CurlyR)
        .zeroOrMore(followedBy = Some((TokenKind.Bar, () => expression())))

      close(mark, TreeKind.Expr.RecordOperation)
    }

    private def recordOp()(implicit s: State): Mark.Closed = {
      val mark = open()
      nth(0) match {
        case TokenKind.Plus =>
          advance()
          name(NAME_FIELD)
          expect(TokenKind.Equal)
          expression()
          close(mark, TreeKind.Expr.RecordOpExtend)
        case TokenKind.Minus =>
          advance()
          name(NAME_FIELD)
          close(mark, TreeKind.Expr.RecordOpRestrict)
        case TokenKind.NameLowerCase =>
          name(NAME_FIELD)
          expect(TokenKind.Equal)
          expression()
          close(mark, TreeKind.Expr.RecordOpUpdate)
        case k => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected record operation but found $k"))
      }
    }

    /**
     * block -> '{' expression? '}'
     */
    private def block()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      expect(TokenKind.CurlyL)
      if (eat(TokenKind.CurlyR)) { // Handle empty block
        return close(mark, TreeKind.Expr.LiteralRecord)
      }
      statement()
      expect(TokenKind.CurlyR)
      close(mark, TreeKind.Expr.Block)
    }

    private def unary()(implicit s: State): Mark.Closed = {
      val mark = open()
      val op = nth(0)
      val markOp = open()
      expectAny(List(TokenKind.Minus, TokenKind.KeywordNot, TokenKind.Plus, TokenKind.TripleTilde, TokenKind.KeywordLazy, TokenKind.KeywordForce, TokenKind.KeywordDiscard, TokenKind.KeywordDeref))
      close(markOp, TreeKind.Operator)
      expression(left = op, leftIsUnary = true)
      var lhs = close(mark, TreeKind.Expr.Unary)
      // handle ascription after a unary
      if (eat(TokenKind.Colon)) {
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
        Type.ttype()
        close(openBefore(lhs), TreeKind.Expr.Ascribe)
      } else {
        lhs
      }
    }

    private def letImport()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordImport))
      val mark = open()
      expect(TokenKind.KeywordImport)
      val markJvmOp = open()
      nth(0) match {
        case TokenKind.KeywordNew => JvmOp.constructor()
        case TokenKind.KeywordGet => JvmOp.getField()
        case TokenKind.KeywordSet => JvmOp.putField()
        case TokenKind.KeywordStatic => nth(1) match {
          case TokenKind.KeywordGet => JvmOp.staticGetField()
          case TokenKind.KeywordSet => JvmOp.staticPutField()
          case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.staticMethod()
          case _ => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "expected static java import"))
        }
        case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.method()
        case _ => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "expected java import"))
      }
      close(markJvmOp, TreeKind.JvmOp.JvmOp)
      expect(TokenKind.Semi)
      statement()
      close(mark, TreeKind.Expr.LetImport)
    }

    private def holeVariable()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HoleVariable))
      val mark = open()
      name(List(TokenKind.HoleVariable))
      close(mark, TreeKind.Expr.HoleVariable)
    }

    private def hole()(implicit s: State): Mark.Closed = {
      assert(atAny(List(TokenKind.HoleNamed, TokenKind.HoleAnonymous)))
      val mark = open()
      nth(0) match {
        case TokenKind.HoleAnonymous => advance(); close(mark, TreeKind.Expr.Hole)
        case TokenKind.HoleNamed => name(List(TokenKind.HoleNamed)); close(mark, TreeKind.Expr.Hole)
        case _ => throw InternalCompilerException("Parser assert missed case", currentSourceLocation())
      }
    }
  }

  private object Pattern {
    def pattern()(implicit s: State): Mark.Closed = {
      val mark = open()
      var lhs = nth(0) match {
        case TokenKind.ParenL => tuple()
        case TokenKind.NameUpperCase => tag()
        case TokenKind.CurlyL => record()
        case TokenKind.NameLowerCase
             | TokenKind.NameGreek
             | TokenKind.NameMath
             | TokenKind.Underscore
             | TokenKind.KeywordQuery => variable()
        case TokenKind.LiteralString
             | TokenKind.LiteralChar
             | TokenKind.LiteralFloat32
             | TokenKind.LiteralFloat64
             | TokenKind.LiteralBigDecimal
             | TokenKind.LiteralInt8
             | TokenKind.LiteralInt16
             | TokenKind.LiteralInt32
             | TokenKind.LiteralInt64
             | TokenKind.LiteralBigInt
             | TokenKind.KeywordTrue
             | TokenKind.KeywordFalse
             | TokenKind.LiteralRegex
             | TokenKind.KeywordNull => literal()
        case _ => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "expected pattern"))
      }

      // Handle FCons
      if (eat(TokenKind.ColonColon)) {
        lhs = close(openBefore(lhs), TreeKind.Pattern.Pattern)
        pattern()
        close(openBefore(lhs), TreeKind.Pattern.FCons)
      }

      close(mark, TreeKind.Pattern.Pattern)
    }

    private def literal()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Pattern.Literal)
    }

    private def variable()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_VARIABLE)
      close(mark, TreeKind.Pattern.Variable)
    }

    private def tag()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_TAG, allowQualified = true)
      if (at(TokenKind.ParenL)) {
        tuple()
      }
      close(mark, TreeKind.Pattern.Tag)
    }

    private def tuple()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      separated(pattern).zeroOrMore()
      close(mark, TreeKind.Pattern.Tuple)
    }

    private def record()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      expect(TokenKind.CurlyL)
      var continue = true
      while (continue && !atAny(List(TokenKind.CurlyR, TokenKind.Bar)) && !eof()) {
        recordField()
        if (!atAny(List(TokenKind.CurlyR, TokenKind.Bar))) {
          expect(TokenKind.Comma)
        }
        if (eat(TokenKind.Bar)) {
          pattern()
          continue = false
        }
      }

      expect(TokenKind.CurlyR)
      close(mark, TreeKind.Pattern.Record)
    }

    private def recordField()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_FIELD)
      if (eat(TokenKind.Equal)) {
        pattern()
      }
      close(mark, TreeKind.Pattern.RecordFieldFragment)
    }
  }

  private object Type {
    def typeAndEffect()(implicit s: State): Mark.Closed = {
      val lhs = ttype()
      if (eat(TokenKind.Backslash)) {
        effectSet()
      } else lhs
    }

    /**
     * ttype -> (typeDelimited arguments? | typeFunction) ( '\' effectSet )?
     */
    def ttype(left: TokenKind = TokenKind.Eof)(implicit s: State): Mark.Closed = {
      var lhs = if (left == TokenKind.ArrowThinR) typeAndEffect() else typeDelimited()

      // handle Type argument application
      while (at(TokenKind.BracketL)) {
        val mark = openBefore(lhs)
        arguments()
        lhs = close(mark, TreeKind.Type.Apply)
        // Wrap Apply in Type.Type
        lhs = close(openBefore(lhs), TreeKind.Type.Type)
      }

      // Handle binary operators
      var continue = true
      while (continue) {
        val right = nth(0)
        if (rightBindsTighter(left, right)) {
          val mark = openBefore(lhs)
          val markOp = open()
          advance()
          close(markOp, TreeKind.Operator)
          ttype(right)
          lhs = close(mark, TreeKind.Type.Binary)
          lhs = close(openBefore(lhs), TreeKind.Type.Type)
        } else {
          continue = false
        }
      }

      // Handle kind ascriptions
      if (eat(TokenKind.Colon)) {
        Type.kind()
        lhs = close(openBefore(lhs), TreeKind.Type.Ascribe)
        lhs = close(openBefore(lhs), TreeKind.Type.Type)
      }

      lhs
    }

    // A precedence table for binary operators in types, lower is higher precedence
    private def TYPE_OP_PRECEDENCE: List[List[TokenKind]] = List(
      // BINARY OPS
      List(TokenKind.ArrowThinR), // ->
      // TODO: NB. UserDefinedOperator only really means '++' here. This is checked in the weeder
      // But since '++' is used as a custom operator in 'Semigroup.combine' it needs to be a user operator.
      List(TokenKind.UserDefinedOperator, TokenKind.MinusMinus),
      List(TokenKind.AmpersandAmpersand), // &&
      List(TokenKind.Plus, TokenKind.Minus), // +, -
      List(TokenKind.Ampersand), // &
      List(TokenKind.KeywordXor), // xor
      List(TokenKind.KeywordOr), // or
      List(TokenKind.KeywordAnd), // and
      List(TokenKind.Colon), // :
      // UNARY OPS
      List(TokenKind.TildeTilde, TokenKind.Tilde, TokenKind.KeywordNot) // ~~~, ~, not
    )

    private def rightBindsTighter(left: TokenKind, right: TokenKind): Boolean = {
      val rt = TYPE_OP_PRECEDENCE.indexWhere(l => l.contains(right))
      if (rt == -1) {
        return false
      }

      val lt = TYPE_OP_PRECEDENCE.indexWhere(l => l.contains(left))
      if (lt == -1) {
        assert(left == TokenKind.Eof)
        return true
      }
      // NOTE: This >= rather than > makes it so that operators with equal precedence are left-associative.
      // IE. 't + eff1 + eff2' becomes '(t + eff1) + eff2' rather than 't + (eff1 + eff2)'
      rt >= lt
    }

    /**
     * arguments -> '[' ( argument ( ',' argument )* ) ']'?
     */
    def arguments()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(argument)
        .within(TokenKind.BracketL, TokenKind.BracketR)
        .zeroOrMore()
      close(mark, TreeKind.Type.ArgumentList)
    }

    def argument()(implicit s: State): Mark.Closed = {
      val mark = open()
      ttype()
      close(mark, TreeKind.Type.Argument)
    }

    /**
     * parameters -> '[' (typeParameter (',' typeParameter)* )? ']'
     */
    def parameters()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(parameter, () => atAny(NAME_VARIABLE ++ NAME_TYPE))
        .within(TokenKind.BracketL, TokenKind.BracketR)
        .zeroOrMore()
      close(mark, TreeKind.TypeParameterList)
    }

    def parameter()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_VARIABLE ++ NAME_TYPE)
      if (at(TokenKind.Colon)) {
        expect(TokenKind.Colon)
        Type.kind()
      }
      close(mark, TreeKind.Parameter)
    }

    def constraints()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordWith))
      val mark = open()
      expect(TokenKind.KeywordWith)
      var continue = true
      constraint()
      while (continue && !eof()) {
        if (eat(TokenKind.Comma)) {
          constraint()
        } else {
          continue = false
        }
      }
      if (at(TokenKind.Comma)) {
        advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "Trailing comma."))
      }
      close(mark, TreeKind.Type.ConstraintList)
    }

    private def constraint()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_DEFINITION, allowQualified = true)
      expect(TokenKind.BracketL)
      Type.ttype()
      expect(TokenKind.BracketR)
      close(mark, TreeKind.Type.Constraint)
    }

    def derivations()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordWith))
      val mark = open()
      expect(TokenKind.KeywordWith)
      var continue = true
      name(NAME_QNAME, allowQualified = true)
      while (continue && !eof()) {
        if (eat(TokenKind.Comma)) {
          name(NAME_QNAME, allowQualified = true)
        } else {
          continue = false
        }
      }
      if (at(TokenKind.Comma)) {
        advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "Trailing comma."))
      }
      close(mark, TreeKind.DerivationList)
    }

    /**
     * typeDelimited -> record | tuple | typeName | variable
     * Detects clearly delimited types such as names, records and tuples
     */
    private def typeDelimited()(implicit s: State): Mark.Closed = {
      val mark = open()
      nth(0) match {
        case TokenKind.CurlyL => recordOrEffectSet()
        case TokenKind.HashCurlyL => schema()
        case TokenKind.HashParenL => schemaRow()
        case TokenKind.AngleL => caseSet()
        case TokenKind.ParenL => tupleOrRecordRow()
        case TokenKind.NameUpperCase => name(NAME_TYPE, allowQualified = true)
        case TokenKind.NameJava => native()
        case TokenKind.NameLowerCase => variable()
        case TokenKind.NameMath
             | TokenKind.NameGreek
             | TokenKind.Underscore => name(NAME_VARIABLE)
        case TokenKind.KeywordUniv
             | TokenKind.KeywordPure
             | TokenKind.KeywordFalse
             | TokenKind.KeywordTrue => constant()
        case TokenKind.KeywordNot
             | TokenKind.Tilde
             | TokenKind.TildeTilde => unary()
        // TODO: Static is used as a type name in std.lib but that should be an error since 'Static' is a reserved keyword
        case TokenKind.KeywordStaticUppercase => name(List(TokenKind.KeywordStaticUppercase))
        case t => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected type, found $t"))
      }
      close(mark, TreeKind.Type.Type)
    }

    private def caseSet()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.AngleL))
      val mark = open()
      separated(() => name(NAME_DEFINITION, allowQualified = true))
        .within(TokenKind.AngleL, TokenKind.AngleR)
        .zeroOrMore()
      close(mark, TreeKind.Type.CaseSet)
    }

    private def schema()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashCurlyL))
      val mark = open()
      separated(schemaTerm)
        .within(TokenKind.HashCurlyL, TokenKind.CurlyR)
        .zeroOrMore(followedBy = Some((TokenKind.Bar, () => name(NAME_VARIABLE))))
      close(mark, TreeKind.Type.Schema)
    }

    private def schemaRow()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashParenL))
      val mark = open()
      separated(schemaTerm)
        .within(TokenKind.HashParenL, TokenKind.ParenR)
        .zeroOrMore(followedBy = Some((TokenKind.Bar, () => name(NAME_VARIABLE))))
      close(mark, TreeKind.Type.SchemaRow)
    }

    private def schemaTerm()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PREDICATE, allowQualified = true)
      if (at(TokenKind.BracketL)) {
        arguments()
        close(mark, TreeKind.Type.PredicateWithAlias)
      } else {
        separated(() => ttype()).zeroOrMore(followedBy = Some(TokenKind.Semi, () => {
          val mark = open()
          ttype()
          close(mark, TreeKind.Predicate.LatticeTerm)
        }))
        close(mark, TreeKind.Type.PredicateWithTypes)
      }
    }

    private def unary()(implicit s: State): Mark.Closed = {
      val mark = open()
      val op = nth(0)
      val markOp = open()
      expectAny(List(TokenKind.Tilde, TokenKind.KeywordNot, TokenKind.TildeTilde))
      close(markOp, TreeKind.Operator)
      ttype(left = op)
      close(mark, TreeKind.Type.Unary)
    }

    private def constant()(implicit s: State): Mark.Closed = {
      val mark = open()
      expectAny(List(TokenKind.KeywordUniv, TokenKind.KeywordPure, TokenKind.KeywordFalse, TokenKind.KeywordTrue))
      close(mark, TreeKind.Type.Constant)
    }

    private def variable()(implicit s: State): Mark.Closed = {
      val mark = open()
      expectAny(List(TokenKind.NameLowerCase, TokenKind.NameGreek, TokenKind.NameMath, TokenKind.Underscore))
      close(mark, TreeKind.Type.Variable)
    }

    private def native()(implicit s: State): Mark.Closed = {
      val mark = open()
      var continue = true
      while (continue && !eof()) {
        nth(0) match {
          case TokenKind.NameJava
               | TokenKind.NameUpperCase
               | TokenKind.NameLowerCase
               | TokenKind.Dot
               | TokenKind.Dollar => advance()
          case _ => continue = false
        }
      }
      close(mark, TreeKind.Type.Native)
    }

    def tupleOrRecordRow()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      // Record rows follow the rule '(' (name '=' type)* ('|' name)? )
      // So the prefix is always "( name '='" or "'|'"
      val next = nth(1)
      val nextnext = nth(2)
      val isRecordRow = next == TokenKind.Bar || next == TokenKind.NameLowerCase && nextnext == TokenKind.Equal || next == TokenKind.ParenR
      if (isRecordRow) {
        recordRow()
      } else {
        tuple()
      }
    }

    def tuple()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      separated(() => ttype()).zeroOrMore()
      close(mark, TreeKind.Type.Tuple)
    }

    def recordRow()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      expect(TokenKind.ParenL)
      while (!atAny(List(TokenKind.ParenR, TokenKind.Bar)) && !eof()) {
        recordField()
      }

      if (at(TokenKind.Comma)) {
        advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "Trailing comma."))
      }

      if (at(TokenKind.Bar)) {
        val mark = open()
        expect(TokenKind.Bar)
        name(NAME_VARIABLE)
        close(mark, TreeKind.Type.RecordVariable)
      }

      expect(TokenKind.ParenR)
      close(mark, TreeKind.Type.RecordRow)
    }

    private def recordOrEffectSet()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val isRecord = (nth(1), nth(2)) match {
        case (TokenKind.CurlyR, _)
             | (TokenKind.Bar, _)
             | (_, TokenKind.Bar)
             | (_, TokenKind.Equal) => true
        case _ => false
      }
      if (isRecord) record() else effectSet()
    }

    /**
     * record -> '{' (typeRecordField (',' typeRecordField)* )? ('|' Name.Variable)| '}'
     */
    private def record()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      expect(TokenKind.CurlyL)
      while (!atAny(List(TokenKind.CurlyR, TokenKind.Bar)) && !eof()) {
        recordField()
      }

      if (at(TokenKind.Comma)) {
        advanceWithError(Parser2Error.DevErr(currentSourceLocation(), "Trailing comma."))
      }

      if (at(TokenKind.Bar)) {
        expect(TokenKind.Bar)
        variable()
      }

      expect(TokenKind.CurlyR)
      close(mark, TreeKind.Type.Record)
    }

    /**
     * typeRecordField -> Names.Field '=' ttype
     */
    private def recordField()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_FIELD)
      expect(TokenKind.Equal)
      ttype()
      if (!atAny(List(TokenKind.CurlyR, TokenKind.Bar))) {
        expect(TokenKind.Comma)
      }
      close(mark, TreeKind.Type.RecordFieldFragment)
    }

    /**
     * effectSet -> '\' effect | '{' ( effect ( ',' effect )* )? '}'
     */
    private def effectSet()(implicit s: State): Mark.Closed = {
      if (at(TokenKind.CurlyL)) {
        val mark = open()
        separated(() => ttype())
          .within(TokenKind.CurlyL, TokenKind.CurlyR)
          .zeroOrMore()
        close(mark, TreeKind.Type.EffectSet)
      } else {
        val mark = open()
        ttype()
        close(mark, TreeKind.Type.EffectSet)
      }
    }

    def kind()(implicit s: State): Mark.Closed = {
      val mark = open()
      if (eat(TokenKind.ParenL)) {
        kind()
        expect(TokenKind.ParenR)
      } else {
        name(NAME_KIND)
      }

      if (eat(TokenKind.ArrowThinR)) {
        kind()
      }

      close(mark, TreeKind.Kind)
    }
  }

  private object Predicate {
    def head()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PREDICATE)
      termList()
      close(mark, TreeKind.Predicate.Head)
    }

    private def termList()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      separated(() => Expr.expression())
        .zeroOrMore(followedBy = Some((TokenKind.Semi, () => {
          val mark = open()
          Expr.expression()
          close(mark, TreeKind.Predicate.LatticeTerm)
        })))
      close(mark, TreeKind.Predicate.TermList)
    }

    private def patternList()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(Pattern.pattern)
        .zeroOrMore(followedBy = Some((TokenKind.Semi, () => {
          val mark = open()
          Pattern.pattern()
          close(mark, TreeKind.Predicate.LatticeTerm)
        })))
      close(mark, TreeKind.Predicate.PatternList)
    }

    def body()(implicit s: State): Mark.Closed = {
      val mark = open()
      nth(0) match {
        case TokenKind.KeywordIf => guard()
        case TokenKind.KeywordLet => functional()
        case TokenKind.KeywordNot | TokenKind.KeywordFix | TokenKind.NameUpperCase => atom()
        case k => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected predicate body but found $k"))
      }
      close(mark, TreeKind.Predicate.Body)
    }

    private def guard()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordIf))
      val mark = open()
      expect(TokenKind.KeywordIf)
      Expr.expression(allowQualified = false)
      close(mark, TreeKind.Predicate.Guard)
    }

    private def functional()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordLet))
      val mark = open()
      expect(TokenKind.KeywordLet)
      nth(0) match {
        case TokenKind.ParenL => separated(() => name(NAME_VARIABLE)).zeroOrMore()
        case TokenKind.NameLowerCase
             | TokenKind.NameMath
             | TokenKind.NameGreek
             | TokenKind.Underscore => name(NAME_VARIABLE)
        case k => advanceWithError(Parser2Error.DevErr(currentSourceLocation(), s"Expected ${TokenKind.ParenL} or name but found $k"))
      }
      expect(TokenKind.Equal)
      Expr.expression(allowQualified = false)
      close(mark, TreeKind.Predicate.Functional)
    }

    def atom()(implicit s: State): Mark.Closed = {
      val mark = open()
      eat(TokenKind.KeywordNot)
      eat(TokenKind.KeywordFix)
      name(NAME_PREDICATE)
      patternList()
      close(mark, TreeKind.Predicate.Atom)
    }

    def params()(implicit s: State): Mark.Closed = {
      val mark = open()
      separated(Predicate.param).within(TokenKind.HashParenL, TokenKind.ParenR).zeroOrMore()
      close(mark, TreeKind.Predicate.ParamList)
    }

    def param()(implicit s: State): Mark.Closed = {
      var kind: TreeKind = TreeKind.Predicate.ParamUntyped
      val mark = open()
      name(NAME_PREDICATE)
      if (at(TokenKind.ParenL)) {
        kind = TreeKind.Predicate.Param
        separated(() => Type.ttype())
          .zeroOrMore(followedBy = Some((TokenKind.Semi, () => {
            val mark = open()
            Type.ttype()
            close(mark, TreeKind.Predicate.LatticeTerm)
          })))
      }
      close(mark, kind)
    }

  }

  private object JvmOp {
    private def signature()(implicit s: State): Unit = {
      if (at(TokenKind.ParenL)) {
        val mark = open()
        separated(() => Type.ttype()).zeroOrMore()
        close(mark, TreeKind.JvmOp.Sig)
      }
    }

    private def ascription()(implicit s: State): Mark.Closed = {
      val mark = open()
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      close(mark, TreeKind.JvmOp.Ascription)
    }

    def constructor()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordNew))
      val mark = open()
      expect(TokenKind.KeywordNew)
      name(NAME_JAVA, allowQualified = true)
      signature()
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
      close(mark, TreeKind.JvmOp.Constructor)
    }

    private def methodBody()(implicit s: State): Unit = {
      name(NAME_JAVA, allowQualified = true)
      signature()
      ascription()
      if (eat(TokenKind.KeywordAs)) {
        name(NAME_VARIABLE)
      }
    }

    def method()(implicit s: State): Mark.Closed = {
      val mark = open()
      methodBody()
      close(mark, TreeKind.JvmOp.Method)
    }

    def staticMethod()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      methodBody()
      close(mark, TreeKind.JvmOp.StaticMethod)
    }

    private def getBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordGet)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    private def putBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordSet)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    def getField()(implicit s: State): Mark.Closed = {
      val mark = open()
      getBody()
      close(mark, TreeKind.JvmOp.GetField)
    }

    def staticGetField()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      getBody()
      close(mark, TreeKind.JvmOp.StaticGetField)
    }

    def putField()(implicit s: State): Mark.Closed = {
      val mark = open()
      putBody()
      close(mark, TreeKind.JvmOp.PutField)
    }

    def staticPutField()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      putBody()
      close(mark, TreeKind.JvmOp.StaticPutField)
    }
  }

  // A helper function that runs both the old and the new parser for comparison.
  // Both [[WeededAst]]s are printed for diffing to find inconsistencies.
  // Example of diffing with git:
  // >./gradlew run --args="--Xlib=nix foo.flix" > run.txt;
  //   sed -n '/\[\[\[ OLD PARSER \]\]\]/,/\[\[\[ NEW PARSER \]\]\]/p' run.txt > old.txt &&
  //   sed -n '/\[\[\[ NEW PARSER \]\]\]/,/\[\[\[\[ END \]\]\]/p' run.txt > new.txt &&
  //   git difftool -y --no-index ./old.txt ./new.txt

  // A helper function for formatting pretty printing ASTs.
  // It is generic to scala objects with some special handling for source positions and locations.
  private def formatWeededAst(obj: Any, depth: Int = 0, matchingWithOldParser: Boolean = false, paramName: Option[String] = None): String = {
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case obj: SourcePosition => if (!matchingWithOldParser) s"SourcePosition (${obj.line}, ${obj.col})" else ""
      case obj: Ast.Doc => if (!matchingWithOldParser) s"Doc (${obj.lines})" else "Doc"
      case obj: SourceLocation => if (!matchingWithOldParser) s"SourceLocation (${obj.beginLine}, ${obj.beginCol}) -> (${obj.endLine}, ${obj.endCol})" else ""
      case _: Iterable[Any] => ""
      case obj: Product => obj.productPrefix
      case _ => obj.toString
    }

    val acc = s"$indent$prettyName$ptype\n"

    obj match {
      case _: SourceLocation => acc
      case _: SourcePosition => acc
      case _: Ast.Doc => acc
      case seq: Iterable[Any] =>
        acc + seq.map(formatWeededAst(_, depth + 1, matchingWithOldParser, None)).mkString("")
      case obj: Product =>
        acc + (obj.productIterator zip obj.productElementNames)
          .map { case (subObj, paramName) => formatWeededAst(subObj, depth + 1, matchingWithOldParser, Some(paramName)) }.mkString("")
      case _ => acc
    }
  }

  /**
   * Utility function that computes a textual representation of a [[SyntaxTree.Tree]].
   * Meant for debugging use.
   */
  def syntaxTreeToDebugString(tree: SyntaxTree.Tree, nesting: Int = 1): String = {
    s"${tree.kind} (${tree.loc.beginLine}, ${tree.loc.beginCol}) -> (${tree.loc.endLine}, ${tree.loc.endCol}) ${
      tree.children.map {
        case SyntaxTree.Child.TokenChild(token) => s"\n${"  " * nesting}'${token.text}'"
        case SyntaxTree.Child.TreeChild(tree) => s"\n${"  " * nesting}${syntaxTreeToDebugString(tree, nesting + 1)}"
      }.mkString("")
    }"
  }
}
