/*
 * Copyright 2024 Herluf Baggesen
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
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.{ParseError, WeederError}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import org.parboiled2.ParserInput
import scala.collection.mutable.ArrayBuffer

/**
  * A resilient LL parser.
  * Parses a stream of tokens into a [[SyntaxTree.Tree]].
  * This parser works in two steps:
  * 1. First the list of tokens is traversed while emitting Open, Advance and Close events.
  * Conceptually this is exactly the same as inserting parenthesis in a stream of tokens, only here each parenthesis is annotated with a kind.
  * For instance:
  * def main(): Int32 = 123
  * Becomes:
  * (Def 'def' (Name 'main' ) '(' ')' ':' (Type 'Int32' ) '=' (Literal '123' ) )
  * 2. The flat list of events is automatically turned into a SyntaxTree.Tree.
  *
  * This parser is adopted from 'Resilient LL Parsing Tutorial' by Alex Kladov who works on rust-analyzer.
  * The tutorial is also a great resource for understanding this parser (and a great read to boot!)
  * https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
  */
object Parser2 {

  private sealed trait Event

  /**
    * An event emitted by the parser while traversing a list of [[Token]]s
    */
  private object Event {
    /**
      * Opens a grouping of tokens with the [[TreeKind]] kind.
      */
    case class Open(kind: TreeKind) extends Event

    /**
      * Closed the most recently opened group.
      */
    case object Close extends Event

    /**
      * Advances one token adding it to the currently open group.
      */
    case object Advance extends Event
  }

  private class State(val tokens: Array[Token], val src: Ast.Source) {
    /**
      * The current token being considered by the parser.
      */
    var position: Int = 0
    /**
      * The parser avoids endless loops via a fuel abstraction.
      * When a look-up is made with [[nth]] one unit of fuel is lost.
      * If fuel reaches zero by this, that is a compiler error, since the parser is stuck.
      * Whenever progress is made with [[advance]] fuel is reset to its original amount.
      */
    var fuel: Int = 256
    /**
      * The Parsing events emitted during parsing.
      * Note that this is a flat collection that later gets turned into a [[SyntaxTree.Tree]] by [[buildTree()]].
      */
    var events: ArrayBuffer[Event] = ArrayBuffer.empty
    /**
      * Errors reside both within the produced `Tree` but are also kept here.
      * This is done to avoid crawling the tree for errors.
      * Note that there is data-duplication, but not in the happy case.
      * An alternative could be to collect errors as part of [[buildTree]] and return them in a list there.
      */
    val errors: ArrayBuffer[CompilationMessage] = ArrayBuffer.empty
    /*
     * This is necessary to display source code in error messages.
     * But since it comes from parboiled we probably want to get rid of it some day.
     */
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  private sealed trait Mark

  /**
    * Marks point to positions in a list of tokens where an Open or Close event resides.
    * This is useful because it lets the parser open a group, without knowing exactly what [[TreeKind]] the group should have.
    * For instance we need to look past doc-comments, annotations and modifiers to see what kind of declaration we a dealing with (def, enum, trait...).
    * The convention used throughout is to open a group with kind [[TreeKind.ErrorTree]] and then later close it with the correct kind.
    * This happens via the [[open]] and [[close]] functions.
    *
    * Conversely we sometimes need to open a group before the currently open one.
    * The best example is binary expressions. In '1 + 2' we first see '1' and open a group for a Literal.
    * Then we see '+' which means we want to open a Expr.Binary group that also includes the '1'.
    * This is done with [[openBefore]] which takes a [[Mark.Closed]] and inserts an Open event before it.
    * Whenever possible grammar rules return a [[Mark.Closed]] so another rule may wrap it.
    */
  private object Mark {
    case class Opened(index: Int) extends Mark

    case class Closed(index: Int) extends Mark
  }

  def run(tokens: Map[Ast.Source, Array[Token]], oldRoot: SyntaxTree.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[SyntaxTree.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.success(SyntaxTree.empty)
    }

    flix.phase("Parser2") {
      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(tokens, oldRoot.units)

      // Parse each stale source in parallel and join them into a WeededAst.Root
      val refreshed = ParOps.parMap(stale) {
        case (src, tokens) => mapN(parse(src, tokens))(trees => src -> trees)
      }

      // Join refreshed syntax trees with the already fresh ones.
      mapN(sequence(refreshed)) {
        refreshed => SyntaxTree.Root(refreshed.toMap ++ fresh)
      }
    }
  }

  private def parse(src: Ast.Source, tokens: Array[Token]): Validation[SyntaxTree.Tree, CompilationMessage] = {
    implicit val s: State = new State(tokens, src)
    // Call the top-most grammar rule to gather all events into state.
    root()
    // Build the syntax tree using events in state.
    val tree = buildTree()
    // Return with errors as soft failures to run subsequent phases for more validations.
    Validation.success(tree).withSoftFailures(s.errors)
  }

  private def buildTree()(implicit s: State): SyntaxTree.Tree = {
    val tokens = s.tokens.iterator.buffered
    // We are using lists as stacks here (Scala does have a 'Stack', but it is backed by an ArrayDeque).
    // Lists are fastest here though since they have constant time prepend (+:) and tail implementations.
    // We use prepend on Event.Open and stack.tail on Event.Close.
    var stack: List[SyntaxTree.Tree] = List.empty
    var locationStack: List[Token] = List.empty

    // Pop the last event, which must be a Close,
    // to ensure that the stack is not empty when handling event below.
    val lastEvent = s.events.last
    s.events = s.events.dropRight(1)
    assert(lastEvent match {
      case Event.Close => true
      case _ => false
    })

    // Make a synthetic token to begin with, to make the SourceLocations generated below be correct.
    var lastAdvance = Token(TokenKind.Eof, s.src.data, 0, 0, 0, 0, 0, 0)
    for (event <- s.events) {
      event match {
        case Event.Open(kind) =>
          locationStack = tokens.head +: locationStack
          stack = SyntaxTree.Tree(kind, Array.empty, SourceLocation.Unknown) +: stack

        case Event.Close =>
          val child = SyntaxTree.Child.TreeChild(stack.head)
          val openToken = locationStack.head
          stack.head.loc = if (stack.head.children.length == 0)
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
          locationStack = locationStack.tail
          stack = stack.tail
          stack.head.children = stack.head.children :+ child

        case Event.Advance =>
          val token = tokens.next()
          lastAdvance = token
          stack.head.children = stack.head.children :+ SyntaxTree.Child.TokenChild(token)
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
    * Get first non-comment previous position of the parser as a [[SourceLocation]].
    * TODO: It might make sense to seek the first non-comment position here.
    */
  private def previousSourceLocation()(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val token = s.tokens((s.position - 1).max(0))
    val beginLine = token.beginLine + 1
    val beginCol = (token.beginCol + 1).toShort
    val endLine = token.endLine + 1
    val endCol = (token.endCol + 1).toShort
    SourceLocation(s.parserInput, s.src, isReal = true, beginLine, beginCol, endLine, endCol)
  }

  /**
    * Get current position of the parser as a [[SourceLocation]]
    */
  private def currentSourceLocation()(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val token = s.tokens(s.position)
    val beginLine = token.beginLine + 1
    val beginCol = (token.beginCol + 1).toShort
    val endLine = token.endLine + 1
    val endCol = (token.endCol + 1).toShort
    SourceLocation(s.parserInput, s.src, isReal = true, beginLine, beginCol, endLine, endCol)
  }

  /**
    * Opens a group with kind [[TreeKind.ErrorTree]].
    * Each call to [[open]] must have a pairing call to [[close]]. This is asserted in [[buildTree]].
    * [[open]] consumes comments into the opened group.
    */
  private def open(consumeDocComments: Boolean = true)(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    val error = ParseError("Unclosed parser mark", SyntacticContext.Unknown, currentSourceLocation())
    s.events.append(Event.Open(TreeKind.ErrorTree(error)))
    // Consume any comments just before opening a new mark
    comments(canStartOnDoc = consumeDocComments)
    mark
  }

  /**
    * Closes a group and marks it with `kind`.
    * [[close]] consumes comments into the group before closing.
    */
  private def close(mark: Mark.Opened, kind: TreeKind)(implicit s: State): Mark.Closed = {
    s.events(mark.index) = Event.Open(kind)
    // Consume any comments just before closing a mark
    comments()
    s.events.append(Event.Close)
    Mark.Closed(mark.index)
  }

  /**
    * Opens a group before another one.
    * This is useful when we want to wrap one or more groups in a parent group.
    * For instance:
    * (Literal '1') '+' (Literal '2')
    * Into:
    * (Expr.Binary (Literal '1') '+' (Literal '2'))
    */
  private def openBefore(before: Mark.Closed)(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(before.index)
    val error = ParseError("Unclosed parser mark", SyntacticContext.Unknown, currentSourceLocation())
    s.events.insert(before.index, Event.Open(TreeKind.ErrorTree(error)))
    mark
  }

  /**
    * Advances the parser one token. [[advance]] refuels the parser.
    */
  private def advance()(implicit s: State): Unit = {
    if (eof()) {
      return
    }
    s.fuel = 256
    s.events.append(Event.Advance)
    s.position += 1
  }

  private def closeWithError(mark: Mark.Opened, error: CompilationMessage)(implicit s: State): Mark.Closed = {
    nth(0) match {
      // Avoid double reporting lexer errors.
      case TokenKind.Err(_) =>
      case _ => s.errors.append(error)
    }
    close(mark, TreeKind.ErrorTree(error))
  }

  /**
    * Wrap the next token in an error.
    */
  private def advanceWithError(error: CompilationMessage, mark: Option[Mark.Opened] = None)(implicit s: State): Mark.Closed = {
    val m = mark.getOrElse(open())
    nth(0) match {
      // Avoid double reporting lexer errors.
      case TokenKind.Err(_) =>
      case _ => s.errors.append(error)
    }
    advance()
    close(m, TreeKind.ErrorTree(error))
  }

  /**
    * Check if the parser is at the end-of-file.
    */
  private def eof()(implicit s: State): Boolean = {
    s.position == s.tokens.length - 1
  }

  /**
    * Look-ahead `lookahead` tokens.
    * Consumes one fuel and throws [[InternalCompilerException]] if the parser is out of fuel.
    * Does not consume fuel if parser has hit end-of-file.
    * This lets the parser return what ever errors were produced from a deep call-stack without running out of fuel.
    */
  private def nth(lookahead: Int)(implicit s: State): TokenKind = {
    if (s.fuel == 0) {
      throw InternalCompilerException(s"[${currentSourceLocation()}] Parser is stuck", currentSourceLocation())
    }

    if (s.position + lookahead >= s.tokens.length - 1) {
      TokenKind.Eof
    } else {
      s.fuel -= 1
      s.tokens(s.position + lookahead).kind
    }
  }

  /**
    * Checks if the parser is at a token of a specific `kind`.
    */
  private def at(kind: TokenKind)(implicit s: State): Boolean = {
    nth(0) == kind
  }

  /**
    * Checks if the parser is at a token of kind in `kinds`.
    * TODO: We can get a little performance from using BitSets here.
    * It complicates the implementation though, since each TokenKind must then have an Int representation.
    */
  private def atAny(kinds: Set[TokenKind])(implicit s: State): Boolean = {
    kinds.contains(nth(0))
  }

  /**
    * Checks if the parser is at a token of a specific `kind` and advances past it if it is.
    */
  private def eat(kind: TokenKind)(implicit s: State): Boolean = {
    if (at(kind)) {
      advance()
      true
    } else {
      false
    }
  }

  /**
    * Checks if the parser is at a token of kind in `kinds` and advances past it if it is.
    */
  private def eatAny(kinds: Set[TokenKind])(implicit s: State): Boolean = {
    if (atAny(kinds)) {
      advance()
      true
    } else {
      false
    }
  }

  /**
    * Advance past current token if it is of kind `kind`. Otherwise wrap it in an error.
    * Something to note about [[expect]] is that it does __not__ consume any comments.
    * That means that consecutive calls to expect work in an atomic manner.
    * expect(TokenKind.KeywordIf)
    * expect(TokenKind.ParenL)
    * Means "if (" with no comment in-between "if" and "(".
    */
  private def expect(kind: TokenKind)(implicit s: State): Unit = {
    if (eat(kind)) {
      return
    }

    val mark = open()
    val error = nth(0) match {
      case TokenKind.CommentLine => ParseError(s"Invalid comment", SyntacticContext.Unknown, previousSourceLocation())
      case TokenKind.CommentBlock => ParseError(s"Invalid comment", SyntacticContext.Unknown, previousSourceLocation())
      case TokenKind.CommentDoc => ParseError(s"Doc-comments can only decorate declarations.", SyntacticContext.Unknown, previousSourceLocation())
      case at => ParseError(s"Expected ${kind.display} before ${at.display}", SyntacticContext.Unknown, previousSourceLocation())
    }
    closeWithError(mark, error)
  }

  /**
    * Joins items nicely with comma separation ending with an "or".
    * For instance prettyJoin(List("def", "enum", "trait")) gives "def, enum or trait".
    */
  private def prettyJoin[T](items: Seq[T]): String = items match {
    case i1 :: i2 :: Nil => s"$i1 or $i2"
    case i1 :: Nil => s"$i1"
    case i :: tail => s"$i, ${prettyJoin(tail)}"
  }

  /**
    * Advance past current token if it is of kind in `kinds`. Otherwise wrap it in an error.
    */
  private def expectAny(kinds: Set[TokenKind])(implicit s: State): Unit = {
    if (eatAny(kinds)) {
      return
    }
    val mark = open()
    val error = nth(0) match {
      case TokenKind.CommentLine => ParseError(s"Invalid comment", SyntacticContext.Unknown, previousSourceLocation())
      case TokenKind.CommentBlock => ParseError(s"Invalid comment", SyntacticContext.Unknown, previousSourceLocation())
      case TokenKind.CommentDoc => ParseError(s"Doc-comments can only decorate declarations.", SyntacticContext.Unknown, previousSourceLocation())
      case at =>
        val kindsDisplayed = prettyJoin(kinds.toList.map(k => s"${k.display}"))
        ParseError(s"Expected $kindsDisplayed before ${at.display}", SyntacticContext.Unknown, previousSourceLocation())
    }
    closeWithError(mark, error)
  }

  /**
    * Checks if a token of kind `needle` can be found before any token of a kind in `before`.
    * This is useful for detecting which grammar rule to use, when language constructs share a prefix.
    * For instance, when sitting on a '{' it's not clear if a block or a record is to come.
    * But if we can find a '|' before either '{' or '}' we know it is a record.
    */
  private def findBefore(needle: TokenKind, before: Array[TokenKind])(implicit s: State): Boolean = {
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

  /**
    * A helper function for parsing a number of items surrounded by delimiters and separated by some token.
    * Examples of language features that use [[zeroOrMore]]:
    * Tuples "(1, 2, 3)".
    * Records "{ -y, +z = 3 | r }" <- Note the '| r' part. That can be handled by `optionallyWith`
    * ParFieldFragments "par (x <- e1; y <- e2; z <- e3) yield ...".
    * and many many more...
    *
    * @param displayName       The name of an item to be used in an error message. ie. "Expected <$displayName> before xyz".
    * @param getItem           Function for parsing a single item.
    * @param checkForItem      Function used to check if the next token indicates an item. getItem is only called when this returns true.
    * @param recoverOn         Function for deciding if an unexpected token is in the recover set of the current context. If it is then parsing of items stops.
    * @param separator         Token that separates each item.
    * @param optionalSeparator If the separator is optional, a missing separator does not produce an error.
    * @param delimiterL        The left delimiter.
    * @param delimiterR        The right delimiter.
    * @param optionallyWith    Used to parse a single rule before delimiterR but after all items if a specific token is found.
    *                          For instance in a record operation "{ +x, -y, z = 3 | w }" we can parse the "| w" part with
    *                          optionallyWith = Some((TokenKind.Bar, () => expression()))
    * @return The number of items successfully parsed.
    */
  private def zeroOrMore(
                          displayName: String,
                          getItem: () => Mark.Closed,
                          checkForItem: () => Boolean,
                          recoverOn: () => Boolean,
                          separator: TokenKind = TokenKind.Comma,
                          delimiterL: TokenKind = TokenKind.ParenL,
                          delimiterR: TokenKind = TokenKind.ParenR,
                          optionalSeparator: Boolean = false,
                          optionallyWith: Option[(TokenKind, () => Unit)] = None,
                        )(implicit s: State): Int = {
    def atEnd(): Boolean = at(delimiterR) || optionallyWith.exists { case (indicator, _) => at(indicator) }

    if (!at(delimiterL)) {
      return 0
    }
    expect(delimiterL)
    var continue = true
    var numItems = 0
    while (continue && !atEnd() && !eof()) {
      comments()
      if (checkForItem()) {
        getItem()
        numItems += 1
        if (!atEnd()) {
          if (optionalSeparator) eat(separator) else expect(separator)
          if (atEnd()) {
            closeWithError(open(), ParseError(s"Trailing ${separator.display}", SyntacticContext.Unknown, previousSourceLocation()))
          }
        }
      } else {
        // We are not at an item (checkForItem returned false).
        // Break out of the loop if we hit one of the tokens in the recover set.
        if (recoverOn()) {
          continue = false
        } else {
          // Otherwise eat one token and continue parsing next item.
          val error = ParseError(s"Expected <$displayName>", SyntacticContext.Unknown, currentSourceLocation())
          advanceWithError(error)
        }
      }
    }
    optionallyWith match {
      case Some((indicator, rule)) => if (eat(indicator)) {
        rule()
      }
      case None =>
    }
    expect(delimiterR)
    numItems
  }

  /**
    * A helper function for parsing one ore more items surrounded by delimiters and separated by some token.
    * Works by forwarding parameters to [[zeroOrMore]] and then checking that there was at least one item parsed.
    * Otherwise an error is produced.
    * See [[zeroOrMore]] for documentation of parameters.
    *
    * @return An optional parse error. If one or more item is found, None is returned.
    */
  def oneOrMore(
                 displayName: String,
                 getItem: () => Mark.Closed,
                 checkForItem: () => Boolean,
                 recoverOn: () => Boolean,
                 separator: TokenKind = TokenKind.Comma,
                 delimiterL: TokenKind = TokenKind.ParenL,
                 delimiterR: TokenKind = TokenKind.ParenR,
                 optionalSeparator: Boolean = false,
                 optionallyWith: Option[(TokenKind, () => Unit)] = None,
               )(implicit s: State): Option[ParseError] = {
    val locBefore = previousSourceLocation()
    val itemCount = zeroOrMore(displayName, getItem, checkForItem, recoverOn, separator, delimiterL, delimiterR, optionalSeparator, optionallyWith)
    val locAfter = currentSourceLocation()
    if (itemCount < 1) {
      val loc = SourceLocation.mk(locBefore.sp1, locAfter.sp1)
      val error = ParseError(s"Expected one or more <$displayName>", SyntacticContext.Unknown, loc)
      Some(error)
    } else {
      None
    }
  }

  /**
    * Groups of [[TokenKind]]s that make of the different kinds of names in Flix.
    * So for instance NAME_PARAMETER is all the kinds of tokens that may occur as a parameter identifier.
    * Use these together with the [[name]] helper function.
    */
  private val NAME_DEFINITION: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.UserDefinedOperator)
  private val NAME_PARAMETER: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_VARIABLE: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_JAVA: Set[TokenKind] = Set(TokenKind.NameJava, TokenKind.NameLowerCase, TokenKind.NameUpperCase)
  private val NAME_QNAME: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameUpperCase)
  private val NAME_USE: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.UserDefinedOperator)
  private val NAME_FIELD: Set[TokenKind] = Set(TokenKind.NameLowerCase)
  // TODO: Static is used as a type in Prelude.flix. Should we allow this?
  private val NAME_TYPE: Set[TokenKind] = Set(TokenKind.NameUpperCase, TokenKind.KeywordStaticUppercase)
  private val NAME_KIND: Set[TokenKind] = Set(TokenKind.NameUpperCase)
  private val NAME_EFFECT: Set[TokenKind] = Set(TokenKind.NameUpperCase)
  private val NAME_MODULE: Set[TokenKind] = Set(TokenKind.NameUpperCase)
  // TODO: Pure is used in enums as tags in Prelude.flix. Should we allow this?
  private val NAME_TAG: Set[TokenKind] = Set(TokenKind.NameUpperCase, TokenKind.KeywordPure)
  private val NAME_PREDICATE: Set[TokenKind] = Set(TokenKind.NameUpperCase)

  private val MODIFIERS: Set[TokenKind] = Set(TokenKind.KeywordSealed, TokenKind.KeywordLawful, TokenKind.KeywordPub, TokenKind.KeywordInline, TokenKind.KeywordOverride)

  /**
    * Set of the [[TokenKind]]s that can appear as the first token of declarations.
    * Note that a CommentDoc, a Modifier and/or an annotation can appear before the token indicating what declaration we are dealing with.
    */
  private val FIRST_DECL: Set[TokenKind] = MODIFIERS ++ Set(
    TokenKind.CommentDoc,
    TokenKind.Annotation,
    TokenKind.KeywordMod,
    TokenKind.KeywordDef,
    TokenKind.KeywordEnum,
    TokenKind.KeywordTrait,
    TokenKind.KeywordInstance,
    TokenKind.KeywordType,
    TokenKind.KeywordEff,
    TokenKind.KeywordRestrictable
  )

  /**
    * Set of the [[TokenKind]]s that can appear as the first token of expressions.
    * This is used for error recovery, specifically to recover out of expression
    */
  private val FIRST_EXPR: Set[TokenKind] = Set(TokenKind.KeywordOpenVariant,
    TokenKind.KeywordOpenVariantAs,
    TokenKind.HoleNamed,
    TokenKind.HoleAnonymous,
    TokenKind.HoleVariable,
    TokenKind.KeywordUse,
    TokenKind.LiteralString,
    TokenKind.LiteralChar,
    TokenKind.LiteralFloat32,
    TokenKind.LiteralFloat64,
    TokenKind.LiteralBigDecimal,
    TokenKind.LiteralInt8,
    TokenKind.LiteralInt16,
    TokenKind.LiteralInt32,
    TokenKind.LiteralInt64,
    TokenKind.LiteralBigInt,
    TokenKind.KeywordTrue,
    TokenKind.KeywordFalse,
    TokenKind.KeywordNull,
    TokenKind.LiteralRegex,
    TokenKind.ParenL,
    TokenKind.Underscore,
    TokenKind.NameLowerCase,
    TokenKind.NameUpperCase,
    TokenKind.NameMath,
    TokenKind.NameGreek,
    TokenKind.Minus,
    TokenKind.KeywordNot,
    TokenKind.Plus,
    TokenKind.TripleTilde,
    TokenKind.KeywordLazy,
    TokenKind.KeywordForce,
    TokenKind.KeywordDiscard,
    TokenKind.KeywordDeref,
    TokenKind.KeywordIf,
    TokenKind.KeywordLet,
    TokenKind.Annotation,
    TokenKind.KeywordDef,
    TokenKind.KeywordImport,
    TokenKind.KeywordRegion,
    TokenKind.KeywordMatch,
    TokenKind.KeywordTypeMatch,
    TokenKind.KeywordChoose,
    TokenKind.KeywordChooseStar,
    TokenKind.KeywordForA,
    TokenKind.KeywordForeach,
    TokenKind.KeywordForM,
    TokenKind.CurlyL,
    TokenKind.ArrayHash,
    TokenKind.VectorHash,
    TokenKind.ListHash,
    TokenKind.SetHash,
    TokenKind.MapHash,
    TokenKind.KeywordRef,
    TokenKind.KeywordCheckedCast,
    TokenKind.KeywordCheckedECast,
    TokenKind.KeywordUncheckedCast,
    TokenKind.KeywordMaskedCast,
    TokenKind.KeywordTry,
    TokenKind.KeywordDo,
    TokenKind.KeywordNew,
    TokenKind.KeywordStaticUppercase,
    TokenKind.KeywordSelect,
    TokenKind.KeywordSpawn,
    TokenKind.KeywordPar,
    TokenKind.HashCurlyL,
    TokenKind.HashParenL,
    TokenKind.KeywordSolve,
    TokenKind.KeywordInject,
    TokenKind.KeywordQuery,
    TokenKind.BuiltIn,
    TokenKind.LiteralStringInterpolationL,
    TokenKind.LiteralDebugStringL,
    TokenKind.KeywordDebug,
    TokenKind.KeywordDebugBang,
    TokenKind.KeywordDebugBangBang,
    TokenKind.NameJava)

  /**
    * Set of the [[TokenKind]]s that can appear as the first token of types.
    */
  private val FIRST_TYPE: Set[TokenKind] = Set(TokenKind.NameUpperCase,
    TokenKind.NameMath,
    TokenKind.NameGreek,
    TokenKind.Underscore,
    TokenKind.NameLowerCase,
    TokenKind.KeywordUniv,
    TokenKind.KeywordPure,
    TokenKind.KeywordFalse,
    TokenKind.KeywordTrue,
    TokenKind.ParenL,
    TokenKind.CurlyL,
    TokenKind.HashCurlyL,
    TokenKind.HashParenL,
    TokenKind.NameJava,
    TokenKind.AngleL,
    TokenKind.KeywordNot,
    TokenKind.Tilde,
    TokenKind.KeywordRvnot,
    TokenKind.KeywordStaticUppercase)

  /**
    * Set of the [[TokenKind]]s that can appear as the first token of patterns.
    */
  private val FIRST_PATTERN: Set[TokenKind] = Set(TokenKind.NameLowerCase,
    TokenKind.NameGreek,
    TokenKind.NameMath,
    TokenKind.Underscore,
    TokenKind.KeywordQuery,
    TokenKind.LiteralString,
    TokenKind.LiteralChar,
    TokenKind.LiteralFloat32,
    TokenKind.LiteralFloat64,
    TokenKind.LiteralBigDecimal,
    TokenKind.LiteralInt8,
    TokenKind.LiteralInt16,
    TokenKind.LiteralInt32,
    TokenKind.LiteralInt64,
    TokenKind.LiteralBigInt,
    TokenKind.KeywordTrue,
    TokenKind.KeywordFalse,
    TokenKind.LiteralRegex,
    TokenKind.KeywordNull,
    TokenKind.NameUpperCase,
    TokenKind.ParenL,
    TokenKind.CurlyL,
    TokenKind.Minus)


  /**
    * Sets of [[TokenKind]]s used for recovery.
    * For instance, when parsing sequences of expressions in a loop and an unexpected token is found, a recovery means breaking the loop.
    * An example would be:
    * def foo(): Unit = bar(1,
    * enum Legumes { case Beans, Chickpeas }
    * Here we recover from parsing argument expressions of the unfinished call to `bar` and still discover `Legumes`, because [[TokenKind.KeywordEnum]] is part of [[RECOVER_EXPR]].
    */
  private val RECOVER_DECL: Set[TokenKind] = FIRST_DECL
  private val RECOVER_EXPR: Set[TokenKind] = FIRST_DECL + TokenKind.Semi
  private val RECOVER_TYPE: Set[TokenKind] = FIRST_DECL ++ Set(TokenKind.Equal, TokenKind.Semi)
  private val RECOVER_PATTERN: Set[TokenKind] = FIRST_DECL ++ Set(TokenKind.ArrowThickR, TokenKind.KeywordCase)
  private val RECOVER_TOP_LEVEL_USE: Set[TokenKind] = FIRST_DECL ++ Set(TokenKind.Semi, TokenKind.KeywordUse, TokenKind.KeywordImport)
  private val RECOVER_PARAMETERS: Set[TokenKind] = FIRST_DECL ++ Set(TokenKind.Colon, TokenKind.Equal)

  /**
    * Consumes a token if kind is in `kinds`. If `allowQualified` is passed also consume subsequent dot-separated tokens with kind in `kinds`.
    */
  private def name(kinds: Set[TokenKind], allowQualified: Boolean = false)(implicit s: State): Mark.Closed = {
    val mark = open(consumeDocComments = false)
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

  private val NON_DOC_COMMENTS: Set[TokenKind] = Set(TokenKind.CommentLine, TokenKind.CommentBlock)
  private val COMMENTS: Set[TokenKind] = Set(TokenKind.CommentDoc, TokenKind.CommentLine, TokenKind.CommentBlock)

  /**
    * Consumes subsequent comments.
    * In cases where doc-comments cannot occur (above expressions for instance), we would like to treat them as regular comments.
    * This is achieved by passing `canStartOnDoc = true`.
    */
  private def comments(canStartOnDoc: Boolean = false)(implicit s: State): Unit = {
    // Note: In case of a misplaced CommentDoc, we would just like to consume it into the comment list.
    // This is forgiving in the common case of accidentally inserting an extra '/'.
    val starters: Set[TokenKind] = if (canStartOnDoc) COMMENTS else NON_DOC_COMMENTS

    if (atAny(starters)) {
      val mark = Mark.Opened(s.events.length)
      val error = ParseError("Unclosed parser mark.", SyntacticContext.Unknown, currentSourceLocation())
      s.events.append(Event.Open(TreeKind.ErrorTree(error)))
      // Note: This loop will also consume doc-comments that are preceded or surrounded by either line or block comments.
      while (atAny(COMMENTS) && !eof()) {
        advance()
      }
      close(mark, TreeKind.CommentList)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////
  /// GRAMMAR                                                                             //
  //////////////////////////////////////////////////////////////////////////////////////////
  private def root()(implicit s: State): Unit = {
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
      zeroOrMore(
        displayName = "name",
        getItem = () => aliasedName(NAME_USE),
        checkForItem = () => atAny(NAME_USE),
        recoverOn = () => atAny(RECOVER_TOP_LEVEL_USE),
        delimiterL = TokenKind.DotCurlyL,
        delimiterR = TokenKind.CurlyR
      )
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
      zeroOrMore(
        displayName = "name",
        getItem = () => aliasedName(NAME_JAVA),
        checkForItem = () => atAny(NAME_JAVA),
        recoverOn = () => atAny(RECOVER_TOP_LEVEL_USE),
        delimiterL = TokenKind.DotCurlyL,
        delimiterR = TokenKind.CurlyR
      )
      close(mark, TreeKind.UsesOrImports.ImportMany)
    }
    close(mark, TreeKind.UsesOrImports.Import)
  }

  private def aliasedName(names: Set[TokenKind])(implicit s: State): Mark.Closed = {
    var lhs = name(names)
    if (eat(TokenKind.ArrowThickR)) {
      name(names)
      lhs = close(openBefore(lhs), TreeKind.UsesOrImports.Alias)
    }
    lhs
  }

  private object Decl {
    def declaration()(implicit s: State): Mark.Closed = {
      val mark = open(consumeDocComments = false)
      docComment()
      // Handle case where the last thing in a file or module is a doc-comment
      if (eof() || at(TokenKind.CurlyR)) {
        println(currentSourceLocation(), nth(0))
        return close(mark, TreeKind.CommentList)
      }
      // Handle modules
      if (at(TokenKind.KeywordMod)) {
        return moduleDecl(mark)
      }
      // Handle declarations
      annotations()
      modifiers()
      // If a new declaration is added to this, make sure to add it to FIRST_DECL too.
      nth(0) match {
        case TokenKind.KeywordTrait => traitDecl(mark)
        case TokenKind.KeywordInstance => instanceDecl(mark)
        case TokenKind.KeywordDef => definitionDecl(mark)
        case TokenKind.KeywordEnum | TokenKind.KeywordRestrictable => enumerationDecl(mark)
        case TokenKind.KeywordType => typeAliasDecl(mark)
        case TokenKind.KeywordEff => effectDecl(mark)
        case at =>
          val loc = currentSourceLocation()
          // Skip ahead until we hit another declaration.
          while (!atAny(RECOVER_DECL) && !eof()) {
            advance()
          }
          val error = ParseError(s"Expected <declaration> before ${at.display}", SyntacticContext.Decl.OtherDecl, loc)
          closeWithError(mark, error)
      }
    }

    private def moduleDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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

    private def traitDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordTrait)
      name(NAME_DEFINITION)
      Type.parameters()
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      if (at(TokenKind.CurlyL)) {
        expect(TokenKind.CurlyL)
        while (!at(TokenKind.CurlyR) && !eof()) {
          val mark = open(consumeDocComments = false)
          docComment()
          annotations()
          modifiers()
          nth(0) match {
            case TokenKind.KeywordLaw => lawDecl(mark)
            case TokenKind.KeywordDef => signatureDecl(mark)
            case TokenKind.KeywordType => associatedTypeSigDecl(mark)
            case at =>
              val error = ParseError(s"Expected ${TokenKind.KeywordType.display}, ${TokenKind.KeywordDef.display} or ${TokenKind.KeywordLaw.display} before ${at.display}", SyntacticContext.Decl.Trait, currentSourceLocation())
              advanceWithError(error, Some(mark))
          }
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Trait)
    }

    private def instanceDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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
          val mark = open(consumeDocComments = false)
          docComment()
          annotations()
          modifiers()
          nth(0) match {
            case TokenKind.KeywordDef => definitionDecl(mark)
            case TokenKind.KeywordType => associatedTypeDefDecl(mark)
            case at =>
              val error = ParseError(s"Expected ${TokenKind.KeywordType.display} or ${TokenKind.KeywordDef.display}, found ${at.display}", SyntacticContext.Decl.Instance, currentSourceLocation())
              advanceWithError(error, Some(mark))
          }
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Instance)
    }

    private def signatureDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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
      if (at(TokenKind.KeywordWhere)) {
        equalityConstraints(TokenKind.Equal)
      }
      if (eat(TokenKind.Equal)) {
        Expr.statement()
      }
      close(mark, TreeKind.Decl.Signature)
    }

    private def definitionDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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
      if (at(TokenKind.KeywordWhere)) {
        equalityConstraints(TokenKind.Equal)
      }

      // We want to only parse an expression if we see an equal sign to avoid consuming following definitions as LetRecDefs.
      // Here is an example. We want to avoid consuming 'main' as a nested function, even though 'def' signifies an Expr.LetRecDef:
      // def f(): Unit // <- no equal sign
      // def main(): Unit = ()
      if (eat(TokenKind.Equal)) {
        Expr.statement()
      } else {
        expect(TokenKind.Equal) // Produce an error for missing '='
      }

      close(mark, TreeKind.Decl.Def)
    }

    private def lawDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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

    private def enumerationDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(atAny(Set(TokenKind.KeywordRestrictable, TokenKind.KeywordEnum)))
      val isRestrictable = eat(TokenKind.KeywordRestrictable)
      expect(TokenKind.KeywordEnum)
      val nameLoc = currentSourceLocation()
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
      val isShorthand = at(TokenKind.ParenL)
      if (isShorthand) {
        val markType = open()
        Type.tuple()
        close(markType, TreeKind.Type.Type)
      }
      // derivations
      if (at(TokenKind.KeywordWith)) {
        Type.derivations()
      }

      // Check for illegal enum using both shorthand and body
      if (isShorthand && eat(TokenKind.CurlyL)) {
        val mark = open()
        enumCases()
        expect(TokenKind.CurlyR)
        closeWithError(mark, WeederError.IllegalEnum(nameLoc))
      }

      // enum body
      if (eat(TokenKind.CurlyL)) {
        enumCases()
        expect(TokenKind.CurlyR)
      }
      close(mark, if (isRestrictable) TreeKind.Decl.RestrictableEnum else TreeKind.Decl.Enum)
    }

    private def FIRST_ENUM_CASE: Set[TokenKind] = Set(TokenKind.CommentDoc, TokenKind.KeywordCase, TokenKind.Comma)

    private def enumCases()(implicit s: State): Unit = {
      // Clear non-doc comments that appear before any cases.
      comments()
      while (!eof() && atAny(FIRST_ENUM_CASE)) {
        val mark = open(consumeDocComments = false)
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
          val markTuple = open()
          oneOrMore(
            displayName = "type",
            getItem = () => Type.ttype(),
            checkForItem = () => atAny(FIRST_TYPE),
            recoverOn = () => atAny(RECOVER_DECL)
          ) match {
            case Some(error) =>
              close(markTuple, TreeKind.Type.Tuple)
              closeWithError(mark, error)
            case None =>
              close(markTuple, TreeKind.Type.Tuple)
              close(mark, TreeKind.Type.Type)
          }
        }
        close(mark, TreeKind.Case)
      }
    }

    private def typeAliasDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
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

    private def associatedTypeSigDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordType))
      expect(TokenKind.KeywordType)
      name(NAME_TYPE)
      if (at(TokenKind.BracketL)) {
        Type.parameters()
      }
      if (at(TokenKind.Colon)) {
        expect(TokenKind.Colon)
        Type.kind()
      }
      if (at(TokenKind.Equal)) {
        expect(TokenKind.Equal)
        Type.ttype()
      }
      close(mark, TreeKind.Decl.AssociatedTypeSig)
    }

    private def associatedTypeDefDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      expect(TokenKind.KeywordType)
      name(NAME_TYPE)
      if (at(TokenKind.BracketL)) {
        Type.arguments()
      }
      if (eat(TokenKind.Equal)) {
        Type.ttype()
      }
      close(mark, TreeKind.Decl.AssociatedTypeDef)
    }

    private def effectDecl(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordEff))
      expect(TokenKind.KeywordEff)
      name(NAME_EFFECT)

      // Check for illegal type parameters.
      if (at(TokenKind.BracketL)) {
        val mark = open()
        val loc = currentSourceLocation()
        Type.parameters()
        closeWithError(mark, WeederError.IllegalEffectTypeParams(loc))
      }

      if (eat(TokenKind.CurlyL)) {
        while (!at(TokenKind.CurlyR) && !eof()) {
          operationDecl()
        }
        expect(TokenKind.CurlyR)
      }
      close(mark, TreeKind.Decl.Effect)
    }

    private def operationDecl()(implicit s: State): Mark.Closed = {
      val mark = open(consumeDocComments = false)
      docComment()
      annotations()
      modifiers()
      expect(TokenKind.KeywordDef)
      name(NAME_DEFINITION)

      // Check for illegal type parameters.
      if (at(TokenKind.BracketL)) {
        val mark = open()
        val loc = currentSourceLocation()
        Type.parameters()
        closeWithError(mark, WeederError.IllegalEffectTypeParams(loc))
      }

      if (at(TokenKind.ParenL)) {
        parameters()
      }
      if (eat(TokenKind.Colon)) {
        val typeLoc = currentSourceLocation()
        Type.ttype()
        // Check for illegal effect
        if (at(TokenKind.Backslash)) {
          val mark = open()
          eat(TokenKind.Backslash)
          Type.effectSet()
          closeWithError(mark, WeederError.IllegalEffectfulOperation(typeLoc))
        }
      }
      if (at(TokenKind.KeywordWith)) {
        Type.constraints()
      }
      close(mark, TreeKind.Decl.Op)
    }

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

    def parameters()(implicit s: State): Mark.Closed = {
      val mark = open()
      zeroOrMore(
        displayName = "parameter",
        getItem = parameter,
        checkForItem = () => atAny(NAME_PARAMETER),
        recoverOn = () => atAny(RECOVER_PARAMETERS)
      )
      close(mark, TreeKind.ParameterList)
    }

    private def parameter()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PARAMETER)
      if (eat(TokenKind.Colon)) {
        Type.ttype()
      }
      close(mark, TreeKind.Parameter)
    }

    private def equalityConstraints(terminator: TokenKind)(implicit s: State): Mark.Closed = {
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

    def expression(left: TokenKind = TokenKind.Eof, leftIsUnary: Boolean = false)(implicit s: State): Mark.Closed = {
      var lhs = exprDelimited()
      // Handle calls
      while (at(TokenKind.ParenL)) {
        val mark = openBefore(lhs)
        arguments()
        lhs = close(mark, TreeKind.Expr.Apply)
        lhs = close(openBefore(lhs), TreeKind.Expr.Expr)
      }
      // Handle record select after function call. Example: funcReturningRecord().field
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
      // Handle binary operators
      var continue = true
      while (continue) {
        val right = nth(0)
        if (rightBindsTighter(left, right, leftIsUnary)) {
          val mark = openBefore(lhs)
          val markOp = open()
          advance()
          close(markOp, TreeKind.Operator)
          expression(right)
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
          oneOrMore(
            displayName = "effect",
            getItem = () => name(NAME_EFFECT, allowQualified = true),
            checkForItem = () => atAny(NAME_EFFECT),
            recoverOn = () => atAny(RECOVER_EXPR),
            delimiterL = TokenKind.CurlyL,
            delimiterR = TokenKind.CurlyR
          ) match {
            case Some(error) => closeWithError(mark, error)
            case None => close(mark, TreeKind.Type.EffectSet)
          }
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

    /**
      * A precedence table for operators, lower is higher precedence.
      * Note that [[OpKind]] is necessary for the cases where the same token kind can be both unary and binary. IE. Plus or Minus.
      */
    private def PRECEDENCE: List[(OpKind, Array[TokenKind])] = List(
      (OpKind.Binary, Array(TokenKind.ColonEqual, TokenKind.KeywordInstanceOf)), // :=, instanceof
      (OpKind.Binary, Array(TokenKind.KeywordOr)),
      (OpKind.Binary, Array(TokenKind.KeywordAnd)),
      (OpKind.Binary, Array(TokenKind.TripleBar)), // |||
      (OpKind.Binary, Array(TokenKind.TripleCaret)), // ^^^
      (OpKind.Binary, Array(TokenKind.TripleAmpersand)), // &&&
      (OpKind.Binary, Array(TokenKind.EqualEqual, TokenKind.AngledEqual, TokenKind.BangEqual)), // ==, <=>, !=
      (OpKind.Binary, Array(TokenKind.AngleL, TokenKind.AngleR, TokenKind.AngleLEqual, TokenKind.AngleREqual)), // <, >, <=, >=
      (OpKind.Binary, Array(TokenKind.ColonColon, TokenKind.TripleColon)), // ::
      (OpKind.Binary, Array(TokenKind.TripleAngleL, TokenKind.TripleAngleR)), // <<<, >>>
      (OpKind.Binary, Array(TokenKind.Plus, TokenKind.Minus)), // +, -
      (OpKind.Binary, Array(TokenKind.Star, TokenKind.StarStar, TokenKind.Slash)), // *, **, /
      (OpKind.Binary, Array(TokenKind.AngledPlus)), // <+>
      (OpKind.Unary, Array(TokenKind.KeywordDiscard)), // discard
      (OpKind.Binary, Array(TokenKind.InfixFunction)), // `my_function`
      (OpKind.Binary, Array(TokenKind.UserDefinedOperator, TokenKind.NameMath)), // +=+ user defined op like '+=+' or '++'
      (OpKind.Unary, Array(TokenKind.KeywordLazy, TokenKind.KeywordForce, TokenKind.KeywordDeref)), // lazy, force, deref
      (OpKind.Unary, Array(TokenKind.Plus, TokenKind.Minus, TokenKind.TripleTilde)), // +, -, ~~~
      (OpKind.Unary, Array(TokenKind.KeywordNot))
    )

    // These operators are right associative, meaning for instance that "x :: y :: z" becomes "x :: (y :: z)" rather than "(x :: y) :: z"
    private val rightAssoc: Array[TokenKind] = Array(TokenKind.ColonColon, TokenKind.TripleColon) // FCons, FAppend

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

    // Remove KeywordDef from FIRST_EXPR for arguments only.
    // This handles the common case of incomplete arguments followed by another declaration gracefully.
    // For instance:
    // def foo(): Int32 = bar(
    // def main(): Unit = ()
    // In this example, if we had KeywordDef, main would be read as a LetRecDef expression!
    private val FIRST_EXPR_NO_KEYWORD_DEF = FIRST_EXPR.filter(t => t != TokenKind.KeywordDef)

    private def arguments()(implicit s: State): Mark.Closed = {
      val mark = open()
      zeroOrMore(
        displayName = "argument",
        getItem = argument,
        checkForItem = () => atAny(FIRST_EXPR_NO_KEYWORD_DEF),
        recoverOn = () => atAny(RECOVER_EXPR)
      )
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

    private def exprDelimited()(implicit s: State): Mark.Closed = {
      // If a new expression is added here, remember to add it to FIRST_EXPR also.
      val mark = open()
      nth(0) match {
        case TokenKind.KeywordOpenVariant => openVariantExpr()
        case TokenKind.KeywordOpenVariantAs => openVariantAsExpr()
        case TokenKind.HoleNamed
             | TokenKind.HoleAnonymous => holeExpr()
        case TokenKind.HoleVariable => holeVariableExpr()
        case TokenKind.KeywordUse => useExpr()
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
             | TokenKind.LiteralRegex => literalExpr()
        case TokenKind.ParenL => parenOrTupleOrLambdaExpr()
        case TokenKind.Underscore => if (nth(1) == TokenKind.ArrowThinR) unaryLambdaExpr() else name(NAME_VARIABLE)
        case TokenKind.NameLowerCase => if (nth(1) == TokenKind.ArrowThinR) unaryLambdaExpr() else name(NAME_FIELD, allowQualified = true)
        case TokenKind.NameUpperCase
             | TokenKind.NameMath
             | TokenKind.NameGreek => if (nth(1) == TokenKind.ArrowThinR) unaryLambdaExpr() else name(NAME_DEFINITION, allowQualified = true)
        case TokenKind.Minus
             | TokenKind.KeywordNot
             | TokenKind.Plus
             | TokenKind.TripleTilde
             | TokenKind.KeywordLazy
             | TokenKind.KeywordForce
             | TokenKind.KeywordDiscard
             | TokenKind.KeywordDeref => unaryExpr()
        case TokenKind.KeywordIf => ifThenElseExpr()
        case TokenKind.KeywordLet => letMatchExpr()
        case TokenKind.Annotation | TokenKind.KeywordDef => letRecDefExpr()
        case TokenKind.KeywordImport => letImportExpr()
        case TokenKind.KeywordRegion => scopeExpr()
        case TokenKind.KeywordMatch => matchOrMatchLambdaExpr()
        case TokenKind.KeywordTypeMatch => typematchExpr()
        case TokenKind.KeywordChoose
             | TokenKind.KeywordChooseStar => restrictableChooseExpr()
        case TokenKind.KeywordForA => forApplicativeExpr()
        case TokenKind.KeywordForeach => foreachExpr()
        case TokenKind.KeywordForM => forMonadicExpr()
        case TokenKind.CurlyL => blockOrRecordExpr()
        case TokenKind.ArrayHash => arrayLiteralExpr()
        case TokenKind.VectorHash => vectorLiteralExpr()
        case TokenKind.ListHash => listLiteralExpr()
        case TokenKind.SetHash => setLiteralExpr()
        case TokenKind.MapHash => mapLiteralExpr()
        case TokenKind.KeywordRef => refExpr()
        case TokenKind.KeywordCheckedCast => checkedTypeCastExpr()
        case TokenKind.KeywordCheckedECast => checkedEffectCastExpr()
        case TokenKind.KeywordUncheckedCast => uncheckedCastExpr()
        case TokenKind.KeywordMaskedCast => uncheckedMaskingCastExpr()
        case TokenKind.KeywordTry => tryExpr()
        case TokenKind.KeywordDo => doExpr()
        case TokenKind.KeywordNew => newObjectExpr()
        case TokenKind.KeywordStaticUppercase => staticExpr()
        case TokenKind.KeywordSelect => selectExpr()
        case TokenKind.KeywordSpawn => spawnExpr()
        case TokenKind.KeywordPar => parYieldExpr()
        case TokenKind.HashCurlyL => fixpointConstraintSetExpr()
        case TokenKind.HashParenL => fixpointLambdaExpr()
        case TokenKind.KeywordSolve => fixpointSolveExpr()
        case TokenKind.KeywordInject => fixpointInjectExpr()
        case TokenKind.KeywordQuery => fixpointQueryExpr()
        case TokenKind.BuiltIn => intrinsicExpr()
        case TokenKind.LiteralStringInterpolationL
             | TokenKind.LiteralDebugStringL => interpolatedStringExpr()
        case TokenKind.KeywordDebug
             | TokenKind.KeywordDebugBang
             | TokenKind.KeywordDebugBangBang => debugExpr()
        case TokenKind.NameJava => name(NAME_JAVA, allowQualified = true)
        case t =>
          val mark = open()
          val error = ParseError(s"Expected <expression> before ${t.display}", SyntacticContext.Expr.OtherExpr, previousSourceLocation())
          closeWithError(mark, error)
      }
      close(mark, TreeKind.Expr.Expr)
    }

    private def openVariantExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordOpenVariant))
      val mark = open()
      expect(TokenKind.KeywordOpenVariant)
      name(NAME_QNAME, allowQualified = true)
      close(mark, TreeKind.Expr.OpenVariant)
    }

    private def openVariantAsExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordOpenVariantAs))
      val mark = open()
      expect(TokenKind.KeywordOpenVariantAs)
      name(NAME_QNAME, allowQualified = true)
      expression()
      close(mark, TreeKind.Expr.OpenVariantAs)
    }

    private def holeExpr()(implicit s: State): Mark.Closed = {
      assert(atAny(Set(TokenKind.HoleNamed, TokenKind.HoleAnonymous)))
      val mark = open()
      nth(0) match {
        case TokenKind.HoleAnonymous => advance(); close(mark, TreeKind.Expr.Hole)
        case TokenKind.HoleNamed => name(Set(TokenKind.HoleNamed)); close(mark, TreeKind.Expr.Hole)
        case _ => throw InternalCompilerException("Parser assert missed case", currentSourceLocation())
      }
    }

    private def holeVariableExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HoleVariable))
      val mark = open()
      name(Set(TokenKind.HoleVariable))
      close(mark, TreeKind.Expr.HoleVariable)
    }

    private def useExpr()(implicit s: State): Mark.Closed = {
      val mark = open()
      use()
      expect(TokenKind.Semi)
      statement()
      close(mark, TreeKind.Expr.Use)
    }

    private def literalExpr()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Expr.Literal)
    }

    private def parenOrTupleOrLambdaExpr()(implicit s: State): Mark.Closed = {
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
            var curlyLevel = 0
            var lookAhead = 0
            while (level > 0 && !eof()) {
              lookAhead += 1
              nth(lookAhead) match {
                case TokenKind.ParenL => level += 1
                case TokenKind.ParenR => level -= 1
                case TokenKind.CurlyL | TokenKind.HashCurlyL => curlyLevel += 1
                case TokenKind.CurlyR if level == 1 =>
                  if (curlyLevel == 0) {
                    // Hitting '}' on top-level is a clear indicator that something is wrong. Most likely the terminating ')' was forgotten.
                    return advanceWithError(ParseError("Malformed tuple.", SyntacticContext.Expr.OtherExpr, currentSourceLocation()))
                  } else {
                    curlyLevel -= 1
                  }
                case TokenKind.Eof => return advanceWithError(ParseError("Malformed tuple.", SyntacticContext.Expr.OtherExpr, currentSourceLocation()))
                case _ =>
              }
            }
            nth(lookAhead + 1) == TokenKind.ArrowThinR
          }

          if (isLambda) {
            lambda()
          } else {
            parenOrTupleOrAscribe()
          }

        case (t1, _) =>
          val error = ParseError(s"Expected ${TokenKind.ParenL.display} found ${t1.display}", SyntacticContext.Expr.OtherExpr, currentSourceLocation())
          advanceWithError(error)
      }
    }

    private def lambda()(implicit s: State): Mark.Closed = {
      val mark = open()
      Decl.parameters()
      expect(TokenKind.ArrowThinR)
      expression()
      close(mark, TreeKind.Expr.Lambda)
    }

    private def parenOrTupleOrAscribe()(implicit s: State): Mark.Closed = {
      val mark = open()
      expect(TokenKind.ParenL)
      val markExpr = expression()
      // Distinguish between expression in parenthesis, type ascriptions and tuples
      nth(0) match {
        // Type ascription
        case TokenKind.Colon =>
          expect(TokenKind.Colon)
          Type.typeAndEffect()
          expect(TokenKind.ParenR)
          close(mark, TreeKind.Expr.Ascribe)
        // Tuple
        case TokenKind.Equal | TokenKind.Comma =>
          if (eat(TokenKind.Equal)) {
            expression()
            close(openBefore(markExpr), TreeKind.ArgumentNamed)
          } else {
            close(openBefore(markExpr), TreeKind.Argument)
          }
          while (!at(TokenKind.ParenR) && !eof()) {
            eat(TokenKind.Comma)
            argument()
          }
          expect(TokenKind.ParenR)
          close(mark, TreeKind.Expr.Tuple)
        // Paren
        case _ =>
          expect(TokenKind.ParenR)
          close(mark, TreeKind.Expr.Paren)
      }
    }

    private def unaryLambdaExpr()(implicit s: State): Mark.Closed = {
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

    private val FIRST_EXPR_UNARY: Set[TokenKind] = Set(
      TokenKind.Minus,
      TokenKind.KeywordNot,
      TokenKind.Plus,
      TokenKind.TripleTilde,
      TokenKind.KeywordLazy,
      TokenKind.KeywordForce,
      TokenKind.KeywordDiscard,
      TokenKind.KeywordDeref)

    private def unaryExpr()(implicit s: State): Mark.Closed = {
      val mark = open()
      val op = nth(0)
      val markOp = open()
      expectAny(FIRST_EXPR_UNARY)
      close(markOp, TreeKind.Operator)
      expression(left = op, leftIsUnary = true)
      close(mark, TreeKind.Expr.Unary)
    }

    private def ifThenElseExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordIf))
      val mark = open()
      expect(TokenKind.KeywordIf)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      expression()
      if (eat(TokenKind.KeywordElse)) {
        expression()
      }
      close(mark, TreeKind.Expr.IfThenElse)
    }

    private def letMatchExpr()(implicit s: State): Mark.Closed = {
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

    private def letRecDefExpr()(implicit s: State): Mark.Closed = {
      assert(atAny(Set(TokenKind.Annotation, TokenKind.KeywordDef, TokenKind.CommentDoc)))
      val mark = open(consumeDocComments = false)
      Decl.docComment()
      Decl.annotations()
      expect(TokenKind.KeywordDef)
      name(NAME_DEFINITION)
      Decl.parameters()
      if (eat(TokenKind.Colon)) {
        Type.typeAndEffect()
      }
      expect(TokenKind.Equal)
      statement()
      close(mark, TreeKind.Expr.LetRecDef)
    }

    private def letImportExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordImport))
      val mark = open()
      expect(TokenKind.KeywordImport)
      val markJvmOp = open()
      nth(0) match {
        case TokenKind.KeywordJavaNew => JvmOp.constructor()
        case TokenKind.KeywordJavaGetField => JvmOp.getField()
        case TokenKind.KeywordJavaSetField => JvmOp.putField()
        case TokenKind.KeywordStatic => nth(1) match {
          case TokenKind.KeywordJavaGetField => JvmOp.staticGetField()
          case TokenKind.KeywordJavaSetField => JvmOp.staticPutField()
          case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.staticMethod()
          case t =>
            val error = ParseError(s"Expected static java import before ${t.display}.", SyntacticContext.Unknown, previousSourceLocation())
            advanceWithError(error)
        }
        case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.method()
        case t =>
          val error = ParseError(s"Expected java import before ${t.display}.", SyntacticContext.Unknown, previousSourceLocation())
          advanceWithError(error)
      }
      close(markJvmOp, TreeKind.JvmOp.JvmOp)
      expect(TokenKind.Semi)
      statement()
      close(mark, TreeKind.Expr.LetImport)
    }

    private def scopeExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordRegion))
      val mark = open()
      expect(TokenKind.KeywordRegion)
      name(NAME_VARIABLE)
      if (at(TokenKind.CurlyL)) {
        block()
      }
      close(mark, TreeKind.Expr.Scope)
    }

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

    private val FIRST_EXPR_DEBUG: Set[TokenKind] = Set(
      TokenKind.KeywordDebug,
      TokenKind.KeywordDebugBang,
      TokenKind.KeywordDebugBangBang
    )

    private def debugExpr()(implicit s: State): Mark.Closed = {
      assert(atAny(FIRST_EXPR_DEBUG))
      val mark = open()
      expectAny(FIRST_EXPR_DEBUG)
      expect(TokenKind.ParenL)
      expression()
      expect(TokenKind.ParenR)
      close(mark, TreeKind.Expr.Debug)
    }

    private def matchOrMatchLambdaExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordMatch))
      val mark = open()
      expect(TokenKind.KeywordMatch)
      // Detect match lambda
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
            case TokenKind.Eof => return closeWithError(mark, ParseError("Malformed match expression.", SyntacticContext.Expr.OtherExpr, currentSourceLocation()))
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
          comments()
          while (at(TokenKind.KeywordCase) && !eof()) {
            matchRule()
            eat(TokenKind.Comma)
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
      // TODO: It's common to type '=' instead of '=>' here. Should we make a specific error?
      if (eat(TokenKind.ArrowThickR)) {
        statement()
      }
      close(mark, TreeKind.Expr.MatchRuleFragment)
    }

    private def typematchExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordTypeMatch))
      val mark = open()
      expect(TokenKind.KeywordTypeMatch)
      expression()
      if (eat(TokenKind.CurlyL)) {
        comments()
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
      // TODO: It's common to type '=' instead of '=>' here. Should we make a specific error?
      if (eat(TokenKind.ArrowThickR)) {
        statement()
      }
      close(mark, TreeKind.Expr.TypeMatchRuleFragment)
    }

    private val FIRST_EXPR_CHOOSE: Set[TokenKind] = Set(TokenKind.KeywordChoose, TokenKind.KeywordChooseStar)

    private def restrictableChooseExpr()(implicit s: State): Mark.Closed = {
      assert(atAny(FIRST_EXPR_CHOOSE))
      val mark = open()
      val isStar = eat(TokenKind.KeywordChooseStar)
      if (!isStar) {
        expect(TokenKind.KeywordChoose)
      }
      expression()
      expect(TokenKind.CurlyL)
      comments()
      while (at(TokenKind.KeywordCase) && !eof()) {
        matchRule()
      }
      expect(TokenKind.CurlyR)
      close(mark, if (isStar) TreeKind.Expr.RestrictableChooseStar else TreeKind.Expr.RestrictableChoose)
    }

    private def forApplicativeExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordForA))
      val mark = open()
      expect(TokenKind.KeywordForA)
      if (at(TokenKind.ParenL)) {
        // Note: Only generator patterns are allowed here. Weeder verifies this.
        forFragments()
      }
      expect(TokenKind.KeywordYield)
      expression()
      close(mark, TreeKind.Expr.ForApplicative)
    }

    private def foreachExpr()(implicit s: State): Mark.Closed = {
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

    private def forMonadicExpr()(implicit s: State): Mark.Closed = {
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

    private def blockOrRecordExpr()(implicit s: State): Mark.Closed = {
      // Determines if a '{' is opening a block, a record literal or a record operation.
      assert(at(TokenKind.CurlyL))

      // We can discern between record ops and literals vs. blocks by looking at the two next tokens.
      (nth(1), nth(2)) match {
        case (TokenKind.CurlyR, _)
             | (TokenKind.NameLowerCase, TokenKind.Equal)
             | (TokenKind.Plus, TokenKind.NameLowerCase)
             | (TokenKind.Minus, TokenKind.NameLowerCase) =>
          // Now check for record operation or record literal,
          // by looking for a '|' before the closing '}'
          val isRecordOp = {
            var lookahead = 1
            var nestingLevel = 0
            var isRecordOp = false
            var continue = true
            while (continue && !eof()) {
              nth(lookahead) match {
                // Found closing '}' so stop seeking.
                case TokenKind.CurlyR if nestingLevel == 0 => continue = false
                // found '|' before closing '}' -> It is a record operation.
                case TokenKind.Bar if nestingLevel == 0 =>
                  isRecordOp = true
                  continue = false
                case TokenKind.CurlyL | TokenKind.HashCurlyL =>
                  nestingLevel += 1
                  lookahead += 1
                case TokenKind.CurlyR =>
                  nestingLevel -= 1
                  lookahead += 1
                case _ => lookahead += 1
              }
            }
            isRecordOp
          }

          if (isRecordOp) {
            recordOperation()
          } else {
            recordLiteral()
          }
        case _ => block()
      }
    }

    private def recordLiteral()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      zeroOrMore(
        displayName = "record field",
        getItem = recordLiteralField,
        checkForItem = () => atAny(NAME_FIELD),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
      close(mark, TreeKind.Expr.LiteralRecord)
    }

    private def recordLiteralField()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_FIELD)
      expect(TokenKind.Equal)
      expression()
      close(mark, TreeKind.Expr.LiteralRecordFieldFragment)
    }

    private val FIRST_RECORD_OP: Set[TokenKind] = Set(TokenKind.Plus, TokenKind.Minus, TokenKind.NameLowerCase)

    private def recordOperation()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      oneOrMore(
        displayName = "record operation",
        getItem = recordOp,
        checkForItem = () => atAny(FIRST_RECORD_OP),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR,
        optionallyWith = Some((TokenKind.Bar, () => expression()))
      ) match {
        case Some(error) => closeWithError(mark, error)
        case None => close(mark, TreeKind.Expr.RecordOperation)
      }
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
        case at =>
          val error = ParseError(s"Expected ${TokenKind.Plus.display}, ${TokenKind.Minus.display} or ${TokenKind.NameLowerCase.display} before ${at.display}", SyntacticContext.Expr.OtherExpr, currentSourceLocation())
          advanceWithError(error, Some(mark))
      }
    }

    private def arrayLiteralExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ArrayHash))
      val mark = open()
      expect(TokenKind.ArrayHash)
      zeroOrMore(
        displayName = "expression",
        getItem = () => expression(),
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.LiteralArray)
    }

    private def scopeName()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.At))
      val mark = open()
      expect(TokenKind.At)
      expression()
      close(mark, TreeKind.Expr.ScopeName)
    }

    private def vectorLiteralExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.VectorHash))
      val mark = open()
      expect(TokenKind.VectorHash)
      zeroOrMore(
        displayName = "expression",
        getItem = () => expression(),
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
      close(mark, TreeKind.Expr.LiteralVector)
    }

    private def listLiteralExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ListHash))
      val mark = open()
      expect(TokenKind.ListHash)
      zeroOrMore(
        displayName = "expression",
        getItem = () => expression(),
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
      close(mark, TreeKind.Expr.LiteralList)
    }

    private def setLiteralExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.SetHash))
      val mark = open()
      expect(TokenKind.SetHash)
      zeroOrMore(
        displayName = "expression",
        getItem = () => expression(),
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
      close(mark, TreeKind.Expr.LiteralSet)
    }

    private def mapLiteralExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.MapHash))
      val mark = open()
      expect(TokenKind.MapHash)
      zeroOrMore(
        displayName = "map literal",
        getItem = mapLiteralValue,
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      )
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

    private def refExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordRef))
      val mark = open()
      expect(TokenKind.KeywordRef)
      expression()
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.Ref)
    }

    private def checkedTypeCastExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCheckedCast))
      val mark = open()
      expect(TokenKind.KeywordCheckedCast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.CheckedTypeCast)
    }

    private def checkedEffectCastExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordCheckedECast))
      val mark = open()
      expect(TokenKind.KeywordCheckedECast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.CheckedEffectCast)
    }

    private def uncheckedCastExpr()(implicit s: State): Mark.Closed = {
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

    private def uncheckedMaskingCastExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordMaskedCast))
      val mark = open()
      expect(TokenKind.KeywordMaskedCast)
      if (eat(TokenKind.ParenL)) {
        expression()
        expect(TokenKind.ParenR)
      }
      close(mark, TreeKind.Expr.UncheckedMaskingCast)
    }

    private def tryExpr()(implicit s: State): Mark.Closed = {
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
      oneOrMore(
        displayName = "catch rule",
        getItem = catchRule,
        checkForItem = () => at(TokenKind.KeywordCase),
        recoverOn = () => atAny(RECOVER_EXPR),
        optionalSeparator = true,
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      ) match {
        case Some(error) => closeWithError(mark, error)
        case None => close(mark, TreeKind.Expr.TryCatchBodyFragment)
      }
    }

    private def catchRule()(implicit s: State): Mark.Closed = {
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
      oneOrMore(
        displayName = "with rule",
        getItem = withRule,
        checkForItem = () => at(TokenKind.KeywordDef),
        recoverOn = () => atAny(RECOVER_EXPR),
        optionalSeparator = true,
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR
      ) match {
        case Some(error) => closeWithError(mark, error)
        case None => close(mark, TreeKind.Expr.TryWithBodyFragment)
      }
    }

    private def withRule()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordDef))
      val mark = open()
      expect(TokenKind.KeywordDef)
      name(Set(TokenKind.NameLowerCase))
      Decl.parameters()
      if (eat(TokenKind.Equal)) {
        expression()
      }
      close(mark, TreeKind.Expr.TryWithRuleFragment)
    }

    private def doExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordDo))
      val mark = open()
      expect(TokenKind.KeywordDo)
      name(NAME_QNAME, allowQualified = true)
      arguments()
      close(mark, TreeKind.Expr.Do)
    }

    private def newObjectExpr()(implicit s: State): Mark.Closed = {
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
      if (eat(TokenKind.Colon)) {
        Type.typeAndEffect()
      }
      if (eat(TokenKind.Equal)) {
        Expr.statement()
      }
      close(mark, TreeKind.Expr.JvmMethod)
    }

    private def staticExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStaticUppercase))
      val mark = open()
      expect(TokenKind.KeywordStaticUppercase)
      close(mark, TreeKind.Expr.Static)
    }

    private def selectExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSelect))
      val mark = open()
      expect(TokenKind.KeywordSelect)

      expect(TokenKind.CurlyL)
      comments()
      var continue = true
      while (continue && at(TokenKind.KeywordCase) && !eof()) {
        val ruleMark = open()
        expect(TokenKind.KeywordCase)
        val isDefault = findBefore(TokenKind.ArrowThickR, Array(TokenKind.ArrowThinL))
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
          // Note that only "Channel.recv" and "recv" are allowed for this name.
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

    private def spawnExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordSpawn))
      val mark = open()
      expect(TokenKind.KeywordSpawn)
      expression()
      if (at(TokenKind.At)) {
        scopeName()
      }
      close(mark, TreeKind.Expr.Spawn)
    }

    private val RECOVER_EXPR_PAR_YIELD_FRAG: Set[TokenKind] = RECOVER_EXPR + TokenKind.KeywordYield

    private def parYieldExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordPar))
      val mark = open()
      expect(TokenKind.KeywordPar)
      if (at(TokenKind.ParenL)) {
        oneOrMore(
          displayName = "'pattern <- expression'",
          getItem = parYieldFragment,
          checkForItem = () => atAny(FIRST_PATTERN),
          separator = TokenKind.Semi,
          recoverOn = () => atAny(RECOVER_EXPR_PAR_YIELD_FRAG)
        ) match {
          case Some(error) => closeWithError(open(), error)
          case None =>
        }
      }
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

    private def fixpointConstraintSetExpr()(implicit s: State): Mark.Closed = {
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

    private def fixpointLambdaExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashParenL))
      val mark = open()
      Predicate.params()
      expect(TokenKind.ArrowThinR)
      expression()
      close(mark, TreeKind.Expr.FixpointLambda)
    }

    private def fixpointSolveExpr()(implicit s: State): Mark.Closed = {
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

    private def fixpointInjectExpr()(implicit s: State): Mark.Closed = {
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

    private def fixpointQueryExpr()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordQuery))
      val mark = open()
      expect(TokenKind.KeywordQuery)
      expression()
      while (eat(TokenKind.Comma) && !eof()) {
        expression()
      }
      if (at(TokenKind.KeywordSelect)) {
        fixpointQuerySelect()
      }
      if (at(TokenKind.KeywordFrom)) {
        fixpointQueryFrom()
      }
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
        case (TokenKind.ParenL, _) => zeroOrMore(
          displayName = "expression",
          getItem = () => expression(),
          checkForItem = () => atAny(FIRST_EXPR),
          recoverOn = () => atAny(RECOVER_EXPR),
        )
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

    private def intrinsicExpr()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Expr.Intrinsic)
    }

    private val FIRST_EXPR_INTERPOLATED_STRING: Set[TokenKind] = Set(TokenKind.LiteralStringInterpolationL, TokenKind.LiteralDebugStringL)

    private def interpolatedStringExpr()(implicit s: State): Mark.Closed = {
      assert(atAny(FIRST_EXPR_INTERPOLATED_STRING))
      val mark = open()

      def atTerminator(kind: Option[TokenKind]) = kind match {
        case Some(TokenKind.LiteralStringInterpolationL) => at(TokenKind.LiteralStringInterpolationR)
        case Some(TokenKind.LiteralDebugStringL) => at(TokenKind.LiteralDebugStringR)
        case _ => false
      }

      def getOpener: Option[TokenKind] = nth(0) match {
        case TokenKind.LiteralStringInterpolationL => advance(); Some(TokenKind.LiteralStringInterpolationL)
        case TokenKind.LiteralDebugStringL => advance(); Some(TokenKind.LiteralDebugStringR)
        case _ => None
      }

      var lastOpener = getOpener
      while (lastOpener.isDefined && !eof()) {
        if (atTerminator(lastOpener)) {
          lastOpener = None // Terminate the loop
        } else {
          expression()
          lastOpener = getOpener // Try to get nested interpolation
        }
      }
      expectAny(Set(TokenKind.LiteralStringInterpolationR, TokenKind.LiteralDebugStringR))
      close(mark, TreeKind.Expr.StringInterpolation)
    }
  }

  private object Pattern {
    def pattern()(implicit s: State): Mark.Closed = {
      // If a new pattern is added here, remember to add it to FIRST_PATTERN too.
      val mark = open()
      var lhs = nth(0) match {
        case TokenKind.NameLowerCase
             | TokenKind.NameGreek
             | TokenKind.NameMath
             | TokenKind.Underscore
             | TokenKind.KeywordQuery => variablePat()
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
             | TokenKind.KeywordNull => literalPat()
        case TokenKind.NameUpperCase => tagPat()
        case TokenKind.ParenL => tuplePat()
        case TokenKind.CurlyL => recordPat()
        case TokenKind.Minus => unaryPat()
        case t =>
          val mark = open()
          val error = ParseError(s"Expected <pattern> before ${t.display}", SyntacticContext.Pat.OtherPat, previousSourceLocation())
          closeWithError(mark, error)
      }
      // Handle FCons
      if (eat(TokenKind.ColonColon)) {
        lhs = close(openBefore(lhs), TreeKind.Pattern.Pattern)
        pattern()
        close(openBefore(lhs), TreeKind.Pattern.FCons)
      }
      close(mark, TreeKind.Pattern.Pattern)
    }

    private def variablePat()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_VARIABLE)
      close(mark, TreeKind.Pattern.Variable)
    }

    private def literalPat()(implicit s: State): Mark.Closed = {
      val mark = open()
      advance()
      close(mark, TreeKind.Pattern.Literal)
    }

    private def tagPat()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_TAG, allowQualified = true)
      if (at(TokenKind.ParenL)) {
        tuplePat()
      }
      close(mark, TreeKind.Pattern.Tag)
    }

    private def tuplePat()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      zeroOrMore(
        displayName = "pattern",
        getItem = pattern,
        checkForItem = () => atAny(FIRST_PATTERN),
        recoverOn = () => atAny(RECOVER_EXPR)
      )
      close(mark, TreeKind.Pattern.Tuple)
    }

    private def recordPat()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      zeroOrMore(
        displayName = "field name",
        getItem = recordField,
        checkForItem = () => atAny(NAME_FIELD),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR,
        optionallyWith = Some((TokenKind.Bar, () => pattern())),
        recoverOn = () => atAny(RECOVER_EXPR)
      )
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

    private def unaryPat()(implicit s: State): Mark.Closed = {
      val mark = open()
      val opMark = open()
      advance()
      close(opMark, TreeKind.Operator)
      advance()
      close(mark, TreeKind.Pattern.Unary)
    }
  }

  private object Type {
    def typeAndEffect()(implicit s: State): Mark.Closed = {
      val lhs = ttype()
      if (eat(TokenKind.Backslash)) {
        effectSet()
      } else lhs
    }

    def ttype(left: TokenKind = TokenKind.Eof)(implicit s: State): Mark.Closed = {
      var lhs = if (left == TokenKind.ArrowThinR) typeAndEffect() else typeDelimited()
      // handle Type argument application
      while (at(TokenKind.BracketL)) {
        val mark = openBefore(lhs)
        arguments()
        lhs = close(mark, TreeKind.Type.Apply)
        lhs = close(openBefore(lhs), TreeKind.Type.Type)
      }
      // Handle binary operators
      var continue = true
      while (continue) {
        val right = nth(0)
        if (right == TokenKind.Tilde) {
          // This branch is hit when tilde is used like a binary operator.
          // That only happens in equality constraints, an in that case,
          // we want to let that rule continue instead of producing a binary type expression.
          continue = false
        } else if (rightBindsTighter(left, right)) {
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

    // A precedence table for type operators, lower is higher precedence
    private def TYPE_OP_PRECEDENCE: List[Array[TokenKind]] = List(
      // BINARY OPS
      Array(TokenKind.ArrowThinR), // ->
      Array(TokenKind.KeywordRvadd, TokenKind.KeywordRvsub), // rvadd, rvsub
      Array(TokenKind.KeywordRvand), // rvand
      Array(TokenKind.Plus, TokenKind.Minus), // +, -
      Array(TokenKind.Ampersand), // &
      Array(TokenKind.KeywordXor), // xor
      Array(TokenKind.KeywordOr), // or
      Array(TokenKind.KeywordAnd), // and
      // UNARY OPS
      Array(TokenKind.KeywordRvnot, TokenKind.Tilde, TokenKind.KeywordNot) // rvnot, ~, not
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
      // This >= rather than > makes it so that operators with equal precedence are left-associative.
      // IE. 't + eff1 + eff2' becomes '(t + eff1) + eff2' rather than 't + (eff1 + eff2)'
      rt >= lt
    }

    def arguments()(implicit s: State): Mark.Closed = {
      val mark = open()
      zeroOrMore(
        displayName = "type argument",
        getItem = argument,
        checkForItem = () => atAny(FIRST_TYPE),
        delimiterL = TokenKind.BracketL,
        delimiterR = TokenKind.BracketR,
        recoverOn = () => atAny(RECOVER_TYPE)
      )
      close(mark, TreeKind.Type.ArgumentList)
    }

    private def argument()(implicit s: State): Mark.Closed = {
      val mark = open()
      ttype()
      close(mark, TreeKind.Type.Argument)
    }

    def parameters()(implicit s: State): Mark.Closed = {
      val mark = open()
      oneOrMore(
        displayName = "type parameter",
        getItem = parameter,
        checkForItem = () => atAny(NAME_VARIABLE ++ NAME_TYPE),
        delimiterL = TokenKind.BracketL,
        delimiterR = TokenKind.BracketR,
        recoverOn = () => atAny(RECOVER_TYPE)
      ) match {
        case Some(error) => closeWithError(mark, error)
        case None => close(mark, TreeKind.TypeParameterList)
      }
    }

    private def parameter()(implicit s: State): Mark.Closed = {
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
      // Note, Can't use zeroOrMore here since there's are no delimiterR.
      var continue = true
      while (continue && !eof()) {
        if (atAny(NAME_DEFINITION)) {
          constraint()
        } else {
          val error = ParseError("Expected type constraint", SyntacticContext.WithClause, currentSourceLocation())
          closeWithError(open(), error)
          continue = false
        }
        if (!eat(TokenKind.Comma)) {
          continue = false
        }
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
      // Note, Can't use zeroOrMore here since there's are no delimiterR.
      var continue = true
      while (continue && !eof()) {
        if (atAny(NAME_QNAME)) {
          name(NAME_QNAME, allowQualified = true)
        } else {
          val error = ParseError("Expected type derivation", SyntacticContext.WithClause, currentSourceLocation())
          closeWithError(open(), error)
          continue = false
        }
        if (!eat(TokenKind.Comma)) {
          continue = false
        }
      }
      close(mark, TreeKind.DerivationList)
    }

    private def typeDelimited()(implicit s: State): Mark.Closed = {
      // If a new type is added here, remember to add it to TYPE_FIRST too.
      val mark = open()
      nth(0) match {
        case TokenKind.NameUpperCase => name(NAME_TYPE, allowQualified = true)
        case TokenKind.NameMath
             | TokenKind.NameGreek
             | TokenKind.Underscore => name(NAME_VARIABLE)
        case TokenKind.NameLowerCase => variableType()
        case TokenKind.KeywordUniv
             | TokenKind.KeywordPure
             | TokenKind.KeywordFalse
             | TokenKind.KeywordTrue => constantType()
        case TokenKind.ParenL => tupleOrRecordRowType()
        case TokenKind.CurlyL => recordOrEffectSetType()
        case TokenKind.HashCurlyL => schemaType()
        case TokenKind.HashParenL => schemaRowType()
        case TokenKind.NameJava => nativeType()
        case TokenKind.AngleL => caseSetType()
        case TokenKind.KeywordNot
             | TokenKind.Tilde
             | TokenKind.KeywordRvnot => unaryType()
        // TODO: Static is used as a type name in Prelude.flix. That requires special handling here.
        // If we remove this rule, remove KeywordStaticUppercase from FIRST_TYPE too.
        case TokenKind.KeywordStaticUppercase => name(Set(TokenKind.KeywordStaticUppercase))
        case t =>
          val mark = open()
          val error = ParseError(s"Expected <type> before ${t.display}.", SyntacticContext.Type.OtherType, previousSourceLocation())
          closeWithError(mark, error)
      }
      close(mark, TreeKind.Type.Type)
    }

    private val TYPE_VAR: Set[TokenKind] = Set(TokenKind.NameLowerCase, TokenKind.NameGreek, TokenKind.NameMath, TokenKind.Underscore)

    private def variableType()(implicit s: State): Mark.Closed = {
      val mark = open()
      expectAny(TYPE_VAR)
      close(mark, TreeKind.Type.Variable)
    }

    private val TYPE_CONSTANT: Set[TokenKind] = Set(TokenKind.KeywordUniv, TokenKind.KeywordPure, TokenKind.KeywordFalse, TokenKind.KeywordTrue)

    private def constantType()(implicit s: State): Mark.Closed = {
      val mark = open()
      expectAny(TYPE_CONSTANT)
      close(mark, TreeKind.Type.Constant)
    }

    private def tupleOrRecordRowType()(implicit s: State): Mark.Closed = {
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

    private def recordRow()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      zeroOrMore(
        displayName = "record field",
        getItem = recordField,
        checkForItem = () => atAny(NAME_FIELD),
        recoverOn = () => atAny(RECOVER_TYPE),
        optionallyWith = Some((TokenKind.Bar, variableType))
      )
      close(mark, TreeKind.Type.RecordRow)
    }

    def tuple()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      zeroOrMore(
        displayName = "type",
        getItem = () => ttype(),
        checkForItem = () => atAny(FIRST_TYPE),
        recoverOn = () => atAny(RECOVER_TYPE)
      )
      close(mark, TreeKind.Type.Tuple)
    }

    private def recordOrEffectSetType()(implicit s: State): Mark.Closed = {
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

    private def record()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.CurlyL))
      val mark = open()
      zeroOrMore(
        displayName = "record field",
        getItem = recordField,
        checkForItem = () => atAny(NAME_FIELD),
        recoverOn = () => atAny(RECOVER_TYPE),
        delimiterL = TokenKind.CurlyL,
        delimiterR = TokenKind.CurlyR,
        optionallyWith = Some((TokenKind.Bar, variableType))
      )
      close(mark, TreeKind.Type.Record)
    }

    private def recordField()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_FIELD)
      expect(TokenKind.Equal)
      ttype()
      close(mark, TreeKind.Type.RecordFieldFragment)
    }

    def effectSet()(implicit s: State): Mark.Closed = {
      if (at(TokenKind.CurlyL)) {
        val mark = open()
        zeroOrMore(
          displayName = "effect",
          getItem = () => ttype(),
          checkForItem = () => atAny(FIRST_TYPE),
          recoverOn = () => atAny(RECOVER_TYPE),
          delimiterL = TokenKind.CurlyL,
          delimiterR = TokenKind.CurlyR
        )
        close(mark, TreeKind.Type.EffectSet)
      } else {
        val mark = open()
        ttype()
        close(mark, TreeKind.Type.EffectSet)
      }
    }

    private def schemaType()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashCurlyL))
      val mark = open()
      zeroOrMore(
        displayName = "schema term",
        getItem = schemaTerm,
        checkForItem = () => atAny(NAME_PREDICATE),
        delimiterL = TokenKind.HashCurlyL,
        delimiterR = TokenKind.CurlyR,
        recoverOn = () => atAny(RECOVER_TYPE),
        optionallyWith = Some(TokenKind.Bar, () => name(NAME_VARIABLE))
      )
      close(mark, TreeKind.Type.Schema)
    }

    private def schemaRowType()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.HashParenL))
      val mark = open()
      zeroOrMore(
        displayName = "schema term",
        getItem = schemaTerm,
        checkForItem = () => atAny(NAME_PREDICATE),
        delimiterL = TokenKind.HashParenL,
        recoverOn = () => atAny(RECOVER_TYPE),
        optionallyWith = Some(TokenKind.Bar, () => name(NAME_VARIABLE))
      )
      close(mark, TreeKind.Type.SchemaRow)
    }

    private def schemaTerm()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PREDICATE, allowQualified = true)
      if (at(TokenKind.BracketL)) {
        arguments()
        close(mark, TreeKind.Type.PredicateWithAlias)
      } else {
        zeroOrMore(
          displayName = "type",
          getItem = () => ttype(),
          checkForItem = () => atAny(FIRST_TYPE),
          recoverOn = () => atAny(RECOVER_TYPE),
          optionallyWith = Some(TokenKind.Semi, () => {
            val mark = open()
            ttype()
            close(mark, TreeKind.Predicate.LatticeTerm)
          })
        )
        close(mark, TreeKind.Type.PredicateWithTypes)
      }
    }

    private def nativeType()(implicit s: State): Mark.Closed = {
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

    private def caseSetType()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.AngleL))
      val mark = open()
      zeroOrMore(
        displayName = "definition name",
        getItem = () => name(NAME_DEFINITION, allowQualified = true),
        checkForItem = () => atAny(NAME_DEFINITION),
        recoverOn = () => atAny(RECOVER_TYPE),
        delimiterL = TokenKind.AngleL,
        delimiterR = TokenKind.AngleR
      )
      close(mark, TreeKind.Type.CaseSet)
    }

    private val FIRST_TYPE_UNARY: Set[TokenKind] = Set(TokenKind.Tilde, TokenKind.KeywordNot, TokenKind.KeywordRvnot)

    private def unaryType()(implicit s: State): Mark.Closed = {
      val mark = open()
      val op = nth(0)
      val markOp = open()
      expectAny(FIRST_TYPE_UNARY)
      close(markOp, TreeKind.Operator)
      ttype(left = op)
      close(mark, TreeKind.Type.Unary)
    }

    def kind()(implicit s: State): Mark.Closed = {
      def kindFragment(): Unit = {
        val inParens = eat(TokenKind.ParenL)
        name(NAME_KIND)
        // Check for arrow kind
        if (eat(TokenKind.ArrowThinR)) {
          kind()
        }
        // consume ')' is necessary
        if (inParens) {
          expect(TokenKind.ParenR)
        }
      }

      val mark = open()
      kindFragment()
      close(mark, TreeKind.Kind)
    }
  }

  private object Predicate {
    def head()(implicit s: State): Mark.Closed = {
      val mark = open()
      name(NAME_PREDICATE)
      if (at(TokenKind.ParenL)) {
        termList()
      }
      close(mark, TreeKind.Predicate.Head)
    }

    private def termList()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.ParenL))
      val mark = open()
      zeroOrMore(
        displayName = "expression",
        getItem = () => Expr.expression(),
        checkForItem = () => atAny(FIRST_EXPR),
        recoverOn = () => atAny(RECOVER_EXPR),
        optionallyWith = Some(TokenKind.Semi, () => {
          val mark = open()
          Expr.expression()
          close(mark, TreeKind.Predicate.LatticeTerm)
        })
      )
      close(mark, TreeKind.Predicate.TermList)
    }

    private def patternList()(implicit s: State): Mark.Closed = {
      val mark = open()
      zeroOrMore(
        displayName = "pattern",
        getItem = Pattern.pattern,
        checkForItem = () => atAny(FIRST_PATTERN),
        recoverOn = () => atAny(RECOVER_PATTERN),
        optionallyWith = Some(TokenKind.Semi, () => {
          val mark = open()
          Pattern.pattern()
          close(mark, TreeKind.Predicate.LatticeTerm)
        })
      )
      close(mark, TreeKind.Predicate.PatternList)
    }

    def body()(implicit s: State): Mark.Closed = {
      val mark = open()
      nth(0) match {
        case TokenKind.KeywordIf => guard()
        case TokenKind.KeywordLet => functional()
        case TokenKind.KeywordNot | TokenKind.KeywordFix | TokenKind.NameUpperCase => atom()
        case at =>
          val error = ParseError(s"Expected ${TokenKind.KeywordIf.display}, ${TokenKind.KeywordLet.display}, ${TokenKind.KeywordNot.display}, ${TokenKind.KeywordFix} or ${TokenKind.NameUpperCase.display} before ${at.display}", SyntacticContext.Unknown, currentSourceLocation())
          advanceWithError(error)
      }
      close(mark, TreeKind.Predicate.Body)
    }

    private def guard()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordIf))
      val mark = open()
      expect(TokenKind.KeywordIf)
      Expr.expression()
      close(mark, TreeKind.Predicate.Guard)
    }

    private def functional()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordLet))
      val mark = open()
      expect(TokenKind.KeywordLet)
      nth(0) match {
        case TokenKind.ParenL => zeroOrMore(
          displayName = "variable name",
          getItem = () => name(NAME_VARIABLE),
          checkForItem = () => atAny(NAME_VARIABLE),
          recoverOn = () => atAny(FIRST_EXPR + TokenKind.Equal)
        )
        case TokenKind.NameLowerCase
             | TokenKind.NameMath
             | TokenKind.NameGreek
             | TokenKind.Underscore => name(NAME_VARIABLE)
        case at =>
          val error = ParseError(s"Expected ${TokenKind.ParenL.display} or name before ${at.display}", SyntacticContext.Unknown, currentSourceLocation())
          advanceWithError(error)
      }
      expect(TokenKind.Equal)
      Expr.expression()
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
      zeroOrMore(
        displayName = "parameter predicate",
        getItem = Predicate.param,
        checkForItem = () => atAny(NAME_PREDICATE),
        delimiterL = TokenKind.HashParenL,
        recoverOn = () => atAny(FIRST_EXPR + TokenKind.ArrowThinR)
      )
      close(mark, TreeKind.Predicate.ParamList)
    }

    private def param()(implicit s: State): Mark.Closed = {
      var kind: TreeKind = TreeKind.Predicate.ParamUntyped
      val mark = open()
      name(NAME_PREDICATE)
      kind = TreeKind.Predicate.Param
      zeroOrMore(
        displayName = "type",
        getItem = () => Type.ttype(),
        checkForItem = () => atAny(FIRST_TYPE),
        recoverOn = () => atAny(RECOVER_TYPE),
        optionallyWith = Some(TokenKind.Semi, () => {
          val mark = open()
          Type.ttype()
          close(mark, TreeKind.Predicate.LatticeTerm)
        })
      )
      close(mark, kind)
    }

  }

  private object JvmOp {
    private def signature()(implicit s: State): Unit = {
      val mark = open()
      zeroOrMore(
        displayName = "type",
        getItem = () => Type.ttype(),
        checkForItem = () => atAny(FIRST_TYPE),
        recoverOn = () => atAny(RECOVER_TYPE)
      )
      close(mark, TreeKind.JvmOp.Sig)
    }

    private def ascription()(implicit s: State): Mark.Closed = {
      val mark = open()
      expect(TokenKind.Colon)
      Type.typeAndEffect()
      close(mark, TreeKind.JvmOp.Ascription)
    }

    def constructor()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordJavaNew))
      val mark = open()
      expect(TokenKind.KeywordJavaNew)
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

    private def fieldGetBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordJavaGetField)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    private def putBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordJavaSetField)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    def getField()(implicit s: State): Mark.Closed = {
      val mark = open()
      fieldGetBody()
      close(mark, TreeKind.JvmOp.GetField)
    }

    def staticGetField()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      fieldGetBody()
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

  /**
    * Utility function that computes a textual representation of a [[SyntaxTree.Tree]].
    * Meant for debugging use.
    */
  def syntaxTreeToDebugString(tree: SyntaxTree.Tree, nesting: Int = 1): String = {
    s"${tree.kind}${
      tree.children.map {
        case SyntaxTree.Child.TokenChild(token) => s"\n${"  " * nesting}${token.text}"
        case SyntaxTree.Child.TreeChild(tree) => s"\n${"  " * nesting}${syntaxTreeToDebugString(tree, nesting + 1)}"
      }.mkString("")
    }"
  }
}
