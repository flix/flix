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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugTokens
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.PrefixTree

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/**
  * A lexer that is able to tokenize multiple `Source`s in parallel.
  * This lexer is resilient, meaning that when an unrecognized character is encountered,
  * the lexer will simply produce a token of kind `TokenKind.Err` an move on instead of halting.
  * There are some unrecoverable errors though, for example unterminated block-comments or unclosed
  * string literals. In these cases a `TokenKind.Err` will still be produced but it will contain the
  * rest of the source text.
  *
  * See `LexerError` for all error states.
  */
object Lexer {

  /** The end-of-file character (`'\u0000'`). */
  private val EOF = '\u0000'

  /** Keywords - tokens consumed as long as no name-like char follows. */
  private val Keywords: PrefixTree.Node[TokenKind] = {
    // N.B.: `advanceIfInTree` takes the longest match, regardless of the ordering here.
    val keywords = Array(
      ("Array#", TokenKind.ArrayHash),
      ("List#", TokenKind.ListHash),
      ("Map#", TokenKind.MapHash),
      ("Set#", TokenKind.SetHash),
      ("Static", TokenKind.KeywordStaticUppercase),
      ("Univ", TokenKind.KeywordUniv),
      ("Vector#", TokenKind.VectorHash),
      ("alias", TokenKind.KeywordAlias),
      ("and", TokenKind.KeywordAnd),
      ("as", TokenKind.KeywordAs),
      ("case", TokenKind.KeywordCase),
      ("catch", TokenKind.KeywordCatch),
      ("checked_cast", TokenKind.KeywordCheckedCast),
      ("checked_ecast", TokenKind.KeywordCheckedECast),
      ("choose", TokenKind.KeywordChoose),
      ("choose*", TokenKind.KeywordChooseStar),
      ("def", TokenKind.KeywordDef),
      ("discard", TokenKind.KeywordDiscard),
      ("eff", TokenKind.KeywordEff),
      ("else", TokenKind.KeywordElse),
      ("ematch", TokenKind.KeywordEMatch),
      ("enum", TokenKind.KeywordEnum),
      ("false", TokenKind.KeywordFalse),
      ("fix", TokenKind.KeywordFix),
      ("forA", TokenKind.KeywordForA),
      ("forM", TokenKind.KeywordForM),
      ("forall", TokenKind.KeywordForall),
      ("force", TokenKind.KeywordForce),
      ("foreach", TokenKind.KeywordForeach),
      ("from", TokenKind.KeywordFrom),
      ("handler", TokenKind.KeywordHandler),
      ("if", TokenKind.KeywordIf),
      ("import", TokenKind.KeywordImport),
      ("inject", TokenKind.KeywordInject),
      ("instance", TokenKind.KeywordInstance),
      ("instanceof", TokenKind.KeywordInstanceOf),
      ("into", TokenKind.KeywordInto),
      ("law", TokenKind.KeywordLaw),
      ("lawful", TokenKind.KeywordLawful),
      ("lazy", TokenKind.KeywordLazy),
      ("let", TokenKind.KeywordLet),
      ("match", TokenKind.KeywordMatch),
      ("mod", TokenKind.KeywordMod),
      ("mut", TokenKind.KeywordMut),
      ("new", TokenKind.KeywordNew),
      ("not", TokenKind.KeywordNot),
      ("null", TokenKind.KeywordNull),
      ("open_variant", TokenKind.KeywordOpenVariant),
      ("open_variant_as", TokenKind.KeywordOpenVariantAs),
      ("or", TokenKind.KeywordOr),
      ("override", TokenKind.KeywordOverride),
      ("par", TokenKind.KeywordPar),
      ("pquery", TokenKind.KeywordPQuery),
      ("project", TokenKind.KeywordProject),
      ("psolve", TokenKind.KeywordPSolve),
      ("pub", TokenKind.KeywordPub),
      ("query", TokenKind.KeywordQuery),
      ("redef", TokenKind.KeywordRedef),
      ("region", TokenKind.KeywordRegion),
      ("restrictable", TokenKind.KeywordRestrictable),
      ("run", TokenKind.KeywordRun),
      ("rvadd", TokenKind.KeywordRvadd),
      ("rvand", TokenKind.KeywordRvand),
      ("rvnot", TokenKind.KeywordRvnot),
      ("rvsub", TokenKind.KeywordRvsub),
      ("sealed", TokenKind.KeywordSealed),
      ("select", TokenKind.KeywordSelect),
      ("solve", TokenKind.KeywordSolve),
      ("spawn", TokenKind.KeywordSpawn),
      ("static", TokenKind.KeywordStatic),
      ("struct", TokenKind.KeywordStruct),
      ("throw", TokenKind.KeywordThrow),
      ("trait", TokenKind.KeywordTrait),
      ("true", TokenKind.KeywordTrue),
      ("try", TokenKind.KeywordTry),
      ("type", TokenKind.KeywordType),
      ("typematch", TokenKind.KeywordTypeMatch),
      ("unchecked_cast", TokenKind.KeywordUncheckedCast),
      ("unsafe", TokenKind.KeywordUnsafe),
      ("unsafely", TokenKind.KeywordUnsafely),
      ("use", TokenKind.KeywordUse),
      ("where", TokenKind.KeywordWhere),
      ("with", TokenKind.KeywordWith),
      ("without", TokenKind.KeywordWithout),
      ("xor", TokenKind.KeywordXor),
      ("xvar", TokenKind.KeywordXvar),
      ("yield", TokenKind.KeywordYield),
    )
    PrefixTree.mk(keywords)
  }

  /** Simple tokens - tokens consumed no matter the following char. */
  private val SimpleTokens: PrefixTree.Node[TokenKind] = {
    // N.B.: `advanceIfInTree` takes the longest match, regardless of the ordering here.
    val simpleTokens = Array(
      ("#", TokenKind.Hash),
      ("#(", TokenKind.HashParenL),
      ("#{", TokenKind.HashCurlyL),
      ("#|", TokenKind.HashBar),
      ("(", TokenKind.ParenL),
      (")", TokenKind.ParenR),
      (",", TokenKind.Comma),
      (".{", TokenKind.DotCurlyL),
      (";", TokenKind.Semi),
      ("???", TokenKind.HoleAnonymous),
      ("[", TokenKind.BracketL),
      ("\\", TokenKind.Backslash),
      ("]", TokenKind.BracketR),
      ("{", TokenKind.CurlyL),
      ("|#", TokenKind.BarHash),
      ("}", TokenKind.CurlyR),
      ("~", TokenKind.Tilde),
    )
    PrefixTree.mk(simpleTokens)
  }

  /** Operators - tokens consumed as long as no operator-like char follows (see [[isUserOp]]). */
  private val Operators: PrefixTree.Node[TokenKind] = {
    // N.B.: `advanceIfInTree` takes the longest match, regardless of the ordering here.
    val simpleTokens = Array(
      ("!", TokenKind.Bang),
      ("!=", TokenKind.BangEqual),
      ("=", TokenKind.Equal),
      ("&", TokenKind.Ampersand),
      ("*", TokenKind.Star),
      ("+", TokenKind.Plus),
      ("-", TokenKind.Minus),
      (":", TokenKind.Colon),
      (":-", TokenKind.ColonMinus),
      ("::", TokenKind.ColonColon),
      (":::", TokenKind.TripleColon),
      ("<", TokenKind.AngleL),
      ("<+>", TokenKind.AngledPlus),
      ("<-", TokenKind.ArrowThinL),
      ("<=", TokenKind.AngleLEqual),
      ("==", TokenKind.EqualEqual),
      ("=>", TokenKind.ArrowThickR),
      (">", TokenKind.AngleR),
      (">=", TokenKind.AngleREqual),
      ("^", TokenKind.Caret),
      ("|", TokenKind.Bar),
    )
    PrefixTree.mk(simpleTokens)
  }

  /** The internal state of the lexer as it tokenizes a single source. */
  private class State(val src: Source) {

    /** A string cursor on `src.data`. */
    val sc: StringCursor = new StringCursor(src.data)

    /** `start` is the first position of the token that is currently being lexed. */
    var start: Position = new Position(sc.getLine, sc.getColumn, sc.getOffset)

    /** Set `start` to the current position. */
    def resetStart(): Unit = {
      start = new Position(sc.getLine, sc.getColumn, sc.getOffset)
    }

    /**
      * The sequence of tokens produced by the lexer.
      *
      * Note: The initial size of the array buffer has been determined by careful profiling.
      */
    val tokens: mutable.ArrayBuffer[Token] = new mutable.ArrayBuffer(initialSize = 256)

  }

  /** A source position keeping track of both line, column as well as absolute character offset. */
  private class Position(val line: Int, val column: Int, val offset: Int)

  /** Run the lexer on multiple `Source`s in parallel. */
  def run(root: ReadAst.Root, oldTokens: Map[Source, Array[Token]], changeSet: ChangeSet)(implicit flix: Flix): (Map[Source, Array[Token]], List[LexerError]) =
    flix.phaseNew("Lexer") {
      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(root.sources, oldTokens)

      // Sort the stale inputs by size to increase throughput to start work early on the biggest
      // tasks.
      val staleByDecreasingSize = stale.keys.toList.sortBy(s => -s.data.length)

      // Lex each stale source file in parallel.
      val (results, errors) = ParOps.parMap(staleByDecreasingSize) {
        src =>
          val (tokens, errors) = lex(src)
          val fuzzedTokens = fuzz(tokens)
          (src -> fuzzedTokens, errors)
      }.unzip

      // Construct a map from each source to its tokens.
      val all = results ++ fresh
      (all.toMap, errors.flatten.toList)
    }

  /** Lexes a single source (file) into an array of tokens. */
  def lex(src: Source): (Array[Token], List[LexerError]) = {
    implicit val s: State = new State(src)
    while (!eof()) {
      s.sc.advanceWhile(_.isWhitespace)
      if (!eof()) {
        s.resetStart()
        val k = scanToken()
        addToken(k)
      }
    }

    // Add a virtual eof token at the last position.
    s.resetStart()
    addToken(TokenKind.Eof)

    val errors = s.tokens.collect {
      case Token(TokenKind.Err(err), _, _, _, _, _) => err
    }

    (s.tokens.toArray, errors.toList)
  }

  /** Checks if the current position has landed on end-of-file. */
  private def eof()(implicit s: State): Boolean =
    s.sc.eof

  /** Returns a single-width [[SourceLocation]] starting at [[State.start]]. */
  private def sourceLocationAtStart()(implicit s: State): SourceLocation =
    sourceLocationFromZeroIndex(s.src, line = s.start.line, column = s.start.column)

  /** Returns a single-width [[SourceLocation]] starting at the current position. */
  private def sourceLocationAtCurrent()(implicit s: State): SourceLocation =
    sourceLocationFromZeroIndex(s.src, line = s.sc.getLine, column = s.sc.getColumn)

  /**
    * Returns a single-width [[SourceLocation]] with the `src` and the zero-indexed `line` and
    * `column`.
    */
  private def sourceLocationFromZeroIndex(src: Source, line: Int, column: Int): SourceLocation = {
    val sp1 = SourcePosition.mkFromZeroIndexed(line, column)
    // It is safe not consider line breaks because `column + 1` is an exclusive index.
    val sp2 = SourcePosition.mkFromZeroIndexed(line, column + 1)
    SourceLocation(isReal = true, src, sp1, sp2)
  }

  /**
    * Consumes the text between `s.start` and `s.offset` to produce a token.
    * Afterwards `s.start` is reset to the next position after the previous token.
    */
  private def addToken(kind: TokenKind)(implicit s: State): Unit = {
    val b = SourcePosition.mkFromZeroIndexed(s.start.line, s.start.column)
    // If we are currently at the start of a line, create a non-existent position and the
    // end of the previous line as the exclusive end position.
    // This should not happen for zero-width tokens at the start of lines.
    val (endLine, endColumn) = if (s.start.offset != s.sc.getOffset) s.sc.getExclusiveEndPosition else s.sc.getPosition
    val e = SourcePosition.mkFromZeroIndexed(endLine, endColumn)
    s.tokens.append(Token(kind, s.src, s.start.offset, s.sc.getOffset, b, e))
    s.resetStart()
  }

  /**
    * Scans the source for the next available token.
    * This is the heart of the lexer implementation.
    * `scanToken` determines what TokenKind the next
    * available token should be by looking at the coming character in source.
    * This requires potentially infinite look-ahead for things like block-comments and formatted
    * strings, but in many cases one or two characters is enough.
    */
  private def scanToken()(implicit s: State): TokenKind = {
    // Beware that the order of these match cases affect both behaviour and performance.
    // If the order needs to change, make sure to run tests and benchmarks.

    acceptIfSimpleToken() match {
      case Some(token) => return token
      case None => // nop
    }

    acceptIfOperator() match {
      case Some(token) => return token
      case None => // nop
    }

    acceptIfKeyword() match {
      case Some(token) => return token
      case None => // nop
    }

    s.sc.peekAndAdvance() match {
      case '.' =>
        if (s.sc.advanceIfMatch("..")) {
          TokenKind.DotDotDot
        } else if (s.sc.nth(-2).exists(_.isWhitespace)) {
          // If the dot is prefixed with whitespace we treat that as an error.
          TokenKind.Err(LexerError.FreeDot(sourceLocationAtStart()))
        } else if (s.sc.peekIs(_.isWhitespace)) {
          // A dot with trailing whitespace is it's own TokenKind.
          // That way we can use that as a terminator for fixpoint constraints,
          // without clashing with qualified names.
          // For example this is not allowed "Shape.    Rectangle".
          TokenKind.DotWhiteSpace
        } else {
          TokenKind.Dot
        }
      case '%' if s.sc.peekIs(_ == '%') => acceptBuiltIn()
      case '$' if s.sc.peekIs(isFirstNameChar) =>
        // Don't include the $ sign in the name.
        s.resetStart()
        acceptName(isUpper = false)
      case '$' if s.sc.peekIs(c => !isUserOp(c)) => TokenKind.Dollar
      case '\"' => acceptString()
      case '\'' => acceptChar()
      case '`' => acceptInfixFunction()
      case '/' if s.sc.advanceIfMatch('/') => acceptLineOrDocComment()
      case '/' if s.sc.advanceIfMatch('*') => acceptBlockComment()
      case '/' => TokenKind.Slash
      case '@' if s.sc.peekIs(isAnnotationChar) => acceptAnnotation()
      case '@' => TokenKind.At
      case '?' if s.sc.peekIs(isFirstNameChar) => acceptNamedHole()
      case '-' if s.sc.peekIs(_ == '>') && s.sc.nthIsPOrOutOfBounds(1, c => !isUserOp(c)) =>
        s.sc.advance() // Consume '>'
        // If any whitespace exists around the `->`, it is `ArrowThinR`. Otherwise it is `StructArrow`.
        // Examples:
        // a->b:   StructArrow
        // a ->b:  ArrowThinR
        // a-> b:  ArrowThinR
        // a -> b: ArrowThinR
        if (s.sc.nthIsPOrOutOfBounds(-3, _.isWhitespace) || s.sc.peekIs(_.isWhitespace)) {
          TokenKind.ArrowThinR
        } else {
          TokenKind.StructArrow
        }
      case 'd' if s.sc.peekIs(_ == '"') => TokenKind.DebugInterpolator
      case 'r' if s.sc.advanceIfMatch("egex\"") => acceptRegex()
      case c if isFirstNameChar(c) => acceptName(c.isUpper)
      case c if isMathNameChar(c) => acceptMathName()
      case '_' =>
        if (!eof()) {
          val p = s.sc.peek
          if (isFirstNameChar(p)) {
            s.sc.advance()
            acceptName(p.isUpper)
          } else if (isMathNameChar(p)) {
            s.sc.advance()
            acceptMathName()
          } else if (isUserOp(p)) {
            s.sc.advance()
            acceptUserDefinedOp()
          } else TokenKind.Underscore
        } else TokenKind.Underscore
      case '0' if s.sc.peekIs(_ == 'x') => acceptHexNumber()
      case c if c.isDigit => acceptNumber()
      case c if isUserOp(c) => acceptUserDefinedOp()
      case c => TokenKind.Err(LexerError.UnexpectedChar(c.toString, sourceLocationAtStart()))
    }
  }

  /** Advance the current position past an operator if any operator completely matches the current position. */
  private def acceptIfOperator()(implicit s: State): Option[TokenKind] =
    advanceIfInTree(Operators, c => !isUserOp(c))

  /** Advance the current position past a simple token if any simple token matches the current position. */
  private def acceptIfSimpleToken()(implicit s: State): Option[TokenKind] =
    advanceIfInTree(SimpleTokens, _ => true)

  /** Advance the current position past a keyword if any keyword completely matches the current word. */
  private def acceptIfKeyword()(implicit s: State): Option[TokenKind] =
    advanceIfInTree(Keywords, c => !isNameChar(c))

  /**
    * Advance the current position past a longest match in  `node` if it is followed by a char
    * where `tailCondition(c)` is `true` or is followed by EOF.
    */
  private def advanceIfInTree(node: PrefixTree.Node[TokenKind], tailCondition: Char => Boolean)(implicit s: State): Option[TokenKind] = {
    @tailrec
    def loop(offset: Int, node: PrefixTree.Node[TokenKind]): Option[TokenKind] = {
      // A potential match must be:
      //   - The longest match in the keyword map.
      //   - Followed by a char that matches `tailCondition` (or EOF).
      s.sc.nth(offset) match {
        case Some(c) =>
          node.getNode(c) match {
            case Some(nextNode) =>
              loop(offset + 1, nextNode)
            case None =>
              if (!tailCondition(c)) {
                // Not a full match - Not a keyword.
                return None
              }
              // Full match - A keyword if node has a token.
              val res = node.getValue
              res.foreach(_ => s.sc.advanceN(offset))
              res
          }
        case None =>
          // EOF - A keyword if node has a token.
          val res = node.getValue
          res.foreach(_ => s.sc.advanceN(offset))
          res
      }
    }

    loop(0, node)
  }

  /** Moves current position past a built-in function (e.g. "%%BUILT_IN%%"). */
  private def acceptBuiltIn()(implicit s: State): TokenKind = {
    s.sc.advance() // Consume `%`.
    s.sc.advanceWhile(isBuiltInChar)
    if (s.sc.advanceIfMatch("%%")) {
      TokenKind.BuiltIn
    } else {
      TokenKind.Err(LexerError.UnterminatedBuiltIn(sourceLocationAtStart()))
    }
  }

  /** Returns `true` if `c` is allowed inside a built-in name. */
  private def isBuiltInChar(c: Char): Boolean =
    (c.isLetter && c.isUpper) || c.isDigit || c == '_'

  /**
    * Moves the current position past all pairs of `\` and any other character, returning
    * true if any '\' are seen.
    *
    * This is useful to avoid `\'` and `\"` ending the lexing of literals, and to
    * determine whether a '$' before a '{' is escaped or a string interpolation indicator.
    */
  private def consumeSingleEscapes()(implicit s: State): Boolean =
    if (s.sc.advanceIfMatch('\\')) {
      s.sc.advance()
      while (s.sc.advanceIfMatch('\\')) {
        s.sc.advance()
      }
      true
    } else {
      false
    }

  /**
    * Moves current position past a name (both upper- and lower-case).
    * There are edge cases of variable holes (e.g. "x?"), and java names (e.g. "Map$Entry"),
    * which is the reason this function will return a `TokenKind`.
    */
  private def acceptName(isUpper: Boolean)(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isNameChar)
    if (s.sc.advanceIfMatch('?')) {
      TokenKind.HoleVariable
    } else if (isUpper) {
      TokenKind.NameUpperCase
    } else {
      TokenKind.NameLowerCase
    }
  }

  /**
    * Returns true if `c` is allowed as the first char of a name (see [[isNameChar]]).
    *
    * All chars where `true` is returned has lower/upper case defined.
    */
  private def isFirstNameChar(c: Char): Boolean =
    c.isLetter

  /** Returns `true` if `c` is allowed inside a name (see [[isFirstNameChar]]). */
  private def isNameChar(c: Char): Boolean =
    c.isLetter || c.isDigit || c == '_' || c == '!' || c == '$'

  /**
    * Moves current position past a math name.
    * Math names must lie in the unicode range U+2190 to U+22FF (e.g. "⊆")
    */
  private def acceptMathName()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isMathNameChar)
    TokenKind.NameMath
  }

  /** Checks whether `c` lies in unicode range U+2190 to U+22FF. */
  private def isMathNameChar(c: Char): Boolean =
    0x2190 <= c && c <= 0x22FF

  /** Moves current position past a named hole (e.g. "?foo"). */
  private def acceptNamedHole()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isNameChar)
    TokenKind.HoleNamed
  }

  /** Moves current position past an infix function. */
  private def acceptInfixFunction()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(
      c => isNameChar(c) || c == '.' || isMathNameChar(c)
    )
    if (s.sc.advanceIfMatch('`')) {
      TokenKind.InfixFunction
    } else {
      TokenKind.Err(LexerError.UnterminatedInfixFunction(sourceLocationAtStart()))
    }
  }

  /**
    * Moves current position past a user defined operator (e.g. "<*>").
    * A user defined operator may be any combination of length 2 or more
    * of the characters in [[isUserOp]].
    */
  private def acceptUserDefinedOp()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isUserOp)
    TokenKind.UserDefinedOperator
  }

  /** The characters allowed in a user defined operator. */
  private def isUserOp(c: Char): Boolean = {
    c match {
      case '+' => true
      case '-' => true
      case '*' => true
      case '<' => true
      case '>' => true
      case '=' => true
      case '!' => true
      case '&' => true
      case '|' => true
      case '^' => true
      case '$' => true
      case _ => false
    }
  }

  /**
    * Moves current position past a string literal.
    * If the string is unterminated a `TokenKind.Err` is returned.
    */
  private def acceptString()(implicit s: State): TokenKind = {
    var kind: TokenKind = TokenKind.LiteralString
    while (!eof()) {
      val hasEscapes = consumeSingleEscapes()
      var p = if (!eof()) {
        s.sc.peek
      } else {
        return TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
      }
      // Check for the beginning of a string interpolation.
      val prev = s.sc.previous
      val isInterpolation = !hasEscapes && prev.contains('$') & p == '{'
      if (isInterpolation) {
        acceptStringInterpolation() match {
          case e@TokenKind.Err(_) => return e
          case k =>
            // Resume regular string literal tokenization by resetting p and prev.
            kind = k
            consumeSingleEscapes()
            if (!eof()) {
              p = s.sc.peek
            } else {
              return TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
            }
        }
      }
      // Check for termination.
      if (p == '\"') {
        s.sc.advance()
        return kind
      }
      // Check if file ended on a '\', meaning that the string was unterminated.
      if (p == '\n') {
        return TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
      }
      s.sc.advance()
    }
    TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
  }

  /**
    * Moves current position past an interpolated expression within a string. (e.g. "Hi ${name}!").
    * This function also handles debug strings like "Value: %{x}".
    * Note that this function works a little differently to the other `accept*` functions
    * since it will produce a number of tokens before returning.
    * This is necessary since it must be able to move past any expressions,
    * including nested string literals, which might also include interpolation.
    * This is done by calling `scanToken` and `addToken` manually like in the top-most `lex`
    * function, while looking for the terminating '}'.
    *
    * Some tricky but valid strings include:
    *   - "Hello ${" // "} world!"
    *   - "My favorite number is ${ { "${"//"}"40 + 2} }!"
    *   - "${"${}"}"
    */
  private def acceptStringInterpolation()(implicit s: State): TokenKind = {
    val startLocation = sourceLocationAtCurrent()
    s.sc.advance() // Consume '{'.
    addToken(TokenKind.LiteralStringInterpolationL)
    // Consume tokens until a terminating '}' is found.
    var blockNestingLevel = 0
    while (!eof()) {
      s.sc.advanceWhile(_.isWhitespace)
      if (!eof()) {
        s.resetStart()
        val kind = scanToken()

        // Handle nested block expressions like `"my favorite number is ${ {40 + 2} }!"`.
        if (kind == TokenKind.CurlyL) {
          blockNestingLevel += 1
        }

        // Check for the terminating '}'.
        if (kind == TokenKind.CurlyR) {
          if (blockNestingLevel == 0) {
            return TokenKind.LiteralStringInterpolationR
          }
          blockNestingLevel -= 1
        }
        addToken(kind)
      }
    }
    TokenKind.Err(LexerError.UnterminatedStringInterpolation(startLocation))
  }

  /**
    * Moves current position past a char literal.
    * If the char is unterminated a `TokenKind.Err` is returned.
    * Note that chars might contain unicode hex codes like these '\u00ff'.
    */
  private def acceptChar()(implicit s: State): TokenKind = {
    var prev = ' '
    while (!eof()) {
      consumeSingleEscapes()
      if (s.sc.peekIs(_ == '\'')) {
        s.sc.advance()
        return TokenKind.LiteralChar
      }
      prev = s.sc.peekAndAdvance()
    }

    TokenKind.Err(LexerError.UnterminatedChar(sourceLocationAtStart()))
  }

  /**
    * Moves current position past a regex literal.
    * If the regex  is unterminated a `TokenKind.Err` is returned.
    */
  private def acceptRegex()(implicit s: State): TokenKind = {
    while (!eof()) {
      consumeSingleEscapes()
      if (s.sc.peekIs(_ == '"')) {
        s.sc.advance()
        return TokenKind.LiteralRegex
      }
      s.sc.advance()
    }

    TokenKind.Err(LexerError.UnterminatedRegex(sourceLocationAtStart()))
  }

  /** Returns `true` if `c` is recognized by `[0-9a-z._]`. */
  private def isNumberLikeChar(c: Char): Boolean =
    c.isDigit || c.isLetter || c == '.' || c == '_'

  /** Consumes the remaining [[isNumberLikeChar]] characters and returns `error`. */
  private def wrapAndConsume(error: LexerError)(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isNumberLikeChar)
    TokenKind.Err(error)
  }

  /** This should be understood as a control effect - fully handled inside [[acceptNumber]]. */
  private sealed case class NumberError(kind: TokenKind) extends RuntimeException

  /**
    * Moves current position past a number literal. (e.g. "123i32", "3_456.78f32", or "2.1e4").
    * It is optional to have a trailing type indicator on number literals.
    * If it is missing Flix defaults to `f64` for decimals and `i32` for integers.
    *
    * A number is accepted by `\D([.]\D)?(e([+]|[-])?\D([.]\D)?)?(i8|i16|i32|i64|ii|f32|f64|ff)?`
    * where `\D = [0-9]+(_[0-9]+)*`.
    *
    * Note that any characters in `[0-9a-zA-Z_.]` following a number should be treated as an error
    * part of the same number, e.g., `32q` should be parsed as a single wrong number, and not a
    * number (`32`) and a name (`q`).
    */
  private def acceptNumber()(implicit s: State): TokenKind = {
    var mustBeFloat = false

    /**
      * Consume a '\D' ('[0-9]+(_[0-9]+)*') string, or '[0-9]*(_[0-9]+)*' if 'soft'.
      * Throws [[NumberError]] if not matched.
      */
    def acceptDigits(soft: Boolean): Unit = {
      if (s.sc.advanceWhileWithCount(_.isDigit) == 0 && !soft) {
        throw NumberError(wrapAndConsume(LexerError.ExpectedDigit(sourceLocationAtCurrent())))
      }
      while (s.sc.advanceIfMatch('_')) {
        if (s.sc.advanceWhileWithCount(_.isDigit) == 0) {
          throw NumberError(wrapAndConsume(LexerError.ExpectedDigit(sourceLocationAtCurrent())))
        }
      }
    }

    /** Consume a '([.]\D)?' string or throws [[NumberError]]. */
    def acceptCommaTail(): Unit = {
      if (s.sc.advanceIfMatch('.')) {
        mustBeFloat = true
        acceptDigits(soft = false)
      }
    }

    try {
      // Consume a '\D' string (An initial digit has already been consumed externally, so `soft = true`).
      acceptDigits(soft = true)
      // Consume a '([.]\D)?' string.
      acceptCommaTail()
      // Consume a '(e([+]|[-])?\D([.]\D)?)?' string.
      if (s.sc.advanceIfMatch('e')) {
        mustBeFloat = true
        // Consume a '([+]|[-])?' string.
        if (!s.sc.advanceIfMatch('+')) {
          s.sc.advanceIfMatch('-')
        }
        // Consume a '\D' string.
        acceptDigits(soft = false)
        // Consume a '([.]\D)?' string.
        acceptCommaTail()
      }
    } catch {
      case NumberError(kind) => return kind
    }

    // Now the main number is parsed. Next is the suffix.

    def acceptOrSuffixError(token: TokenKind, intSuffix: Boolean, start: SourceLocation): TokenKind = {
      if (s.sc.peekIs(isNumberLikeChar)) {
        wrapAndConsume(LexerError.IncorrectNumberSuffix(start))
      } else if (mustBeFloat && intSuffix) {
        wrapAndConsume(LexerError.IntegerSuffixOnFloat(start))
      } else token
    }

    // Consume a '(i8|i16|i32|i64|ii|f32|f64|ff)?' string.
    // For better errors, anything starting with 'i' or 'f' will be considered a suffix (but maybe invalid).
    // This means that '32i33' will report 'i33' is an invalid suffix instead of saying that 'i' is unexpected.
    val c = s.sc.peek
    if (c == 'i') {
      // Construct the location now, for cases like `42i322`.
      val loc = sourceLocationAtCurrent()

      if (s.sc.advanceIfMatch("i8")) acceptOrSuffixError(TokenKind.LiteralInt8, intSuffix = true, loc)
      else if (s.sc.advanceIfMatch("i16")) acceptOrSuffixError(TokenKind.LiteralInt16, intSuffix = true, loc)
      else if (s.sc.advanceIfMatch("i32")) acceptOrSuffixError(TokenKind.LiteralInt32, intSuffix = true, loc)
      else if (s.sc.advanceIfMatch("i64")) acceptOrSuffixError(TokenKind.LiteralInt64, intSuffix = true, loc)
      else if (s.sc.advanceIfMatch("ii")) acceptOrSuffixError(TokenKind.LiteralBigInt, intSuffix = true, loc)
      else wrapAndConsume(LexerError.IncorrectNumberSuffix(loc))
    } else if (c == 'f') {
      // Construct the location now, for cases like `42f322`.
      val loc = sourceLocationAtCurrent()

      if (s.sc.advanceIfMatch("f32")) acceptOrSuffixError(TokenKind.LiteralFloat32, intSuffix = false, loc)
      else if (s.sc.advanceIfMatch("f64")) acceptOrSuffixError(TokenKind.LiteralFloat64, intSuffix = false, loc)
      else if (s.sc.advanceIfMatch("ff")) acceptOrSuffixError(TokenKind.LiteralBigDecimal, intSuffix = false, loc)
      else wrapAndConsume(LexerError.IncorrectNumberSuffix(loc))
    } else if (isNumberLikeChar(c)) {
      wrapAndConsume(LexerError.MalformedNumber(c.toString, sourceLocationAtCurrent()))
    } else {
      if (mustBeFloat) TokenKind.LiteralFloat
      else TokenKind.LiteralInt
    }
  }

  /**
    * Moves current position past a hex number literal (e.g. "0x123i32" or "0xAB21CD").
    * It is optional to have a trailing type indicator on number literals.
    * If it is missing Flix defaults to `i32`.
    *
    * A hex number is accepted by `0x\h+(_\h+)*(i8|i16|i32|i64|ii)?` where `\h = [0-9a-fA-F]`.
    *
    * Note that any characters in `[0-9a-zA-Z_.]` following a number should be treated as an error
    * part of the same number, e.g., `0x32q` should be parsed as a single wrong number, and not a
    * number (`0x32`) and a name (`q`).
    */
  private def acceptHexNumber()(implicit s: State): TokenKind = {
    def isHexDigit(c: Char): Boolean = '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

    s.sc.advance() // Consume 'x'.

    // Consume a `\h+` string
    if (s.sc.advanceWhileWithCount(isHexDigit) == 0) {
      return wrapAndConsume(LexerError.ExpectedHexDigit(sourceLocationAtCurrent()))
    }

    // Consume a `(_\h+)*`
    while (s.sc.advanceIfMatch('_')) {
      if (s.sc.advanceWhileWithCount(isHexDigit) == 0) {
        return wrapAndConsume(LexerError.ExpectedHexDigit(sourceLocationAtCurrent()))
      }
    }

    // Consume a '(i8|i16|i32|i64|ii)?' string.
    // For better errors, anything starting with 'i' will be considered a suffix (but maybe invalid).
    // This means that '0xFFi33' will report 'i33' is an invalid suffix instead of saying that 'i' is unexpected.
    val c = s.sc.peek
    if (c == 'i') {
      // Construct the location now, for cases like `0xi322`.
      val loc = sourceLocationAtCurrent()

      def acceptOrSuffixError(token: TokenKind): TokenKind = {
        if (s.sc.peekIs(isNumberLikeChar)) {
          wrapAndConsume(LexerError.IncorrectHexNumberSuffix(loc))
        } else token
      }

      if (s.sc.advanceIfMatch("i8")) acceptOrSuffixError(TokenKind.LiteralInt8)
      else if (s.sc.advanceIfMatch("i16")) acceptOrSuffixError(TokenKind.LiteralInt16)
      else if (s.sc.advanceIfMatch("i32")) acceptOrSuffixError(TokenKind.LiteralInt32)
      else if (s.sc.advanceIfMatch("i64")) acceptOrSuffixError(TokenKind.LiteralInt64)
      else if (s.sc.advanceIfMatch("ii")) acceptOrSuffixError(TokenKind.LiteralBigInt)
      else wrapAndConsume(LexerError.IncorrectHexNumberSuffix(sourceLocationAtCurrent()))
    } else if (isNumberLikeChar(c)) {
      wrapAndConsume(LexerError.MalformedHexNumber(c.toString, sourceLocationAtCurrent()))
    } else TokenKind.LiteralInt
  }

  /** Moves current position past an annotation (e.g. "@Test"). */
  private def acceptAnnotation()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isAnnotationChar)
    TokenKind.Annotation
  }

  /** Returns `true` if `c` can be used in annotation names. */
  private def isAnnotationChar(c: Char): Boolean =
    c.isLetter

  /**
    * Moves current position past a line-comment or a line of a doc-comment.
    *
    * N.B.: The content just before the current position is assumed to be "X//"
    * where `X != '/'` if it exists.
    */
  private def acceptLineOrDocComment()(implicit s: State): TokenKind = {
    // A doc comment leads with exactly 3 slashes, for example `//// example` is NOT a doc comment.
    val slashCount = s.sc.advanceWhileWithCount(_ == '/')
    s.sc.advanceWhile(c => c != '\n')
    if (slashCount == 1) TokenKind.CommentDoc else TokenKind.CommentLine
  }

  /**
    * Moves current position past a block-comment. Note that block-comments can be nested.
    * A block-comment is unterminated if there are less terminations than levels of nesting.
    */
  private def acceptBlockComment()(implicit s: State): TokenKind = {
    var level = 1
    while (s.sc.inBounds) {
      if (s.sc.advanceIfMatch("/*")) {
        level += 1
      } else if (s.sc.advanceIfMatch("*/")) {
        level -= 1
        if (level == 0) {
          return TokenKind.CommentBlock
        }
      } else {
        s.sc.advance()
      }
    }
    TokenKind.Err(LexerError.UnterminatedBlockComment(sourceLocationAtStart()))
  }

  /**
    * Returns a fuzzed array of tokens based on the given array of `tokens`.
    *
    * Must not modify the last token since it is end-of-file.
    */
  private def fuzz(tokens: Array[Token])(implicit flix: Flix): Array[Token] = {
    // Return immediately if fuzzing is disabled.
    if (!flix.options.xfuzzer) {
      return tokens
    }

    // Return immediately if there are few tokens.
    if (tokens.length <= 10) {
      return tokens
    }

    // Fuzz the array by picking two random indices and swapping their tokens.
    val copy = tokens.clone()
    val lastIndex = copy.length - 1 // Don't remove the last EOF token.
    val r = new Random()
    val i = r.nextInt(lastIndex)
    val j = r.nextInt(lastIndex)

    val tmp = copy(i)
    copy(i) = copy(j)
    copy(j) = tmp

    copy
  }

  /**
    * A class to iterate through an array of characters while maintaining the line and column index
    * of the cursor.
    */
  private final class StringCursor(val data: Array[Char]) {

    /** The cursor pointing into `data`. */
    private var offset: Int = 0

    /** The line index of `offset`. */
    private var line: Int = 0

    /** The column index of `offset`. */
    private var column: Int = 0

    /** The max column index of the previous line or `0` if there is no previous line. */
    private var prevLineMaxColumn = 0

    /** Returns the current line index. */
    def getLine: Int = line

    /** Returns the current column index. */
    def getColumn: Int = column

    /** Returns the current source offset. */
    def getOffset: Int = offset

    /** Returns `(line, column)`. */
    def getPosition: (Int, Int) = (line, column)

    /**
      * Returns `(line, column)` where non-existent positions are preferred instead of the first
      * position of the next line.
      *
      * In this example, the current position is on 'v' (1,0) but this function will then return
      * the position just after 'Example' (0, 7). This is sometimes preferable for exclusive
      * end positions of ranges.
      *
      * {{{
      *   Example
      *   v
      * }}}
      */
    def getExclusiveEndPosition: (Int, Int) =
      if (line <= 0 || column > 0) {
        (line, column)
      } else {
        (line - 1, prevLineMaxColumn + 1)
      }

    /**
      * Advances cursor one char forward, returning the char it was previously sitting on.
      *
      * A faster alternative to
      * {{{
      * {val c = this.peek; this.advance(); c}
      * }}}
      */
    def peekAndAdvance(): Char = {
      if (this.inBounds) {
        val c = data(offset)
        advance()
        c
      } else {
        EOF
      }
    }

    /** Advances cursor one char forward, unless out of bounds. */
    def advance(): Unit = {
      if (!this.eof) {
        if (data(offset) == '\n') {
          prevLineMaxColumn = column
          line += 1
          column = 0
        } else {
          column += 1
        }
        offset += 1
      }
    }

    /** Advances cursor `n` chars forward, or until out of bounds. */
    @tailrec
    def advanceN(n: Int): Unit = {
      if (0 < n) {
        advance()
        advanceN(n - 1)
      }
    }

    /** Peeks the character that is `n` characters ahead of the cursor if available. */
    def nth(n: Int): Option[Char] = {
      val index = offset + n
      if (0 <= index && index < data.length) {
        Some(data(index))
      } else {
        None
      }
    }

    /** Peeks the previous character if available. */
    def previous: Option[Char] = nth(-1)

    /**
      * Peeks the character that cursor is currently sitting on without advancing.
      *
      * If the cursor has advanced past the content, [[EOF]] is returned.
      */
    def peek: Char =
      if (this.inBounds) {
        data(offset)
      } else {
        EOF
      }

    /** Returns `p(this.peek)` if peek is available, otherwise `false`. */
    def peekIs(p: Char => Boolean): Boolean =
      if (this.inBounds) {
        p(data(offset))
      } else {
        false
      }

    /** Returns true if the cursor has moved past the end. */
    def eof: Boolean = offset >= data.length

    /** Returns true if the cursor has not reached end of file. */
    def inBounds: Boolean = offset < data.length

    /**
      * Advance the cursor past `s` if it matches the current content.
      *
      * Returns true if the cursor was advanced.
      */
    def advanceIfMatch(s: String): Boolean = {
      if (this.offset + s.length > data.length) {
        return false
      }

      var sIndex = 0
      while (sIndex < s.length) {
        if (data(this.offset + sIndex) != s(sIndex)) {
          return false
        }
        sIndex += 1
      }

      for (_ <- 0 until s.length) {
        advance()
      }

      true
    }

    /**
      * Advance the cursor past `c` if it matches the current char.
      *
      * Returns true if the cursor was advanced.
      */
    def advanceIfMatch(c: Char): Boolean =
      if (this.inBounds && data(offset) == c) {
        advance()
        true
      } else {
        false
      }

    /** Continuously advance the cursor while `p` returns true. */
    def advanceWhile(p: Char => Boolean): Unit =
      while (this.inBounds && p(data(offset))) {
        advance()
      }

    /** Continuously advance the cursor while `p` returns true. Returns the number of advances. */
    def advanceWhileWithCount(p: Char => Boolean): Int = {
      val startingOffset = offset
      while (this.inBounds && p(data(offset))) {
        advance()
      }
      offset - startingOffset
    }

    /** Faster equivalent of `nth(n).map(p).getOrElse(true)`. */
    def nthIsPOrOutOfBounds(n: Int, p: Char => Boolean): Boolean = {
      val index = offset + n
      if (0 <= index && index < data.length) {
        p(data(index))
      } else {
        true
      }
    }

  }

}

