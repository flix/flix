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
import ca.uwaterloo.flix.util.{ParOps, StringCursor}
import ca.uwaterloo.flix.util.collection.PrefixTree

import scala.annotation.{tailrec, unused}
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

  /** A median chars per token estimation used for the initial size of the token array buffer. */
  private val CharsPerToken: Int = {
    // As of September 2025 the standard library has a median of 6.9.
    8
  }

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
      ("static", TokenKind.KeywordStaticLowercase),
      ("struct", TokenKind.KeywordStruct),
      ("super", TokenKind.KeywordSuper),
      ("throw", TokenKind.KeywordThrow),
      ("trait", TokenKind.KeywordTrait),
      ("true", TokenKind.KeywordTrue),
      ("try", TokenKind.KeywordTry),
      ("type", TokenKind.KeywordType),
      ("unchecked_cast", TokenKind.KeywordUncheckedCast),
      ("unsafe", TokenKind.KeywordUnsafe),
      ("use", TokenKind.KeywordUse),
      ("where", TokenKind.KeywordWhere),
      ("with", TokenKind.KeywordWith),

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
      (";", TokenKind.Semi),
      ("???", TokenKind.HoleAnonymous),
      ("[", TokenKind.BracketL),
      ("\\", TokenKind.Backslash),
      ("]", TokenKind.BracketR),
      ("`", TokenKind.Tick),
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
      ("&", TokenKind.Ampersand),
      ("*", TokenKind.Star),
      ("+", TokenKind.Plus),
      ("-", TokenKind.Minus),
      (":", TokenKind.Colon),
      (":-", TokenKind.ColonMinus),
      ("::", TokenKind.ColonColon),
      (":::", TokenKind.ColonColonColon),
      ("<", TokenKind.AngleL),
      ("<+>", TokenKind.AngledPlus),
      ("<-", TokenKind.ArrowThinL),
      ("<=", TokenKind.AngleLEqual),
      ("<=>", TokenKind.AngledEqual),
      ("=", TokenKind.Equal),
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

    /** The first position of the token that is currently being lexed. */
    var startPos: SourcePosition = SourcePosition.mkFromZeroIndexed(sc.getLine, sc.getColumn.toShort)

    /** The first offset of the token that is currently being lexed. */
    var startOffset: Int = sc.getIndex

    /** Set `start` to the current position. */
    def resetStart(): Unit = {
      startPos = SourcePosition.mkFromZeroIndexed(sc.getLine, sc.getColumn.toShort)
      startOffset = sc.getIndex
    }

    /** The sequence of tokens produced by the lexer. */
    val tokens: mutable.ArrayBuffer[Token] = {
      // The needed size is guessed based on the chars per token estimate, reducing the amount of enlargements.
      new mutable.ArrayBuffer(initialSize = src.data.length / CharsPerToken)
    }

    /** The list of errors in the `tokens` array. */
    val errors: mutable.ArrayBuffer[LexerError] = new mutable.ArrayBuffer[LexerError]()

  }

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
          (src -> tokens, errors)
      }.unzip

      // Construct a map from each source to its tokens.
      val all = fresh.concat(results)
      (all, errors.flatten.toList)
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

    (s.tokens.toArray, s.errors.toList)
  }

  /** Checks if the current position has landed on end-of-file. */
  private def eof()(implicit s: State): Boolean =
    s.sc.isEof

  /**
    * Consumes the text between `s.start` and `s.offset` to produce a token.
    * Afterwards `s.start` is reset to the next position after the previous token.
    */
  private def addToken(kind: TokenKind)(implicit s: State): Unit = {
    val (b, e) = getRangeFromStart()
    s.tokens.append(Token(kind, s.src, s.startOffset, s.sc.getIndex, b, e))
    s.resetStart()
  }

  /** Wraps `error` in [[TokenKind]] and pushes the error to [[State]]. */
  private def mkErrorKind(error: LexerError)(implicit s: State): TokenKind = {
    s.errors.append(error)
    TokenKind.Err(error)
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
      case 'd' if s.sc.peekIs(_ == '"', outOfBounds = false) => TokenKind.DebugInterpolator
      case 'r' if s.sc.advanceIfMatch("egex\"") => acceptRegex()
      case c if isFirstNameChar(c) => acceptName(c.isUpper)
      case '.' =>
        if (s.sc.advanceIfMatch("..")) {
          TokenKind.DotDotDot
        } else if (s.sc.nth(-2).exists(_.isWhitespace)) {
          // If the dot is prefixed with whitespace we treat that as an error.
          mkErrorKind(LexerError.FreeDot(sourceLocationAtStart()))
        } else if (s.sc.peekIs(_.isWhitespace, outOfBounds = false)) {
          // A dot with trailing whitespace is it's own TokenKind.
          // That way we can use that as a terminator for fixpoint constraints,
          // without clashing with qualified names.
          // For example this is not allowed "Shape.    Rectangle".
          TokenKind.DotWhiteSpace
        } else {
          TokenKind.Dot
        }
      case '%' if s.sc.advanceIfMatch('%') => acceptBuiltIn()
      case '$' =>
        if (s.sc.peekIs(isFirstNameChar, outOfBounds = false)) {
          acceptEscapedName()
        } else if (s.sc.peekIs(isUserOp, outOfBounds = false)) {
          acceptUserDefinedOp()
        } else {
          TokenKind.Dollar
        }
      case '\"' => acceptString()
      case '\'' => acceptChar()
      case '/' =>
        if (s.sc.advanceIfMatch('/')) {
          acceptLineOrDocComment()
        } else if (s.sc.advanceIfMatch('*')) {
          acceptBlockComment()
        } else TokenKind.Slash
      case '@' =>
        if (s.sc.peekIs(isAnnotationChar, outOfBounds = false)) {
          acceptAnnotation()
        } else {
          TokenKind.At
        }
      case '?' if s.sc.peekIs(isFirstNameChar, outOfBounds = false) => acceptNamedHole()
      case '-' if s.sc.peekIs(_ == '>', outOfBounds = false) && s.sc.nthIs(1, c => !isUserOp(c), outOfBounds = true) =>
        s.sc.advance() // Consume '>'
        // If any whitespace exists around the `->`, it is `ArrowThinR`. Otherwise it is `StructArrow`.
        // Examples:
        // a->b:   StructArrow
        // a ->b:  ArrowThinR
        // a-> b:  ArrowThinR
        // a -> b: ArrowThinR
        if (s.sc.nthIs(-3, _.isWhitespace, outOfBounds = true) || s.sc.peekIs(_.isWhitespace, outOfBounds = true)) {
          TokenKind.ArrowThinRWhitespace
        } else {
          TokenKind.ArrowThinRTight
        }
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
      case c if isDigit(c) =>
        if (c == '0' && s.sc.advanceIfMatch('x')) {
          acceptHexNumber()
        } else {
          acceptNumber()
        }
      case c if isUserOp(c) => acceptUserDefinedOp()
      case c => mkErrorKind(LexerError.UnexpectedChar(c, sourceLocationAtCurrent()))
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
    s.sc.advanceWhile(isBuiltInChar)
    if (s.sc.advanceIfMatch("%%")) {
      TokenKind.BuiltIn
    } else {
      mkErrorKind(LexerError.UnterminatedBuiltIn(sourceLocationFromStart()))
    }
  }

  /** Returns `true` if `c` is allowed inside a built-in name. */
  private def isBuiltInChar(c: Char): Boolean =
    (isLetter(c) && c.isUpper) || isDigit(c) || c == '_'

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
      TokenKind.NameUppercase
    } else {
      TokenKind.NameLowercase
    }
  }

  /**
    * Returns true if `c` is allowed as the first char of a name (see [[isNameChar]]).
    *
    * All chars where `true` is returned has lower/upper case defined.
    */
  private def isFirstNameChar(c: Char): Boolean =
    isLetter(c)

  /** Returns `true` if `c` is allowed inside a name (see [[isFirstNameChar]]). */
  private def isNameChar(c: Char): Boolean =
    isLetter(c) || isDigit(c) || c == '_' || c == '!' || c == '$'

  /**
    * Moves current position past a math name.
    * Math names must lie in the unicode range U+2190 to U+22FF (e.g. "⊆")
    */
  private def acceptMathName()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isMathNameChar)
    TokenKind.NameMath
  }

  /** Checks whether `c` lies in unicode range U+2200 to U+22FF. */
  private def isMathNameChar(c: Char): Boolean =
    0x2200 <= c && c <= 0x22FF

  /** Moves current position past a named hole (e.g. "?foo"). */
  private def acceptNamedHole()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isNameChar)
    TokenKind.HoleNamed
  }

  /** Moves current position past an escaped name (e.g. `$run`). */
  private def acceptEscapedName()(implicit s: State): TokenKind = {
    // Don't include the $ sign in the name.
    s.resetStart()
    acceptName(isUpper = false)
  }

  /**
    * Moves current position past a user defined operator (e.g. "<*>").
    * A user defined operator may be any combination of length 2 or more
    * of the characters in [[isUserOp]].
    */
  private def acceptUserDefinedOp()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isUserOp)
    TokenKind.GenericOperator
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
        return mkErrorKind(LexerError.UnterminatedString(sourceLocationFromStart()))
      }
      // Check for the beginning of a string interpolation.
      val prev = s.sc.nth(-1)
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
              return mkErrorKind(LexerError.UnterminatedString(sourceLocationFromStart()))
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
        return mkErrorKind(LexerError.UnterminatedString(sourceLocationFromStart()))
      }
      s.sc.advance()
    }
    mkErrorKind(LexerError.UnterminatedString(sourceLocationFromStart()))
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
    mkErrorKind(LexerError.UnterminatedStringInterpolation(startLocation))
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
      if (s.sc.advanceIfMatch('\'')) {
        return TokenKind.LiteralChar
      }
      prev = s.sc.peekAndAdvance()
    }

    mkErrorKind(LexerError.UnterminatedChar(sourceLocationFromStart()))
  }

  /**
    * Moves current position past a regex literal.
    * If the regex  is unterminated a `TokenKind.Err` is returned.
    */
  private def acceptRegex()(implicit s: State): TokenKind = {
    while (!eof()) {
      consumeSingleEscapes()
      if (s.sc.advanceIfMatch('"')) {
        return TokenKind.LiteralRegex
      }
      s.sc.advance()
    }

    mkErrorKind(LexerError.UnterminatedRegex(sourceLocationFromStart()))
  }

  /** Returns `true` if `c` is recognized by `[0-9a-z._]`. */
  private def isNumberLikeChar(c: Char): Boolean =
    isDigit(c) || isLetter(c) || c == '.' || c == '_'

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
      if (s.sc.advanceWhileWithCount(isDigit) == 0 && !soft) {
        val loc = sourceLocationAtCurrent()
        s.sc.advanceWhile(isNumberLikeChar)
        throw NumberError(mkErrorKind(LexerError.ExpectedDigit(loc)))
      }
      while (s.sc.advanceIfMatch('_')) {
        if (s.sc.advanceWhileWithCount(isDigit) == 0) {
          val loc = sourceLocationAtCurrent()
          s.sc.advanceWhile(isNumberLikeChar)
          throw NumberError(mkErrorKind(LexerError.ExpectedDigit(loc)))
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

    def acceptOrSuffixError(token: TokenKind, intSuffix: Boolean, suffixStart: SourcePosition): TokenKind = {
      if (s.sc.peekIs(isNumberLikeChar, outOfBounds = false)) {
        s.sc.advanceWhile(isNumberLikeChar)
        mkErrorKind(LexerError.IncorrectNumberSuffix(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
      } else if (mustBeFloat && intSuffix) {
        s.sc.advanceWhile(isNumberLikeChar)
        mkErrorKind(LexerError.IntegerSuffixOnFloat(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
      } else token
    }

    // Consume a '(i8|i16|i32|i64|ii|f32|f64|ff)?' string.
    // For better errors, anything starting with 'i' or 'f' will be considered a suffix (but maybe invalid).
    // This means that '32i33' will report 'i33' is an invalid suffix instead of saying that 'i' is unexpected.
    val c = s.sc.peek
    if (c == 'i') {
      // Construct the position now, for cases like `42i322`.
      val suffixStart = sourcePositionAtCurrent()

      if (s.sc.advanceIfMatch("i8")) acceptOrSuffixError(TokenKind.LiteralInt8, intSuffix = true, suffixStart)
      else if (s.sc.advanceIfMatch("i16")) acceptOrSuffixError(TokenKind.LiteralInt16, intSuffix = true, suffixStart)
      else if (s.sc.advanceIfMatch("i32")) acceptOrSuffixError(TokenKind.LiteralInt32, intSuffix = true, suffixStart)
      else if (s.sc.advanceIfMatch("i64")) acceptOrSuffixError(TokenKind.LiteralInt64, intSuffix = true, suffixStart)
      else if (s.sc.advanceIfMatch("ii")) acceptOrSuffixError(TokenKind.LiteralBigInt, intSuffix = true, suffixStart)
      else {
        s.sc.advanceWhile(isNumberLikeChar)
        mkErrorKind(LexerError.IncorrectNumberSuffix(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
      }
    } else if (c == 'f') {
      // Construct the position now, for cases like `42f322`.
      val suffixStart = sourcePositionAtCurrent()

      if (s.sc.advanceIfMatch("f32")) acceptOrSuffixError(TokenKind.LiteralFloat32, intSuffix = false, suffixStart)
      else if (s.sc.advanceIfMatch("f64")) acceptOrSuffixError(TokenKind.LiteralFloat64, intSuffix = false, suffixStart)
      else if (s.sc.advanceIfMatch("ff")) acceptOrSuffixError(TokenKind.LiteralBigDecimal, intSuffix = false, suffixStart)
      else {
        s.sc.advanceWhile(isNumberLikeChar)
        mkErrorKind(LexerError.IncorrectNumberSuffix(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
      }
    } else if (isNumberLikeChar(c)) {
      val loc = sourceLocationAtCurrent()
      s.sc.advanceWhile(isNumberLikeChar)
      mkErrorKind(LexerError.MalformedNumber(c, loc))
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

    // Consume a `\h+` string
    if (s.sc.advanceWhileWithCount(isHexDigit) == 0) {
      val loc = sourceLocationAtCurrent()
      s.sc.advanceWhile(isNumberLikeChar)
      return mkErrorKind(LexerError.ExpectedHexDigit(loc))
    }

    // Consume a `(_\h+)*`
    while (s.sc.advanceIfMatch('_')) {
      if (s.sc.advanceWhileWithCount(isHexDigit) == 0) {
        val loc = sourceLocationAtCurrent()
        s.sc.advanceWhile(isNumberLikeChar)
        return mkErrorKind(LexerError.ExpectedHexDigit(loc))
      }
    }

    // Consume a '(i8|i16|i32|i64|ii)?' string.
    // For better errors, anything starting with 'i' will be considered a suffix (but maybe invalid).
    // This means that '0xFFi33' will report 'i33' is an invalid suffix instead of saying that 'i' is unexpected.
    val c = s.sc.peek
    if (c == 'i') {
      // Construct the position now, for cases like `0xi322`.
      val suffixStart = sourcePositionAtCurrent()

      def acceptOrSuffixError(token: TokenKind): TokenKind = {
        if (s.sc.peekIs(isNumberLikeChar, outOfBounds = false)) {
          s.sc.advanceWhile(isNumberLikeChar)
          mkErrorKind(LexerError.IncorrectHexNumberSuffix(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
        } else token
      }

      if (s.sc.advanceIfMatch("i8")) acceptOrSuffixError(TokenKind.LiteralInt8)
      else if (s.sc.advanceIfMatch("i16")) acceptOrSuffixError(TokenKind.LiteralInt16)
      else if (s.sc.advanceIfMatch("i32")) acceptOrSuffixError(TokenKind.LiteralInt32)
      else if (s.sc.advanceIfMatch("i64")) acceptOrSuffixError(TokenKind.LiteralInt64)
      else if (s.sc.advanceIfMatch("ii")) acceptOrSuffixError(TokenKind.LiteralBigInt)
      else {
        s.sc.advanceWhile(isNumberLikeChar)
        mkErrorKind(LexerError.IncorrectHexNumberSuffix(mkSourceLocation(suffixStart, sourcePositionAtCurrent())))
      }
    } else if (isNumberLikeChar(c)) {
      val loc = sourceLocationAtCurrent()
      s.sc.advanceWhile(isNumberLikeChar)
      mkErrorKind(LexerError.MalformedHexNumber(c, loc))
    } else TokenKind.LiteralInt
  }

  /** Moves current position past an annotation (e.g. "@Test"). */
  private def acceptAnnotation()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isAnnotationChar)
    TokenKind.Annotation
  }

  /** Returns `true` if `c` can be used in annotation names. */
  private def isAnnotationChar(c: Char): Boolean =
    isLetter(c)

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
    while (s.sc.isInBounds) {
      s.sc.advanceWhile(c => c != '/' && c != '*')
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
    mkErrorKind(LexerError.UnterminatedBlockComment(sourceLocationFromStart()))
  }

  /** Returns `true` if `c` is a letter. */
  private def isLetter(c: Char): Boolean =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  /** Returns `true` if `c` is a digit. */
  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  /** Creates a [[SourceLocation]] of the inclusive `start` and exclusive `end` in the current source. */
  private def mkSourceLocation(start: SourcePosition, end: SourcePosition)(implicit s: State): SourceLocation =
    SourceLocation(isReal = true, s.src, start, end)

  /** Returns the [[SourcePosition]] at [[State.startPos]]. */
  private def sourcePositionAtStart()(implicit s: State): SourcePosition =
    s.startPos

  /** Returns a single-width [[SourceLocation]] starting at [[State.startPos]]. */
  private def sourceLocationAtStart()(implicit s: State): SourceLocation =
    SourceLocation.point(isReal = true, s.src, sourcePositionAtStart())

  /** Returns the [[SourcePosition]] at the current cursor position (might be out of bounds). */
  private def sourcePositionAtCurrent()(implicit s: State): SourcePosition =
    SourcePosition.mkFromZeroIndexed(s.sc.getLine, s.sc.getColumn)

  /** Returns a single-width [[SourceLocation]] starting at the current position. */
  private def sourceLocationAtCurrent()(implicit s: State): SourceLocation =
    SourceLocation.point(isReal = true, s.src, sourcePositionAtCurrent())

  /** Returns a [[SourceLocation]] spanning the current consumed input since the last token, exclusive of the current position. */
  private def sourceLocationFromStart()(implicit s: State): SourceLocation = {
    val (b, e) = getRangeFromStart()
    mkSourceLocation(b, e)
  }

  /** Returns the position of [[State.startPos]] and the exclusive endpoint of the current position. */
  private def getRangeFromStart()(implicit s: State): (SourcePosition, SourcePosition) = {
    val b = sourcePositionAtStart()
    val e = if (s.startOffset != s.sc.getIndex) exclusiveSourcePositionAtCurrent() else sourcePositionAtCurrent()
    (b, e)
  }

  /** Returns the [[SourcePosition]] at the current cursor position as an exclusive endpoint. */
  private def exclusiveSourcePositionAtCurrent()(implicit s: State): SourcePosition = {
    // If we are currently at the start of a line, create a non-existent position and the
    // end of the previous line as the exclusive end position.
    // This should not happen for zero-width tokens at the start of lines.
    val (endLine, endColumn) = s.sc.getExclusiveEndPosition
    SourcePosition.mkFromZeroIndexed(endLine, endColumn)
  }

  /**
    * Returns a fuzzed array of tokens based on the given array of `tokens`.
    *
    * Must not modify the last token since it is end-of-file.
    */
  @unused
  private def fuzz(tokens: Array[Token])(implicit flix: Flix): Array[Token] = {
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

}

