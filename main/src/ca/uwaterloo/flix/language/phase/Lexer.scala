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

  /** The characters allowed in a user defined operator mapped to their `TokenKind`s. */
  private def isUserOp(c: Char): Option[TokenKind] = {
    c match {
      case '+' => Some(TokenKind.Plus)
      case '-' => Some(TokenKind.Minus)
      case '*' => Some(TokenKind.Star)
      case '<' => Some(TokenKind.AngleL)
      case '>' => Some(TokenKind.AngleR)
      case '=' => Some(TokenKind.Equal)
      case '!' => Some(TokenKind.Bang)
      case '&' => Some(TokenKind.Ampersand)
      case '|' => Some(TokenKind.Bar)
      case '^' => Some(TokenKind.Caret)
      case '$' => Some(TokenKind.Dollar)
      case _ => None
    }
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

  /** Peeks the previous character that state was on if available. */
  private def previous()(implicit s: State): Option[Char] =
    s.sc.previous

  /** Peeks the character before the previous that state was on if available. */
  private def previousPrevious()(implicit s: State): Option[Char] =
    s.sc.nth(-2)

  /** Peeks the character that is `n` characters before the current if available. */
  private def previousN(n: Int)(implicit s: State): Option[Char] =
    s.sc.nth(-(n + 1))

  /** Peeks the character after the one that state is sitting on if available. */
  private def peekPeek()(implicit s: State): Option[Char] =
    s.sc.nth(1)

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
    val sp1 = SourcePosition.mkFromZeroIndexed(src, line, column)
    // It is safe not consider line breaks because `column + 1` is an exclusive index.
    val sp2 = SourcePosition.mkFromZeroIndexed(src, line, column + 1)
    SourceLocation(isReal = true, sp1, sp2)
  }

  /**
    * Consumes the text between `s.start` and `s.offset` to produce a token.
    * Afterwards `s.start` is reset to the next position after the previous token.
    */
  private def addToken(kind: TokenKind)(implicit s: State): Unit = {
    val b = SourcePosition.mkFromZeroIndexed(s.src, s.start.line, s.start.column)
    // If we are currently at the start of a line, create a non-existent position and the
    // end of the previous line as the exclusive end position.
    // This should not happen for zero-width tokens at the start of lines.
    val (endLine, endColumn) = if (s.start.offset != s.sc.getOffset) s.sc.getExclusiveEndPosition else s.sc.getPosition
    val e = SourcePosition.mkFromZeroIndexed(s.src, endLine, endColumn)
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
    s.sc.peekAndAdvance() match {
      case '(' => TokenKind.ParenL
      case ')' => TokenKind.ParenR
      case '{' => TokenKind.CurlyL
      case '}' => TokenKind.CurlyR
      case '[' => TokenKind.BracketL
      case ']' => TokenKind.BracketR
      case ';' => TokenKind.Semi
      case ',' => TokenKind.Comma
      case '\\' => TokenKind.Backslash
      case _ if isMatchPrev(".{") => TokenKind.DotCurlyL
      case '.' =>
        if (s.sc.advanceIfMatch("..")) {
          TokenKind.DotDotDot
        } else if (previousPrevious().exists(_.isWhitespace)) {
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
      case '$' if s.sc.peekIs(_.isUpper) => acceptBuiltIn()
      case '$' if s.sc.peekIs(_.isLower) =>
        // Don't include the $ sign in the name.
        s.resetStart()
        acceptName(isUpper = false)
      case '\"' => acceptString()
      case '\'' => acceptChar()
      case '`' => acceptInfixFunction()
      case _ if isMatchPrev("#|") => TokenKind.HashBar
      case _ if isMatchPrev("|#") => TokenKind.BarHash
      case _ if isMatchPrev("#{") => TokenKind.HashCurlyL
      case _ if isMatchPrev("#(") => TokenKind.HashParenL
      case '#' => TokenKind.Hash
      case _ if isMatchPrev("//") => acceptLineOrDocComment()
      case _ if isMatchPrev("/*") => acceptBlockComment()
      case '/' => TokenKind.Slash
      case '@' if s.sc.peekIs(_.isLetter) => acceptAnnotation()
      case '@' => TokenKind.At
      case _ if isMatchPrev("???") => TokenKind.HoleAnonymous
      case '?' if s.sc.peekIs(_.isLetter) => acceptNamedHole()
      case _ if isOperator(":::") => TokenKind.TripleColon
      case _ if isOperator("::") => TokenKind.ColonColon
      case _ if isOperator(":=") => TokenKind.ColonEqual
      case _ if isOperator(":-") => TokenKind.ColonMinus
      case _ if isOperator(":") => TokenKind.Colon
      case _ if isOperator("**") => TokenKind.StarStar
      case _ if isOperator("<-") => TokenKind.ArrowThinL
      case _ if isOperator("->") =>
        // If any whitespace exists around the `->`, it is `ArrowThinR`. Otherwise it is `StructArrow`.
        // Examples:
        // a->b:   StructArrow
        // a ->b:  ArrowThinR
        // a-> b:  ArrowThinR
        // a -> b: ArrowThinR
        if (previousN(2).exists(_.isWhitespace) || s.sc.peekIs(_.isWhitespace)) {
          TokenKind.ArrowThinR
        } else {
          TokenKind.StructArrow
        }
      case _ if isOperator("=>") => TokenKind.ArrowThickR
      case _ if isOperator("<=") => TokenKind.AngleLEqual
      case _ if isOperator(">=") => TokenKind.AngleREqual
      case _ if isOperator("==") => TokenKind.EqualEqual
      case _ if isOperator("!=") => TokenKind.BangEqual
      case _ if isOperator("<+>") => TokenKind.AngledPlus
      case _ if isOperator("&&&") => TokenKind.TripleAmpersand
      case _ if isOperator("<<<") => TokenKind.TripleAngleL
      case _ if isOperator(">>>") => TokenKind.TripleAngleR
      case _ if isOperator("^^^") => TokenKind.TripleCaret
      case _ if isOperator("|||") => TokenKind.TripleBar
      case _ if isOperator("~~~") => TokenKind.TripleTilde
      case '~' => TokenKind.Tilde
      case _ if isKeyword("alias") => TokenKind.KeywordAlias
      case _ if isKeyword("and") => TokenKind.KeywordAnd
      case _ if isKeyword("as") => TokenKind.KeywordAs
      case _ if isKeyword("case") => TokenKind.KeywordCase
      case _ if isKeyword("catch") => TokenKind.KeywordCatch
      case _ if isKeyword("checked_cast") => TokenKind.KeywordCheckedCast
      case _ if isKeyword("checked_ecast") => TokenKind.KeywordCheckedECast
      case _ if isKeyword("choose*") => TokenKind.KeywordChooseStar
      case _ if isKeyword("choose") => TokenKind.KeywordChoose
      case _ if isKeyword("def") => TokenKind.KeywordDef
      case _ if isKeyword("discard") => TokenKind.KeywordDiscard
      case _ if isKeyword("eff") => TokenKind.KeywordEff
      case _ if isKeyword("else") => TokenKind.KeywordElse
      case _ if isKeyword("ematch") => TokenKind.KeywordEMatch
      case _ if isKeyword("enum") => TokenKind.KeywordEnum
      case _ if isKeyword("false") => TokenKind.KeywordFalse
      case _ if isKeyword("fix") => TokenKind.KeywordFix
      case _ if isKeyword("forall") => TokenKind.KeywordForall
      case _ if isKeyword("forA") => TokenKind.KeywordForA
      case _ if isKeyword("force") => TokenKind.KeywordForce
      case _ if isKeyword("foreach") => TokenKind.KeywordForeach
      case _ if isKeyword("forM") => TokenKind.KeywordForM
      case _ if isKeyword("from") => TokenKind.KeywordFrom
      case _ if isKeyword("handler") => TokenKind.KeywordHandler
      case _ if isKeyword("if") => TokenKind.KeywordIf
      case _ if isKeyword("import") => TokenKind.KeywordImport
      case _ if isKeyword("inject") => TokenKind.KeywordInject
      case _ if isKeyword("instanceof") => TokenKind.KeywordInstanceOf
      case _ if isKeyword("instance") => TokenKind.KeywordInstance
      case _ if isKeyword("into") => TokenKind.KeywordInto
      case _ if isKeyword("lawful") => TokenKind.KeywordLawful
      case _ if isKeyword("law") => TokenKind.KeywordLaw
      case _ if isKeyword("lazy") => TokenKind.KeywordLazy
      case _ if isKeyword("let") => TokenKind.KeywordLet
      case _ if isKeyword("match") => TokenKind.KeywordMatch
      case _ if isKeyword("mod") => TokenKind.KeywordMod
      case _ if isKeyword("mut") => TokenKind.KeywordMut
      case _ if isKeyword("new") => TokenKind.KeywordNew
      case _ if isKeyword("not") => TokenKind.KeywordNot
      case _ if isKeyword("null") => TokenKind.KeywordNull
      case _ if isKeyword("open_variant") => TokenKind.KeywordOpenVariant
      case _ if isKeyword("open_variant_as") => TokenKind.KeywordOpenVariantAs
      case _ if isKeyword("or") => TokenKind.KeywordOr
      case _ if isKeyword("override") => TokenKind.KeywordOverride
      case _ if isKeyword("par") => TokenKind.KeywordPar
      case _ if isKeyword("pub") => TokenKind.KeywordPub
      case _ if isKeyword("project") => TokenKind.KeywordProject
      case _ if isKeyword("pquery") => TokenKind.KeywordPQuery
      case _ if isKeyword("psolve") => TokenKind.KeywordPSolve
      case _ if isKeyword("query") => TokenKind.KeywordQuery
      case _ if isKeyword("redef") => TokenKind.KeywordRedef
      case _ if isKeyword("region") => TokenKind.KeywordRegion
      case _ if isKeyword("restrictable") => TokenKind.KeywordRestrictable
      case _ if isKeyword("run") => TokenKind.KeywordRun
      case _ if isKeyword("rvadd") => TokenKind.KeywordRvadd
      case _ if isKeyword("rvand") => TokenKind.KeywordRvand
      case _ if isKeyword("rvsub") => TokenKind.KeywordRvsub
      case _ if isKeyword("rvnot") => TokenKind.KeywordRvnot
      case _ if isKeyword("sealed") => TokenKind.KeywordSealed
      case _ if isKeyword("select") => TokenKind.KeywordSelect
      case _ if isKeyword("solve") => TokenKind.KeywordSolve
      case _ if isKeyword("spawn") => TokenKind.KeywordSpawn
      case _ if isKeyword("static") => TokenKind.KeywordStatic
      case _ if isKeyword("Static") => TokenKind.KeywordStaticUppercase
      case _ if isKeyword("struct") => TokenKind.KeywordStruct
      case _ if isKeyword("throw") => TokenKind.KeywordThrow
      case _ if isKeyword("trait") => TokenKind.KeywordTrait
      case _ if isKeyword("true") => TokenKind.KeywordTrue
      case _ if isKeyword("try") => TokenKind.KeywordTry
      case _ if isKeyword("type") => TokenKind.KeywordType
      case _ if isKeyword("typematch") => TokenKind.KeywordTypeMatch
      case _ if isKeyword("unchecked_cast") => TokenKind.KeywordUncheckedCast
      case _ if isKeyword("Univ") => TokenKind.KeywordUniv
      case _ if isKeyword("unsafe") => TokenKind.KeywordUnsafe
      case _ if isKeyword("unsafely") => TokenKind.KeywordUnsafely
      case _ if isKeyword("use") => TokenKind.KeywordUse
      case _ if isKeyword("where") => TokenKind.KeywordWhere
      case _ if isKeyword("with") => TokenKind.KeywordWith
      case _ if isKeyword("without") => TokenKind.KeywordWithout
      case _ if isKeyword("yield") => TokenKind.KeywordYield
      case _ if isKeyword("xor") => TokenKind.KeywordXor
      case _ if isKeyword("xvar") => TokenKind.KeywordXvar
      case _ if isKeyword("Set#") => TokenKind.SetHash
      case _ if isKeyword("Array#") => TokenKind.ArrayHash
      case _ if isKeyword("Map#") => TokenKind.MapHash
      case _ if isKeyword("List#") => TokenKind.ListHash
      case _ if isKeyword("Vector#") => TokenKind.VectorHash
      case 'd' if s.sc.peekIs(_ == '"') => TokenKind.DebugInterpolator
      case _ if isMatchPrev("regex\"") => acceptRegex()
      case c if isMathNameChar(c) => acceptMathName()
      case c if isGreekNameChar(c) => acceptGreekName()
      case '_' =>
        if (!eof()) {
          val p = s.sc.peek
          if (p.isLetterOrDigit) {
            acceptName(p.isUpper)
          } else if (isMathNameChar(p)) {
            s.sc.advance()
            acceptMathName()
          } else if (isUserOp(p).isDefined) {
            s.sc.advance()
            acceptUserDefinedOp()
          } else TokenKind.Underscore
        } else TokenKind.Underscore
      case c if c.isLetter => acceptName(c.isUpper)
      case '0' if s.sc.peekIs(_ == 'x') => acceptHexNumber()
      case c if c.isDigit => acceptNumber()
      // User defined operators.
      case '<' if s.sc.peekIs(_ == '>') && peekPeek().flatMap(isUserOp).isEmpty =>
        // Make sure '<>' is read as AngleL, AngleR and not UserDefinedOperator for empty case sets.
        TokenKind.AngleL
      case c if isUserOp(c).isDefined =>
        if (!eof()) {
          val p = s.sc.peek
          if (isUserOp(p).isDefined) {
            acceptUserDefinedOp()
          } else {
            isUserOp(c).get
          }
        } else isUserOp(c).get
      case c => TokenKind.Err(LexerError.UnexpectedChar(c.toString, sourceLocationAtStart()))
    }
  }

  /**
    * Check that the potential keyword is sufficiently separated.
    * A keyword is separated if it is followed by anything __but__ a character, digit, or underscore.
    * Note that __comparison includes current__.
    */
  private def isSeparated(keyword: String)(implicit s: State): Boolean =
    s.sc.nthIsPOrOutOfBounds(keyword.length - 1, c => !(c.isLetter || c.isDigit || c == '_'))

  /**
    * Check that the potential operator is sufficiently separated.
    * An operator is separated if it is followed by anything __but__ another valid user operator
    * character.
    * Note that __comparison includes current__.
    */
  private def isSeparatedOperator(keyword: String)(implicit s: State): Boolean =
    s.sc.nthIsPOrOutOfBounds(keyword.length - 1, c => isUserOp(c).isEmpty)

  /**
    * Checks whether the previous char and the following substring matches a string.
    * Will advance the current position past the string if there is a match.
    */
  private def isMatchPrev(str: String)(implicit s: State): Boolean = {
    // Check if the string can appear before eof.
    if (s.sc.getOffset + str.length - 1 > s.src.data.length) {
      return false
    }

    // Check if the next n characters in source matches those of str one at a time.
    val start = s.sc.getOffset - 1
    var matches = true
    var offset = 0
    while (matches && offset < str.length) {
      if (s.src.data(start + offset) != str(offset)) {
        matches = false
      } else {
        offset += 1
      }
    }

    if (matches) {
      for (_ <- 1 until str.length) {
        s.sc.advance()
      }
    }

    matches
  }

  /**
    * Checks whether the following substring matches a operator.
    * Note that __comparison includes current__.
    * Also note that this will advance the current position past the keyword if there is a match.
    */
  private def isOperator(op: String)(implicit s: State): Boolean =
    isSeparatedOperator(op) && isMatchPrev(op)

  /**
    * Checks whether the following substring matches a keyword.
    * Note that __comparison includes current__.
    * Also note that this will advance the current position past the keyword if there is a match.
    */
  private def isKeyword(keyword: String)(implicit s: State): Boolean =
    isSeparated(keyword) && isMatchPrev(keyword)

  /** Moves current position past a built-in function (e.g. "$BUILT_IN$"). */
  private def acceptBuiltIn()(implicit s: State): TokenKind = {
    var advanced = false
    while (!eof()) {
      val p = s.sc.peek

      if (p == '$') {
        // Check for termination.
        s.sc.advance()
        return TokenKind.BuiltIn
      }

      if (!p.isLetter && !p.isDigit && p != '_') {
        // Do not allow non-letters other than _.
        // This handles things like block comments e.g. `$BUILT_/*IN*/$` is disallowed.
        return TokenKind.Err(LexerError.UnterminatedBuiltIn(sourceLocationAtStart()))
      }

      s.sc.advance()
      advanced = true
    }
    TokenKind.Err(LexerError.UnterminatedBuiltIn(sourceLocationAtStart()))
  }

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
    s.sc.advanceWhile(c => c.isLetter || c.isDigit || c == '_' || c == '!' || c == '$')
    if (s.sc.advanceIfMatch('?')) {
      TokenKind.HoleVariable
    } else if (isUpper) {
      TokenKind.NameUpperCase
    } else {
      TokenKind.NameLowerCase
    }
  }

  /**
    * Moves current position past a greek name.
    * Greek names must lie in the unicode range U+0370 to U+03FF (e.g. "Χαίρετε").
    */
  private def acceptGreekName()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(isGreekNameChar)
    TokenKind.NameGreek
  }

  /** Checks whether `c` lies in unicode range U+0370 to U+03FF. */
  private def isGreekNameChar(c: Char): Boolean =
    0x0370 <= c && c <= 0x03FF

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
    s.sc.advanceWhile(c => c.isLetter || c.isDigit)
    TokenKind.HoleNamed
  }


  /** Moves current position past an infix function. */
  private def acceptInfixFunction()(implicit s: State): TokenKind = {
    s.sc.advanceWhile(
      c => c == '.' || c == '!' || c.isLetter || c.isDigit || isMathNameChar(c) || isGreekNameChar(c)
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
    s.sc.advanceWhile(c => isUserOp(c).isDefined)
    TokenKind.UserDefinedOperator
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
      val prev = previous()
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

      if (prev == '/' && s.sc.peekIs(_ == '*')) {
        // This handles block comment within a char.
        return TokenKind.Err(LexerError.UnterminatedChar(sourceLocationAtStart()))
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
    s.sc.advanceWhile(_.isLetter)
    TokenKind.Annotation
  }

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

