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
import ca.uwaterloo.flix.language.ast.{Ast, LexerErr, ReadAst, Token, TokenKind}
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._
import scala.collection.mutable

/**
 * A lexer that is able to tokenize multiple `Ast.Source`s in parallel.
 * This lexer is resilient, meaning that when an unrecognized character is encountered,
 * the lexer will simply produce a token of kind `TokenKind.Err` an move on instead of halting.
 * There are some unrecoverable errors though, for example unterminated block-comments or unclosed string literals.
 * In these cases a `TokenKind.Err` will still be produced but it will contain the rest of the source text.
 * See `LexerError` for all error states.
 */
object Lexer {
  /**
   * The maximal allowed nesting level of block-comments.
   */
  val BlockCommentMaxNestingLevel = 32

  /**
   * The maximal allowed nesting level of string interpolation.
   */
  val InterpolatedStringMaxNestingLevel = 32

  /**
   * The characters allowed in a user defined operator mapped to their `TokenKind`s
   */
  private val ValidUserOpTokens = Map(
    '+' -> TokenKind.Plus,
    '-' -> TokenKind.Minus,
    '*' -> TokenKind.Star,
    '<' -> TokenKind.AngleL,
    '>' -> TokenKind.AngleR,
    '=' -> TokenKind.Equal,
    '!' -> TokenKind.Bang,
    '&' -> TokenKind.Ampersand,
    '|' -> TokenKind.Bar,
    '^' -> TokenKind.Caret,
  )

  /**
   * The internal state of the lexer as it tokenizes a single source.
   * At any point execution `start` represents the start of the token currently being considered,
   * while `current` is the current read head of the lexer.
   * Note that both start and current are `Position`s since they are not necessarily on the same line.
   * `current` will always be on the same character as or past `start`.
   * As tokens are produced they are placed in `tokens`.
   */
  private class State(val src: Ast.Source) {
    var start: Position = new Position(0, 0, 0)
    val current: Position = new Position(0, 0, 0)
    val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
    var interpolationNestingLevel: Int = 0
  }

  /**
   * A source position keeping track of both line, column as well as absolute character offset.
   */
  private class Position(var line: Int, var column: Int, var offset: Int)

  /**
   * Run the lexer on multiple `Ast.Source`s in parallel.
   */
  def run(root: ReadAst.Root)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] = {
    flix.phase("Lexer") {
      // Lex each source file in parallel.
      val results = ParOps.parMap(root.sources) {
        case (src, _) => mapN(lex(src))(tokens => src -> tokens)
      }

      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(root.sources, oldTokens)

      // Lex each stale source file in parallel.
      val results = ParOps.parMap(stale.keys)(src => mapN(tryLex(src))(tokens => src -> tokens))

      // Construct a map from each source to its tokens.
      val reused = fresh.map(_.toSuccess[(Ast.Source, Array[Token]), CompilationMessage])
      mapN(sequence(results ++ reused))(_.toMap)
    }

  /**
   * Lexes a single source (file) into an array of tokens.
   */
  private def lex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    implicit val s: State = new State(src)
    while (!isAtEnd()) {
      whitespace()
      if (!isAtEnd()) {
        s.start = new Position(s.current.line, s.current.column, s.current.offset)
        val k = scanToken()
        addToken(k)
      }
    }

    // Add a virtual eof token at the last position.
    addToken(TokenKind.Eof)
    
    val errorTokens = s.tokens.collect {
      case t@Token(TokenKind.Err(err), _, _, _, _, _) => tokenErrToCompilationMessage(err, t)
    }
    if (errorTokens.nonEmpty) {
      Validation.SoftFailure(s.tokens.toArray, LazyList.from(errorTokens))
    } else {
      s.tokens.toArray.toSuccess
    }
  }

  /**
   * Advances current position one char forward, returning the char it was previously sitting on,
   * while keeping track of line and column numbers too.
   */
  private def advance()(implicit s: State): Char = {
    val c = s.src.data(s.current.offset)
    s.current.offset += 1
    if (c == '\n') {
      s.current.line += 1
      s.current.column = 0
    } else {
      s.current.column += 1
    }
    c
  }

  /**
   * Retreats current position one char backwards while keeping track of line and column numbers too.
   */
  private def retreat()(implicit s: State): Unit = {
    if (s.current.offset == 0) {
      return
    }
    s.current.offset -= 1
    val c = s.src.data(s.current.offset)
    if (c == '\n') {
      s.current.line -= 1
      s.current.column = 0
    } else {
      s.current.column -= 1
    }
  }

  /**
   * Peeks the previous character that state was on if available.
   */
  private def previous()(implicit s: State): Option[Char] = {
    if (s.current.offset == 0) {
      None
    } else {
      Some(s.src.data(s.current.offset - 1))
    }
  }

  /**
   * Peeks the character before the previous that state was on if available.
   */
  private def previousPrevious()(implicit s: State): Option[Char] = {
    if (s.current.offset <= 1) {
      None
    } else {
      Some(s.src.data(s.current.offset - 2))
    }
  }

  /**
   * Peeks the character that state is currently sitting on without advancing.
   */
  private def peek()(implicit s: State): Char = {
    s.src.data(s.current.offset)
  }

  // Peeks the character after the one that state is sitting on if available
  private def peekPeek()(implicit s: State): Option[Char] = {
    if (s.current.offset >= s.src.data.length) {
      None
    } else {
      Some(s.src.data(s.current.offset + 1))
    }
  }

  /**
   * A helper function wrapping peek, with special handling for escaped characters.
   * This is useful for "\"" or `'\''`
   */
  private def escapedPeek()(implicit s: State): Option[Char] = {
    var p = peek()
    while (p == '\\') {
      advance()
      // This check is for a source that ends on a '\'.
      if (s.current.offset >= s.src.data.length - 1) {
        return None
      }
      advance()
      p = peek()
    }
    Some(p)
  }

  /**
   * Checks if the current position has passed the end of the source
   */
  private def isAtEnd()(implicit s: State): Boolean = {
    s.current.offset >= s.src.data.length
  }

  /**
   * Consumes the text between `s.start` and `s.offset` to produce a token.
   * Afterwards `s.start` is reset to the next position after the previous token.
   */
  private def addToken(kind: TokenKind)(implicit s: State): Unit = {
    s.tokens += Token(kind, s.src.data, s.start.offset, s.current.offset, s.start.line, s.start.column)
    s.start = new Position(s.current.line, s.current.column, s.current.offset)
  }

  /**
   * Scans the source for the next available token.
   * This is the heart of the lexer implementation.
   * `scanToken` determines what TokenKind the next
   * available token should be by looking at the coming character in source.
   * This requires potentially infinite look-ahead for things like block-comments and formatted strings,
   * but in many cases one or two characters is enough.
   */
  private def scanToken()(implicit s: State): TokenKind = {
    val c = advance()

    // Beware that the order of these match cases affect both behaviour and performance.
    // If the order needs to change, make sure to run tests and benchmarks.
    c match {
      case '(' => TokenKind.ParenL
      case ')' => TokenKind.ParenR
      case '{' => TokenKind.CurlyL
      case '}' => TokenKind.CurlyR
      case '[' => TokenKind.BracketL
      case ']' => TokenKind.BracketR
      case ';' => TokenKind.Semi
      case ',' => TokenKind.Comma
      case '_' => TokenKind.Underscore
      case '.' => TokenKind.Dot
      case '~' => TokenKind.Tilde
      case '\\' => TokenKind.Backslash
      case '$' => if (peek().isUpper) {
        acceptBuiltIn()
      } else {
        TokenKind.Dollar
      }
      case '\"' => acceptString()
      case '\'' => acceptChar()
      case '`' => acceptInfixFunction()
      case '#' => if (peek() == '#') {
        acceptJavaName()
      } else {
        TokenKind.Hash
      }
      case '/' => if (peek() == '/') {
        if (peekPeek().contains('/')) {
          acceptDocComment()
        } else {
          acceptLineComment()
        }
      } else if (peek() == '*') {
        acceptBlockComment()
      } else {
        TokenKind.Slash
      }
      case '.' => if (peek() == '.') {
        advance()
        TokenKind.DotDot
      } else {
        TokenKind.Dot
      }
      case ':' => if (peek() == ':') {
        advance()
        TokenKind.ColonColon
      } else if (peek() == '=') {
        advance()
        TokenKind.ColonEqual
      } else {
        TokenKind.Colon
      }
      case '@' => if (peek().isUpper) {
        acceptAnnotation()
      } else {
        TokenKind.At
      }
      case _ if isKeyword("???") => TokenKind.HoleAnonymous
      case '?' if peek().isLetter => acceptNamedHole()
      case _ if isKeyword("**") => TokenKind.StarStar
      case _ if isKeyword("<-") => TokenKind.BackArrow
      case _ if isKeyword("=>") => TokenKind.Arrow
      case _ if isKeyword("<=") => TokenKind.AngleLEqual
      case _ if isKeyword(">=") => TokenKind.AngleREqual
      case _ if isKeyword("==") => TokenKind.EqualEqual
      case _ if isKeyword("&&&") => TokenKind.TripleAmpersand
      case _ if isKeyword("<<<") => TokenKind.TripleAngleL
      case _ if isKeyword(">>>") => TokenKind.TripleAngleR
      case _ if isKeyword("^^^") => TokenKind.TripleCaret
      case _ if isKeyword("|||") => TokenKind.TripleBar
      case _ if isKeyword("~~~") => TokenKind.TripleTilde
      case _ if isKeyword("<+>") => TokenKind.AngledPlus
      case _ if isKeyword("<=>") => TokenKind.AngledEqual
      case _ if isKeyword("absent") => TokenKind.KeywordAbsent
      case _ if isKeyword("alias") => TokenKind.KeywordAlias
      case _ if isKeyword("and") => TokenKind.KeywordAnd
      case _ if isKeyword("as") => TokenKind.KeywordAs
      case _ if isKeyword("case") => TokenKind.KeywordCase
      case _ if isKeyword("catch") => TokenKind.KeywordCatch
      case _ if isKeyword("checked_cast") => TokenKind.KeywordCheckedCast
      case _ if isKeyword("checked_ecast") => TokenKind.KeywordCheckedECast
      case _ if isKeyword("choose") => TokenKind.KeywordChoose
      case _ if isKeyword("class") => TokenKind.KeywordClass
      case _ if isKeyword("debug") => TokenKind.KeywordDebug
      case _ if isKeyword("def") => TokenKind.KeywordDef
      case _ if isKeyword("deref") => TokenKind.KeywordDeref
      case _ if isKeyword("discard") => TokenKind.KeywordDiscard
      case _ if isKeyword("do") => TokenKind.KeywordDo
      case _ if isKeyword("eff") => TokenKind.KeywordEff
      case _ if isKeyword("else") => TokenKind.KeywordElse
      case _ if isKeyword("enum") => TokenKind.KeywordEnum
      case _ if isKeyword("false") => TokenKind.KeywordFalse
      case _ if isKeyword("fix") => TokenKind.KeywordFix
      case _ if isKeyword("for") => TokenKind.KeywordFor
      case _ if isKeyword("forA") => TokenKind.KeywordForA
      case _ if isKeyword("forall") => TokenKind.KeywordForall
      case _ if isKeyword("force") => TokenKind.KeywordForce
      case _ if isKeyword("foreach") => TokenKind.KeywordForeach
      case _ if isKeyword("forM") => TokenKind.KeywordForM
      case _ if isKeyword("from") => TokenKind.KeywordFrom
      case _ if isKeyword("get") => TokenKind.KeywordGet
      case _ if isKeyword("if") => TokenKind.KeywordIf
      case _ if isKeyword("import") => TokenKind.KeywordImport
      case _ if isKeyword("impure") => TokenKind.KeywordImpure
      case _ if isKeyword("inject") => TokenKind.KeywordInject
      case _ if isKeyword("inline") => TokenKind.KeywordInline
      case _ if isKeyword("instance") => TokenKind.KeywordInstance
      case _ if isKeyword("into") => TokenKind.KeywordInto
      case _ if isKeyword("law") => TokenKind.KeywordLaw
      case _ if isKeyword("lawful") => TokenKind.KeywordLawful
      case _ if isKeyword("lazy") => TokenKind.KeywordLazy
      case _ if isKeyword("let") => TokenKind.KeywordLet
      case _ if isKeyword("masked_cast") => TokenKind.KeywordMaskedCast
      case _ if isKeyword("match") => TokenKind.KeywordMatch
      case _ if isKeyword("mod") => TokenKind.KeywordMod
      case _ if isKeyword("new") => TokenKind.KeywordNew
      case _ if isKeyword("not") => TokenKind.KeywordNot
      case _ if isKeyword("null") => TokenKind.KeywordNull
      case _ if isKeyword("open") => TokenKind.KeywordOpen
      case _ if isKeyword("open_as") => TokenKind.KeywordOpenAs
      case _ if isKeyword("or") => TokenKind.KeywordOr
      case _ if isKeyword("override") => TokenKind.KeywordOverride
      case _ if isKeyword("par") => TokenKind.KeywordPar
      case _ if isKeyword("present") => TokenKind.KeywordPresent
      case _ if isKeyword("project") => TokenKind.KeywordProject
      case _ if isKeyword("pub") => TokenKind.KeywordPub
      case _ if isKeyword("pure") => TokenKind.KeywordPure
      case _ if isKeyword("query") => TokenKind.KeywordQuery
      case _ if isKeyword("ref") => TokenKind.KeywordRef
      case _ if isKeyword("region") => TokenKind.KeywordRegion
      case _ if isKeyword("relational_choose") => TokenKind.KeywordRelationalChoose
      case _ if isKeyword("restrictable") => TokenKind.KeywordRestrictable
      case _ if isKeyword("resume") => TokenKind.KeywordResume
      case _ if isKeyword("sealed") => TokenKind.KeywordSealed
      case _ if isKeyword("select") => TokenKind.KeywordSelect
      case _ if isKeyword("solve") => TokenKind.KeywordSolve
      case _ if isKeyword("spawn") => TokenKind.KeywordSpawn
      case _ if isKeyword("static") => TokenKind.KeywordStatic
      case _ if isKeyword("true") => TokenKind.KeywordTrue
      case _ if isKeyword("try") => TokenKind.KeywordTry
      case _ if isKeyword("type") => TokenKind.KeywordType
      case _ if isKeyword("typematch") => TokenKind.KeywordTypeMatch
      case _ if isKeyword("unchecked_cast") => TokenKind.KeywordUncheckedCast
      case _ if isKeyword("use") => TokenKind.KeywordUse
      case _ if isKeyword("where") => TokenKind.KeywordWhere
      case _ if isKeyword("with") => TokenKind.KeywordWith
      case _ if isKeyword("without") => TokenKind.KeywordWithout
      case _ if isKeyword("yield") => TokenKind.KeywordYield
      case _ if isKeyword("Set#") => TokenKind.SetHash
      case _ if isKeyword("Array#") => TokenKind.ArrayHash
      case _ if isKeyword("Map#") => TokenKind.MapHash
      case _ if isKeyword("List#") => TokenKind.ListHash
      case _ if isKeyword("Vector#") => TokenKind.VectorHash
      case _ if isMathNameChar(c) => acceptMathName()
      case _ if isGreekNameChar(c) => acceptGreekName()
      // User defined operators.
      case _ if ValidUserOpTokens.contains(c) =>
        val p = peek()
        if (ValidUserOpTokens.contains(p)) {
          acceptUserDefinedOp()
        } else if (c == '-' && p.isDigit) {
          acceptNumber() // negative numbers.
        } else {
          ValidUserOpTokens.apply(c)
        }
      case c if c.isLetter => acceptName(c.isUpper)
      case c if c.isDigit => acceptNumber()
      case _ => TokenKind.Err(TokenErrorKind.UnexpectedChar)
    }
  }

  /**
   * Checks whether the following substring matches a keyword. Note that __comparison includes current__.
   */
  private def isMatch(keyword: String)(implicit s: State): Boolean = {
    // Check if the keyword can appear before eof.
    if (s.current.offset + keyword.length - 1 > s.src.data.length) {
      return false
    }

    // Check if the next n characters in source matches those of keyword one at a time.
    val start = s.current.offset - 1
    var matches = true
    var offset = 0
    while (matches && offset < keyword.length) {
      if (s.src.data(start + offset) != keyword(offset)) {
        matches = false
      } else {
        offset += 1
      }
    }
    matches
  }

  /**
   * Checks whether the following substring matches a keyword. Note that __comparison includes current__.
   * Also note that this will advance the current position past the keyword if there is a match
   */
  private def isKeyword(keyword: String)(implicit s: State): Boolean = {
    val matches = isMatch(keyword)
    if (matches) {
      for (_ <- 1 until keyword.length) {
        advance()
      }
    }
    matches
  }

  /**
   * Moves current position past a built-in function, IE. "$BUILT_IN$".
   * Note that $ can be used as a separator in java-names too. IE. "Map$Entry".
   * When encountering a "$" there is no way to discern
   * between a built-in and a java-name without looking ahead.
   * Only a TokenKind.Dollar needs to be emitted in the java name case
   * and then the lexer needs to be retreated to just after "$".
   */
  private def acceptBuiltIn()(implicit s: State): TokenKind = {
    var advances = 0
    while (!isAtEnd()) {
      val p = peek()
      if (p.isLower) {
        // This means that the opening '$' was a separator.
        // we need to rewind the lexer to just after '$'.
        for (_ <- 0 until advances) {
          retreat()
        }
        return TokenKind.Dollar
      }
      if (p == '$') {
        advance()
        return TokenKind.BuiltIn
      }
      advance()
      advances += 1
    }
    TokenKind.Err(TokenErrorKind.UnterminatedBuiltIn)
  }

  /**
   * Moves current position past all whitespace characters.
   */
  private def whitespace()(implicit s: State): Unit = {
    while (!isAtEnd()) {
      if (!peek().isWhitespace) {
        return
      }
      advance()
    }
  }

  /**
   * Moves current position past a name (both upper- and lower-case).
   * There are edge cases of variable holes, IE. "x?", and java names like "Map$Entry",
   * which is the reason this function will return a `TokenKind`.
   */
  private def acceptName(isUpper: Boolean)(implicit s: State): TokenKind = {
    val kind = if (isUpper) {
      TokenKind.NameUpperCase
    } else {
      TokenKind.LowercaseName
    }
    while (!isAtEnd()) {
      val p = peek()
      if (p == '?') {
        advance()
        return TokenKind.HoleVariable
      } else if (!p.isLetter && !p.isDigit && p != '_' && p != '!') {
        return kind
      }
      advance()
    }
    kind
  }


  /**
   * Moves current position past a java name. IE. "##java"
   */
  private def acceptJavaName()(implicit s: State): TokenKind = {
    advance()
    while (!isAtEnd()) {
      val p = peek()
      if (!p.isLetter && !p.isDigit && p != '_' && p != '!') {
        return TokenKind.NameJava
      }
      advance()
    }
    TokenKind.NameJava
  }

  /**
   * Moves current position past a greek name.
   * Greek names must lie in the unicode range U+0370 to U+03FF.
   * IE. "Χαίρετε"
   */
  private def acceptGreekName()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!isGreekNameChar(peek())) {
        return TokenKind.GreekName
      }
      advance()
    }
    TokenKind.GreekName
  }

  /**
   * Checks whether `c` lies in unicode range U+0370 to U+03FF
   */
  private def isGreekNameChar(c: Char): Boolean = {
    val i = c.toInt
    0x0370 <= i && i <= 0x03FF
  }

  /**
   * Moves current position past a math name.
   * Math names must lie in the unicode range U+2190 to U+22FF
   * IE. "⊆"
   */
  private def acceptMathName()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!isMathNameChar(peek())) {
        return TokenKind.MathName
      }
      advance()
    }
    TokenKind.MathName
  }

  // Checks whether c lies in unicode range U+2190 to U+22FF
  private def isMathNameChar(c: Char): Boolean = {
    val i = c.toInt
    0x2190 <= i && i <= 0x22FF
  }

  /**
   * Moves current position past a named hole. IE. "?foo".
   */
  private def acceptNamedHole()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!peek().isLetter) {
        return TokenKind.NamedHole
      }
      advance()
    }
    TokenKind.HoleNamed
  }


  /**
   * Moves current position past an infix function.
   */
  private def acceptInfixFunction()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '`') {
        advance()
        return TokenKind.InfixFunction
      }
      advance()
    }
    TokenKind.Err(TokenErrorKind.UnterminatedInfixFunction)
  }

  /**
   * Moves current position past a user defined operator. IE. "<*>".
   * A user defined operator may be any combination of length 2 or more
   * of the characters in `validUserOpTokens`.
   */
  private def acceptUserDefinedOp()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!ValidUserOpTokens.contains(peek())) {
        return TokenKind.UserDefinedOperator
      } else {
        advance()
      }
    }
    TokenKind.UserDefinedOperator
  }

  /**
   * Moves current position past a string literal.
   * If the string is unterminated a `TokenKind.Err` is returned.
   */
  private def acceptString()(implicit s: State): TokenKind = {
    var kind: TokenKind = TokenKind.LiteralString
    while (!isAtEnd()) {
      var p = escapedPeek()
      // Check for the beginning of an string interpolation.
      if (!previousPrevious().contains('\\') && previous().contains('$') && p.contains('{')) {
        acceptStringInterpolation() match {
          case e@TokenKind.Err(_) => return e
          case k =>
            // Resume regular string literal tokenization by resetting p and prev.
            kind = k
            p = escapedPeek()
        }
      }
      // Check for termination
      if (p.contains('\"')) {
        advance()
        return kind
      }
      // Check if file ended on a '\', meaning that the string was unterminated
      if (p.isEmpty) {
        return TokenKind.Err(TokenErrorKind.UnterminatedString)
      }
      advance()
    }
    TokenKind.Err(TokenErrorKind.UnterminatedString)
  }

  /**
   * Moves current position past an interpolated expression within a string. IE. "Hi ${name}!".
   * Note that this function works a little differently to the other `accept*` functions
   * since it will produce a number of tokens before returning.
   * This is necessary since it must be able to move past any expressions,
   * including nested string literals, which might also include interpolation.
   * This is done by calling `scanToken` and `addToken` manually like in the top-most `lex` function,
   * while looking for the terminating '}'.
   * A max nesting level is enforced via `state.interpolationNestingLevel` to avoid blowing the stack.
   *
   * Some tricky but valid strings include:
   * "Hello ${" // "} world!"
   * "My favorite number is ${ { "${"//"}"40 + 2} }!"
   * "${"${}"}"
   */
  private def acceptStringInterpolation()(implicit s: State): TokenKind = {
    // Handle max nesting level
    s.interpolationNestingLevel += 1
    if (s.interpolationNestingLevel > InterpolatedStringMaxNestingLevel) {
      s.interpolationNestingLevel = 0
      return TokenKind.Err(TokenErrorKind.StringInterpolationTooDeep)
    }

    advance() // Consume '{'.
    addToken(TokenKind.LiteralStringInterpolationL)
    // consume tokens until a terminating '}' is found
    var blockNestingLevel = 0
    while (!isAtEnd()) {
      whitespace()
      if (!isAtEnd()) {
        s.start = new Position(s.current.line, s.current.column, s.current.offset)
        val kind = scanToken()

        // Handle nested block expressions like `"my favorite number is ${ {40 + 2} }!"`.
        if (kind == TokenKind.CurlyL) {
          blockNestingLevel += 1
        }

        // Check for the terminating '}'.
        if (kind == TokenKind.CurlyR) {
          if (blockNestingLevel == 0) {
            s.interpolationNestingLevel -= 1
            return TokenKind.LiteralStringInterpolationR
          }
          blockNestingLevel -= 1
        }
        addToken(kind)
      }
    }
    TokenKind.Err(TokenErrorKind.UnterminatedStringInterpolation)
  }

  /**
   * Moves current position past a char literal.
   * If the char is unterminated a `TokenKind.Err` is returned
   * Note that chars might contain unicode hex codes like these "\u00ff"
   */
  private def acceptChar()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (escapedPeek().contains('\'')) {
        advance()
        return TokenKind.Char
      }
      advance()
    }

    TokenKind.Err(LexerErr.UnterminatedChar)
  }

  /**
   * Moves current position past a number literal. IE. "123i32" or "456.78f32"
   * It is optional to have a trailing type indicator on number literals.
   * If it is missing Flix defaults to `f64` for decimals and `i32` for integers.
   */
  private def acceptNumber()(implicit s: State): TokenKind = {
    var isDecimal = false
    while (!isAtEnd()) {
      peek() match {
        // Digits and _ are just consumed
        case c if c.isDigit || c == '_' => advance()

        // Dots mark a decimal but are otherwise ignored
        case '.' =>
          if (isDecimal) {
            return TokenKind.Err(LexerErr.DoubleDottedNumber)
          }
          isDecimal = true
          advance()

        // If this is reached an explicit number type might occur next
        case _ => return advance() match {
          case _ if isKeyword("f32") => TokenKind.LiteralFloat32
          case _ if isKeyword("f64") => TokenKind.LiteralFloat64
          case _ if isKeyword("i8") => TokenKind.LiteralInt8
          case _ if isKeyword("i16") => TokenKind.LiteralInt16
          case _ if isKeyword("i32") => TokenKind.LiteralInt32
          case _ if isKeyword("i64") => TokenKind.LiteralInt64
          case _ if isKeyword("ii") => TokenKind.LiteralBigInt
          case _ if isKeyword("ff") => TokenKind.LiteralBigDecimal
          case _ =>
            retreat()
            if (isDecimal) {
              TokenKind.Float64
            } else {
              TokenKind.Int32
            }
        }
      }
    }
    TokenKind.Err(TokenErrorKind.UnexpectedChar)
  }

  /**
   * Moves current position past an annotation. IE. "@Test".
   */
  private def acceptAnnotation()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!peek().isLetter) {
        return TokenKind.Annotation
      } else {
        advance()
      }
    }
    TokenKind.Annotation
  }

  /**
   * Moves current position past a line-comment
   */
  private def acceptLineComment()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '\n') {
        return TokenKind.LineComment
      } else {
        advance()
      }
    }
    TokenKind.CommentLine
  }

  private def acceptDocComment()(implicit s: State): TokenKind = {
    while (!eof()) {
      if (peek() == '\n') {
        return TokenKind.CommentDoc
      } else {
        advance()
      }
    }
    TokenKind.CommentDoc
  }

  /**
   * Moves current position past a block-comment.
   * Note that block-comments can be nested, in which case we need to handle multiple terminating "* /".
   * This is done be counting the nesting level and enforcing a max nesting level.
   * If this level is reached a `TokenKind.Err` is returned.
   * A block-comment might also be unterminated if there is less terminations than levels of nesting.
   * In this case a `TokenKind.Err` is returned as well.
   */
  private def acceptBlockComment()(implicit s: State): TokenKind = {
    var level = 1
    while (!isAtEnd()) {
      (peek(), peekPeek()) match {
        case ('/', Some('*')) =>
          level += 1
          if (level >= BlockCommentMaxNestingLevel) {
            return TokenKind.Err(TokenErrorKind.BlockCommentTooDeep)
          }
          advance()
        case ('*', Some('/')) =>
          level -= 1
          advance()
          advance()
          if (level == 0) {
            return TokenKind.CommentBlock
          }
        case _ => advance()
      }
    }
    TokenKind.Err(TokenErrorKind.UnterminatedBlockComment)
  }

  /**
   * Converts a `Token` of kind `TokenKind.Err` into a CompilationMessage.
   * Why is this necessary? We would like the lexer to capture as many errors as possible before terminating.
   * To do this, the lexer will produce error tokens instead of halting,
   * each holding a kind of the simple type `ErrKind`.
   * So we need this mapping to produce a `CompilationMessage`, which is a case class, if there were any errors.
   */
  private def tokenErrToCompilationMessage(e: TokenErrorKind, token: Token)(implicit s: State): CompilationMessage = {
    val t = token.text

    val offset = e match {
      case TokenErrorKind.UnexpectedChar | TokenErrorKind.DoubleDottedNumber => t.length
      case TokenErrorKind.BlockCommentTooDeep => 2
      case _ => 1
    }

    val loc = SourceLocation(None, s.src, SourceKind.Real, token.line, token.col, token.line, token.col + offset)
    e match {
      case TokenErrorKind.BlockCommentTooDeep => LexerError.BlockCommentTooDeep(loc)
      case TokenErrorKind.DoubleDottedNumber => LexerError.DoubleDottedNumber(loc)
      case TokenErrorKind.UnexpectedChar => LexerError.UnexpectedChar(t, loc)
      case TokenErrorKind.UnterminatedBlockComment => LexerError.UnterminatedBlockComment(loc)
      case TokenErrorKind.UnterminatedBuiltIn => LexerError.UnterminatedBuiltIn(loc)
      case TokenErrorKind.UnterminatedChar => LexerError.UnterminatedChar(loc)
      case TokenErrorKind.UnterminatedInfixFunction => LexerError.UnterminatedInfixFunction(loc)
      case TokenErrorKind.UnterminatedString => LexerError.UnterminatedString(loc)
      case TokenErrorKind.UnterminatedStringInterpolation => LexerError.UnterminatedStringInterpolation(loc)
      case TokenErrorKind.StringInterpolationTooDeep => LexerError.StringInterpolationTooDeep(loc)
    }
  }

}
