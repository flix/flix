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
import ca.uwaterloo.flix.language.ast.{Ast, TokenErrorKind, ReadAst, SourceKind, SourceLocation, Token, TokenKind}
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._
import scala.collection.mutable

// TODO: Format strings

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
  private val MAX_BLOCK_COMMENT_NESTING_LEVEL = 32

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

      // Construct a map from each source to its tokens.
      mapN(sequence(results))(_.toMap)
    }
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
        scanToken()
      }
    }

    // Add a virtual eof token at the last position.
    s.tokens += Token(TokenKind.Eof, "<eof>", s.current.line, s.current.column)

    if (src.name == "foo.flix") {
      println(s.tokens.mkString("\n"))
    }

    val errorTokens = s.tokens.collect {
      case Token(TokenKind.Err(err), text, line, col) => tokenErrToCompilationMessage(err, text, line, col)
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
   * Retreats current position one char backwards returning the char it was previously sitting on,
   * while keeping track of line and column numbers too.
   */
  private def retreat()(implicit s: State): Char = {
    val c = s.src.data(s.current.offset)
    s.current.offset -= 1
    if (c == '\n') {
      s.current.line -= 1
      s.current.column = 0
    } else {
      s.current.column -= 1
    }
    c
  }

  /**
   * Peeks the character that state is currently sitting on without advancing.
   */
  private def peek()(implicit s: State): Char = {
    s.src.data(s.current.offset)
  }

  /**
   * Peeks the character after the one that state is sitting on if available.
   */
  private def peekPeek()(implicit s: State): Option[Char] = {
    if (s.current.offset >= s.src.data.length) {
      None
    } else {
      Some(s.src.data(s.current.offset + 1))
    }
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
    val text = s.src.data.slice(s.start.offset, s.current.offset).mkString("")
    s.tokens += Token(kind, text, s.start.line, s.start.column)
    s.start = new Position(s.current.line, s.current.column, s.current.offset)
  }

  /**
   * Scans the source for the next available token.
   * This is the heart of the lexer implementation.
   * `scanToken` determines what TokenKind the next
   * available token should be by looking at the coming character in source.
   * This requires potentially infinite look-ahead for things like block-comments and formatted strings,
   * but in many cases one or two characters is enough.
   * Finally `addToken` is called to actually consume the token from source.
   */
  private def scanToken()(implicit s: State): Unit = {
    val c = advance()

    // Beware that the order of these match cases affect both behaviour and performance.
    // If the order needs to change, make sure to run tests and benchmarks.
    val kind = c match {
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
      case '\\' => TokenKind.Backslash
      case '#' => if (peek() == '#') {
        javaName()
      } else {
        TokenKind.Hash
      }
      case '/' => if (peek() == '/') {
        lineComment()
      } else if (peek() == '*') {
        blockComment()
      } else {
        TokenKind.Slash
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
        annotation()
      } else {
        TokenKind.At
      }
      case _ if keyword("???") => TokenKind.HoleAnonymous
      case '?' if peek().isLetter => namedHole()
      case _ if keyword("**") => TokenKind.StarStar
      case _ if keyword("<-") => TokenKind.BackArrow
      case _ if keyword("=>") => TokenKind.Arrow
      case _ if keyword("<=") => TokenKind.AngleLEqual
      case _ if keyword(">=") => TokenKind.AngleREqual
      case _ if keyword("==") => TokenKind.EqualEqual
      case _ if keyword("&&&") => TokenKind.TripleAmpersand
      case _ if keyword("<<<") => TokenKind.TripleAngleL
      case _ if keyword(">>>") => TokenKind.TripleAngleR
      case _ if keyword("^^^") => TokenKind.TripleCaret
      case _ if keyword("|||") => TokenKind.TripleBar
      case _ if keyword("~~~") => TokenKind.TripleTilde
      case _ if keyword("<+>") => TokenKind.AngledPlus
      case _ if keyword("<=>") => TokenKind.AngledEqual
      case _ if keyword("absent") => TokenKind.KeywordAbsent
      case _ if keyword("alias") => TokenKind.KeywordAlias
      case _ if keyword("and") => TokenKind.KeywordAnd
      case _ if keyword("as") => TokenKind.KeywordAs
      case _ if keyword("case") => TokenKind.KeywordCase
      case _ if keyword("catch") => TokenKind.KeywordCatch
      case _ if keyword("checked_cast") => TokenKind.KeywordCheckedCast
      case _ if keyword("checked_ecast") => TokenKind.KeywordCheckedECast
      case _ if keyword("choose") => TokenKind.KeywordChoose
      case _ if keyword("class") => TokenKind.KeywordClass
      case _ if keyword("debug") => TokenKind.KeywordDebug
      case _ if keyword("def") => TokenKind.KeywordDef
      case _ if keyword("deref") => TokenKind.KeywordDeref
      case _ if keyword("discard") => TokenKind.KeywordDiscard
      case _ if keyword("do") => TokenKind.KeywordDo
      case _ if keyword("eff") => TokenKind.KeywordEff
      case _ if keyword("else") => TokenKind.KeywordElse
      case _ if keyword("enum") => TokenKind.KeywordEnum
      case _ if keyword("false") => TokenKind.KeywordFalse
      case _ if keyword("fix") => TokenKind.KeywordFix
      case _ if keyword("for") => TokenKind.KeywordFor
      case _ if keyword("forA") => TokenKind.KeywordForA
      case _ if keyword("forall") => TokenKind.KeywordForall
      case _ if keyword("force") => TokenKind.KeywordForce
      case _ if keyword("foreach") => TokenKind.KeywordForeach
      case _ if keyword("forM") => TokenKind.KeywordForM
      case _ if keyword("from") => TokenKind.KeywordFrom
      case _ if keyword("get") => TokenKind.KeywordGet
      case _ if keyword("if") => TokenKind.KeywordIf
      case _ if keyword("import") => TokenKind.KeywordImport
      case _ if keyword("impure") => TokenKind.KeywordImpure
      case _ if keyword("inject") => TokenKind.KeywordInject
      case _ if keyword("inline") => TokenKind.KeywordInline
      case _ if keyword("instance") => TokenKind.KeywordInstance
      case _ if keyword("into") => TokenKind.KeywordInto
      case _ if keyword("law") => TokenKind.KeywordLaw
      case _ if keyword("lawful") => TokenKind.KeywordLawful
      case _ if keyword("lazy") => TokenKind.KeywordLazy
      case _ if keyword("let") => TokenKind.KeywordLet
      case _ if keyword("masked_cast") => TokenKind.KeywordMaskedCast
      case _ if keyword("match") => TokenKind.KeywordMatch
      case _ if keyword("mod") => TokenKind.KeywordMod
      case _ if keyword("new") => TokenKind.KeywordNew
      case _ if keyword("not") => TokenKind.KeywordNot
      case _ if keyword("null") => TokenKind.KeywordNull
      case _ if keyword("open") => TokenKind.KeywordOpen
      case _ if keyword("open_as") => TokenKind.KeywordOpenAs
      case _ if keyword("or") => TokenKind.KeywordOr
      case _ if keyword("override") => TokenKind.KeywordOverride
      case _ if keyword("par") => TokenKind.KeywordPar
      case _ if keyword("present") => TokenKind.KeywordPresent
      case _ if keyword("project") => TokenKind.KeywordProject
      case _ if keyword("pub") => TokenKind.KeywordPub
      case _ if keyword("pure") => TokenKind.KeywordPure
      case _ if keyword("query") => TokenKind.KeywordQuery
      case _ if keyword("ref") => TokenKind.KeywordRef
      case _ if keyword("region") => TokenKind.KeywordRegion
      case _ if keyword("relational_choose") => TokenKind.KeywordRelationalChoose
      case _ if keyword("restrictable") => TokenKind.KeywordRestrictable
      case _ if keyword("resume") => TokenKind.KeywordResume
      case _ if keyword("sealed") => TokenKind.KeywordSealed
      case _ if keyword("select") => TokenKind.KeywordSelect
      case _ if keyword("solve") => TokenKind.KeywordSolve
      case _ if keyword("spawn") => TokenKind.KeywordSpawn
      case _ if keyword("static") => TokenKind.KeywordStatic
      case _ if keyword("true") => TokenKind.KeywordTrue
      case _ if keyword("try") => TokenKind.KeywordTry
      case _ if keyword("type") => TokenKind.KeywordType
      case _ if keyword("typematch") => TokenKind.KeywordTypeMatch
      case _ if keyword("unchecked_cast") => TokenKind.KeywordUncheckedCast
      case _ if keyword("use") => TokenKind.KeywordUse
      case _ if keyword("where") => TokenKind.KeywordWhere
      case _ if keyword("with") => TokenKind.KeywordWith
      case _ if keyword("without") => TokenKind.KeywordWithout
      case _ if keyword("yield") => TokenKind.KeywordYield
      case _ if keyword("Set#") => TokenKind.SetHash
      case _ if keyword("Array#") => TokenKind.ArrayHash
      case _ if keyword("Map#") => TokenKind.MapHash
      case _ if keyword("List#") => TokenKind.ListHash
      case _ if keyword("Vector#") => TokenKind.VectorHash
      case _ if isMathNameChar(c) => mathName()
      case _ if isGreekNameChar(c) => greekName()
      // User defined operators.
      case _ if validUserOpTokens.contains(c) =>
        val p = peek()
        if (validUserOpTokens.contains(p)) {
          userDefinedOp()
        } else if (c == '-' && p.isDigit) {
          number() // negative numbers.
        } else {
          validUserOpTokens.apply(c)
        }
      case c if c.isLetter => name(c.isUpper)
      case c if c.isDigit => number()
      case '$' => builtIn()
      case '\"' => string()
      case '\'' => char()
      case '`' => infixFunction()
      case _ => TokenKind.Err(TokenErrorKind.UnexpectedChar)
    }

    addToken(kind)
  }

  /**
   * Checks whether the following substring matches a keyword. Note that __comparison includes current__.
   */
  private def keyword(k: String)(implicit s: State): Boolean = {
    // check if the keyword can appear before eof
    if (s.current.offset + k.length > s.src.data.length) {
      return false
    }

    val start = s.current.offset - 1
    var matches = true
    var offset = 0
    while (matches && offset < k.length) {
      if (s.src.data(start + offset) != k(offset)) {
        matches = false
      } else {
        offset += 1
      }
    }
    if (matches) {
      for (_ <- 1 until k.length) {
        advance()
      }
    }

    matches
  }

  /**
   * Moves current position past a built-in function, IE. "$BUILT_IN$".
   */
  private def builtIn()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '$') {
        advance()
        return TokenKind.BuiltIn
      }
      advance()
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
  private def name(isUpper: Boolean)(implicit s: State): TokenKind = {
    var kind = if (isUpper) {
      TokenKind.NameUpperCase
    } else {
      TokenKind.NameLowerCase
    }
    while (!isAtEnd()) {
      val p = peek()
      if (p == '$' && peekPeek().exists(_.isLetter)) {
        // Java names can have "$" in them. IE. "Map$Entry"
        kind = TokenKind.NameJava
      } else if (p == '?') {
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
  private def javaName()(implicit s: State): TokenKind = {
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
  private def greekName()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!isGreekNameChar(peek())) {
        return TokenKind.NameGreek
      }
      advance()
    }
    TokenKind.NameGreek
  }

  /**
   * Checks whether `c` lies in unicode range U+0370 to U+03FF
   */
  private def isGreekNameChar(c: Char): Boolean = {
    val i = c.toInt
    i >= 880 && i <= 1023
  }

  /**
   * Moves current position past a math name.
   * Math names must lie in the unicode range U+2190 to U+22FF
   * IE. "⊆"
   */
  private def mathName()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!isMathNameChar(peek())) {
        return TokenKind.NameMath
      }
      advance()
    }
    TokenKind.NameMath
  }

  /**
   * Checks whether `c` lies in unicode range U+2190 to U+22FF
   */
  private def isMathNameChar(c: Char): Boolean = {
    val i = c.toInt
    i >= 8592 && i <= 8959
  }

  /**
   * Moves current position past a named hole. IE. "?foo".
   */
  private def namedHole()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!peek().isLetter) {
        return TokenKind.HoleNamed
      }
      advance()
    }
    TokenKind.HoleNamed
  }


  /**
   * Moves current position past an infix function.
   */
  private def infixFunction()(implicit s: State): TokenKind = {
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
  private def userDefinedOp()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!validUserOpTokens.contains(peek())) {
        return TokenKind.UserDefinedOperator
      } else {
        advance()
      }
    }
    TokenKind.UserDefinedOperator
  }

  /**
   * The characters allowed in a user defined operator mapped to their `TokenKind`s
   */
  private val validUserOpTokens = Map(
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
   * Moves current position past a string literal.
   * If the string is unterminated a `TokenKind.Err` is returned.
   */
  private def string()(implicit s: State): TokenKind = {
    var prev = ' '
    while (!isAtEnd()) {
      val p = peek()
      // Check for termination while handling escaped '\"'
      if (p == '\"' && prev != '\\') {
        advance()
        return TokenKind.LiteralString
      }
      prev = advance()
    }

    TokenKind.Err(TokenErrorKind.UnterminatedString)
  }

  /**
   * Moves current position past a char literal.
   * If the char is unterminated a `TokenKind.Err` is returned
   */
  private def char()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '\'') {
        advance()
        return TokenKind.LiteralChar
      }
      advance()
    }

    TokenKind.Err(TokenErrorKind.UnterminatedChar)
  }

  /**
   * Moves current position past a number literal. IE. "123i32" or "456.78f32"
   * It is optional to have a trailing type indicator on number literals.
   * If it is missing Flix defaults to `f64` for decimals and `i32` for integers.
   */
  private def number()(implicit s: State): TokenKind = {
    var isDecimal = false
    while (!isAtEnd()) {
      peek() match {
        // Digits and _ are just consumed
        case c if c.isDigit || c == '_' => advance()

        // Dots mark a decimal but are otherwise ignored
        case '.' =>
          if (isDecimal) {
            return TokenKind.Err(TokenErrorKind.DoubleDottedNumber)
          }
          isDecimal = true
          advance()

        // If this is reached an explicit number type might occur next
        case _ => return advance() match {
          case _ if keyword("f32") => TokenKind.LiteralFloat32
          case _ if keyword("f64") => TokenKind.LiteralFloat64
          case _ if keyword("i8") => TokenKind.LiteralInt8
          case _ if keyword("i16") => TokenKind.LiteralInt16
          case _ if keyword("i32") => TokenKind.LiteralInt32
          case _ if keyword("i64") => TokenKind.LiteralInt64
          case _ if keyword("ii") => TokenKind.LiteralBigInt
          case _ if keyword("ff") => TokenKind.LiteralBigDecimal
          case _ =>
            retreat()
            if (isDecimal) {
              TokenKind.LiteralFloat64
            } else {
              TokenKind.LiteralInt32
            }
        }
      }
    }
    TokenKind.Err(TokenErrorKind.UnexpectedChar)
  }

  /**
   * Moves current position past an annotation. IE. "@Test".
   */
  private def annotation()(implicit s: State): TokenKind = {
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
  private def lineComment()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '\n') {
        return TokenKind.CommentLine
      } else {
        advance()
      }
    }
    TokenKind.CommentLine
  }

  /**
   * Moves current position past a block-comment.
   * Note that block-comments can be nested, in which case we need to handle multiple terminating "* /".
   * This is done be counting the nesting level and enforcing a max nesting level.
   * If this level is reached a `TokenKind.Err` is returned.
   * A block-comment might also be unterminated if there is less terminations than levels of nesting.
   * In this case a `TokenKind.Err` is returned as well.
   */
  private def blockComment()(implicit s: State): TokenKind = {
    var level = 1
    while (!isAtEnd()) {
      (peek(), peekPeek()) match {
        case ('/', Some('*')) =>
          level += 1
          if (level >= MAX_BLOCK_COMMENT_NESTING_LEVEL) {
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
  private def tokenErrToCompilationMessage(e: TokenErrorKind, t: String, l: Int, c: Int)(implicit s: State): CompilationMessage = {
    val o = e match {
      case TokenErrorKind.UnexpectedChar | TokenErrorKind.DoubleDottedNumber => t.length
      case TokenErrorKind.BlockCommentTooDeep => 2
      case _ => 1
    }

    val loc = SourceLocation(None, s.src, SourceKind.Real, l, c, l, c + o)
    e match {
      case TokenErrorKind.BlockCommentTooDeep => LexerError.BlockCommentTooDeep(loc)
      case TokenErrorKind.DoubleDottedNumber => LexerError.DoubleDottedNumber(loc)
      case TokenErrorKind.UnexpectedChar => LexerError.UnexpectedChar(t, loc)
      case TokenErrorKind.UnterminatedBlockComment => LexerError.UnterminatedBlockComment(loc)
      case TokenErrorKind.UnterminatedBuiltIn => LexerError.UnterminatedBuiltIn(loc)
      case TokenErrorKind.UnterminatedChar => LexerError.UnterminatedChar(loc)
      case TokenErrorKind.UnterminatedInfixFunction => LexerError.UnterminatedInfixFunction(loc)
      case TokenErrorKind.UnterminatedString => LexerError.UnterminatedString(loc)
    }
  }
}
