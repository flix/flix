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

object Lexer {
  /**
   * The maximal allowed nesting level of block-comments.
   */
  private val BlockCommentMaxNestingLevel = 32

  /**
   * The maximal allowed nesting level of string interpolation.
   */
  private val InterpolatedStringMaxNestingLevel = 32

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
    // Compute a `ParserInput` when initializing a state for lexing a source.
    // This is necessary to display source code in error messages.
    // See `sourceLocationAtStart` for usage and `SourceLocation` for more information.
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  /**
   * A source position keeping track of both line, column as well as absolute character offset.
   */
  private class Position(var line: Int, var column: Int, var offset: Int)

  /**
   * Run the lexer on multiple `Ast.Source`s in parallel.
   */
  def run(root: ReadAst.Root, oldTokens: Map[Ast.Source, Array[Token]], changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] = {
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

  private def lex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    implicit val s: State = new State(src)
    while (!isAtEnd()) {
      whitespace()
      if (!isAtEnd()) {
        s.start = new Position(s.current.line, s.current.column, s.current.offset)
        scanToken()
      }
    }

    // Add a virtual eof token at the last position
    s.tokens += Token(TokenKind.Eof, "<eof>", s.current.line, s.current.column)

    val hasErrors = s.tokens.exists(t => t.kind.isInstanceOf[TokenKind.Err])
    if (hasErrors) {
      Validation.SoftFailure(s.tokens.toArray, LazyList.from(s.tokens).collect {
        case Token(TokenKind.Err(e), t, l, c) => tokenErrToCompilationMessage(e, t, l, c)
      })
    } else {
      printf(s.tokens.mkString("\n"))
      s.tokens.toArray.toSuccess
    }
  }

  // Advances state one char forward returning the char it was previously sitting on
  // keeps track of line and column numbers too
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

  // Retreats state one char backwards returning the char it was previously sitting on
  // keeps track of line and column numbers too
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

  // Peeks the character that state is currently sitting on without advancing
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

  // Returns whether state is at the end of its input
  private def isAtEnd()(implicit s: State): Boolean = {
    s.current.offset == s.src.data.length
  }

  // Scans for the next token in input
  private def scanToken()(implicit s: State): Unit = {
    val c = advance()

    val kind = c match {
      case '(' => TokenKind.LParen
      case ')' => TokenKind.RParen
      case '{' => TokenKind.LCurly
      case '}' => TokenKind.RCurly
      case '[' => TokenKind.LBracket
      case ']' => TokenKind.RBracket
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
        annotation()
      } else {
        TokenKind.At
      }
      case _ if keyword("???") => TokenKind.HoleAnonymous
      case '?' if peek().isLetter => namedHole()
      case _ if keyword("**") => TokenKind.StarStar
      case _ if keyword("<-") => TokenKind.BackArrow
      case _ if keyword("=>") => TokenKind.Arrow
      case _ if keyword("<=") => TokenKind.LAngleEqual
      case _ if keyword(">=") => TokenKind.RAngleEqual
      case _ if keyword("==") => TokenKind.EqualEqual
      case _ if keyword("&&&") => TokenKind.TripleAmpersand
      case _ if keyword("<<<") => TokenKind.TripleLAngle
      case _ if keyword(">>>") => TokenKind.TripleRAngle
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
      // User defined operators
      case _ if validUserOpTokens.contains(c) =>
        val p = peek()
        if (validUserOpTokens.contains(p)) {
          userDefinedOp()
        } else if (c == '-' && p.isDigit) {
          number() // negative numbers
        }  else {
          validUserOpTokens.apply(c)
        }
      case c if c.isLetter => name(c.isUpper)
      case c if c.isDigit => number()
      case '$' => builtIn()
      case '\"' => string()
      case '\'' => char()
      case '`' => infixFunction()
      case _ => TokenKind.Err(LexerErr.UnexpectedChar)
    }

    addToken(kind)
  }

  private def addToken(k: TokenKind)(implicit s: State): Unit = {
    val t = s.src.data.slice(s.start.offset, s.current.offset).mkString("")
    s.tokens += Token(k, t, s.start.line, s.start.column)
    s.start = new Position(s.current.line, s.current.column, s.current.offset)
  }

  /**
   * Checks whether the following substring matches a keyword. Note that *comparison includes current*
   */
  private def keyword(k: String)(implicit s: State): Boolean = {
    // check if the keyword can appear before eof
    if (s.current.offset + k.length > s.src.data.length) {
      return false
    }

    val start = s.current.offset - 1
    var matches = true
    var o = 0
    while (matches && o < k.length) {
      if (s.src.data(start + o) != k(o)) {
        matches = false
      } else {
        o += 1
      }
    }
    if (matches) {
      for (_ <- 1 until k.length) {
        advance()
      }
    }

    matches
  }

  private def whitespace()(implicit s: State): Unit = {
    while (!isAtEnd()) {
      if (!peek().isWhitespace) {
        return
      }
      advance()
    }
  }

  private def name(isUpper: Boolean)(implicit s: State): TokenKind = {
    val kind = if (isUpper) {
      TokenKind.UppercaseName
    } else {
      TokenKind.LowercaseName
    }
    while (!isAtEnd()) {
      val c = peek()
      if (!c.isLetter && !c.isDigit && c != '_' && c != '!') {
        return kind
      }
      if (c == '?') {
        advance()
        return TokenKind.VariableHole
      }

      advance()
    }

    kind
  }

  private def builtIn()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      val c = peek()
      if (c == '$') {
        advance()
        return TokenKind.BuiltIn
      } else if (!c.isLetter && c != '_') {
        return TokenKind.JavaName
      }

      advance()
    }

    TokenKind.Err(LexerErr.UnterminatedBuiltIn)
  }

  private def javaName()(implicit s: State): TokenKind = {
    advance()
    while (!isAtEnd()) {
      val c = peek()
      if (!c.isLetter && !c.isDigit && c != '_' && c != '!') {
        return TokenKind.JavaName
      }

      advance()
    }

    TokenKind.JavaName
  }

  // Advances state past a greek name
  private def greekName()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!isGreekNameChar(peek())) {
        return TokenKind.GreekName
      }
      advance()
    }
    TokenKind.GreekName
  }

  // Advances state past a math name
  private def mathName()(implicit s: State): TokenKind = {
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
    i >= 8592 && i <= 8959
  }

  // Checks whether c lies in unicode range U+0370 to U+03FF
  private def isGreekNameChar(c: Char): Boolean = {
    val i = c.toInt
    i >= 880 && i <= 1023
  }

  private def namedHole()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (!peek().isLetter) {
        return TokenKind.NamedHole
      }

      advance()
    }

    TokenKind.NamedHole
  }

  // Advances state past an infix function
  private def infixFunction()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      val c = peek()
      if (c == '`') {
        advance()
        return TokenKind.InfixFunction
      }
      advance()
    }

    TokenKind.Err(LexerErr.UnterminatedInfixFunction)
  }

  // Advances state past a user defined operator
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

  // Advances state past a string
  private def string()(implicit s: State): TokenKind = {
    var prev = ' '
    while (!isAtEnd()) {
      val c = peek()
      // Check for termination while handling escaped '\"'
      if (c == '\"' && prev != '\\') {
        advance()
        return TokenKind.String
      }
      prev = advance()
    }

    TokenKind.Err(LexerErr.UnterminatedString)
  }

  // Advances state past a char
  private def char()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      val c = peek()
      if (c == '\'') {
        advance()
        return TokenKind.Char
      }
      advance()
    }

    TokenKind.Err(LexerErr.UnterminatedChar)
  }

  // Advances state past a number of any type
  private def number()(implicit s: State): TokenKind = {
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
              TokenKind.Float64
            } else {
              TokenKind.Int32
            }
        }
      }
    }

    TokenKind.Err(LexerErr.MalformedNumber)
  }

  // Advances state past a decorator by looking for the next non-letter character
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

  // Advances state past a line comment by looking for the next newline
  private def lineComment()(implicit s: State): TokenKind = {
    while (!isAtEnd()) {
      if (peek() == '\n') {
        return TokenKind.LineComment
      } else {
        advance()
      }
    }

    TokenKind.LineComment
  }

  // Advances state past a block-comment. Supports nested block comments by maintaining a level counter.
  private def blockComment()(implicit s: State): TokenKind = {
    var l = 1
    while (!isAtEnd()) {
      (peek(), peekPeek()) match {
        case ('/', Some('*')) =>
          l += 1
          if (l >= 32) {
            return TokenKind.Err(LexerErr.BlockCommentTooDeep)
          }
          advance()
        case ('*', Some('/')) =>
          l -= 1
          advance()
          advance()
          if (l == 0) {
            return TokenKind.BlockComment
          }
        case _ => advance()
      }
    }

    TokenKind.Err(LexerErr.UnterminatedBlockComment)
  }

  private class Position(var line: Int, var column: Int, var offset: Int)

  private class State(val src: Ast.Source) {
    var start: Position = new Position(0, 0, 0)
    val current: Position = new Position(0, 0, 0)
    val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  }

  private val validUserOpTokens = Map(
    '+' -> TokenKind.Plus,
    '-' -> TokenKind.Minus,
    '*' -> TokenKind.Star,
    '<' -> TokenKind.LAngle,
    '>' -> TokenKind.RAngle,
    '=' -> TokenKind.Equal,
    '!' -> TokenKind.Bang,
    '&' -> TokenKind.Ampersand,
    '|' -> TokenKind.Bar,
    '^' -> TokenKind.Caret,
    '$' -> TokenKind.Dollar
  )

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
