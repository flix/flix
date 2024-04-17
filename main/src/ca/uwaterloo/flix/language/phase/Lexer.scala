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
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, ReadAst, SourceLocation, Token, TokenKind}
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._
import org.parboiled2.ParserInput

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
  private val BlockCommentMaxNestingLevel = 32

  /**
   * The maximal allowed nesting level of string interpolation.
   */
  private val InterpolatedStringMaxNestingLevel = 32

  /**
   * The characters allowed in a user defined operator mapped to their `TokenKind`s
   */
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

  /**
   * Since Flix support hex decimals, a 'digit' can also be some select characters.
   */
  private def isDigit(c: Char): Boolean = '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

  /**
   * The internal state of the lexer as it tokenizes a single source.
   * At any point execution `start` represents the start of the token currently being considered.
   * Likewise `end` represents the end of the token currently being considered,
   * while `current` is the current read head of the lexer.
   * Note that both start and current are `Position`s since they are not necessarily on the same line.
   * `current` will always be on the same character as or past `start`.
   * As tokens are produced they are placed in `tokens`.
   */
  private class State(val src: Ast.Source) {
    var start: Position = new Position(0, 0, 0)
    val current: Position = new Position(0, 0, 0)
    var end: Position = new Position(0, 0, 0)
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
      if (flix.options.xparser) {
        // New lexer and parser disabled. Return immediately.
        return Validation.success(Map.empty[Ast.Source, Array[Token]])
      }

      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(root.sources, oldTokens)

      // Lex each stale source file in parallel.
      val results = ParOps.parMap(stale.keys)(src => mapN(tryLex(src))(tokens => src -> tokens))

      // Construct a map from each source to its tokens.
      val reused = fresh.map(m => Validation.success(m))
      mapN(sequence(results ++ reused))(_.toMap)
    }
  }

  /**
   * A workaround to allow using the new lexer but avoid crashing the compiler on unforeseen bugs.
   * This is not viable long term and should never be merged into a stable release,
   * but it allows us to battle-test the lexer in nightly, without inconveniencing users too much.
   */
  private def tryLex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    try {
      lex(src)
    } catch {
      case except: Throwable =>
        println(src.data.mkString)
        except.printStackTrace()
        Validation.success(Array.empty[Token])
    }
  }

  /**
   * Lexes a single source (file) into an array of tokens.
   */
  private def lex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    implicit val s: State = new State(src)
    while (!eof()) {
      whitespace()
      if (!eof()) {
        s.start = new Position(s.current.line, s.current.column, s.current.offset)
        val k = scanToken()
        addToken(k)
      }
    }

    // Add a virtual eof token at the last position.
    addToken(TokenKind.Eof)

    val errors = s.tokens.collect {
      case Token(TokenKind.Err(err), _, _, _, _, _, _, _) => err
    }

    Validation.toSuccessOrSoftFailure(s.tokens.toArray, errors)
  }

  /**
   * Advances current position one char forward, returning the char it was previously sitting on,
   * while keeping track of line and column numbers too.
   * Note: If the lexer has arrived at EOF advance will continuously return EOF without advancing.
   * This is a design choice to avoid returning an Option[Char], which would be doable but tedious to work with.
   */
  private def advance()(implicit s: State): Char = {
    if (s.current.offset >= s.src.data.length) {
      return '\u0000'
    }

    val c = s.src.data(s.current.offset)
    if (c == '\n') {
      s.end = new Position(s.current.line, s.current.column, s.current.offset)
      s.current.offset += 1
      s.current.line += 1
      s.current.column = 0
    } else {
      s.end = new Position(s.current.line, (s.current.column + 1).toShort, s.current.offset)
      s.current.offset += 1
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
   * Note: Peek does not to bound checks. This is done under the assumption that the lexer
   * is only ever advanced using `advance`.
   * Since `advance` cannot move past EOF peek will always be in bounds.
   */
  private def peek()(implicit s: State): Char = {
    if (s.current.offset >= s.src.data.length) {
      return s.src.data.last
    }
    s.src.data(s.current.offset)
  }

  /**
   * Peeks the character after the one that state is sitting on if available.
   */
  private def peekPeek()(implicit s: State): Option[Char] = {
    if (s.current.offset >= s.src.data.length - 1) {
      None
    } else {
      Some(s.src.data(s.current.offset + 1))
    }
  }

  /**
   * A helper function wrapping peek, with special handling for escaped characters.
   * This is useful for "\"" or `'\''`.
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
   * Checks if the current position has landed on end-of-file
   */
  private def eof()(implicit s: State): Boolean = {
    s.current.offset >= s.src.data.length
  }

  /**
   * A helper function for producing a `SourceLocation` starting at `s.start`.
   *
   * @param length the length that the source location should span
   */
  private def sourceLocationAtStart(length: Int = 1)(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val line = s.start.line + 1
    val column = s.start.column + 1
    SourceLocation(s.parserInput, s.src, isReal = true, line, column.toShort, line, (column + length).toShort)
  }

  /**
   * A helper function for producing a `SourceLocation` starting at `s.current`.
   *
   * @param length the length that the source location should span
   */
  private def sourceLocationAtCurrent(length: Int = 1)(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val line = s.current.line + 1
    val column = s.current.column + 1
    SourceLocation(s.parserInput, s.src, isReal = true, line, column.toShort, line, (column + length).toShort)
  }

  /**
   * Consumes the text between `s.start` and `s.offset` to produce a token.
   * Afterwards `s.start` is reset to the next position after the previous token.
   */
  private def addToken(kind: TokenKind)(implicit s: State): Unit = {
    s.tokens += Token(kind, s.src.data, s.start.offset, s.current.offset, s.start.line, s.start.column.toShort, s.end.line, s.end.column.toShort)
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
      case '\\' => TokenKind.Backslash
      case _ if isMatch(".{") => TokenKind.DotCurlyL
      case '.' => TokenKind.Dot
      case '$' if peek().isUpper => acceptBuiltIn()
      case '\"' => acceptString()
      case '\'' => acceptChar()
      case '`' => acceptInfixFunction()
      case _ if isMatch("##") => acceptJavaName()
      case _ if isMatch("#{") => TokenKind.HashCurlyL
      case _ if isMatch("#(") => TokenKind.HashParenL
      case '#' => TokenKind.Hash
      case _ if isMatch("///") => acceptDocComment()
      case _ if isMatch("//") => acceptLineComment()
      case _ if isMatch("/*") => acceptBlockComment()
      case '/' => TokenKind.Slash
      case '@' if peek().isLetter => acceptAnnotation()
      case '@' => TokenKind.At
      case _ if isMatch("???") => TokenKind.HoleAnonymous
      case '?' if peek().isLetter => acceptNamedHole()
      case _ if isOperator(":::") => TokenKind.TripleColon
      case _ if isOperator("::") => TokenKind.ColonColon
      case _ if isOperator(":=") => TokenKind.ColonEqual
      case _ if isOperator(":-") => TokenKind.ColonMinus
      case _ if isOperator(":") => TokenKind.Colon
      case _ if isOperator("**") => TokenKind.StarStar
      case _ if isOperator("<-") => TokenKind.ArrowThinL
      case _ if isOperator("->") => TokenKind.ArrowThinR
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
      case _ if isKeyword("debug") => TokenKind.KeywordDebug
      case _ if isKeyword("debug!") => TokenKind.KeywordDebugBang
      case _ if isKeyword("debug!!") => TokenKind.KeywordDebugBangBang
      case _ if isKeyword("def") => TokenKind.KeywordDef
      case _ if isKeyword("deref") => TokenKind.KeywordDeref
      case _ if isKeyword("discard") => TokenKind.KeywordDiscard
      case _ if isKeyword("do") => TokenKind.KeywordDo
      case _ if isKeyword("eff") => TokenKind.KeywordEff
      case _ if isKeyword("else") => TokenKind.KeywordElse
      case _ if isKeyword("enum") => TokenKind.KeywordEnum
      case _ if isKeywordLiteral("false") => TokenKind.KeywordFalse
      case _ if isKeyword("fix") => TokenKind.KeywordFix
      case _ if isKeyword("forall") => TokenKind.KeywordForall
      case _ if isKeyword("forA") => TokenKind.KeywordForA
      case _ if isKeyword("force") => TokenKind.KeywordForce
      case _ if isKeyword("foreach") => TokenKind.KeywordForeach
      case _ if isKeyword("forM") => TokenKind.KeywordForM
      case _ if isKeyword("from") => TokenKind.KeywordFrom
      case _ if isKeyword("java_get_field") => TokenKind.KeywordJavaGetField
      case _ if isKeyword("java_set_field") => TokenKind.KeywordJavaSetField
      case _ if isKeyword("java_new") => TokenKind.KeywordJavaNew
      case _ if isKeyword("if") => TokenKind.KeywordIf
      case _ if isKeyword("import") => TokenKind.KeywordImport
      case _ if isKeyword("inject") => TokenKind.KeywordInject
      case _ if isKeyword("inline") => TokenKind.KeywordInline
      case _ if isKeyword("instanceof") => TokenKind.KeywordInstanceOf
      case _ if isKeyword("instance") => TokenKind.KeywordInstance
      case _ if isKeyword("into") => TokenKind.KeywordInto
      case _ if isKeyword("lawful") => TokenKind.KeywordLawful
      case _ if isKeyword("law") => TokenKind.KeywordLaw
      case _ if isKeyword("lazy") => TokenKind.KeywordLazy
      case _ if isKeyword("let") => TokenKind.KeywordLet
      case _ if isKeyword("masked_cast") => TokenKind.KeywordMaskedCast
      case _ if isKeyword("match") => TokenKind.KeywordMatch
      case _ if isKeyword("mod") => TokenKind.KeywordMod
      case _ if isKeyword("new") => TokenKind.KeywordNew
      case _ if isKeyword("not") => TokenKind.KeywordNot
      case _ if isKeywordLiteral("null") => TokenKind.KeywordNull
      case _ if isKeyword("open_variant") => TokenKind.KeywordOpenVariant
      case _ if isKeyword("open_variant_as") => TokenKind.KeywordOpenVariantAs
      case _ if isKeyword("or") => TokenKind.KeywordOr
      case _ if isKeyword("override") => TokenKind.KeywordOverride
      case _ if isKeyword("par") => TokenKind.KeywordPar
      case _ if isKeyword("pub") => TokenKind.KeywordPub
      case _ if isKeyword("Pure") => TokenKind.KeywordPure
      case _ if isKeyword("project") => TokenKind.KeywordProject
      case _ if isKeyword("query") => TokenKind.KeywordQuery
      case _ if isKeyword("ref") => TokenKind.KeywordRef
      case _ if isKeyword("region") => TokenKind.KeywordRegion
      case _ if isKeyword("restrictable") => TokenKind.KeywordRestrictable
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
      case _ if isKeyword("trait") => TokenKind.KeywordTrait
      case _ if isKeywordLiteral("true") => TokenKind.KeywordTrue
      case _ if isKeyword("try") => TokenKind.KeywordTry
      case _ if isKeyword("type") => TokenKind.KeywordType
      case _ if isKeyword("typematch") => TokenKind.KeywordTypeMatch
      case _ if isKeyword("unchecked_cast") => TokenKind.KeywordUncheckedCast
      case _ if isKeyword("Univ") => TokenKind.KeywordUniv
      case _ if isKeyword("use") => TokenKind.KeywordUse
      case _ if isKeyword("where") => TokenKind.KeywordWhere
      case _ if isKeyword("with") => TokenKind.KeywordWith
      case _ if isKeyword("without") => TokenKind.KeywordWithout
      case _ if isKeyword("yield") => TokenKind.KeywordYield
      case _ if isKeyword("xor") => TokenKind.KeywordXor
      case _ if isKeyword("Set#") => TokenKind.SetHash
      case _ if isKeyword("Array#") => TokenKind.ArrayHash
      case _ if isKeyword("Map#") => TokenKind.MapHash
      case _ if isKeyword("List#") => TokenKind.ListHash
      case _ if isKeyword("Vector#") => TokenKind.VectorHash
      case _ if isMatch("regex\"") => acceptRegex()
      case _ if isMathNameChar(c) => acceptMathName()
      case _ if isGreekNameChar(c) => acceptGreekName()
      case '_' =>
        val p = peek()
        if (p.isLetterOrDigit) {
          acceptName(p.isUpper)
        } else if (isMathNameChar(p)) {
          advance()
          acceptMathName()
        } else if (isUserOp(p).isDefined) {
          advance()
          acceptUserDefinedOp()
        } else TokenKind.Underscore
      case c if c.isLetter => acceptName(c.isUpper)
      case '0' if peek() == 'x' => acceptHexNumber()
      case c if isDigit(c) => acceptNumber()
      // User defined operators.
      case _ if isUserOp(c).isDefined =>
        val p = peek()
        if (c == '<' && p == '>' && peekPeek().flatMap(isUserOp).isEmpty) {
          // Make sure '<>' is read as AngleL, AngleR and not UserDefinedOperator for empty case sets.
          TokenKind.AngleL
        } else if (isUserOp(p).isDefined) {
          acceptUserDefinedOp()
        } else {
          isUserOp(c).get
        }
      case c => TokenKind.Err(LexerError.UnexpectedChar(c.toString, sourceLocationAtStart()))
    }
  }

  /**
   * Check that the potential keyword is sufficiently separated, taking care not to go out-of-bounds.
   * A keyword is separated if it is surrounded by anything __but__ a character, digit a dot or underscore.
   * Note that __comparison includes current__.
   */
  private def isSeparated(keyword: String, allowDot: Boolean = false)(implicit s: State): Boolean = {
    def isSep(c: Char) = !(c.isLetter || c.isDigit || c == '_' || !allowDot && c == '.')

    val leftIndex = s.current.offset - 2
    val rightIndex = s.current.offset + keyword.length - 1
    val isSeperatedLeft = leftIndex < 0 || isSep(s.src.data(leftIndex))
    val isSeperatedRight = rightIndex > s.src.data.length - 1 || isSep(s.src.data(rightIndex))
    isSeperatedLeft && isSeperatedRight
  }

  /**
   * Check that the potential operator is sufficiently separated, taking care not to go out-of-bounds.
   * An operator is separated if it is surrounded by anything __but__ another valid user operator character.
   * Note that __comparison includes current__.
   */
  private def isSeparatedOperator(keyword: String)(implicit s: State): Boolean = {
    val leftIndex = s.current.offset - 2
    val rightIndex = s.current.offset + keyword.length - 1
    val isSeperatedLeft = leftIndex < 0 || isUserOp(s.src.data(leftIndex)).isEmpty
    val isSeperatedRight = rightIndex > s.src.data.length - 1 || isUserOp(s.src.data(rightIndex)).isEmpty
    isSeperatedLeft && isSeperatedRight
  }

  /**
   * Checks whether the following substring matches a keyword. Note that __comparison includes current__.
   * Also note that this will advance the current position past the keyword if there is a match.
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

    if (matches) {
      for (_ <- 1 until keyword.length) {
        advance()
      }
    }

    matches
  }

  /**
   * Checks whether the following substring matches a operator.
   * Note that __comparison includes current__.
   * Also note that this will advance the current position past the keyword if there is a match.
   */
  private def isOperator(op: String)(implicit s: State): Boolean = {
    isSeparatedOperator(op) && isMatch(op)
  }

  /**
   * Checks whether the following substring matches a keyword literal. IE. "true" or "null"
   * Note that __comparison includes current__.
   * Also note that this will advance the current position past the keyword if there is a match.
   */
  private def isKeywordLiteral(keyword: String)(implicit s: State): Boolean = {
    // Allow dot here means that the literal gets recognized even when it is followed by a '.'.
    // We want this for literals like 'true', but not for keywords like 'not'.
    // This is because a symbol like 'not' can be imported from Java in a qualified path.
    // For instance `import java.math.BigInteger.not(): BigInt \ {} as bNot;`. <- 'not' needs to be read as a name here.
    // We are assuming no literal keyword needs to be imported. So no importing something called 'true' from java.
    isSeparated(keyword, allowDot = true) && isMatch(keyword)
  }

  /**
   * Checks whether the following substring matches a keyword.
   * Note that __comparison includes current__.
   * Also note that this will advance the current position past the keyword if there is a match.
   */
  private def isKeyword(keyword: String)(implicit s: State): Boolean = {
    isSeparated(keyword) && isMatch(keyword)
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
    while (!eof()) {
      val p = peek()

      if (p == '$') {
        // Check for termination.
        advance()
        return TokenKind.BuiltIn
      }

      if (p.isLower) {
        // This means that the opening '$' was a separator.
        // we need to rewind the lexer to just after '$'.
        for (_ <- 0 until advances) {
          retreat()
        }
        return TokenKind.Dollar
      }

      if (!p.isLetter && !p.isDigit && p != '_') {
        // Do not allow non-letters other than _.
        // This handles cases like a block comment for instance
        // IE. `$BUILT_/*IN*/$` is disallowed.
        return TokenKind.Err(LexerError.UnterminatedBuiltIn(sourceLocationAtStart()))
      }

      advance()
      advances += 1
    }
    TokenKind.Err(LexerError.UnterminatedBuiltIn(sourceLocationAtStart()))
  }

  /**
   * Moves current position past all whitespace characters.
   */
  private def whitespace()(implicit s: State): Unit = {
    while (!eof()) {
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
      TokenKind.NameLowerCase
    }
    while (!eof()) {
      val p = peek()
      if (p == '?') {
        advance()
        return TokenKind.HoleVariable
      } else if (!p.isLetter && !p.isDigit && p != '_' && p != '!' && p != '$') {
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
    while (!eof()) {
      val p = peek()
      if (!p.isLetter && !p.isDigit && p != '_' && p != '!' && p != '$') {
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
    while (!eof()) {
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
    0x0370 <= i && i <= 0x03FF
  }

  /**
   * Moves current position past a math name.
   * Math names must lie in the unicode range U+2190 to U+22FF
   * IE. "⊆"
   */
  private def acceptMathName()(implicit s: State): TokenKind = {
    while (!eof()) {
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
    0x2190 <= i && i <= 0x22FF
  }

  /**
   * Moves current position past a named hole. IE. "?foo".
   */
  private def acceptNamedHole()(implicit s: State): TokenKind = {
    while (!eof()) {
      if (!peek().isLetter && !peek().isDigit) {
        return TokenKind.HoleNamed
      }
      advance()
    }
    TokenKind.HoleNamed
  }


  /**
   * Moves current position past an infix function.
   */
  private def acceptInfixFunction()(implicit s: State): TokenKind = {
    while (!eof()) {
      val p = peek()
      if (p == '`') {
        advance()
        return TokenKind.InfixFunction
      }

      if (p != '.' && p != '!' && !p.isLetter && !p.isDigit && !isMathNameChar(p) && !isGreekNameChar(p)) {
        // check for chars that are not allowed in function names,
        // to handle cases like '`my function` or `my/**/function`'
        return TokenKind.Err(LexerError.UnterminatedInfixFunction(sourceLocationAtStart()))
      }

      advance()
    }
    TokenKind.Err(LexerError.UnterminatedInfixFunction(sourceLocationAtStart()))
  }

  /**
   * Moves current position past a user defined operator. IE. "<*>".
   * A user defined operator may be any combination of length 2 or more
   * of the characters in [[isUserOp]].
   */
  private def acceptUserDefinedOp()(implicit s: State): TokenKind = {
    while (!eof()) {
      if (isUserOp(peek()).isEmpty) {
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
    while (!eof()) {
      var p = escapedPeek()
      // Check for the beginning of a string interpolation.
      val prevPrev = previousPrevious()
      val prev = previous()
      val isInterpolation = !prevPrev.contains('\\') && prev.contains('$') && p.contains('{')
      val isDebug = !prevPrev.contains('\\') && prev.contains('%') && p.contains('{')
      if (isInterpolation || isDebug) {
        acceptStringInterpolation(isDebug) match {
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
        return TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
      }
      advance()
    }
    TokenKind.Err(LexerError.UnterminatedString(sourceLocationAtStart()))
  }

  /**
   * Moves current position past an interpolated expression within a string. IE. "Hi ${name}!".
   * This function also handles debug strings like "Value: %{x}".
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
  private def acceptStringInterpolation(isDebug: Boolean = false)(implicit s: State): TokenKind = {
    // Handle max nesting level
    s.interpolationNestingLevel += 1
    if (s.interpolationNestingLevel > InterpolatedStringMaxNestingLevel) {
      s.interpolationNestingLevel = 0
      return TokenKind.Err(LexerError.StringInterpolationTooDeep(sourceLocationAtCurrent()))
    }
    val startLocation = sourceLocationAtCurrent()
    advance() // Consume '{'.
    addToken(if (isDebug) TokenKind.LiteralDebugStringL else TokenKind.LiteralStringInterpolationL)
    // consume tokens until a terminating '}' is found
    var blockNestingLevel = 0
    while (!eof()) {
      whitespace()
      if (!eof()) {
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
            return if (isDebug) TokenKind.LiteralDebugStringR else TokenKind.LiteralStringInterpolationR
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
      val p = escapedPeek()
      if (p.contains('\'')) {
        advance()
        return TokenKind.LiteralChar
      }

      if ((prev, p) == ('/', Some('*'))) {
        // This handles block comment within a char.
        return TokenKind.Err(LexerError.UnterminatedChar(sourceLocationAtStart()))
      }
      prev = advance()
    }

    TokenKind.Err(LexerError.UnterminatedChar(sourceLocationAtStart()))
  }

  /**
   * Moves current position past a regex literal.
   * If the regex  is unterminated a `TokenKind.Err` is returned.
   */
  private def acceptRegex()(implicit s: State): TokenKind = {
    while (!eof()) {
      val p = escapedPeek()
      if (p.contains('"')) {
        advance()
        return TokenKind.LiteralRegex
      }
      advance()
    }

    TokenKind.Err(LexerError.UnterminatedRegex(sourceLocationAtStart()))
  }

  /**
   * Moves current position past a number literal. IE. "123i32" or "456.78f32"
   * It is optional to have a trailing type indicator on number literals.
   * If it is missing Flix defaults to `f64` for decimals and `i32` for integers.
   * NB. The char 'e' might appear as part of scientific notation.
   * */
  private def acceptNumber()(implicit s: State): TokenKind = {
    var isDecimal = false
    var isScientificNotation = false
    var error: Option[TokenKind] = None
    while (!eof()) {
      peek() match {
        case c if c.isDigit => advance()
        // 'e' mark scientific notation if not handling a hex number
        case 'e' =>
          if (isScientificNotation) {
            error = Some(TokenKind.Err(LexerError.DoubleEInNumber(sourceLocationAtCurrent())))
          }
          isScientificNotation = true
          advance()
        // Dots mark a decimal
        case '.' if isDecimal =>
          val loc = sourceLocationAtCurrent()
          advance()
          error = Some(TokenKind.Err(LexerError.DoubleDottedNumber(loc)))
        case '.' if peekPeek().exists(c => c.isDigit || c == '.') =>
          isDecimal = true
          advance()
        // '_' that is not in tail-position
        case '_' if peekPeek().exists(_.isDigit) => advance()
        // sequence of underscores
        case '_' if peekPeek().contains('_') =>
          // Consume the whole sequence of '_'
          advance()
          advance()
          while (!eof() && peek() == '_') {
            advance()
          }
          error = Some(TokenKind.Err(LexerError.DoubleUnderscoreInNumber(sourceLocationAtCurrent())))
        // If this is reached an explicit number type might occur next
        case _ => return advance() match {
          case '_' => TokenKind.Err(LexerError.TrailingUnderscoreInNumber(sourceLocationAtCurrent()))
          case _ if isMatch("f32") => error.getOrElse(TokenKind.LiteralFloat32)
          case _ if isMatch("f64") => error.getOrElse(TokenKind.LiteralFloat64)
          case _ if isMatch("i8") => error.getOrElse(TokenKind.LiteralInt8)
          case _ if isMatch("i16") => error.getOrElse(TokenKind.LiteralInt16)
          case _ if isMatch("i32") => error.getOrElse(TokenKind.LiteralInt32)
          case _ if isMatch("i64") => error.getOrElse(TokenKind.LiteralInt64)
          case _ if isMatch("ii") => error.getOrElse(TokenKind.LiteralBigInt)
          case _ if isMatch("ff") => error.getOrElse(TokenKind.LiteralBigDecimal)
          case _ =>
            retreat()
            if (isDecimal) {
              error.getOrElse(TokenKind.LiteralFloat64)
            } else {
              error.getOrElse(TokenKind.LiteralInt32)
            }
        }
      }
    }
    // The very last char of the file was a digit so return the appropriate token.
    if (isDecimal) {
      error.getOrElse(TokenKind.LiteralFloat64)
    } else {
      error.getOrElse(TokenKind.LiteralInt32)
    }
  }

  /**
   * Moves current position past a hex number literal. IE. "0x123i32" or "0xAB21CD"
   * It is optional to have a trailing type indicator on number literals.
   * If it is missing Flix defaults to `i32`.
   * */
  private def acceptHexNumber()(implicit s: State): TokenKind = {
    advance() // consume 'x'
    var error: Option[TokenKind] = if (peek() == '_') {
      val loc = sourceLocationAtCurrent()
      advance()
      Some(TokenKind.Err(LexerError.HexLiteralStartsOnUnderscore(loc)))
    } else {
      None
    }
    while (!eof()) {
      peek() match {
        case c if isDigit(c) => advance()
        // '_' that is not in tail-position
        case '_' if peekPeek().exists(isDigit) => advance()
        // sequence of underscores
        case '_' if peekPeek().contains('_') =>
          // Consume the whole sequence of '_'
          advance()
          advance()
          while (!eof() && peek() == '_') {
            advance()
          }
          error = Some(TokenKind.Err(LexerError.DoubleUnderscoreInNumber(sourceLocationAtCurrent())))
        // underscore in tail position
        case '_' =>
          advance()
          return TokenKind.Err(LexerError.TrailingUnderscoreInNumber(sourceLocationAtCurrent()))
        // If this is reached an explicit number type might occur next
        case _ => return advance() match {
          case '_' => TokenKind.Err(LexerError.TrailingUnderscoreInNumber(sourceLocationAtCurrent()))
          case _ if isMatch("f32") => error.getOrElse(TokenKind.LiteralFloat32)
          case _ if isMatch("f64") => error.getOrElse(TokenKind.LiteralFloat64)
          case _ if isMatch("i8") => error.getOrElse(TokenKind.LiteralInt8)
          case _ if isMatch("i16") => error.getOrElse(TokenKind.LiteralInt16)
          case _ if isMatch("i32") => error.getOrElse(TokenKind.LiteralInt32)
          case _ if isMatch("i64") => error.getOrElse(TokenKind.LiteralInt64)
          case _ if isMatch("ii") => error.getOrElse(TokenKind.LiteralBigInt)
          case _ if isMatch("ff") => error.getOrElse(TokenKind.LiteralBigDecimal)
          case _ =>
            retreat()
            error.getOrElse(TokenKind.LiteralInt32)
        }
      }
    }
    // The very last char of the file was a digit so return the appropriate token.
    error.getOrElse(TokenKind.LiteralInt32)
  }

  /**
   * Moves current position past an annotation. IE. "@Test".
   */
  private def acceptAnnotation()(implicit s: State): TokenKind = {
    while (!eof()) {
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
    while (!eof()) {
      if (peek() == '\n') {
        return TokenKind.CommentLine
      } else {
        advance()
      }
    }
    TokenKind.CommentLine
  }

  /**
   * Moves current position past a doc-comment
   */
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
    while (!eof()) {
      (peek(), peekPeek()) match {
        case ('/', Some('*')) =>
          level += 1
          if (level >= BlockCommentMaxNestingLevel) {
            return TokenKind.Err(LexerError.BlockCommentTooDeep(sourceLocationAtCurrent()))
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
    TokenKind.Err(LexerError.UnterminatedBlockComment(sourceLocationAtStart()))
  }
}
