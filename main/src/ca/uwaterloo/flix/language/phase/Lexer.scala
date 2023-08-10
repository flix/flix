/*
 * Copyright 2023 Magnus Madsen
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

// NOTES:


// TODO: How do we test if UTF-8 support works? just put emojis in strings?

// TODO: How do we handle positions in multiline tokens? Stuff like strings with newlines and block comments.
// the line field would be at the last line of the token not the first. We can back track by counting newlines though but that seems expensive.
// Maybe we need a "ignore newline" mode? or maybe its better to make start and current into (line, column, offset) tuples

object Lexer {

  def run(root: ReadAst.Root)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] =
    flix.phase("Lexer") {
      // Lex each source file in parallel.
      val results = ParOps.parMap(root.sources) {
        case (src, _) => mapN(lex(src))({
          case tokens => src -> tokens
        })
      }

      // Construct a map from each source to its tokens.
      mapN(sequence(results))(_.toMap)
    }

  private def lex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    implicit val s: State = new State(src)
    while (!isAtEnd()) {
      whitespace()
      if (!isAtEnd()) {
        s.start = s.current
        scanToken()
      }
    }

    s.tokens += Token(TokenKind.Eof, "<eof>", s.line, s.column)

    println(f"> ${src.name}\n${src.data.mkString("")}\n${s.tokens.mkString("\n")}")
    s.tokens.toArray.toSuccess
  }

  private def advance()(implicit s: State): Char = {
    val c = s.src.data(s.current)
    s.current += 1
    if (c == '\n') {
      s.line += 1
      s.column = 0
    } else {
      s.column += 1
    }

    c
  }

  private def peek()(implicit s: State): Char = {
    s.src.data(s.current)
  }

  private def isAtEnd()(implicit s: State): Boolean = {
    s.current == s.src.data.length
  }

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
      case '+' => TokenKind.Plus
      case '-' => {
        if (peek().isDigit) {
          number()
        } else {
          TokenKind.Minus
        }
      }
      case '/' => TokenKind.Slash
      case '@' => TokenKind.At
      case '=' => if (peek() == '=') {
        advance()
        TokenKind.EqualEqual
      } else if (peek() == '>') {
        advance()
        TokenKind.Arrow
      } else {
        TokenKind.Equal
      }
      case '.' => if (peek() == '.') {
        advance()
        TokenKind.DotDot
      } else {
        TokenKind.Dot
      }
      case '<' => if (peek() == '-') {
        advance()
        TokenKind.BackArrow
      } else if (peek() == '=') {
        advance()
        TokenKind.LessEqual
      } else {
        TokenKind.Less
      }
      case '>' => if (peek() == '=') {
        advance()
        TokenKind.GreaterEqual
      } else {
        TokenKind.Greater
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
      case '*' => if (peek() == '*') {
        advance()
        TokenKind.StarStar
      } else {
        TokenKind.Star
      }
      case _ if keyword("and") => TokenKind.AndKeyword
      case _ if keyword("or") => TokenKind.OrKeyword
      case _ if keyword("mod") => TokenKind.ModKeyword
      case _ if keyword("not") => TokenKind.NotKeyword
      case _ if keyword("rem") => TokenKind.RemKeyword
      case _ if keyword("Absent") => TokenKind.AbsentKeyword
      case _ if keyword("Unit") => TokenKind.UnitKeyword
      case _ if keyword("Bool") => TokenKind.BoolKeyword
      case _ if keyword("Char") => TokenKind.CharKeyword
      case _ if keyword("String") => TokenKind.StringKeyword
      case _ if keyword("Float32") => TokenKind.Float32Keyword
      case _ if keyword("Float64") => TokenKind.Float64Keyword
      case _ if keyword("Int8") => TokenKind.Int8Keyword
      case _ if keyword("Int16") => TokenKind.Int16Keyword
      case _ if keyword("Int32") => TokenKind.Int32Keyword
      case _ if keyword("Int64") => TokenKind.Int64Keyword
      case _ if keyword("BigInt") => TokenKind.BigIntKeyword
      case _ if keyword("BigDecimal") => TokenKind.BigDecimalKeyword
      case _ if keyword("Impure") => TokenKind.ImpureKeyword
      case _ if keyword("Nil") => TokenKind.NilKeyword
      case _ if keyword("Predicate") => TokenKind.PredicateKeyword
      case _ if keyword("Present") => TokenKind.PresentKeyword
      case _ if keyword("Pure") => TokenKind.PureKeyword
      case _ if keyword("Read") => TokenKind.ReadKeyword
      case _ if keyword("RecordRow") => TokenKind.RecordRowKeyword
      case _ if keyword("Region") => TokenKind.UppercaseRegionKeyword
      case _ if keyword("SchemaRow") => TokenKind.SchemaRowKeyword
      case _ if keyword("Type") => TokenKind.UppercaseTypeKeyword
      case _ if keyword("Write") => TokenKind.WriteKeyword
      case _ if keyword("alias") => TokenKind.AliasKeyword
      case _ if keyword("case") => TokenKind.CaseKeyword
      case _ if keyword("catch") => TokenKind.CatchKeyword
      case _ if keyword("chan") => TokenKind.ChanKeyword
      case _ if keyword("class") => TokenKind.ClassKeyword
      case _ if keyword("def") => TokenKind.DefKeyword
      case _ if keyword("deref") => TokenKind.DerefKeyword
      case _ if keyword("else") => TokenKind.ElseKeyword
      case _ if keyword("enum") => TokenKind.EnumKeyword
      case _ if keyword("false") => TokenKind.FalseKeyword
      case _ if keyword("fix") => TokenKind.FixKeyword
      case _ if keyword("force") => TokenKind.ForceKeyword
      case _ if keyword("if") => TokenKind.IfKeyword
      case _ if keyword("import") => TokenKind.ImportKeyword
      case _ if keyword("inline") => TokenKind.InlineKeyword
      case _ if keyword("instance") => TokenKind.InstanceKeyword
      case _ if keyword("into") => TokenKind.IntoKeyword
      case _ if keyword("lat") => TokenKind.LatKeyword
      case _ if keyword("law") => TokenKind.LawKeyword
      case _ if keyword("lawful") => TokenKind.LawfulKeyword
      case _ if keyword("lazy") => TokenKind.LazyKeyword
      case _ if keyword("let") => TokenKind.LetKeyword
      case _ if keyword("match") => TokenKind.MatchKeyword
      case _ if keyword("namespace") => TokenKind.NamespaceKeyword
      case _ if keyword("null") => TokenKind.NullKeyword
      case _ if keyword("opaque") => TokenKind.OpaqueKeyword
      case _ if keyword("override") => TokenKind.OverrideKeyword
      case _ if keyword("pub") => TokenKind.PubKeyword
      case _ if keyword("ref") => TokenKind.RefKeyword
      case _ if keyword("region") => TokenKind.RegionKeyword
      case _ if keyword("reify") => TokenKind.ReifyKeyword
      case _ if keyword("reifyBool") => TokenKind.ReifyBoolKeyword
      case _ if keyword("reifyEff") => TokenKind.ReifyEffKeyword
      case _ if keyword("reifyType") => TokenKind.ReifyTypeKeyword
      case _ if keyword("rel") => TokenKind.RelKeyword
      case _ if keyword("sealed") => TokenKind.SealedKeyword
      case _ if keyword("set") => TokenKind.SetKeyword
      case _ if keyword("spawn") => TokenKind.SpawnKeyword
      case _ if keyword("Static") => TokenKind.StaticKeyword
      case _ if keyword("true") => TokenKind.TrueKeyword
      case _ if keyword("type") => TokenKind.TypeKeyword
      case _ if keyword("use") => TokenKind.UseKeyword
      case _ if keyword("where") => TokenKind.WhereKeyword
      case _ if keyword("with") => TokenKind.WithKeyword
      case _ if keyword("discard") => TokenKind.DiscardKeyword
      case _ if keyword("object") => TokenKind.ObjectKeyword
      case c if c.isLetter => name(c.isUpper)
      case c if c.isDigit => number()
      case '\"' => string()
      case '\'' => char()

      // TODO: What to do with these?
      //      case _ if keyword("&&&") => TokenKind.Keyword
      //      case _ if keyword("<+>") => TokenKind.Keyword
      //      case _ if keyword("<=>") => TokenKind.Keyword
      //      case _ if keyword("<<<") => TokenKind.Keyword
      //      case _ if keyword(">>>") => TokenKind.Keyword
      //      case _ if keyword("???") => TokenKind.Keyword
      //      case _ if keyword("^^^") => TokenKind.Keyword
      //      case _ if keyword("|||") => TokenKind.Keyword
      //      case _ if keyword("~~~") => TokenKind.Keyword
      //      case _ if keyword("$DEFAULT$") => TokenKind.Keyword

      case _ => TokenKind.Err(LexerErr.UnexpectedSymbol)
    }

    addToken(kind)
  }

  private def addToken(k: TokenKind)(implicit s: State): Unit = {
    val t = s.src.data.slice(s.start, s.current).mkString("")
    val c = s.column - t.length // get the starting column
    s.tokens += Token(k, t, s.line, c)
    s.start = s.current
  }

  private def keyword(k: String)(implicit s: State): Boolean = {
    // check if the keyword can appear before eof
    if (s.current + k.length > s.src.data.length) {
      return false
    }

    val matches = s.src.data.slice(s.current - 1, s.current + k.length - 1).sameElements(k.toCharArray)
    if (matches) { // advance the lexer past the keyword
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
      advance()
    }

    kind
  }

  private def string()(implicit s: State): TokenKind = {
    var prev: Char = ' '
    while (!isAtEnd()) {
      val c = peek()
      if (c == '\"' && prev != '\\') {
        advance()
        return TokenKind.String
      }
      prev = advance()
    }

    TokenKind.Err(LexerErr.UnterminatedString)
  }

  private def char()(implicit s: State): TokenKind = {
    advance()
    if (advance() != '\'') {
      TokenKind.Err(LexerErr.UnterminatedChar)
    } else {
      TokenKind.Char
    }
  }

  private def number()(implicit s: State): TokenKind = {
    var isDecimal = false
    while (!isAtEnd()) {
      peek() match {
        case '.' => {
          isDecimal = true
          advance()
        }
        case c if c.isDigit => {
          advance()
        }
        case c if c.isWhitespace =>
          return if (isDecimal) {
            TokenKind.Float64
          } else {
            TokenKind.Int32
          }
        case _ => return advance() match {
          case _ if keyword("f32") => TokenKind.Float32
          case _ if keyword("f64") => TokenKind.Float64
          case _ if keyword("i8") => TokenKind.Int8
          case _ if keyword("i16") => TokenKind.Int16
          case _ if keyword("i32") => TokenKind.Int32
          case _ if keyword("i64") => TokenKind.Int64
          case _ if keyword("ii") => TokenKind.BigInt
          case _ if keyword("ff") => TokenKind.BigDecimal
          case _ => TokenKind.Err(LexerErr.MalformedNumberType)
        }
      }
    }

    TokenKind.Err(LexerErr.MalformedNumber)
  }

  private def blockComment()(implicit s: State): Unit = ???

  private def comment()(implicit s: State): Unit = ???

  private class State(val src: Ast.Source) {
    var start: Int = 0
    var current: Int = 0
    var line: Int = 0
    var column: Int = 0
    var tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  }

}
