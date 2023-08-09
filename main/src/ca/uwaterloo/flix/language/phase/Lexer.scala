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
import ca.uwaterloo.flix.language.ast.{Ast, ReadAst, Token, TokenKind}
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

// TODO: How do we handle positions in multiline tokens? Stuff like strings with newlines and block comments.
// the line field would be at the last line of the token not the first. We can back track by counting newlines though but that seems expensive.
// Maybe we need a "ignore newline" mode? or maybe its better to make start and current into (line, column) tuples

object Lexer {

  def run(root: ReadAst.Root)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] = {
    if (!flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Map.empty[Ast.Source, Array[Token]].toSuccess
    }

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

    println(f"> ${src.name}\n${src.data.mkString("")}\n${s.tokens}")
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
      case ';' => TokenKind.Semi
      case ',' => TokenKind.Comma
      case '+' => TokenKind.Plus
      case '-' => TokenKind.Minus
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
      } else if (peek() == '='){
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

      case _ if matches("and") => TokenKind.AndKeyword
      case _ if matches("or") => TokenKind.OrKeyword
      case _ if matches("mod") => TokenKind.ModKeyword
      case _ if matches("not") => TokenKind.NotKeyword
      case _ if matches("rem") => TokenKind.RemKeyword
      case _ if matches("Absent") => TokenKind.AbsentKeyword
      case _ if matches("Bool") => TokenKind.BoolKeyword
      case _ if matches("Impure") => TokenKind.ImpureKeyword
      case _ if matches("Nil") => TokenKind.NilKeyword
      case _ if matches("Predicate") => TokenKind.PredicateKeyword
      case _ if matches("Present") => TokenKind.PresentKeyword
      case _ if matches("Pure") => TokenKind.PureKeyword
      case _ if matches("Read") => TokenKind.ReadKeyword
      case _ if matches("RecordRow") => TokenKind.RecordRowKeyword
      case _ if matches("Region") => TokenKind.UppercaseRegionKeyword
      case _ if matches("SchemaRow") => TokenKind.SchemaRowKeyword
      case _ if matches("Type") => TokenKind.UppercaseTypeKeyword
      case _ if matches("Write") => TokenKind.WriteKeyword
      case _ if matches("alias") => TokenKind.AliasKeyword
      case _ if matches("case") => TokenKind.CaseKeyword
      case _ if matches("catch") => TokenKind.CatchKeyword
      case _ if matches("chan") => TokenKind.ChanKeyword
      case _ if matches("class") => TokenKind.ClassKeyword
      case _ if matches("def") => TokenKind.DefKeyword
      case _ if matches("deref") => TokenKind.DerefKeyword
      case _ if matches("else") => TokenKind.ElseKeyword
      case _ if matches("enum") => TokenKind.EnumKeyword
      case _ if matches("false") => TokenKind.FalseKeyword
      case _ if matches("fix") => TokenKind.FixKeyword
      case _ if matches("force") => TokenKind.ForceKeyword
      case _ if matches("if") => TokenKind.IfKeyword
      case _ if matches("import") => TokenKind.ImportKeyword
      case _ if matches("inline") => TokenKind.InlineKeyword
      case _ if matches("instance") => TokenKind.InstanceKeyword
      case _ if matches("into") => TokenKind.IntoKeyword
      case _ if matches("lat") => TokenKind.LatKeyword
      case _ if matches("law") => TokenKind.LawKeyword
      case _ if matches("lawful") => TokenKind.LawfulKeyword
      case _ if matches("lazy") => TokenKind.LazyKeyword
      case _ if matches("let") => TokenKind.LetKeyword
      case _ if matches("match") => TokenKind.MatchKeyword
      case _ if matches("namespace") => TokenKind.NamespaceKeyword
      case _ if matches("null") => TokenKind.NullKeyword
      case _ if matches("opaque") => TokenKind.OpaqueKeyword
      case _ if matches("override") => TokenKind.OverrideKeyword
      case _ if matches("pub") => TokenKind.PubKeyword
      case _ if matches("ref") => TokenKind.RefKeyword
      case _ if matches("region") => TokenKind.RegionKeyword
      case _ if matches("reify") => TokenKind.ReifyKeyword
      case _ if matches("reifyBool") => TokenKind.ReifyBoolKeyword
      case _ if matches("reifyEff") => TokenKind.ReifyEffKeyword
      case _ if matches("reifyType") => TokenKind.ReifyTypeKeyword
      case _ if matches("rel") => TokenKind.RelKeyword
      case _ if matches("sealed") => TokenKind.SealedKeyword
      case _ if matches("set") => TokenKind.SetKeyword
      case _ if matches("spawn") => TokenKind.SpawnKeyword
      case _ if matches("Static") => TokenKind.StaticKeyword
      case _ if matches("true") => TokenKind.TrueKeyword
      case _ if matches("type") => TokenKind.TypeKeyword
      case _ if matches("use") => TokenKind.UseKeyword
      case _ if matches("where") => TokenKind.WhereKeyword
      case _ if matches("with") => TokenKind.WithKeyword
      case _ if matches("discard") => TokenKind.DiscardKeyword
      case _ if matches("object") => TokenKind.ObjectKeyword

      case c if c.isLetter => name()
      case c if c.isDigit => number()
      case '\"' => string()

      case _ => TokenKind.Err
    }

    addToken(kind)
  }

  private def addToken(k: TokenKind)(implicit s: State): Unit = {
    val t = s.src.data.slice(s.start, s.current).mkString("")
    val c = s.column - t.length // get the starting column
    s.tokens += Token(k, t, s.line, c)
    s.start = s.current
  }

  private def matches(k: String)(implicit s: State): Boolean = {
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

  private def name()(implicit s: State): TokenKind = TokenKind.Name

  private def string()(implicit s: State): TokenKind = TokenKind.String

  private def number()(implicit s: State): TokenKind = TokenKind.Integer

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
