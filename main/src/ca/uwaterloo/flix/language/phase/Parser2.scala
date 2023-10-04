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
import ca.uwaterloo.flix.language.ast.UnstructuredTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, SourceKind, SourceLocation, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

// TODO: How to handle errors?
// An ErrorTree might wrap a ParserError -> Then the tree needs to be traversed to gather errors
// An ErrorTree might have an index pointing into a separate error list maintained in State.

object Parser2 {

  private sealed trait Event

  private object Event {
    case class Open(kind: TreeKind) extends Event

    case object Close extends Event

    case object Advance extends Event
  }

  private class State(val tokens: Array[Token], val src: Ast.Source) {
    var position: Int = 0
    var fuel: Int = 256
    var events: Array[Event] = Array.empty
    var errors: Array[Parse2Error] = Array.empty
  }

  private sealed trait Mark

  private object Mark {
    case class Opened(index: Int) extends Mark

    case class Closed(index: Int) extends Mark
  }

  private object Name {
    val Definition: List[TokenKind] = List(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek)
    val Parameter: List[TokenKind] = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)

  }

  def run(root: Map[Ast.Source, Array[Token]])(implicit flix: Flix): Validation[Map[Ast.Source, Tree], CompilationMessage] = {
    if (!flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Map.empty[Ast.Source, Tree].toSuccess
    }

    flix.phase("Parser2") {
      // Parse each source file in parallel.
      val results = ParOps.parMap(root) {
        case (src, tokens) => mapN(parse(src, tokens))({
          case trees => src -> trees
        })
      }

      // Construct a map from each source to its tokens.
      mapN(sequence(results))(_.toMap)
    }
  }

  def f(tree: Tree): WeededAst.Declaration.Def = {
    tree match {
      case Tree(TreeKind.Def, children) =>
    }
  }

  private def parse(src: Ast.Source, ts: Array[Token]): Validation[Tree, CompilationMessage] = {
    implicit val s: State = new State(ts, src)
    source()
    val tree = buildTree()

    //    println(s.events.mkString("\n"))
    println(tree.toDebugString())


    if (s.errors.length > 0) {
      Validation.SoftFailure(tree, LazyList.from(s.errors))
    } else {
      tree.toSuccess
    }
  }

  private def buildTree()(implicit s: State): Tree = {
    val tokens = s.tokens.iterator
    var stack: List[Tree] = List.empty

    // Pop the last event, which must be a Close,
    // to ensure that the stack is not empty when handling event below.
    val lastEvent = s.events.last
    s.events = s.events.dropRight(1)
    assert(lastEvent match {
      case Event.Close => true
      case _ => false
    })

    for (event <- s.events) {
      event match {
        case Event.Open(kind) =>
          stack = stack :+ Tree(kind, Array.empty)

        case Event.Close =>
          val child = Child.Tree(stack.last)
          stack = stack.dropRight(1)
          stack.last.children = stack.last.children :+ child

        case Event.Advance =>
          val token = tokens.next()
          stack.last.children = stack.last.children :+ Child.Token(token)
      }
    }

    // The stack should now contain a single Source tree,
    // and there should only be an <eof> token left.
    assert(stack.length == 1)
    assert(tokens.next() match {
      case Token(TokenKind.Eof, _, _, _, _, _) => true
      case _ => false
    })

    stack.head
  }

  private def open()(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    s.events = s.events :+ Event.Open(TreeKind.ErrorTree(-1))
    mark
  }

  private def close(mark: Mark.Opened, kind: TreeKind)(implicit s: State): Unit = {
    s.events(mark.index) = Event.Open(kind)
    s.events = s.events :+ Event.Close
  }

  private def advance()(implicit s: State): Unit = {
    assert(!eof())
    s.fuel = 256
    s.events = s.events :+ Event.Advance
    s.position += 1
  }

  private def eof()(implicit s: State): Boolean = {
    s.position == s.tokens.length - 1
  }

  private def nth(lookahead: Int)(implicit s: State): TokenKind = {
    if (s.fuel == 0) {
      // TODO: How to properly throw a compiler level error?
      throw new Error("parser is stuck")
    }

    s.fuel -= 1
    s.tokens.lift(s.position + lookahead) match {
      case Some(Token(kind, _, _, _, _, _)) => kind
      case None => TokenKind.Eof
    }
  }

  private def at(kind: TokenKind)(implicit s: State): Boolean = {
    nth(0) == kind
  }

  private def atAny(kinds: List[TokenKind])(implicit s: State): Boolean = {
    kinds.contains(nth(0))
  }

  private def eat(kind: TokenKind)(implicit s: State): Boolean = {
    if (at(kind)) {
      advance()
      true
    } else {
      false
    }
  }

  private def eatAny(kinds: List[TokenKind])(implicit s: State): Boolean = {
    if (atAny(kinds)) {
      advance()
      true
    } else {
      false
    }
  }

  private def expect(kind: TokenKind)(implicit s: State): Unit = {
    if (!eat(kind)) {
      val error = expectedError(kind)
      s.errors = s.errors :+ error
    }
  }

  private def expectAny(kinds: List[TokenKind])(implicit s: State): Unit = {
    if (!eatAny(kinds)) {
      val error = expectedAnyError(kinds)
      s.errors = s.errors :+ error
    }
  }

  private def advanceWithError(error: Parse2Error)(implicit s: State): Unit = {
    val mark = open()
    advance()
    closeWithError(mark, error)
  }

  private def closeWithError(mark: Mark.Opened, error: Parse2Error)(implicit s: State): Unit = {
    s.errors = s.errors :+ error
    close(mark, TreeKind.ErrorTree(s.errors.length - 1))
  }

  private def expectedError(kind: TokenKind)(implicit s: State): Parse2Error = {
    val token = s.tokens(s.position)
    val loc = SourceLocation(None, s.src, SourceKind.Real, token.line, token.col, token.line, token.col + token.text.length)
    Parse2Error.UnexpectedToken(loc, kind)
  }

  private def expectedAnyError(kinds: List[TokenKind])(implicit s: State): Parse2Error = {
    val token = s.tokens(s.position)
    val loc = SourceLocation(None, s.src, SourceKind.Real, token.line, token.col, token.line, token.col + token.text.length)
    Parse2Error.UnexpectedToken(loc, kinds(0)) // TODO: Fix this
  }

  ////////// GRAMMAR ///////////////
  private def source()(implicit s: State): Unit = {
    val mark = open()
    while (!eof()) {
      if (at(TokenKind.KeywordDef)) {
        definition()
      } else {
        advanceWithError(expectedError(TokenKind.KeywordDef))
      }
    }

    close(mark, TreeKind.Source)
  }

  // definition -> 'def' name parameters ':' ttype '=' expression (';' expression)*
  private def definition()(implicit s: State): Unit = {
    assert(at(TokenKind.KeywordDef))
    val mark = open()
    expect(TokenKind.KeywordDef)
    expectAny(Name.Definition)
    if (at(TokenKind.ParenL)) {
      parameters()
    }

    expect(TokenKind.Colon)
    ttype()
    expect(TokenKind.Equal)

    expression()
    while (at(TokenKind.Semi) && !eof()) {
      expect(TokenKind.Semi)
      expression()
    }
    close(mark, TreeKind.Def)
  }

  private def parameters()(implicit s: State): Unit = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    var continue = true
    while (continue && !at(TokenKind.ParenR) && !eof()) {
      if (atAny(Name.Parameter)) {
        parameter()
      } else {
        continue = false
      }
    }

    if (at(TokenKind.Comma)) {
      //TODO: trailing comma error
      println("error: trailing comma")
    }

    expect(TokenKind.ParenR)
    close(mark, TreeKind.Parameters)
  }

  private def parameter()(implicit s: State): Unit = {
    assert(atAny(Name.Parameter))
    val mark = open()
    expectAny(Name.Parameter)
    expect(TokenKind.Colon)
    ttype()
    if (at(TokenKind.Comma)) {
      advance()
    }
    close(mark, TreeKind.Parameter)
  }

  // expression ->
  private def expression()(implicit s: State): Unit = {
    val mark = open()
    // Handle clearly delimited expressions
    nth(0) match {
      case TokenKind.LiteralString
           | TokenKind.LiteralChar
           | TokenKind.LiteralFloat32
           | TokenKind.LiteralFloat64
           | TokenKind.LiteralBigDecimal
           | TokenKind.LiteralInt8
           | TokenKind.LiteralInt16
           | TokenKind.LiteralInt32
           | TokenKind.LiteralInt64
           | TokenKind.LiteralBigInt =>
        advance()
        close(mark, TreeKind.ExprLiteral)
      case TokenKind.NameUpperCase | TokenKind.NameLowerCase | TokenKind.NameGreek | TokenKind.NameMath =>
        advance()
        close(mark, TreeKind.ExprName)
      case TokenKind.ParenL =>
        expect(TokenKind.ParenL)
        expression()
        expect(TokenKind.ParenR)
        close(mark, TreeKind.ExprParen)
      case _ =>
        if (!eof()) {
          advance()
        }
        close(mark, TreeKind.ErrorTree(-1))
    }
  }

  private def ttype()(implicit s: State): Unit = {
    val mark = open()
    expect(TokenKind.NameUpperCase) // TODO: a whole lot of parsing
    close(mark, TreeKind.ExprType)
  }
}
