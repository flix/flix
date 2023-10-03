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
import ca.uwaterloo.flix.language.ast.{Ast, Token, TokenKind}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

object Parser2 {

  private sealed trait Event

  private object Event {
    case class Open(kind: TreeKind) extends Event

    case object Close extends Event

    case object Advance extends Event
  }

  private class State(ts: Array[Token]) {
    val tokens: Array[Token] = ts
    var position: Int = 0
    var fuel: Int = 256
    var events: Array[Event] = Array.empty
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
      ParOps.parTraverseValues(root)(parse)
    }
  }

  private def parse(ts: Array[Token]): Validation[Tree, CompilationMessage] = {
    implicit val s: State = new State(ts)
    source()
    println(s.events.mkString("\n"))

    val tree = buildTree()
    println(tree)

    tree.toSuccess
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

    assert(stack.length == 1)
    assert(!tokens.hasNext)
    stack.head
  }

  private def open()(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    s.events = s.events :+ Event.Open(TreeKind.ErrorTree)
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
    s.position == s.tokens.length
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
      //TODO: Do some error reporting here
    }
  }

  private def expectAny(kinds: List[TokenKind])(implicit s: State): Unit = {
    if (!eatAny(kinds)) {
      //TODO: Do some error reporting here
    }
  }

  // TODO: err should be some ParserError type
  private def advanceWithError(err: String)(implicit s: State): Unit = {
    val mark = open()
    println(err) // TODO: don't print here
    advance()
    close(mark, TreeKind.ErrorTree)
  }


  ////////// GRAMMAR ///////////////
  private def source()(implicit s: State): Unit = {
    val mark = open()
    while (!eof()) {
      if (at(TokenKind.KeywordDef)) {
        definition()
      } else {
        advanceWithError("expected a definition")
      }
    }

    close(mark, TreeKind.Source)
  }

  private def definition()(implicit s: State): Unit = {
    assert(at(TokenKind.KeywordDef))
    val mark = open()
    expect(TokenKind.KeywordDef)
    expectAny(Name.Definition)
    if (at(TokenKind.ParenL)) {
      parameters()
    }

    expect(TokenKind.Colon)
    expressionType()
    expect(TokenKind.Equal)
    expressionStatement()
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
    expressionType()
    if (at(TokenKind.Comma)) {
      advance()
    }
    close(mark, TreeKind.Parameter)
  }

  private def expressionType()(implicit s: State): Unit = {
    val mark = open()
    expect(TokenKind.NameUpperCase) // TODO: a whole lot of parsing
    close(mark, TreeKind.ExprType)
  }

  private def expressionStatement()(implicit s: State): Unit = {
    val mark = open()
    expect(TokenKind.NameUpperCase) // TODO: a whole lot of parsing
    close(mark, TreeKind.ExprStmt)
  }
}
