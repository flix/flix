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
import ca.uwaterloo.flix.language.ast.UnstructuredTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, SourceKind, SourceLocation, Token, TokenKind}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.annotation.tailrec

// TODO: Add change set support

// TODO: Add a way to transform Tree into a weededAst


/**
 * Errors reside both within the produced `Tree` but are also kept in an array in `state.errors`
 * to make it easy to return them as a `CompilationMessage` after parsing.
 */
object Parser2 {

  private sealed trait Event

  private object Event {
    // TODO: Make `openBefore` links
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
    val Variable: List[TokenKind] = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
    val Type: List[TokenKind] = List(TokenKind.NameUpperCase)
  }

  def run(root: Map[Ast.Source, Array[Token]])(implicit flix: Flix): Validation[Map[Ast.Source, Tree], CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Map.empty[Ast.Source, Tree].toSuccess
    }

    flix.phase("Parser2") {
      // Parse each source file in parallel.
      val results = ParOps.parMap(root) {
        case (src, tokens) => mapN(parse(src, tokens))(trees => src -> trees)
      }

      // Construct a map from each source to its tokens.
      mapN(sequence(results))(_.toMap)
    }
  }

  private def parse(src: Ast.Source, ts: Array[Token]): Validation[Tree, CompilationMessage] = {
    implicit val s: State = new State(ts, src)
    source()
    val tree = buildTree()

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

  /**
   * A helper function for turning the current position of the parser as a `SourceLocation`
   */
  private def currentSourceLocation()(implicit s: State): SourceLocation = {
    val token = s.tokens(s.position)
    SourceLocation(None, s.src, SourceKind.Real, token.line, token.col, token.line, token.col + token.text.length)
  }

  private def open()(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    s.events = s.events :+ Event.Open(TreeKind.ErrorTree(Parse2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))
    mark
  }

  private def close(mark: Mark.Opened, kind: TreeKind)(implicit s: State): Mark.Closed = {
    s.events(mark.index) = Event.Open(kind)
    s.events = s.events :+ Event.Close
    Mark.Closed(mark.index)
  }

  private def openBefore(before: Mark.Closed)(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(before.index)
    s.events = s.events.patch(before.index, Array(Event.Open(TreeKind.ErrorTree(Parse2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))), 0)
    mark
  }

  private def closeWithError(mark: Mark.Opened, error: Parse2Error)(implicit s: State): Mark.Closed = {
    s.errors = s.errors :+ error
    close(mark, TreeKind.ErrorTree(error))
  }

  private def advance()(implicit s: State): Unit = {
    assert(!eof())
    s.fuel = 256
    s.events = s.events :+ Event.Advance
    s.position += 1
  }

  private def advanceWithError(error: Parse2Error)(implicit s: State): Unit = {
    val mark = open()
    advance()
    closeWithError(mark, error)
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
      val mark = open()
      val error = Parse2Error.UnexpectedToken(currentSourceLocation(), kind)
      closeWithError(mark, error)
    }
  }

  private def expectAny(kinds: List[TokenKind])(implicit s: State): Unit = {
    if (!eatAny(kinds)) {
      val mark = open()
      val error = Parse2Error.DevErr(currentSourceLocation(), s"Expected one of ${kinds.mkString(", ")}")
      closeWithError(mark, error)
    }
  }

  ////////// GRAMMAR ///////////////
  private def source()(implicit s: State): Unit = {
    val mark = open()
    while (!eof()) {
      if (at(TokenKind.KeywordDef)) {
        definition()
      } else {
        advanceWithError(Parse2Error.UnexpectedToken(currentSourceLocation(), TokenKind.KeywordDef))
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
  private def expression(left: TokenKind = TokenKind.Eof)(implicit s: State): Mark.Closed = {
    var lhs = exprDelimited()
    while (at(TokenKind.ParenL)) {
      val mark = openBefore(lhs)
      argList()
      lhs = close(mark, TreeKind.ExprCall)
    }

    var continue = true
    while (continue) {
      val right = nth(0)
      if (rightBindsTighter(left, right)) {
        val mark = openBefore(lhs)
        advance()
        expression(right)
        lhs = close(mark, TreeKind.ExprBinary)
      } else {
        continue = false
      }
    }

    lhs
  }

  private def rightBindsTighter(left: TokenKind, right: TokenKind): Boolean = {
    def tightness(kind: TokenKind): Int = {
      // A precedence table, lower is higher precedence
      List(
        List(TokenKind.BackArrow), // TODO: This goes here?
        List(TokenKind.KeywordOr),
        List(TokenKind.KeywordAnd),
        List(TokenKind.TripleBar), // |||
        List(TokenKind.TripleCaret), // ^^^
        List(TokenKind.TripleAmpersand), // &&&
        List(TokenKind.EqualEqual, TokenKind.AngledEqual), // ==, <=>  // TODO: !=
        List(TokenKind.AngleL, TokenKind.AngleR, TokenKind.AngleLEqual, TokenKind.AngleREqual), // <, >, <=, >=
        List(TokenKind.TripleAngleL, TokenKind.TripleAngleR), // <<<, >>>
        List(TokenKind.Plus, TokenKind.Minus), // +, -
        List(TokenKind.Star, TokenKind.StarStar, TokenKind.Slash, TokenKind.KeywordMod), // *, **, /, mod // TODO: rem?
        List(TokenKind.AngledPlus), // <+>
        List(TokenKind.InfixFunction), // `my_function`
        List(TokenKind.UserDefinedOperator, TokenKind.NameMath) // +=+
      ).indexWhere(l => l.contains(kind))
    }

    val rt = tightness(right)
    if (rt == -1) {
      return false
    }
    val lt = tightness(left)
    if (lt == -1) {
      assert(left == TokenKind.Eof)
      return true
    }

    rt > lt
  }

  private def argList()(implicit s: State): Unit = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    while (!at(TokenKind.ParenR) && !eof()) {
      arg()
    }
    expect(TokenKind.ParenR)
    close(mark, TreeKind.Arguments)
  }

  private def arg()(implicit s: State): Unit = {
    val mark = open()
    expression()
    if (!at(TokenKind.ParenR)) {
      expect(TokenKind.Comma)
    }
    close(mark, TreeKind.Argument)
  }

  private def exprDelimited()(implicit s: State): Mark.Closed = {
    // Handle clearly delimited expressions
    val mark = open()

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
           | TokenKind.LiteralBigInt
           | TokenKind.KeywordTrue
           | TokenKind.KeywordFalse =>
        advance()
        close(mark, TreeKind.ExprLiteral)
      case t if Name.Definition.contains(t) =>
        // TODO: Check for qualified name
        advance()
        close(mark, TreeKind.ExprName)
      case TokenKind.ParenL =>
        close(mark, exprParen())
      case TokenKind.CurlyL =>
        exprBlock()
        close(mark, TreeKind.ExprBlock)
      case TokenKind.Minus | TokenKind.KeywordNot | TokenKind.Plus | TokenKind.TripleTilde =>
        advance()
        exprDelimited()
        close(mark, TreeKind.ExprUnary)
      case _ =>
        if (!eof()) {
          advance()
        }
        closeWithError(mark, Parse2Error.DevErr(currentSourceLocation(), "Expected expression."))
    }
  }

  @tailrec
  private def statement()(implicit s: State): Unit = {
    val lhs = expression()
    if (eat(TokenKind.Semi)) {
      val mark  = openBefore(lhs)
      close(mark, TreeKind.Statement)
      statement()
    }
  }

  private def exprParen()(implicit s: State): TreeKind = {
    assert(at(TokenKind.ParenL))
    expect(TokenKind.ParenL)
    if (eat(TokenKind.ParenR)) { // Handle unit literal `()`
      return TreeKind.ExprLiteral
    }
    expression()
    expect(TokenKind.ParenR)
    TreeKind.ExprParen
  }

  private def exprBlock()(implicit s: State): Unit = {
    assert(at(TokenKind.CurlyL))
    expect(TokenKind.CurlyL)
    if (eat(TokenKind.CurlyR)) { // Handle empty block
      return
    }
    statement()
    expect(TokenKind.CurlyR)
  }

  /////////// TYPES //////////////
  private def ttype()(implicit s: State): Unit = {
    val mark = open()
    typePrimary()
    close(mark, TreeKind.ExprType)
  }

  private def typePrimary()(implicit s: State): Unit = {
    nth(0) match {
      // TODO: Do we need Primary.True and Primary.False ?

      case TokenKind.ParenL =>
        val mark = open()
        typeList()
        close(mark, TreeKind.TypeTuple)

      case t if (Name.Variable ++ Name.Type).contains(t) =>
        val mark = open()
        advance()
        if (nth(0) == TokenKind.Dot) {
          typeQualified()
          close(mark, TreeKind.TypeQualified)
        } else {
          val kind = if (Name.Type.contains(t)) TreeKind.TypeName else TreeKind.TypeVariable
          close(mark, kind)
        }

      case _ =>
        val mark = open()
        if (!eof()) {
          advance()
        }
        closeWithError(mark, Parse2Error.DevErr(currentSourceLocation(), "Expected type."))
    }
  }

  private def typeList()(implicit s: State): Unit = {
    assert(at(TokenKind.ParenL))
    expect(TokenKind.ParenL)
    while (!at(TokenKind.ParenR) && !eof()) {
      ttype()
      if (at(TokenKind.Comma)) {
        advance()
      }
    }

    if (at(TokenKind.Comma)) {
      //TODO: trailing comma error
      println("error: trailing comma")
    }
    expect(TokenKind.ParenR)
  }

  // A helper for qualified names such as `List.map`.
  private def typeQualified()(implicit s: State): Unit = {
    assert(at(TokenKind.Dot))
    while (at(TokenKind.Dot) && !eof()) {
      expect(TokenKind.Dot)
      if (Name.Variable.contains(nth(0))) {
        advance()
      }
    }
    expectAny(Name.Type)
  }
}
