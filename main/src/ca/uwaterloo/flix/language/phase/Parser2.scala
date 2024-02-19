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
import org.parboiled2.ParserInput

// TODO: Add change set support

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
    // Compute a `ParserInput` when initializing a state for lexing a source.
    // This is necessary to display source code in error messages.
    // See `sourceLocationAtStart` for usage and `SourceLocation` for more information.
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  private sealed trait Mark

  private object Mark {
    case class Opened(index: Int) extends Mark

    case class Closed(index: Int) extends Mark
  }

  def run(root: Map[Ast.Source, Array[Token]])(implicit flix: Flix): Validation[Map[Ast.Source, Tree], CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.Failure(LazyList.empty)
    }

    flix.phase("Parser2") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(root) {
        case (src, tokens) => mapN(parse(src, tokens))(trees => src -> trees)
      }

      mapN(sequence(results))(_.toMap)
    }
  }

  private def parse(src: Ast.Source, ts: Array[Token]): Validation[Tree, CompilationMessage] = {
    implicit val s: State = new State(ts, src)
    source()
    val tree = buildTree()

    println(tree.toDebugString())

    if (s.errors.length > 0) {
      Validation.Failure(LazyList.from(s.errors))
    } else {
      tree.toSuccess
    }
  }

  private def buildTree()(implicit s: State): Tree = {
    val tokens = s.tokens.iterator.buffered
    var stack: List[Tree] = List.empty
    var locationStack: List[Token] = List.empty

    // Pop the last event, which must be a Close,
    // to ensure that the stack is not empty when handling event below.
    val lastEvent = s.events.last
    s.events = s.events.dropRight(1)
    assert(lastEvent match {
      case Event.Close => true
      case _ => false
    })

    // NB, make a synthetic token to begin with, to make the SourceLocations
    // generated below be correct.
    var lastAdvance = Token(TokenKind.Eof, s.src.data, 0, 0, 0, 0, 0, 0)
    for (event <- s.events) {
      event match {
        case Event.Open(kind) =>
          locationStack = locationStack :+ tokens.head
          stack = stack :+ Tree(kind, SourceLocation.Unknown, Array.empty)

        case Event.Close =>
          val child = Child.Tree(stack.last)
          val openToken = locationStack.last
          stack.last.loc = if (stack.last.children.length == 0)
          // If the subtree has no children, give it a zero length position just after the last token
            SourceLocation.mk(
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput)),
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput))
            )
          else {
            // Otherwise the source location can span from the first to the last token in the sub tree
            SourceLocation.mk(
              openToken.mkSourcePosition(s.src, Some(s.parserInput)),
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput))
            )
          }
          locationStack = locationStack.dropRight(1)
          stack = stack.dropRight(1)
          stack.last.children = stack.last.children :+ child

        case Event.Advance =>
          val token = tokens.next()
          lastAdvance = token
          stack.last.children = stack.last.children :+ Child.Token(token)
      }
    }

    // Set source location of the root
    val openToken = locationStack.last
    stack.last.loc = SourceLocation.mk(
      openToken.mkSourcePosition(s.src, Some(s.parserInput)),
      tokens.head.mkSourcePositionEnd(s.src, Some(s.parserInput))
    )

    // The stack should now contain a single Source tree,
    // and there should only be an <eof> token left.
    assert(stack.length == 1)
    assert(tokens.next().kind == TokenKind.Eof)

    stack.head
  }

  /**
   * A helper function for turning the current position of the parser as a `SourceLocation`
   */
  private def currentSourceLocation()(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val token = s.tokens(s.position)
    val line = token.line + 1
    val column = token.col + 1
    SourceLocation(Some(s.parserInput), s.src, SourceKind.Real, line, column, line, column + token.text.length)
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
    if (eof()) {
      return
    }
    s.fuel = 256
    s.events = s.events :+ Event.Advance
    s.position += 1
  }

  private def advanceWithError(error: Parse2Error)(implicit s: State): Mark.Closed = {
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
      case Some(Token(kind, _, _, _, _, _, _, _)) => kind
      case None => TokenKind.Eof
    }
  }


  private def at(kind: TokenKind)(implicit s: State): Boolean = {
    nth(0) == kind
  }

  // next skips comments and annotations. That is useful when looking for a declaration start
  private def next(kind: TokenKind)(implicit s: State): Boolean = {
    var lookAhead = 0
    while (true) {
      nth(lookAhead) match {
        case TokenKind.Eof => return false
        case TokenKind.CommentDoc
             | TokenKind.CommentLine
             | TokenKind.CommentBlock
             | TokenKind.Annotation =>
        case k => return kind == k
      }
      lookAhead += 1
    }
    false
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

  // A precedence table for binary operators, lower is higher precedence
  private def BINARY_PRECEDENCE: List[List[TokenKind]] = List(
    List(TokenKind.BackArrowThin), // TODO: This goes here?
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
  )

  // A precedence table for unary operators, lower is higher precedence
  private def UNARY_PRECEDENCE: List[List[TokenKind]] = List(
    List(TokenKind.Plus, TokenKind.Minus), // +, -, ~~~
    List(TokenKind.KeywordNot)
  )

  private def rightBindsTighter(left: TokenKind, right: TokenKind, leftIsUnary: Boolean): Boolean = {
    def unaryTightness(kind: TokenKind): Int = {
      // Unary operators all bind tighter than binary ones.
      // This is done by adding the length of the binary precedence table here:
      UNARY_PRECEDENCE.indexWhere(l => l.contains(kind)) + BINARY_PRECEDENCE.length
    }

    def tightness(kind: TokenKind): Int = {
      BINARY_PRECEDENCE.indexWhere(l => l.contains(kind))
    }

    val rt = tightness(right)
    if (rt == -1) {
      return false
    }
    val lt = if (leftIsUnary) unaryTightness(left) else tightness(left)
    if (lt == -1) {
      assert(left == TokenKind.Eof)
      return true
    }

    rt > lt
  }

  ////////// GRAMMAR ///////////////

  /**
   * source -> definition*
   */
  private def source()(implicit s: State): Unit = {
    val mark = open()
    while (!eof()) {
      if (next(TokenKind.KeywordDef)) {
        definition()
      } else {
        advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected definition, found ${nth(0)}"))
      }
    }

    close(mark, TreeKind.Source)
  }

  /**
   * definition -> docComment? annotations? 'def' name parameters ':' ttype '=' expression (';' expression)*
   */
  private def definition()(implicit s: State): Mark.Closed = {
    val mark = open()
    docComment()
    annotations()
    expect(TokenKind.KeywordDef)
    nameDefinition()
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

  private def annotations()(implicit s: State): Mark.Closed = {
    val mark = open()
    while (at(TokenKind.Annotation) && !eof()) {
      advance()
    }
    close(mark, TreeKind.Annotations)
  }

  private def docComment()(implicit s: State): Mark.Closed = {
    val mark = open()
    while (at(TokenKind.CommentDoc) && !eof()) {
      advance()
    }
    close(mark, TreeKind.Doc)
  }

  /**
   * parameters -> '(' (parameter (',' parameter)* )? ')'
   */
  private def parameters()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    var continue = true
    while (continue && !at(TokenKind.ParenR) && !eof()) {
      if (atAny(List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore))) {
        parameter()
      } else {
        continue = false
      }
    }

    if (at(TokenKind.Comma)) {
      advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "Trailing comma."))
    }

    expect(TokenKind.ParenR)
    close(mark, TreeKind.Parameters)
  }

  /**
   * parameter -> expression ':' ttype
   */
  private def parameter()(implicit s: State): Mark.Closed = {
    val mark = open()
    nameParameter()
    expect(TokenKind.Colon)
    ttype()
    if (at(TokenKind.Comma)) {
      advance()
    }
    close(mark, TreeKind.Parameter)
  }

  /**
   * expression -> TODO
   */
  private def expression(left: TokenKind = TokenKind.Eof, leftIsUnary: Boolean = false)(implicit s: State): Mark.Closed = {
    val mark = open()
    var lhs = exprDelimited()
    while (at(TokenKind.ParenL)) {
      val mark = openBefore(lhs)
      arguments()
      lhs = close(mark, TreeKind.Expr.Call)
    }

    var continue = true
    while (continue) {
      val right = nth(0)
      if (rightBindsTighter(left, right, leftIsUnary)) {
        val mark = openBefore(lhs)
        advance()
        expression(right)
        lhs = close(mark, TreeKind.Expr.Binary)
      } else {
        continue = false
      }
    }

    close(mark, TreeKind.Expr.Expr)
  }

  /**
   * arguments -> '(' (argument (',' argument)* )? ')'
   */
  private def arguments()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    while (!at(TokenKind.ParenR) && !eof()) {
      argument()
    }
    expect(TokenKind.ParenR)
    close(mark, TreeKind.Arguments)
  }

  /**
   * argument -> expression
   */
  private def argument()(implicit s: State): Mark.Closed = {
    val mark = open()
    expression()
    if (!at(TokenKind.ParenR)) {
      expect(TokenKind.Comma)
    }
    close(mark, TreeKind.Argument)
  }

  private def exprDelimited()(implicit s: State): Mark.Closed = {
    // Handle clearly delimited expressions
    nth(0) match {
      case TokenKind.ParenL => exprParen()
      case TokenKind.CurlyL => exprBlock()
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
           | TokenKind.KeywordFalse => exprLiteral()
      case TokenKind.NameLowerCase
           | TokenKind.NameUpperCase
           | TokenKind.NameMath
           | TokenKind.NameGreek => exprName()
      case TokenKind.Minus
           | TokenKind.KeywordNot
           | TokenKind.Plus
           | TokenKind.TripleTilde => exprUnary()
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected expression, found $t"))
    }
  }

  /**
   * exprLiteral -> integer | float | boolean
   */
  private def exprLiteral()(implicit s: State): Mark.Closed = {
    val mark = open()
    advance()
    close(mark, TreeKind.Expr.Literal)
  }

  /**
   * exprName -> Name.Definition ('.' Name.Definition)*
   */
  private def exprName()(implicit s: State): Mark.Closed = {
    val first = nameDefinition()
    var wasQualified = false
    while (eat(TokenKind.Dot) && !eof()) {
      wasQualified = true
      nameDefinition()
    }

    if (wasQualified) {
      val mark = openBefore(first)
      close(mark, TreeKind.QName)
    } else {
      first
    }
  }

  /**
   * exprParen -> '(' expression? ')'
   */
  private def exprParen()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    if (eat(TokenKind.ParenR)) { // Handle unit literal `()`
      return close(mark, TreeKind.Expr.Literal)
    }
    expression()
    expect(TokenKind.ParenR)
    close(mark, TreeKind.Expr.Paren)
  }

  /**
   * exprBlock -> '{' statement? '}'
   */
  private def exprBlock()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.CurlyL))
    val mark = open()
    expect(TokenKind.CurlyL)
    if (eat(TokenKind.CurlyR)) { // Handle empty block
      return close(mark, TreeKind.Expr.Block)
    }
    statement()
    expect(TokenKind.CurlyR)
    close(mark, TreeKind.Expr.Block)
  }

  /**
   * statement -> expression (';' statement)?
   */
  private def statement()(implicit s: State): Mark.Closed = {
    val lhs = expression()
    var last = lhs
    while (eat(TokenKind.Semi)) {
      val mark = openBefore(lhs)
      close(mark, TreeKind.Statement)
      last = expression()
    }
    last
  }

  private def exprUnary()(implicit s: State): Mark.Closed = {
    val mark = open()
    val op = nth(0)
    expectAny(List(TokenKind.Minus, TokenKind.KeywordNot, TokenKind.Plus, TokenKind.TripleTilde))
    expression(left = op, leftIsUnary = true)
    close(mark, TreeKind.Expr.Unary)
  }

  /////////// TYPES //////////////

  /**
   * ttype -> (typeDelimited typeArguments? | typeFunction) ( '\' effectSet )?
   */
  private def ttype()(implicit s: State): Mark.Closed = {
    val mark = open()
    val lhs = typeDelimited()

    // handle Type argument application
    if (at(TokenKind.BracketL)) {
      typeArguments()
    }

    // Handle function types
    if (at(TokenKind.Arrow)) {
      val mark = openBefore(lhs)
      ttype()
      if (at(TokenKind.Slash)) {
        effectSet()
      }
      close(mark, TreeKind.Type.Function)
    }

    // Handle effect
    if (at(TokenKind.Backslash)) {
      effectSet()
    }

    close(mark, TreeKind.Type.Type)
  }

  /**
   * effectSet -> '\' effect | '{' ( effect ( ',' effect )* )? '}'
   */
  private def effectSet()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.Backslash))
    val mark = open()
    expect(TokenKind.Backslash)
    if (eat(TokenKind.CurlyL)) {
      while (!at(TokenKind.CurlyR) && !eof()) {
        effect()
        if (!at(TokenKind.CurlyR)) {
          expect(TokenKind.Comma)
        }
      }
      expect(TokenKind.CurlyR)
    } else {
      effect()
    }

    close(mark, TreeKind.Type.Effect)
  }

  private def effect()(implicit s: State): Mark.Closed = {
    nth(0) match {
      case TokenKind.NameUpperCase => nameEffect()
      case TokenKind.NameLowerCase => nameVariable()
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected effect, found $t"))
    }
  }

  /**
   * typeArguments -> '[' ( argument ( ',' argument )* ) ']'?
   */
  private def typeArguments()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.BracketL))
    val mark = open()
    expect(TokenKind.BracketL)
    while (!at(TokenKind.BracketR) && !eof()) {
      typeArgument()
    }
    expect(TokenKind.BracketR)
    close(mark, TreeKind.Type.Arguments)
  }

  /**
   * typeArgument -> ttype
   */
  private def typeArgument()(implicit s: State): Mark.Closed = {
    val mark = open()
    ttype()
    if (!at(TokenKind.BracketR)) {
      expect(TokenKind.Comma)
    }
    close(mark, TreeKind.Type.Argument)
  }

  /**
   * typeDelimited -> typeRecord | typeTuple | typeName | typeVariable
   * Detects clearly delimited types such as names, records and tuples
   */
  private def typeDelimited()(implicit s: State): Mark.Closed = {
    nth(0) match {
      // TODO: Do we need Primary.True and Primary.False ?
      case TokenKind.CurlyL => typeRecord()
      case TokenKind.ParenL => typeTuple()
      case TokenKind.NameUpperCase => typeName()
      case TokenKind.NameLowerCase
           | TokenKind.NameMath
           | TokenKind.NameGreek
           | TokenKind.Underscore => nameVariable()
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected type, found $t"))
    }
  }

  /**
   * typeName -> NameType | typeQualifiedName
   * A type name, type variable or a qualified type.
   * IE. `Map`, `MyModule.MyEnum`.
   */
  private def typeName()(implicit s: State): Mark.Closed = {
    val first = nameType()
    var wasQualified = false
    while (eat(TokenKind.Dot) && !eof()) {
      wasQualified = true
      nameType()
    }

    if (wasQualified) {
      val mark = openBefore(first)
      close(mark, TreeKind.QName)
    } else {
      first
    }
  }

  /**
   * typeTuple -> '(' (type (',' type)* )? ')'
   */
  private def typeTuple()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    while (!at(TokenKind.ParenR) && !eof()) {
      ttype()
      if (at(TokenKind.Comma)) {
        advance()
      }
    }

    if (at(TokenKind.Comma)) {
      advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "Trailing comma."))
    }
    expect(TokenKind.ParenR)
    close(mark, TreeKind.Type.Tuple)
  }

  /**
   * typeRecord -> '{' (typeRecordField (',' typeRecordField)* )? ('|' Name.Variable)| '}'
   */
  private def typeRecord()(implicit s: State): Mark.Closed = {
    // TODO: What does RecordRow do?

    assert(at(TokenKind.CurlyL))
    val mark = open()
    expect(TokenKind.CurlyL)
    while (!atAny(List(TokenKind.CurlyR, TokenKind.Bar)) && !eof()) {
      typeRecordField()
    }

    if (at(TokenKind.Comma)) {
      advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "Trailing comma."))
    }

    if (at(TokenKind.Bar)) {
      val mark = open()
      expect(TokenKind.Bar)
      nameVariable()
      close(mark, TreeKind.Type.RecordVariable)
    }

    expect(TokenKind.CurlyR)
    close(mark, TreeKind.Type.Record)
  }

  /**
   * typeRecordField -> Names.Field '=' ttype
   */
  private def typeRecordField()(implicit s: State): Mark.Closed = {
    val mark = open()
    nameField()
    expect(TokenKind.Equal)
    ttype()
    if (!atAny(List(TokenKind.CurlyR, TokenKind.Bar))) {
      expect(TokenKind.Comma)
    }
    close(mark, TreeKind.Type.RecordField)
  }

  /////// NAMES ////////////
  private def nameDefinition()(implicit s: State): Mark.Closed = {
    val mark = open()
    expectAny(List(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek))
    close(mark, TreeKind.Ident)
  }

  private def nameParameter()(implicit s: State): Mark.Closed = {
    val mark = open()
    expectAny(List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore))
    close(mark, TreeKind.Ident)
  }

  private def nameVariable()(implicit s: State): Mark.Closed = {
    val mark = open()
    expectAny(List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore))
    close(mark, TreeKind.Ident)
  }

  private def nameField()(implicit s: State): Mark.Closed = {
    val mark = open()
    expect(TokenKind.NameLowerCase)
    close(mark, TreeKind.Ident)
  }

  private def nameType()(implicit s: State): Mark.Closed = {
    val mark = open()
    expect(TokenKind.NameUpperCase)
    close(mark, TreeKind.Ident)
  }

  private def nameEffect()(implicit s: State): Mark.Closed = {
    val mark = open()
    expect(TokenKind.NameUpperCase)
    close(mark, TreeKind.Ident)
  }
}
