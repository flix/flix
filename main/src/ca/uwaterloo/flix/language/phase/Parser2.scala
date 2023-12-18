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
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, ParsedAst, ReadAst, SourceKind, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst}
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

  // A helper for development that will run the new parsing pipeline on each source
  // and fall back on the old parsing pipeline if there is a failure.
  def runWithFallback(
                       afterReader: ReadAst.Root,
                       afterLexer: Map[Ast.Source, Array[Token]],
                       entryPoint: Option[Symbol.DefnSym],
                       changeSet: ChangeSet
                     )(implicit flix: Flix): Validation[WeededAst.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.Failure(LazyList.empty)
    }


    flix.phase("Parser2") {
      // The file that is the current focus of development
      val DEBUG_FOCUS = ""

      println("p\tw\tfile")

      // For each file: If the parse was successful run Weeder2 on it.
      // If either the parse or the weed was bad pluck the WeededAst from the previous pipeline and use that.
      val afterParser = Parser.run(afterReader, entryPoint, ParsedAst.empty, changeSet)
      val afterWeeder = flatMapN(afterParser)(parsedAst => Weeder.run(parsedAst, WeededAst.empty, changeSet))

      def fallback(src: Ast.Source, errors: LazyList[CompilationMessage]): Validation[WeededAst.CompilationUnit, CompilationMessage] = {
        if (DEBUG_FOCUS == src.name) {
          // If the current file is the debug focus, actually report the errors from the parser/weeder
          flatMapN(afterWeeder)(tree => SoftFailure(tree.units(src), errors))
        } else {
          // Otherwise silently fallback on the old CompilationUnit.
          mapN(afterWeeder)(_.units(src))
        }
      }

      def diffWeededAsts(src: Ast.Source, newAst: WeededAst.CompilationUnit): Boolean = {
        // Print asts for closer inspection
        val matches = mapN(afterWeeder)(t => {
          val oldAst = t.units(src)
          if (src.name == DEBUG_FOCUS) {
            println("[[[ OLD PARSER ]]]")
            println(formatWeededAst(oldAst))
            println("[[[ NEW PARSER ]]]")
            println(formatWeededAst(newAst))
            println("[[[ END ]]]")
          }
          val hasSameStructure = formatWeededAst(oldAst, withLocs = false) == formatWeededAst(newAst, withLocs = false)
          hasSameStructure
        })

        matches match {
          case Success(isMatch) => isMatch
          case _ => false
        }
      }

      val results = ParOps.parMap(afterLexer) {
        case (src, tokens) =>
          var outString = ""
          val weededAst = parse(src, tokens) match {
            case Success(t) =>
              outString += s"${Console.GREEN}✔︎ ${Console.RESET}"
              if (DEBUG_FOCUS == src.name) {
                println(t.toDebugString())
              }
              Weeder2.weed(src, t) match {
                case Success(t) =>
                  val hasSameStructure = diffWeededAsts(src, t)
                  if (hasSameStructure) {
                    outString += s"\t${Console.GREEN}✔︎ ${Console.RESET}"
                  } else {
                    outString += s"\t${Console.YELLOW}!=${Console.RESET}"
                  }
                  t.toSuccess
                case SoftFailure(t, errors) =>
                  outString += s"\t${Console.YELLOW}✘ ${Console.RESET}"
                  diffWeededAsts(src, t)
                  fallback(src, errors)
                case Failure(errors) =>
                  outString += s"\t${Console.RED}✘ ${Console.RESET}"
                  fallback(src, errors)
              }
            case SoftFailure(t, errors) =>
              outString += s"${Console.YELLOW}✘ ${Console.RESET}\t-"
              if (DEBUG_FOCUS == src.name) {
                println(t.toDebugString())
              }
              fallback(src, errors)
            case Failure(errors) =>
              outString += s"${Console.RED}✘ ${Console.RESET}\t-"
              fallback(src, errors)
          }
          outString += s"\t${src.name}"
          println(outString)
          mapN(weededAst)(src -> _)
      }

      mapN(sequence(results))(_.toMap).map(m => WeededAst.Root(m, entryPoint, afterReader.names))
    }
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
    if (s.errors.length > 0) {
      Validation.SoftFailure(tree, LazyList.from(s.errors))
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
          else
          // Otherwise the source location can span from the first to the last token in the sub tree
            SourceLocation.mk(
              openToken.mkSourcePosition(s.src, Some(s.parserInput)),
              lastAdvance.mkSourcePositionEnd(s.src, Some(s.parserInput))
            )
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
    // TODO: Add these back in
    //    assert(stack.length == 1)
    //    assert(tokens.next().kind == TokenKind.Eof)

    stack.head
  }

  /**
   * A helper function for turning the current position of the parser as a `SourceLocation`
   */
  private def currentSourceLocation()(implicit s: State): SourceLocation = {
    // state is zero-indexed while SourceLocation works as one-indexed.
    val token = s.tokens(s.position)
    val line = token.beginLine + 1
    val column = token.beginCol + 1
    SourceLocation(Some(s.parserInput), s.src, SourceKind.Real, line, column, line, column + token.text.length)
  }

  private def open()(implicit s: State): Mark.Opened = {
    val mark = Mark.Opened(s.events.length)
    s.events = s.events :+ Event.Open(TreeKind.ErrorTree(Parse2Error.DevErr(currentSourceLocation(), "Unclosed parser mark")))
    // advance past any comments when opening a new mark.
    // This places the comments within the closest sub-tree.
    while (atAny(List(TokenKind.CommentLine, TokenKind.CommentBlock)) && !eof()) {
      advance()
    }
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
      case Some(t) => t.kind
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

  private def commaSeparated(kind: TreeKind, getItem: () => Mark.Closed, delimiters: (TokenKind, TokenKind) = (TokenKind.ParenL, TokenKind.ParenR), checkForItem: () => Boolean = () => true)(implicit s: State): Mark.Closed = {
    assert(at(delimiters._1))
    val mark = open()
    expect(delimiters._1)
    var continue = true
    while (continue && !at(delimiters._2) && !eof()) {
      if (checkForItem()) {
        getItem()
      } else {
        continue = false
      }
    }

    if (at(TokenKind.Comma)) {
      advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "Trailing comma."))
    }

    expect(delimiters._2)
    close(mark, kind)
  }

  private def asArgument(kind: TreeKind, rule: () => Mark.Closed, delimiter: TokenKind = TokenKind.ParenR)(implicit s: State): () => Mark.Closed = {
    () => {
      val mark = open()
      rule()
      if (!at(delimiter)) {
        expect(TokenKind.Comma)
      }
      close(mark, kind)
    }
  }

  private def asArgumentFlat(rule: () => Mark.Closed, delimiter: TokenKind = TokenKind.ParenR)(implicit s: State): () => Mark.Closed = {
    () => {
      val closed = rule()
      if (!at(delimiter)) {
        expect(TokenKind.Comma)
      }
      closed
    }
  }

  private val NAME_DEFINITION = List(TokenKind.NameLowerCase, TokenKind.NameUpperCase, TokenKind.NameMath, TokenKind.NameGreek)
  private val NAME_PARAMETER = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_VARIABLE = List(TokenKind.NameLowerCase, TokenKind.NameMath, TokenKind.NameGreek, TokenKind.Underscore)
  private val NAME_JAVA = List(TokenKind.NameJava, TokenKind.NameLowerCase, TokenKind.NameUpperCase)
  private val NAME_FIELD = List(TokenKind.NameLowerCase)
  private val NAME_TYPE = List(TokenKind.NameUpperCase)
  private val NAME_KIND = List(TokenKind.NameUpperCase)
  private val NAME_EFFECT = List(TokenKind.NameUpperCase)

  private def name(kinds: List[TokenKind], allowQualified: Boolean = false)(implicit s: State): Mark.Closed = {
    val mark = open()
    expectAny(kinds)
    val first = close(mark, TreeKind.Ident)
    if (!allowQualified) {
      return first
    }
    while (eat(TokenKind.Dot) && !eof()) {
      val mark = open()
      expectAny(kinds)
      close(mark, TreeKind.Ident)
    }
    if (allowQualified) {
      val mark = openBefore(first)
      close(mark, TreeKind.QName)
    } else {
      first
    }
  }

  ////////// GRAMMAR ///////////////

  /**
   * source -> declaration*
   */
  private def source()(implicit s: State): Unit = {
    val mark = open()
    while (!eof()) {
      declaration()
    }

    close(mark, TreeKind.Source)
  }

  private def declaration()(implicit s: State): Mark.Closed = {
    val mark = open()
    docComment()
    annotations()
    modifiers()
    nth(0) match {
      case TokenKind.KeywordDef => definition(mark)
      case TokenKind.KeywordClass | TokenKind.KeywordTrait => typeClass(mark)
      case TokenKind.KeywordInstance => instance(mark)
      case _ => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected declaration"))
    }
  }

  /**
   * instance -> docComment? annotations? modifiers? 'instance' qname typeParams '{' associatedTypeDef* definition*'}'
   */
  private def instance(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expect(TokenKind.KeywordInstance)
    name(NAME_DEFINITION, allowQualified = true)
    if (eat(TokenKind.BracketL)) {
      ttype()
      expect(TokenKind.BracketR)
    }
    if (at(TokenKind.CurlyL)) {
      expect(TokenKind.CurlyL)
      while (!at(TokenKind.CurlyR) && !eof()) {
        val mark = open()
        docComment()
        annotations() // TODO: associated types cant have annotations
        modifiers()
        nth(0) match {
          case TokenKind.KeywordDef => definition(mark)
          case TokenKind.KeywordType => associatedTypeDef(mark)
          case _ => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected associated type or definition"))
        }
      }
      expect(TokenKind.CurlyR)
    }
    close(mark, TreeKind.Instance)
  }

  /**
   * typeClass -> docComment? annotations? name typeParameters '{' (signature | associatedType)* '}'
   */
  private def typeClass(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expectAny(List(TokenKind.KeywordTrait, TokenKind.KeywordClass))
    name(NAME_DEFINITION)
    if (at(TokenKind.BracketL)) {
      typeParameters()
    }
    if (at(TokenKind.CurlyL)) {
      expect(TokenKind.CurlyL)
      while (!at(TokenKind.CurlyR) && !eof()) {
        val mark = open()
        docComment()
        annotations() // TODO: associated types cant have annotations
        modifiers()
        nth(0) match {
          // TODO: Law
          case TokenKind.KeywordDef => signature(mark)
          case TokenKind.KeywordType => associatedTypeSig(mark)
          case _ => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected associated type, signature or law"))
        }
      }
      expect(TokenKind.CurlyR)
    }
    close(mark, TreeKind.Class)
  }

  private def associatedTypeDef(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expect(TokenKind.KeywordType)
    name(NAME_TYPE)
    if (at(TokenKind.BracketL)) {
      typeArguments()
    }
    expect(TokenKind.Equal)
    ttype()
    close(mark, TreeKind.AssociatedTypeDef)
  }

  private def associatedTypeSig(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expect(TokenKind.KeywordType)
    name(NAME_TYPE)
    if (at(TokenKind.BracketL)) {
      typeParameters()
    }
    if (at(TokenKind.Colon)) {
      expect(TokenKind.Colon)
      kind()
    }
    close(mark, TreeKind.AssociatedTypeSig)
  }

  /**
   * signature -> 'def' name typeParameters parameters ':' ttype ('=' expression (';' expression)*)?
   */
  private def signature(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expect(TokenKind.KeywordDef)
    name(NAME_DEFINITION)
    if (at(TokenKind.BracketL)) {
      typeParameters()
    }
    if (at(TokenKind.ParenL)) {
      parameters()
    }
    expect(TokenKind.Colon)
    ttype(allowTrailingEffect = true)
    if (at(TokenKind.KeywordWith)) {
      withClause()
    }
    if (at(TokenKind.Equal)) {
      expect(TokenKind.Equal)
      while (at(TokenKind.Semi) && !eof()) {
        expect(TokenKind.Semi)
        expression()
      }
    }
    close(mark, TreeKind.Signature)
  }

  private def withClause()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.KeywordWith))
    val mark = open()
    expect(TokenKind.KeywordWith)
    var continue = true
    while (continue && !eof()) {
      if (atAny(NAME_DEFINITION)) {
        typeConstraint()
      } else {
        continue = false
      }
    }
    if (at(TokenKind.Comma)) {
      advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "Trailing comma."))
    }
    close(mark, TreeKind.WithClause)
  }

  private def typeConstraint()(implicit s: State): Mark.Closed = {
    val mark = open()
    name(NAME_DEFINITION, allowQualified = true)
    expect(TokenKind.BracketL)
    ttype()
    expect(TokenKind.BracketR)
    close(mark, TreeKind.Type.Constraint)
  }

  /**
   * definition -> docComment? annotations? 'def' name typeParameters parameters ':' ttype '=' expression (';' expression)*
   */
  private def definition(mark: Mark.Opened)(implicit s: State): Mark.Closed = {
    expect(TokenKind.KeywordDef)
    name(NAME_DEFINITION)
    if (at(TokenKind.BracketL)) {
      typeParameters()
    }
    if (at(TokenKind.ParenL)) {
      parameters()
    }
    expect(TokenKind.Colon)
    ttype(allowTrailingEffect = true)
    expect(TokenKind.Equal)
    expression()
    while (at(TokenKind.Semi) && !eof()) {
      expect(TokenKind.Semi)
      expression()
    }
    close(mark, TreeKind.Def)
  }

  private val MODIFIERS = List(
    TokenKind.KeywordSealed,
    TokenKind.KeywordLawful,
    TokenKind.KeywordPub,
    TokenKind.KeywordInline,
    TokenKind.KeywordOverride)

  private def modifiers()(implicit s: State): Mark.Closed = {
    val mark = open()
    while (atAny(MODIFIERS) && !eof()) {
      advance()
    }
    close(mark, TreeKind.Modifiers)
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

  ////////////////// PARAMETERS ///////////////

  /**
   * parameters -> '(' (parameter (',' parameter)* )? ')'
   */
  private def parameters()(implicit s: State): Mark.Closed = {
    commaSeparated(
      TreeKind.Parameters,
      asArgument(TreeKind.Parameter, () => {
        name(NAME_PARAMETER)
        expect(TokenKind.Colon)
        ttype()
      }),
      (TokenKind.ParenL, TokenKind.ParenR),
      () => atAny(NAME_PARAMETER),
    )
  }

  /**
   * typeParameters -> '[' (typeParameter (',' typeParameter)* )? ']'
   */
  private def typeParameters()(implicit s: State): Mark.Closed = {
    commaSeparated(
      TreeKind.TypeParameters,
      asArgument(
        TreeKind.Parameter, () => {
          var closed = name(NAME_VARIABLE ++ NAME_TYPE)
          if (at(TokenKind.Colon)) {
            expect(TokenKind.Colon)
            closed = kind()
          }
          closed
        },
        TokenKind.BracketR),
      (TokenKind.BracketL, TokenKind.BracketR),
      () => atAny(NAME_VARIABLE ++ NAME_TYPE),
    )
  }

  /**
   * kind -> name ('->' kind)?
   */
  private def kind()(implicit s: State): Mark.Closed = {
    val mark = open()
    name(NAME_KIND)
    if (eat(TokenKind.ArrowThin)) {
      kind()
    }
    close(mark, TreeKind.Kind)
  }

  /**
   * expression -> TODO
   */
  private def expression(left: TokenKind = TokenKind.Eof, leftIsUnary: Boolean = false)(implicit s: State): Mark.Closed = {
    var lhs = exprDelimited()

    // Handle calls
    while (at(TokenKind.ParenL)) {
      val mark = openBefore(lhs)
      arguments()
      lhs = close(mark, TreeKind.Expr.Call)
      close(openBefore(lhs), TreeKind.Expr.Expr)
    }

    // Handle binary operators
    var continue = true
    while (continue) {
      val right = nth(0)
      if (rightBindsTighter(left, right, leftIsUnary)) {
        val mark = openBefore(lhs)
        val markOp = open()
        advance()
        close(markOp, TreeKind.Operator)
        expression(right)
        lhs = close(mark, TreeKind.Expr.Binary)
        close(openBefore(lhs), TreeKind.Expr.Expr)
      } else {
        continue = false
      }
    }

    lhs
  }

  /**
   * arguments -> '(' (argument (',' argument)* )? ')'
   */
  private def arguments()(implicit s: State): Mark.Closed = {
    commaSeparated(
      TreeKind.Arguments,
      asArgument(TreeKind.Argument, () => expression()),
    )
  }

  private def exprDelimited()(implicit s: State): Mark.Closed = {
    val mark = open()
    // Handle clearly delimited expressions
    nth(0) match {
      case TokenKind.ParenL => exprParen()
      case TokenKind.CurlyL => exprBlock()
      case TokenKind.KeywordIf => exprIfThenElse()
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
           | TokenKind.NameGreek => name(NAME_DEFINITION, allowQualified = true)
      case TokenKind.KeywordImport => exprLetImport()
      case TokenKind.BuiltIn => exprIntrinsic()
      case TokenKind.Minus
           | TokenKind.KeywordNot
           | TokenKind.Plus
           | TokenKind.TripleTilde => exprUnary()
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected expression, found $t"))
    }
    close(mark, TreeKind.Expr.Expr)
  }

  private def exprIfThenElse()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.KeywordIf))
    val mark = open()
    expect(TokenKind.KeywordIf)
    if (eat(TokenKind.ParenL)) {
      expression()
      expect(TokenKind.ParenR)
    }
    expression()
    if (at(TokenKind.KeywordElse)) {
      expect(TokenKind.KeywordElse)
      expression()
    }
    close(mark, TreeKind.Expr.IfThenElse)
  }

  private def exprIntrinsic()(implicit s: State): Mark.Closed = {
    val mark = open()
    advance()
    close(mark, TreeKind.Expr.Intrinsic)
  }

  /////// JvmOp ///////////
  private object JvmOp {
    private def signature()(implicit s: State): Unit = {
      if (at(TokenKind.ParenL)) {
        commaSeparated(TreeKind.JvmOp.Signature, asArgumentFlat(() => ttype()))
      }
    }

    private def ascription()(implicit s: State): Mark.Closed = {
      val mark = open()
      expect(TokenKind.Colon)
      ttype(allowTrailingEffect = true)
      close(mark, TreeKind.JvmOp.Ascription)
    }

    def constructor()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordNew))
      val mark = open()
      expect(TokenKind.KeywordNew)
      name(NAME_JAVA, allowQualified = true)
      signature()
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
      close(mark, TreeKind.JvmOp.Constructor)
    }

    private def methodBody()(implicit s: State): Unit = {
      name(NAME_JAVA, allowQualified = true)
      signature()
      ascription()
      if (eat(TokenKind.KeywordAs)) {
        name(NAME_VARIABLE)
      }
    }

    def method()(implicit s: State): Mark.Closed = {
      val mark = open()
      methodBody()
      close(mark, TreeKind.JvmOp.Method)
    }

    def staticMethod()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      methodBody()
      close(mark, TreeKind.JvmOp.StaticMethod)
    }

    private def getBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordGet)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    private def putBody()(implicit s: State): Unit = {
      expect(TokenKind.KeywordSet)
      name(NAME_JAVA, allowQualified = true)
      ascription()
      expect(TokenKind.KeywordAs)
      name(NAME_VARIABLE)
    }

    def getField()(implicit s: State): Mark.Closed = {
      val mark = open()
      getBody()
      close(mark, TreeKind.JvmOp.GetField)
    }

    def staticGetField()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      getBody()
      close(mark, TreeKind.JvmOp.StaticGetField)
    }

    def putField()(implicit s: State): Mark.Closed = {
      val mark = open()
      putBody()
      close(mark, TreeKind.JvmOp.PutField)
    }

    def staticPutField()(implicit s: State): Mark.Closed = {
      assert(at(TokenKind.KeywordStatic))
      val mark = open()
      expect(TokenKind.KeywordStatic)
      putBody()
      close(mark, TreeKind.JvmOp.StaticPutField)
    }
  }

  private def exprLetImport()(implicit s:State): Mark.Closed = {
    assert(at(TokenKind.KeywordImport))
    val mark = open()
    expect(TokenKind.KeywordImport)
    val markJvmOp = open()
    nth(0) match {
      case TokenKind.KeywordNew => JvmOp.constructor()
      case TokenKind.KeywordGet => JvmOp.getField()
      case TokenKind.KeywordSet => JvmOp.putField()
      case TokenKind.KeywordStatic => nth(1) match {
        case TokenKind.KeywordGet => JvmOp.staticGetField()
        case TokenKind.KeywordSet => JvmOp.staticPutField()
        case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.staticMethod()
        case _ => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "expected static java import"))
      }
      case TokenKind.NameJava | TokenKind.NameLowerCase | TokenKind.NameUpperCase => JvmOp.method()
      case _ => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), "expected java import"))
    }
    close(markJvmOp, TreeKind.JvmOp.JvmOp)
    expect(TokenKind.Semi)
    expression()
    close(mark, TreeKind.Expr.LetImport)
  }

  /**
   * exprLiteral -> integer | float | boolean | string
   */
  private def exprLiteral()(implicit s: State): Mark.Closed = {
    val mark = open()
    advance()
    close(mark, TreeKind.Expr.Literal)
  }

  /**
   * exprParen -> '(' expression? ')'
   */
  private def exprParen()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.ParenL))
    val mark = open()
    expect(TokenKind.ParenL)
    if (eat(TokenKind.ParenR)) { // Handle unit tuple `()`
      return close(mark, TreeKind.Expr.Tuple)
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
  private def ttype(allowTrailingEffect: Boolean = false)(implicit s: State): Mark.Closed = {
    val mark = open()
    typeDelimited()
    var lhs = close(mark, TreeKind.Type.Type)

    // handle Type argument application
    if (at(TokenKind.BracketL)) {
      val mark = openBefore(lhs)
      typeArguments()
      lhs = close(mark, TreeKind.Type.Apply)
      // Wrap Apply in Type.Type
      lhs = close(openBefore(lhs), TreeKind.Type.Type)
    }

    // Handle function types
    if (at(TokenKind.Arrow)) {
      val mark = openBefore(lhs)
      ttype()
      if (at(TokenKind.Slash)) {
        effectSet()
      }
      lhs = close(mark, TreeKind.Type.Function)
      // Wrap Function in Type.Type
      lhs = close(openBefore(lhs), TreeKind.Type.Type)
    }

    // Handle effect
    if (at(TokenKind.Backslash)) {
      val effMark = effectSet()
      if (!allowTrailingEffect) {
        val mark = openBefore(effMark)
        closeWithError(mark, Parse2Error.DevErr(currentSourceLocation(), "Effect is not allowed here"))
      }
    }

    lhs
  }

  /**
   * effectSet -> '\' effect | '{' ( effect ( ',' effect )* )? '}'
   */
  private def effectSet()(implicit s: State): Mark.Closed = {
    assert(at(TokenKind.Backslash))
    expect(TokenKind.Backslash)
    if (at(TokenKind.CurlyL)) {
      commaSeparated(
        TreeKind.Type.EffectSet,
        asArgumentFlat(effect, TokenKind.CurlyR),
        (TokenKind.CurlyL, TokenKind.CurlyR),
      )
    } else {
      val mark = open()
      effect()
      close(mark, TreeKind.Type.EffectSet)
    }
  }

  private def effect()(implicit s: State): Mark.Closed = {
    val mark = open()
    nth(0) match {
      case TokenKind.NameUpperCase => name(NAME_EFFECT)
      case TokenKind.NameLowerCase => typeVariable()
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected effect, found $t"))
    }
    close(mark, TreeKind.Type.Type)
  }

  /**
   * typeArguments -> '[' ( argument ( ',' argument )* ) ']'?
   */
  private def typeArguments()(implicit s: State): Mark.Closed = {
    commaSeparated(
      TreeKind.Type.Arguments,
      asArgument(TreeKind.Type.Argument, () => ttype(), TokenKind.BracketR),
      (TokenKind.BracketL, TokenKind.BracketR),
    )
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
      case TokenKind.NameUpperCase => name(NAME_TYPE, allowQualified = true)
      case TokenKind.NameLowerCase => typeVariable()
      case TokenKind.NameMath
           | TokenKind.NameGreek
           | TokenKind.Underscore => name(NAME_VARIABLE)
      case t => advanceWithError(Parse2Error.DevErr(currentSourceLocation(), s"Expected type, found $t"))
    }
  }

  private def typeVariable()(implicit s: State): Mark.Closed = {
    val mark = open()
    expect(TokenKind.NameLowerCase)
    close(mark, TreeKind.Type.Variable)
  }

  /**
   * typeTuple -> '(' (type (',' type)* )? ')'
   */
  private def typeTuple()(implicit s: State): Mark.Closed = {
    commaSeparated(TreeKind.Type.Tuple, asArgumentFlat(() => ttype()))
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
      name(NAME_VARIABLE)
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
    name(NAME_FIELD)
    expect(TokenKind.Equal)
    ttype()
    if (!atAny(List(TokenKind.CurlyR, TokenKind.Bar))) {
      expect(TokenKind.Comma)
    }
    close(mark, TreeKind.Type.RecordField)
  }

  // A helper function that runs both the old and the new parser for comparison.
  // Both [[WeededAst]]s are printed for diffing to find inconsistencies.
  // Example of diffing with git:
  // >./gradlew run --args="--Xlib=nix foo.flix" > run.txt;
  //   sed -n '/\[\[\[ OLD PARSER \]\]\]/,/\[\[\[ NEW PARSER \]\]\]/p' run.txt > old.txt &&
  //   sed -n '/\[\[\[ NEW PARSER \]\]\]/,/\[\[\[\[ END \]\]\]/p' run.txt > new.txt &&
  //   git difftool -y --no-index ./old.txt ./new.txt

  // A helper function for formatting pretty printing ASTs.
  // It is generic to scala objects with some special handling for source positions and locations.
  private def formatWeededAst(obj: Any, depth: Int = 0, withLocs: Boolean = true, paramName: Option[String] = None): String = {
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case obj: SourcePosition => if (withLocs) s"SourcePosition (${obj.line}, ${obj.col})" else ""
      case obj: SourceLocation => if (withLocs) s"SourceLocation (${obj.beginLine}, ${obj.beginCol}) -> (${obj.endLine}, ${obj.endCol})" else ""
      case _: Iterable[Any] => ""
      case obj: Product => obj.productPrefix
      case _ => obj.toString
    }

    val acc = s"$indent$prettyName$ptype\n"

    obj match {
      case _: SourceLocation => acc
      case _: SourcePosition => acc
      case seq: Iterable[Any] =>
        acc + seq.map(formatWeededAst(_, depth + 1, withLocs, None)).mkString("")
      case obj: Product =>
        acc + (obj.productIterator zip obj.productElementNames)
          .map { case (subObj, paramName) => formatWeededAst(subObj, depth + 1, withLocs, Some(paramName)) }.mkString("")
      case _ => acc
    }
  }
}
