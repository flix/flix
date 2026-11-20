/*
 * Copyright 2024 Magnus Madsen, Herluf Baggesen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.shared.SyntacticContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, TokenKind, TypedAst}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for parser errors.
  */
sealed trait ParseError extends CompilationMessage {
  def kind: CompilationMessageKind = CompilationMessageKind.ParseError
  def sctx: SyntacticContext
  def loc: SourceLocation
}

object ParseError {

  /**
    * An error raised to indicate that a Thick Right Arrow was expected, but got a Thin Right Arrow
    *
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    */
  case class ExpectedArrowThickRGotArrowThinR(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError
    def code: ErrorCode = ErrorCode.E3169
    def summary: String = s"Expected '=>' got '->'"

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Expected '=>' got '->'
         |
         |${src(loc, s"Use '=>' instead of '->'")}
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a Thick Right Arrow was expected, but got an Equal.
    *
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    */
  case class ExpectedArrowThickRGotEqual(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError
    def code: ErrorCode = ErrorCode.E8652
    def summary: String = s"Expected '=>' got '='"

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Expected '=>' got '='
         |
         |${src(loc, s"Use '=>' instead of '='")}
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a Backslash was expected, but got a Slash.
    *
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    */
  case class ExpectedBackslashGotSlash(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError
    def code: ErrorCode = ErrorCode.E3795
    def summary: String = s"Expected '\\' got '/'"

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Expected '\\' got '/'
         |
         |${src(loc, s"Use '\\' instead of '/'")}
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a semicolon was expected.
    *
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    * @param found     The token that was found instead.
    */
  case class ExpectedSemicolon(sctx: SyntacticContext, loc: SourceLocation, found: TokenKind) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError
    def code: ErrorCode = ErrorCode.E3190

    def summary: String = s"Expected ';' but found '$found'"

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Expected ';' but found '$found'
         |
         |${src(loc, s"Expected ';' here")}
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate an operation which itself has an effect.
    *
    * @param sctx the syntactic context.
    * @param loc  the location where the error occurred.
    */
  case class IllegalEffectfulOperation(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    def code: ErrorCode = ErrorCode.E9356

    def summary: String = "Unexpected effect on operation."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected effect on operation.
         |
         |${src(loc, "unexpected effect")}
         |
         |${underline("Explanation:")} Effect operations may not themselves have effects.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an enum using both singleton and multiton syntaxes.
    *
    * @param sctx the syntactic context.
    * @param loc  the location where the error occurred.
    */
  case class IllegalEnum(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    def code: ErrorCode = ErrorCode.E9467

    def summary: String = "Unexpected enum format."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected enum format.
         |
         |${src(loc, "mixed singleton and case syntax")}
         |
         |${underline("Explanation:")} Only one enum form may be used.
         |If you only need one case, use the singleton syntax:
         |
         |    enum E(Int32)
         |
         |If you need multiple cases, use the case syntax:
         |
         |    enum E {
         |        case C1(Int32)
         |        case C2(Bool)
         |    }
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that type parameters are present on an effect operation.
    *
    * @param sctx the syntactic context.
    * @param loc  the location where the error occurred.
    */
  case class IllegalOperationTypeParams(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    def code: ErrorCode = ErrorCode.E9245

    def summary: String = "Unexpected effect operation type parameters."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected effect operation type parameters.
         |
         |${src(loc, "effect operations cannot declare type parameters")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that something was syntactically malformed.
    * This error is very generic, and should be avoided in favor of other ParserErrors if possible.
    *
    * @param namedTokenSet Name of the malformed token set.
    * @param sctx          The syntactic context.
    * @param hint          Optional hint with more details about the error.
    * @param loc           The source location.
    */
  case class Malformed(namedTokenSet: NamedTokenSet, sctx: SyntacticContext, hint: Option[String] = None, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6063

    def summary: String = s"Malformed ${namedTokenSet.display(Formatter.NoFormatter)}."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s""">> Malformed ${red(namedTokenSet.display(formatter))}.
         |
         |${src(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that one or more doc-comments are misplaced.
    *
    * @param sctx The syntactic context.
    * @param loc  The source location.
    */
  case class MisplacedDocComments(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6289

    def summary: String = s"Misplaced doc-comment(s)."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Misplaced doc-comment(s).
         |
         |${src(loc, s"Here")}
         |Hint: doc-comments must annotate declarations.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a binary operator is missing between two expressions.
    *
    * @param sctx The syntactic context.
    * @param loc  The source location.
    */
  case class MissingBinaryOperator(sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError
    def code: ErrorCode = ErrorCode.E9061
    def summary: String = s"Missing operator between expressions."
    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Missing operator between expressions.
         |
         |${src(loc, s"Expected an operator here")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a regioned expression is missing a region.
    *
    * @param token Name of the expression missing a region. See [[TokenKind.display]].
    * @param sctx  The syntactic context.
    * @param loc   The source location.
    */
  case class MissingRegion(token: TokenKind, sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6392

    def summary: String = s"Expected region on ${token.display}."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Expected ${red("region")} on ${cyan(token.display)}.
         |
         |${src(loc, s"Here")}
         |Hint: Add a region using `@ <region>`
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that no items were found in a context where one or more is needed.
    *
    * @param expected Names of the items that are expected at least one of. See [[TokenKind.display]].
    * @param sctx     The syntactic context.
    * @param hint     Optional hint with more details about the error
    * @param loc      The source location.
    */
  case class NeedAtleastOne(expected: NamedTokenSet, sctx: SyntacticContext, hint: Option[String] = None, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6405

    def summary: String = s"Expected at least one ${expected.display(Formatter.NoFormatter)}."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s""">> Expected at least one ${expected.display(formatter)}.
         |
         |${src(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a trailing separator is present.
    *
    * @param separator Name of the separator that is trailing. See [[TokenKind.display]].
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    */
  case class TrailingSeparator(separator: TokenKind, sctx: SyntacticContext, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6518

    def summary: String = s"Trailing ${separator.display}."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Trailing ${red(separator.display)}.
         |
         |${src(loc, s"Here")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unexpected token was found.
    *
    * @param expected Names of the tokens that are expected at the location.
    * @param actual   Name of the token that was actually found. See [[TokenKind.display]].
    * @param sctx     The syntactic context.
    * @param hint     Optional hint with more details about the error
    * @param loc      The source location.
    */
  case class UnexpectedToken(expected: NamedTokenSet, actual: Option[TokenKind], sctx: SyntacticContext = SyntacticContext.Unknown, hint: Option[String] = None, loc: SourceLocation) extends ParseError {
    override val kind: CompilationMessageKind = CompilationMessageKind.ParseError

    def code: ErrorCode = ErrorCode.E6629

    def summary: String = {
      val expectedStr = s"Expected ${expected.display(Formatter.NoFormatter)}"
      val actualStr = actual.map(a => s" before $a").getOrElse("")
      s"$expectedStr$actualStr."
    }

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      val expectedStr = s"Expected ${expected.display(formatter)}"
      val actualStr = actual.map(a => s" before ${red(a.display)}").getOrElse("")
      s""">> $expectedStr$actualStr.
         |
         |${src(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }
}

