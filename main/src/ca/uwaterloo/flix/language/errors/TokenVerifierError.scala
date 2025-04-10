package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Token, TokenKind}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

sealed trait TokenVerifierError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.TokenVerifierError
}

object TokenVerifierError {

  case class EmptyTokens(src: Source) extends TokenVerifierError {
    override def loc: SourceLocation =
      SourceLocation.zeroPoint(isReal = true, SourcePosition.firstPosition(src))

    override def summary: String = s"Found empty token array for ${src.name}."

    override def message(formatter: Formatter): String =
      s"Found empty token array for ${src.name}."
  }

  case class MissingEof(found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Found non-eof last token: ${found.kind}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Found non-eof last token: ${found.kind}.
         |
         |${code(loc, "here")}
         |
         |""".stripMargin
    }
  }

  case class UnexpectedEof(found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Found ${TokenKind.Eof} with tokens following it."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Found ${TokenKind.Eof} with tokens following it.
         |
         |${code(loc, "here")}
         |
         |""".stripMargin
    }
  }

  case class WrongSource(src: Source, found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Found token with source ${found.src.name} in the tokens of ${src.name}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Found token with source ${found.src.name} in the tokens of ${src.name}.
         |
         |${code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    }
  }

  case class WrongOffsetRange(found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Invalid offset range: ${found.start} - ${found.end}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Invalid offset range: ${found.start} - ${found.end}.
         |
         |${code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    }
  }

  case class WrongPositionRange(found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Invalid position range: ${found.sp1.lineOneIndexed}:${found.sp1.colOneIndexed} - ${found.sp2.lineOneIndexed}:${found.sp2.colOneIndexed}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Invalid position range: ${found.sp1.lineOneIndexed}:${found.sp1.colOneIndexed} - ${found.sp2.lineOneIndexed}:${found.sp2.colOneIndexed}.
         |
         |${code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    }
  }

  case class OutOfBoundOffset(found: Token) extends TokenVerifierError {
    override val loc: SourceLocation = found.mkSourceLocation()

    override def summary: String = s"Token with out-of-bound offsets: ${found.start} - ${found.end}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Token with out-of-bound offsets: ${found.start} - ${found.end}.
         |
         |${code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    }
  }

  case class OutOfOrderOffsets(left: Token, right: Token) extends TokenVerifierError {
    override val loc: SourceLocation = left.mkSourceLocation()

    override def summary: String = s"Overlapping tokens (offset): ${left.start} - ${right.end} and ${right.start} - ${right.end}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Overlapping tokens: ${left.start} - ${right.end} and ${right.start} - ${right.end}.
         |
         |${code(left.mkSourceLocation(), s"left token here (${left.kind})")}
         |
         |${code(right.mkSourceLocation(), s"right token here (${right.kind})")}
         |
         |""".stripMargin
    }
  }

  case class OutOfOrderPositions(left: Token, right: Token) extends TokenVerifierError {
    override val loc: SourceLocation = left.mkSourceLocation()

    override def summary: String = s"Overlapping tokens (position): ${left.sp1.lineOneIndexed}:${left.sp1.colOneIndexed} - ${left.sp2.lineOneIndexed}:${left.sp2.colOneIndexed} and ${right.sp1.lineOneIndexed}:${right.sp1.colOneIndexed} - ${right.sp2.lineOneIndexed}:${right.sp2.colOneIndexed}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Overlapping tokens (position): ${left.sp1.lineOneIndexed}:${left.sp1.colOneIndexed} - ${left.sp2.lineOneIndexed}:${left.sp2.colOneIndexed} and ${right.sp1.lineOneIndexed}:${right.sp1.colOneIndexed} - ${right.sp2.lineOneIndexed}:${right.sp2.colOneIndexed}.
         |
         |${code(left.mkSourceLocation(), s"left token here (${left.kind})")}
         |
         |${code(right.mkSourceLocation(), s"right token here (${right.kind})")}
         |
         |""".stripMargin
    }
  }

}
