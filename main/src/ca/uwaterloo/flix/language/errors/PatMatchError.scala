package ca.uwaterloo.flix.language.errors


import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

sealed trait PatMatchError extends CompilationMessage with Recoverable {
  val kind: String = "Pattern Match Error"
}

object PatMatchError {
  
  /**
  * An error raised to indicate a non-exhaustive pattern match expression.
  */
  case class NonExhaustiveMatchError(pat: String, loc: SourceLocation) extends PatMatchError {
    def summary: String = s"Non-exhaustive match. Missing case: '$pat'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Non-Exhaustive Pattern. Missing case: ${red(pat)} in match expression.
        |
        |${code(loc, "incomplete pattern.")}
        |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""Flix requires every pattern match expression to be exhaustive, i.e. to cover all
        |possible cases. A wild card pattern, written with an underscore, can be used to
        |handle all other cases. For example:
        |
        |    case _ => // handle all other cases.
        |
        |""".stripMargin
    })

  }


  /**
  * An error raised to indicate a subtype exception following it's supertype exception.
  */
  case class ExceptionTypeMatchError(laterException: java.lang.Class[_], earlierException: java.lang.Class[_], loc: SourceLocation) extends PatMatchError {

    def summary: String = s"Exception typing error: $laterException follows $earlierException."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exception typing error. Subtype exception $laterException cannot follow supertype exception $earlierException.
        |
        |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""To avoid unreachable code in try-catch blocks, always place catch blocks for more specific exceptions before 
        |their supertypes in exception handling. For example:
        |
        |    case ex: FileNotFoundException => ...
        |    case _: IOException => ...
        |
        |""".stripMargin
    })

  }


}