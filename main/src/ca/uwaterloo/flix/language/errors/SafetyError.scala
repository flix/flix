package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.Format

/**
  * A common super-type for safety errors.
  */
sealed trait SafetyError extends CompilationMessage {
  val kind: String = "Safety Error"
}

object SafetyError {

  /**
    * An error raised to indicate an illegal use of a non-positively bound variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNonPositivelyBoundVariable(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal non-positively bound variable '$sym'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal non-positively bound variable '${Format.red(sym.text)}'.
         |
         |${Format.code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }

    override def explain: String = {
      if (!sym.isWild)
        s"""
           |${Format.underline("Tip:")} Ensure that the variable occurs in at least one positive atom.
           |""".stripMargin
      else
        ""
    }
  }

  /**
    * An error raised to indicate an illegal use of a wild variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNegativelyBoundWildVariable(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal negatively bound variable '$sym'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal negatively bound variable '${Format.red(sym.text)}'.
         |
         |${Format.code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a wildcard in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal wildcard.
    */
  case class IllegalNegativelyBoundWildcard(loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal negatively bound wildcard '_'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal negatively bound wildcard '${Format.red("_")}'.
         |
         |${Format.code(loc, "the wildcard occurs in this negated atom.")}
         |""".stripMargin
    }
  }
}
