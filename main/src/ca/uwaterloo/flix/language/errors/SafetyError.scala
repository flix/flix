package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.Formatter

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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal non-positively bound variable '${red(sym.text)}'.
         |
         |${code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      if (!sym.isWild)
        s"""
           |${underline("Tip:")} Ensure that the variable occurs in at least one positive atom.
           |""".stripMargin
      else
        ""
    })
  }

  /**
    * An error raised to indicate an illegal use of a wild variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNegativelyBoundWildVariable(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal negatively bound variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal negatively bound variable '${red(sym.text)}'.
         |
         |${code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal use of a wildcard in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal wildcard.
    */
  case class IllegalNegativelyBoundWildcard(loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal negatively bound wildcard '_'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal negatively bound wildcard '${red("_")}'.
         |
         |${code(loc, "the wildcard occurs in this negated atom.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal relational use of the lattice variable `sym`.
    *
    * @param sym the variable symbol.
    * @param loc the source location of the atom where the illegal use occurs.
    */
  case class IllegalRelationalUseOfLatticeVariable(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal relational use of the lattice variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal relational use of the lattice variable '${red(sym.text)}'. Use `fix`?
         |
         |${code(loc, "the illegal use occurs here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |A lattice variable cannot be used as relational variable unless the atom
         |from which it originates is marked with `fix`.
         |""".stripMargin
    })
  }

  case class UnsafeUpcast(loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe upcast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unsafe upcast
         |
         |${code(loc, "the upcast occurs here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""An upcast is only allowed if it casts from
         |Pure -> ef -> Impure
         |or from a Java type to a Java super-type.""".stripMargin
    })

  }

}
