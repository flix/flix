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

  case class IllegalObjectDerivation(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal object derivation from '${clazz}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> '${red(clazz.toString)}' is not a Java interface.
        |
        |${code(loc, "the illegal derivation occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  case class InvalidThis(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Invalid `this` parameter for method '${name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Invalid `this` parameter for method '${red(name)}''.
        |
        |${code(loc, "the method occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
        | The first argument to any method must be `this`, and must have the same type as the interface
        | being implemented.
        |""".stripMargin
    })
  }

  case class UnimplementedMethod(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"No implementation found for method '${name}' of interface '${clazz.getName}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> No implementation found for method '${red(name)}' of interface '${red(clazz.getName)}'.
        |
        |${code(loc, "the object occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  case class ExtraMethod(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Method '${name}' not found in interface '${clazz.getName}'"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Method '${red(name)}' not found in interface '${red(clazz.getName)}'
        |
        |${code(loc, "the method occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }
}
