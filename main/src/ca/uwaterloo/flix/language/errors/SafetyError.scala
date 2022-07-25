package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Type, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.Safety
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

  /**
    * An error raised to indicate an illegal object derivation. Objects can only be derived from Java interfaces.
    *
    * @param clazz The (illegal) superclass.
    * @param loc   The source location of the object derivation.
    */
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

  /**
    * An error raised to indicate a missing `this` parameter for a method.
    *
    * @param thisType The expected `this` type.
    * @param name     The name of the method with the invalid `this` parameter.
    * @param loc      The source location of the method.
    */
  case class MissingThis(thisType: Type, name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Missing `this` parameter for method '${name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Missing `this` parameter for method '${red(name)}''.
        |
        |The `this` parameter should have type ${cyan(thisType.toString)}
        |
        |${code(loc, "the method occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
        | The first argument to any method must be `this`, and must have the same type as the superclass.
        |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an invalid `this` parameter for a method.
    *
    * @param thisType        The expected `this` type.
    * @param illegalThisType The incorrect `this` type.
    * @param name            The name of the method with the invalid `this` parameter.
    * @param loc             The source location of the method.
    */
  case class IllegalThisType(thisType: Type, illegalThisType: Type, name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Invalid `this` parameter for method '${name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Invalid `this` parameter for method '${red(name)}''.
        |
        |Expected `this` type is ${cyan(thisType.toString)}, but the first argument is declared as type ${cyan(illegalThisType.toString)}
        |
        |${code(loc, "the method occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
        | The first argument to any method must be `this`, and must have the same type as the superclass.
        |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an unimplemented superclass method
    *
    * @param thisType The type of the superclass.
    * @param method   The signature of the unimplemented method
    * @param loc      The source location of the object derivation.
    */
  case class UnimplementedMethod(thisType: Type, method: Safety.MethodSignature, loc: SourceLocation) extends SafetyError {
    def summary: String = s"No implementation found for method '${method.name}' of superclass '${thisType}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> No implementation found for method '${red(method.name)}' of superclass '${red(thisType.toString)}'.
        |
        |${code(loc, "the object occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
        | Try adding a method with the following signature:
        |
        | def ${method.name}(${method.paramTypes.mkString(", ")}): ${method.retTpe}
        |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that an object derivation includes a method that doesn't exist
    * in the superclass being implemented.
    *
    * @param thisType The type of superclass
    * @param name     The name of the extra method.
    * @param loc      The source location of the method.
    */
  case class ExtraMethod(thisType: Type, method: Safety.MethodSignature, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Method '${method.name}' not found in superclass '${thisType}'"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Method '${red(method.name)}' not found in superclass '${red(thisType.toString)}'
        |
        |${code(loc, "the method occurs here.")}
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

}
