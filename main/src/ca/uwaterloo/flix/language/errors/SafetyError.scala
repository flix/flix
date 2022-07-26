package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
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

  /**
    * An error raised to indicate an illegal relational use of the lattice variable `sym`.
    *
    * @param actualType     the type of the expression being upcast.
    * @param actualPurity   the purity of the expression being upcast.
    * @param actualEffect   the effect of the expression being upcast.
    * @param expectedType   the expected type being upcast to.
    * @param expectedPurity the expected purity being upcast to.
    * @param expectedEffect the expected effect being upcast to.
    * @param loc            the source location of the unsafe upcast.
    */
  case class UnsafeUpcast(actualType: Type, actualPurity: Type, actualEffect: Type, expectedType: Type, expectedPurity: Type, expectedEffect: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe upcast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following upcast is unsafe and not allowed.
         |
         |${code(loc, "the upcast occurs here.")}
         |
         |Actual type  : $actualType
         |Actual purity: $actualPurity
         |Actual effect: $actualEffect
         |
         |Expected type  : $expectedType
         |Expected purity: $expectedPurity
         |Expected effect: $expectedEffect
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
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
    * @param clazz The expected `this` type.
    * @param name  The name of the method with the invalid `this` parameter.
    * @param loc   The source location of the method.
    */
  case class MissingThis(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Missing `this` parameter for method '${name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Missing 'this' parameter for method '${red(name)}''.
         |
         |The 'this' parameter should have type ${cyan(s"##${clazz.getName}")}
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         | The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an invalid `this` parameter for a method.
    *
    * @param clazz           The expected `this` type.
    * @param illegalThisType The incorrect `this` type.
    * @param name            The name of the method with the invalid `this` parameter.
    * @param loc             The source location of the method.
    */
  case class IllegalThisType(clazz: java.lang.Class[_], illegalThisType: Type, name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Invalid `this` parameter for method '${name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Invalid 'this' parameter for method '${red(name)}''.
         |
         |Expected 'this' type is ${cyan(s"##${clazz.getName}")}, but the first argument is declared as type ${cyan(illegalThisType.toString)}
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         | The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    })
  }

  /**
    * Format a Java type suitable for method implementation.
    */
  private def formatJavaType(t: java.lang.Class[_]) = {
    if (t.isPrimitive() || t.isArray())
      Type.getFlixType(t).toString
    else
      s"##${t.getName}"
  }

  /**
    * An error raised to indicate an unimplemented superclass method
    *
    * @param clazz  The type of the superclass.
    * @param method The unimplemented method.
    * @param loc    The source location of the object derivation.
    */
  case class UnimplementedMethod(clazz: java.lang.Class[_], method: java.lang.reflect.Method, loc: SourceLocation) extends SafetyError {
    def summary: String = s"No implementation found for method '${method.getName}' of superclass '${clazz.getName}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> No implementation found for method '${red(method.getName)}' of superclass '${red(clazz.getName)}'.
         |
         |${code(loc, "the object occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      val parameterTypes = (clazz +: method.getParameterTypes).map(formatJavaType)
      val returnType = formatJavaType(method.getReturnType)
      s"""
         | Try adding a method with the following signature:
         |
         | def ${method.getName}(${parameterTypes.mkString(", ")}): ${returnType}
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that an object derivation includes a method that doesn't exist
    * in the superclass being implemented.
    *
    * @param clazz The type of superclass
    * @param name  The name of the extra method.
    * @param loc   The source location of the method.
    */
  case class ExtraMethod(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Method '${name}' not found in superclass '${clazz.getName}'"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Method '${red(name)}' not found in superclass '${red(clazz.getName)}'
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  case class NonDefaultConstructor(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Superclass '${clazz.getName}' has a non-default constructor"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Method Superclass '${red(clazz.getName)}' has a non-default constructor
         |
         |${code(loc, "the object occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""
        | Flix 'object' statements only support interfaces and classes with default (no-argument) constructors.
        |""".stripMargin
    )
  }

  case class InaccessibleSuperclass(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Superclass '${clazz.getName}' is not public"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Superclass '${red(clazz.getName)}' is not public
         |
         |${code(loc, "the object occurs here.")}
         |""".stripMargin
    }
    
    def explain(formatter: Formatter): Option[String] = None
  }
}
