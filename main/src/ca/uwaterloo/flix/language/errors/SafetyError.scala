package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for safety errors.
  *
  * All [[SafetyError]]s are [[Recoverable]].
  */
sealed trait SafetyError extends CompilationMessage with Recoverable {
  val kind: String = "Safety Error"
}

object SafetyError {

  /**
    * An error raised to indicate an illegal checked type cast.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Illegal checked cast"

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal checked cast.
         |
         |${code(loc, "illegal cast.")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a cast from a non-Java type to a Java type.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastFromNonJava(from: Type, to: java.lang.Class[_], loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Illegal checked cast: Attempt to cast a non-Java type to a Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal checked cast: Attempt to cast a non-Java type to a Java type.
         |
         |${code(loc, "illegal cast")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${formatJavaType(to)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a cast from a type variable to a type.
    *
    * @param from the source type (the variable).
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastFromVar(from: Type.Var, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Illegal checked cast: Attempt to cast a type variable to a type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal checked cast: Attempt to cast a type variable to a type.
         |
         |${code(loc, "illegal cast")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a cast from a Java type to a non-Java type.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastToNonJava(from: java.lang.Class[_], to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Illegal checked cast: Attempt to cast a Java type to a non-Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal checked cast: Attempt to cast a Java type to a non-Java type.
         |
         |${code(loc, "illegal cast")}
         |
         |From: ${formatJavaType(from)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a cast from a type to a type variable.
    *
    * @param from the source type.
    * @param to   the destination type (the variable).
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastToVar(from: Type, to: Type.Var, loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Illegal checked cast: Attempt to cast a type to a type variable."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal checked cast: Attempt to cast a type to a type variable.
         |
         |${code(loc, "illegal checked cast.")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a reachable entry point has an illegal signature.
    *
    * @param loc the location of the defn.
    */
  case class IllegalEntryPointSignature(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Illegal entry point signature"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal entry point signature. An entry point must take a single Unit
         |>> argument and be pure or have the IO effect.
         |
         |${code(loc, "illegal signature.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function has an illegal namespace.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportNamespace(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must be in a module (e.g. not in the root namespace)"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must be in a module (e.g. not in the root namespace).
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function is not public.
    *
    * @param loc the location of the defn.
    */
  case class NonPublicExport(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must be public"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must be public.
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function has an invalid name.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportName(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must have a Java valid name"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must have a Java valid name.
         |
         |${code(loc, "invalid Java name.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function has an invalid effect.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportEffect(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must be pure or have the IO effect"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must be pure or have the IO effect.
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function is type-polymorphic.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportPolymorphism(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must not have type variables (i.e. not polymorphic)"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must not have type variables (i.e. not polymorphic).
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an exported function uses an illegal type.
    *
    * @param t the type that is not allowed.
    * @param loc the location of the type.
    */
  case class IllegalExportType(t: Type, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exported functions must use primitive Java types or Object, not '$t'"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Exported functions must use primitive Java types or Object, not '$t'.
         |
         |${code(loc, "unsupported type.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the Java class in a catch clause is not a Throwable.
    *
    * @param loc the location of the catch parameter.
    */
  case class IllegalCatchType(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Exception type is not a subclass of Throwable."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> $summary
         |
         |${code(loc, "Type should be java.lang.Throwable or a subclass.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a try-catch expression contains another try-catch expression.
    *
    * @param loc the location of the inner try-catch.
    */
  case class IllegalNestedTryCatch(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Try-catch expressions cannot be nested."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> $summary
         |
         |${code(loc, "The inner try-catch expression.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"""${underline("Tip:")} Put the inner try-catch expression in a function.""".stripMargin
    })
  }

  /**
    * An error raised to indicate an illegal use of a wildcard in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal wildcard.
    */
  case class IllegalNegativelyBoundWildCard(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Illegal negatively bound wildcard '_'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal negatively bound wildcard '${red("_")}'.
         |
         |${code(loc, "the wildcard occurs in this negated atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a wild variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNegativelyBoundWildVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Illegal negatively bound variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal negatively bound variable '${red(sym.text)}'.
         |
         |${code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a non-positively bound variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNonPositivelyBoundVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Illegal non-positively bound variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal non-positively bound variable '${red(sym.text)}'.
         |
         |${code(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      if (!sym.isWild)
        s"""${underline("Tip:")} Ensure that the variable occurs in at least one positive atom.""".stripMargin
      else
        ""
    })
  }

  /**
    * An error raised to indicate an unexpected pattern bound in a body atom.
    *
    * @param loc the position of the body atom containing the unexpected pattern.
    */
  case class IllegalPatternInBodyAtom(loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Unexpected pattern in body atom."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unexpected pattern in body atom.
         |
         |${code(loc, "pattern occurs in this body atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal relational use of the lattice variable `sym`.
    *
    * @param sym the variable symbol.
    * @param loc the source location of the atom where the illegal use occurs.
    */
  case class IllegalRelationalUseOfLatticeVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Illegal relational use of the lattice variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Illegal relational use of the lattice variable '${red(sym.text)}'. Use `fix`?
         |
         |${code(loc, "the illegal use occurs here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = Some({
      s"""A lattice variable cannot be used as relational variable unless the atom
         |from which it originates is marked with `fix`.
         |""".stripMargin
    })
  }

  /**
    * Unable to derive Sendable error
    *
    * @param tpe the type that is not sendable.
    * @param loc the location where the error occurred.
    */
  case class IllegalSendableInstance(tpe: Type, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Cannot derive Sendable for $tpe"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Cannot derive 'Sendable' for type ${red(tpe.toString)}
         |
         |Because it takes a type parameter of kind 'Region'.
         |
         |${code(loc, "unable to derive Sendable.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(
      s"""
         |An example of a type parameter of kind 'Region':
         |
         |enum MyEnum[r: Region] { ... }
         |""".stripMargin
    )
  }

  /**
    * An error raised to indicate an impossible unchecked cast.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class ImpossibleUncheckedCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError with Recoverable {
    override def summary: String = "Impossible cast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> The following cast is impossible and will never succeed.
         |
         |${code(loc, "the cast occurs here.")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a TypeMatch expression is missing a default case.
    *
    * @param loc the location where the error occurred.
    */
  case class MissingDefaultTypeMatchCase(loc: SourceLocation) extends SafetyError with Recoverable {
    override def summary: String = s"Missing default case."

    override def message(formatter: Formatter): String = {
      import formatter._
      s""">> Missing default case.
         |
         |${code(loc, "missing default case.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         | A typematch expression must have a default case. For example:
         |
         | typematch x {
         |     case y: Int32 => ...
         |     case _: _ => ... // default case
         | }
         |
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
  case class NewObjectIllegalThisType(clazz: java.lang.Class[_], illegalThisType: Type, name: String, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Invalid `this` parameter for method '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Invalid 'this' parameter for method '${red(name)}''.
         |
         |Expected 'this' type is ${cyan(s"##${clazz.getName}")}, but the first argument is declared as type ${cyan(illegalThisType.toString)}
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         | The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an unimplemented superclass method
    *
    * @param clazz  The type of the superclass.
    * @param method The unimplemented method.
    * @param loc    The source location of the object derivation.
    */
  case class NewObjectMissingMethod(clazz: java.lang.Class[_], method: java.lang.reflect.Method, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"No implementation found for method '${method.getName}' of superclass '${clazz.getName}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> No implementation found for method '${red(method.getName)}' of superclass '${red(clazz.getName)}'.
         |>> Signature: '${method.toString}'
         |
         |${code(loc, "the object occurs here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      val parameterTypes = (clazz +: method.getParameterTypes).map(formatJavaType)
      val returnType = formatJavaType(method.getReturnType)
      s"""
         | Try adding a method with the following signature:
         |
         | def ${method.getName}(${parameterTypes.mkString(", ")}): $returnType
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that a class lacks a public zero argument constructor.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NewObjectMissingPublicZeroArgConstructor(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Class '${clazz.getName}' lacks a public zero argument constructor."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Class '${red(clazz.getName)}' lacks a public zero argument constructor.
         |
         |${code(loc, "missing constructor.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a missing `this` parameter for a method.
    *
    * @param clazz The expected `this` type.
    * @param name  The name of the method with the invalid `this` parameter.
    * @param loc   The source location of the method.
    */
  case class NewObjectMissingThisArg(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Missing `this` parameter for method '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Missing 'this' parameter for method '${red(name)}''.
         |
         |The 'this' parameter should have type ${cyan(s"##${clazz.getName}")}
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         | The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that a class is non-public.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NewObjectNonPublicClass(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Class '${clazz.getName}' is not public"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Class '${red(clazz.getName)}' is not public.
         |
         |${code(loc, "non-public class.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an object derivation includes a method that doesn't exist in the superclass being implemented.
    *
    * @param clazz The type of superclass
    * @param name  The name of the extra method.
    * @param loc   The source location of the method.
    */
  case class NewObjectUnreachableMethod(clazz: java.lang.Class[_], name: String, loc: SourceLocation) extends SafetyError with Recoverable {
    def summary: String = s"Method '$name' not found in superclass '${clazz.getName}'"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Method '${red(name)}' not found in superclass '${red(clazz.getName)}'
         |
         |${code(loc, "the method occurs here.")}
         |""".stripMargin
    }
  }

  /**
    * Format a Java type suitable for method implementation.
    */
  private def formatJavaType(t: java.lang.Class[_]): String = {
    if (t.isPrimitive || t.isArray)
      Type.getFlixType(t).toString
    else
      s"##${t.getName}"
  }
}
