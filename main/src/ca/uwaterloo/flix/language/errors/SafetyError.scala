package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
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
    * An error raised to indicate an invalid use of upcast.
    *
    * @param actual   the type of the expression being upcast.
    * @param expected the type being cast to, i.e. the type of the upcast expression itself.
    * @param loc      the source location of the unsafe upcast.
    */
  case class UnsafeUpcast(actual: Type, expected: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe upcast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following upcast is unsafe and not allowed.
         |
         |${code(loc, "the upcast occurs here.")}
         |
         |Actual type:      $actual
         |Tried casting to: $expected
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an invalid use of supercast.
    *
    * @param from the type of the expression being supercast.
    * @param to   the type being cast to, i.e. the type of the supercast expression itself.
    * @param loc  the source location of the supercast.
    */
  case class UnsafeSupercast(from: Type, to: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe supercast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following supercast is unsafe and not allowed.
         |
         |${code(loc, "the supercast occurs here.")}
         |
         |Actual type:      $from
         |Tried casting to: $to
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate use of supercast on a non-Java type.
    *
    * @param nonJavaType the type that is **not** a Java type.
    * @param javaType    the Java class.
    * @param loc         the source location of the supercast.
    */
  case class FromNonJavaTypeSupercast(nonJavaType: Type, javaType: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe supercast: Attempted to cast from a non-Java type to a Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following supercast tries to cast a non-Java to a Java type.
         |
         |${code(loc, "the supercast occurs here.")}
         |
         |Non-Java type:    $nonJavaType
         |Tried casting to: ${formatJavaType(javaType)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate use of supercast to a non-Java type.
    *
    * @param javaType    the Java class.
    * @param nonJavaType the type that is **not** a Java type.
    * @param loc         the source location of the supercast.
    */
  case class ToNonJavaTypeSupercast(javaType: java.lang.Class[_], nonJavaType: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe supercast: Attempted to cast from a Java type to a non-Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following supercast attempts to cast a Java type to a non-Java type.
         |
         |${code(loc, "the supercast occurs here.")}
         |
         |Java type:        ${formatJavaType(javaType)}
         |Tried casting to: $nonJavaType
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate use of supercast from a type variable,
    * i.e. the actual type is not known, possibly caused by
    * supercasting a type to a type also being supercast.
    *
    * @param tvar the type of the expression being supercast (in this case a type variable).
    * @param to   the type being cast to, i.e. the type of the supercast expression itself.
    * @param loc  the source location of the supercast.
    */
  case class FromTypeVariableSupercast(tvar: Type, to: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe supercast: Attempted to cast from a type variable."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following supercast attempts to cast from a type variable.
         |
         |${code(loc, "the supercast occurs here.")}
         |
         |Type variable:    $tvar
         |Tried casting to: ${formatIfJavaType(to)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] =
      Some({
        s"""Did you try to supercast two types at the same time? e.g.
           |
           |  if (true) super_cast exp1 else super_cast exp2
           |
           |where exp1 and exp2 have Java types.
           |""".stripMargin
      })
  }

  /**
    * An error raised to indicate use of supercast to a type variable,
    * i.e. the expected type is not known, possibly caused by
    * supercasting a type to a type also being supercast.
    *
    * @param from the type of the expression being supercast.
    * @param tvar the type being cast to, i.e. the type of the supercast expression itself (in this case a type variable).
    * @param loc  the source location of the supercast.
    */
  case class ToTypeVariableSupercast(from: Type, tvar: Type, loc: SourceLocation) extends SafetyError {
    override def summary: String = "Unsafe supercast: Attempted to cast to a type variable."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following supercast attempts to cast to a type variable.
         |
         |${code(loc, "the supercast occurs here.")}
         |
         |Actual type:      ${formatIfJavaType(from)}
         |Tried casting to: $tvar
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] =
      Some({
        s"""Did you try to supercast two types at the same time? e.g.
           |
           |  if (true) super_cast exp1 else super_cast exp2
           |
           |where exp1 and exp2 have Java types.
           |""".stripMargin
      })
  }

  /**
    * An error raised to indicate an illegal object derivation. Objects can only be derived from Java interfaces.
    *
    * @param clazz The (illegal) superclass.
    * @param loc   The source location of the object derivation.
    */
  case class IllegalObjectDerivation(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Illegal object derivation from '$clazz'."

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
    def summary: String = s"Missing `this` parameter for method '$name'."

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
    def summary: String = s"Invalid `this` parameter for method '$name'."

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
  private def formatJavaType(t: java.lang.Class[_]): String = {
    if (t.isPrimitive || t.isArray)
      Type.getFlixType(t).toString
    else
      s"##${t.getName}"
  }

  /**
    * Returns a formatted Java type if `t` is a Java type.
    * Returns `t` as a string otherwise.
    */
  private def formatIfJavaType(t: Type): String = Type.eraseAliases(t).baseType match {
    case Type.Cst(TypeConstructor.Native(clazz), _) => formatJavaType(clazz)
    case _ => t.toString
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
         | def ${method.getName}(${parameterTypes.mkString(", ")}): $returnType
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
    def summary: String = s"Method '$name' not found in superclass '${clazz.getName}'"

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

  /**
    * An error raised to indicate that a class lacks a public zero argument constructor.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class MissingPublicZeroArgConstructor(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Class '${clazz.getName}' lacks a public zero argument constructor."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Class '${red(clazz.getName)}' lacks a public zero argument constructor.
         |
         |${code(loc, "missing constructor.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a class is non-public.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NonPublicClass(clazz: java.lang.Class[_], loc: SourceLocation) extends SafetyError {
    def summary: String = s"Class '${clazz.getName}' is not public"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Class '${red(clazz.getName)}' is not public.
         |
         |${code(loc, "non-public class.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a `par expression` is not supported.
    *
    * @param exp the par expression.
    * @param loc the source location of the expression.
    */
  case class IllegalParExpression(exp: Expression, loc: SourceLocation) extends SafetyError {
    override def summary: String = s"Unable to parallelize $exp"

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to parallelize expression.
         |
         |${code(loc, "illegal par expression")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] =
      Some("Only tuples and function applications can be parallelized with par.")
  }

  /**
    * An error raised to indicate that a TypeMatch expression is missing a default case.
    *
    * @param loc the location where the error occurred.
    */
  case class MissingDefaultMatchTypeCase(loc: SourceLocation) extends SafetyError {
    override def summary: String = s"Missing default case."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Missing default case.
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
    * Unable to derive Sendable error
    *
    * @param sym the enum for which we're trying to derive Sendable
    * @param loc the location where the error occurred.
    */
  case class SendableError(tpe: Type, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Cannot derive Sendable for ${tpe}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Cannot derive 'Sendable' for type ${red(tpe.toString)}
        |
        |Because it takes a type parameter of kind 'Region'.
        |
        |${code(loc, "unable to derive Sendable.")}
        |
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""
        |An example of a type parameter of kind 'Region':
        |
        |enum MyEnum[r: Region] { ... }
        |""".stripMargin
    )
  }
}
