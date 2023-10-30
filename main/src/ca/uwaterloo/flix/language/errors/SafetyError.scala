package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for safety errors.
  */
sealed trait SafetyError extends CompilationMessage {
  val kind: String = "Safety Error"
}

object SafetyError {

  /**
    * An error raised to indicate an unexpected pattern bound in a body atom.
    *
    * @param loc the position of the body atom containing the unexpected pattern.
    */
  case class UnexpectedPatternInBodyAtom(loc: SourceLocation) extends SafetyError {
    def summary: String = s"Unexpected pattern in body atom."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected pattern in body atom.
         |
         |${code(loc, "pattern occurs in this body atom.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

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
    * An error raised to indicate an impossible cast.
    *
    * @param from the type of the expression being cast.
    * @param to   the type being cast to, i.e. the declared type or effect of the cast.
    * @param loc  the source location of the cast.
    */
  case class ImpossibleCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Impossible cast."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The following cast is impossible and will never succeed.
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
    * An error raised to indicate an illegal checked type cast.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedTypeCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Illegal checked cast"

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal checked cast.
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
  case class IllegalCastFromNonJava(from: Type, to: java.lang.Class[_], loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Illegal checked cast: Attempt to cast a non-Java type to a Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal checked cast: Attempt to cast a non-Java type to a Java type.
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
    * An error raised to indicate a cast from a Java type to a non-Java type.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCastToNonJava(from: java.lang.Class[_], to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Illegal checked cast: Attempt to cast a Java type to a non-Java type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal checked cast: Attempt to cast a Java type to a non-Java type.
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
    * An error raised to indicate a cast from a type variable to a type.
    *
    * @param from the source type (the variable).
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCastFromVar(from: Type.Var, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Illegal checked cast: Attempt to cast a type variable to a type."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal checked cast: Attempt to cast a type variable to a type.
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
    * An error raised to indicate a cast from a type to a type variable.
    *
    * @param from the source type.
    * @param to   the destination type (the variable).
    * @param loc  the source location of the cast.
    */
  case class IllegalCastToVar(from: Type, to: Type.Var, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    override def summary: String = "Illegal checked cast: Attempt to cast a type to a type variable."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal checked cast: Attempt to cast a type to a type variable.
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
  case class IllegalParExpression(exp: Expr, loc: SourceLocation) extends SafetyError {
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
  case class MissingDefaultTypeMatchCase(loc: SourceLocation) extends SafetyError {
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
    * @param tpe the type that is not sendable.
    * @param loc the location where the error occurred.
    */
  case class SendableError(tpe: Type, loc: SourceLocation) extends SafetyError {
    def summary: String = s"Cannot derive Sendable for $tpe"

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

  /**
    * An error raised to indicate that a function marked with the `@test` annotation
    * has at least one non-unit parameter.
    *
    * @param loc the source location of the parameter.
    */
  case class IllegalTestParameters(loc: SourceLocation) extends SafetyError {
    def summary: String = s"Test entry point must not have parameters."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Test entry point must not have parameters.
         |
         |${code(loc, "Parameter for test function occurs here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""
         |A test function must not have parameters.
         |
         |If you need to test your code with different values,
         |you can create a helper function that takes parameters.
         |Then, you can call the helper function with different
         |values to perform various tests.
         |
         |Example
         |
         |    @test
         |    def test01(x: Int32): Int32 = x + 1
         |
         |Should be
         |
         |    @test
         |    def test01(): Int32 = testHelper(1)
         |
         |    def testHelper(x: Int32): Int32 = x + 1
         |
         |""".stripMargin
    )
  }

  /**
    * An error raised to indicate that a function marked with the `@Tailrec` annotation
    * has at least non-tail-recursive function call.
    *
    * @param sym the symbol of the function annotated with `@Tailrec`.
    * @param loc the location of the non-tail-recursive call.
    */
  case class NonTailRecursiveFunction(sym: Symbol.DefnSym, loc: SourceLocation) extends SafetyError {
    override def summary: String = s"Function '$sym' annotated with @Tailrec can only have recursive calls in tail position."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Function '$sym' annotated with @Tailrec can only have recursive calls in tail position.
         |
         |${code(loc, "A recursive call in non-tail position occurs here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      """"
        |The @Tailrec annotation checks that a function is indeed tail recursive.
        |
        |A function that calls itself directly is tail recursive.
        |The fibonacci function can be written as below:
        |
        |  def fib(n: Int32): Int32 =
        |      if (n < 1) 1 else n * fib(n - 1)
        |
        |However, this grows the stack with each recursive call.
        |This can be fixed by adding an additional argument, allowing
        |the last expression to be a directly recursive call.
        |
        |  def fib(n: Int32, acc: Int32): Int32 =
        |      if (n < 1) acc else fib(n - 1, n * acc)
        |
        |"""".stripMargin
    })
  }

  /**
    * An error raised to indicate that a function marked with the `@Tailrec` annotation
    * does not contain a recursive call.
    *
    * @param sym the symbol of the function annotated with `@Tailrec`.
    * @param loc the location of the function body.
    */
  case class TailRecursiveFunctionWithoutRecursiveCall(sym: Symbol.DefnSym, loc: SourceLocation) extends SafetyError {
    override def summary: String = s"Function '$sym' annotated with @Tailrec must contain a recursive call."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Function '$sym' annotated with @Tailrec must contain a recursive call.
         |
         |${code(loc, "The function does not contain a recursive call.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }
}
