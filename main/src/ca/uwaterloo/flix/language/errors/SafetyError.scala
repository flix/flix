package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/** A common super-type for safety errors. */
sealed trait SafetyError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.SafetyError
}

object SafetyError {

  /**
    * An error raised to indicate a forbidden operation.
    *
    * @param sctx the security context of the location where the error occurred.
    * @param loc  the source location of the forbidden operation.
    */
  case class Forbidden(sctx: SecurityContext, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E3685

    def summary: String = s"Operation not permitted in '$sctx' security context."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Operation not permitted in '${red(sctx.toString)}' security context.
         |
         |${src(loc, "not permitted")}
         |
         |${underline("Explanation:")} Security contexts control which language features a
         |dependency can use, reducing the risk of supply-chain attacks.
         |
         |The security contexts are:
         |  - paranoid: forbids Java interop, unchecked casts, and the IO effect.
         |  - plain (default): forbids Java interop and unchecked casts, but allows IO.
         |  - unrestricted: permits everything.
         |
         |To resolve this error, either replace the dependency with one that respects the
         |security context, or grant the dependency broader permissions. Be aware that
         |granting broader permissions may increase your exposure to supply-chain attacks.
         |
         |To grant unrestricted permissions, update your flix.toml:
         |
         |  [dependencies]
         |  "github:xxx/yyy" = { version = "1.2.3", security = "unrestricted" }
         |
         |Learn more: https://doc.flix.dev/packages.html#security
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal checked type cast.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E3796

    def summary: String = "Impossible cast: neither type is a subtype of the other."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Impossible cast: neither type is a subtype of the other.
         |
         |${src(loc, "impossible cast")}
         |
         |From: ${red(FormatType.formatType(from, None))}
         |To  : ${red(FormatType.formatType(to, None))}
         |
         |${underline("Explanation:")} A checked cast between Java types requires a subtype
         |relationship. Neither type is a subtype of the other.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a cast from a non-Java type to a Java type.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastFromNonJava(from: Type, to: java.lang.Class[?], loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E3807

    def summary: String = "Impossible cast: cannot cast a Flix type to a Java type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Impossible cast: cannot cast a Flix type to a Java type.
         |
         |${src(loc, "impossible cast")}
         |
         |From: ${red(FormatType.formatType(from, None))}
         |To  : ${red(formatJavaType(to))}
         |
         |${underline("Explanation:")} A checked cast can only be used between Java types.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a cast from a type variable to a type.
    *
    * @param from the source type (the variable).
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastFromVar(from: Type.Var, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E3918

    def summary: String = "Illegal checked cast: Attempt to cast a type variable to a type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal checked cast: Attempt to cast a type variable to a type.
         |
         |${src(loc, "illegal cast")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a cast from a Java type to a non-Java type.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastToNonJava(from: java.lang.Class[?], to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4029

    def summary: String = "Illegal checked cast: Attempt to cast a Java type to a non-Java type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal checked cast: Attempt to cast a Java type to a non-Java type.
         |
         |${src(loc, "illegal cast")}
         |
         |From: ${formatJavaType(from)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a cast from a type to a type variable.
    *
    * @param from the source type.
    * @param to   the destination type (the variable).
    * @param loc  the source location of the cast.
    */
  case class IllegalCheckedCastToVar(from: Type, to: Type.Var, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4132

    def summary: String = "Illegal checked cast: Attempt to cast a type to a type variable."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal checked cast: Attempt to cast a type to a type variable.
         |
         |${src(loc, "illegal checked cast.")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal method effect in a new expression.
    *
    * @param eff the illegal effect.
    * @param loc the source location of the method.
    */
  case class IllegalMethodEffect(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4243

    def summary: String = "Illegal method effect"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal method effect: '${red(FormatType.formatType(eff, None))}'. A method must be pure or have a primitive effect.
         |
         |${src(loc, "illegal effect.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the Java class in a catch clause is not a Throwable.
    *
    * @param loc the location of the catch parameter.
    */
  case class IllegalCatchType(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4354

    def summary: String = s"Exception type is not a subclass of Throwable."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> $summary
         |
         |${src(loc, "Type should be java.lang.Throwable or a subclass.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the object in a `throw` expression is not a Throwable.
    *
    * @param loc the location of the object
    */
  case class IllegalThrowType(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4465

    def summary: String = s"Exception type is not a subclass of Throwable."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> $summary
         |
         |${src(loc, "Type should be java.lang.Throwable or a subclass.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a wildcard in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal wildcard.
    */
  case class IllegalNegativelyBoundWildCard(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4576

    def summary: String = s"Illegal negatively bound wildcard '_'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal negatively bound wildcard '${red("_")}'.
         |
         |${src(loc, "the wildcard occurs in this negated atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a wild variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNegativelyBoundWildVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4687

    def summary: String = s"Illegal negatively bound variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal negatively bound variable '${red(sym.text)}'.
         |
         |${src(loc, "the variable occurs in this negated atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal use of a non-positively bound variable in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal variable.
    */
  case class IllegalNonPositivelyBoundVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4798

    def summary: String = s"Illegal non-positively bound variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      val explanation = if (!sym.isWild) {
        s"""
           |
           |${underline("Tip:")} Ensure that the variable occurs in at least one positive atom.""".stripMargin
      } else ""
      s""">> Illegal non-positively bound variable '${red(sym.text)}'.
         |
         |${src(loc, "the variable occurs in this negated atom.")}$explanation
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unexpected pattern bound in a body atom.
    *
    * @param loc the position of the body atom containing the unexpected pattern.
    */
  case class IllegalPatternInBodyAtom(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4809

    def summary: String = s"Unexpected pattern in body atom."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected pattern in body atom.
         |
         |${src(loc, "pattern occurs in this body atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal relational use of the lattice variable `sym`.
    *
    * @param sym the variable symbol.
    * @param loc the source location of the atom where the illegal use occurs.
    */
  case class IllegalRelationalUseOfLatticeVar(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4912

    def summary: String = s"Illegal relational use of the lattice variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal relational use of the lattice variable '${red(sym.text)}'. Use `fix`?
         |
         |${src(loc, "the illegal use occurs here.")}
         |
         |${underline("Explanation:")}
         |A lattice variable cannot be used as relational variable unless the atom
         |from which it originates is marked with `fix`.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an impossible unchecked cast.
    *
    * @param from the source type.
    * @param to   the destination type.
    * @param loc  the source location of the cast.
    */
  case class ImpossibleUncheckedCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5023

    def summary: String = "Impossible cast."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The following cast is impossible and will never succeed.
         |
         |${src(loc, "the cast occurs here.")}
         |
         |From: ${FormatType.formatType(from, None)}
         |To  : ${FormatType.formatType(to, None)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a TypeMatch expression is missing a default case.
    *
    * @param loc the location where the error occurred.
    */
  case class MissingDefaultTypeMatchCase(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5134

    def summary: String = s"Missing default case."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing default case.
         |
         |${src(loc, "missing default case.")}
         |
         |${underline("Explanation:")}
         |A typematch expression must have a default case. For example:
         |
         | typematch x {
         |     case y: Int32 => ...
         |     case _: _ => ... // default case
         | }
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a primitive effect is attempted to be handled in a try-with expression.
    *
    * @param sym the effect symbol.
    * @param loc the location where the error occurred.
    */
  case class PrimitiveEffectInRunWith(sym: Symbol.EffSym, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5245

    def summary: String = s"The ${sym.name} effect cannot be handled."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The ${sym.name} effect cannot be handled.
         |
         |${src(loc, s"attempted to handle the ${sym.name} effect here.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an invalid `this` parameter for a method.
    *
    * @param clazz           The expected `this` type.
    * @param illegalThisType The incorrect `this` type.
    * @param name            The name of the method with the invalid `this` parameter.
    * @param loc             The source location of the method.
    */
  case class NewObjectIllegalThisType(clazz: java.lang.Class[?], illegalThisType: Type, name: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5356

    def summary: String = s"Invalid `this` parameter for method '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Invalid 'this' parameter for method '${red(name)}''.
         |
         |Expected 'this' type is ${cyan(s"${clazz.getName}")}, but the first argument is declared as type ${cyan(illegalThisType.toString)}
         |
         |${src(loc, "the method occurs here.")}
         |
         |${underline("Explanation:")}
         |The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unimplemented superclass method
    *
    * @param clazz  The type of the superclass.
    * @param method The unimplemented method.
    * @param loc    The source location of the object derivation.
    */
  case class NewObjectMissingMethod(clazz: java.lang.Class[?], method: java.lang.reflect.Method, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5467

    def summary: String = s"No implementation found for method '${method.getName}' of superclass '${clazz.getName}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      val parameterTypes = (clazz +: method.getParameterTypes).map(formatJavaType)
      val returnType = formatJavaType(method.getReturnType)
      s""">> No implementation found for method '${red(method.getName)}' of superclass '${red(clazz.getName)}'.
         |>> Signature: '${method.toString}'
         |
         |${src(loc, "the object occurs here.")}
         |
         |${underline("Explanation:")}
         |Try adding a method with the following signature:
         |
         | def ${method.getName}(${parameterTypes.mkString(", ")}): $returnType
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a class lacks a public zero argument constructor.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NewObjectMissingPublicZeroArgConstructor(clazz: java.lang.Class[?], loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5578

    def summary: String = s"Class '${clazz.getName}' lacks a public zero argument constructor."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Class '${red(clazz.getName)}' lacks a public zero argument constructor.
         |
         |${src(loc, "missing constructor.")}
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
  case class NewObjectMissingThisArg(clazz: java.lang.Class[?], name: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5689

    def summary: String = s"Missing `this` parameter for method '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing 'this' parameter for method '${red(name)}''.
         |
         |The 'this' parameter should have type ${cyan(s"${clazz.getName}")}
         |
         |${src(loc, "the method occurs here.")}
         |
         |${underline("Explanation:")}
         |The first argument to any method must be 'this', and must have the same type as the superclass.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a class is non-public.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NewObjectNonPublicClass(clazz: java.lang.Class[?], loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5792

    def summary: String = s"Class '${clazz.getName}' is not public"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Class '${red(clazz.getName)}' is not public.
         |
         |${src(loc, "non-public class.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an object derivation includes a method that doesn't exist
    * in the superclass being implemented.
    *
    * @param clazz The type of superclass
    * @param name  The name of the undefined method.
    * @param loc   The source location of the method.
    */
  case class NewObjectUndefinedMethod(clazz: java.lang.Class[?], name: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5803

    def summary: String = s"Method '$name' not found in superclass '${clazz.getName}' with the written signature"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Method '${red(name)}' not found in superclass '${red(clazz.getName)}' with the written signature
         |
         |${src(loc, "the method occurs here.")}
         |""".stripMargin
    }
  }

  /** Returns the string representation of `tpe`. */
  private def formatJavaType(tpe: java.lang.Class[?]): String = {
    if (tpe.isPrimitive || tpe.isArray)
      Type.getFlixType(tpe).toString
    else
      tpe.getName
  }
}
