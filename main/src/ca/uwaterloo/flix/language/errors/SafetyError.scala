package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.{Formatter, StdlibProfile}
import ca.uwaterloo.flix.util.CompilationTarget

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

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Operation not permitted in '${red(sctx.toString)}' security context.
         |
         |${highlight(loc, "not permitted", fmt)}
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
    * An error raised to indicate that Java interop is not available under the portable stdlib profile.
    */
  case class JavaInteropNotSupportedInPortableProfile(feature: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6021

    def summary: String = "Java interop is not supported under the portable stdlib profile."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Java interop is not supported under the portable stdlib profile.
         |
         |${highlight(loc, "not supported", fmt)}
         |
         |Feature: ${red(feature)}
         |
         |${underline("Explanation:")} The portable stdlib profile is intended for non-JVM targets
         |(e.g. future LLVM-based backends) and therefore disallows JVM-specific features such as:
         |
         |  - importing Java classes,
         |  - constructing Java objects, and
         |  - invoking Java methods / accessing Java fields.
         |
         |${underline("To fix:")}
         |
         |  - If you need Java interop, compile with: ${cyan("--Xstdlib-profile jvm")}
         |  - Otherwise, remove the Java interop usage (or move it behind a JVM-only overlay).
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a non-JVM compilation target requires the portable stdlib profile.
    */
  case class PortableStdlibProfileRequiredForTarget(target: CompilationTarget, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6022

    def summary: String = "Non-JVM compilation targets require the portable stdlib profile."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      val targetText = target match {
        case CompilationTarget.Jvm => "jvm"
        case CompilationTarget.LlvmNative => "llvm-native"
        case CompilationTarget.LlvmWasm => "llvm-wasm"
      }
      s""">> Non-JVM compilation targets require the portable stdlib profile.
         |
         |Target: ${red(targetText)}
         |
         |${underline("Explanation:")} Non-JVM targets (e.g. LLVM-based backends) cannot use JVM-only
         |stdlib overlays or Java interop. The portable stdlib profile provides a JVM-independent
         |baseline intended to run on both the JVM and future native/wasm targets.
         |
         |${underline("To fix:")}
         |
         |  - Compile with: ${cyan("--Xstdlib-profile portable")}
         |  - Or select the JVM target: ${cyan("--Xtarget jvm")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that `extern native` is only available on the native target.
    */
  case class NativeImportNotSupportedOnTarget(target: CompilationTarget, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6023

    def summary: String = "`extern native` is only supported on the native compilation target."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      val targetText = target match {
        case CompilationTarget.Jvm => "jvm"
        case CompilationTarget.LlvmNative => "native"
        case CompilationTarget.LlvmWasm => "wasm"
      }
      s""">> `extern native` is only supported on the native compilation target.
         |
         |${highlight(loc, "unsupported target", fmt)}
         |
         |Target: ${red(targetText)}
         |
         |${underline("Explanation:")} `extern native` is a direct C ABI import mechanism.
         |That mechanism only makes sense when compiling to a native binary. It is not
         |available on the JVM or wasm targets.
         |
         |${underline("To fix:")}
         |
         |  - Compile with: ${cyan("--target native")}
         |  - Or replace the import with a target-appropriate mechanism.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that `extern native` does not support polymorphic signatures.
    */
  case class NativeImportTypeParametersNotSupported(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6024

    def summary: String = "`extern native` does not support type parameters."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern native` does not support type parameters.
         |
         |${highlight(loc, "type parameters not supported", fmt)}
         |
         |${underline("Explanation:")} The v0 native import ABI is a direct raw C ABI.
         |It only supports monomorphic scalar signatures. Generic type parameters would require
         |an additional marshalling or specialization contract, which is intentionally not part
         |of this first implementation.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an `extern native` type is not supported by the raw C ABI.
    */
  case class IllegalNativeImportType(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6026

    def summary: String = "`extern native` uses a type that is not supported by the native import ABI."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern native` uses a type that is not supported by the native import ABI.
         |
         |${highlight(loc, "unsupported type", fmt)}
         |
         |Type: ${red(FormatType.formatType(tpe))}
         |
         |${underline("Explanation:")} The current native import ABI supports:
         |
         |  - Unit
         |  - Bool
         |  - Int8 / Int16 / Int32 / Int64
         |  - Float32 / Float64
         |  - String
         |  - Array[Int8, Static] as Bytes
         |  - recursive portable ABI aggregates:
         |    - List[t]
         |    - Array[t, Static]
         |    - Tuple[...]
         |    - Option[t]
         |    - Result[e, t]
         |    - closed records
         |
         |Arbitrary ADTs, open records, region-polymorphic arrays, and polymorphic types are not part
         |of the native import ABI.
         |""".stripMargin
    }
  }

  case class WasmImportNotSupportedOnTarget(target: CompilationTarget, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6030

    def summary: String = "`extern wasm` is only supported on the wasm compilation target."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      val targetText = target match {
        case CompilationTarget.Jvm => "jvm"
        case CompilationTarget.LlvmNative => "native"
        case CompilationTarget.LlvmWasm => "wasm"
      }
      s""">> `extern wasm` is only supported on the wasm compilation target.
         |
         |${highlight(loc, "unsupported target", fmt)}
         |
         |Target: ${red(targetText)}
         |
         |${underline("Explanation:")} `extern wasm` binds directly to typed component-model imports.
         |That mechanism only makes sense when compiling to the wasm target.
         |""".stripMargin
    }
  }

  case class WasmImportTypeParametersNotSupported(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6027

    def summary: String = "`extern wasm` does not support type parameters."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern wasm` does not support type parameters.
         |
         |${highlight(loc, "type parameters not supported", fmt)}
         |
         |${underline("Explanation:")} The v0 wasm import ABI is deliberately narrow and monomorphic.
         |Generic imports would require a separate specialization or marshalling contract.
         |""".stripMargin
    }
  }

  case class IllegalWasmImportType(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6028

    def summary: String = "`extern wasm` uses a type that is not supported by the v0 direct wasm import ABI."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern wasm` uses a type that is not supported by the v0 direct wasm import ABI.
         |
         |${highlight(loc, "unsupported type", fmt)}
         |
         |Type: ${red(FormatType.formatType(tpe))}
         |
         |${underline("Explanation:")} The v0 direct wasm import path supports the same portable
         |sync data shapes as the current export ABI:
         |
         |  - Unit
         |  - Bool
         |  - Int8 / Int16 / Int32 / Int64
         |  - Float32 / Float64
         |  - String / Bytes
         |  - List[T] / Array[T, Static]
         |  - Tuples, closed records, Option[T], and Result[Ok, Err]
         |
         |Every nested field type must itself be portable and monomorphic.
         |Open records, arbitrary ADTs, and polymorphic signatures are still excluded.
         |""".stripMargin
    }
  }

  case class MalformedWasmImportInterface(interface: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6029

    def summary: String = "`extern wasm` requires a valid WIT interface identifier."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern wasm` requires a valid WIT interface identifier.
         |
         |${highlight(loc, "invalid interface", fmt)}
         |
         |Interface: ${red(interface)}
         |
         |${underline("Expected shape:")} ${cyan("namespace:package/interface@major.minor.patch")}
         |
         |Example: ${cyan("host:math/basic@0.1.0")}
         |""".stripMargin
    }
  }

  case class ConflictingWasmImportSignature(interface: String, func: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E6031

    def summary: String = "`extern wasm` binds the same imported function with conflicting signatures."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> `extern wasm` binds the same imported function with conflicting signatures.
         |
         |${highlight(loc, "conflicting import signature", fmt)}
         |
         |Interface: ${red(interface)}
         |Function : ${red(func)}
         |
         |${underline("Explanation:")} A WIT world can only declare one typed signature for a given
         |imported function. If multiple Flix defs bind the same imported symbol, they must agree on
         |the exact parameter and result types.
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

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: neither type is a subtype of the other.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(FormatType.formatType(from))}
         |To  : ${red(FormatType.formatType(to))}
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

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: cannot cast a Flix type to a Java type.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(FormatType.formatType(from))}
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

    def summary: String = "Impossible cast: cannot cast from a type variable."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: cannot cast from a type variable.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(FormatType.formatType(from))}
         |To  : ${red(FormatType.formatType(to))}
         |
         |${underline("Explanation:")} A checked cast requires the source type to be known
         |at compile time.
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

    def summary: String = "Impossible cast: cannot cast a Java type to a Flix type."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: cannot cast a Java type to a Flix type.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(formatJavaType(from))}
         |To  : ${red(FormatType.formatType(to))}
         |
         |${underline("Explanation:")} A checked cast can only be used between Java types.
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

    def summary: String = "Impossible cast: cannot cast to a type variable."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: cannot cast to a type variable.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(FormatType.formatType(from))}
         |To  : ${red(FormatType.formatType(to))}
         |
         |${underline("Explanation:")} A checked cast requires the target type to be known
         |at compile time.
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

    def summary: String = s"Unexpected method effect: '${FormatType.formatType(eff)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected method effect: '${red(FormatType.formatType(eff))}'.
         |
         |${highlight(loc, "unexpected effect", fmt)}
         |
         |${underline("Explanation:")} Methods in a 'new' expression must be pure or have
         |primitive effects. Control effects cannot escape to Java.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the catch type is invalid for the active stdlib profile.
    *
    * @param loc the location of the catch parameter.
    */
  case class IllegalCatchType(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4354

    def summary: String = flix.options.stdlibProfile match {
      case StdlibProfile.Portable =>
        s"Unexpected catch type: '${FormatType.formatType(tpe)}' is not a valid portable exception matcher."
      case _ =>
        s"Unexpected catch type: '${FormatType.formatType(tpe)}' is not a subclass of Throwable."
    }

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      flix.options.stdlibProfile match {
        case StdlibProfile.Portable =>
          s""">> Unexpected catch type: '${red(FormatType.formatType(tpe))}' is not a valid portable exception matcher.
             |
             |${highlight(loc, "unexpected type", fmt)}
             |
             |${underline("Explanation:")} Under the portable stdlib profile, catch clauses participate in the
             |portable exception system and match a payload type (kind) or `Exn` for catch-all.
             |""".stripMargin
        case _ =>
          s""">> Unexpected catch type: '${red(FormatType.formatType(tpe))}' is not a subclass of Throwable.
             |
             |${highlight(loc, "unexpected type", fmt)}
             |
             |${underline("Explanation:")} A catch clause can only catch subclasses of
             |'java.lang.Throwable'.
             |""".stripMargin
      }
    }
  }

  /**
    * An error raised to indicate that the object in a `throw` expression is not a Throwable.
    *
    * @param loc the location of the object
    */
  case class IllegalThrowType(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4465

    def summary: String = flix.options.stdlibProfile match {
      case StdlibProfile.Portable =>
        s"Unexpected throw type: '${FormatType.formatType(tpe)}' is not a portable exception value."
      case _ =>
        s"Unexpected throw type: '${FormatType.formatType(tpe)}' is not a subclass of Throwable."
    }

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      flix.options.stdlibProfile match {
        case StdlibProfile.Portable =>
          s""">> Unexpected throw type: '${red(FormatType.formatType(tpe))}' is not a portable exception value.
             |
             |${highlight(loc, "unexpected type", fmt)}
             |
             |${underline("Explanation:")} Under the portable stdlib profile, a throw expression must throw the
             |designated portable exception value type `Exn` (payload + trace).
             |""".stripMargin
        case _ =>
          s""">> Unexpected throw type: '${red(FormatType.formatType(tpe))}' is not a subclass of Throwable.
             |
             |${highlight(loc, "unexpected type", fmt)}
             |
             |${underline("Explanation:")} A throw expression can only throw subclasses of
             |'java.lang.Throwable'.
             |""".stripMargin
      }
    }
  }

  /**
    * An error raised to indicate an illegal use of a wildcard in a negative atom.
    *
    * @param loc the position of the body atom containing the illegal wildcard.
    */
  case class IllegalNegativelyBoundWildCard(loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E4576

    def summary: String = "Unexpected wildcard '_' in negated atom."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected wildcard '${red("_")}' in negated atom.
         |
         |${highlight(loc, "negated atom", fmt)}
         |
         |${underline("Explanation:")} Wildcards cannot appear in negated atoms because
         |negation requires all variables to be bound by a positive atom.
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

    def summary: String = s"Unexpected wild variable '$sym' in negated atom."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected wild variable '${red(sym.text)}' in negated atom.
         |
         |${highlight(loc, "negated atom", fmt)}
         |
         |${underline("Explanation:")} Wild variables cannot appear in negated atoms because
         |negation requires all variables to be bound by a positive atom.
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

    def summary: String = s"Unexpected variable '$sym' in negated atom: not bound by a positive atom."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected variable '${red(sym.text)}' in negated atom: not bound by a positive atom.
         |
         |${highlight(loc, "negated atom", fmt)}
         |
         |${underline("Explanation:")} Variables in negated atoms must be bound by a positive atom.
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

    def summary: String = "Unexpected pattern in body atom."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected pattern in body atom.
         |
         |${highlight(loc, "body atom", fmt)}
         |
         |${underline("Explanation:")} Body atoms can only contain variables, wildcards, and constants.
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

    def summary: String = s"Unexpected use of lattice variable '$sym' in relational position."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected use of lattice variable '${red(sym.text)}' in relational position.
         |
         |${highlight(loc, "relational use", fmt)}
         |
         |${underline("Explanation:")} A lattice variable cannot be used in a relational atom
         |unless its origin is marked with 'fix'. For example:
         |
         |  P(v) :- L(x; v), Q(v).      // Error: 'v' is a lattice variable used relationally
         |  P(v) :- fix L(x; v), Q(v).  // OK: 'fix' allows relational use
         |
         |Add 'fix' to the atom where '${magenta(sym.text)}' originates.
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

    def summary: String = "Impossible cast: types are incompatible."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Impossible cast: types are incompatible.
         |
         |${highlight(loc, "impossible cast", fmt)}
         |
         |From: ${red(FormatType.formatType(from))}
         |To  : ${red(FormatType.formatType(to))}
         |
         |${underline("Explanation:")} An unchecked cast between these types will never succeed.
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

    def summary: String = "Missing default case in typematch."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Missing default case in typematch.
         |
         |${highlight(loc, "typematch expression", fmt)}
         |
         |${underline("Explanation:")} A typematch expression must have a default case. For example:
         |
         |  typematch x {
         |      case y: Int32 => ...
         |      case _: _ => ... // default case
         |  }
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

    def summary: String = s"Primitive effect '${sym.name}' cannot be handled."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Primitive effect '${red(sym.name)}' cannot be handled.
         |
         |${highlight(loc, "handler", fmt)}
         |
         |${underline("Explanation:")} Primitive effects like IO cannot be handled with a 'run-with' expression.
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

    def summary: String = s"Unexpected 'this' type for method '$name'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected 'this' type for method '${red(name)}'.
         |
         |${highlight(loc, "method definition", fmt)}
         |
         |Expected: ${cyan(clazz.getName)}
         |Actual:   ${red(illegalThisType.toString)}
         |
         |${underline("Explanation:")} The first formal parameter of any method must be 'this' and must
         |have the same type as the superclass. For example:
         |
         |  new ${clazz.getSimpleName} {
         |      def $name(_this: ${clazz.getSimpleName}, ...): ... = ...
         |  }
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unimplemented superclass method.
    *
    * @param clazz  The type of the superclass.
    * @param method The unimplemented method.
    * @param loc    The source location of the object derivation.
    */
  case class NewObjectMissingMethod(clazz: java.lang.Class[?], method: java.lang.reflect.Method, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5467

    def summary: String = s"Missing implementation of method '${method.getName}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      val parameterTypes = (clazz +: method.getParameterTypes).map(formatJavaType)
      val returnType = formatJavaType(method.getReturnType)
      s""">> Missing implementation of method '${red(method.getName)}' of '${magenta(clazz.getName)}'.
         |
         |${highlight(loc, "new object", fmt)}
         |
         |${underline("Explanation:")} Add a method with the following signature:
         |
         |  def ${method.getName}(${parameterTypes.mkString(", ")}): $returnType
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a class lacks a public zero-argument constructor.
    *
    * @param clazz the class.
    * @param loc   the source location of the new object expression.
    */
  case class NewObjectMissingPublicZeroArgConstructor(clazz: java.lang.Class[?], loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5578

    def summary: String = s"Class '${clazz.getName}' lacks a public zero-argument constructor."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Class '${red(clazz.getName)}' lacks a public zero-argument constructor.
         |
         |${highlight(loc, "missing constructor", fmt)}
         |
         |${underline("Explanation:")} A 'new' expression requires the class to have a public
         |constructor with no arguments.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a missing `this` parameter for a method.
    *
    * @param clazz The expected `this` type.
    * @param name  The name of the method with the missing `this` parameter.
    * @param loc   The source location of the method.
    */
  case class NewObjectMissingThisArg(clazz: java.lang.Class[?], name: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5689

    def summary: String = s"Missing 'this' parameter for method '$name'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Missing 'this' parameter for method '${red(name)}'.
         |
         |${highlight(loc, "missing 'this' parameter", fmt)}
         |
         |${underline("Explanation:")} The first formal parameter of any method must be 'this' and must
         |have the same type as the superclass. For example:
         |
         |  new ${clazz.getSimpleName} {
         |      def $name(_this: ${clazz.getSimpleName}, ...): ... = ...
         |  }
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

    def summary: String = s"Class '${clazz.getName}' is not public."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Class '${red(clazz.getName)}' is not public.
         |
         |${highlight(loc, "non-public class", fmt)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an object derivation includes a method that doesn't exist
    * in the superclass being implemented.
    *
    * @param clazz The type of superclass.
    * @param name  The name of the undefined method.
    * @param loc   The source location of the method.
    */
  case class NewObjectUndefinedMethod(clazz: java.lang.Class[?], name: String, loc: SourceLocation) extends SafetyError {
    def code: ErrorCode = ErrorCode.E5803

    def summary: String = s"Method '$name' not found in superclass '${clazz.getName}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Method '${red(name)}' not found in superclass '${magenta(clazz.getName)}'.
         |
         |${highlight(loc, "undefined method", fmt)}
         |
         |${underline("Explanation:")} The method does not exist in the superclass with
         |the given signature. Check the method name and parameter types.
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
