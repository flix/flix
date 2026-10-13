/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for errors produced by [[ca.uwaterloo.flix.language.phase.EntryPoints]].
  */
sealed trait EntryPointError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.EntryPointError
}

object EntryPointError {

  /**
    * Error indicating the specified entry point is missing.
    *
    * @param sym the entry point function.
    */
  case class EntryPointNotFound(sym: Symbol.DefnSym) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1625

    def summary: String = s"Entry point '${sym.name}' not found."

    // NB: We do not print the symbol source location as it is always Unknown.
    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Entry point '${red(sym.toString)}' not found.
         |
         |${underline("Possible fixes:")}
         |
         |  (1) Change the specified entry point to an existing function.
         |  (2) Add an entry point function '${magenta(sym.toString)}'.
         |""".stripMargin
    }

    def loc: SourceLocation = SourceLocation.Unknown
  }

  /**
    * An error raised to indicate that an entry point function has an unexpected formal parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointArgs(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1512

    def summary: String = s"Unexpected formal parameter in entry point."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected formal parameter in entry point function.
         |
         |${highlight(loc, "formal parameter not allowed", fmt)}
         |
         |${underline("Explanation:")} Entry point functions (main and tests) must have
         |no formal parameters.
         |
         |Expected signature:
         |
         |  def main(): Unit = ...
         |
         |or for tests:
         |
         |  @Test
         |  def testFoo(): Unit = ...
         |""".stripMargin
    }
  }

  /**
    * Error indicating an unhandled effect in an entry point function.
    *
    * @param eff the effect.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointEffect(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0958

    def summary: String = s"Unhandled effect: '${FormatType.formatType(eff)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unhandled effect: '${red(FormatType.formatType(eff))}'.
         |
         |${highlight(loc, "unhandled effect", fmt)}
         |
         |${underline("Explanation:")} Entry point functions (main and tests) can only
         |use primitive effects (like IO) or effects with default handlers. The effect
         |'${magenta(FormatType.formatType(eff))}' has no default handler.
         |
         |To fix this, either:
         |
         |  (a) Handle the effect within the function using 'run-with', or
         |  (b) Add a default handler for the effect.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an entry point function has a non-Unit return type.
    *
    * @param tpe the return type.
    * @param loc the location of the return type.
    */
  case class IllegalEntryPointReturnType(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1403

    def summary: String = s"Unexpected return type for entry point: '${FormatType.formatType(tpe)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected return type '${red(FormatType.formatType(tpe))}' for entry point function.
         |
         |${highlight(loc, "the return type must be Unit", fmt)}
         |
         |${underline("Explanation:")} Entry point functions (main and tests) must return Unit.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an entry point function has type
    * variables in its signature.
    *
    * @param loc the location of the function symbol.
    */
  case class IllegalEntryPointTypeVariables(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1069

    def summary: String = s"Unexpected type variable in entry point."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected type variable in entry point function.
         |
         |${highlight(loc, "type variable not allowed here", fmt)}
         |
         |${underline("Explanation:")} Entry point functions (main and tests) must have
         |concrete types. Type variables like 'a' or 't' are not allowed because the runtime
         |needs to know the exact types at the entry point.
         |""".stripMargin
    }
  }

}
