/*
 * Copyright 2022 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
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
    * An error raised to indicate that a default handler is not in the companion module of its effect.
    *
    * @param handlerSym the symbol of the default handler.
    * @param loc        the location of the default handler.
    */
  case class DefaultHandlerNotInModule(handlerSym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0621

    def summary: String = s"The default handler '${handlerSym.name}' is not in the companion module of an effect."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The default handler '${red(handlerSym.name)}' is not in the companion module of an effect.
         |
         |${src(loc, "default handler.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that there are multiple default handlers for the same effect.
    *
    * @param sym  the symbol of the effect.
    * @param loc1 the location of the first default handler.
    * @param loc2 the location of the second default handler.
    */
  case class DuplicateDefaultHandler(sym: Symbol.EffSym, loc1: SourceLocation, loc2: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0734

    def summary: String = s"Duplicate default handler for '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate default handler for '${red(sym.toString)}'.
         |
         |${src(loc1, "the first default handler was here.")}
         |
         |${src(loc2, "the second default handler was here.")}
         |
         |""".stripMargin
    }

    override def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the signature of a default handler is illegal.
    *
    * @param effSym     the symbol of the effect.
    * @param handlerSym the symbol of the handler.
    * @param loc        the location of the default handler.
    */
  case class IllegalDefaultHandlerSignature(effSym: Symbol.EffSym, handlerSym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0847

    def summary: String = s"Illegal signature for '$effSym's default handler."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal default effect handler signature for '${red(effSym.toString)}'.
         |
         |The default handler for '${red(effSym.toString)}' should have the exact signature:
         |
         |  pub def ${handlerSym.name}(f: Unit -> a \\ ef) : a \\ (ef - ${effSym.name}) + IO
         |
         |The default handler was declared here:
         |
         |${src(loc, "illegal signature.")}
         |
         |""".stripMargin
    }
  }

  /**
    * Error indicating an illegal effect of an entry point function.
    *
    * @param eff the effect.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointEffect(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0958

    override def summary: String = s"Unexpected entry point effect: ${FormatType.formatType(eff)}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unhandled effect: '${red(FormatType.formatType(eff))}'.
         |
         |${src(loc, "unhandled effect")}
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

    def summary: String = s"An entry point function cannot have type variables"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> An entry point function cannot have type variables.
         |
         |${src(loc, "illegal entry point")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function has an invalid name.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportName(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1172

    def summary: String = s"Exported functions must have a Java valid name"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must have a Java valid name.
         |
         |${src(loc, "invalid Java name.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function has an illegal namespace.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportNamespace(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1285

    def summary: String = s"An exported function must be in a module (not in the root namespace)"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> An exported function must be in a module (not in the root namespace).
         |
         |${src(loc, "exported function.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function uses an illegal type.
    *
    * @param t   the type that is not allowed.
    * @param loc the location of the type.
    */
  case class IllegalExportType(t: Type, loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1396

    def summary: String = s"Exported functions must use primitive Java types or Object, not '$t'"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must use primitive Java types or Object, not '$t'.
         |
         |${src(loc, "unsupported type.")}
         |
         |""".stripMargin
    }
  }

  /**
    * Error indicating an illegal result type to the main entry point function.
    *
    * @param tpe the result type.
    * @param loc the location where the error occurred.
    */
  case class IllegalMainEntryPointResult(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1403

    override def summary: String = s"Unexpected result type for main: ${FormatType.formatType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The type: '${red(FormatType.formatType(tpe))}' is not a valid result type for the main function.
         |
         |${src(loc, "Unexpected result type for main.")}
         |
         |${underline("Explanation:")}
         |A ToString instance must be defined for the result type.
         |
         |To define a string representation of '${FormatType.formatType(tpe)}', either:
         |
         |  (a) define an instance of ToString for '${FormatType.formatType(tpe)}', or
         |  (b) derive an instance of ToString for '${FormatType.formatType(tpe)}'.
         |
         |To automatically derive an instance, you can write:
         |
         |  enum Color with ToString {
         |    case Red, Green, Blue
         |  }
         |""".stripMargin
    }
  }

  /**
    * Error indicating one or more arguments to a runnable (test or main) entry point function.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRunnableEntryPointArgs(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1512

    override def summary: String = s"Unexpected entry point argument(s)."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Arguments to the entry point function are not permitted.
         |
         |${src(loc, "unexpected entry point argument(s).")}
         |""".stripMargin
    }
  }

  /**
    * Error indicating the specified main entry point is missing.
    *
    * @param sym the entry point function.
    */
  case class MainEntryPointNotFound(sym: Symbol.DefnSym) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1625

    override def summary: String = s"Entry point $sym not found."

    // NB: We do not print the symbol source location as it is always Unknown.
    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The entry point $sym cannot be found.
         |
         |${underline("Possible fixes:")}
         |
         |  (1)  Change the specified entry point to an existing function.
         |  (2)  Add an entry point function $sym.
         |""".stripMargin
    }

    override def loc: SourceLocation = SourceLocation.Unknown
  }

  /**
    * An error raised to indicate that a default handler is not public.
    *
    * @param handlerSym the symbol of the handler.
    * @param loc        the location of the handler.
    */
  case class NonPublicDefaultHandler(handlerSym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1738

    def summary: String = s"The default handler '${handlerSym.name}' must be public (`pub`)"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The default handler '${red(handlerSym.name)}' must be public (`${cyan("pub")}`).
         |
         |${src(loc, "non-public default handler.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function is not public.
    *
    * @param loc the location of the defn.
    */
  case class NonPublicExport(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1849

    def summary: String = s"Exported functions must be public"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must be public.
         |
         |${src(loc, "exported function.")}
         |
         |""".stripMargin
    }
  }

}
