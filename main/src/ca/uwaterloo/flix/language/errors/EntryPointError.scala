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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for errors produced by [[ca.uwaterloo.flix.language.phase.EntryPoints]].
  */
sealed trait EntryPointError extends CompilationMessage {
  val kind: String = "Entry Point Error"
}

object EntryPointError {

  /**
    * Error indicating an illegal effect of the entry point function.
    *
    * @param eff the effect.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointEffect(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    override def summary: String = s"Unexpected entry point effect: ${FormatType.formatType(eff)}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unhandled effect: '${red(FormatType.formatType(eff))}'.
         |
         |${code(loc, "unhandled effect")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an entry point function has type
    * variables in its signature.
    *
    * @param loc the location of the function symbol.
    */
  case class IllegalEntryPointTypeVariables(loc: SourceLocation) extends EntryPointError {
    def summary: String = s"An entry point function cannot have type variables"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> An entry point function cannot have type variables.
         |
         |${code(loc, "illegal entry point")}
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
    def summary: String = s"Exported functions must have a Java valid name"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must have a Java valid name.
         |
         |${code(loc, "invalid Java name.")}
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
    def summary: String = s"An exported function must be in a module (not in the root namespace)"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> An exported function must be in a module (not in the root namespace).
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function uses an illegal type.
    *
    * @param t the type that is not allowed.
    * @param loc the location of the type.
    */
  case class IllegalExportType(t: Type, loc: SourceLocation) extends EntryPointError {
    def summary: String = s"Exported functions must use primitive Java types or Object, not '$t'"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must use primitive Java types or Object, not '$t'.
         |
         |${code(loc, "unsupported type.")}
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
    override def summary: String = s"Unexpected result type for main: ${FormatType.formatType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The type: '${red(FormatType.formatType(tpe))}' is not a valid result type for the main function.
         |
         |${code(loc, "Unexpected result type for main.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""A ToString instance must be defined for the result type.
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
         |
         |""".stripMargin
    })
  }

  /**
    * Error indicating one or more arguments to a runnable (test or main) entry point function.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRunnableEntryPointArgs(loc: SourceLocation) extends EntryPointError {
    override def summary: String = s"Unexpected entry point argument(s)."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Arguments to the entry point function are not permitted.
         |
         |${code(loc, "unexpected entry point argument(s).")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Error indicating the specified main entry point is missing.
    *
    * @param sym the entry point function.
    */
  case class MainEntryPointNotFound(sym: Symbol.DefnSym) extends EntryPointError {
    override def summary: String = s"Entry point $sym not found."

    // NB: We do not print the symbol source location as it is always Unknown.
    override def message(formatter: Formatter): String = {
      s""">> The entry point $sym cannot be found.
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Change the specified entry point to an existing function.
         |  (2)  Add an entry point function $sym.
         |
         |""".stripMargin
    })

    override def loc: SourceLocation = SourceLocation.Unknown
  }

  /**
    * An error raised to indicate that an exported function is not public.
    *
    * @param loc the location of the defn.
    */
  case class NonPublicExport(loc: SourceLocation) extends EntryPointError {
    def summary: String = s"Exported functions must be public"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported functions must be public.
         |
         |${code(loc, "exported function.")}
         |
         |""".stripMargin
    }
  }
}
