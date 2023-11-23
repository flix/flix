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

sealed trait EntryPointError extends CompilationMessage {
  val kind: String = "Entry Point Error"
}

object EntryPointError {

  /**
    * Error indicating one or more arguments to an entry point function.
    *
    * @param sym the entry point function.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointArgs(sym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError with Recoverable {

    override def summary: String = s"Unexpected entry point argument(s)."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Arguments to the entry point function are not permitted.
         |
         |${code(loc, "unexpected entry point argument(s).")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Error indicating an illegal result type to an entry point function.
    *
    * @param sym the entry point function.
    * @param tpe the result type.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointResult(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError with Recoverable {

    override def summary: String = s"Unexpected entry point result type: ${FormatType.formatType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The result type: '${red(FormatType.formatType(tpe))}' is not a valid entry point result type.
         |
         |${code(loc, "unexpected entry point result type.")}
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
    * Error indicating the specified entry point is missing.
    *
    * @param sym the entry point function.
    * @param loc the location where the error occurred.
    */
  case class EntryPointNotFound(sym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError with Recoverable {
    override def summary: String = s"Entry point ${sym} not found."

    // NB: We do not print the source location,
    // as it is arbitrary and not related to the error.
    override def message(formatter: Formatter): String = {
      s""">> The entry point ${sym} cannot be found.
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Change the specified entry point to an existing function.
         |  (2)  Add an entry point function ${sym}.
         |
         |""".stripMargin
    })
  }
}
