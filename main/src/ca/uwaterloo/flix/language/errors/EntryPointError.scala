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
    * Error indicating an unhandled effect in an entry point function.
    *
    * @param eff the effect.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointEffect(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E0958

    def summary: String = s"Unhandled effect: '${FormatType.formatType(eff)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unhandled effect: '${red(FormatType.formatType(eff))}'.
         |
         |${src(loc, "unhandled effect")}
         |
         |${underline("Explanation:")} Entry point functions (main, tests, exports) can only
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
    * An error raised to indicate that an entry point function has type
    * variables in its signature.
    *
    * @param loc the location of the function symbol.
    */
  case class IllegalEntryPointTypeVariables(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1069

    def summary: String = s"Unexpected type variable in entry point."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type variable in entry point function.
         |
         |${src(loc, "type variable not allowed here")}
         |
         |${underline("Explanation:")} Entry point functions (main, tests, exports) must have
         |concrete types. Type variables like 'a' or 't' are not allowed because the runtime
         |needs to know the exact types at the entry point.
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

    def summary: String = s"Unexpected name for exported function."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected name for exported function.
         |
         |${src(loc, "name not valid in Java")}
         |
         |${underline("Explanation:")} Exported functions must have names that are valid Java
         |identifiers. A valid name starts with a lowercase letter and contains only letters
         |and digits (e.g., 'getValue', 'process123').
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function is in the root namespace.
    *
    * @param loc the location of the defn.
    */
  case class IllegalExportNamespace(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1285

    def summary: String = s"Exported function in root namespace."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported function must be in a module.
         |
         |${src(loc, "function in root namespace")}
         |
         |${underline("Explanation:")} Exported functions generate Java methods in a class
         |named after the module. Functions in the root namespace have no module name,
         |so there is no class to contain the exported method.
         |
         |To fix this, move the function into a module:
         |
         |  mod MyModule {
         |      @Export
         |      pub def myFunction(): Int32 = ...
         |  }
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an exported function uses an unexpected type.
    *
    * @param t   the type that is not allowed.
    * @param loc the location of the type.
    */
  case class IllegalExportType(t: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1396

    def summary: String = s"Unexpected type in exported function: '${FormatType.formatType(t)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type '${red(FormatType.formatType(t))}' in exported function.
         |
         |${src(loc, "type not exportable")}
         |
         |${underline("Explanation:")} Exported functions can only use primitive Java types:
         |
         |  Bool, Char, Int8, Int16, Int32, Int64, Float32, Float64, or java.lang.Object
         |""".stripMargin
    }
  }

  /**
    * Error indicating an unexpected result type for the main entry point function.
    *
    * @param tpe the result type.
    * @param loc the location where the error occurred.
    */
  case class IllegalMainEntryPointResult(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1403

    def summary: String = s"Unexpected result type for main: '${FormatType.formatType(tpe)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected result type '${red(FormatType.formatType(tpe))}' for main.
         |
         |${src(loc, "type has no ToString instance")}
         |
         |${underline("Explanation:")} The main function must return Unit or a type with a
         |ToString instance so the result can be printed.
         |
         |To fix this, either:
         |
         |  (a) Change the return type to Unit,
         |  (b) Define an instance of ToString for '${magenta(FormatType.formatType(tpe))}', or
         |  (c) Derive an instance of ToString for '${magenta(FormatType.formatType(tpe))}'.
         |""".stripMargin
    }
  }

  /**
    * Error indicating unexpected arguments in a runnable (test or main) entry point function.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRunnableEntryPointArgs(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1512

    def summary: String = s"Unexpected arguments in entry point."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected arguments in entry point function.
         |
         |${src(loc, "arguments not allowed")}
         |
         |${underline("Explanation:")} Entry point functions (main and tests) are called by
         |the runtime without arguments. They must take a single Unit parameter.
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
    * Error indicating the specified main entry point is missing.
    *
    * @param sym the entry point function.
    */
  case class MainEntryPointNotFound(sym: Symbol.DefnSym) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1625

    def summary: String = s"Entry point '${sym.name}' not found."

    // NB: We do not print the symbol source location as it is always Unknown.
    def message(formatter: Formatter): String = {
      import formatter.*
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
    * An error raised to indicate that an exported function is not public.
    *
    * @param loc the location of the defn.
    */
  case class NonPublicExport(loc: SourceLocation) extends EntryPointError {
    def code: ErrorCode = ErrorCode.E1849

    def summary: String = s"Non-public exported function."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Exported function is not public.
         |
         |${src(loc, "missing 'pub' modifier")}
         |
         |${underline("Explanation:")} Exported functions must be declared with the 'pub'
         |modifier to be visible from Java code. Private functions cannot be exported
         |because they are not accessible outside their module.
         |
         |To fix this, add the 'pub' modifier:
         |
         |  @Export
         |  pub def myFunction(): Int32 = ...
         |""".stripMargin
    }
  }

}
