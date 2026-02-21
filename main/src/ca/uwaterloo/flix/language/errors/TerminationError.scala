/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.shared.QualifiedSym
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

/** A common super-type for termination errors. */
sealed trait TerminationError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.TerminationError
}

object TerminationError {

  /**
    * An error raised when a forbidden expression appears in a @Terminates function.
    *
    * @param sym the symbol of the function.
    * @param loc the source location of the forbidden expression.
    */
  case class ForbiddenExpression(sym: QualifiedSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9983

    def summary: String = s"Forbidden expression in @Terminates function '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Forbidden expression in @Terminates function '${magenta(sym.name)}'.
         |
         |${highlight(loc, "forbidden expression", fmt)}
         |
         |${underline("Explanation:")} A function annotated with @Terminates must not use features
         |that could break the termination guarantee.
         |""".stripMargin
    }
  }

  /**
    * An error raised when the decreasing argument's type is not strictly positive.
    *
    * @param sym     the symbol of the function.
    * @param caseSym the enum case that contains a recursive occurrence in a negative position.
    * @param loc     the source location of the type.
    */
  case class NonStrictlyPositiveType(sym: QualifiedSym, caseSym: Symbol.CaseSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9972

    def summary: String = s"Non-strictly positive type in '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Non-strictly positive type in '${magenta(sym.name)}'.
         |
         |${highlight(loc, "non-strictly positive type", fmt)}
         |
         |The enum case '${red(caseSym.name)}' contains a recursive occurrence in a negative position.
         |
         |${highlight(caseSym.loc, "negative occurrence", fmt)}
         |
         |${underline("Explanation:")} A function annotated with @Terminates requires that any
         |type used for structural recursion is strictly positive. A type is not strictly
         |positive if it contains a recursive occurrence in a negative position (e.g.,
         |to the left of an arrow in a function type). This would allow circumventing
         |the termination guarantee.
         |""".stripMargin
    }
  }


  /**
    * An error raised when a recursive call is not on a strict substructure of a formal parameter.
    *
    * @param sym  the symbol of the function.
    * @param args per-argument diagnostics explaining why the call is non-structural.
    * @param loc  the source location of the recursive call.
    */
  case class NonStructuralRecursion(sym: QualifiedSym, args: List[TerminationError.ArgInfo], loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9961

    def summary: String = s"Non-structural recursion in '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*

      val headers = List("Parameter", "Argument", "Status")

      def statusText(info: TerminationError.ArgInfo): String = info.status match {
        case TerminationError.ArgStatus.Decreasing        => s"strict sub (decreasing)"
        case TerminationError.ArgStatus.NotAVariable      => s"not a variable"
        case TerminationError.ArgStatus.Untracked         => s"untracked variable"
        case TerminationError.ArgStatus.AliasOf(p)        => s"alias of '${p.text}' (not destructured)"
        case TerminationError.ArgStatus.WrongParam(p)     => s"strict sub of '${p.text}' (wrong position)"
      }

      val rows = args.map(a => List(a.paramName, a.argText, statusText(a)))
      val formatters: List[String => String] = List(
        identity,
        identity,
        s => if (s.contains("decreasing")) green(s) else red(s)
      )

      val tableStr = fmt.table(headers, formatters, rows)

      s""">> Non-structural recursion in '${magenta(sym.name)}'.
         |
         |${highlight(loc, "non-structural recursive call", fmt)}
         |
         |$tableStr
         |
         |${underline("Explanation:")} A function annotated with @Terminates must be structurally
         |recursive. Every recursive call must pass a strict substructure of a formal
         |parameter as an argument. A strict substructure is a variable bound inside a
         |pattern match on the parameter (e.g. the tail of a list). To fix this, ensure
         |each recursive call argument is a variable bound by pattern matching on the
         |corresponding parameter.
         |""".stripMargin
    }
  }

  /**
    * An error raised when a self-recursive call in a `@TailRec` function is not in tail position.
    *
    * @param sym the symbol of the function.
    * @param loc the source location of the non-tail recursive call.
    */
  case class NonTailRecursiveCall(sym: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9939

    def summary: String = s"Non-tail recursive call in @TailRec function '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Non-tail recursive call in @TailRec function '${magenta(sym.name)}'.
         |
         |${highlight(loc, "non-tail recursive call", fmt)}
         |
         |${underline("Explanation:")} A function annotated with @TailRec must only call itself
         |in tail position. This call is not in tail position because its result is used
         |in a further computation (e.g. passed to another function or combined with
         |another expression).
         |""".stripMargin
    }
  }

  /**
    * An error raised when a `@Terminates` function calls a def that is not annotated with `@Terminates`.
    *
    * @param sym       the symbol of the enclosing `@Terminates` function.
    * @param calledSym the symbol of the called non-`@Terminates` function.
    * @param loc       the source location of the call.
    */
  case class NonTerminatingCall(sym: QualifiedSym, calledSym: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9950

    def summary: String = s"Call to non-@Terminates function '${calledSym.name}' in @Terminates function '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Call to non-@Terminates function '${red(calledSym.name)}' in @Terminates function '${magenta(sym.name)}'.
         |
         |${highlight(loc, "non-terminating call", fmt)}
         |
         |${underline("Explanation:")} A function annotated with @Terminates may only call other
         |functions that are also annotated with @Terminates. The function '${calledSym.name}' is
         |not annotated with @Terminates.
         |""".stripMargin
    }
  }

  /** Describes why a single argument in a recursive call does or does not satisfy the
    * structural recursion requirement for its corresponding formal parameter. */
  sealed trait ArgStatus

  object ArgStatus {
    /** The argument is a strict substructure of the correct formal parameter. */
    case object Decreasing extends ArgStatus

    /** The argument is not a simple variable — it is a complex expression. */
    case object NotAVariable extends ArgStatus

    /** The argument variable is not tracked (not derived from any formal parameter). */
    case object Untracked extends ArgStatus

    /** The argument is an alias (or renaming) of the given parameter — not destructured. */
    case class AliasOf(param: Symbol.VarSym) extends ArgStatus

    /** The argument is a strict sub of a *different* parameter than the one in this position. */
    case class WrongParam(actualParam: Symbol.VarSym) extends ArgStatus
  }

  /** Per-argument diagnostic for a non-structural recursive call. */
  case class ArgInfo(paramName: String, argText: String, status: ArgStatus)

}
