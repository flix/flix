package ca.uwaterloo.flix.language.errors

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
  case class ForbiddenExpression(sym: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9983

    def summary: String = s"Forbidden expression in @Terminates function '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Forbidden expression in @Terminates function '${red(sym.name)}'.
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
  case class NonStrictlyPositiveType(sym: Symbol.DefnSym, caseSym: Symbol.CaseSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9972

    def summary: String = s"Non-strictly positive type in '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Non-strictly positive type in '${red(sym.name)}'.
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
  case class NonStructuralRecursion(sym: Symbol.DefnSym, args: List[TerminationError.ArgInfo], loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9961

    def summary: String = s"Non-structural recursion in '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*

      val headers = List("Parameter", "Argument", "Status")

      def statusText(info: TerminationError.ArgInfo): String = info.status match {
        case TerminationError.ArgStatus.Decreasing       => "strict sub (decreasing)"
        case TerminationError.ArgStatus.NotAVariable      => "not a variable"
        case TerminationError.ArgStatus.Untracked         => "untracked variable"
        case TerminationError.ArgStatus.AliasOf(p)        => s"alias of '$p' (not destructured)"
        case TerminationError.ArgStatus.WrongParam(p)     => s"strict sub of '$p' (wrong position)"
      }

      val rows = args.map(a => List(a.paramName, a.argText, statusText(a)))
      val formatters: List[String => String] = List(
        identity,
        identity,
        s => if (s.contains("decreasing")) green(s) else red(s)
      )

      val tableStr = fmt.table(headers, formatters, rows)

      s""">> Non-structural recursion in '${red(sym.name)}'.
         |
         |${highlight(loc, "non-structural recursive call", fmt)}
         |
         |$tableStr
         |
         |${underline("Explanation:")} A function annotated with @Terminates must be structurally
         |recursive. Every recursive call must pass a strict substructure of a formal
         |parameter as an argument. A strict substructure is a variable bound inside a
         |pattern match on the parameter (e.g. the tail of a list).
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
    case class AliasOf(param: String) extends ArgStatus

    /** The argument is a strict sub of a *different* parameter than the one in this position. */
    case class WrongParam(actualParam: String) extends ArgStatus
  }

  /** Per-argument diagnostic for a non-structural recursive call. */
  case class ArgInfo(paramName: String, argText: String, status: ArgStatus)

}
