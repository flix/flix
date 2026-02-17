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
    * @param sym the symbol of the function.
    * @param loc the source location of the recursive call.
    */
  case class NonStructuralRecursion(sym: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    def code: ErrorCode = ErrorCode.E9961

    def summary: String = s"Non-structural recursion in '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Non-structural recursion in '${red(sym.name)}'.
         |
         |${highlight(loc, "non-structural recursive call", fmt)}
         |
         |${underline("Explanation:")} A function annotated with @Terminates must be structurally
         |recursive. Every recursive call must pass a strict substructure of a formal
         |parameter as an argument. A strict substructure is a variable bound inside a
         |pattern match on the parameter (e.g. the tail of a list).
         |""".stripMargin
    }
  }

}
