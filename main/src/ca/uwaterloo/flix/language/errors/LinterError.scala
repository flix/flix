package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug.PrettyExpression
import ca.uwaterloo.flix.util.Format

/**
 * A common super-type for trivial errors.
 */
sealed trait LinterError extends CompilationMessage {
  val kind = "Lint"
}

object LinterError {

  /**
   * An error raised to indicate that an expression can be simplified.
   *
   * @param sym the symbol of the lint.
   * @param loc the location of the expression.
   */
  case class Lint(sym: Symbol.DefnSym, replacement: Expression, loc: SourceLocation) extends LinterError {
    def summary: String = s"The expression matches the '$sym' lint."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |   __
         |  /  \\        __________________________________________________
         |  |  |       /                                                  \\
         |  @  @       | Hello!                                           |
         |  || |/      | It looks like you are trying to write a program. |
         |  || ||   <--|                                                  |
         |  |\\_/|      | Do you want me to help you with that?            |
         |  \\___/      \\__________________________________________________/
         |
         |>> The ${Format.red(sym.name)} lint applies to the code at:
         |
         |${Format.code(loc, s"matches ${sym.name}.")}
         |""".stripMargin
    }

    override def explain: String = {
      s"""The lint suggests that this code can be replaced by:
         |
         |  ${Format.magenta(PrettyExpression.pretty(replacement))}
         |
         |The lint was declared at: '${Format.cyan(sym.loc.format)}'.
         |""".stripMargin
    }
  }

}
