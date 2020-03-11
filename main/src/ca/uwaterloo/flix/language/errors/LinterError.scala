package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug.PrettyExpression
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for trivial errors.
  */
sealed trait LinterError extends CompilationError {
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
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format)
      vt << s"   __" << NewLine
      vt << s"  /  \\        __________________________________________________ " << NewLine
      vt << s"  |  |       /                                                  \\" << NewLine
      vt << s"  @  @       | Hello!                                           |" << NewLine
      vt << s"  || |/      | It looks like you are trying to write a program. |" << NewLine
      vt << s"  || ||   <--|                                                  |" << NewLine
      vt << s"  |\\_/|      | Do you want me to help you with that?            |" << NewLine
      vt << s"  \\___/      \\__________________________________________________/" << NewLine
      vt << NewLine
      vt << ">> The " << Red(sym.name) << " lint applies to the code at: " << NewLine
      vt << NewLine
      vt << Code(loc, s"matches ${sym.name}.") << NewLine
      vt << "The lint suggests that this code can be replaced by: " << NewLine
      vt << NewLine
      vt << "  " << Magenta(PrettyExpression.pretty(replacement)) << NewLine
      vt << NewLine
      vt << "The lint was declared at: '" << Cyan(sym.loc.format) << "'." << NewLine
    }
  }

}
