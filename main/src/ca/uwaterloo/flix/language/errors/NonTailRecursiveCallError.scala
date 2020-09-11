package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine, Red, Underline}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

case class NonTailRecursiveCallError(loc: SourceLocation) extends CompilationError {
  /**
    * Returns the kind of error message, e.g. "Syntax Error" or "Type Error".
    */
  override def kind: String = "Non-tail-recursive call Error."

  /**
    * Returns a short description of the error message.
    */
  override def summary: String = "Non-tail-recursive call."

  /**
    * Returns the formatted error message.
    */
  override def message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Non-tail-recursive call."
    vt << NewLine
    vt << Code(loc, "call not in tail position.")
    vt << NewLine
    vt << Underline("Tip:") << " Refactor the function to use tail recursion, or remove the @tailrec annotation." << NewLine
  }
}
