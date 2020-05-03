package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for safety errors.
  */
sealed trait SafetyError extends CompilationError

object SafetyError {

  /**
    * An error raised to indicate an illegal use of a non-positively bound variable in a negative atom.
    */
  case class IllegalNonPositivelyBoundVariable(sym: Symbol.VarSym, loc: SourceLocation) extends SafetyError {
    def kind: String = "Safety Error"
    def summary: String = s"Illegal non-positively bound variable '$sym'."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal non-positively bound variable '" << Red(sym.text) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "the variable occurs in this negated atom.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Ensure that the variable occurs in at least one positive atom." << NewLine
    }
  }

}
