package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for trivial errors.
  */
sealed trait TrivialError extends CompilationError {
  val kind = "Trivial Error"
}

object TrivialError {

  /**
    * An error raised to indicate that an expression is trivial.
    *
    * @param loc the location of the expression.
    */
  case class TrivialExpression(loc: SourceLocation) extends TrivialError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Trivial expression: It performs a suspicious computation." << NewLine
      vt << NewLine
      vt << Code(loc, "trivial expression.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Ensure that there is not a typo in the expression." << NewLine
      vt << "  (2)  Simplify the expression to remove the redundancy." << NewLine
      vt << NewLine
      vt
    }
  }

}
