package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.SourceLocation
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
    * @param loc the location of the expression.
    */
  case class Simplify(msg: String, loc: SourceLocation) extends LinterError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << s"   __" << NewLine
      vt << s"  /  \\        _____________ " << NewLine
      vt << s"  |  |       /             \\" << NewLine
      vt << s"  @  @       | Hello!       |" << NewLine
      vt << s"  || |/      | $msg         |" << NewLine
      vt << s"  || ||   <--|              |" << NewLine
      vt << s"  |\\_/|      |              |" << NewLine
      vt << s"  \\___/      \\_____________/" << NewLine
      vt
    }
  }

}
