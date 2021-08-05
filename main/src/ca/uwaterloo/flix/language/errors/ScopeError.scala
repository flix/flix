package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for scope errors.
  */
sealed trait ScopeError extends CompilationError {
  val kind = "Scope Error"
}

object ScopeError {
  case class EscapingScopedValue(loc: SourceLocation) extends ScopeError {
    override val summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal // MATT
  }
}
