package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

// MATT docs
sealed trait KindError extends CompilationError {
  def kind: String = "Type Error"
}

object KindError {
  case class MismatchedKinds(k1: Kind, k2: Kind, loc: SourceLocation) extends KindError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }
}
