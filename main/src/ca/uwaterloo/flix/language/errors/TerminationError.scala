package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

// MATT license
// MATT docs
sealed trait TerminationError extends CompilationError {
  override def kind: String = "Termination Error"
}

object TerminationError {
  case class UnconditionalDefRecursion(defn: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }

  case class UnconditionalSigRecursion(sig: Symbol.SigSym, loc: SourceLocation) extends TerminationError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }
}
