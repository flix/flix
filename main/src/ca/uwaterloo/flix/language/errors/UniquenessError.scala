package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.{Source, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine}
import ca.uwaterloo.flix.util.vt.VirtualTerminal


trait UniquenessError extends CompilationError {
  val kind = "Syntax Error"
}

object UniquenessError {

  case class DeadSymbol( loc: SourceLocation) extends UniquenessError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Dead symbol. " << NewLine
      vt << NewLine
      vt << Code(loc, "The symbol has already been used.") << NewLine
    }
  }
}