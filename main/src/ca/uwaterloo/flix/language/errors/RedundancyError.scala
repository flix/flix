package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

trait RedundancyError extends CompilationError {
  def kind: String = "Redundancy Error"
}

object RedundancyError {

  /**
    */
  case class Dead(loc: SourceLocation) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Redundancy" << NewLine
      vt << NewLine
      vt << Code(loc, "impossible.") << NewLine
      vt << NewLine
      vt
    }
  }


  case class ImpossibleMatch(loc1: SourceLocation, loc2: SourceLocation) extends RedundancyError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Impossible pattern match due to prior pattern match." << NewLine
      vt << NewLine
      vt << Code(loc2, "impossible due to prior match.") << NewLine
      vt << NewLine
      vt << ">> Previous match ensure that this cannot match" << NewLine
      vt << NewLine
      vt << ">> First pattern match was here:" << NewLine
      vt << Code(loc1, "first match.") << NewLine
      vt << ">> Second pattern match was here:" << NewLine
      vt << Code(loc2, "second match.") << NewLine
      vt << NewLine
      vt
    }
  }

  // TODO: Unused variable.

  // TODO: Unused type parameter.

  // TODO: Unused algebraic data type.

  // TODO: Unused ...

}
