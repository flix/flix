package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Source, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal


trait UniquenessError extends CompilationError {
  val kind = "Uniqueness Error"
}

object UniquenessError {
/*
  case class DeadSymbol(loc1: SourceLocation, oc2: SourceLocation) extends UniquenessError {
    val source: Source = loc1.source
    val source2: Source = loc2.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Dead symbol. " << NewLine
      vt << NewLine
      vt << Code(loc1, "The symbol is invalid.") << NewLine
      vt << NewLine
      vt << Code(loc2, "The symbol was killed here.") << NewLine
    }
  }*/

  case class DeadSymbol(loc1: SourceLocation) extends UniquenessError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Dead variable. " << NewLine
      vt << NewLine
      vt << Code(loc1, "The variable is dead.") << NewLine
      vt << NewLine
    }
  }

  case class UniquePrimitiveType(loc: SourceLocation) extends UniquenessError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unique primitive type " << NewLine
      vt << NewLine
      vt << Code(loc, "A primitive type cannot be unique.") << NewLine
    }
  }
}