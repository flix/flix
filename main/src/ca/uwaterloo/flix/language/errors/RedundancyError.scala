package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Cyan, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

trait RedundancyError extends CompilationError {
  def kind: String = "Redundancy Error"
}

object RedundancyError {

  /**
    * An error raised to indicate that the given formal parameter `varSym` is not used within `defSym`.
    *
    * @param varSym    the unused variable symbol.
    * @param defSymOpt the optional definition symbol.
    */
  case class UnusedFormalParam(varSym: Symbol.VarSym, defSymOpt: Option[Symbol.DefnSym]) extends RedundancyError {
    val source: Source = varSym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      defSymOpt match {
        case None =>
          vt << ">> Unused formal parameter '" << Red(varSym.text) << "'. The parameter is not used inside the function." << NewLine
        case Some(defSym) =>
          vt << ">> Unused formal parameter '" << Red(varSym.text) << "'. The parameter is not used inside '" << Cyan(defSym.text) << "'." << NewLine
      }
      vt << NewLine
      vt << Code(varSym.loc, "unused formal parameter.") << NewLine
      vt << NewLine
      vt << "Remove the formal parameter or use it inside the function."
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given variable symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedVarSym(sym: Symbol.VarSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused local variable '" << Red(sym.text) << "'. The variable is not referenced within its scope." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused local variable.") << NewLine
      vt << NewLine
      vt << "Remove the variable declaration or use the variable within its scope."
      vt << NewLine
      vt
    }
  }


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
