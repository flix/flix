package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Ast

// MATT docs
// MATT license
object FormatTypeConstraint {
  def formatTypeConstraint(tconstr: Ast.TypeConstraint)(implicit audience: Audience): String = tconstr match {
    case Ast.TypeConstraint(sym, arg, _) =>
      val typeString = FormatType.formatType(arg)
      s"${sym.name}[${typeString}]"
  }
}
