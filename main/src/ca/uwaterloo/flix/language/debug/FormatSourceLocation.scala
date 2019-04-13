package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.SourceLocation

object FormatSourceLocation {

  def format(loc: SourceLocation): String = loc.source.format + ":" + loc.beginLine

}
