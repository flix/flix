package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.SourceLocation

object FormatSourceLocation {

  def format(loc: SourceLocation): String = loc.source.name + ":" + loc.beginLine

}
