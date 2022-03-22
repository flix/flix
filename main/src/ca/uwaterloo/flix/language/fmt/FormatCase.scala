package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.TypedAst

object FormatCase {

  /**
    * Returns a markdown string for the given `caze`.
    */
  def asMarkDown(caze: TypedAst.Case)(implicit audience: Audience): String = {
    s"case **${caze.tag.name}**: ${FormatScheme.formatScheme(caze.sc)}"
  }

}
