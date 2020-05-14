package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Scheme

object FormatScheme {
  def formatScheme(scheme: Scheme)(implicit audience: Audience): String = {
    if (scheme.quantifiers.isEmpty)
      FormatType.formatType(scheme.base)
    else
      s"∀(${scheme.quantifiers.map(tvar => tvar.getText.getOrElse(tvar.id)).mkString(", ")}). ${FormatType.formatType(scheme.base)}"
  }
}
