package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.Scheme

object FormatScheme {

  /**
    * Construct a string representation of the type scheme,  e.g.
    * `∀(a, b).a -> Int -> b with Show[a], Eq[b]`
    */
  def formatScheme(sc: Scheme)(implicit audience: Audience): String = {
    val mainPart = formatSchemeWithoutConstraints(sc)

    val tconstrPart =
      if (sc.constraints.isEmpty)
        ""
      else
        " with " + sc.constraints.map(FormatTypeConstraint.formatTypeConstraint).mkString(", ")

    mainPart + tconstrPart
  }

  /**
    * Construct a string representation of the type scheme, excluding type constraints, e.g.,
    * `∀(a, b).a -> Int -> b`
    */
  def formatSchemeWithoutConstraints(sc: Scheme)(implicit audience: Audience): String = {
    val quantifiersPart =
      if (sc.quantifiers.isEmpty)
        ""
      else
        "∀(" + sc.quantifiers.map(FormatType.formatWellKindedType).mkString(", ") + "). "

    val typePart = FormatType.formatWellKindedType(sc.base)

    quantifiersPart + typePart
  }
}
