package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.tools.fmt.rules.{BinaryExpressionRules, FormatterModule, ImportRules, ParameterListRules}

object FormatterRegistry {
  val modules: List[FormatterModule] = List(
    ImportRules,
    ParameterListRules,
    BinaryExpressionRules
  )
}
