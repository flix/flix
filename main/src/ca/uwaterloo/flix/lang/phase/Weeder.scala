package ca.uwaterloo.flix.lang.phase

/**
 * The Weeder phase is responsible for:
 *
 * (1) reporting basic syntactic problems, and
 * (2) performing basic syntactic rewritings.
 */
object Weeder {

  sealed trait WeedingErrors

  object WeedingErrors {
    // TODO:
    // - Duplicate variable in pattern.
    // - Duplicate variable in function argument.
    // - Duplicate ???

    // - Disallow Ast.Term.Apply in body of rules.

    // TODO: Shadowing?
  }


}
