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
    // - Duplicate variable in function argument. lambda/def
    // - Duplicate tag in enum
    // - duplicate name in relation

    // TODO: JoinSemiLattice vs. CompleteLattice.

    // TODO: valid "Traits"

    // - Disallow Ast.Term.Apply in body of rules.
  }


}
