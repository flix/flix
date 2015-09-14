package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.ParsedAst

import util.Validation

/**
 * The Weeder phase is responsible for:
 *
 * (1) reporting basic syntactic problems, and
 * (2) performing basic syntactic rewritings.
 */
object Weeder {

  sealed trait WeederError

  object WeederError {

    /**
     *
     */
    case class DuplicateTag() extends WeederError

    // TODO:
    // - Duplicate variable in pattern.
    // - Duplicate variable in function argument. lambda/def
    // - Duplicate tag in enum
    // - duplicate name in relation

    // TODO: JoinSemiLattice vs. CompleteLattice.

    // TODO: valid "Traits"
    // TODO: Allow nested lattice types? <<foo>> ?

    // rewrite all functions to lambdas of one argument?

    // - Disallow Ast.Term.Apply in body of rules.
    // TODO Disallow Term.Tag in body of rules. Is this always monotone? does it depend on the arguments??? hmm...

  }


  def compile(past: ParsedAst.Declaration.Enum): Validation[ParsedAst.Declaration.Enum, WeederError] =
    ???

}
