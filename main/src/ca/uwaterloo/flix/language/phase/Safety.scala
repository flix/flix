package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.{Constraint, Predicate, Root, Stratum}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * Performs safety and well-formedness checks on a typed ast.
  */
object Safety extends Phase[Root, Root] {

  /**
    * Performs safety and well-formedness checks on a typed ast.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Safety") {
    //
    // Check each stratum for safety errors.
    //
    val strataErrors = root.strata flatMap checkStratum

    //
    // Combine all errors.
    //
    val allErrors = strataErrors

    //
    // Check if any errors were detected.
    //
    if (allErrors.isEmpty)
      root.toSuccess
    else
      Validation.Failure(allErrors.toStream)
  }

  /**
    * Performs safety and well-formedness checks on the given stratum `st0`.
    */
  private def checkStratum(st0: Stratum): List[CompilationError] = {
    st0.constraints flatMap checkConstraint
  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint): List[CompilationError] = {
    //
    // Compute the set of positively defined variable symbols in the constraint.
    //
    val posVars = positivelyDefinedVariables(c0)

    //
    // Check that all negative atoms only use positively defined variable symbols.
    //
    c0.body.flatMap(checkBodyPredicate(_, posVars))
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym]): List[CompilationError] = p0 match {
    case Predicate.Body.Atom(sym, polarity, terms, loc) => polarity match {
      case Polarity.Positive => Nil
      case Polarity.Negative =>
        // Compute the free variables in the terms.
        val freeVars = terms.flatMap(freeVarsOf).toSet

        // Check if any free variables are not positively bound.
        ((freeVars -- posVars) map {
          case unboundVar => SafetyError.IllegalNonPositivelyBoundVariable(unboundVar, p0.loc)
        }).toList
    }
    case Predicate.Body.Filter(sym, terms, loc) => Nil
    case Predicate.Body.Loop(sym, term, loc) => Nil
  }

  /**
    * Returns all the positively defined variable symbols in the given constraint `c0`.
    */
  private def positivelyDefinedVariables(c0: Constraint): Set[Symbol.VarSym] = c0.body.flatMap(positivelyDefinedVariables).toSet

  /**
    * Returns all positively defiend variable symbols in the given body predicate `p0`.
    */
  private def positivelyDefinedVariables(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(sym, polarity, terms, loc) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }
    case Predicate.Body.Filter(sym, terms, loc) => Set.empty
    case Predicate.Body.Loop(sym, term, loc) => Set.empty
  }

}
