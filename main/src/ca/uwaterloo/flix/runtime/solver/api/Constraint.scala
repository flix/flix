package ca.uwaterloo.flix.runtime.solver.api

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import ca.uwaterloo.flix.runtime.solver.api.predicate.{AtomPredicate, FilterPredicate, Predicate}

case class Constraint(cparams: List[ConstraintParam], head: Predicate, body: List[Predicate]) {

  // TODO: Head predicate cannot be filter,
  // TODO: Head predicate cannot be negated atom.

  /**
    * Returns the arity of the constraint.
    *
    * The arity of a constraint is the number of constraint parameters (i.e. variables in the constraint).
    * Not to be confused with the number of predicates or terms.
    */
  val arity: Int = cparams.length

  /**
    * Returns `true` if the constraint is a fact.
    */
  val isFact: Boolean = body.isEmpty

  /**
    * Returns `true` if the constraint is a rule.
    */
  val isRule: Boolean = body.nonEmpty

  /**
    * Returns the atoms predicates in the body of the constraint.
    */
  val atoms: List[AtomPredicate] = body.collect {
    case p: AtomPredicate => p
  }

  /**
    * Returns the filter predicates in the body of the constraint.
    */
  val filters: Array[FilterPredicate] = body.collect {
    case p: FilterPredicate => p
  }.toArray

  /**
    * Records the number of times this rule has been evaluated.
    */
  val hits = new AtomicInteger()

  /**
    * Records the amount of time spent evaluating this rule.
    */
  val time = new AtomicLong()

}