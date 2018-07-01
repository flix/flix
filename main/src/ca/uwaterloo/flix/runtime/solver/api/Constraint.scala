package ca.uwaterloo.flix.runtime.solver.api

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import ca.uwaterloo.flix.runtime.solver.api.predicate.{AtomPredicate, FilterPredicate, Predicate}
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

class Constraint(cparams: List[VarSym], head: Predicate, body: List[Predicate]) {

  /**
    * A head predicate cannot be a filter predicate.
    */
  if (head.isInstanceOf[FilterPredicate]) {
    throw new IllegalArgumentException("A head predicate cannot be a filter predicate.")
  }

  /**
    * A head predicate cannot be a negated atom predicate.
    */
  if (head.isInstanceOf[AtomPredicate]) {
    if (head.asInstanceOf[AtomPredicate].isNegative()) {
      throw new IllegalArgumentException("A head predicate cannot be negated.")
    }
  }

  /**
    * Returns `true` if the constraint is a fact.
    */
  def isFact(): Boolean = body.isEmpty

  /**
    * Returns `true` if the constraint is a rule.
    */
  def isRule(): Boolean = body.nonEmpty

  /**
    * Returns the head predicate.
    */
  def getHeadPredicate(): Predicate = head

  /**
    * Returns the body predicates.
    */
  def getBodyPredicates(): Array[Predicate] = body.toArray

  /**
    * Returns the number of variables in the constraint.
    */
  def getNumberOfParameters(): Int = cparams.length


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

  val hits = new AtomicInteger()

  val time = new AtomicLong()

}