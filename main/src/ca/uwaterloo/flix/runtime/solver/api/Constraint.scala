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
    * Assign a number to each constraint parameter in the current constraint.
    */
  private var offset = 0
  for (cparam <- cparams) {
    cparam.setStackOffset(offset)
    offset = offset + 1
  }

  /**
    * Numbers of times the constraint has been evaluated.
    */
  private val hits = new AtomicInteger()

  /**
    * Number of nanoseconds spent during evaluation of the constraint.
    */
  private val time = new AtomicLong()

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
    * Returns the number of times the constraint has been evaluated.
    */
  def getNumberOfHits(): Int = hits.get()

  /**
    * Increments the number of times the constraint has been evaluated.
    */
  def incrementNumberOfUnits(): Unit = {
    hits.getAndIncrement()
  }

  /**
    * Returns the number of nanoseconds spent during evaluation of the constraint.
    */
  def getElapsedTime(): Long = time.get()

  /**
    * Increments the number of times the constraint has been evaluated.
    */
  def incrementElapsedTime(ns: Long): Unit = {
    time.addAndGet(ns)
  }

  /**
    * Returns the atoms predicates in the body of the constraint.
    */
  def getAtoms(): List[AtomPredicate] = body.collect {
    case p: AtomPredicate => p
  }

  /**
    * Returns the filter predicates in the body of the constraint.
    */
  def getFilters(): Array[FilterPredicate] = body.collect {
    case p: FilterPredicate => p
  }.toArray

}