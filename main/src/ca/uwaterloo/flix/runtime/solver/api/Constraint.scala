package ca.uwaterloo.flix.runtime.solver.api

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import ca.uwaterloo.flix.runtime.solver.api.predicate.{AtomPredicate, FilterPredicate, FunctionalPredicate, Predicate}
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents a constraint.
  *
  * A constraint is a horn clause that consists of
  * a sequence of universally quantified variables,
  * a head predicate, and a sequence of body predicates.
  */
class Constraint(cparams: Array[VarSym], head: Predicate, body: Array[Predicate]) {

  /**
    * A head predicate cannot be a filter predicate.
    */
  if (head.isInstanceOf[FilterPredicate]) {
    throw new IllegalArgumentException("A head predicate cannot be a filter predicate.")
  }

  /**
    * A head predicate cannot be a functional predicate.
    */
  if (head.isInstanceOf[FunctionalPredicate]) {
    throw new IllegalArgumentException("A head predicate cannot be a functional predicate.")
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
    * Returns the constraint parameters.
    */
  def getParams(): Array[VarSym] = cparams

  /**
    * Returns the head predicate.
    */
  def getHeadPredicate(): Predicate = head

  /**
    * Returns the body predicates.
    */
  def getBodyPredicates(): Array[Predicate] = body

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
  def incrementNumberOfHits(): Unit = {
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
  def getAtoms(): Array[AtomPredicate] = body.collect {
    case p: AtomPredicate => p
  }

  /**
    * Returns the filter predicates in the body of the constraint.
    */
  def getFilters(): Array[FilterPredicate] = body.collect {
    case p: FilterPredicate => p
  }

  /**
    * Returns the functional predicates in the body of the constraint.
    */
  def getFunctionals(): Array[FunctionalPredicate] = body.collect {
    case p: FunctionalPredicate => p
  }

  /**
    * Returns a string representation of `this` constraint.
    */
  override def toString: String = {
    if (isFact())
      head.toString + "."
    else
      head.toString + " :- " + body.mkString(", ") + "."
  }

}