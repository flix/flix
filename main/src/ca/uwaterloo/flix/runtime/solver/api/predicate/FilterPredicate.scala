package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.term.{Term, WildTerm}

/**
  * Represents a filter predicate with function `f` and arguments `terms`.
  */
class FilterPredicate(f: Array[AnyRef] => Boolean, args: Array[Term]) extends Predicate {

  // TODO: Maybe the args should just be variables?

  /**
    * Invariant: A filter predicate cannot have wild card terms as arguments.
    */
  for (t <- args) {
    if (t.isInstanceOf[WildTerm]) {
      throw new IllegalArgumentException("A filter predicate cannot take a wild card term as an argument.")
    }
  }

  /**
    * Returns a copy of this predicate.
    */
  override def copy(): Predicate = new FilterPredicate(f, args)

  /**
    * Returns the function.
    */
  def getFunction(): Array[AnyRef] => Boolean = f

  /**
    * Returns the arguments.
    */
  def getArguments(): Array[Term] = args

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "<<filter>>" + "(" + args.mkString(", ") + ")"

}
