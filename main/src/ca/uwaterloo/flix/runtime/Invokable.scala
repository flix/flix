package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.IValue

/**
  * A Java functional interface for JVM methods that are invokable by Flix.
  */
@FunctionalInterface
abstract class Invokable {

  // TODO: Introduce Safe and UnsafeInvokable.

  /**
    * Invokes the method with the given Flix arguments `args`.
    *
    * The method *must* return a Flix value. Returning `null` corresponds to the `None` object of the `Opt` type.
    *
    * The method *must not* throw any exception.
    *
    * The arguments array `args` is guaranteed never to be `null`.
    *
    * An entry in the arguments array may be `null`. If so, it corresponds to `None` of the `Opt` type.
    */
  def apply(args: Array[IValue]): IValue

}
