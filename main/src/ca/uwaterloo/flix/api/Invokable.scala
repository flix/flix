package ca.uwaterloo.flix.api

/**
  * A Java functional interface for JVM methods that are invokable by Flix.
  */
@FunctionalInterface
trait Invokable {

  /**
    * Invokes the method with the given Flix arguments `args`.
    *
    * The method *must* return a Flix value. Returning `null` corresponds to the `None` object of the `Opt` type.
    *
    * The method *must not* throw any exception.
    *
    * The arguments array `args` is guaranteed never to be `null`.
    */
  def apply(args: Array[IValue]): IValue

}
