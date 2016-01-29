package ca.uwaterloo.flix.api

/**
  * A Java functional interface for unsafe JVM methods that are invokable by Flix.
  */
@FunctionalInterface
trait InvokableUnsafe {

  /**
    * Invokes the method with the given Flix arguments `args`.
    *
    * The arguments are passed using the internal Flix representation of values.
    *
    * Similar the returned value must correspond to Flix' internal representation of values.
    */
  def apply(args: Array[AnyRef]): AnyRef

}
