package ca.uwaterloo.flix.util

trait CachedHash {
  this: Product =>

  /**
   * Cache hashCode for immutable values.
   */
  final override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

}
