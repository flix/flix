package ca.uwaterloo.flix.util

trait SmartHash {
  this: Product =>

  /**
   * Cached hashCode for immutable values.
   */
  final override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

}
