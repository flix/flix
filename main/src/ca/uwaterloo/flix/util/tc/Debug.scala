package ca.uwaterloo.flix.util.tc

import ca.uwaterloo.flix.api.Flix

/**
  * Type class for values that can be debugged.
  */
trait Debug[-A] {
  /**
    * Returns a string representation of `a`.
    */
  def emit(name: String, a: A)(implicit flix: Flix): Unit
}
