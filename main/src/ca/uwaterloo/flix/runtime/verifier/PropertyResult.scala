package ca.uwaterloo.flix.runtime.verifier

import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError

/**
  * A type to hold the result of a property verification.
  */
sealed trait PropertyResult {

  /**
    * Returns the number of paths explored by symbolic execution for this property.
    */
  def paths: Int

  /**
    * Returns the number of SMT queries issued for this property.
    */
  def queries: Int

  /**
    * Returns the total time spent evaluating this property.
    */
  def elapsed: Long
}

object PropertyResult {

  /**
    * A property that was proven.
    */
  case class Success(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long) extends PropertyResult

  /**
    * A property that was disproved.
    */
  case class Failure(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long, error: VerifierError) extends PropertyResult

  /**
    * A property whose validity is unknown.
    */
  case class Unknown(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long, error: VerifierError) extends PropertyResult

}
