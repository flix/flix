/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.verifier

import ca.uwaterloo.flix.language.ast.ExecutableAst
import Verifier.VerifierError

// TODO: Rename, and move

/**
  * A type to hold the result of a property verification.
  */
sealed trait PropertyResult {

  /**
    * Returns the property associated with `this` property result.
    */
  def property: ExecutableAst.Property

  /**
    * Returns the number of paths explored by symbolic execution for `this` property.
    */
  def paths: Int

  /**
    * Returns the number of SMT queries issued for `this` property.
    */
  def queries: Int

  /**
    * Returns the total time spent evaluating `this` property.
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
