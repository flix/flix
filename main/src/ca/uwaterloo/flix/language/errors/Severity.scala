/*
 * Copyright 2021 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.errors

sealed trait Severity

object Severity {

  /**
    * A severity that represents a program error.
    */
  case object Error extends Severity

  /**
    * A severity that represents information.
    */
  case object Info extends Severity

  /**
    * A severity that represents a code hint.
    */
  case object Hint extends Severity

}
