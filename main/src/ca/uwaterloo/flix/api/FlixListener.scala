/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

trait FlixListener {

  /**
    * Invoked to notify the listener of the given Flix event.
    */
  def notify(e: FlixEvent): Unit

}
