/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

/**
  * A mutable class used for tracking whether progress has been made.
  */
case class Progress(private var progressMade: Boolean = false) {
  def markProgress(): Unit = {
    progressMade = true
  }

  def query(): Boolean = {
    progressMade
  }
}
