/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

/**
  * A case class to track the compile time spent in a compiler phase.
  */
case class PhaseTime(phase: String, time: Long)
