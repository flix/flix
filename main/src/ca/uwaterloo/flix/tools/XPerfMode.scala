/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools

/**
  * The experiments that `Xperf` runs (see [[CompilerPerf]]).
  */
sealed trait XPerfMode

object XPerfMode {

  /** Compiles with all cores, non-incrementally. The default. */
  case object Parallel extends XPerfMode

  /** Compiles with a single thread, non-incrementally. */
  case object Sequential extends XPerfMode

  /** Runs every experiment: single-threaded, all cores, and all cores incrementally. */
  case object All extends XPerfMode

}
