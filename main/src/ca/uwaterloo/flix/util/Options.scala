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

package ca.uwaterloo.flix.util

import scala.concurrent.duration.Duration

object Options {
  /**
    * Default options.
    */
  val Default = Options(
    debug = false,
    evaluation = Evaluation.Compiled,
    invariants = false,
    monitor = false,
    optimize = false,
    quickchecker = false,
    timeout = Duration.Inf,
    threads = Runtime.getRuntime.availableProcessors(),
    verbosity = Verbosity.Normal,
    verifier = false
  )

  /**
    * Default test options.
    */
  val DefaultTest = Default.copy(verbosity = Verbosity.Silent)
}

/**
  * General Flix options.
  *
  * @param debug        enables the emission of debugging information.
  * @param evaluation   selects the evaluation strategy.
  * @param invariants   enables checking of compiler invariants.
  * @param optimize     enables compiler optimizations.
  * @param monitor      enables the debugger and profiler.
  * @param quickchecker enables the quickchecker.
  * @param timeout      selects the solver timeout.
  * @param threads      selects the number of threads to use.
  * @param verbosity    selects the level of verbosity.
  * @param verifier     enables the verifier.
  */
case class Options(debug: Boolean, evaluation: Evaluation, invariants: Boolean, optimize: Boolean, monitor: Boolean, quickchecker: Boolean, timeout: Duration, threads: Int, verbosity: Verbosity, verifier: Boolean)

/**
  * An option to control the level of verbosity.
  */
sealed trait Verbosity

object Verbosity {

  /**
    * Output verbose information. Useful for debugging.
    */
  case object Verbose extends Verbosity

  /**
    * Output condensed information. The default.
    */
  case object Normal extends Verbosity

  /**
    * Output nothing. Useful for when Flix is used as a library.
    */
  case object Silent extends Verbosity

}

/**
  * An option to control the evaluation strategy.
  */
sealed trait Evaluation

object Evaluation {

  /**
    * Enables JVM code generation of Flix functions.
    */
  case object Compiled extends Evaluation

  /**
    * Disables JVM code generation of Flix functions.
    */
  case object Interpreted extends Evaluation

}
