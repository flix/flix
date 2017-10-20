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
    core = false,
    debug = false,
    documentor = false,
    evaluation = Evaluation.Compiled,
    impure = false,
    invariants = false,
    monitor = false,
    optimizations = Optimization.All,
    quickchecker = false,
    release = false,
    safe = false,
    test = false,
    timeout = Duration.Inf,
    threads = Runtime.getRuntime.availableProcessors(),
    verbosity = Verbosity.Normal,
    verifier = false
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(core = true, test = true, verbosity = Verbosity.Silent)
}

/**
  * General Flix options.
  *
  * @param core         disables loading of all non-essential namespaces.
  * @param debug        enables the emission of debugging information.
  * @param documentor   enables generation of flixdoc.
  * @param evaluation   selects the evaluation strategy.
  * @param impure       enables impure functions.
  * @param invariants   enables checking of compiler invariants.
  * @param monitor      enables the debugger and profiler.
  * @param quickchecker enables the quickchecker.
  * @param release      enables release mode.
  * @param test         enables test mode.
  * @param safe         disables unsafe operations.
  * @param timeout      selects the solver timeout.
  * @param threads      selects the number of threads to use.
  * @param verbosity    selects the level of verbosity.
  * @param verifier     enables the verifier.
  */
case class Options(core: Boolean,
                   debug: Boolean,
                   documentor: Boolean,
                   evaluation: Evaluation,
                   impure: Boolean,
                   invariants: Boolean,
                   optimizations: Set[Optimization],
                   monitor: Boolean,
                   quickchecker: Boolean,
                   release: Boolean,
                   safe: Boolean,
                   test: Boolean,
                   timeout: Duration,
                   threads: Int,
                   verbosity: Verbosity,
                   verifier: Boolean)

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

/**
  * A common super-type for optimizations.
  */
sealed trait Optimization

object Optimization {

  /**
    * All optimization supported by the compiler.
    */
  val All: Set[Optimization] = Set(
    ClosureElimination,
    EnumCompaction,
    // SingleCaseEnum // TODO: Disabled due to unsoundness with types as keys in ASTs.
    TagTupleFusion,
    TailRecursion,
    Uncurrying)

  /**
    * Enables closure elimination.
    */
  case object ClosureElimination extends Optimization

  /**
    * Enables compilation of compact enums into nulls and a single class.
    */
  case object EnumCompaction extends Optimization

  /**
    * Enables compilation of pattern matching to labels and jumps.
    */
  case object PatMatchLabels extends Optimization

  /**
    * Enables compilation of single-case enums to nothingness.
    */
  case object SingleCaseEnum extends Optimization

  /**
    * Enables compilation of tags and tuples into a single class.
    */
  case object TagTupleFusion extends Optimization

  /**
    * Enables compilation of tail recursive calls into loops.
    *
    * Note: General tail call optimization is always enabled.
    * This optimization handles the special case where a function calls itself in a tail recursive way.
    */
  case object TailRecursion extends Optimization

  /**
    * Enables compilation of curried functions into uncurried functions.
    */
  case object Uncurrying extends Optimization

}
