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
    mode = CompilationMode.Development,
    monitor = false,
    optimizations = Optimization.All,
    quickchecker = false,
    safe = false,
    test = false,
    target = JvmTarget.Version18,
    timeout = Duration.Inf,
    threads = Runtime.getRuntime.availableProcessors(),
    verbosity = Verbosity.Normal,
    verifier = false,
    writeClassFiles = true
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(core = true, test = true, verbosity = Verbosity.Silent)
}

/**
  * General Flix options.
  *
  * @param core            disables loading of all non-essential namespaces.
  * @param debug           enables the emission of debugging information.
  * @param documentor      enables generation of flixdoc.
  * @param evaluation      selects the evaluation strategy.
  * @param impure          enables impure functions.
  * @param invariants      enables checking of compiler invariants.
  * @param mode            the compilation mode.
  * @param monitor         enables the debugger and profiler.
  * @param quickchecker    enables the quickchecker.
  * @param test            enables test mode.
  * @param safe            disables unsafe operations.
  * @param target          the target JVM.
  * @param timeout         selects the solver timeout.
  * @param threads         selects the number of threads to use.
  * @param verbosity       selects the level of verbosity.
  * @param verifier        enables the verifier.
  * @param writeClassFiles enables output of class files.
  */
case class Options(core: Boolean,
                   debug: Boolean,
                   documentor: Boolean,
                   evaluation: Evaluation,
                   impure: Boolean,
                   invariants: Boolean,
                   optimizations: Set[Optimization],
                   mode: CompilationMode,
                   monitor: Boolean,
                   quickchecker: Boolean,
                   safe: Boolean,
                   target: JvmTarget,
                   test: Boolean,
                   timeout: Duration,
                   threads: Int,
                   verbosity: Verbosity,
                   verifier: Boolean,
                   writeClassFiles: Boolean)

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
    * All optimizations supported by the compiler.
    */
  val All: Set[Optimization] = Set(
    NullableEnums,
    PatMatchLabels,
    SingleCaseEnums,
    TagTupleFusion,
    TailCalls
  )

  /**
    * Enables compilation with nullable enums.
    *
    * A nullable enum is an enum with exactly two cases where one case is the unit constructor.
    */
  case object NullableEnums extends Optimization

  /**
    * Enables compilation of pattern matches to labels and jumps.
    */
  case object PatMatchLabels extends Optimization

  /**
    * Enables compilation with elimination of single case enums.
    */
  case object SingleCaseEnums extends Optimization

  /**
    * Enables compilation with tag and tuple fusion.
    */
  case object TagTupleFusion extends Optimization

  /**
    * Enables compilation with full tail calls.
    */
  case object TailCalls extends Optimization

}

/**
  * A common super-type for the compilation mode.
  */
sealed trait CompilationMode

object CompilationMode {

  /**
    * Enables the development mode of the compiler.
    */
  case object Development extends CompilationMode


  /**
    * Enables the release mode of the compiler.
    */
  case object Release extends CompilationMode

}

/**
  * An option to control the version of emitted JVM bytecode.
  */
sealed trait JvmTarget

object JvmTarget {

  /**
    * Emit bytecode for Java 1.6.
    */
  object Version16 extends JvmTarget

  /**
    * Emit bytecode for Java 1.7.
    */
  object Version17 extends JvmTarget

  /**
    * Emit bytecode for Java 1.8.
    */
  object Version18 extends JvmTarget

  /**
    * Emit bytecode for Java 1.9.
    */
  object Version19 extends JvmTarget

}

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
