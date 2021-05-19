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

import java.nio.file.{Path, Paths}
import java.time.{Duration => JDuration}

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    inclusion = Inclusion.Full,
    debug = false,
    documentor = false,
    invariants = false,
    json = false,
    mode = CompilationMode.Development,
    optimizations = Optimization.All,
    quickchecker = false,
    test = false,
    target = JvmTarget.Version18,
    targetDirectory = Paths.get("./target/flix/"),
    timeout = None,
    threads = Runtime.getRuntime.availableProcessors(),
    verbosity = Verbosity.Normal,
    verifier = false,
    loadClassFiles = true,
    writeClassFiles = true,
    xallowredundancies = false,
    xlinter = false,
    xnoboolunification = false,
    xnostratifier = false,
    xstatistics = false
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(inclusion = Inclusion.Full, test = true, verbosity = Verbosity.Silent)

  /**
    * Default test options with the standard library.
    */
  val TestWithLibrary: Options = DefaultTest

  /**
    * Default test options without the standard library.
    */
  val TestWithoutLibrary: Options = DefaultTest.copy(inclusion = Inclusion.Core)

  /**
    * Default test options without any library.
    */
  val TestWithoutCore: Options = DefaultTest.copy(inclusion = Inclusion.SubCore)
}

/**
  * General Flix options.
  *
  * @param inclusion          selects the level of libraries to include.
  * @param debug              enables the emission of debugging information.
  * @param documentor         enables generation of flixdoc.
  * @param invariants         enables checking of compiler invariants.
  * @param json               enable json output
  * @param mode               the compilation mode.
  * @param quickchecker       enables the quickchecker.
  * @param test               enables test mode.
  * @param target             the target JVM.
  * @param targetDirectory    the target directory for compiled code.
  * @param timeout            selects the solver timeout.
  * @param threads            selects the number of threads to use.
  * @param verbosity          selects the level of verbosity.
  * @param verifier           enables the verifier.
  * @param loadClassFiles     loads the generated class files into the JVM.
  * @param writeClassFiles    enables output of class files.
  * @param xallowredundancies disables the redundancy checker.
  * @param xlinter            enables the semantic linter.
  * @param xnoboolunification disables boolean unification.
  * @param xnostratifier      disables computation of stratification.
  * @param xstatistics        prints compiler statistics.
  */
case class Options(inclusion: Inclusion,
                   debug: Boolean,
                   documentor: Boolean,
                   invariants: Boolean,
                   json: Boolean,
                   optimizations: Set[Optimization],
                   mode: CompilationMode,
                   quickchecker: Boolean,
                   target: JvmTarget,
                   targetDirectory: Path,
                   test: Boolean,
                   timeout: Option[JDuration],
                   threads: Int,
                   verbosity: Verbosity,
                   verifier: Boolean,
                   loadClassFiles: Boolean,
                   writeClassFiles: Boolean,
                   xallowredundancies: Boolean,
                   xlinter: Boolean,
                   xnoboolunification: Boolean,
                   xnostratifier: Boolean,
                   xstatistics: Boolean
                  )

/**
  * A common super-type for optimizations.
  */
sealed trait Optimization

object Optimization {

  /**
    * All optimizations supported by the compiler.
    */
  val All: Set[Optimization] = Set(
    TailCalls
  )

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

sealed trait Inclusion

object Inclusion {

  /**
    * Do not include any libraries, even those essential for basic functionality.
    */
  case object SubCore extends Inclusion

  /**
    * Only include essential libraries.
    */
  case object Core extends Inclusion

  /**
    * Include the full standard library.
    */
  case object Full extends Inclusion
}
