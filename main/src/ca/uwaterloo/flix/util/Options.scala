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

import ca.uwaterloo.flix.language.debug.Audience

import java.nio.file.{Path, Paths}

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    lib = LibLevel.All,
    debug = false,
    documentor = false,
    json = false,
    test = false,
    target = JvmTarget.Version18,
    targetDirectory = Paths.get("./target/flix/"),
    threads = Runtime.getRuntime.availableProcessors(),
    loadClassFiles = true,
    writeClassFiles = true,
    xallowredundancies = false,
    xlinter = false,
    xnoboolunification = false,
    xnostratifier = false,
    xstatistics = false,
    audience = Audience.External
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(lib = LibLevel.All, test = true)

  /**
    * Default test options with the standard library.
    */
  val TestWithLibAll: Options = DefaultTest

  /**
    * Default test options with the minimal library.
    */
  val TestWithLibMin: Options = DefaultTest.copy(lib = LibLevel.Min)

  /**
    * Default test options without any library.
    */
  val TestWithLibNix: Options = DefaultTest.copy(lib = LibLevel.Nix)
}

/**
  * General Flix options.
  *
  * @param lib                selects the level of libraries to include.
  * @param debug              enables the emission of debugging information.
  * @param documentor         enables generation of flixdoc.
  * @param json               enable json output
  * @param test               enables test mode.
  * @param target             the target JVM.
  * @param targetDirectory    the target directory for compiled code.
  * @param threads            selects the number of threads to use.
  * @param loadClassFiles     loads the generated class files into the JVM.
  * @param writeClassFiles    enables output of class files.
  * @param xallowredundancies disables the redundancy checker.
  * @param xlinter            enables the semantic linter.
  * @param xnoboolunification disables boolean unification.
  * @param xnostratifier      disables computation of stratification.
  * @param xstatistics        enables printing of compiler statistics.
  * @param audience           the audience to use for error messages.
  */
case class Options(lib: LibLevel,
                   debug: Boolean,
                   documentor: Boolean,
                   json: Boolean,
                   target: JvmTarget,
                   targetDirectory: Path,
                   test: Boolean,
                   threads: Int,
                   loadClassFiles: Boolean,
                   writeClassFiles: Boolean,
                   xallowredundancies: Boolean,
                   xlinter: Boolean,
                   xnoboolunification: Boolean,
                   xnostratifier: Boolean,
                   xstatistics: Boolean,
                   audience: Audience
                  )

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

sealed trait LibLevel

object LibLevel {

  /**
    * Do not include any libraries, even those essential for basic functionality.
    */
  case object Nix extends LibLevel

  /**
    * Only include essential libraries.
    */
  case object Min extends LibLevel

  /**
    * Include the full standard library.
    */
  case object All extends LibLevel
}
