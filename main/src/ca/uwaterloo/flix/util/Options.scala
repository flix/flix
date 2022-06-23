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

import ca.uwaterloo.flix.language.ast.Symbol

import java.nio.file.Path

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    lib = LibLevel.All,
    debug = false,
    documentor = false,
    entryPoint = None,
    explain = false,
    incremental = true,
    json = false,
    output = None,
    progress = false,
    test = false,
    target = JvmTarget.Version18,
    threads = Runtime.getRuntime.availableProcessors(),
    loadClassFiles = true,
    xallowredundancies = false,
    xnobooltable = false,
    xstatistics = false,
    xstrictmono = false,
    xeffectMetrics = false
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(lib = LibLevel.All, progress = false, test = true)

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
  * @param entryPoint         specifies the main entry point.
  * @param explain            enables additional explanations.
  * @param json               enable json output.
  * @param output             the optional output directory where to place JVM bytecode.
  * @param progress           print progress during compilation.
  * @param test               enables test mode.
  * @param target             the target JVM.
  * @param threads            selects the number of threads to use.
  * @param loadClassFiles     loads the generated class files into the JVM.
  * @param xallowredundancies disables the redundancy checker.
  * @param xnobooltable       disable Boolean minimization via tabling.
  * @param xstatistics        enables statistics collection.
  * @param xstrictmono        enables strict monomorphization.
  * @param xeffectMetrics      enables collection of effect use metrics.
  */
case class Options(lib: LibLevel,
                   debug: Boolean,
                   documentor: Boolean,
                   entryPoint: Option[Symbol.DefnSym],
                   explain: Boolean,
                   incremental: Boolean,
                   json: Boolean,
                   progress: Boolean,
                   output: Option[Path],
                   target: JvmTarget,
                   test: Boolean,
                   threads: Int,
                   loadClassFiles: Boolean,
                   xallowredundancies: Boolean,
                   xnobooltable: Boolean,
                   xstatistics: Boolean,
                   xstrictmono: Boolean,
                   xeffectMetrics: Boolean,
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
