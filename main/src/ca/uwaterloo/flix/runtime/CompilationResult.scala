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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}

/**
  * A class representing the result of a compilation.
  *
  * @param main      the reflected main function, if present.
  * @param tests     the tests in the program.
  * @param sources   the sources of the program.
  * @param classes   the JVM classes of the program.
  * @param totalTime the total compilation time, excluding class writing/loading.
  */
class CompilationResult(main: Option[Array[String] => Unit],
                        tests: Map[Symbol.DefnSym, TestFn],
                        sources: Map[Source, SourceLocation],
                        classes: Map[JvmName, JvmClass],
                        val totalTime: Long
                       ) {

  /** Optionally returns the main function. */
  def getMain: Option[Array[String] => Unit] =
    main

  /** Returns all the test functions in the program. */
  def getTests: Map[Symbol.DefnSym, TestFn] =
    tests

  /** Returns the total number of lines of compiled code. */
  def getTotalLines: Int = sources.foldLeft(0) {
    case (acc, (_, sl)) => acc + sl.endLine
  }

  /** Returns the total amount of bytes in the generated classes. */
  def getTotalByteSize: Int =
    classes.values.map(_.bytecode.length).sum

}
