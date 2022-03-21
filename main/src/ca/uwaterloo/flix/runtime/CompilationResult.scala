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

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast._

/**
  * A class representing the result of a compilation.
  *
  * @param root     the abstract syntax tree of the program.
  * @param defs     the definitions in the program.
  * @param codeSize the number of bytes the compiler generated.
  */
class CompilationResult(root: Root,
                        main: Option[Array[String] => Unit],
                        defs: Map[Symbol.DefnSym, () => AnyRef],
                        val totalTime: Long,
                        val codeSize: Int) {

  /**
    * Returns the root AST.
    */
  def getRoot: Root = root

  /**
    * Optionally returns the main function.
    */
  def getMain: Option[Array[String] => Unit] = main

  /**
    * Returns all the benchmark functions in the program.
    */
  def getBenchmarks: Map[Symbol.DefnSym, () => AnyRef] = {
    defs filter {
      case (sym, _) => root.defs(sym).ann.isBenchmark
    }
  }

  /**
    * Returns all the test functions in the program.
    */
  def getTests: Map[Symbol.DefnSym, () => AnyRef] = {
    defs filter {
      case (sym, _) => root.defs(sym).ann.isTest
    }
  }

  /**
    * Returns the total number of lines of compiled code.
    */
  def getTotalLines: Int = getRoot.sources.foldLeft(0) {
    case (acc, (_, sl)) => acc + sl.endLine
  }

}
