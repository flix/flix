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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast._
import flix.runtime.ProxyObject

/**
  * A class representing the result of a compilation.
  *
  * @param root the abstract syntax tree of the program.
  * @param defs the definitions in the program.
  */
class CompilationResult(root: Root, main: Option[Array[String] => Int], defs: Map[Symbol.DefnSym, () => ProxyObject])(implicit flix: Flix) {

  /**
    * Returns the root AST.
    */
  def getRoot: Root = root

  /**
    * Optionally returns the main function.
    */
  def getMain: Option[Array[String] => Int] = main

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
    * Immediately evaluates the given fully-qualified name `fqn`.
    *
    * Returns the raw result.
    */
  def eval(fqn: String): AnyRef = {
    // Construct the definition symbol.
    val sym = Symbol.mkDefnSym(fqn)

    // Retrieve the function and call it.
    defs.get(sym) match {
      case None => throw new IllegalArgumentException(s"Undefined fully-qualified name: '$fqn'.")
      case Some(fn) => fn().getValue
    }
  }

  /**
    * Immediately evaluates the given fully-qualified name `fqn`.
    *
    * Returns a string representation of the result.
    */
  def evalToStringDeprecated(fqn: String): String = {
    // Construct the definition symbol.
    val sym = Symbol.mkDefnSym(fqn)

    // Retrieve the definition.
    root.defs.get(sym) match {
      case None => throw new IllegalArgumentException(s"Undefined fully-qualified name: '$fqn'.")
      case Some(defn) =>
        // Retrieve the function and call it.
        val resultValue = defs(sym)()

        resultValue.toString
    }
  }

  /**
    * Returns the total number of lines of compiled code.
    */
  def getTotalLines(): Int = getRoot.sources.foldLeft(0) {
    case (acc, (_, sl)) => acc + sl.endLine
  }

  /**
    * Returns the total compilation time in nanoseconds.
    */
  def getTotalTime(): Long = flix.phaseTimers.foldLeft(0L) {
    case (acc, phase) => acc + phase.time
  }

  /**
    * Returns the result type of the given lambda type.
    */
  private def getResultType(tpe: Type): Type = tpe.typeArguments.last

}