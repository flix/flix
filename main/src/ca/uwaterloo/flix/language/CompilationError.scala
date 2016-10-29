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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.SourceInput

/**
  * A common super-type for compilation errors.
  */
trait CompilationError {

  /**
    * The kind of the error message, e.g. "Syntax Error" or "Type Error".
    */
  def kind: String


  /**
    * TODO DOC
    *
    * @return
    */
  def source: SourceInput


  /**
    * The error message.
    */
  def message: String


  // TODO: Explaination.

  def explanation: String = ""

  /**
    * TODO: DOC
    *
    * @return
    */
  def render: String = {
    // TODO
    implicit val consoleCtx = Compiler.ConsoleCtx

    s"""${consoleCtx.blue(s"-- ${kind} -------------------------------------------------- ${source}\n")}""" + message
  }


}
