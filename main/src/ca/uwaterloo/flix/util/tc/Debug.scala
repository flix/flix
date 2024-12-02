/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.util.tc

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.dbg.AstPrinter

/**
  * Type class for values that can be debugged.
  */
trait Debug[-A] {
  def hasAst: Boolean = true

  /**
    * Returns a string representation of `a`.
    */
  def output(name: String, a: A)(implicit flix: Flix): Unit = {
    AstPrinter.appendPhaseToDisk(name, hasAst)
    emit(name, a)
  }

  /**
    * Returns a string representation of `a`.
    */
  protected def emit(name: String, a: A)(implicit flix: Flix): Unit
}
