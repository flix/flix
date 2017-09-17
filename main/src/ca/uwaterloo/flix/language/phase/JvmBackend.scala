/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object JvmBackend extends Phase[Root, Root] {


  case class JvmName(pkg: List[String], name: String)


  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {
    //
    // Compute the set of instantiated types in the program.
    //
    val instantiatedTypes = instantiatedTypesOf(root)

    //
    // Compute the set of types to that should be represented at runtime.
    //

    //
    // Emit functional interfaces for each function type in the program.
    //


    root.toSuccess
  }

  /**
    * Returns the set of all types in the given AST `root`.
    */
  private def instantiatedTypesOf(root: Root)(implicit flix: Flix): Set[Type] = ???

  /**
    * Given a set of types `s` returns the set of types that should be represented at runtime.
    */
  private def representableTypes(s: Set[Type]): Set[Type] = ???

}
