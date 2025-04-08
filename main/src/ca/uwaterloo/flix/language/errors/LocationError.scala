/*
 * Copyright 2025 Chenhao Gao
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for location errors.
  */
trait LocationError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.LocationError
}

object LocationError {

  /**
    * An error raised when a location is not contained in location of its parent.
    *
    * @param parentLoc the location of the parent node.
    * @param loc       the location of the chile node.
    */
  case class ChildOutOfBoundError(parentLoc: SourceLocation, loc: SourceLocation) extends LocationError {
    def summary: String = "The location of the child is not contained in the location of its parent."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The child location is not contained in its parent location.
         |
         |${code(loc, "Child location.")}
         |""".stripMargin
    }
  }
}
