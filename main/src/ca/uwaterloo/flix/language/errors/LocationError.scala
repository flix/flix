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
import ca.uwaterloo.flix.util.Formatter.NoFormatter.code
import ca.uwaterloo.flix.util.InternalCompilerException

object LocationError {

  private def format(loc: SourceLocation): String = s"${loc.source.name}:${loc.beginLine}:${loc.beginCol}-${loc.endLine}:${loc.endCol}"

  /**
    * An error raised when a location is not contained in location of its parent.
    *
    * @param parentLoc the location of the parent node.
    * @param loc       the location of the chile node.
    */
  def mkChildOutOfBoundError(parentLoc: SourceLocation, loc: SourceLocation): InternalCompilerException = {
    val message =
      s""">> The location of the child ${format(loc)} is not contained in the location of its parent ${format(parentLoc)}.
         |
         |${code(loc, "Child location.")}
         |""".stripMargin
    InternalCompilerException(message, loc)
  }

/**
  * An error raised when a location is not after the location of the preceding node.
  *
  * @param prevLoc the location of the preceding node.
  * @param loc     the location of the child node.
  */
def mkAppearanceOrderError(prevLoc: SourceLocation, loc: SourceLocation): InternalCompilerException = {
  val message: String =
    s""">> The location of the node ${format(loc)} is not after the location of the preceding node ${format(prevLoc)}.
       |
       |${code(loc, "node location.")}
       |""".stripMargin
  InternalCompilerException(message, loc)
}

  /**
    * An error raised when the location of the last child has a different ending than its parent.
    *
    * @param parentLoc the location of the parent node.
    * @param loc       the location of the child node.
    */
  def mkDifferentEndingError(parentLoc: SourceLocation, loc: SourceLocation): InternalCompilerException = {
    val message: String =
      s""">> The location of the last child ${format(loc)} has a different ending than its parent ${format(parentLoc)}.
         |
         |${code(loc, "Child location.")}
         |""".stripMargin
    InternalCompilerException(message, loc)
  }
}
