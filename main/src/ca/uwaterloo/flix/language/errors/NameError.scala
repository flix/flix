/*
 * Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation}
import ca.uwaterloo.flix.language.errors.Token._

/**
  * A common super-type for naming errors.
  */
sealed trait NameError extends CompilationError {
  val kind = "Name Error"
}

object NameError {

  /**
    * An error raised to indicate that the given definition `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateDefinition(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val source: SourceInput = loc1.source
    val message: FormattedMessage = new FormattedMessage().
      header(kind, source).
      text(">> Duplicate definition of ").quote(Red(name)).text(".").newLine().
      newLine().
      highlight(loc1, "the first definition was here.").newLine().
      newLine().
      highlight(loc2, "the second definition was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove or rename one of the definitions.").newLine()
  }

  /**
    * An error raised to indicate that an index is defined multiple times for the same relation/lattice.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateIndex(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val source: SourceInput = loc1.source
    val message: FormattedMessage = new FormattedMessage().
      header(kind, source).
      text(">> Duplicate index declaration for table ").quote(Red(name)).text(".").newLine().
      newLine().
      highlight(loc1, "the first declaration was here.").newLine().
      newLine().
      highlight(loc2, "the second declaration was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove one of the two index declarations.").newLine()
  }

}
