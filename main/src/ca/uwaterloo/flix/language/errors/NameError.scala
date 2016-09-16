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

import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * A common super-type for naming errors.
  */
sealed trait NameError extends CompilationError

object NameError {

  implicit val consoleCtx = Compiler.ConsoleCtx

  /**
    * An error raised to indicate that the given definition `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  // TODO: Use sym, and/or namespace?
  case class DuplicateDefinition(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val message =
      s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate definition of '$name'.")}
         |
         |First definition was here:
         |${loc1.highlight}
         |Second definition was here:
         |${loc2.highlight}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a definition name must be lowercase.
    *
    * @param name the invalid name.
    * @param loc  the location of the name.
    */
  case class IllegalDefinitionName(name: String, loc: SourceLocation) extends NameError {
    val message =
      s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal uppercase name '$name'.")}
         |
         |${loc.highlight}
         |A function definition must start with a lowercase letter.
         """.stripMargin
  }

  /**
    * An error raised to indicate that the given table name must be uppercase.
    *
    * @param name the invalid name.
    * @param loc  the location of the name.
    */
  case class IllegalTableName(name: String, loc: SourceLocation) extends NameError {
    val message =
      s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal lowercase name '$name'.")}
         |
         |${loc.highlight}
         |A relation or lattice definition must start with an uppercase letter.
         """.stripMargin
  }

}
