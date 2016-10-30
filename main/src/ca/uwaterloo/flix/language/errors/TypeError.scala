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

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}

/**
  * A common super-type for type errors.
  */
trait TypeError extends CompilationError // TODO: Make sealed

object TypeError {

  implicit val consoleCtx = Compiler.ConsoleCtx

  /**
    * Unification Error.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param loc  the location where the error occurred.
    */
  case class UnificationError(tpe1: Type, tpe2: Type, loc: SourceLocation) extends TypeError {
    val kind = "Type Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unable to unify '$tpe1' and '$tpe2'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  // TODO
  case class OccursCheck() extends TypeError {
    val kind = ???
    val source = ???
    val message = ???
  }

}