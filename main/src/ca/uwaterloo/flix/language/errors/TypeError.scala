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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Highlight._

/**
  * A common super-type for type errors.
  */
trait TypeError extends CompilationError

// TODO: Make sealed

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
      hl"""|>> Unable to unify '${Red(tpe1.toString)}' and '${Cyan(tpe2.toString)}'.
           |
           |${Code(loc, "mismatched types.")}
        """.stripMargin
  }

  // TODO: Need test case.
  case class OccursCheck() extends TypeError {
    val kind = ???
    val source = ???
    val message = ???
  }

}