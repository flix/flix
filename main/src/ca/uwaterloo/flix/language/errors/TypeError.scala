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
sealed trait TypeError extends CompilationError

object TypeError {

  implicit val consoleCtx = Compiler.ConsoleCtx


  // TODO: Check arity of function calls, predicates, etc.

  /**
    * An error raised to indicate that a type has no associated lattice.
    *
    * @param tpe the type that has no lattice.
    * @param loc the source location.
    */
  // TODO
  case class NoSuchLattice(tpe: Type, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> No lattice declared for `$tpe'.")}
         |
         |${loc.highlight}
         |Tip: Associate a lattice with the type.
         """.stripMargin
  }

  // TODO
  case class MergeError() extends TypeError {
    val message = "MergeError" // TODO
  }

  // TODO
  case class UnificationError(tpe1: Type, tpe2: Type) extends TypeError {
    val message = "UnificationError" // TODO
  }

  // TODO
  case class OccursCheck() extends TypeError {
    val message = "OccursCheck" // TODO
  }

  // TODO
  case class KindError() extends TypeError {
    val message = "KindError" // TODO
  }

  /**
    * An error raised to indicate that a definition was not found.
    *
    * @param name the invalid name.
    * @param loc  the location of the name.
    */
  case class UnresolvedDefinition(name: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown definition '$name' not found in the namespace '$ns'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a tag was not found.
    *
    * @param name the invalid name.
    * @param loc  the location of the name.
    */
  case class UnresolvedTag(name: Name.QName, tag: Name.Ident, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown tag '${tag.name}' not found in the namespace '$ns'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a type was not found.
    *
    * @param name the invalid name.
    * @param loc  the location of the name.
    */
  case class UnresolvedType(name: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown type '$name' not found in the namespace '$ns'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }


}