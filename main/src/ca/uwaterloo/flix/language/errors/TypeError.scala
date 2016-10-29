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

  /**
    * Ambiguous Reference Error.
    *
    * @param qn  the ambiguous name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class AmbiguousRef(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Ambiguous reference '$qn' (in namespace '$ns').")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * Unresolved Attribute Error.
    *
    * @param name the unresolved attribute name.
    * @param loc  the location where the error occurred.
    */
  case class UnresolvedAttribute(name: Name.Ident, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown attribute '$name'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * Unresolved Reference Error.
    *
    * @param qn  the unresolved reference name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UnresolvedRef(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown definition '$qn' (in namespace '$ns').")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * Unresolved Table Error.
    *
    * @param qn  the unresolved table name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UnresolvedTable(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown relation or lattice '$qn' (in namespace '$ns').")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * Unresolved Tag Error.
    *
    * @param tag the tag name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UnresolvedTag(tag: Name.Ident, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown tag '${tag.name}'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * Unresolved Type Error.
    *
    * @param qn  the unresolved name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UnresolvedType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends TypeError {
    val kind = "Disambiguation Error"
    val source = loc.source
    val message =
      s"""${consoleCtx.blue(s"-- TYPER ERROR --------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unknown type '$qn' (in namespace '$ns').")}
         |
         |${loc.highlight}
         """.stripMargin
  }

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


  // TODO -----------------------------------------------------------------------

  // TODO
  case class OccursCheck() extends TypeError {
    val kind = "Type Error"
    val source = null // TODO
    val message = "OccursCheck" // TODO
  }

}