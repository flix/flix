/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.util.Highlight._

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends TypeError {
  // TODO: Should extend CompilationError
  val kind = "Resolution Error"
}

object ResolutionError {

  /**
    * Ambiguous Reference Error.
    *
    * @param qn  the ambiguous name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  // TODO: Replace by DuplicateDefinition during naming!
  case class AmbiguousRef(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Ambiguous reference '${Red(qn.toString)}'.
           |
           |${Code(loc, "ambiguous reference.")}
           |
        """.stripMargin
  }

  /**
    * Undefined Attribute Error.
    *
    * @param attribute the attribute name.
    * @param loc       the location where the error occurred.
    */
  case class UndefinedAttribute(table: String, attribute: String, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Undefined attribute '${Red(attribute)}' in table '${Cyan(table)}'.
           |
           |${Code(loc, "attribute not found.")}
           |
           |${Underline("Tip")}: Possible typo or non-existent attribute?
        """.stripMargin
  }

  /**
    * Unresolved Reference Error.
    *
    * @param qn  the unresolved reference name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRef(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Undefined reference '${Red(qn.toString)}'.
           |
           |${Code(loc, "name not found.")}
           |
           |${Underline("Tip")}: Possible typo or non-existent definition?
        """.stripMargin
  }

  /**
    * Undefined Table Error.
    *
    * @param qn  the table name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTable(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Undefined table '${Red(qn.toString)}'.
           |
           |${Code(loc, "table not found.")}
           |
           |${Underline("Tip")}: Possible typo or non-existent table?
        """.stripMargin
  }

  /**
    * Undefined Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Undefined tag '${Red(tag)}'.
           |
           |${Code(loc, "tag not found.")}
           |
           |${Underline("Tip")}: Possible typo or non-existent tag?
        """.stripMargin
  }

  /**
    * Undefined Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source = loc.source
    val message =
      hl"""|>> Undefined type '${Red(qn.toString)}'.
           |
           |${Code(loc, "type not found.")}
           |
           |${Underline("Tip")}: Possible typo or non-existent type?
        """.stripMargin
  }


}
