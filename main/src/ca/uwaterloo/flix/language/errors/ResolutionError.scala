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

import ca.uwaterloo.flix.language.ast.{Name, SourceInput, SourceLocation}
import ca.uwaterloo.flix.language.errors.Token._

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends TypeError {
  // TODO: Should extend CompilationError
  val kind = "Resolution Error"

  // TODO: refactor
  def msg: FormattedMessage

  def message = msg.fmt(ColorContext.AnsiColor)
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
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Ambiguous reference ").quote(Red(qn.toString)).text(".").newLine().
      newLine().
      highlight(loc, "ambiguous reference.").newLine()
  }

  /**
    * Ambiguous Tag Error.
    *
    * @param tag  the tag.
    * @param ns   the current namespace.
    * @param locs the source location of the matched tags.
    * @param loc  the location where the error occurred.
    */
  // TODO: Improve error message.
  case class AmbiguousTag(tag: String, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Ambiguous tag ").quote(Red(tag)).text(".").newLine().
      newLine().
      highlight(loc, "ambiguous tag name.").newLine().
      newLine().
      text("Multiple matches found here:").newLine().
      newLine().
      text(locs.map(_.format).mkString("\n")).newLine() // TODO: Add special thing?
  }

  /**
    * Undefined Attribute Error.
    *
    * @param attribute the attribute name.
    * @param loc       the location where the error occurred.
    */
  case class UndefinedAttribute(table: String, attribute: String, loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Undefined attribute ").quote(Red(attribute)).text(" in table ").text(Cyan(table)).newLine().
      newLine().
      highlight(loc, "attribute not found.").newLine().
      newLine().
      text(Underline("Tip")).text(": Possible typo or non-existent attribute?").newLine()
  }

  /**
    * Unresolved Reference Error.
    *
    * @param qn  the unresolved reference name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRef(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source

    val msg: FormattedMessage = new FormattedMessage().
      text(">> Undefined reference ").quote(Red(qn.toString)).text(".").newLine().
      newLine().
      highlight(loc, "name not found").newLine().
      text(Underline("Tip")).text(" Possible typo or non-existent definition?").newLine()

  }

  /**
    * Undefined Table Error.
    *
    * @param qn  the table name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTable(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Undefined table ").quote(Red(qn.toString)).text(".").newLine().
      newLine().
      highlight(loc, "table not found.").newLine().
      newLine().
      text(Underline("Tip")).text(": Possible typo or non-existent table?").newLine()
  }

  /**
    * Undefined Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Undefined tag ").text(Red(tag)).newLine().
      newLine().
      highlight(loc, "tag not found.").newLine().
      newLine().
      text(Underline("Tip")).text(": Possible typo or non-existent tag?").newLine()
  }

  /**
    * Undefined Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: SourceInput = loc.source
    val msg: FormattedMessage = new FormattedMessage().
      text(">> Undefined type ").quote(Red(qn.toString)).text(".").newLine().
      newLine().
      highlight(loc, "type not found.").newLine().
      newLine().
      text(Underline("Tip")).text(" Possible typo or non-existent type?").newLine()
  }

}
