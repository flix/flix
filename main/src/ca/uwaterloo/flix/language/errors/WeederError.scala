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
import ca.uwaterloo.flix.util.vt.VirtualString.{Cyan, Red, Underline}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for weeding errors.
  */
sealed trait WeederError extends CompilationError {
  val kind = "Syntax Error"
}

object WeederError {

  /**
    * An error raised to indicate that the annotation `name` was used multiple times.
    *
    * @param name the name of the attribute.
    * @param loc1 the location of the first annotation.
    * @param loc2 the location of the second annotation.
    */
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: SourceInput = loc1.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Multiple occurrences of the annotation ").quote(Red("@" + name)).text(".").newLine().
      newLine().
      highlight(loc1, "the first occurrence was here.").newLine().
      newLine().
      highlight(loc2, "the second occurrence was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove one of the two annotations.").newLine()
  }

  /**
    * An error raised to indicate that the attribute `name` was declared multiple times.
    *
    * @param name the name of the attribute.
    * @param loc1 the location of the first attribute.
    * @param loc2 the location of the second attribute.
    */
  case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: SourceInput = loc1.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Multiple declarations of the attribute ").quote(Red(name)).text(".").newLine().
      newLine().
      highlight(loc1, "the first declaration was here.").newLine().
      newLine().
      highlight(loc2, "the second declaration was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove or rename one of the attributes to avoid the name clash.").newLine()
  }

  /**
    * An error raised to indicate that the formal parameter `name` was declared multiple times.
    *
    * @param name the name of the parameter.
    * @param loc1 the location of the first parameter.
    * @param loc2 the location of the second parameter.
    */
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: SourceInput = loc1.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Multiple declarations of the formal parameter ").quote(Red(name)).text(".").newLine().
      newLine().
      highlight(loc1, "the first declaration was here.").newLine().
      newLine().
      highlight(loc2, "the second declaration was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove or rename one of the formal parameters to avoid the name clash.").newLine()
  }

  /**
    * An error raised to indicate that the tag `name` was declared multiple times.
    *
    * @param enumName the name of the enum.
    * @param tagName  the name of the tag.
    * @param loc1     the location of the first tag.
    * @param loc2     the location of the second tag.
    */
  case class DuplicateTag(enumName: String, tagName: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: SourceInput = loc1.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Multiple declarations of the tag ").quote(Red(tagName)).text(" in the enum ").text(Cyan(enumName)).text(".").newLine().
      newLine().
      highlight(loc1, "the first declaration was here.").newLine().
      newLine().
      highlight(loc2, "the second declaration was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove or rename one of the tags to avoid the name clash.").newLine()
  }

  /**
    * An error raised to indicate that an index declaration declares no indexes.
    *
    * @param name the name of the table.
    * @param loc  the location where the declaration occurs.
    */
  case class EmptyIndex(name: String, loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The index for table ").quote(Red(name)).text(" does not declare any attribute groups.").newLine().
      newLine().
      highlight(loc, "an index must declare at least one group of attributes.").newLine().
      newLine().
      text(Underline("Tip")).text(": Add an index on at least one attribute.").newLine()
  }

  /**
    * An error raised to indicate that a relation declares no attributes.
    *
    * @param name the name of the relation.
    * @param loc  the location of the declaration.
    */
  case class EmptyRelation(name: String, loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The relation ").quote(Red(name)).text(" does not declare any attributes.").newLine().
      newLine().
      highlight(loc, "a relation must declare at least one attribute.").newLine()
  }

  /**
    * An error raised to indicate that a lattice declares no attributes.
    *
    * @param name the name of the lattice.
    * @param loc  the location of the declaration.
    */
  case class EmptyLattice(name: String, loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The lattice ").quote(Red(name)).text(" does not declare any attributes.").newLine().
      newLine().
      highlight(loc, "a lattice must declare at least one attribute.").newLine()
  }

  /**
    * An error raised to indicate an illegal existential quantification expression.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalExistential(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The existential quantifier does not declare any formal parameters.").newLine().
      newLine().
      highlight(loc, "quantifier must declare at least one parameter.").newLine().
      newLine().
      text(Underline("Tip")).text(": Add a formal parameter or remove the quantifier.").newLine()
  }

  /**
    * An error raised to indicate an illegal universal quantification expression.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalUniversal(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The universal quantifier does not declare any formal parameters.").newLine().
      newLine().
      highlight(loc, "quantifier must declare at least one parameter.").newLine().
      newLine().
      text(Underline("Tip")).text(": Add a formal parameter or remove the quantifier.").newLine()
  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class IllegalFloat(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Illegal float.").newLine().
      newLine().
      highlight(loc, "illegal float.").newLine().
      newLine().
      text(Underline("Tip")).text(": Ensure that the literal is within bounds.").newLine()
  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class IllegalInt(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Illegal int.").newLine().
      newLine().
      highlight(loc, "illegal int.").newLine().
      newLine().
      text(Underline("Tip")).text(": Ensure that the literal is within bounds.").newLine()
  }

  /**
    * An error raised to indicate that an index declaration defines an index on zero attributes.
    *
    * @param loc the location where the illegal index occurs.
    */
  case class IllegalIndex(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> The attribute group does not declare any attributes.").newLine().
      newLine().
      highlight(loc, "an attribute group must contain at least one attribute.").newLine()
  }

  /**
    * An error raised to indicate an illegal bounded lattice definition.
    *
    * @param loc the location where the illegal definition occurs.
    */
  case class IllegalLattice(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> A lattice definition must have exactly five components: bot, top, leq, lub and glb.").newLine().
      newLine().
      highlight(loc, "illegal definition.").newLine().
      newLine().
      text("the 1st component must be the bottom element,").newLine().
      text("the 2nd component must be the top element,").newLine().
      text("the 3rd component must be the partial order function,").newLine().
      text("the 4th component must be the least upper bound function, and").newLine().
      text("the 5th component must be the greatest upper bound function.").newLine()
  }

  /**
    * An error raised to indicate an illegal parameter list.
    *
    * @param loc the location where the illegal parameter list occurs.
    */
  case class IllegalParameterList(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">>  A parameter list must contain at least one parameter or be omitted.").newLine().
      newLine().
      highlight(loc, "empty parameter list.").newLine().
      newLine().
      text(Underline("Tip")).text(": Remove the parenthesis or add a parameter.").newLine()
  }

  /**
    * An error raised to indicate an illegal wildcard in an expression.
    *
    * @param loc the location where the illegal wildcard occurs.
    */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">>  Wildcard not allowed here.").newLine().
      newLine().
      highlight(loc, "illegal wildcard.").newLine()
  }

  /**
    * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
    *
    * @param name the name of the variable.
    * @param loc1 the location of the first use of the variable.
    * @param loc2 the location of the second use of the variable.
    */
  case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: SourceInput = loc1.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Multiple occurrences of ").quote(Red(name)).text(" in pattern. ").newLine().
      newLine().
      highlight(loc1, "the first occurrence was here.").newLine().
      newLine().
      highlight(loc2, "the second occurrence was here.").newLine().
      newLine().
      text(Underline("Tip")).text(": A variable may only occur once in a pattern.").newLine()
  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">>  Undefined annotation ").quote(Red(name)).text(".").newLine().
      newLine().
      highlight(loc, "undefined annotation.").newLine()
  }

}