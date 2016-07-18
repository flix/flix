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

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * A common super-type for weeding errors.
  */
sealed trait WeederError extends CompilationError

object WeederError {

  implicit val consoleCtx = Compiler.ConsoleCtx

  /**
    * An error raised to indicate that the alias `name` was defined multiple times.
    *
    * @param name the name of the variable.
    * @param loc1 the location of the first declaration.
    * @param loc2 the location of the second declaration.
    */
  case class DuplicateAlias(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate definition of the variable '$name'.")}
         |
         |First definition was here:
         |${loc1.underline}
         |Second definition was here:
         |${loc2.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the annotation `name` was used multiple times.
    *
    * @param name the name of the attribute.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate annotation '$name'.")}
         |
         |First definition was here:
         |${loc1.underline}
         |Second definition was here:
         |${loc2.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the attribute `name` was declared multiple times.
    *
    * @param name the name of the attribute.
    * @param loc1 the location of the first declaration.
    * @param loc2 the location of the second declaration.
    */
  case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate attribute name '$name'.")}
         |
         |First definition was here:
         |${loc1.underline}
         |Second definition was here:
         |${loc2.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the formal parameter `name` was declared multiple times.
    *
    * @param name the name of the argument.
    * @param loc1 the location of the first declaration.
    * @param loc2 the location of the second declaration.
    */
  case class DuplicateFormal(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate formal argument '$name'.")}
         |
         |First definition was here:
         |${loc1.underline}
         |Second definition was here:
         |${loc2.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the tag `name` was declared multiple times.
    *
    * @param name the name of the tag.
    * @param loc1 the location of the first declaration.
    * @param loc2 the location of the second declaration.
    */
  case class DuplicateTag(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate tag name '$name'.")}
         |
         |First declaration was here:
         |${loc1.underline}
         |Second declaration was here:
         |${loc2.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that an index declaration declares no indexes.
    *
    * @param loc the location where the declaration occurs.
    */
  case class EmptyIndex(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> An index must declare at least one group of attributes.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a relation declares no attributes.
    *
    * @param loc the location of the declaration.
    */
  case class EmptyRelation(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> A relation must have at least one attribute (column).")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a lattice declares no attributes.
    *
    * @param loc the location of the declaration.
    */
  case class EmptyLattice(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> A lattice must have at least one attribute (column).")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate the presence of an illegal annotation.
    *
    * @param name the name of the illegal annotation.
    * @param loc  the location of the annotation.
    */
  case class IllegalAnnotation(name: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal annotation '$name'.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate an illegal existential quantification expression.
    *
    * @param msg the error message.
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalExistential(msg: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal existential quantification.")}
         |
         |${loc.underline}
         |$msg
         """.stripMargin
  }


  /**
    * An error raised to indicate that an float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class IllegalFloat(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal float.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that an index declaration defines an index on zero attributes.
    *
    * @param loc the location where the illegal index occurs.
    */
  case class IllegalIndex(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal index. An index must select at least one attribute.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that a predicate is not allowed in the head of a fact/rule.
    *
    * @param loc the location where the illegal predicate occurs.
    */
  case class IllegalHeadPredicate(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal predicate in the head of a fact/rule.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that an illegal term occurs in a body predicate.
    *
    * @param msg the error message.
    * @param loc the location where the illegal term occurs.
    */
  case class IllegalBodyTerm(msg: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal term in the body of a rule.")}
         |
         |${loc.underline}
         |$msg
         """.stripMargin
  }

  /**
    * An error raised to indicate that an illegal term occurs in a head predicate.
    *
    * @param msg the error message.
    * @param loc the location where the illegal term occurs.
    */
  case class IllegalHeadTerm(msg: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal term in the head of a fact/rule.")}
         |
         |${loc.underline}
         |$msg
         """.stripMargin
  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class IllegalInt(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal int.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate an illegal bounded lattice definition.
    *
    * @param loc the location where the illegal definition occurs.
    */
  case class IllegalLattice(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Lattice definition must have exactly five components: bot, top, leq, lub and glb.")}
         |
         |${loc.underline}
         |the 1st component must be the bottom element,
         |the 2nd component must be the top element,
         |the 3rd component must be the partial order function,
         |the 4th component must be the least upper bound function, and
         |the 5th component must be the greatest upper bound function.
         """.stripMargin
  }

  /**
    * An error raised to indicate an illegal parameter list.
    *
    * @param loc the location where the illegal parameter list occurs.
    */
  case class IllegalParameterList(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal parameter list.")}
         |
         |${loc.underline}
         |A parameter list must contain at least one parameter or be omitted.
         """.stripMargin
  }

  /**
    * An error raised to indicate an illegal universal quantification expression.
    *
    * @param msg the error message.
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalUniversal(msg: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal universal quantification.")}
         |
         |${loc.underline}
         |$msg
         """.stripMargin
  }

  /**
    * An error raised to indicate an illegal wildcard in an expression.
    *
    * @param loc the location where the illegal definition occurs.
    */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Illegal wildcard in expression.")}
         |
         |${loc.underline}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
    *
    * @param name the name of the variable.
    * @param loc1 the location of the first use of the variable.
    * @param loc2 the location of the second use of the variable.
    */
  case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Duplicate definition of the same variable '$name' in pattern.")}
         |
         |First definition was here:
         |${loc1.underline}
         |Second definition was here:
         |${loc2.underline}
         |
         |A variable must only occurs once in a pattern.
         """.stripMargin
  }

  /**
    * An error raised to indicate that a syntactic construct, although successfully parsed, is currently not supported.
    *
    * @param msg the error message.
    * @param loc the location of the syntactic construct.
    */
  case class Unsupported(msg: String, loc: SourceLocation) extends WeederError {
    val message =
      s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Unsupported feature: $msg")}
         |
         |${loc.underline}
         |This feature is not yet supported, implemented or considered stable.
         |
         |Tip: Avoid using this feature.
         """.stripMargin
  }

}