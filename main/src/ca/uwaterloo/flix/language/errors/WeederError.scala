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
import ca.uwaterloo.flix.language.ast.{Source, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._
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
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple occurrences of the annotation '" << Red("@" + name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove one of the two annotations." << NewLine
    }
  }

  /**
    * An error raised to indicate that the attribute `name` was declared multiple times.
    *
    * @param name the name of the attribute.
    * @param loc1 the location of the first attribute.
    * @param loc2 the location of the second attribute.
    */
  case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple declarations of the attribute '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first declaration was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second declaration was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the attributes to avoid the name clash." << NewLine
    }
  }

  /**
    * An error raised to indicate that the formal parameter `name` was declared multiple times.
    *
    * @param name the name of the parameter.
    * @param loc1 the location of the first parameter.
    * @param loc2 the location of the second parameter.
    */
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple declarations of the formal parameter '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first declaration was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second declaration was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the formal parameters to avoid the name clash." << NewLine
    }
  }

  /**
    * An error raised to indicate that the modifier `name` was used multiple times.
    *
    * @param name the name of the modifier.
    * @param loc1 the location of the first modifier.
    * @param loc2 the location of the second modifier.
    */
  case class DuplicateModifier(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple occurrences of the modifier '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
    }
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
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple declarations of the tag '" << Red(tagName) << "' in the enum '" << Cyan(enumName) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first declaration was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second declaration was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the tags to avoid the name clash." << NewLine
    }
  }

  /**
    * An error raised to indicate that an index declaration declares no indexes.
    *
    * @param name the name of the table.
    * @param loc  the location where the declaration occurs.
    */
  case class EmptyIndex(name: String, loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The index for table '" << Red(name) << "' does not declare any attribute groups." << NewLine
      vt << NewLine
      vt << Code(loc, "an index must declare at least one group of attributes.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Add an index on at least one attribute." << NewLine
    }
  }

  /**
    * An error raised to indicate that a relation declares no attributes.
    *
    * @param name the name of the relation.
    * @param loc  the location of the declaration.
    */
  case class EmptyRelation(name: String, loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The relation '" << Red(name) << "' does not declare any attributes." << NewLine
      vt << NewLine
      vt << Code(loc, "a relation must declare at least one attribute.") << NewLine
    }
  }

  /**
    * An error raised to indicate that a lattice declares no attributes.
    *
    * @param name the name of the lattice.
    * @param loc  the location of the declaration.
    */
  case class EmptyLattice(name: String, loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The lattice '" << Red(name) << "' does not declare any attributes." << NewLine
      vt << NewLine
      vt << Code(loc, "a lattice must declare at least one attribute.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal array length,
    *
    * @param loc the location where the illegal array length occurs.
    */
  case class IllegalArrayLength(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal array length." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal array length.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class IllegalFormalParameter(name: String, loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The formal parameter '" << Red(name) << "' must have a declared type." << NewLine
      vt << NewLine
      vt << Code(loc, "has no declared type.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Explicitly declare the type of the formal parameter." << NewLine
    }
  }

  /**
    * An error raised to indicate that an effect is unknown.
    *
    * @param loc the location where the illegal effect occurs.
    */
  case class IllegalEffect(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal effect." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal effect.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal existential quantification expression.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalExistential(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The existential quantifier does not declare any formal parameters." << NewLine
      vt << NewLine
      vt << Code(loc, "quantifier must declare at least one parameter.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Add a formal parameter or remove the quantifier." << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal universal quantification expression.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalUniversal(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The universal quantifier does not declare any formal parameters." << NewLine
      vt << NewLine
      vt << Code(loc, "quantifier must declare at least one parameter.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Add a formal parameter or remove the quantifier." << NewLine
    }
  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class IllegalFloat(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal float." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal float.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Ensure that the literal is within bounds." << NewLine
    }
  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class IllegalInt(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal int." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal int.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Ensure that the literal is within bounds." << NewLine
    }
  }

  /**
    * An error raised to indicate the presence of a hole in release mode.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalHole(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Hole expressions are not allowed in release mode." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal hole.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Implement the hole or disable release mode." << NewLine
    }
  }

  /**
    * An error raised to indicate that an index declaration defines an index on zero attributes.
    *
    * @param loc the location where the illegal index occurs.
    */
  case class IllegalIndex(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> The attribute group does not declare any attributes." << NewLine
      vt << NewLine
      vt << Code(loc, "an attribute group must contain at least one attribute.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal bounded lattice definition.
    *
    * @param loc the location where the illegal definition occurs.
    */
  case class IllegalLattice(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> A lattice definition must have exactly six components: bot, top, equ, leq, lub and glb." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal definition.") << NewLine
      vt << NewLine
      vt << "the 1st component must be the bottom element," << NewLine
      vt << "the 2nd component must be the top element," << NewLine
      vt << "the 3rd component must be the equality function," << NewLine
      vt << "the 4th component must be the partial order function," << NewLine
      vt << "the 5th component must be the least upper bound function, and" << NewLine
      vt << "the 6th component must be the greatest upper bound function." << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal modifier.
    *
    * @param loc the location where the illegal modifier occurs.
    */
  case class IllegalModifier(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal modifier." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal modifier.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal native field or method name.
    *
    * @param loc the location of the name.
    */
  case class IllegalNativeFieldOrMethodName(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal native field or method name." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal name.") << NewLine
    }
  }

  /**
    * An error raised to indicate that an unsafe expression does not occur inside an unsafe scope.
    *
    * @param loc the location of the expression.
    */
  case class IllegalUnsafeExpression(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unsafe operation must occur in unsafe scope." << NewLine
      vt << NewLine
      vt << Code(loc, "unsafe expression.") << NewLine
    }
  }

  /**
    * An error raised to indicate that an unsafe operations have been disabled.
    *
    * @param loc the location of the expression.
    */
  case class IllegalUnsafeExpressionInSafeMode(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unsafe operations are disabled in safe mode." << NewLine
      vt << NewLine
      vt << Code(loc, "unsafe expression.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal wildcard in an expression.
    *
    * @param loc the location where the illegal wildcard occurs.
    */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Wildcard not allowed here." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal wildcard.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
    *
    * @param name the name of the variable.
    * @param loc1 the location of the first use of the variable.
    * @param loc2 the location of the second use of the variable.
    */
  case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple occurrences of '" << Red(name) << "'  in pattern." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " A variable may only occur once in a pattern." << NewLine
    }
  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined annotation '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined annotation.") << NewLine
    }
  }

  case class IllegalVectorLength(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal vector length. " << NewLine
      vt << NewLine
      vt << Code(loc, "Vector length must be an integer of minimum 0.") << NewLine
    }
  }

  case class IllegalVectorIndex(loc: SourceLocation) extends WeederError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal vector index. " << NewLine
      vt << NewLine
      vt << Code(loc, "Illegal vector index.") << NewLine
    }
  }

}