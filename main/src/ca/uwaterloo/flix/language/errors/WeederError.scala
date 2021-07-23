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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for weeding errors.
  */
sealed trait WeederError extends CompilationError {
  def kind = "Syntax Error"
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
    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message: VirtualTerminal = {
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

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the formal parameter `name` was declared multiple times.
    *
    * @param name the name of the parameter.
    * @param loc1 the location of the first parameter.
    * @param loc2 the location of the second parameter.
    */
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple declarations of the formal parameter '$name'."

    def message: VirtualTerminal = {
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

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the modifier `name` was used multiple times.
    *
    * @param name the name of the modifier.
    * @param loc1 the location of the first modifier.
    * @param loc2 the location of the second modifier.
    */
  case class DuplicateModifier(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate modifier '$name'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple occurrences of the modifier '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the tag `name` was declared multiple times.
    *
    * @param enumName the name of the enum.
    * @param tag      the name of the tag.
    * @param loc1     the location of the first tag.
    * @param loc2     the location of the second tag.
    */
  case class DuplicateTag(enumName: String, tag: Name.Tag, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate tag: '$tag'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Multiple declarations of the tag '" << Red(tag.name) << "' in the enum '" << Cyan(enumName) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first declaration was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second declaration was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the tags to avoid the name clash." << NewLine
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate an illegal array length.
    *
    * @param loc the location where the illegal array length occurs.
    */
  case class IllegalArrayLength(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal array length"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal array length." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal array length.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal field name.
    *
    * @param loc the location where the illegal field name occurs.
    */
  case class IllegalFieldName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal field name"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal field name." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal field name.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class IllegalFormalParameter(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = "The formal parameter must have a declared type."

    def message: VirtualTerminal = {
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
    * An error raised to indicate an illegal existential quantification expression.
    *
    * @param loc the location where the illegal expression occurs.
    */
  case class IllegalExistential(loc: SourceLocation) extends WeederError {
    def summary: String = "The existential quantifier does not declare any formal parameters."

    def message: VirtualTerminal = {
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
    def summary: String = "The universal quantifier does not declare any formal parameters."

    def message: VirtualTerminal = {
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
    def summary: String = "Illegal float."

    def message: VirtualTerminal = {
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
    def summary: String = "Illegal int."

    def message: VirtualTerminal = {
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
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class IllegalIntrinsic(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal intrinsic"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal intrinsic." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal intrinsic.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal modifier.
    *
    * @param loc the location where the illegal modifier occurs.
    */
  case class IllegalModifier(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal modifier."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal modifier." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal modifier.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal null pattern.
    *
    * @param loc the location where the illegal pattern occurs.
    */
  case class IllegalNullPattern(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal null pattern"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal null pattern." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal null pattern.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal jvm field or method name.
    *
    * @param loc the location of the name.
    */
  case class IllegalJvmFieldOrMethodName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal jvm field or method name."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal jvm field or method name." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal name.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal wildcard in an expression.
    *
    * @param loc the location where the illegal wildcard occurs.
    */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    def summary: String = "Wildcard not allowed here."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Wildcard not allowed here." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal wildcard.") << NewLine
    }
  }

  /**
    * An error raised to indicate a mismatched arity.
    *
    * @param expected the expected arity.
    * @param actual   the actual arity.
    * @param loc      the location where mismatch occurs.
    */
  case class MismatchedArity(expected: Int, actual: Int, loc: SourceLocation) extends WeederError {
    def summary: String = s"Mismatched arity: expected: $expected, actual: $actual."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << s">> Mismatched arity: expected: $expected, actual: $actual." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched arity.") << NewLine
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
    def summary: String = s"Multiple occurrences of '$name' in pattern."

    def message: VirtualTerminal = {
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

    def loc: SourceLocation = loc1 min loc2
  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Undefined annotation $name"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined annotation '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined annotation.") << NewLine
    }
  }

  /**
    * An error raised to indicate an illegal private declaration.
    *
    * @param ident the name of the declaration.
    * @param loc   the location where the error occurred.
    */
  case class IllegalPrivateDeclaration(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal private declaration '${ident.name}'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal private declaration '" << Red(ident.name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal private declaration") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << s" Mark the declaration as 'pub'."
    }
  }

  /**
    * An error raised to indicate an illegal type constraint parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalTypeConstraintParameter(loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal type constraint parameter."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal type constraint parameter." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal type constraint parameter") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << s" Type constraint parameters can only be type variables."
    }
  }

  /**
    * An error raised to indicate type params where some (but not all) are explicitly kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class InconsistentTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Inconsistent type parameters." << NewLine
      vt << NewLine
      vt << Code(loc, "inconsistent type parameters") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << s" Either all or none of the type parameters must be annotated with a kind."
    }
  }

  /**
    * An error raised to indicate type params that are not kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class UnkindedTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Type parameters here must be annotated with a kind."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unkinded type parameters." << NewLine
      vt << NewLine
      vt << Code(loc, "unkinded type parameters") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << s" Type parameters here must be annotated with a kind."
    }
  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param code the escape sequence
    * @param loc  the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(code: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed unicode escape sequence '${code}'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Malformed unicode escape sequence." << NewLine
      vt << NewLine
      vt << Code(loc, "malformed unicode escape sequence") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal."
    }
  }

  /**
    * An error raised to indicate an invalid escape sequence.
    *
    * @param char the invalid escape character.
    * @param loc  the location where the error occurred.
    */
  case class InvalidEscapeSequence(char: Char, loc: SourceLocation) extends WeederError {
    def summary: String = s"Invalid escape sequence '\\${char}'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Invalid escape sequence." << NewLine
      vt << NewLine
      vt << Code(loc, "invalid escape sequence") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\n', and '\\r'."
    }
  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class NonSingleCharacter(chars: String, loc: SourceLocation) extends WeederError {
    def summary: String = "Non-single-character literal."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Non-single-character literal." << NewLine
      vt << NewLine
      vt << Code(loc, "non-single-character literal") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " A character literal must consist of a single character."
    }
  }

}