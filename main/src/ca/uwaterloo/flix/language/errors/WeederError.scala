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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for weeding errors.
  */
sealed trait WeederError extends CompilationMessage {
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
    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of the annotation '${red("@" + name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove one of the two annotations."
    })

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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple declarations of the formal parameter '${red(name)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove or rename one of the formal parameters to avoid the name clash."
    })

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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of the modifier '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None

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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple declarations of the tag '${red(tag.name)}' in the enum '${cyan(enumName)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove or rename one of the tags to avoid the name clash."
    })

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate an illegal array length.
    *
    * @param loc the location where the illegal array length occurs.
    */
  case class IllegalArrayLength(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal array length"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal array length.
         |
         |${code(loc, "illegal array length.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal field name.
    *
    * @param loc the location where the illegal field name occurs.
    */
  case class IllegalFieldName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal field name"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |>> Illegal field name.
         |
         |${code(loc, "illegal field name.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a negative atom is marked as fixed.
    *
    * @param loc the location where the illegal fixed atom occurs.
    */
  case class IllegalFixedAtom(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal fixed atom"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |>> Illegal fixed atom. A negative atom is implicitly fixed.
         |
         |${code(loc, "Illegal fixed atom.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class MissingFormalParamAscription(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = "The formal parameter must have a declared type."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |>> The formal parameter '${red(name)}' must have a declared type.
         |
         |${code(loc, "has no declared type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Explicitly declare the type of the formal parameter."
    })

  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class IllegalFloat(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal float."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal float.
         |
         |${code(loc, "illegal float.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class IllegalInt(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal int."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal int.
         |
         |${code(loc, "illegal int.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class IllegalIntrinsic(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal intrinsic"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal intrinsic.
         |
         |${code(loc, "illegal intrinsic.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal modifier.
    *
    * @param loc the location where the illegal modifier occurs.
    */
  case class IllegalModifier(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal modifier."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal modifier.
         |
         |${code(loc, "illegal modifier.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal null pattern.
    *
    * @param loc the location where the illegal pattern occurs.
    */
  case class IllegalNullPattern(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal null pattern"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal null pattern.
         |
         |${code(loc, "illegal null pattern.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal jvm field or method name.
    *
    * @param loc the location of the name.
    */
  case class IllegalJvmFieldOrMethodName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal jvm field or method name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal jvm field or method name.
         |
         |${code(loc, "illegal name.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal wildcard in an expression.
    *
    * @param loc the location where the illegal wildcard occurs.
    */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    def summary: String = "Wildcard not allowed here."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Wildcard not allowed here.
         |
         |${code(loc, "illegal wildcard.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Mismatched arity: expected: $expected, actual: $actual.
         |
         |${code(loc, "mismatched arity.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of '${red(name)}'  in pattern.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} A variable may only occur once in a pattern."
    })

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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined annotation '${red(name)}'.
         |
         |${code(loc, "undefined annotation.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal private declaration.
    *
    * @param ident the name of the declaration.
    * @param loc   the location where the error occurred.
    */
  case class IllegalPrivateDeclaration(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal private declaration '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal private declaration '${red(ident.name)}'.
         |
         |${code(loc, "illegal private declaration")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the declaration as 'pub'."
    })

  }

  /**
    * An error raised to indicate an illegal type constraint parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalTypeConstraintParameter(loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal type constraint parameter."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal type constraint parameter.
         |
         |${code(loc, "illegal type constraint parameter")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type constraint parameters must be composed only of type variables."
    })

  }

  /**
    * An error raised to indicate type params where some (but not all) are explicitly kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class InconsistentTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Inconsistent type parameters.
         |
         |${code(loc, "inconsistent type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Either all or none of the type parameters must be annotated with a kind."
    })

  }

  /**
    * An error raised to indicate type params that are not kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class UnkindedTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Type parameters here must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unkinded type parameters.
         |
         |${code(loc, "unkinded type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type parameters here must be annotated with a kind."
    })

  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param code the escape sequence
    * @param loc  the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(code: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed unicode escape sequence '$code'."

    def message(formatter: Formatter): String = {
      import formatter.{line, code => fmtcode}
      s"""${line(kind, source.name)}
         |>> Malformed unicode escape sequence.
         |
         |${fmtcode(loc, "malformed unicode escape sequence")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")}" + " A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal."
    })

  }

  /**
    * An error raised to indicate an invalid escape sequence.
    *
    * @param char the invalid escape character.
    * @param loc  the location where the error occurred.
    */
  case class InvalidEscapeSequence(char: Char, loc: SourceLocation) extends WeederError {
    def summary: String = s"Invalid escape sequence '\\$char'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Invalid escape sequence.
         |
         |${code(loc, "invalid escape sequence")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")}" + " The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\${', '\\n', and '\\r'."
    })

  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class NonSingleCharacter(chars: String, loc: SourceLocation) extends WeederError {
    def summary: String = "Non-single-character literal."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Non-single-character literal.
         |
         |${code(loc, "non-single-character literal")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} A character literal must consist of a single character."
    })

  }

  /**
    * An error raised to indicate an empty interpolated expression (`"${}"`)
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyInterpolatedExpression(loc: SourceLocation) extends WeederError {
    def summary: String = "Empty interpolated expression."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Empty interpolated expression.
         |
         |${code(loc, "empty interpolated expression")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Add an expression to the interpolation or remove the interpolation."
    })

  }

  /**
    * An error raised to indicate that a newly defined name is reserved.
    *
    * @param ident the reserved name that conflicts.
    * @param loc   the location where the error occurred.
    */
  case class ReservedName(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def summary: String = "Re-definition of a reserved name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Re-definition of reserved name '${red(ident.name)}'.
         |
         |${code(loc, "re-definition of a reserved name")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Try to find a new name that doesn't match one that is reserved."
    })

  }

  /**
    * An error raised to indicate that type parameters are present on an effect or operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectTypeParams(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected effect type parameters."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected effect type parameters.
         |
         |${code(loc, "unexpected effect type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type parameters are not allowed on effects."
    })
  }

  /**
    * An error raised to indicate a use of resume outside an effect handler.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalResume(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected use of 'resume'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected use of 'resume'.
         |
         |${code(loc, "unexpected use of 'resume'")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} The 'resume' expression may only be used in effect handlers."
    })

  }

  /**
    * An error raised to indicate an illegal ascription on a formal parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalFormalParamAscription(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected type ascription."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected type ascription.
         |
         |${code(loc, "unexpected type ascription")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type ascriptions are not permitted on effect handler cases."
    })
  }

  /**
    * An error raised to indicate an illegal effect on an effect operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalOperationEffect(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected effect."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected effect.
         |
         |${code(loc, "unexpected effect")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Effects are not permitted on effect operations."
    })
  }

}
