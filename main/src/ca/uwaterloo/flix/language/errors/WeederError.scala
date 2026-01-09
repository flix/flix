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

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for weeding errors.
  */
sealed trait WeederError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.WeederError
}

object WeederError {

  /**
    * An error raised to indicate that the annotation `name` was used multiple times.
    *
    * @param name the name of the annotation.
    * @param loc1 the location of the first annotation.
    * @param loc2 the location of the second annotation.
    */
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E8465

    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of the annotation '${red("@" + name)}'.
         |
         |${src(loc1, "the first occurrence was here.")}
         |
         |${src(loc2, "the second occurrence was here.")}
         |
         |${underline("Tip:")} Remove one of the two annotations.
         |""".stripMargin
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
    def code: ErrorCode = ErrorCode.E8576

    def summary: String = s"Multiple declarations of the formal parameter '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple declarations of the formal parameter '${red(name)}'.
         |
         |${src(loc1, "the first declaration was here.")}
         |
         |${src(loc2, "the second declaration was here.")}
         |
         |${underline("Tip:")} Remove or rename one of the formal parameters to avoid the name clash.
         |""".stripMargin
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
    def code: ErrorCode = ErrorCode.E8687

    def summary: String = s"Duplicate modifier '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of the modifier '${red(name)}'.
         |
         |${src(loc1, "the first occurrence was here.")}
         |
         |${src(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate a struct contains duplicate fields
    *
    * @param structName the name of the struct
    * @param fieldName  the name of the field
    * @param field1Loc  the location of the first field
    * @param field2Loc  the location of the second field
    * @param loc        the location of the struct declaration
    */
  case class DuplicateStructField(structName: String, fieldName: String, field1Loc: SourceLocation, field2Loc: SourceLocation, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E8798

    def summary: String = s"struct has duplicate fields"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Struct has duplicate fields
         |
         |${src(loc, "struct declaration has duplicate fields")}
         |
         |${src(field1Loc, "the first occurrence was here")}
         |
         |${src(field2Loc, "the second occurrence was here")}
         |
         |${underline("Tip:")} Remove one of the two fields.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a loop does not contain any fragments.
    *
    * @param loc the location of the for-loop with no fragments.
    */
  case class EmptyForFragment(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E8809

    def summary: String = "A loop must iterate over some collection."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop does not iterate over any collection.
         |
         |${src(loc, "Loop does not iterate over any collection.")}
         |
         |${underline("Explanation:")}
         |A loop must contain a collection comprehension.
         |
         |A minimal loop is written as follows:
         |
         |    foreach (x <- xs) yield x
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an empty interpolated expression (`"${}"`)
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyInterpolatedExpression(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E8912

    def summary: String = "Empty interpolated expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Empty interpolated expression.
         |
         |${src(loc, "empty interpolated expression")}
         |
         |${underline("Tip:")} Add an expression to the interpolation or remove the interpolation.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a record pattern has shape the illegal shape `{ | r }`.
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyRecordExtensionPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9023

    override def summary: String = "A record pattern must specify at least one field."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record pattern.
         |
         |${src(loc, "A record pattern must specify at least one field.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a specific annotation is not allowed here.
    *
    * @param loc the location of the illegal annotation.
    */
  case class IllegalAnnotation(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9134

    override def summary: String = "Unexpected annotation."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected annotation not allowed here.
         |
         |${src(loc, "unexpected annotation")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that type parameters are present on an effect or operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectTypeParams(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9245

    def summary: String = "Unexpected effect type parameters."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected effect type parameters.
         |
         |${src(loc, "unexpected effect type parameters")}
         |
         |${underline("Tip:")} Type parameters are not allowed on effects.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an operation which itself has an effect.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectfulOperation(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9356

    def summary: String = "Unexpected effect. Effect operations may not themselves have effects."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected effect. Effect operations may not themselves have effects.
         |
         |${src(loc, "unexpected effect")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an enum using both singleton and multiton syntaxes.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEnum(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9467

    def summary: String = "Unexpected enum format."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected enum format.
         |
         |${src(loc, "unexpected enum format")}
         |
         |${underline("Explanation:")}
         |This enum uses both the singleton syntax and the case syntax.
         |
         |Only one of the enum forms may be used.
         |If you only need one case for the enum, use the singleton syntax:
         |
         |    enum E(Int32)
         |
         |If you need multiple cases, use the case syntax:
         |
         |    enum E {
         |        case C1(Int32)
         |        case C2(Bool)
         |    }
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an ill-formed equality constraint.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEqualityConstraint(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9578

    override def summary: String = "Illegal equality constraint."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal equality constraint.
         |
         |${src(loc, s"Equality constraints must have the form: `Assoc[var] ~ Type`.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an invalid escape sequence.
    *
    * @param char the invalid escape character.
    * @param loc  the location where the error occurred.
    */
  case class IllegalEscapeSequence(char: Char, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9689

    def summary: String = s"Invalid escape sequence '\\$char'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Invalid escape sequence.
         |
         |${src(loc, "invalid escape sequence")}
         |
         |${underline("Tip:")} The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\$$', '\\n', and '\\r'.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal extensible variant pattern.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalExtPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9792

    override def summary: String = "Unexpected extensible variant pattern."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected extensible variant pattern.
         |
         |${src(loc, "unexpected pattern")}
         |
         |${underline("Tip:")} Only a default pattern or tags with wild or variable patterns are allowed, e.g., '_' or 'A(x, _, z)', respectively.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a negative atom is marked as fixed.
    *
    * @param loc the location where the illegal fixed atom occurs.
    */
  case class IllegalFixedAtom(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9803

    def summary: String = "Illegal fixed atom"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal fixed atom. A negative atom is implicitly fixed.
         |
         |${src(loc, "Illegal fixed atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a loop does not iterate over any collection.
    *
    * @param loc the location of the for-loop in which the for-fragment appears.
    */
  case class IllegalForFragment(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9914

    def summary: String = s"A foreach expression must start with a collection comprehension."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop does not start with collection comprehension.
         |
         |${src(loc, "Loop does not start with collection comprehension.")}
         |
         |${underline("Explanation:")}
         |A loop must start with collection comprehension where the collection
         |has an instance of the Iterable trait on it.
         |
         |A minimal loop is written as follows:
         |
         |    foreach (x <- xs) yield x
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a ForA-loop contains other ForFragments than Generators.
    *
    * @param loc the location of the for-loop in which the for-fragment appears.
    */
  case class IllegalForAFragment(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0125

    def summary: String = s"A forA loop may only contain comprehensions of the form `x <- xs`."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop contains bad for-comprehension.
         |
         |${src(loc, "Loop contains bad for-comprehension.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal ascription on a formal parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalFormalParamAscription(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0236

    def summary: String = "Unexpected type ascription. Type ascriptions are not permitted on effect handler cases."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type ascription. Type ascriptions are not permitted on effect handler cases.
         |
         |${src(loc, "unexpected type ascription")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a provenance query was executed on a lattice relation, which is not supported.
    *
    * @param loc the location of the illegal latticenal atom.
    */
  case class IllegalLatticeProvenance(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0347

    override def summary: String = "Illegal lattice relation in provenance query."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal lattice relation in provenance query. Provenance on lattice relations is not supported.
         |
         |${src(loc, "illegal lattice relation")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal modifier.
    *
    * @param loc the location where the illegal modifier occurs.
    */
  case class IllegalModifier(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0458

    def summary: String = "Illegal modifier."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal modifier.
         |
         |${src(loc, "illegal modifier.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal null pattern.
    *
    * @param loc the location where the illegal pattern occurs.
    */
  case class IllegalNullPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0569

    def summary: String = "Illegal null pattern"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal null pattern.
         |
         |${src(loc, "illegal null pattern.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal predicate arity.
    *
    * @param loc the location where the error occurs.
    */
  case class IllegalPredicateArity(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0672

    override def summary: String = "Illegal predicate arity."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal predicate arity. Arity must be an integer larger than zero.
         |
         |${src(loc, "illegal arity.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal private declaration.
    *
    * @param ident the name of the declaration.
    * @param loc   the location where the error occurred.
    */
  case class IllegalPrivateDeclaration(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0783

    def summary: String = s"Declaration must be public: '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Declaration must be public: '${red(ident.name)}'.
         |
         |${src(loc, "illegal private declaration")}
         |
         |Mark the declaration as public with `pub'.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a qualified extensible variant pattern.
    *
    * @param qname the offending qualified name.
    */
  case class IllegalQualifiedExtPattern(qname: Name.QName) extends WeederError {
    def code: ErrorCode = ErrorCode.E0894

    override val loc: SourceLocation = qname.loc

    override def summary: String = "Unexpected qualified extensible variant pattern."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected qualified extensible variant pattern.
         |
         |${src(loc, "unexpected qualified pattern")}
         |
         |${underline("Tip:")} Extensible variants can never be qualified, i.e., A.B is not allowed. Consider using just B instead.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the extension of a record pattern is malformed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRecordExtensionPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0905

    override def summary: String = "A record extension must be either a variable or wildcard."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record extension pattern.
         |
         |${src(loc, "A record extension must be either a variable or wildcard.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a record literal contained an operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRecordOperation(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1016

    override def summary: String = "Illegal record extension in record literal"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal record extension in record literal.
         |
         |${src(loc, "A record literal may not contain record extensions or restrictions.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal regex pattern.
    *
    * @param loc the location where the illegal regex pattern occurs.
    */
  case class IllegalRegexPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1127

    def summary: String = "Illegal regex pattern"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal regex pattern.
         |
         |${src(loc, "regex not allowed here.")}
         |
         |${underline("Tip:")} A regex cannot be used as a pattern. It can be used in an `if` guard, e.g using `isMatch` or `isSubmatch`.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate more than one trait parameters was declared.
    *
    * @param loc the location where the error occurs.
    */
  case class IllegalNumberOfTraitParameters(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1238

    override def summary: String = "Illegal number of trait parameters."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal number of trait parameters. Exactly one trait parameter must be declared.
         |
         |${src(loc, "exactly one trait parameter required.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal BigDecimal pattern.
    *
    * @param loc the location where the illegal BigDecimal pattern occurs.
    */
  case class IllegalBigDecimalPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1349

    def summary: String = "Illegal BigDecimal pattern"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal BigDecimal pattern.
         |
         |${src(loc, "BigDecimal not allowed here.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal constant pattern.
    *
    * @param loc the location where the constant pattern occurs.
    */
  case class IllegalConstantPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1452

    def summary: String = "Unexpected constant pattern"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected constant pattern.
         |
         |${src(loc, "Constants are not allowed in let or lambda matches.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate illegal syntax: empty tuple type.
    *
    * @param loc the location where the error occurs.
    */
  case class IllegalEmptyTupleType(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1563

    def summary: String = "Illegal syntax: empty tuple type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal syntax: empty tuple type.
         |
         |${src(loc, "empty tuple type")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal trait constraint parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalTraitConstraintParameter(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1674

    def summary: String = s"Illegal type constraint parameter."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal type constraint parameter.
         |
         |${src(loc, "illegal type constraint parameter")}
         |
         |${underline("Tip:")} Type constraint parameters must be composed only of type variables.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param fromName the original name.
    * @param toName   the alias.
    * @param loc      the location where the error occurred.
    */
  case class IllegalUse(fromName: String, toName: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1785

    def summary: String = s"The case of '$fromName' does not match the case of '$toName'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched alias case.
         |
         |${src(loc, s"The case of '$fromName' does not match the case of '$toName'.")}
         |
         |${underline("Explanation:")}
         |An alias must match the case of the name it replaces.
         |
         |If a name is lowercase, the alias must be lowercase.
         |If a name is uppercase, the alias must be uppercase.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal qualified name.
    *
    * @param loc the location of the illegal qualified name.
    */
  case class IllegalQualifiedName(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1896

    override def summary: String = "Unexpected qualified name"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected qualified name. Java names must be imported, e.g., `import java.lang.Object`.
         |
         |${src(loc, "illegal qualified name")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a function is annotated with both `@Inline` and `@DontInline`.
    *
    * @param inlineLoc     the source location of the `@Inline` annotation.
    * @param dontInlineLoc the source location of the `@DontInline` annotation.
    */
  case class InlineAndDontInline(inlineLoc: SourceLocation, dontInlineLoc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E1907

    override def summary: String = "A def cannot be marked both `@Inline` and `@DontInline`"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> A def cannot be marked both `@Inline` and `@DontInline`.
         |
         |${src(inlineLoc, "the `@Inline` occurs here")}
         |
         |${src(dontInlineLoc, "the `@DontInline` occurs here")}
         |
         |""".stripMargin
    }

    override def loc: SourceLocation = inlineLoc.min(dontInlineLoc)
  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class MalformedChar(chars: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2018

    def summary: String = "Malformed, non-single-character literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed, non-single-character literal.
         |
         |${src(loc, "non-single-character literal")}
         |
         |${underline("Tip:")} A character literal must consist of a single character.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class MalformedFloat(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2129

    def summary: String = "Malformed float."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed float.
         |
         |${src(loc, "malformed float.")}
         |
         |${underline("Tip:")} Ensure that the literal is within bounds.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class MalformedInt(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2232

    def summary: String = "Malformed int."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed int.
         |
         |${src(loc, "malformed int.")}
         |
         |${underline("Tip:")} Ensure that the literal is within bounds.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param pat the invalid regular expression
    * @param loc the location where the error occurred
    */
  case class MalformedRegex(pat: String, err: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2343

    def summary: String = s"Malformed regular expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed regular expression.
         |
         |${src(loc, "malformed regex.")}
         |
         |Pattern compilation error:
         |$err
         |
         |${underline("Explanation:")}
         |A pattern literal must be a valid regular expression.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param code the escape sequence
    * @param loc  the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(escapeCode: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2454

    def summary: String = s"Malformed unicode escape sequence."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed unicode escape sequence.
         |
         |${src(loc, "malformed unicode escape sequence")}
         |
         |${underline("Tip:")} A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal.
         |""".stripMargin
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
    def code: ErrorCode = ErrorCode.E2565

    def summary: String = s"Mismatched arity: expected: $expected, actual: $actual."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched arity: expected: $expected, actual: $actual.
         |
         |${src(loc, "mismatched arity.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate type params where some (but not all) are explicitly kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class MismatchedTypeParameters(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2678

    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Inconsistent type parameters.
         |
         |${src(loc, "inconsistent type parameters")}
         |
         |${underline("Tip:")} Either all or none of the type parameters must be annotated with a kind.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that an argument list is missing a kind.
    *
    * @param loc the location of the argument list.
    */
  case class MissingArgumentList(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2781

    def summary: String = "An argument list is required here"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing argument list. An argument list is required here.
         |
         |${src(loc, "missing argument list.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class MissingFormalParamAscription(name: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2892

    def summary: String = "Missing type ascription. Type ascriptions are required for parameters here."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The formal parameter '${red(name)}' must have a declared type.
         |
         |${src(loc, "has no declared type.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a type parameter is missing a kind.
    *
    * @param loc the location of the type parameter.
    */
  case class MissingTypeParamKind(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2903

    def summary: String = "Type parameter must be annotated with its kind."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing kind annotation. The type parameter must be annotated with its kind.
         |
         |${src(loc, "missing kind.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an empty type parameter list.
    *
    * @param loc the location of the list.
    */
  case class EmptyTypeParamList(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3014

    def summary: String = "Empty type parameter list."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Empty type parameter list.
         |
         |${src(loc, "empty list.")}
         |
         |""".stripMargin
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
    def code: ErrorCode = ErrorCode.E3125

    def summary: String = s"Multiple occurrences of '$name' in pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of '${red(name)}' in pattern.
         |
         |${src(loc1, "the first occurrence was here.")}
         |
         |${src(loc2, "the second occurrence was here.")}
         |
         |A variable may only occur once in a pattern.
         |
         |${underline("Explanation:")}
         |Tip: You can replace
         |
         |  case (x, x) => ...
         |
         |with a guard:
         |
         |  case (x, y) if x == y => ...
         |""".stripMargin
    }

    def loc: SourceLocation = loc1 min loc2

  }

  /**
    * An error raised to indicate a non-unary associated type.
    *
    * @param n   the number of parameters of the associated type.
    * @param loc the location where the error occurred.
    */
  case class NonUnaryAssocType(n: Int, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3236

    override def summary: String = "Non-unary associated type signature."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Associated types must have exactly one parameter, but $n are given here.
         |
         |${src(loc, s"too many parameters")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3347

    def summary: String = s"Undefined annotation '$name'.'"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined annotation '${red(name)}'.
         |
         |${src(loc, "undefined annotation.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class UndefinedIntrinsic(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3458

    def summary: String = "Undefined or misapplied intrinsic"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined or misapplied intrinsic.
         |
         |${src(loc, "undefined or misapplied intrinsic.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unexpected binary type operator.
    *
    * @param op  the unexpected operator.
    * @param loc the location of the operator.
    */
  case class UnexpectedBinaryTypeOperator(op: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3561

    override def summary: String = s"Unexpected binary type operator '$op'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected binary type operator.
         |
         |${src(loc, "unknown binary type operator.")}
         |
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate an invalid function call in a select rule.
    *
    * @param qname the name of the function being called
    */
  case class UnexpectedSelectChannelRuleFunction(qname: Name.QName) extends WeederError {
    def code: ErrorCode = ErrorCode.E3672

    val loc: SourceLocation = qname.loc

    override def summary: String = s"Unexpected channel function '$qname'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected channel function.
         |
         |${src(loc, "select-rules must apply `Channel.recv` to the channel.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param qn  the qualified name of the illegal intrinsic.
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class UnqualifiedUse(qn: Name.QName, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3783

    def summary: String = "Unqualified use."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unqualified use.
         |
         |${src(loc, "unqualified use.")}
         |
         |${underline("Tip:")} A use must be qualified: It should have the form `use Foo.bar`
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unsupported restrictable choice rule pattern.
    *
    * @param star whether the choose is of the star kind.
    * @param loc  the location where the error occurs.
    */
  case class UnsupportedRestrictedChoicePattern(star: Boolean, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3894

    private val operationName: String = if (star) "choose*" else "choose"

    def summary: String = s"Unsupported $operationName pattern, only enums with variables are allowed."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> $summary
         |
         |${src(loc, "Unsupported pattern.")}
         |""".stripMargin
    }
  }

}
