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

    def summary: String = s"Duplicate annotation '@$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate annotation '${red("@" + name)}'.
         |
         |${src(loc1, "first occurrence")}
         |
         |${src(loc2, "duplicate")}
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

    def summary: String = s"Duplicate formal parameter: '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate formal parameter '${red(name)}'.
         |
         |${src(loc1, "first declaration")}
         |
         |${src(loc2, "duplicate")}
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
      s""">> Duplicate modifier '${red(name)}'.
         |
         |${src(loc1, "first occurrence")}
         |
         |${src(loc2, "duplicate")}
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

    def summary: String = s"Duplicate struct field: '$fieldName'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate struct field '${red(fieldName)}' in '${magenta(structName)}'.
         |
         |${src(field1Loc, "first occurrence")}
         |
         |${src(field2Loc, "duplicate")}
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

    def summary: String = "Empty loop: missing collection comprehension."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Empty loop: missing collection comprehension.
         |
         |${src(loc, "empty loop")}
         |
         |${underline("Explanation:")} A loop must contain a collection comprehension.
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
         |${src(loc, "missing expression")}
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

    def summary: String = "Empty record pattern: missing field."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Empty record pattern: missing field.
         |
         |${src(loc, "record pattern must specify at least one field")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a specific annotation is not allowed here.
    *
    * @param name the name of the annotation.
    * @param loc  the location of the annotation.
    */
  case class IllegalAnnotation(name: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E9134

    def summary: String = s"Unexpected annotation '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected annotation '${red(name)}'.
         |
         |${src(loc, "annotation not allowed here")}
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
         |${src(loc, "type parameters on effects are not yet supported")}
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

    def summary: String = "Unexpected effect on operation."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected effect on operation.
         |
         |${src(loc, "unexpected effect")}
         |
         |${underline("Explanation:")} Effect operations may not themselves have effects.
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
         |${src(loc, "mixed singleton and case syntax")}
         |
         |${underline("Explanation:")} Only one enum form may be used.
         |If you only need one case, use the singleton syntax:
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

    def summary: String = "Malformed equality constraint."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed equality constraint.
         |
         |${src(loc, "malformed constraint")}
         |
         |${underline("Explanation:")} Equality constraints must have the form: Trait.Assoc[var] ~ Type.
         |For example:
         |
         |    Readable.Elm[t] ~ Int8
         |    Foldable.Aef[t] ~ Pure
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
      s""">> Invalid escape sequence '${red("\\" + char)}'.
         |
         |${src(loc, "invalid escape sequence")}
         |
         |${underline("Explanation:")} The valid escape sequences are:
         |
         |    \\n    newline
         |    \\r    carriage return
         |    \\t    tab
         |    \\\\    backslash
         |    \\'    single quote
         |    \\"    double quote
         |    \\$$    dollar sign
         |
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

    def summary: String = "Unexpected extensible variant pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected extensible variant pattern.
         |
         |${src(loc, "unexpected pattern")}
         |
         |${underline("Explanation:")} Only wildcards and variables are allowed in extensible patterns.
         |
         |    case A(x, _, z) => ...  // allowed
         |    case A(1, 2, 3) => ...  // not allowed
         |
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

    def summary: String = "Unexpected 'fix' on negative atom."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected 'fix' on negative atom.
         |
         |${src(loc, "unexpected 'fix'")}
         |
         |${underline("Explanation:")} Negative atoms are implicitly fixed.
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

    def summary: String = "Unexpected for-fragment: loop must start with a generator."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected for-fragment: loop must start with a generator.
         |
         |${src(loc, "unexpected for-fragment")}
         |
         |${underline("Explanation:")} A loop must start with a generator (x <- xs).
         |For example:
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

    def summary: String = "Unexpected forA fragment: only generators allowed."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected forA fragment: only generators allowed.
         |
         |${src(loc, "unexpected fragment")}
         |
         |${underline("Explanation:")} A forA loop may only contain generators (x <- xs).
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

    def summary: String = "Unexpected type ascription on effect handler parameter."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type ascription on effect handler parameter.
         |
         |${src(loc, "unexpected type ascription")}
         |
         |${underline("Explanation:")} Type ascriptions are not permitted on effect handler parameters.
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

    def summary: String = "Unexpected lattice in provenance query."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected lattice in provenance query.
         |
         |${src(loc, "lattice")}
         |
         |${underline("Explanation:")} Provenance on lattices is not supported.
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

    def summary: String = "Unexpected modifier."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected modifier.
         |
         |${src(loc, "modifier not allowed here")}
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

    def summary: String = "Unexpected null pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected null pattern.
         |
         |${src(loc, "null cannot be used as a pattern")}
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

    def summary: String = "Malformed predicate arity."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed predicate arity.
         |
         |${src(loc, "arity must be a positive integer")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a non-public signature in a trait.
    *
    * @param ident the name of the signature.
    * @param loc   the location where the error occurred.
    */
  case class IllegalNonPublicSignature(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0783

    def summary: String = s"Missing 'pub' modifier on '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing 'pub' modifier on '${red(ident.name)}'.
         |
         |${src(loc, "non-public signature")}
         |
         |${underline("Explanation:")} All signatures in a trait must be public.
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

    def summary: String = "Unexpected qualified extensible variant pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected qualified extensible variant pattern.
         |
         |${src(loc, "qualified pattern not allowed")}
         |
         |${underline("Explanation:")} Extensible variants cannot be qualified. Use '${cyan("B")}' instead of '${red("A.B")}'.
         |""".stripMargin
    }

    def loc: SourceLocation = qname.loc
  }

  /**
    * An error raised to indicate that the extension of a record pattern is malformed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRecordExtensionPattern(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E0905

    def summary: String = "Unexpected record extension pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record extension pattern.
         |
         |${src(loc, "unexpected extension")}
         |
         |${underline("Explanation:")} A record extension must be either a variable or wildcard.
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

    def summary: String = "Unexpected record operation in record literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record operation in record literal.
         |
         |${src(loc, "unexpected operation")}
         |
         |${underline("Explanation:")} Record literals may not contain record extensions or restrictions.
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

    def summary: String = "Unexpected regex pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected regex pattern.
         |
         |${src(loc, "regex not allowed here")}
         |
         |${underline("Explanation:")} Regex cannot be used as a pattern. Use an 'if' guard instead:
         |
         |    case s if Regex.isMatch(regex"...", s) => ...
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

    def summary: String = "Mismatched number of trait parameters."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched number of trait parameters.
         |
         |${src(loc, "exactly one parameter required")}
         |
         |${underline("Explanation:")} A trait must have exactly one type parameter.
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

    def summary: String = "Unexpected BigDecimal pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected BigDecimal pattern.
         |
         |${src(loc, "BigDecimal not allowed here")}
         |
         |${underline("Explanation:")} BigDecimal values cannot be used in pattern matching.
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

    def summary: String = "Unexpected constant pattern."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected constant pattern.
         |
         |${src(loc, "constant not allowed here")}
         |
         |${underline("Explanation:")} Constants are not allowed in let-bindings or lambda parameters.
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

    def summary: String = "Unexpected empty tuple type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected empty tuple type.
         |
         |${src(loc, "empty tuple type")}
         |
         |${underline("Explanation:")} Use '${cyan("Unit")}' instead of an empty tuple type '${red("()")}' .
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

    def summary: String = s"Unexpected type constraint parameter."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type constraint parameter.
         |
         |${src(loc, "unexpected parameter")}
         |
         |${underline("Explanation:")} Type constraint parameters must only contain type variables.
         |
         |    def foo(x: a): ... with ToString[a]         // allowed
         |    def foo(x: a): ... with ToString[Int32]     // not allowed
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

    def summary: String = s"Mismatched alias casing: '$fromName' and '$toName'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched alias casing: '${red(fromName)}' and '${red(toName)}'.
         |
         |${src(loc, "mismatched casing")}
         |
         |${underline("Explanation:")} An alias must match the casing of the name it replaces.
         |
         |    use List.{Nil => Empty}       // OK: both uppercase
         |    use List.{isEmpty => empty}   // OK: both lowercase
         |    use List.{Nil => empty}       // not OK: mismatched casing
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

    def summary: String = "Unexpected qualified name."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected qualified name.
         |
         |${src(loc, "qualified name not allowed here")}
         |
         |${underline("Explanation:")} Java names must be imported, e.g. 'import java.lang.Object'.
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

    def summary: String = "Mismatched annotations: '@Inline' and '@DontInline'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched annotations: '${red("@Inline")}' and '${red("@DontInline")}'.
         |
         |${src(inlineLoc, "@Inline")}
         |
         |${src(dontInlineLoc, "@DontInline")}
         |""".stripMargin
    }

    def loc: SourceLocation = inlineLoc.min(dontInlineLoc)
  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class MalformedChar(chars: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2018

    def summary: String = "Malformed character literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed character literal.
         |
         |${src(loc, "expected single character")}
         |
         |${underline("Explanation:")} A character literal must contain exactly one character.
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

    def summary: String = "Malformed float literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed float literal.
         |
         |${src(loc, "value out of bounds")}
         |
         |${underline("Explanation:")} The literal is outside the representable range.
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

    def summary: String = "Malformed int literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed int literal.
         |
         |${src(loc, "value out of bounds")}
         |
         |${underline("Explanation:")} The literal is outside the representable range.
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
         |${src(loc, err)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param escapeCode the escape sequence
    * @param loc        the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(escapeCode: String, loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E2454

    def summary: String = s"Malformed unicode escape sequence."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed unicode escape sequence.
         |
         |${src(loc, "malformed sequence")}
         |
         |${underline("Explanation:")} Unicode escapes must be of the form '\\uXXXX' where X is hexadecimal.
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

    def summary: String = s"Mismatched arity: expected $expected, actual $actual."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched arity: expected ${cyan(expected.toString)}, actual ${red(actual.toString)}.
         |
         |${src(loc, "mismatched arity")}
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

    def summary: String = "Non-unary associated type signature."

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

    def summary: String = s"Unexpected binary type operator '$op'."

    def message(formatter: Formatter): String = {
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

    def summary: String = s"Unexpected channel function '$qname'."

    def message(formatter: Formatter): String = {
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

  /**
    * An error raised to indicate a non-unary associated type.
    */
  case class IllegalUnaryPlus(loc: SourceLocation) extends WeederError {
    def code: ErrorCode = ErrorCode.E3236

    def summary: String = "Unexpected unary '+'"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected unary '+'.
         |
         |${src(loc, s"Unary '+'")}
         |
         |""".stripMargin
    }
  }

}
