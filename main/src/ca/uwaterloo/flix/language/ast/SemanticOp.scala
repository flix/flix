/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

sealed trait SemanticOp

object SemanticOp {

  sealed trait UnaryOp extends SemanticOp

  sealed trait BinaryOp extends SemanticOp

  /**
    * Exception Operators.
    */
  sealed trait ExnOp extends SemanticOp

  object ExnOp {

    /**
      * Returns a stable kind id for the static type of its operand.
      *
      * Used to implement portable exceptions (`Exn`) without relying on JVM `Class` objects.
      */
    case object KindId extends ExnOp with UnaryOp

  }

  /**
    * Boolean Operators.
    */
  sealed trait BoolOp extends SemanticOp

  object BoolOp {

    /**
      * Boolean Not.
      */
    case object Not extends BoolOp with UnaryOp

    /**
      * Boolean And.
      */
    case object And extends BoolOp with BinaryOp

    /**
      * Boolean Or.
      */
    case object Or extends BoolOp with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends BoolOp with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends BoolOp with BinaryOp

  }

  /**
    * Char Operators.
    */
  sealed trait CharOp extends SemanticOp

  object CharOp {

    /**
      * Equality.
      */
    case object Eq extends CharOp with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends CharOp with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends CharOp with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends CharOp with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends CharOp with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends CharOp with BinaryOp

    /**
      * Returns `true` if the character is a Unicode letter.
      */
    case object IsLetter extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is a Unicode digit.
      */
    case object IsDigit extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is a Unicode letter or digit.
      */
    case object IsLetterOrDigit extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is lowercase.
      */
    case object IsLowerCase extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is uppercase.
      */
    case object IsUpperCase extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is titlecase.
      */
    case object IsTitleCase extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is whitespace.
      */
    case object IsWhitespace extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is defined.
      */
    case object IsDefined extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is an ISO control character.
      */
    case object IsISOControl extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is mirrored.
      */
    case object IsMirrored extends CharOp with UnaryOp

    /**
      * Returns `true` if the character is a surrogate code unit.
      */
    case object IsSurrogate extends CharOp with UnaryOp

    /**
      * Returns `true` if the given characters represent a valid Unicode surrogate pair.
      */
    case object IsSurrogatePair extends CharOp with BinaryOp

    /**
      * Converts the character to lowercase.
      */
    case object ToLowerCase extends CharOp with UnaryOp

    /**
      * Converts the character to uppercase.
      */
    case object ToUpperCase extends CharOp with UnaryOp

    /**
      * Converts the character to titlecase.
      */
    case object ToTitleCase extends CharOp with UnaryOp

    /**
      * Returns the numeric value of the character, or `-1` if none.
      */
    case object GetNumericValue extends CharOp with UnaryOp

    /**
      * Returns the supplementary code point value of the surrogate pair.
      */
    case object ToCodePoint extends CharOp with BinaryOp

    /**
      * Returns the numeric value of the character in the given radix, or `-1` if none.
      */
    case object Digit extends CharOp with BinaryOp

    /**
      * Returns the character representation of `n` in the given radix, or `\\u0000` if none.
      */
    case object ForDigit extends CharOp with BinaryOp

  }

  /**
    * Float32 Operators.
    */
  sealed trait Float32Op extends SemanticOp

  object Float32Op {

    /**
      * Negation.
      */
    case object Neg extends Float32Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Float32Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Float32Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Float32Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Float32Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Float32Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Float32Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Float32Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Float32Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Float32Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Float32Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Float32Op with BinaryOp

  }

  /**
    * Float64 Operators.
    */
  sealed trait Float64Op extends SemanticOp

  object Float64Op {

    /**
      * Negation.
      */
    case object Neg extends Float64Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Float64Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Float64Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Float64Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Float64Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Float64Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Float64Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Float64Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Float64Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Float64Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Float64Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Float64Op with BinaryOp

  }

  /**
    * Int8 Operators.
    */
  sealed trait Int8Op extends SemanticOp

  object Int8Op {

    /**
      * Negation.
      */
    case object Neg extends Int8Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int8Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int8Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int8Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int8Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int8Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int8Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int8Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int8Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int8Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int8Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int8Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int8Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int8Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int8Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int8Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int8Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int8Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int8Op with BinaryOp

  }

  /**
    * Int16 Operators.
    */
  sealed trait Int16Op extends SemanticOp

  object Int16Op {

    /**
      * Negation.
      */
    case object Neg extends Int16Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int16Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int16Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int16Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int16Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int16Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int16Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int16Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int16Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int16Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int16Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int16Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int16Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int16Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int16Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int16Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int16Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int16Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int16Op with BinaryOp

  }

  /**
    * Int32 Operators.
    */
  sealed trait Int32Op extends SemanticOp

  object Int32Op {

    /**
      * Negation.
      */
    case object Neg extends Int32Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int32Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int32Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int32Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int32Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int32Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int32Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int32Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int32Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int32Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int32Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int32Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int32Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int32Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int32Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int32Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int32Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int32Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int32Op with BinaryOp

  }

  /**
    * Int64 Operators.
    */
  sealed trait Int64Op extends SemanticOp

  object Int64Op {

    /**
      * Negation.
      */
    case object Neg extends Int64Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int64Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int64Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int64Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int64Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int64Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int64Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int64Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int64Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int64Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int64Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int64Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int64Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int64Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int64Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int64Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int64Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int64Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int64Op with BinaryOp

  }

  /**
    * BigInt Operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait BigIntOp extends SemanticOp

  object BigIntOp {

    /**
      * Negation.
      */
    case object Neg extends BigIntOp with UnaryOp

    /**
      * Addition.
      */
    case object Add extends BigIntOp with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends BigIntOp with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends BigIntOp with BinaryOp

    /**
      * Division.
      */
    case object Div extends BigIntOp with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends BigIntOp with BinaryOp

    /**
      * Shift left.
      */
    case object Shl extends BigIntOp with BinaryOp

    /**
      * Shift right (arithmetic).
      */
    case object Shr extends BigIntOp with BinaryOp

    /**
      * Bitwise and.
      */
    case object And extends BigIntOp with BinaryOp

    /**
      * Bitwise or.
      */
    case object Or extends BigIntOp with BinaryOp

    /**
      * Bitwise xor.
      */
    case object Xor extends BigIntOp with BinaryOp

    /**
      * Bitwise not.
      */
    case object Not extends BigIntOp with UnaryOp

    /**
      * Compares two BigInts, returning -1, 0, or 1.
      */
    case object Cmp extends BigIntOp with BinaryOp

    /**
      * Returns the bit length of the BigInt.
      */
    case object BitLength extends BigIntOp with UnaryOp

    /**
      * Converts an Int64 to a BigInt.
      */
    case object FromInt64 extends BigIntOp with UnaryOp

  }

  /**
    * BigDecimal operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait BigDecimalOp extends SemanticOp

  object BigDecimalOp {

    /**
      * Negation.
      */
    case object Neg extends BigDecimalOp with UnaryOp

    /**
      * Addition.
      */
    case object Add extends BigDecimalOp with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends BigDecimalOp with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends BigDecimalOp with BinaryOp

    /**
      * Exact division.
      */
    case object Div extends BigDecimalOp with BinaryOp

    /**
      * Compares two BigDecimals, returning -1, 0, or 1.
      */
    case object Cmp extends BigDecimalOp with BinaryOp

    /**
      * Returns the scale of the BigDecimal.
      */
    case object Scale extends BigDecimalOp with UnaryOp

    /**
      * Returns the precision of the BigDecimal.
      */
    case object Precision extends BigDecimalOp with UnaryOp

    /**
      * Rounds toward positive infinity to scale 0.
      */
    case object Ceil extends BigDecimalOp with UnaryOp

    /**
      * Rounds toward negative infinity to scale 0.
      */
    case object Floor extends BigDecimalOp with UnaryOp

    /**
      * Rounds to the nearest integer using HALF_EVEN to scale 0.
      */
    case object Round extends BigDecimalOp with UnaryOp

    /**
      * Converts the value to a BigInt by truncating toward zero.
      */
    case object ToBigInt extends BigDecimalOp with UnaryOp

    /**
      * Formats without exponent notation.
      */
    case object ToPlainString extends BigDecimalOp with UnaryOp

  }

  /**
    * CodePoint operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait CodePointOp extends SemanticOp

  object CodePointOp {
    case object IsLetter extends CodePointOp with UnaryOp
    case object IsDigit extends CodePointOp with UnaryOp
    case object IsLowerCase extends CodePointOp with UnaryOp
    case object IsUpperCase extends CodePointOp with UnaryOp
    case object IsTitleCase extends CodePointOp with UnaryOp
    case object IsWhitespace extends CodePointOp with UnaryOp
    case object IsAlphabetic extends CodePointOp with UnaryOp
    case object IsDefined extends CodePointOp with UnaryOp
    case object IsIdeographic extends CodePointOp with UnaryOp
    case object IsISOControl extends CodePointOp with UnaryOp
    case object IsMirrored extends CodePointOp with UnaryOp
    case object ToLowerCase extends CodePointOp with UnaryOp
    case object ToUpperCase extends CodePointOp with UnaryOp
    case object ToTitleCase extends CodePointOp with UnaryOp
    case object GetName extends CodePointOp with UnaryOp
    case object GetNumericValue extends CodePointOp with UnaryOp
  }

  /**
    * String Operators.
    */
  sealed trait StringOp extends SemanticOp

  object StringOp {

    /**
      * Concatenate.
      */
    case object Concat extends StringOp with BinaryOp

    /**
      * Returns the length of the string.
      */
    case object Length extends StringOp with UnaryOp

    /**
      * Returns the character at the given index in the string.
      */
    case object CharAt extends StringOp with BinaryOp

    /**
      * Returns the lower case version of the string.
      */
    case object ToLowerCase extends StringOp with UnaryOp

    /**
      * Returns the upper case version of the string.
      */
    case object ToUpperCase extends StringOp with UnaryOp

    /**
      * Returns the string repeated `n` times.
      */
    case object Repeat extends StringOp with BinaryOp

  }

  /**
    * Parsing Operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait ParseOp extends SemanticOp

  object ParseOp {

    /**
      * Parses the given string as an Int8, returning (success, value).
      */
    case object Int8FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as an Int16, returning (success, value).
      */
    case object Int16FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as an Int32, returning (success, value).
      */
    case object Int32FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as an Int64, returning (success, value).
      */
    case object Int64FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as a Float32, returning (success, value).
      */
    case object Float32FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as a Float64, returning (success, value).
      */
    case object Float64FromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as a BigInt, returning (success, value).
      */
    case object BigIntFromString extends ParseOp with UnaryOp

    /**
      * Parses the given string as a BigDecimal, returning (success, value).
      */
    case object BigDecimalFromString extends ParseOp with UnaryOp

    /**
      * Parses the given (radix, string) tuple as an Int32, returning (success, value).
      */
    case object Int32Parse extends ParseOp with UnaryOp

    /**
      * Parses the given (radix, string) tuple as an Int64, returning (success, value).
      */
    case object Int64Parse extends ParseOp with UnaryOp

  }

  /**
    * StringBuilder Operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait StringBuilderOp extends SemanticOp

  object StringBuilderOp {

    /**
      * Returns a new mutable string builder handle for the given region.
      */
    case object New extends StringBuilderOp with UnaryOp

    /**
      * Appends a string to the given string builder handle.
      */
    case object AppendString extends StringBuilderOp with UnaryOp

    /**
      * Appends a code point to the given string builder handle.
      */
    case object AppendCodePoint extends StringBuilderOp with UnaryOp

    /**
      * Returns the character at the given index in the string builder handle.
      */
    case object CharAt extends StringBuilderOp with UnaryOp

    /**
      * Returns the length of the string builder handle.
      */
    case object Length extends StringBuilderOp with UnaryOp

    /**
      * Sets the length of the string builder handle.
      */
    case object SetLength extends StringBuilderOp with UnaryOp

    /**
      * Returns the string representation of the string builder handle.
      */
    case object ToString extends StringBuilderOp with UnaryOp

  }

  /**
    * Regex Operators.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait RegexOp extends SemanticOp

  object RegexOp {

    /**
      * Returns the int value of the CanonEq flag.
      */
    case object FlagCanonEq extends RegexOp with UnaryOp

    /**
      * Returns the int value of the CaseInsensitive flag.
      */
    case object FlagCaseInsensitive extends RegexOp with UnaryOp

    /**
      * Returns the int value of the Comments flag.
      */
    case object FlagComments extends RegexOp with UnaryOp

    /**
      * Returns the int value of the Dotall flag.
      */
    case object FlagDotall extends RegexOp with UnaryOp

    /**
      * Returns the int value of the Literal flag.
      */
    case object FlagLiteral extends RegexOp with UnaryOp

    /**
      * Returns the int value of the Multiline flag.
      */
    case object FlagMultiline extends RegexOp with UnaryOp

    /**
      * Returns the int value of the UnicodeCase flag.
      */
    case object FlagUnicodeCase extends RegexOp with UnaryOp

    /**
      * Returns the int value of the UnicodeCharacterClass flag.
      */
    case object FlagUnicodeCharacterClass extends RegexOp with UnaryOp

    /**
      * Returns the int value of the UnixLines flag.
      */
    case object FlagUnixLines extends RegexOp with UnaryOp

    /**
      * Compiles the given regular expression pattern.
      */
    case object Compile extends RegexOp with UnaryOp

    /**
      * Compiles the given regular expression pattern with flags.
      */
    case object CompileWithFlags extends RegexOp with UnaryOp

    /**
      * Tries to compile the given regular expression pattern.
      *
      * Returns a tuple: (success, regex, errorMessage).
      */
    case object TryCompile extends RegexOp with UnaryOp

    /**
      * Tries to compile the given regular expression pattern with flags.
      *
      * Returns a tuple: (success, regex, errorMessage).
      */
    case object TryCompileWithFlags extends RegexOp with UnaryOp

    /**
      * Returns the quoted literal regex for the given string.
      */
    case object Quote extends RegexOp with UnaryOp

    /**
      * Returns the pattern string of the regex.
      */
    case object Pattern extends RegexOp with UnaryOp

    /**
      * Returns the flags of the regex.
      */
    case object Flags extends RegexOp with UnaryOp

    /**
      * Splits the given string around matches of the regex.
      */
    case object Split extends RegexOp with UnaryOp

    /**
      * Creates a new matcher for the given regex and input string.
      */
    case object NewMatcher extends RegexOp with UnaryOp

    /**
      * Returns true if the entire input matches.
      */
    case object MatcherMatches extends RegexOp with UnaryOp

    /**
      * Attempts to find the next match.
      */
    case object MatcherFind extends RegexOp with UnaryOp

    /**
      * Attempts to find the next match from the given position.
      */
    case object MatcherFindFrom extends RegexOp with UnaryOp

    /**
      * Returns true if the input matches starting at the beginning (lookingAt).
      */
    case object MatcherLookingAt extends RegexOp with UnaryOp

    /**
      * Replaces all matches with the given replacement string.
      */
    case object MatcherReplaceAll extends RegexOp with UnaryOp

    /**
      * Replaces the first match with the given replacement string.
      */
    case object MatcherReplaceFirst extends RegexOp with UnaryOp

    /**
      * Sets the bounds of the matcher's region.
      */
    case object MatcherSetBounds extends RegexOp with UnaryOp

    /**
      * Returns the start position of the current match.
      */
    case object MatcherStart extends RegexOp with UnaryOp

    /**
      * Returns the end position of the current match.
      */
    case object MatcherEnd extends RegexOp with UnaryOp

    /**
      * Returns the group content at the given index.
      */
    case object MatcherGroup extends RegexOp with UnaryOp

    /**
      * Returns the number of groups in the matcher.
      */
    case object MatcherGroupCount extends RegexOp with UnaryOp

  }

  /**
    * Primitive conversion to String.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait ToStringOp extends SemanticOp

  object ToStringOp {

    case object CharToString extends ToStringOp with UnaryOp

    case object Float32ToString extends ToStringOp with UnaryOp

    case object Float64ToString extends ToStringOp with UnaryOp

    case object Int8ToString extends ToStringOp with UnaryOp

    case object Int16ToString extends ToStringOp with UnaryOp

    case object Int32ToString extends ToStringOp with UnaryOp

    case object Int64ToString extends ToStringOp with UnaryOp

    case object BigIntToString extends ToStringOp with UnaryOp

    case object BigDecimalToString extends ToStringOp with UnaryOp

  }

  /**
    * Primitive numeric conversions.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait ConvertOp extends SemanticOp

  object ConvertOp {

    case object Int8ToInt16 extends ConvertOp with UnaryOp

    case object Int8ToInt32 extends ConvertOp with UnaryOp

    case object Int8ToInt64 extends ConvertOp with UnaryOp

    case object Int8ToFloat32 extends ConvertOp with UnaryOp

    case object Int8ToFloat64 extends ConvertOp with UnaryOp

    case object Int16ToInt8 extends ConvertOp with UnaryOp

    case object Int16ToInt32 extends ConvertOp with UnaryOp

    case object Int16ToInt64 extends ConvertOp with UnaryOp

    case object Int16ToFloat32 extends ConvertOp with UnaryOp

    case object Int16ToFloat64 extends ConvertOp with UnaryOp

    case object Int32ToInt8 extends ConvertOp with UnaryOp

    case object Int32ToInt16 extends ConvertOp with UnaryOp

    case object Int32ToInt64 extends ConvertOp with UnaryOp

    case object Int32ToFloat32 extends ConvertOp with UnaryOp

    case object Int32ToFloat64 extends ConvertOp with UnaryOp

    case object Int64ToInt8 extends ConvertOp with UnaryOp

    case object Int64ToInt16 extends ConvertOp with UnaryOp

    case object Int64ToInt32 extends ConvertOp with UnaryOp

    case object Int64ToFloat32 extends ConvertOp with UnaryOp

    case object Int64ToFloat64 extends ConvertOp with UnaryOp

    case object Float32ToInt8 extends ConvertOp with UnaryOp

    case object Float32ToInt16 extends ConvertOp with UnaryOp

    case object Float32ToInt32 extends ConvertOp with UnaryOp

    case object Float32ToInt64 extends ConvertOp with UnaryOp

    case object Float32ToFloat64 extends ConvertOp with UnaryOp

    case object Float64ToInt8 extends ConvertOp with UnaryOp

    case object Float64ToInt16 extends ConvertOp with UnaryOp

    case object Float64ToInt32 extends ConvertOp with UnaryOp

    case object Float64ToInt64 extends ConvertOp with UnaryOp

    case object Float64ToFloat32 extends ConvertOp with UnaryOp

  }

  /**
    * Primitive operations with IO effect.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait IoOp extends SemanticOp

  object IoOp {

    case object Print extends IoOp with UnaryOp

    case object EPrint extends IoOp with UnaryOp

    case object Readln extends IoOp with UnaryOp

    case object Println extends IoOp with UnaryOp

    case object EPrintln extends IoOp with UnaryOp

    /**
      * Suspends the current thread for the given number of milliseconds.
      */
    case object SleepMillis extends IoOp with UnaryOp

    /**
      * Exits the current process with the given exit code.
      */
    case object Exit extends IoOp with UnaryOp

    case object NewId extends IoOp with UnaryOp

    /**
      * Returns the current time in milliseconds since the epoch.
      */
    case object TimeNowMillis extends IoOp with UnaryOp

    /**
      * Returns `true` if the file exists.
      */
    case object FileExists extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is a directory.
      */
    case object FileIsDirectory extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is a regular file.
      */
    case object FileIsRegularFile extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is readable.
      */
    case object FileIsReadable extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is a symbolic link.
      */
    case object FileIsSymbolicLink extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is writable.
      */
    case object FileIsWritable extends IoOp with UnaryOp

    /**
      * Returns `true` if the path is executable.
      */
    case object FileIsExecutable extends IoOp with UnaryOp

    /**
      * Returns the last-access time in milliseconds since the epoch.
      */
    case object FileAccessTime extends IoOp with UnaryOp

    /**
      * Returns the creation time in milliseconds since the epoch.
      */
    case object FileCreationTime extends IoOp with UnaryOp

    /**
      * Returns the last-modified time in milliseconds since the epoch.
      */
    case object FileModificationTime extends IoOp with UnaryOp

    /**
      * Returns the file size in bytes.
      */
    case object FileSize extends IoOp with UnaryOp

    /**
      * Reads the entire file into a string (UTF-8, lossy).
      */
    case object FileRead extends IoOp with UnaryOp

    /**
      * Reads the file as an array of lines.
      */
    case object FileReadLines extends IoOp with UnaryOp

    /**
      * Reads the file as an array of bytes.
      */
    case object FileReadBytes extends IoOp with UnaryOp

    /**
      * Lists the contents of a directory.
      */
    case object FileList extends IoOp with UnaryOp

    /**
      * Writes a string to a file (overwrite/create).
      */
    case object FileWrite extends IoOp with UnaryOp

    /**
      * Writes bytes to a file (overwrite/create).
      */
    case object FileWriteBytes extends IoOp with UnaryOp

    /**
      * Appends a string to a file (append/create).
      */
    case object FileAppend extends IoOp with UnaryOp

    /**
      * Appends bytes to a file (append/create).
      */
    case object FileAppendBytes extends IoOp with UnaryOp

    /**
      * Truncates a file to length 0.
      */
    case object FileTruncate extends IoOp with UnaryOp

    /**
      * Creates a directory.
      */
    case object FileMkDir extends IoOp with UnaryOp

    /**
      * Creates a directory and any missing parent directories.
      */
    case object FileMkDirs extends IoOp with UnaryOp

    /**
      * Creates a temporary directory with the given prefix.
      */
    case object FileMkTempDir extends IoOp with UnaryOp

    /**
      * Reads from a TCP socket.
      */
    case object TcpSocketRead extends IoOp with UnaryOp

    /**
      * Writes to a TCP socket.
      */
    case object TcpSocketWrite extends IoOp with UnaryOp

    /**
      * Connects to a TCP server, returning a new socket handle.
      */
    case object TcpSocketConnect extends IoOp with UnaryOp

    /**
      * Closes a TCP socket.
      */
    case object TcpSocketClose extends IoOp with UnaryOp

    /**
      * Binds a TCP server socket, returning a new server handle.
      */
    case object TcpServerBind extends IoOp with UnaryOp

    /**
      * Returns the local port that a TCP server socket is bound to.
      */
    case object TcpServerLocalPort extends IoOp with UnaryOp

    /**
      * Accepts a TCP server connection, returning a new socket handle.
      */
    case object TcpServerAccept extends IoOp with UnaryOp

    /**
      * Closes a TCP server socket.
      */
    case object TcpServerClose extends IoOp with UnaryOp

    /**
      * Writes to a process stdin stream.
      */
    case object ProcessStdinWrite extends IoOp with UnaryOp

    /**
      * Executes a process, returning a new process handle id.
      */
    case object ProcessExec extends IoOp with UnaryOp

    /**
      * Returns the exit value of a process.
      */
    case object ProcessExitValue extends IoOp with UnaryOp

    /**
      * Returns true iff a process is alive.
      */
    case object ProcessIsAlive extends IoOp with UnaryOp

    /**
      * Returns the pid of a process.
      */
    case object ProcessPid extends IoOp with UnaryOp

    /**
      * Stops a process.
      */
    case object ProcessStop extends IoOp with UnaryOp

    /**
      * Waits for a process to exit.
      */
    case object ProcessWaitFor extends IoOp with UnaryOp

    /**
      * Waits with a timeout for a process to exit.
      */
    case object ProcessWaitForTimeout extends IoOp with UnaryOp

    /**
      * Reads from a process stdout stream.
      */
    case object ProcessStdoutRead extends IoOp with UnaryOp

    /**
      * Reads from a process stderr stream.
      */
    case object ProcessStderrRead extends IoOp with UnaryOp

    /**
      * Releases a process handle from the runtime handle table.
      */
    case object ProcessRelease extends IoOp with UnaryOp

    /**
      * Executes an HTTP request, returning status code, response headers, and body.
      */
    case object HttpRequest extends IoOp with UnaryOp

    case object EnvGetArgs extends IoOp with UnaryOp

    case object EnvGetEnvPairs extends IoOp with UnaryOp

    case object EnvGetVar extends IoOp with UnaryOp

    case object EnvGetProp extends IoOp with UnaryOp

    case object EnvVirtualProcessors extends IoOp with UnaryOp

  }

  /**
    * Platform-specific but pure operations.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait PlatformOp extends SemanticOp

  object PlatformOp {

    case object FileSeparator extends PlatformOp with UnaryOp

    case object PathSeparator extends PlatformOp with UnaryOp

    case object LineSeparator extends PlatformOp with UnaryOp

  }

  /**
    * Primitive operations on boxed values / null.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait ObjectOp extends SemanticOp

  object ObjectOp {

    /**
      * Returns true iff the given value is the null reference at runtime.
      */
    case object IsNull extends ObjectOp with UnaryOp

  }

  /**
    * Primitive hashing operations.
    *
    * These operators exist to allow a portable stdlib implementation without JVM interop.
    */
  sealed trait HashOp extends SemanticOp

  object HashOp {

    case object CharHash extends HashOp with UnaryOp

    case object Float32Hash extends HashOp with UnaryOp

    case object Float64Hash extends HashOp with UnaryOp

    case object Int8Hash extends HashOp with UnaryOp

    case object Int16Hash extends HashOp with UnaryOp

    case object Int32Hash extends HashOp with UnaryOp

    case object Int64Hash extends HashOp with UnaryOp

    case object BigIntHash extends HashOp with UnaryOp

    case object BigDecimalHash extends HashOp with UnaryOp

    case object StringHash extends HashOp with UnaryOp

  }

}
