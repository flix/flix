/*
 * Copyright 2023 Stephen Tetley
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

///
/// The `CodePoint` module complements the `Char` module. `Char` only
/// represents characters in Unicode's _Basic Multilingual Plane_
/// (U+0000 to U+FFFF).
/// CodePoint extends access to include _supplementary characters_ so all
/// Unicode characters can be represented.
///
mod CodePoint {

    ///
    /// Returns the minimum integer value of a Unicode code point.
    ///
    pub def minValue(): Int32 =
        import static java_get_field java.lang.Character.MIN_CODE_POINT: Int32 \ {} as get_MIN_CODE_POINT;
        get_MIN_CODE_POINT()

    ///
    /// Returns the maximum integer value of a Unicode code point.
    ///
    pub def maxValue(): Int32 =
        import static java_get_field java.lang.Character.MAX_CODE_POINT: Int32 \ {} as get_MAX_CODE_POINT;
        get_MAX_CODE_POINT()

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(cp: Int32): Bool =
        0x0000 <= cp  and cp <= 0x0080

    ///
    /// Returns `true` if the given code point `cp` represents a letter character.
    ///
    pub def isLetter(cp: Int32): Bool =
        import static java.lang.Character.isLetter(Int32): Bool \ {};
        isLetter(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode digit.
    /// This includes the ASCII range 0..9 but also Arabic-Indic digits, Devagari digits and Fullwidth digits.
    ///
    pub def isDigit(cp: Int32): Bool =
        import static java.lang.Character.isDigit(Int32): Bool \ {};
        isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode letter or digit.
    ///
    pub def isLetterOrDigit(cp: Int32): Bool =
        import static java.lang.Character.isLetterOrDigit(Int32): Bool \ {};
        isLetterOrDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character strictly in the
    /// range of ASCII digits 0...9.
    ///
    pub def isAsciiDigit(cp: Int32): Bool =
        isAscii(cp) and isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...7.
    ///
    pub def isOctDigit(cp: Int32): Bool =
        0x0030 <= cp and cp <= 0x0037     // '0'..'7'

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...F.
    ///
    pub def isHexDigit(cp: Int32): Bool = match cp {
        case i if 0x0030 <= i and i <= 0x0039 => true        // '0'..'9'
        case i if 0x0041 <= i and i <= 0x0046 => true        // 'A'..'F'
        case i if 0x0061 <= i and i <= 0x0066 => true        // 'a'..'f'
        case _ => false
    }

    ///
    /// Returns `true` if the given code point `cp` represents a lowercase letter.
    ///
    pub def isLowerCase(cp: Int32): Bool =
        import static java.lang.Character.isLowerCase(Int32): Bool \ {};
        isLowerCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an uppercase letter.
    ///
    pub def isUpperCase(cp: Int32): Bool =
        import static java.lang.Character.isUpperCase(Int32): Bool \ {};
        isUpperCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a titlecase letter.
    ///
    pub def isTitleCase(cp: Int32): Bool =
        import static java.lang.Character.isTitleCase(Int32): Bool \ {};
        isTitleCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a white space character.
    ///
    pub def isWhiteSpace(cp: Int32): Bool =
        import static java.lang.Character.isWhitespace(Int32): Bool \ {};
        isWhitespace(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an alphabetic character.
    ///
    pub def isAlphabetic(cp: Int32): Bool =
        import static java.lang.Character.isAlphabetic(Int32): Bool \ {};
        isAlphabetic(cp)

    ///
    /// Returns `true` if the given code point `cp` is within Unicode's Basic Multilingual Plane.
    ///
    /// If the code point is within the BMP it can be represented by Flix's `Char` datatype.
    ///
    pub def isBmpCodePoint(cp: Int32): Bool =
        import static java.lang.Character.isBmpCodePoint(Int32): Bool \ {};
        isBmpCodePoint(cp)

    ///
    /// Returns `true` if the given code point `cp` is defined either as a entry in the
    /// UnicodeData file or a value within a range defined in the UnicodeData file.
    ///
    pub def isDefined(cp: Int32): Bool =
        import static java.lang.Character.isDefined(Int32): Bool \ {};
        isDefined(cp)

    ///
    /// Returns `true` if the given code point `cp` is Chinese, Japanase, Korean
    /// or Vietnamese ideograph.
    ///
    pub def isIdeographic(cp: Int32): Bool =
        import static java.lang.Character.isIdeographic(Int32): Bool \ {};
        isIdeographic(cp)

    ///
    /// Returns `true` if the given code point `cp` is an ISO control character.
    ///
    pub def isISOControl(cp: Int32): Bool =
        import static java.lang.Character.isISOControl(Int32): Bool \ {};
        isISOControl(cp)

    ///
    /// Returns `true` if the given code point `cp` is mirrored.
    ///
    pub def isMirrored(cp: Int32): Bool =
        import static java.lang.Character.isMirrored(Int32): Bool \ {};
        isMirrored(cp)

    ///
    /// Returns `true` if the given code point `cp` is in Unicode's supplementary character range.
    ///
    pub def isSupplementaryCodePoint(cp: Int32): Bool =
        import static java.lang.Character.isSupplementaryCodePoint(Int32): Bool \ {};
        isSupplementaryCodePoint(cp)

    ///
    /// Returns `true` if the code point `cp` is between `U+0000` and `U+10FFFF`.
    ///
    pub def isValidCodePoint(cp: Int32): Bool =
        import static java.lang.Character.isValidCodePoint(Int32): Bool \ {};
        isValidCodePoint(cp)

    ///
    /// Converts a code point representing a letter to its lowercase version.
    ///
    /// Returns the original code point if it does not have a lowercase version.
    ///
    pub def toLowerCase(cp: Int32): Int32 =
        import static java.lang.Character.toLowerCase(Int32): Int32 \ {};
        toLowerCase(cp)

    ///
    /// Converts a code point representing a letter to its uppercase version.
    ///
    /// Returns the original code point if it does not have a uppercase version.
    ///
    pub def toUpperCase(cp: Int32): Int32 =
        import static java.lang.Character.toUpperCase(Int32): Int32 \ {};
        toUpperCase(cp)

    ///
    /// Converts a code point representing a letter to its titlecase version.
    ///
    /// Returns the original character if it does not have either a titlecase
    /// version or a mapping to uppercase.
    ///
    pub def toTitleCase(cp: Int32): Int32 =
        import static java.lang.Character.toTitleCase(Int32): Int32 \ {};
        toTitleCase(cp)

    ///
    /// Returns the name of the code point `cp`.
    ///
    /// See the Java JDK documentation of the method `java.lang.Character.getName` for a full
    /// description of how names are derived.
    ///
    pub def getName(cp: Int32): Option[String] =
        try {
            import static java.lang.Character.getName(Int32): String \ {};
            getName(cp) |> Object.toOption
        } catch {
            case _: ##java.lang.Exception => None
        }

    ///
    /// Returns the integer value representated by the code point `cp` e.g. code point 0x0031
    /// which is the char '1' returns `Some(1)`.
    ///
    /// Returns `None` if `cp` does not represent a number.
    ///
    /// This function handles supplementary characters.
    ///
    pub def getNumericValue(cp: Int32): Option[Int32] =
        import static java.lang.Character.getNumericValue(Int32): Int32 \ {};
        match getNumericValue(cp) {
            case i if i < 0 => None
            case i          => Some(i)
        }

    ///
    /// Returns the number of Chars needed to represent the code point `cp`.
    ///
    /// The answer is either 2 for a supplementary character or 1 for any other character.
    ///
    pub def charCount(cp: Int32): Int32 =
        import static java.lang.Character.charCount(Int32): Int32 \ {};
        charCount(cp)

    ///
    /// Optionally returns a vector of Chars representing the code point `cp`.
    ///
    /// A valid `Some(_)` answer is either a length 2 Vector for a supplementary character
    /// or a length 1 Vector for any other valid character.
    ///
    /// Returns 'None' if `cp` is not a valid code point.
    ///
    pub def toChars(cp: Int32): Option[Vector[Char]] = try {
        region rc {
            import static java.lang.Character.toChars(Int32, Array[Char, rc], Int32): Int32 \ rc;
            let arr = Array.empty(rc, 2);
            let size = toChars(cp, arr, 0);
            Array.toVector(arr) |> Vector.takeLeft(size) |> Some
        }
    } catch {
        case _: ##java.lang.Throwable => None
    }

    ///
    /// Optionally returns a Char represention the code point `cp` if `cp` is within
    /// Unicode's Basic Multilingual Plane.
    ///
    pub def toBmpChar(cp: Int32): Option[Char] = try {
        region rc {
            import static java.lang.Character.toChars(Int32, Array[Char, rc], Int32): Int32 \ rc;
            let arr = Array.empty(rc, 2);
            let i = toChars(cp, arr, 0);
            if (i == 1)
                Array.nth(0, arr)
            else
                None
        }
    } catch {
        case _: ##java.lang.Exception => None
    }

    ///
    /// Optionally returns a pair of Chars representing the high and low surrogate characters of the
    /// code point `cp` if `cp` is a supplementary character.
    ///
    pub def toSupplementaryChars(cp: Int32): Option[(Char, Char)] = try {
        region rc {
            import static java.lang.Character.toChars(Int32, Array[Char, rc], Int32): Int32 \ rc;
            let arr = Array.empty(rc, 2);
            let i = toChars(cp, arr, 0);
            if (i == 2)
                Some(Array.get(0, arr), Array.get(1, arr))
            else
                None
        }
    } catch {
        case _: ##java.lang.Exception => None
    }

    ///
    /// Returns a String representing the code point `cp`.
    ///
    pub def toString(cp: Int32): String =
        try {
            import static java.lang.Character.toString(Int32): String \ {};
            toString(cp)
        } catch {
            case _: ##java.lang.Exception => ""
        }

    ///
    /// Optionally returns the high surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def highSurrogate(cp: Int32): Option[Char] =
        import static java.lang.Character.highSurrogate(Int32): Char \ {};
        if (isSupplementaryCodePoint(cp))
            Some(highSurrogate(cp))
        else
            None

    ///
    /// Optionally returns the low surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def lowSurrogate(cp: Int32): Option[Char] =
        import static java.lang.Character.lowSurrogate(Int32): Char \ {};
        if (isSupplementaryCodePoint(cp))
            Some(lowSurrogate(cp))
        else
            None

}
