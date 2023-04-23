/*
 * Copyright 2019 Magnus Madsen
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

instance LowerBound[Char] {
    pub def minValue(): Char = Char.minValue()
}

instance UpperBound[Char] {
    pub def maxValue(): Char = Char.maxValue()
}

mod Char {

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(c: Char): Bool =
        c <= '\u0080'

    ///
    /// Returns `true` if the given char `c` is a letter character.
    ///
    pub def isLetter(c: Char): Bool =
        import static java.lang.Character.isLetter(Char): Bool \ {};
        isLetter(c)

    ///
    /// Returns `true` if the given char `c` is a recognized Unicode digit.
    /// This includes the ASCII range 0..9 but also Arabic-Indic digits, Devagari digits and Fullwidth digits.
    ///
    pub def isDigit(c: Char): Bool =
        import static java.lang.Character.isDigit(Char): Bool \ {};
        isDigit(c)

    ///
    /// Returns `true` if the given char `c` is strictly in the range of ASCII digits 0...9.
    ///
    pub def isAsciiDigit(c: Char): Bool =
        isAscii(c) and isDigit(c)

    ///
    /// Returns `true` if the given char `c` is in the range 0...7.
    ///
    pub def isOctDigit(c: Char): Bool =
        '0' <= c and c <= '7'     // '0'..'7'

    ///
    /// Returns `true` if the given char `c` is in the range 0...F.
    ///
    pub def isHexDigit(c: Char): Bool = match c {
        case i if '0' <= i and i <= '9' => true        // '0'..'9'
        case i if 'A' <= i and i <= 'F' => true        // 'A'..'F'
        case i if 'a' <= i and i <= 'f' => true        // 'a'..'f'
        case _ => false
    }

    ///
    /// Returns `true` if the given char `c` is lowercase.
    ///
    pub def isLowerCase(c: Char): Bool =
        import static java.lang.Character.isLowerCase(Char): Bool \ {};
        isLowerCase(c)

    ///
    /// Returns `true` if the given char `c` is uppercase.
    ///
    pub def isUpperCase(c: Char): Bool =
        import static java.lang.Character.isUpperCase(Char): Bool \ {};
        isUpperCase(c)

    ///
    /// Returns `true` if the given char `c` is a white space character.
    ///
    pub def isWhiteSpace(c: Char): Bool =
        import static java.lang.Character.isWhitespace(Char): Bool \ {};
        isWhitespace(c)

    ///
    /// Converts a letter to its lowercase version.
    ///
    /// Returns the original character if it does not have a lowercase version.
    ///
    pub def toLowerCase(c: Char): Char =
        import static java.lang.Character.toLowerCase(Char): Char \ {};
        toLowerCase(c)

    ///
    /// Converts a letter to its uppercase version.
    ///
    /// Returns the original character if it does not have a uppercase version.
    ///
    pub def toUpperCase(c: Char): Char =
        import static java.lang.Character.toUpperCase(Char): Char \ {};
        toUpperCase(c)

    ///
    /// Returns the character `c` as a string.
    ///
    pub def toString(c: Char): String = ToString.toString(c)

    ///
    /// Returns the character given by the all zero byte.
    ///
    pub def minValue(): Char = '\u0000'

    ///
    /// Returns the character given by the maximum valued byte.
    ///
    pub def maxValue(): Char = '\uffff'

}
