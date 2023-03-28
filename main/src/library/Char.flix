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
    pub def minValue(): Char = '\u0000'
}

instance UpperBound[Char] {
    pub def maxValue(): Char = '\uffff'
}

namespace Char {

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(c: Char): Bool = region rc {
        let _ = rc;
        import static get java.nio.charset.StandardCharsets.US_ASCII: ##java.nio.charset.Charset \ Write(rc) as asciiCharset;
        import java.nio.charset.Charset.newEncoder(): ##java.nio.charset.CharsetEncoder \ Write(rc);
        import java.nio.charset.CharsetEncoder.canEncode(Char): Bool \ Write(rc);
        let cs = asciiCharset();
        let encoder = newEncoder(cs);
        canEncode(encoder, c)
    }

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
    // TODO: Refactor
    pub def isHexDigit(c: Char): Bool = match toInt32(c) {
        case i if i >= 0x0030 and i <= 0x0039 => true        // '0'..'9'
        case i if i >= 0x0041 and i <= 0x0046 => true        // 'A'..'F'
        case i if i >= 0x0061 and i <= 0x0066 => true        // 'a'..'f'
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
    /// Returns the character `c` as an Int32.
    ///
    // TODO: Remove
    pub def toInt32(c: Char): Int32 = region rh {
        import static java.lang.Character.codePointAt(Array[Char, rh], Int32): Int32 \ Read(rh);
        codePointAt(Array.repeat(rh, 1, c), 0)
    }

    ///
    /// Returns the respective character for the int `i`.
    ///
    // TODO: Remove
    pub def fromInt32(i: Int32): Option[Char] = region rh {
        import static java.lang.Character.toChars(Int32): Array[Char, rh] \ Write(rh);
        let _ = Array.repeat(rh, 0, 0); // Avoid redundancy error of unused var rh.
        try {
            Some(Array.get(0, toChars(i)))
        } catch {
            case _: ##java.lang.IllegalArgumentException => None
        }
    }

}
