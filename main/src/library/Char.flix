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

namespace Char {

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(c: Char): Bool =
        import flix.runtime.library.Char:isAscii(Char);
        c.isAscii() as & Pure

    ///
    /// Returns `true` if the given char `c` is a letter character.
    ///
    pub def isLetter(c: Char): Bool =
        import java.lang.Character:isLetter(Char);
        c.isLetter() as & Pure

    ///
    /// Returns `true` if the given char `c` is in the range 0...9.
    ///
    pub def isDigit(c: Char): Bool =
        import java.lang.Character:isDigit(Char);
        c.isDigit() as & Pure

    ///
    /// Returns `true` if the given char `c` is in the range 0...7.
    ///
    pub def isOctDigit(c: Char): Bool =
        import flix.runtime.library.Char:isOctDigit(Char);
        c.isOctDigit() as & Pure

    ///
    /// Returns `true` if the given char `c` is in the range 0...F.
    ///
    pub def isHexDigit(c: Char): Bool =
        import flix.runtime.library.Char:isHexDigit(Char);
        c.isHexDigit() as & Pure

    ///
    /// Returns `true` if the given char `c` is lowercase.
    ///
    pub def isLowerCase(c: Char): Bool =
        import java.lang.Character:isLowerCase(Char);
        c.isLowerCase() as & Pure

    ///
    /// Returns `true` if the given char `c` is uppercase.
    ///
    pub def isUpperCase(c: Char): Bool =
        import java.lang.Character:isUpperCase(Char);
        c.isUpperCase() as & Pure

    ///
    /// Returns `true` if the given char `c` is a white space character.
    ///
    pub def isWhiteSpace(c: Char): Bool =
        import java.lang.Character:isWhitespace(Char);
        c.isWhitespace() as & Pure

    ///
    /// Converts a letter to its lowercase version.
    ///
    /// Returns the original character if it does not have a lowercase version.
    ///
    pub def toLowerCase(c: Char): Char =
        import java.lang.Character:toLowerCase(Char);
        c.toLowerCase() as & Pure

    ///
    /// Converts a letter to its uppercase version.
    ///
    /// Returns the original character if it does not have a uppercase version.
    ///
    pub def toUpperCase(c: Char): Char =
        import java.lang.Character:toUpperCase(Char);
        c.toUpperCase() as & Pure

    ///
    /// Returns the character `c` as a string.
    ///
    pub def toString(c: Char): String =
        import java.lang.Character:toString(Char);
        c.toString() as & Pure

}