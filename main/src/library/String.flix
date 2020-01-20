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
namespace String {

    ///
    /// Returns the character at position `i` in the string `s`.
    ///
    pub def charAt(i: Int, s: String): Char =
        import java.lang.String.charAt(Int32);
        charAt(s, i)

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    pub def concat(s1: String, s2: String): String =
        import java.lang.String.concat(String);
        concat(s1, s2)

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    pub def endsWith(s1: String, s2: String): Bool =
        import java.lang.String.endsWith(String);
        endsWith(s1, s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    pub def isEmpty(s: String): Bool =
        import java.lang.String.isEmpty();
        isEmpty(s)

    ///
    /// Returns the length of the string `s`.
    ///
    pub def length(s: String): Int =
        import java.lang.String.length();
        length(s)

    ///
    /// Returns `true` if the string `s1` starts with the string `s2`.
    ///
    pub def startsWith(s1: String, s2: String): Bool =
        import java.lang.String.startsWith(String);
        startsWith(s1, s2)

    ///
    /// Splits the string `s` around matches of the regular expression `r`.
    ///
    pub def split(s: String, r: String): Array[String] =
        import java.lang.String.split(String);
        split(s, r)

    //
    // Returns the lower case version of the string `s`.
    //
    pub def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase();
        toLowerCase(s)

    //
    // Returns the upper case version of the string `s`.
    //
    pub def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        toUpperCase(s)

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    pub def toList(s: String): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

    ///
    /// Returns a copy of the string `s` without trailing and leading whitespaces.
    ///
    /// Returns a new empty string if there is no characters in `s`.
    ///
    pub def trim(s: String): String =
        import java.lang.String.trim();
        trim(s)

    ///
    /// Get the system line separator.
    ///
    pub def lineSeparator(): String =
        import java.lang.System:lineSeparator();
        lineSeparator()

}
