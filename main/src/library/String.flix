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
    pub def charAt(i: Int, s: Str): Char = native method java.lang.String.charAt(s, i)

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    pub def concat(s1: Str, s2: Str): Str = s1 + s2

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    pub def endsWith(s1: Str, s2: Str): Bool = native method java.lang.String.endsWith(s1, s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    pub def isEmpty(s: Str): Bool = native method java.lang.String.isEmpty(s)

    ///
    /// Returns the length of the string `s`.
    ///
    pub def length(s: Str): Int = native method java.lang.String.length(s)

    ///
    /// Returns `true` if the string `s1` starts with the string `s2`.
    ///
    pub def startsWith(s1: Str, s2: Str): Bool = native method java.lang.String.startsWith(s1, s2)

    //
    // Returns the lower case version of the string `s`.
    //
    pub def toLowerCase(s: Str): Str = native method java.lang.String.toLowerCase(s)

    //
    // Returns the upper case version of the string `s`.
    //
    pub def toUpperCase(s: Str): Str = native method java.lang.String.toUpperCase(s)

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    pub def toList(s: Str): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

}
