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
    def charAt(i: Int, s: Str): Char = unsafe native method java.lang.String.charAt(s, i)

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    def endsWith(s1: Str, s2: Str): Bool = unsafe native method java.lang.String.endsWith(s1, s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    def isEmpty(s: Str): Bool = unsafe native method java.lang.String.isEmpty(s)

    ///
    /// Returns the length of the string `s`.
    ///
    def length(s: Str): Int = unsafe native method java.lang.String.length(s)

    ///
    /// Returns `true` if the string `s1` starts with the string `s2`.
    ///
    def startsWith(s1: Str, s2: Str): Bool = unsafe native method java.lang.String.startsWith(s1, s2)

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    def toList(s: Str): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

}
