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
    def charAt(s: Str, i: Int): Char = unsafe native method java.lang.String.charAt(s, i)

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    def endsWith(s1: Str, s2: Str): Bool = unsafe native method java.lang.String.endsWith(s1, s2)

}
