/*
 * Copyright 2019 Stephen Tetley
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
/// Represents a StringBuilder.
///
opaque type StringBuilder = ##java.lang.StringBuilder

namespace StringBuilder {

    ///
    /// Returns a new mutable StringBuilder.
    ///
    pub def new(): StringBuilder & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        StringBuilder(newStringBuilder())

    ///
    /// Append `x` to the StringBuilder `sb`.
    ///
    pub def append!(x: a, sb: StringBuilder): Unit & Impure with ToString[a] =
        let s = ToString.toString(x);
        s `appendString!` sb;
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(s: String, sb: StringBuilder): Unit & Impure =
        import java.lang.StringBuilder.append(String): ##java.lang.StringBuilder & Impure;
        let StringBuilder(msb) = sb;
        msb `append` s;
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder): Unit & Impure =
        String.lineSeparator() `appendString!` sb

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(s: String, sb: StringBuilder): Unit & Impure =
        s `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(f: a -> String, x: a, sb: StringBuilder): Unit & Impure =
        f(x) `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends each string in the array `a` to the string builder `sb`.
    ///
    pub def appendLines!(a: Array[String], sb: StringBuilder): Unit & Impure =
        Array.foreach(x -> appendLine!(x, sb), a)

    ///
    /// Appends `f(x)` for each x in array `a` to the string builder `sb`.
    ///
    pub def appendLinesWith!(f: a -> String, a: Array[a], sb: StringBuilder): Unit & Impure =
        Array.foreach(x -> appendLineWith!(f, x, sb), a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate!(sep: String, a: ScopedArray[String, r], sb: StringBuilder): Unit \ { Read(r), Write(r) }=
        let append1! = (s,i) ->
            if (i > 0) {
                appendString!(sep, sb);
                appendString!(s, sb)
            } else {
                appendString!(s, sb)
            };
       Array.foreachWithIndex(append1!, a)

    ///
    /// Return the length of the StringBuilder `sb`.
    ///
    pub def length(sb: StringBuilder): Int32 & Impure =
        import java.lang.StringBuilder.length(): Int32 & Impure;
        let StringBuilder(msb) = sb;
        length(msb)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder): String & Impure =
        import java.lang.StringBuilder.toString(): String & Impure;
        let StringBuilder(msb) = sb;
        toString(msb)

}
