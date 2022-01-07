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
        import new java.lang.StringBuilder() as newStringBuilder;
        StringBuilder(newStringBuilder())

    ///
    /// Append `x` to the StringBuilder `sb`.
    ///
    pub def append!(sb: StringBuilder, x: a): Unit & Impure with ToString[a] =
        let s = ToString.toString(x);
        sb `appendString!` s;
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(sb: StringBuilder, s: String): Unit & Impure =
        import java.lang.StringBuilder.append(String);
        let StringBuilder(msb) = sb;
        msb `append` s;
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder): Unit & Impure =
        sb `appendString!` String.lineSeparator()

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(sb: StringBuilder, s: String): Unit & Impure =
        sb `appendString!` s;
        appendLineSeparator!(sb)

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(sb: StringBuilder, f: a -> String, x: a): Unit & Impure =
        sb `appendString!` f(x);
        appendLineSeparator!(sb)

    ///
    /// Appends each string in the array `a` to the string builder `sb`.
    ///
    pub def appendLines!(sb: StringBuilder, a: Array[String]): Unit & Impure =
        Array.foreach(appendLine!(sb), a)

    ///
    /// Appends `f(x)` for each x in array `a` to the string builder `sb`.
    ///
    pub def appendLinesWith!(sb: StringBuilder, f: a -> String, a: Array[a]): Unit & Impure =
        Array.foreach(appendLineWith!(sb, f), a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate!(sb: StringBuilder, sep: String, a: Array[String]): Unit & Impure =
        let append1! = (s,i) ->
            if (i > 0) {
                appendString!(sb, sep);
                appendString!(sb, s)
            } else {
                appendString!(sb, s)
            };
       Array.foreachWithIndex(append1!, a)

    ///
    /// Return the length of the StringBuilder `sb`.
    ///
    pub def length(sb: StringBuilder): Int32 & Impure =
        import java.lang.StringBuilder.length();
        let StringBuilder(msb) = sb;
        length(msb)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder): String & Impure =
        import java.lang.StringBuilder.toString();
        let StringBuilder(msb) = sb;
        toString(msb)

}
