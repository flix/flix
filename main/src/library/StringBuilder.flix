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
    /// Append the StringBuilder `sb2` to the end of StringBuilder `sb1`.
    ///
    pub def append!(sb1: StringBuilder, sb2: StringBuilder): Unit & Impure =
        import java.lang.StringBuilder.append(##java.lang.CharSequence);
        let StringBuilder(msb1) = sb1;
        let StringBuilder(msb2) = sb2;
        let cs2 = msb2 as ##java.lang.CharSequence;
        msb1.append(cs2);
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(sb: StringBuilder, s: String): Unit & Impure =
        import java.lang.StringBuilder.append(String);
        let StringBuilder(msb) = sb;
        msb.append(s);
        ()

    ///
    /// Append the Char `c` to the StringBuilder `sb`.
    ///
    pub def appendChar!(sb: StringBuilder, c: Char): Unit & Impure =
        import java.lang.StringBuilder.append(Char);
        let StringBuilder(msb) = sb;
        msb.append(c);
        ()

    ///
    /// Append the Int8 `i` to the StringBuilder `sb`.
    ///
    pub def appendInt8!(sb: StringBuilder, i: Int8): Unit & Impure =
        sb.appendString!(Int8.toString(i));
        ()

    ///
    /// Append the Int16 `i` to the StringBuilder `sb`.
    ///
    pub def appendInt16!(sb: StringBuilder, i: Int16): Unit & Impure =
        sb.appendString!(Int16.toString(i));
        ()

    ///
    /// Append the Int32 `i` to the StringBuilder `sb`.
    ///
    pub def appendInt32!(sb: StringBuilder, i: Int32): Unit & Impure =
        import java.lang.StringBuilder.append(Int32);
        let StringBuilder(msb) = sb;
        msb.append(i);
        ()

    ///
    /// Append the Int64 `i` to the StringBuilder `sb`.
    ///
    pub def appendInt64!(sb: StringBuilder, i: Int64): Unit & Impure =
        import java.lang.StringBuilder.append(Int64);
        let StringBuilder(msb) = sb;
        msb.append(i);
        ()

    ///
    /// Append the BigInt `i` to the StringBuilder `sb`.
    ///
    pub def appendBigInt!(sb: StringBuilder, i: BigInt): Unit & Impure =
        sb.appendString!(BigInt.toString(i));
        ()

    ///
    /// Append the Float32 `d` to the StringBuilder `sb`.
    ///
    pub def appendFloat32!(sb: StringBuilder, d: Float32): Unit & Impure =
        import java.lang.StringBuilder.append(Float32);
        let StringBuilder(msb) = sb;
        msb.append(d);
        ()

    ///
    /// Append the Float64 `d` to the StringBuilder `sb`.
    ///
    pub def appendFloat64!(sb: StringBuilder, d: Float64): Unit & Impure =
        import java.lang.StringBuilder.append(Float64);
        let StringBuilder(msb) = sb;
        msb.append(d);
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder): Unit & Impure =
        sb.appendString!(String.lineSeparator())

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(sb: StringBuilder, s: String): Unit & Impure =
        sb.appendString!(s);
        sb.appendLineSeparator!()

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(sb: StringBuilder, f: a -> String, x: a): Unit & Impure =
        sb.appendString!(f(x));
        sb.appendLineSeparator!()

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
        msb.length()

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder): String & Impure =
        import java.lang.StringBuilder.toString();
        let StringBuilder(msb) = sb;
        msb.toString()

}
