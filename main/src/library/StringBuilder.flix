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
opaque type StringBuilder = ##flix.runtime.library.MonoStringBuilder

namespace StringBuilder {


    ///
    /// Returns a new mutable StringBuilder.
    ///
    pub def new(): StringBuilder =
        let msb = native new flix.runtime.library.MonoStringBuilder();
        StringBuilder(msb)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder): Str =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.toString(msb) as Str

    ///
    /// Append the StringBuilder `sb1` to the end of StringBuilder `sb`.
    ///
    pub def append(sb: StringBuilder, sb1: StringBuilder): Unit =
        let StringBuilder(msb) = sb;
        let s1: Str = toString(sb1);
        native method flix.runtime.library.MonoStringBuilder.appendString(msb, s1) as Unit

    ///
    /// Alias for `appendString`
    ///
    pub def appendStr(sb: StringBuilder, s: Str): Unit = appendString(sb, s)

    ///
    /// Append the Str `s` to the StringBuffer `sb`.
    ///
    pub def appendString(sb: StringBuilder, s: Str): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendString(msb, s) as Unit

    ///
    /// Append the Char `c` to the StringBuffer `sb`.
    ///
    pub def appendChar(sb: StringBuilder, c: Char): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendChar(msb, c) as Unit

    ///
    /// Append the Int8 `i` to the StringBuffer `sb`.
    ///
    pub def appendInt8(sb: StringBuilder, i: Int8): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendByte(msb, i) as Unit

    ///
    /// Append the Int16 `i` to the StringBuffer `sb`.
    ///
    pub def appendInt16(sb: StringBuilder, i: Int16): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendShort(msb, i) as Unit

    ///
    /// Append the Int32 `i` to the StringBuffer `sb`.
    ///
    pub def appendInt32(sb: StringBuilder, i: Int32): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendInt(msb, i) as Unit

    ///
    /// Append the Int64 `i` to the StringBuffer `sb`.
    ///
    pub def appendInt64(sb: StringBuilder, i: Int64): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendLong(msb, i) as Unit

    ///
    /// Append the BigInt `i` to the StringBuffer `sb`.
    ///
    pub def appendBigInt(sb: StringBuilder, i: BigInt): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendBigInteger(msb, i) as Unit

    ///
    /// Append the Float32 `d` to the StringBuffer `sb`.
    ///
    pub def appendFloat32(sb: StringBuilder, d: Float32): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendFloat(msb, d) as Unit

    ///
    /// Append the Float64 `d` to the StringBuffer `sb`.
    ///
    pub def appendFloat64(sb: StringBuilder, d: Float64): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendDouble(msb, d) as Unit

    ///
    /// Append the system line separator to the StringBuffer `sb`.
    ///
    pub def appendLineSeparator(sb: StringBuilder): Unit =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.appendLineSeparator(msb) as Unit

    ///
    /// Append the String `s` followed by the system line separator to the StringBuffer `sb`.
    ///
    pub def appendLine(sb: StringBuilder, s: Str) : Unit =
        appendString(sb, s);
        appendLineSeparator(sb)

    ///
    /// Return the length of the StringBuffer `sb`.
    ///
    pub def length(sb: StringBuilder): Int32 =
        let StringBuilder(msb) = sb;
        native method flix.runtime.library.MonoStringBuilder.length(msb) as Int32



}
