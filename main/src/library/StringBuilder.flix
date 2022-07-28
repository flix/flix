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
enum StringBuilder[_: Region](##java.lang.StringBuilder)

instance Newable[StringBuilder] {
    pub def new(r: Region[r]): StringBuilder[r] \ Write(r) = StringBuilder.new(r)
}

instance Scoped[StringBuilder] {
    pub def regionOf(_: StringBuilder[r]): Region[r] = () as Region[r]
}

namespace StringBuilder {

    ///
    /// Returns a new mutable StringBuilder.
    ///
    pub def new(_: Region[r]): StringBuilder[r] \ Write(r) =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & r as newStringBuilder;
        StringBuilder(newStringBuilder())

    ///
    /// Append `x` to the StringBuilder `sb`.
    ///
    pub def append!(x: a, sb: StringBuilder[r]): Unit \ Write(r) with ToString[a] =
        let s = ToString.toString(x);
        s `appendString!` sb;
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(s: String, sb: StringBuilder[r]): Unit \ Write(r) =
        import java.lang.StringBuilder.append(String): ##java.lang.StringBuilder & r;
        let StringBuilder(msb) = sb;
        discard msb `append` s;
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder[r]): Unit \ Write(r) =
        String.lineSeparator() `appendString!` sb

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(s: String, sb: StringBuilder[r]): Unit \ Write(r) =
        s `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(f: a -> String & ef, x: a, sb: StringBuilder[r]): Unit \ { ef, Write(r) } =
        f(x) `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends each string in the array `a` to the string builder `sb`.
    ///
    pub def appendLines!(a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { Read(r1), Write(r2) } =
        Array.foreach(x -> appendLine!(x, sb), a)

    ///
    /// Appends `f(x)` for each x in array `a` to the string builder `sb`.
    ///
    pub def appendLinesWith!(f: a -> String & ef, a: Array[a, r1], sb: StringBuilder[r2]): Unit \ { ef, Read(r1), Write(r2) } =
        Array.foreach(x -> appendLineWith!(f, x, sb), a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate!(sep: String, a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { Read(r1), Write(r2) } =
        let append1! = (s,i) ->
            if (i > 0) {
                appendString!(sep, sb);
                appendString!(s, sb)
            } else {
                appendString!(s, sb)
            };
       Array.foreachWithIndex(append1!, a)

    ///
    /// Returns an iterator over `sb`.
    ///
    pub def iterator(r1: Region[r1], sb: StringBuilder[r2]): Iterator[Char, r1 and r2] \ { Read(r2), Write(r1) } =
        import java.lang.StringBuilder.charAt(Int32): Char \ Read(r2);
        let StringBuilder(msb) = sb;
        let len = length(sb);
        let i = ref 0 @ r1;
        let done = () -> { deref i >= len } as & r1 and r2;
        let next = () -> {
            let x = charAt(msb, deref i);
            i := deref i + 1;
            x
        };
        Iterator(done, next)

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(r1: Region[r1], sb: StringBuilder[r2]): Iterator[(Char, Int32), r1 and r2] \ { Read(r2), Write(r1) } =
        iterator(r1, sb) |> Iterator.zipWithIndex

    ///
    /// Return the length of the StringBuilder `sb`.
    ///
    pub def length(sb: StringBuilder[r]): Int32 \ Read(r) =
        import java.lang.StringBuilder.length(): Int32 & r;
        let StringBuilder(msb) = sb;
        length(msb)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder[r]): String \ Read(r) =
        import java.lang.StringBuilder.toString(): String & r;
        let StringBuilder(msb) = sb;
        toString(msb)

}

