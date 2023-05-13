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

mod StringBuilder {

    ///
    /// Returns a new mutable StringBuilder.
    ///
    pub def new(_: Region[r]): StringBuilder[r] \ r =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ r as newStringBuilder;
        StringBuilder(newStringBuilder())

    ///
    /// Append `x` to the StringBuilder `sb`.
    ///
    pub def append!(x: a, sb: StringBuilder[r]): Unit \ r with ToString[a] =
        let s = ToString.toString(x);
        s `appendString!` sb;
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(s: String, sb: StringBuilder[r]): Unit \ r =
        import java.lang.StringBuilder.append(String): ##java.lang.StringBuilder \ r;
        let StringBuilder(msb) = sb;
        discard msb `append` s;
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder[r]): Unit \ r =
        String.lineSeparator() `appendString!` sb

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(s: String, sb: StringBuilder[r]): Unit \ r =
        s `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(f: a -> String \ ef, x: a, sb: StringBuilder[r]): Unit \ { ef, r } =
        f(x) `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends each string in the array `a` to the string builder `sb`.
    ///
    pub def appendLines!(a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { r1, r2 } =
        Array.forEach(x -> appendLine!(x, sb), a)

    ///
    /// Appends `f(x)` for each x in the foldable collection `t` to the string builder `sb`.
    ///
    pub def appendLinesWith(f: a -> String \ ef, t: t[a], sb: StringBuilder[r]): Unit \ { ef, r } with Foldable[t] =
        Foldable.forEach(x -> appendLineWith!(f, x, sb), t)

    ///
    /// Appends `f(x)` for each x in array `a` to the string builder `sb`.
    ///
    pub def appendLinesWith!(f: a -> String \ ef, a: Array[a, r1], sb: StringBuilder[r2]): Unit \ { ef, r1, r2 } =
        Array.forEach(x -> appendLineWith!(f, x, sb), a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate!(sep: String, a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { r1, r2 } =
        let append1! = (i, s) ->
            if (i > 0) {
                appendString!(sep, sb);
                appendString!(s, sb)
            } else {
                appendString!(s, sb)
            };
       Array.forEachWithIndex(append1!, a)

    ///
    /// Returns an iterator over `sb`.
    ///
    pub def iterator(rc: Region[r1], sb: StringBuilder[r2]): Iterator[Char, r1 and r2, r1] \ { r2, r1 } =
        import java.lang.StringBuilder.charAt(Int32): Char \ r2;
        let StringBuilder(msb) = sb;
        Iterator.range(rc, 0, length(sb)) |> Iterator.map(i -> charAt(msb, i))

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(r1: Region[r1], sb: StringBuilder[r2]): Iterator[(Int32, Char), r1 and r2, r1] \ { r2, r1 } =
        iterator(r1, sb) |> Iterator.zipWithIndex

    ///
    /// Return the length of the StringBuilder `sb`.
    ///
    pub def length(sb: StringBuilder[r]): Int32 \ r =
        import java.lang.StringBuilder.length(): Int32 \ r;
        let StringBuilder(msb) = sb;
        length(msb)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder[r]): String \ r =
        import java.lang.StringBuilder.toString(): String \ r;
        let StringBuilder(msb) = sb;
        toString(msb)

}

