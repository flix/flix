/*
 * Copyright 2017 Magnus Madsen, Stephen Tetley
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
    @Time(1) @Space(1)
    pub def charAt(i: Int32, s: String): Char =
        import java.lang.String.charAt(Int32): Char & Pure;
        charAt(s, i)

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    @Time(length(s1) + length(s2)) @Space(length(s1) + length(s2))
    pub def concat(s1: String, s2: String): String =
        import java.lang.String.concat(String): String & Pure;
        (s1 `concat` s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(s: String): Bool =
        import java.lang.String.isEmpty(): Bool & Pure;
        isEmpty(s)

    ///
    /// Returns the length of the string `s`.
    ///
    @Time(1) @Space(1)
    pub def length(s: String): Int32 =
        import java.lang.String.length(): Int32 & Pure;
        length(s)

    ///
    /// Returns `true` if the string `s` starts with the string `prefix`.
    ///
    pub def startsWith(prefix: {prefix :: String}, s: String): Bool =
        import java.lang.String.startsWith(String): Bool & Pure;
        (s `startsWith` prefix.prefix)

    ///
    /// Returns `true` if the string `s` ends with the string `suffix`.
    ///
    pub def endsWith(suffix: {suffix :: String}, s: String): Bool =
        import java.lang.String.endsWith(String): Bool & Pure;
        (s `endsWith` suffix.suffix)

    ///
    /// Splits the string `s` around matches of the regular expression `regex`.
    ///
    pub def split(regex: {regex :: String}, s: String): List[String] = region r {
        import java.lang.String.split(String): Array[String, r] \ Write(r);
        let _ = r; // This avoids a redundancy error.
        Array.toList(split(s, regex.regex))
    }

    ///
    /// Returns the lower case version of the string `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase(): String & Pure;
        toLowerCase(s)

    ///
    /// Returns the upper case version of the string `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase(): String & Pure;
        toUpperCase(s)

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toList(s: String): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

    ///
    /// Returns a copy of the string `s` without trailing and leading whitespaces.
    ///
    /// Returns a new empty string if there is no characters in `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def trim(s: String): String =
        import java.lang.String.trim(): String & Pure;
        trim(s)

    ///
    /// Get the system line separator.
    ///
    @Time(1) @Space(1)
    pub def lineSeparator(): String =
        import static java.lang.System.lineSeparator(): String & Pure;
        lineSeparator()

    ///
    /// Returns the given string `s` as an array of characters.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toArray(s: String, r: Region[r]): Array[Char, r] \ Write(r) =
        Array.init(i -> charAt(i, s), length(s), r)

    ///
    /// Build a string of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init(f: Int32 -> Char & ef, len: Int32): String & ef =
        if (len <= 0) "" else initHelper(f, len)

    ///
    /// Helper function for `init`.
    ///
    /// Assumes that `len > 0`.
    ///
    def initHelper(f: Int32 -> Char & ef, len: Int32): String & ef = region r {
        let sb = new StringBuilder(r);
        def loop(i) = {
            if (i >= len)
                StringBuilder.toString(sb)
            else {
                let c = f(i);
                StringBuilder.append!(c, sb);
                loop(i + 1)
            }
        };
        loop(0)
    }

    ///
    /// Concatenate a list of strings into a single string.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def flatten(xs: List[String]): String = region r {
        let sb = new StringBuilder(r);
        List.foreach(x -> StringBuilder.appendString!(x, sb), xs);
        StringBuilder.toString(sb)
    }

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    @Time(length(sep) * List.length(xs)) @Space(length(sep) * List.length(xs))
    pub def intercalate(sep: String, xs: List[String]): String = region r {
        let sb = new StringBuilder(r);
        let arr = List.toArray(xs, r);
        StringBuilder.intercalate!(sep, arr, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def intercalateChar(sep: Char, xs: List[String]): String = match xs {
        case Nil     => ""
        case x :: rs => region r {
            let sb = new StringBuilder(r);
            StringBuilder.appendString!(x, sb);
            let appendStep = s -> { StringBuilder.append!(sep, sb); StringBuilder.appendString!(s, sb) };
            List.foreach(appendStep, rs);
            StringBuilder.toString(sb)
        }
    }

    ///
    /// Returns the result of applying `f` to every character in `s`.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def map(f: Char -> Char & ef, s: String): String & ef =
        let len = length(s);
        init(i -> f(charAt(i, s)), len)

    ///
    /// Returns the result of applying `f` to every character in `s` along with that character's index.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def mapWithIndex(f: (Char, Int32) -> Char & ef, s: String): String & ef =
        let len = length(s);
        init(i -> f(charAt(i, s), i), len)

    ///
    /// Returns the reverse of `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def reverse(s: String) : String =
        let start = length(s) - 1;
        init(i -> charAt(start - i, s), start + 1)

    ///
    /// Rotate the contents of string `s` by `n` steps to the left.
    ///
    @Time(length(s)) @Space(length(s))
    pub def rotateLeft(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else if (n < 0)
            rotateRightHelper(Int32.abs(n), s)
        else
            rotateLeftHelper(n, s)

    ///
    /// Helper function for `rotateLeft`, `n` must be positive.
    ///
    def rotateLeftHelper(n: Int32, s: String): String =
        let len = length(s);
        let f = i -> { let i1 = n + i; charAt(i1 rem len, s) };
        init(f, len)

    ///
    /// Rotate the contents of string `s` by `n` steps to the right.
    ///
    @Time(length(s)) @Space(length(s))
    pub def rotateRight(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else if (n < 0)
            rotateLeftHelper(Int32.abs(n), s)
        else
            rotateRightHelper(n, s)

    ///
    /// Helper function for `rotateRight`, `n` must be positive.
    ///
    def rotateRightHelper(n: Int32, s: String): String =
        let len = length(s);
        let n1 = n rem len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; charAt(i1 rem len, s)};
        init(f, len)

    ///
    /// Returns `true` if the entire string `s` is matched by the regular expression `regex`.
    ///
    /// Note - use `isSubmatch` to search for a substring.
    ///
    pub def isMatch(regex: {regex :: String}, s: String): Bool =
        import java.lang.String.matches(String): Bool & Pure;
        try {
            matches(s, regex.regex)
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => false
        }

    ///
    /// Returns `true` if the string `s` is matched by the regular expression `regex` at any point.
    ///
    pub def isSubmatch(regex: {regex :: String}, s: String): Bool =
        isSubmatchHelper(regex.regex, s)

    ///
    /// Helper function for `isSubmatch`.
    ///
    def isSubmatchHelper(pattern: String, s: String): Bool = region r {
        import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern \ Write(r);
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence): ##java.util.regex.Matcher \ Read(r);
        import java.util.regex.Matcher.find(Int32): Bool \ Write(r);
        let _ = r; // This avoids a redundancy error.
        try {
            let cs1 = s as ##java.lang.CharSequence;
            let p1 = compile(pattern);
            let m1 = matcher(p1, cs1);
            find(m1, 0)
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => false
        }
    }

    ///
    /// Returns the substring of `s` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// If `b` or `e` are out-of-bounds, return the empty string.
    ///
    pub def slice(b: Int32, e: Int32, s: String): String = try {
        import java.lang.String.substring(Int32, Int32): String & Pure;
        substring(s, b, e)
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => ""
    }

    ///
    /// Get the substring of `s` to the left of index `e` (exclusive).
    ///
    /// `sliceLeft == slice(0 , e, s)`
    ///
    pub def sliceLeft(e: Int32, s: String): String = match length(s) {
        case len if e > len => ""
        case _   if e < 0   => ""
        case _              => slice(0, e, s)
    }

    ///
    /// Get the substring of `s` to the right starting at index `b` (inclusive).
    ///
    /// `sliceRight == slice(b , length(s), s)`
    ///
    pub def sliceRight(b: Int32, s: String): String = match length(s) {
        case len if b >= len => ""
        case _   if b < 0 => ""
        case len => slice(b, len, s)
    }

    ///
    /// Optionally returns the position of the first character in `x` satisfying `f`.
    ///
    /// Returns `None` if no character in `s` satisfies `f`.
    ///
    @Time(time(f) * length(s)) @Space(space(f))
    pub def findIndexOfLeft(f: Char -> Bool & ef, s: String): Option[Int32] & ef =
        let len = length(s);
        def loop(i) = {
            if (i >= len)
                -1
            else if (f(charAt(i, s)))
                i
            else
                loop(i + 1)
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally return the position of the first character in `s` satisfying `f`, reading right to left.
    ///
    /// If nothing satisfies `f` return None.
    ///
    @Time(time(f) * length(s)) @Space(space(f))
    pub def findIndexOfRight(f: Char -> Bool, s: String): Option[Int32] =
        def loop(i) = {
            if (i < 0)
                -1
            else if (f(charAt(i, s)))
                i
            else
                loop(i - 1)
        };
        let end = length(s) - 1;
        let i = loop(end);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `s` satisfying `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def findIndices(f: Char -> Bool, s: String): List[Int32] =
        let len = length(s);
        def loop(i, k) = {
            if (i >= len)
                k(Nil)
            else if (f(charAt(i, s)))
                loop(i + 1, ks -> k(i :: ks))
            else
                loop(i + 1, k)
        };
        loop(0, identity)

    ///
    /// Returns the positions of the all the occurrences of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string.
    ///
    pub def indices(substr: {substr :: String}, s: String): List[Int32] =
        let sublen = length(substr.substr);
        let step = ix ->
            match indexOfLeftWithOffset({substr = substr.substr}, {offset = ix}, s) {
                case None => None
                case Some(ix2) => Some(ix2, ix2 + sublen)
            };
        List.unfold(step, 0)

    ///
    /// Alias for `takeLeft`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, s: String): String = takeLeft(n, s)

    ///
    /// Take the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string `s`, return all the characters
    /// of `s`.
    ///
    @Time(n) @Space(n)
    pub def takeLeft(n: Int32, s: String): String =
        if (n >= length(s)) s else slice(0, n, s)

    ///
    /// Take the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return all the characters
    /// of `s`.
    ///
    @Time(n) @Space(n)
    pub def takeRight(n: Int32, s: String): String =
        let len = length(s);
        if (n >= len) s else slice(len - n, len, s)

    ///
    /// Alias for `dropLeft`.
    ///
    @Time(length(s) - n) @Space(length(s) - n)
    pub def drop(n: Int32, s: String): String = dropLeft(n, s)

    ///
    /// Drop the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string s, return the empty string.
    ///
    @Time(length(s) - n) @Space(length(s) - n)
    pub def dropLeft(n: Int32, s: String): String =
        if (n <= 0)
            s
        else
            slice(n, length(s), s)

    ///
    /// Drop the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return the empty string.
    ///
    @Time(length(s) - n) @Space(length(s) - n)
    pub def dropRight(n: Int32, s: String): String =
        if (n > 0)
            let len = length(s);
            slice(0, len - n, s)
        else
            s

    ///
    /// Alias for `takeWileLeft`.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def takeWhile(f: Char -> Bool, s: String): String = takeWhileLeft(f, s)

    ///
    /// Returns the initial prefix of string `s` where all the chars satisfy
    /// the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def takeWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x -> not (f(x)), s) {
            case None    => s
            case Some(i) => take(i, s)
        }

    ///
    /// Returns the suffix of string `s` where all the characters satisfy
    /// the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def takeWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> not (f(x)), s) {
            case None    => s
            case Some(i) => slice(i + 1, length(s), s)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhile(f: Char -> Bool, s: String): String = dropWhileLeft(f, s)

    ///
    /// Returns the tail of string `s` after dropping all the initial chars
    /// that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x -> not (f(x)), s) {
            case None    => ""
            case Some(i) => sliceRight(i, s)
        }

    ///
    /// Returns the front of string `s` after dropping all the characters
    /// from the right end that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> not (f(x)), s) {
            case None    => ""
            case Some(i) => sliceLeft(i + 1, s)     // must include i
        }

    ///
    /// Returns the middle of string `s` after dropping all the characters
    /// from both ends that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhileAround(f: Char -> Bool, s: String): String =
        match (findIndexOfLeft(x -> not (f(x)), s), findIndexOfRight(x -> not (f(x)), s)) {
            case (None, None)       => ""
            case (Some(l), None)    => sliceRight(l, s)
            case (None, Some(r))    => sliceLeft(r, s)
            case (Some(l), Some(r)) => slice(l, r + 1, s)
        }

    ///
    /// Split the string `s` at the position `n` returning the left and
    /// right parts.
    ///
    /// If `n` exceeds the length of string `s`, return the whole string
    /// paired with the empty string.
    ///
    @Time(length(s)) @Space(length(s))
    pub def splitAt(n: Int32, s: String): (String, String) =
        match length(s) {
            case _   if n <= 0  => ("", s)
            case len if n > len => (s, "")
            case _              => (sliceLeft(n, s), sliceRight(n, s))
        }


    ///
    /// Applies `f` to a start value `x` and all elements in `s` going from left to right.
    ///
    pub def foldLeft(f: (b, Char) -> b & ef, x: b, s: String): b & ef =
        let len = length(s);
        def loop(acc, i) = {
            if (i >= len)
                acc
            else
                let acc1 = f(acc, charAt(i, s));
                loop(acc1, i + 1)
        };
        loop(x, 0)

    ///
    /// Applies `f` to a start value `x and all elements in `s` going from right to left.
    ///
    pub def foldRight(f: (Char, b) -> b & ef, x: b, s: String): b &  ef =
        def loop(acc, i) = {
            if (i < 0)
                acc
            else
                let acc1 = f(charAt(i, s), acc);
                loop(acc1, i - 1)
        };
        let end = length(s) - 1;
        loop(x, end)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from left to right.
    ///
    pub def foldLeft2(f: (b, Char, Char) -> b & ef, x: b, s: String, t: String): b & ef =
        let len = Int32.min(length(s), length(t));
        def loop(acc, i) = {
            if (i >= len)
                acc
            else
                let acc1 = f(acc, charAt(i, s), charAt(i, t));
                loop(acc1, i + 1)
        };
        loop(x, 0)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from right to left.
    ///
    pub def foldRight2(f: (Char, Char, b) -> b & ef, x: b, s: String, t: String): b &  ef =
        let sn = length(s) - 1;
        let tn = length(t) - 1;
        def loop(acc, i, j) = {
            if (i < 0 or j < 0)
                acc
            else
                let acc1 = f(charAt(i, s), charAt(j, t), acc);
                loop(acc1, i - 1, j - 1)
        };
        loop(x, sn, tn)

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    pub def unfold(f: b -> Option[(Char, b)] & ef, x: b): String & ef = region r {
        let sb = new StringBuilder(r);
        def loop(a) = {
            let a1 = f(a);
            match a1 {
                case None         => StringBuilder.toString(sb)
                case Some(c, st1) => {
                    StringBuilder.append!(c, sb);
                    loop(st1)
                }
            }
        };
        loop(x)
    }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(c)` to signal a new char `c`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldWithIter(f: Unit -> Option[Char] & ef): String & ef = region r {
        let sb = new StringBuilder(r);
        def loop() = {
            match f() {
                case None    => StringBuilder.toString(sb)
                case Some(c) => {
                    StringBuilder.append!(c, sb);
                    loop()
                }
            }
        };
        loop()
    }

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    /// This is a version of `unfold` where `f` generates substrings rather than chars.
    ///
    pub def unfoldString(f: b -> Option[(String, b)] & ef, x: b) : String & ef = region r {
        let sb = new StringBuilder(r);
        def loop(a) = {
            let a1 = f(a);
            match a1 {
                case None         => StringBuilder.toString(sb)
                case Some(s, st1) => {
                    StringBuilder.appendString!(s, sb);
                    loop(st1)
                }
            }
        };
        loop(x)
    }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(s)` to signal a new substring `s`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldStringWithIter(f: Unit -> Option[String] & ef): String & ef = region r {
        let sb = new StringBuilder(r);
        def loop() = {
            match f() {
                case None    => StringBuilder.toString(sb)
                case Some(s) => {
                    StringBuilder.appendString!(s, sb);
                    loop()
                }
            }
        };
        loop()
    }

    ///
    /// Returns `true` if and only if at least one char in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def exists(f: Char -> Bool & ef, s: String): Bool & ef = match findIndexOfLeft(f, s) {
        case None    => false
        case Some(_) => true
    }

    ///
    /// Returns `true` if and only if all chars in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is empty.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def forall(f: Char -> Bool & ef, s: String): Bool & ef = match findIndexOfLeft(x -> not (f(x)), s) {
        case None    => true
        case Some(_) => false
    }

    ///
    /// Returns `true` if and only if all chars in `s` are ascii characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    @Time(length(s)) @Space(length(s))
    pub def isAscii(s: String): Bool =
        forall(Char.isAscii, s)

    ///
    /// Returns `true` if and only if all chars in `s` are white space characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    @Time(length(s)) @Space(length(s))
    pub def isWhiteSpace(s: String): Bool =
        forall(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all leading space characters removed.
    ///
    @Time(length(s)) @Space(length(s))
    pub def trimLeft(s: String): String = dropWhileLeft(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all trailing space characters removed.
    ///
    @Time(length(s)) @Space(length(s))
    pub def trimRight(s: String): String = dropWhileRight(Char.isWhiteSpace, s)

    ///
    /// Returns a string with the string `s` repeated `n` times.
    ///
    /// Returns the empty string if `n < 0`.
    ///
    @Time(length(s) * n) @Space(length(s) * n)
    pub def repeat(n: Int32, s: String): String =
        import java.lang.String.repeat(Int32): String & Pure;
        if (n < 0)
            ""
        else
            repeat(s, n)

    ///
    /// Pad the string `s` at the left with the supplied char `c` to fit the width `w`.
    ///
    @Time(w) @Space(w)
    pub def padLeft(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            repeat(w - len, Char.toString(c)) + s

    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit the width `w`.
    ///
    @Time(w) @Space(w)
    pub def padRight(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            s + repeat(w - len, Char.toString(c))

    ///
    /// Returns `s` with every match of the substring `from` replaced by the string `to`.
    ///
    pub def replace(from: {from :: String}, to: {to :: String}, s: String): String =
        import java.lang.String.replace(##java.lang.CharSequence, ##java.lang.CharSequence): String & Pure;
        let t = from.from as ##java.lang.CharSequence;
        let r = to.to as ##java.lang.CharSequence;
        replace(s, t, r)

    ///
    /// Returns `s` with every match of the character `target` replaced by the character `rep`.
    ///
    pub def replaceChar(from: {from :: Char}, to: {to :: Char}, s: String): String =
        import java.lang.String.replace(Char, Char): String & Pure;
        replace(s, from.from, to.to)

    ///
    /// Returns `s` with every match of the regular expression `regex` replaced by the string `to`.
    ///
    pub def replaceMatches(regex: {regex :: String}, to: {to :: String}, s: String): String =
        import java.lang.String.replaceAll(String, String): String & Pure;
        replaceAll(s, regex.regex, to.to)

    ///
    /// Returns `s` with the first match of the regular expression `regex` replaced by the string `to`.
    ///
    pub def replaceFirstMatch(regex: {regex :: String}, to: {to :: String}, s: String): String =
        import java.lang.String.replaceFirst(String, String): String & Pure;
        replaceFirst(s, regex.regex, to.to)

    ///
    /// Returns `s` with the element at index `i` replaced by `a`.
    ///
    /// Returns `s` if `i < 0` or `i > length(xs)-1`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def update(i: Int32, a: Char, s: String): String =
        let f = ix -> if (ix == i) a else charAt(ix, s);
        init(f, length(s))

    ///
    /// Returns `s` with the `n` elements starting at index `i` replaced with the elements of `sub`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `s` then no patching is done at these indices.
    /// If `s` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `s`, then the element at index `j` in `sub` is used.
    ///
    @Time(length(s)) @Space(length(s))
    pub def patch(i: Int32, n: Int32, sub: String, s: String): String =
        let sublen = length(sub);
        let f = ix ->
            if (ix >= i and ix < i + n) {
                let offset = ix - i;
                if (offset < 0 or offset >= sublen) charAt(ix,s) else charAt(offset, sub)
            } else charAt(ix, s);
        init(f, length(s))


    ///
    /// Indent every line in string `s` by `n` spaces. `n` must be greater than `0`.
    ///
    /// If the string `s` in nonempty, the returned string normalizes line
    /// termination characters and adds a line terminator to the last line
    /// of the string if it does not already end with a newline.
    ///
    /// If the string `s` is empty, then the empty string is returned.
    ///
    @Time(length(s) + n) @Space(length(s) + n)
    pub def indent(n: Int32, s: String): String =
        if (n <= 0 or length(s) == 0)
            s
        else
            indentHelper(n, s)

    ///
    /// Helper function for `indent`.
    ///
    def indentHelper(n: Int32, s: String): String = region r {
        let sb = new StringBuilder(r);
        let prefix = repeat(n, " ");
        let step = x -> {
            StringBuilder.appendString!(prefix, sb);
            StringBuilder.appendLine!(x, sb)
        };
        List.foreach(step, lines(s));
        StringBuilder.toString(sb)
    }


    ///
    /// Strip every indented line in string `s` by `n` spaces. `n` must be greater than `0`.
    /// Note, tabs are counted as a single space.
    ///
    /// If the string `s` in nonempty, the returned string normalizes line
    /// termination characters and adds a line terminator to the last line
    /// of the string if it does not already end with a newline.
    ///
    /// If the string `s` is empty, then the empty string is returned.
    ///
    @Time(length(s) + n) @Space(length(s) + n)
    pub def stripIndent(n: Int32, s: String): String =
        if (n <= 0 or length(s) == 0)
            s
        else
            stripIndentHelper(n, s)

    ///
    /// Helper function for `stripIndent`.
    ///
    def stripIndentHelper(n: Int32, s: String): String = region r {
        let sb = new StringBuilder(r);
        let limit = Int32.min(n, length(s));
        def loop(s1, ix) = {
            if (ix >= limit)
                sliceRight(ix, s1)
            else if (not Char.isWhiteSpace(charAt(ix, s1)))
                sliceRight(ix, s1)
            else
                loop(s1, ix+1)
        };
        let step = s1 -> {
            let line = loop(s1, 0);
            StringBuilder.appendLine!(line, sb)
        };
        List.foreach(step, lines(s));
        StringBuilder.toString(sb)
    }

    ///
    /// Split the string `s` into an array of lines, breaking on newline.
    ///
    /// Newline is recognized as any Unicode linebreak sequence including
    /// Windows (carriage return, line feed) or Unix (line feed).
    ///
    @Time(length(s)) @Space(length(s))
    pub def lines(s: String): List[String] =
        if (isEmpty(s))
            Nil
        else
            split({regex = "\\R"}, s)

    ///
    /// Join the array of strings `a` separating each pair of strings and
    /// ending the result string with the system dependent line separator.
    ///
    @Time(List.length(a)) @Space(List.length(a))
    pub def unlines(a: List[String]): String = unlinesHelper(a)

    ///
    /// Helper function for `unlines`.
    ///
    def unlinesHelper(a: List[String]): String = region r {
        let sb = new StringBuilder(r);
        List.foreach(x -> StringBuilder.appendLine!(x, sb), a);
        StringBuilder.toString(sb)
    }

    ///
    /// Split the string `s` into an list of words, dividing on one or more white space characters.
    /// Leading and trailing spaces are trimmed.
    ///
    @Time(length(s)) @Space(length(s))
    pub def words(s: String): List[String] = match trim(s) {
        case s1 if isEmpty(s1) => Nil
        case s1 => split({regex = "\\s+"}, s1)
    }

    ///
    /// Join the array of strings `a` separating each pair of strings with a
    /// single space character.
    ///
    @Time(List.length(a)) @Space(List.length(a))
    pub def unwords(a: List[String]): String = unwordsHelper(a)

    ///
    /// Helper function for `unlines`.
    ///
    def unwordsHelper(xs: List[String]): String = region r {
        let sb = new StringBuilder(r);
        let first = ref true;
        let step = s ->
            if (deref first == true) {
                first := false;
                StringBuilder.appendString!(s, sb)
            } else {
                StringBuilder.append!(' ', sb);
                StringBuilder.appendString!(s, sb)
            };
        List.foreach(step, xs);
        StringBuilder.toString(sb)
    }

    ///
    /// Alias for `indexOfLeft`.
    ///
    pub def indexOf(substr: {substr :: String}, s: String): Option[Int32] = indexOfLeft({substr = substr.substr}, s)

    ///
    /// Return the index of the first occurence of `substr` in `s` from the left.
    ///
    /// If `substr` is not present in `s` return None.
    ///
    /// If `substr` is the empty string return None.
    ///
    pub def indexOfLeft(substr: {substr :: String}, s: String): Option[Int32] =
        import java.lang.String.indexOf(String): Int32 & Pure;
        if (isEmpty(substr.substr))
            None
        else {
            let i = indexOf(s, substr.substr);
            if (i < 0) None else Some(i)
        }

    ///
    /// Return the index of the first occurence of `substr` in `s` from the right.
    ///
    /// If `substr` is not present in `s` return None.
    ///
    /// If `substr` is the empty string return None.
    ///
    pub def indexOfRight(substr: {substr :: String}, s: String): Option[Int32] =
        import java.lang.String.lastIndexOf(String): Int32 & Pure;
        if (isEmpty(substr.substr))
            None
        else {
            let i = lastIndexOf(s, substr.substr);
            if (i < 0) None else Some(i)
        }

    ///
    /// Returns `true` if and only if `substr` is an infix of `s`.
    ///
    pub def contains(substr: {substr :: String}, s: String): Bool =
        if (isEmpty(substr.substr))
            true
        else {
            let a = indexOfLeft({substr = substr.substr}, s);
            not Option.isEmpty(a)
        }

    ///
    /// Return the common prefix of strings `s1` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common prefix.
    ///
    @Time(Int32.min(length(s1), length(s2))) @Space(Int32.min(length(s1), length(s2)))
    pub def commonPrefix(s1: String, s2: String): String =
        let len1 = length(s1);
        let len2 = length(s2);
        def loop(ix) = {
            if (ix >= len1 or ix >= len2)
                takeLeft(ix, s1)
            else {
                let c1 = charAt(ix, s1);
                let c2 = charAt(ix, s2);
                if (c1 != c2)
                    takeLeft(ix, s1)
                else
                    loop(ix+1)
            }
        };
        loop(0)

    ///
    /// Return the common suffix of strings `s1` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common suffix.
    ///
    @Time(Int32.min(length(s1), length(s2))) @Space(Int32.min(length(s1), length(s2)))
    pub def commonSuffix(s1: String, s2: String): String =
            def loop(i, j, acc) = {
                if (i < 0)
                    s1
                else if (j < 0)
                    s2
                else {
                    let c1 = charAt(i, s1);
                    let c2 = charAt(j, s2);
                    if (c1 != c2)
                        takeRight(acc, s1)
                    else
                        loop(i-1, j-1, acc+1)
                }
            };
            let start1 = length(s1) - 1;
            let start2 = length(s2) - 1;
            loop(start1, start2, 0)

    ///
    /// Abbreviate the string `s` if it exceeds the width `w`.
    ///
    /// If the length of `s` exceeds `w` and `w >= 3" then `s` is truncated and the first
    /// three characters are replaced with ellipses.
    ///
    /// If the length of `s` exceeds `w` and `w < 3" then the empty string is returned.
    ///
    @Time(length(s)) @Space(length(s))
    pub def abbreviateLeft(w: Int32, s: String): String =
        match length(s) {
            case len if len < w => s
            case _   if w < 3   => ""
            case len            => {
                let start = len - w + 3;
                "..." + slice(start, len, s)
            }
        }

    ///
    /// Abbreviate the string `s` if it exceeds the width `w`.
    ///
    /// If the length of `s` exceeds `w` and `w >= 3" then `s` is truncated and the last
    /// three characters are replaced with ellipses.
    ///
    /// If the length of `s` exceeds `w` and `w < 3" then the empty string is returned.
    ///
    @Time(length(s)) @Space(length(s))
    pub def abbreviateRight(w: Int32, s: String): String =
        match length(s) {
            case len if len < w => s
            case _   if w < 3   => ""
            case _              => slice(0, w - 3, s) + "..."
        }

    ///
    /// Calculate the Levenshtein distance between the strings `s` and `t`.
    ///
    /// The answer is the number deletions, insertions or substitutions needed to turn
    /// string `s` into string `t`.
    ///
    @Time(length(s) * length(t)) @Space(length(s) * length(t))
    pub def levenshteinDistance(s: String, t: String) : Int32 = levenshteinDistanceHelper(s, t) as & Pure

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    /// This is the Wikipedia two-matrix rows algorithm:
    /// https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_two_matrix_rows
    ///
    def levenshteinDistanceHelper(s: String, t: String) : Int32 & Impure =
        let m = length(s);
        let n = length(t);
        let v0 = Array.new(0, n + 1, Static);
        let v1 = Array.new(0, n + 1, Static);
        forIndex(0, n, i -> v0[i] = i);
        forIndex(0, m - 1, i -> {
            v1[0] = i + 1;
            forIndex(0, n - 1, j -> {
                let substitutionCost = if (charAt(i, s) == charAt(j, t)) 0 else 1;
                let a = v1[j] + 1;
                let b = v0[j + 1] + 1;
                let c = v0[j] + substitutionCost;
                v1[j + 1] = Int32.min(a, Int32.min(b, c))
            });
            arraySwap(v0,v1)
        });
        v0[n]

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    /// This implements a forloop, the end point `e` is inclusive.
    ///
    def forIndex(b: Int32, e: Int32, f: Int32 -> Unit & ef): Unit & ef =
        if (b <= e) { f(b); forIndex(b + 1, e, f) } else ()

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    def arraySwap(a: Array[Int32, Static], b : Array[Int32, Static]): Unit & Impure =
        forIndex(0, Array.length(a) - 1, {i ->  a[i] = b[i]})

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(Int32.min(length(a), length(b))) @Space(Int32.min(length(a), length(b)))
    pub def zip(a: String, b: String): List[(Char, Char)] =
        let len = Int32.min(length(a), length(b));
        // Array.init(i -> (charAt(i, a), charAt(i, b)), len, Static)
        List.unfold(i -> if (i >= len) None else Some((charAt(i, a), charAt(i, b)), i + 1), 0)

    ///
    /// Returns a string where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(Int32.min(length(a), length(b))) @Space(Int32.min(length(a), length(b)))
    pub def zipWith(f: (Char, Char) -> Char & ef, a: String, b: String): String & ef =
        let len = Int32.min(length(a), length(b));
        init(i -> f(charAt(i, a), charAt(i, b)), len)

    ///
    /// Split the string `s` on matches of `substr`.
    ///
    pub def splitOn(substr: {substr :: String}, s: String): List[String] =
        let maxlen = length(s);
        let sublen = length(substr.substr);
        let step = ix ->
            // Note - to stop we have to be greater than maxlen.
            // We want to return a final empty string if `s` terminates with `substr`.
            if (ix > maxlen)
                None
            else {
                match indexOfLeftWithOffset({substr = substr.substr}, {offset = ix}, s) {
                    case None => {
                        let s1 = sliceRight(ix, s);
                        Some(s1, maxlen + 1)
                    }
                    case Some(ix2) => {
                        let s1 = slice(ix, ix2, s);
                        Some(s1, ix2 + sublen)
                    }
                }
            };
        List.unfold(step, 0)

    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfLeftWithOffset(substr: {substr :: String}, offset: {offset :: Int32}, s: String): Option[Int32] =
        import java.lang.String.indexOf(String, Int32): Int32 & Pure;
        if (isEmpty(substr.substr))
            None
        else {
            let ix = indexOf(s, substr.substr, offset.offset);
            if (ix < 0) None else Some(ix)
        }

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfRightWithOffset(substr: {substr :: String}, offset: {offset :: Int32}, s: String): Option[Int32] =
        import java.lang.String.lastIndexOf(String, Int32): Int32 & Pure;
        if (isEmpty(substr.substr))
            None
        else {
            let ix = lastIndexOf(s, substr.substr, offset.offset);
            if (ix < 0) None else Some(ix)
        }

    ///
    /// Split the string `s` into chunks of length `k`, the last chunk may be smaller than `k`.
    ///
    /// `k` should be greater than 0.
    ///
    @Time(length(s) - k) @Space(length(s) - k)
    pub def toChunks(k: Int32, s: String): List[String] =
        if (k <= 0)
            Nil
        else
            toChunksHelper(k, s)

    ///
    /// Helper function for `toChunks`.
    ///
    /// Precondition: size > 0
    ///
    def toChunksHelper(size: Int32, s: String): List[String] =
        let len = length(s);
        let step = pos -> match pos {
            case i if i >= len          => None
            case i if i + size >= len   => Some(sliceRight(i, s), len)
            case i                      => Some(slice(i, i + size, s), i + size)
        };
        List.unfold(step, 0)

    ///
    /// Find the first instance of `substr` in string `s`, return a pair of the
    /// prefix of string `s` up to `substr` and the rest of string `s` including `substr`.
    ///
    pub def breakOnLeft(substr: {substr :: String}, s: String): (String, String) =
        match indexOfLeft({substr = substr.substr}, s) {
            case None => (s, "")
            case Some(i) => (sliceLeft(i, s), sliceRight(i, s))
        }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(substr: {substr :: String}, s: String): (String, String) =
        let sublen = length(substr.substr);
        match indexOfRight({substr = substr.substr}, s) {
            case None => (s, "")
            case Some(i) => (sliceLeft(i + sublen, s), sliceRight(i + sublen, s))
        }

    ///
    /// Count the occurrences of `substr` in string `s`.
    ///
    pub def countSubstring(substr: {substr :: String}, s: String): Int32 =
        let sublen = length(substr.substr);
        def loop(offset, count) = {
            match indexOfLeftWithOffset({substr = substr.substr}, {offset = offset}, s) {
            case None    => count
            case Some(i) => loop(i + sublen, count + 1)
            }
        };
        if (sublen <= 0)
            0
        else
            loop(0, 0)

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `substr`.
    ///
    pub def stripPrefix(substr: {substr :: String}, s: String): Option[String] =
        if (not startsWith({prefix = substr.substr}, s))
            None
        else
            Some(dropLeft(length(substr.substr), s))

    ///
    /// Returns `Some(prefix)` of string `s` if its suffix matches `substr`.
    ///
    pub def stripSuffix(substr: {substr :: String}, s: String): Option[String] =
        if (not endsWith({suffix = substr.substr}, s))
            None
        else
            Some(dropRight(length(substr.substr), s))

}
