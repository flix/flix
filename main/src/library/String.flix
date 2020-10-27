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
    pub def charAt(i: Int, s: String): Char =
        import java.lang.String.charAt(Int32);
        s.charAt(i) as & Pure

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    @Time(length(s1) + length(s2)) @Space(length(s1) + length(s2))
    pub def concat(s1: String, s2: String): String =
        import java.lang.String.concat(String);
        s1.concat(s2) as & Pure

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(s: String): Bool =
        import java.lang.String.isEmpty();
        s.isEmpty() as & Pure

    ///
    /// Returns the length of the string `s`.
    ///
    @Time(1) @Space(1)
    pub def length(s: String): Int =
        import java.lang.String.length();
        s.length() as & Pure

    ///
    /// Returns `true` if the string `s1` starts with the string `s2`.
    ///
    @Time(length(s2)) @Space(1)
    pub def startsWith(s1: String, s2: String): Bool =
        import java.lang.String.startsWith(String);
        s1.startsWith(s2) as & Pure

    ///
    /// Returns `true` if the string `s1` ends with the string `s2`.
    ///
    @Time(length(s2)) @Space(1)
    pub def endsWith(s1: String, s2: String): Bool =
        import java.lang.String.endsWith(String);
        s1.endsWith(s2) as & Pure

    ///
    /// Splits the string `s` around matches of the regular expression `r`.
    ///
    @Time(length(s) + length(r)) @Space(length(s))
    pub def split(s: String, r: String): Array[String] & Impure =
        import java.lang.String.split(String);
        s.split(r)

    ///
    /// Returns the lower case version of the string `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase();
        s.toLowerCase() as & Pure

    ///
    /// Returns the upper case version of the string `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase() as & Pure

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
        import java.lang.String.trim();
        s.trim() as & Pure

    ///
    /// Get the system line separator.
    ///
    @Time(1) @Space(1)
    pub def lineSeparator(): String =
        import java.lang.System:lineSeparator();
        lineSeparator() as & Pure

    ///
    /// Returns the given string `s` as an array of characters.
    ///
    @Time(length(s)) @Space(length(s))
    pub def toArray(s: String): Array[Char] & Impure =
        Array.init(i -> charAt(i, s), length(s))

    ///
    /// Build a string of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init(f: Int32 -> Char & e, len: Int32): String & e = initHelper(f, len) as & e

    ///
    /// Helper function for `init`.
    ///
    def initHelper(f: Int32 -> Char & e, len: Int32): String & Impure =
        if (len <= 0)
            ""
        else {
            let sb = StringBuilder.new();
            initWorker(sb, f, len, 0);
            StringBuilder.toString(sb)
        }

    ///
    /// Helper function for `init`.
    ///
    def initWorker(sb: StringBuilder, f: Int32 -> Char & e, len: Int32, i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            let c = f(i) as & e;
            StringBuilder.appendChar!(sb, c);
            initWorker(sb, f, len, i+1)
        }

    ///
    /// Concatenate a list of strings into a single string.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def flatten(xs: List[String]): String = flattenHelper(xs) as & Pure

    ///
    /// Helper function for `flatten`.
    ///
    def flattenHelper(xs: List[String]): String & Impure =
        let sb = StringBuilder.new();
        List.foreach(StringBuilder.appendString!(sb), xs);
        StringBuilder.toString(sb)

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    @Time(length(sep) * List.length(xs)) @Space(length(sep) * List.length(xs))
    pub def intercalate(sep: String, xs: List[String]): String =
        intercalateHelper(sep, xs) as & Pure

    ///
    /// Helper function for `intercalate`.
    ///
    def intercalateHelper(sep: String, xs: List[String]): String & Impure =
        let sb = StringBuilder.new();
        let arr = List.toArray(xs);
        StringBuilder.intercalate!(sb, sep, arr);
        StringBuilder.toString(sb)

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def intercalateChar(sep: Char, xs: List[String]): String = intercalateCharHelper(sep, xs) as & Pure

    ///
    /// Helper function for `intercalateChar`.
    ///
    def intercalateCharHelper(sep: Char, xs: List[String]): String & Impure = match xs {
        case Nil => ""
        case x :: rs => {
            let sb = StringBuilder.new();
            StringBuilder.appendString!(sb, x);
            let appendStep = s -> { StringBuilder.appendChar!(sb, sep); StringBuilder.appendString!(sb, s) };
            List.foreach(appendStep, rs);
            StringBuilder.toString(sb)
        }
    }

    ///
    /// Returns the result of applying `f` to every character in `s`.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def map(f: Char -> Char & e, s: String): String & e =
        let len = length(s);
        init(i -> f(charAt(i, s)), len)

    ///
    /// Returns the result of applying `f` to every character in `s` along with that character's index.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def mapWithIndex(f: (Char, Int32) -> Char & e, s: String): String & e =
        let len = length(s);
        init(i -> f(charAt(i, s), i), len)

    ///
    /// Returns a string where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `s` and `b` is the element at index `i` in `t`.
    ///
    /// If either `s` or `t` becomes depleted, then no further elements are added to the resulting string.
    /// Functionally equivalent to zipWith.
    ///
    @Time(time(f) * Int32.min(length(s), length(t))) @Space(space(f) * Int32.min(length(s), length(t)))
    pub def map2(f: (Char, Char) -> Char & e, s: String, t: String): String & e =
        let len = Int32.min(length(s), length(t));
        init(i -> f(charAt(i, s), charAt(i, t)), len)

    ///
    /// Returns the reverse of `s`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def reverse(s: String) : String =
        let start = length(s) - 1;
        init(i -> charAt(start - i, s), start+1)

    ///
    /// Rotate the contents of string `s` by `n` steps to the left.
    ///
    @Time(length(s)) @Space(length(s))
    pub def rotateLeft(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else {
            if (n < 0)
                rotateRightHelper(Int32.abs(n), s)
            else
                rotateLeftHelper(n, s)
        }

    ///
    /// Helper function for `rotateLeft`, `n` must be positive.
    ///
    def rotateLeftHelper(n: Int32, s: String): String =
        let len = length(s);
        let f = i -> { let i1 = n + i; charAt(i1 % len, s) };
        init(f, len)

    ///
    /// Rotate the contents of string `s` by `n` steps to the right.
    ///
    @Time(length(s)) @Space(length(s))
    pub def rotateRight(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else {
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), s)
            else
                rotateRightHelper(n, s)
        }
    ///
    /// Helper function for `rotateRight`, `n` must be positive.
    ///
    def rotateRightHelper(n: Int32, s: String): String =
        let len = length(s);
        let n1 = n % len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; charAt(i1 % len, s)};
        init(f, len)

    ///
    /// Returns `true` if the entire string `s` is matched by the regular expression `pattern`.
    ///
    /// Note - use `isSubmatch` to search for a substring.
    ///
    @Time(length(s)) @Space(1)
    pub def isMatch(pattern: String, s: String): Bool =
        try {
            import java.lang.String.matches(String);
            s.matches(pattern) as & Pure
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => false
        }

    ///
    /// Returns `true` if the string `s` is matched by the regular expression `pattern` at any point.
    ///
    @Time(length(s)) @Space(1)
    pub def isSubmatch(pattern: String, s: String): Bool =
        isSubmatchHelper(pattern, s) as & Pure

    ///
    /// Helper function for `isSubmatch`.
    ///
    def isSubmatchHelper(pattern: String, s: String): Bool & Impure =
        try {
            import java.util.regex.Pattern:compile(String);
            import java.util.regex.Pattern.matcher(##java.lang.CharSequence);
            import java.util.regex.Matcher.find(Int32);
            let cs1 = s as ##java.lang.CharSequence;
            let p1 = compile(pattern);
            let m1 = p1.matcher(cs1);
            m1.find(0)
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => false
        }

    ///
    /// Returns the substring of `s` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// If `b` or `e` are out-of-bounds, return the empty string.
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(s: String, b: Int32, e: Int32): String  = try {
        import java.lang.String.substring(Int32, Int32);
        s.substring(b, e) as & Pure
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => ""
    }

    ///
    /// Get the substring of `s` to the left of index `e` (exclusive).
    ///
    /// `sliceLeft == slice(s, 0 , e)`
    ///
    @Time(e) @Space(e)
    pub def sliceLeft(s: String, e: Int32): String = match length(s) {
        case len if e > len => ""
        case _   if e < 0   => ""
        case _              => slice(s, 0, e)
    }

    ///
    /// Get the substring of `s` to the right starting at index `b` (inclusive).
    ///
    /// `sliceRight == slice(s, b , length(s))`
    ///
    @Time(length(s) - b) @Space(length(s) - b)
    pub def sliceRight(s: String, b: Int32): String = match length(s) {
        case len if b >= len => ""
        case _   if b < 0 => ""
        case len => slice(s, b, len)
    }



    ///
    /// Optionally returns the position of the first character in `x` satisfying `f`.
    ///
    /// Returns `None` if no character in `s` satisfies `f`.
    ///
    @Time(time(f) * length(s)) @Space(space(f))
    pub def findIndexOfLeft(f: Char -> Bool, s: String): Option[Int] =
        let i = findIndexOfLeftHelper(f, s, length(s), 0);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `findIndexOfLeft`.
    ///
    def findIndexOfLeftHelper(f: Char -> Bool, s: String, len: Int32, i: Int32): Int32 =
        if (i >= len)
            -1
        else
            if (f(charAt(i, s)))
                i
            else
                findIndexOfLeftHelper(f, s, len, i+1)

    ///
    /// Optionally return the position of the first character in `s` satisfying `f`, reading right to left.
    ///
    /// If nothing satisfies `f` return None.
    ///
    @Time(time(f) * length(s)) @Space(space(f))
    pub def findIndexOfRight(f: Char -> Bool, s: String): Option[Int] =
        let end = length(s) - 1;
        let i = findIndexOfRightHelper(f, s, end);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `findIndexOfRight`.
    ///
    def findIndexOfRightHelper(f: Char -> Bool, s: String, i: Int32): Int32 =
        if (i < 0)
            -1
        else
            if (f(charAt(i, s)))
                i
            else
                findIndexOfRightHelper(f, s, i-1)

    ///
    /// Returns the positions of the all the elements in `s` satisfying `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def findIndices(f: Char -> Bool, s: String): List[Int32] =
        findIndicesHelper(f, s, length(s), 0)

    ///
    /// Helper function for `findIndices`.
    ///
    def findIndicesHelper(f: Char -> Bool, s: String, len: Int32, i: Int32): List[Int32] =
        if (i >= len)
            Nil
        else {
            let r = findIndicesHelper(f, s, len, i+1);
            if (f(charAt(i, s))) i :: r else r
        }

    ///
    /// Returns the positions of the all the occurences of `sub` in `s`.
    ///
    /// Returns `Nil` if `sub` is the empty string.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indices(sub: String, s: String): List[Int32] =
        let sublen = length(sub);
        let step = ix ->
            match indexOfLeftWithOffset(sub, ix, s) {
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
        if (n >= length(s)) s else slice(s, 0, n)

    ///
    /// Take the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return all the characters
    /// of `s`.
    ///
    @Time(n) @Space(n)
    pub def takeRight(n: Int32, s: String): String =
        let len = length(s);
        if (n >= len) s else slice(s, len - n, len)


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
            slice(s, n, length(s))


    ///
    /// Drop the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return the empty string.
    ///
    @Time(length(s) - n) @Space(length(s) - n)
    pub def dropRight(n: Int32, s: String): String =
        if (n > 0) {
            let len = length(s);
            slice(s, 0, len - n)
        } else s

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
        match findIndexOfLeft(x ->!(f(x)), s) {
            case None => s
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
        match findIndexOfRight(x ->!(f(x)), s) {
            case None => s
            case Some(i) => slice(s, i+1, length(s))
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
        match findIndexOfLeft(x -> !(f(x)), s) {
            case None => ""
            case Some(i) => sliceRight(s, i)
        }

    ///
    /// Returns the front of string `s` after dropping all the characters
    /// from the right end that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> !(f(x)), s) {
            case None => ""
            case Some(i) => sliceLeft(s, i + 1)     // must include i
        }

    ///
    /// Returns the middle of string `s` after dropping all the characters
    /// from both ends that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def dropWhileAround(f: Char -> Bool, s: String): String =
        match (findIndexOfLeft(x -> !(f(x)), s), findIndexOfRight(x -> !(f(x)), s)) {
            case (None, None) => ""
            case (Some(l), None) => sliceRight(s, l)
            case (None, Some(r)) => sliceLeft(s, r)
            case (Some(l), Some(r)) => slice(s, l, r + 1)
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
            case _              => (sliceLeft(s, n), sliceRight(s, n))
        }


    ///
    /// Applies `f` to a start value `x` and all elements in `s` going from left to right.
    ///
    pub def foldLeft(f: (b, Char) -> b & e, x: b, s: String): b & e =
        let len = length(s);
        foldLeftHelper(f, x, len, s, 0)

    ///
    /// Helper function for `foldLeft`.
    ///
    def foldLeftHelper(f: (b, Char) -> b & e, x: b, len: Int32, s: String, i: Int32): b & e =
        if (i >= len)
            x
        else {
            let x1 = f(x, charAt(i, s));
            foldLeftHelper(f, x1, len, s, i+1)
        }

    ///
    /// Applies `f` to a start value `x and all elements in `s` going from right to left.
    ///
    pub def foldRight(f: (Char, b) -> b & e, x: b, s: String): b & e=
        let end = length(s) - 1;
        foldRightHelper(f, x, s, end)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (Char, b) -> b & e, x: b, s: String, i: Int32): b & e =
        if (i < 0)
            x
        else {
            let x1 = f(charAt(i, s), x);
            foldRightHelper(f, x1, s, i-1)
        }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from left to right.
    ///
    pub def foldLeft2(f: (b, Char, Char) -> b & e, x: b, s: String, t: String): b & e =
        let len = Int32.min(length(s), length(t));
        foldLeft2Helper(f, x, len, s, t, 0)

    ///
    /// Helper function for `foldLeft2`.
    ///
    def foldLeft2Helper(f: (b, Char, Char) -> b & e, x: b, len: Int32, s: String, t: String, i: Int32): b & e =
        if (i >= len)
            x
        else {
            let x1 = f(x, charAt(i, s), charAt(i, t));
            foldLeft2Helper(f, x1, len, s, t, i+1)
        }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from right to left.
    ///
    pub def foldRight2(f: (Char, Char, b) -> b & e, x: b, s: String, t: String): b & e=
        let sn = length(s) - 1;
        let tn = length(t) - 1;
        foldRight2Helper(f, x, s, t, sn, tn)

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (Char, Char, b) -> b & e, x: b, s: String, t: String, i: Int32, j: Int32) : b & e=
        if (i < 0 || j < 0 )
            x
        else {
            let x1 = f(charAt(i, s), charAt(j, t), x);
            foldRight2Helper(f, x1, s, t, i-1, j-1)
        }

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    pub def unfold(f: b -> Option[(Char, b)] & e, x: b) : String & e =
        let sb = StringBuilder.new() as & e;
        unfoldHelper(f, sb, x) as & e

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: b -> Option[(Char, b)] & e, sb: StringBuilder, x: b) : String & Impure =
        let a1 = f(x) as & Impure;
        match a1 {
            case None => StringBuilder.toString(sb)
            case Some(c, st1) => {
                StringBuilder.appendChar!(sb, c);
                unfoldHelper(f, sb, st1)
            }
        }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(c)` to signal a new char `c`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldWithIter(f: Unit ~> Option[Char]): String =
        let sb = StringBuilder.new() as & Pure;
        unfoldWithIterHelper(f, sb) as & Pure

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper(next: Unit ~> Option[Char], sb: StringBuilder): String & Impure = match next() {
        case None => StringBuilder.toString(sb)
        case Some(c) => {
            StringBuilder.appendChar!(sb, c);
            unfoldWithIterHelper(next, sb)
        }
    }

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    /// This is a version of `unfold` where `f` generates substrings rather than chars.
    ///
    pub def unfoldString(f: b -> Option[(String, b)] & e, x: b) : String & e =
        let sb = StringBuilder.new() as & e;
        unfoldStringHelper(f, sb, x) as & e

    ///
    /// Helper function for `unfoldString`.
    ///
    def unfoldStringHelper(f: b -> Option[(String, b)] & e, sb: StringBuilder, x: b) : String & Impure =
        let a = f(x) as & Impure;
        match a {
            case None => StringBuilder.toString(sb)
            case Some(s, st1) => {
                StringBuilder.appendString!(sb, s);
                unfoldStringHelper(f, sb, st1)
            }
        }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(s)` to signal a new substring `s`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldStringWithIter(f: Unit ~> Option[String]): String =
        let sb = StringBuilder.new() as & Pure;
        unfoldStringWithIterHelper(f, sb) as & Pure

    ///
    /// Helper function for `unfoldStringWithIter`.
    ///
    def unfoldStringWithIterHelper(next: Unit ~> Option[String], sb: StringBuilder): String & Impure = match next() {
        case None => StringBuilder.toString(sb)
        case Some(s) => {
            StringBuilder.appendString!(sb, s);
            unfoldStringWithIterHelper(next, sb)
        }
    }

    ///
    /// Returns `true` if and only if at least one char in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def exists(f: Char -> Bool, s: String): Bool = match findIndexOfLeft(f, s) {
        case None => false
        case Some(_) => true
    }

    ///
    /// Returns `true` if and only if all chars in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is empty.
    ///
    @Time(time(f) * length(s)) @Space(space(f) * length(s))
    pub def forall(f: Char -> Bool, s: String): Bool = match findIndexOfLeft(x -> !(f(x)), s) {
        case None => true
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
    pub def repeat(s: String, n: Int32): String =
        import java.lang.String.repeat(Int32);
        if (n < 0)
            ""
        else
            s.repeat(n) as & Pure

    ///
    /// Pad the string `s` at the left with the supplied char `c` to fit the width `w`.
    ///
    @Time(w) @Space(w)
    pub def padLeft(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            repeat(Char.toString(c), w - len) + s

    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit the width `w`.
    ///
    @Time(w) @Space(w)
    pub def padRight(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            s + repeat(Char.toString(c), w - len)

    ///
    /// Returns `s` with every match of the substring `target` replaced by the string `rep`.
    ///
    @Time(length(s) * length(target)) @Space(length(s) * length(target))
    pub def replace(target: String, rep: String, s: String): String =
        import java.lang.String.replace(##java.lang.CharSequence, ##java.lang.CharSequence);
        let t = target as ##java.lang.CharSequence;
        let r = rep as ##java.lang.CharSequence;
        s.replace(t, r) as & Pure

    ///
    /// Returns `s` with every match of the character `target` replaced by the character `rep`.
    ///
    @Time(length(s)) @Space(length(s))
    pub def replaceChar(target: Char, rep: Char, s: String): String =
        import java.lang.String.replace(Char, Char);
        s.replace(target, rep) as & Pure

    ///
    /// Returns `s` with every match of the regular expression `patt` replaced by the string `rep`.
    ///
    @Time(length(s) * length(rep)) @Space(length(s) * length(rep))
    pub def replaceMatches(patt: String, rep: String, s: String): String =
        import java.lang.String.replaceAll(String, String);
        s.replaceAll(patt, rep) as & Pure

    ///
    /// Returns `s` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    @Time(length(s) + length(rep)) @Space(length(s) + length(rep))
    pub def replaceFirstMatch(patt: String, rep: String, s: String): String =
        import java.lang.String.replaceFirst(String, String);
        s.replaceFirst(patt, rep) as & Pure

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
            if (ix >= i && ix < i + n) {
                let offset = ix - i;
                if (offset < 0 || offset >= sublen) charAt(ix,s) else charAt(offset, sub)
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
        if (n <= 0 || length(s) == 0)
            s
        else
            indentHelper(n, s) as & Pure

    ///
    /// Helper function for `indent`.
    ///
    def indentHelper(n: Int32, s: String): String & Impure =
        let sb = StringBuilder.new();
        let prefix = repeat(" ", n);
        let step = x -> {
            StringBuilder.appendString!(sb, prefix);
            StringBuilder.appendLine!(sb, x)
        };
        List.foreach(step, lines(s));
        StringBuilder.toString(sb)


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
        if (n <= 0 || length(s) == 0)
            s
        else
            stripIndentHelper(n, s) as & Pure

    ///
    /// Helper function for `stripIndent`.
    ///
    def stripIndentHelper(n: Int32, s: String): String & Impure =
        let sb = StringBuilder.new();
        let limit = Int32.min(n, length(s));
        let step = s1 -> {
            let line = stripIndentDropWhiteSpace(s1, limit, 0);
            StringBuilder.appendLine!(sb, line)
        };
        List.foreach(step, lines(s));
        StringBuilder.toString(sb)

    ///
    /// Helper function for `stripIndent`.
    ///
    /// Tab is counted as a single space like Java's `strip`.
    ///
    def stripIndentDropWhiteSpace(s: String, limit: Int32, ix: Int32): String =
        if (ix >= limit)
            sliceRight(s, ix)
        else
            if (!Char.isWhiteSpace(charAt(ix, s)))
                sliceRight(s, ix)
            else
                stripIndentDropWhiteSpace(s, limit, ix+1)

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
            Array.toList(split(s, "\R")) as & Pure

    ///
    /// Join the array of strings `a` separating each pair of strings and
    /// ending the result string with the system dependent line separator.
    ///
    @Time(List.length(a)) @Space(List.length(a))
    pub def unlines(a: List[String]): String = unlinesHelper(a) as & Pure

    ///
    /// Helper function for `unlines`.
    ///
    def unlinesHelper(a: List[String]): String & Impure =
        let sb = StringBuilder.new();
        List.foreach(StringBuilder.appendLine!(sb), a);
        StringBuilder.toString(sb)

    ///
    /// Split the string `s` into an list of words, dividing on one or more white space characters.
    /// Leading and trailing spaces are trimmed.
    ///
    @Time(length(s)) @Space(length(s))
    pub def words(s: String): List[String] = match trim(s) {
        case s1 if isEmpty(s1) => Nil
        case s1 => Array.toList(split(s1, "\s+")) as & Pure
    }

    ///
    /// Join the array of strings `a` separating each pair of strings with a
    /// single space character.
    ///
    @Time(List.length(a)) @Space(List.length(a))
    pub def unwords(a: List[String]): String = unwordsHelper(a) as & Pure

    ///
    /// Helper function for `unlines`.
    ///
    def unwordsHelper(xs: List[String]): String & Impure =
        let sb = StringBuilder.new();
        let first = ref true;
        let step = s ->
            if (deref first == true) {
                first := false;
                StringBuilder.appendString!(sb, s)
            } else {
                StringBuilder.appendChar!(sb, ' ');
                StringBuilder.appendString!(sb, s)
            };
        List.foreach(step, xs);
        StringBuilder.toString(sb)

    ///
    /// Alias for `indexOfLeft`.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indexOf(sub: String, s: String): Option[Int32] = indexOfLeft(sub, s)

    ///
    /// Return the index of the first occurence of `sub` in `s` from the left.
    ///
    /// If `sub` is not present in `s` return None.
    ///
    /// If `sub` is the empty string return None.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indexOfLeft(sub: String, s: String): Option[Int32] =
        import java.lang.String.indexOf(String);
        if (isEmpty(sub))
            None
        else {
            let i = s.indexOf(sub) as & Pure;
            if (i < 0) None else Some(i)
        }

    ///
    /// Return the index of the first occurence of `sub` in `s` from the right.
    ///
    /// If `sub` is not present in `s` return None.
    ///
    /// If `sub` is the empty string return None.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indexOfRight(sub: String, s: String): Option[Int32] =
        import java.lang.String.lastIndexOf(String);
        if (isEmpty(sub))
            None
        else {
            let i = s.lastIndexOf(sub) as & Pure;
            if (i < 0) None else Some(i)
        }

    ///
    /// Returns `true` if and only if `sub` is an infix of `s`.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def contains(s: String, sub: String): Bool =
        if (isEmpty(sub))
            true
        else {
            let a = indexOfLeft(sub, s);
            !Option.isEmpty(a)
        }

    ///
    /// Return the common prefix of strings `s` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common prefix.
    ///
    @Time(Int32.min(length(s1), length(s2))) @Space(Int32.min(length(s1), length(s2)))
    pub def commonPrefix(s1: String, s2: String): String =
        commonPrefixHelper(s1, s2, length(s1), length(s2), 0)

    ///
    /// Helper function for `commonPrefix`.
    ///
    def commonPrefixHelper(s1: String, s2: String, len1: Int32, len2: Int32, ix: Int32): String =
        if (ix >= len1 || ix >= len2)
            takeLeft(ix, s1)
        else {
            let c1 = charAt(ix, s1);
            let c2 = charAt(ix, s2);
            if (c1 != c2)
                takeLeft(ix, s1)
            else
                commonPrefixHelper(s1, s2, len1, len2, ix+1)
        }

    ///
    /// Return the common suffix of strings `s` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common suffix.
    ///
    @Time(Int32.min(length(s1), length(s2))) @Space(Int32.min(length(s1), length(s2)))
    pub def commonSuffix(s1: String, s2: String): String =
        let start1 = length(s1) - 1;
        let start2 = length(s2) - 1;
        commonSuffixHelper(s1, s2, start1, start2, 0)

    ///
    /// Helper function for `commonSuffix`.
    ///
    def commonSuffixHelper(s1: String, s2: String, ix1: Int32, ix2: Int32, ac: Int32): String =
        if (ix1 < 0)
            s1
        else if (ix2 < 0)
            s2
        else {
            let c1 = charAt(ix1, s1);
            let c2 = charAt(ix2, s2);
            if (c1 != c2)
                takeRight(ac, s1)
            else
                commonSuffixHelper(s1, s2, ix1-1, ix2-1, ac+1)
       }

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
                "..." + slice(s, start, len)
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
            case _              => slice(s, 0, w - 3) + "..."
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
        let v0 = Array.new(0, n+1);
        let v1 = Array.new(0, n+1);
        forIndex(0, n, i -> v0[i] = i );
        forIndex(0, m-1, i -> {
            v1[0] = i + 1;
            forIndex(0, n-1, j -> {
                let substitutionCost = if (charAt(i, s) == charAt(j, t)) 0 else 1;
                let a = v1[j] + 1;
                let b = v0[j + 1] + 1;
                let c = v0[j] + substitutionCost;
                v1[j+1] = Int32.min(a, Int32.min(b, c))
            });
            arraySwap(v0,v1)
        });
        v0[n]

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    /// This implements a forloop, the end point `e` is inclusive.
    ///
    def forIndex(b: Int32, e: Int32, f: Int32 ~> Unit): Unit & Impure =
        if (b <= e) { f(b); forIndex(b+1, e, f) } else ()

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    def arraySwap(a: Array[Int32], b : Array[Int32]): Unit & Impure =
        forIndex(0, Array.length(a) - 1, {i ->  a[i] = b[i]})

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(Int32.min(length(a), length(b))) @Space(Int32.min(length(a), length(b)))
    pub def zip(a: String, b: String): Array[(Char, Char)] & Impure =
        let len = Int32.min(length(a), length(b));
        Array.init(i -> (charAt(i, a), charAt(i, b)), len)

    ///
    /// Returns a string where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(Int32.min(length(a), length(b))) @Space(Int32.min(length(a), length(b)))
    pub def zipWith(f: (Char, Char) -> Char & e, a: String, b: String): String & e =
        let len = Int32.min(length(a), length(b));
        init(i -> f(charAt(i, a), charAt(i, b)), len)

    ///
    /// Split the string `s` on matches of `sub`.
    ///
    @Time(length(s) + length(sub)) @Space(length(s) + length(sub))
    pub def splitOn(s: String, sub: String): List[String] =
        let maxlen = length(s);
        let sublen = length(sub);
        let step = ix ->
            // Note - to stop we have to be greater than maxlen.
            // We want to return a final empty string if `s` terminates with `sub`.
            if (ix > maxlen)
                None
            else {
                match indexOfLeftWithOffset(sub, ix, s) {
                    case None => {
                        let s1 = sliceRight(s, ix);
                        Some(s1, maxlen + 1)
                    }
                    case Some(ix2) => {
                        let s1 = slice(s, ix, ix2);
                        Some(s1, ix2 + sublen)
                    }
                }
            };
        List.unfold(step, 0)


    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `sub` is the empty string.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indexOfLeftWithOffset(sub: String, offset: Int32, s: String): Option[Int32] =
        import java.lang.String.indexOf(String, Int32);
        if (isEmpty(sub))
            None
        else {
            let ix = s.indexOf(sub, offset) as & Pure;
            if (ix < 0) None else Some(ix)
        }

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `sub` is the empty string.
    ///
    @Time(length(sub) * length(s)) @Space(length(s))
    pub def indexOfRightWithOffset(sub: String, offset: Int32, s: String): Option[Int32] =
        import java.lang.String.lastIndexOf(String, Int32);
        if (isEmpty(sub))
            None
        else {
            let ix = s.lastIndexOf(sub, offset) as & Pure;
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
            case i if i + size >= len   => Some(sliceRight(s, i), len)
            case i                      => Some(slice(s, i, i + size), i + size)
        };
        List.unfold(step, 0)

    ///
    /// Find the first instance of `sub` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    @Time(length(sub) * length(s)) @Space(length(sub) * length(s))
    pub def breakOnLeft(sub: String, s: String): (String, String) =
        match indexOfLeft(sub, s) {
            case None => (s, "")
            case Some(i) => (sliceLeft(s, i), sliceRight(s, i))
        }

    ///
    /// Find the last instance of `sub` in string `s`, return a pair of the
    /// initial string including `sub` and suffix from `sub`.
    ///
    @Time(length(sub) * length(s)) @Space(length(sub) * length(s))
    pub def breakOnRight(sub: String, s: String): (String, String) =
        let sublen = length(sub);
        match indexOfRight(sub, s) {
            case None => (s, "")
            case Some(i) => (sliceLeft(s, i + sublen), sliceRight(s, i + sublen))
        }

    ///
    /// Count the occurences of `sub` in string `s`.
    ///
    @Time(length(sub) * length(s)) @Space(length(sub) * length(s))
    pub def countSubstring(sub: String, s: String): Int32 =
        let sublen = length(sub);
        if (sublen <= 0)
            0
        else
            countSubstringHelper(sub, sublen, s, 0, 0)

    ///
    /// Helper function for `count``.
    ///
    def countSubstringHelper(sub: String, sublen: Int32, s: String, offset: Int32, count: Int32): Int32 =
        match indexOfLeftWithOffset(sub, offset, s) {
            case None => count
            case Some(i) => countSubstringHelper(sub, sublen, s, i + sublen, count + 1)
        }

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `sub`.
    ///
    @Time(length(s) - length(sub)) @Space(length(s) - length(sub))
    pub def stripPrefix(sub: String, s: String): Option[String] =
        if (!startsWith(s, sub))
            None
        else
            Some(dropLeft(length(sub), s))

    ///
    /// Returns `Some(prefix)` of string `s` if its suffix matches `sub`.
    ///
    @Time(length(s) - length(sub)) @Space(length(s) - length(sub))
    pub def stripSuffix(sub: String, s: String): Option[String] =
        if (!endsWith(s, sub))
            None
        else
            Some(dropRight(length(sub), s))

    /////////////////////////////////////////////////////////////////////////////
    // Lints                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Simplifications.
    ///

    @lint
    def leftConcatenateEmptyString(): Bool =
        forall (s: String). ("" + s) === s

    @lint
    def rightConcatenateEmptyString(): Bool =
        forall (s: String). (s + "") === s

    @lint
    def trimTrim(): Bool =
        forall (s: String). trim(trim(s)) === trim(s)

}
