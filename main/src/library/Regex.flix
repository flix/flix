/*
 *  Copyright 2023 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */


///
/// Basic support for regular expression matching on Strings.
///
/// This module uses the builtin Regex datatype representing a compiled regular expression and
/// alternative implementations of functions from `String.flix` using regular expressions.
///
/// Note - for regex matching all scanning is left-to-right and "chunked" by the matcher.
/// This differs from String.flix where scanning is character at a time and can be left-to-right
/// or right-to-left. Hence this module provides analogue but not exactly equivalent of functions:
/// e.g.
/// > String.indexOfLeft ~> Regex/Text.indexOfFirst
/// > String.indexOfRight ~> Regex/Text.indexOfLast
/// > String.breakOnLeft ~> Regex/Text.breakOnFirst
/// > String.breakOnRight ~> Regex/Text.breakOnLast
///

pub mod Regex {

    use Regex.Flag.{CanonEq, CaseInsenstive,
        Comments, Dotall, Literal, Multiline,
        UnicodeCase, UnicodeCharacterClass, UnixLines
    }

    ///
    /// Represents a flag controlling the compilation of regular expression.
    ///
    pub enum Flag with Eq, Order, ToString {
        case CanonEq
        case CaseInsenstive
        case Comments
        case Dotall
        case Literal
        case Multiline
        case UnicodeCase
        case UnicodeCharacterClass
        case UnixLines
    }

    ///
    /// Returns the int value of `flag`.
    ///
    /// Regex flags are represented as an `Int32` bitmask.
    ///
    def toInt(flag: Flag): Int32 =
        match flag {
            case CanonEq                => %%REGEX_FLAG_CANON_EQ%%()
            case CaseInsenstive         => %%REGEX_FLAG_CASE_INSENSITIVE%%()
            case Comments               => %%REGEX_FLAG_COMMENTS%%()
            case Dotall                 => %%REGEX_FLAG_DOTALL%%()
            case Literal                => %%REGEX_FLAG_LITERAL%%()
            case Multiline              => %%REGEX_FLAG_MULTILINE%%()
            case UnicodeCase            => %%REGEX_FLAG_UNICODE_CASE%%()
            case UnicodeCharacterClass  => %%REGEX_FLAG_UNICODE_CHARACTER_CLASS%%()
            case UnixLines              => %%REGEX_FLAG_UNIX_LINES%%()
        }

    ///
    /// Sum a set of flags to an `Int32` bit mask.
    ///
    pub def sumFlags(flags: Set[Flag]): Int32 =
        Set.foldLeft((ac,e) -> ac + toInt(e), 0, flags)

    ///
    /// Compiles the regular expression `pattern`.
    ///
    /// Throws an exception if the pattern is ill-formed.
    ///
    pub def compile(pattern: String): Regex =
        %%REGEX_COMPILE%%(pattern)

    ///
    /// Compiles the regular expression `pattern` with the given `flags`.
    ///
    /// Throws an exception if the pattern is ill-formed.
    ///
    pub def compileWithFlags(flags: Set[Flag], pattern: String): Regex =
        %%REGEX_COMPILE_WITH_FLAGS%%((sumFlags(flags), pattern))

    ///
    /// Tries to compile the regular expression `pattern`.
    ///
    /// Returns `Err(e)` if compilation fails where `e` is the error message.
    ///
    pub def tryCompile(pattern: String): Result[String, Regex] =
        let (ok, rgx, msg) = %%REGEX_TRY_COMPILE%%(pattern);
        if (ok) Ok(rgx) else Err(msg)

    ///
    /// Tries to compile the regular expression `pattern` with the given `flags`.
    ///
    /// Returns `Err(e)` if compilation fails where `e` is the error message.
    ///
    pub def tryCompileWithFlags(flags: Set[Flag], pattern: String): Result[String, Regex] =
        let (ok, rgx, msg) = %%REGEX_TRY_COMPILE_WITH_FLAGS%%((sumFlags(flags), pattern));
        if (ok) Ok(rgx) else Err(msg)

    ///
    /// Decompose an Int32 representing bit masks into a list of flags.
    ///
    def listFlags(x: Int32): List[Flag] =
        let check = y -> {
            let y1 = toInt(y);
            Int32.bitwiseAnd(x, y1) == y1
        };
        List.filter(check, CanonEq :: CaseInsenstive :: Comments :: Dotall :: Literal ::
                            Multiline :: UnicodeCase :: UnicodeCharacterClass :: UnixLines :: Nil)

    ///
    /// Return the unmatchable Regex - a regular expression that will not match any input.
    ///
    pub def unmatchable(): Regex =
        compile("^\\b$")

    ///
    /// Return the regular expression string that matches the literal string `s`.
    ///
    pub def quote(s: String): String =
        %%REGEX_QUOTE%%(s)

    ///
    /// Return the regular expression string used to build this Regex.
    ///
    pub def pattern(rgx: Regex): String =
        %%REGEX_PATTERN%%(rgx)

    ///
    /// Return the flags used to build the Regex `rgx`.
    ///
    pub def flags(rgx: Regex): Set[Flag] =
        let i = %%REGEX_FLAGS%%(rgx);
        listFlags(i) |> List.toSet


    ///
    /// Returns `true` if the entire string `s` is matched by the Regex `rgx`.
    ///
    pub def isMatch(rgx: Regex, s: String): Bool = region rc {
        let m = newMatcher(rc, rgx, s);
        matches(m)
    }

    ///
    /// Returns `true` if the string `input` is matched by the Regex `rgx`
    /// at any position within the string `s`.
    ///
    pub def isSubmatch(rgx: Regex, s: String): Bool = region rc {
        newMatcher(rc, rgx, s) |> find
    }

    ///
    /// Returns the positions of the all the occurrences of `substr` in `s`.
    ///
    /// If `substr` regexp matches the empty string, positions where an empty match
    /// has been recognized will be returned.
    ///
    pub def indicesOf(substr: Regex, s: String): Vector[Int32] = region rc {
        let m = newMatcher(rc, substr, s);
        foldSubmatches(Chain.snoc, Chain.empty(), matcherStart, m)
            |> Chain.toList
            |> List.toVector
    }

    ///
    /// Returns the contents of the all the occurrences of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string.
    ///
    pub def submatches(substr: Regex, s: String): List[String] = region rc {
        let m = newMatcher(rc, substr, s);
        foldSubmatches(Chain.snoc, Chain.empty(), matcherContent, m)
            |> Chain.toList
    }

    ///
    /// Returns the contents of the all the capture groups of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string or regex contains no groups.
    ///
    pub def capture(rgx: Regex, s: String): List[String] = region rc {
        let m = newMatcher(rc, rgx, s);
        if (find(m)) {
            let cnt = groupCount(m);
            List.map(i -> matcherGroup(m, i), List.range(1, cnt + 1))
        } else {
            Nil
        }
    }

    ///
    /// Count the occurrences of `substr` in string `s`.
    ///
    pub def countSubmatches(substr: Regex, s: String): Int32 = region rc {
        let m = newMatcher(rc, substr, s);
        foldSubmatches((acc, _) -> acc + 1, 0, matcherStart, m)
    }


    ///
    /// Splits the string `s` around matches of the Regex `rgx`.
    ///
    pub def split(rgx: Regex, s: String): List[String] =
        region rc { %%REGEX_SPLIT%%((rc, rgx, s)) |> Array.toList }

    ///
    /// Returns string `s` with every match of the Regex `src` replaced by the string `dst`.
    ///
    pub def replace(src: {src = Regex}, dst: {dst = String}, s: String): String = region rc {
        let m = newMatcher(rc, src#src, s);
        replaceAll(dst#dst, m)
    }

    ///
    /// Returns string `s` with the first match of the regular expression `src` replaced by the string `dst`.
    ///
    pub def replaceFirst(src: {src = Regex}, dst: {dst = String}, s: String): String = region rc {
        let m = newMatcher(rc, src#src, s);
        replaceFirst0(dst#dst, m)
    }

    ///
    /// Returns `true` if the string `s` starts the Regex `prefix`.
    ///
    pub def startsWith(prefix: Regex, s: String): Bool = region rc {
        let m = newMatcher(rc, prefix, s);
        lookingAt(m)
    }

    ///
    /// Returns `true` if the string `input` ends the regular expression Regex `suffix`.
    ///
    /// This will be slower than `startsWith` because there is no primitive function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(suffix: Regex, s: String): Bool = region rc {
        let m = newMatcher(rc, suffix, s);
        match lastSubmatch(matcherRange, m) {
            case None       => false
            case Some(rng)  => String.length(s) == rng#end
        }
    }

    ///
    /// Returns `Some(prefix)` of string `s` if its prefix matches `substr`.
    ///
    pub def getPrefix(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        match firstSubmatch(matcherRange, m) {
            case None     => None
            case Some(r)  => if (r#start == 0) Some(String.takeLeft(r#end, s)) else None
        }
    }

    ///
    /// Returns `Some(suffix)` of string `s` if its suffix matches `substr`.
    ///
    pub def getSuffix(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        match lastSubmatch(matcherRange, m) {
            case None     => None
            case Some(r)  => if (r#end == String.length(s)) Some(String.dropLeft(r#start, s)) else None
        }
    }

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `substr`.
    ///
    pub def stripPrefix(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        match firstSubmatch(matcherRange, m) {
            case None     => None
            case Some(r)  => if (r#start == 0) Some(String.dropLeft(r#end, s)) else None
        }
    }

    ///
    /// Returns `Some(prefix)` of string `s` if its suffix matches `substr`.
    ///
    pub def stripSuffix(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        match lastSubmatch(matcherRange, m) {
            case None     => None
            case Some(r)  => if (r#end == String.length(s)) Some(String.takeLeft(r#start, s)) else None
        }
    }

    ///
    /// Return the index of the first occurence of `substr` in `s` from the left.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def indexOfFirst(substr: Regex, s: String): Option[Int32] = region rc {
        let m = newMatcher(rc, substr, s);
        firstSubmatch(matcherStart, m)
    }

    ///
    /// Return the index of the last occurence of `substr` in `s` starting from the left.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def indexOfLast(substr: Regex, s: String): Option[Int32] = region rc {
        let m = newMatcher(rc, substr, s);
        lastSubmatch(matcherStart, m)
    }

    ///
    /// Return the content of the first occurence of `substr` in `s` from the left.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def getFirst(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        firstSubmatch(matcherContent, m)
    }

    ///
    /// Return the content of the last occurence of `substr` in `s` from the left.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def getLast(substr: Regex, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        lastSubmatch(matcherContent, m)
    }

    ///
    /// This is `indexOfFirst` with a start offset.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def indexOfFirstWithOffset(substr: Regex, offset: Int32, s: String): Option[Int32] = region rc {
        let m = newMatcher(rc, substr, s);
        if (setBounds(start = offset, end = String.length(s), m))
            firstSubmatch(matcherStart, m)
        else
            None
    }

    ///
    /// This is `indexOfLast` with a start offset.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def indexOfLastWithOffset(substr: Regex, offset: Int32, s: String): Option[Int32] = region rc {
        let m = newMatcher(rc, substr, s);
        if (setBounds(start = offset, end = String.length(s), m))
            lastSubmatch(matcherStart, m)
        else
            None
    }

    ///
    /// This is `getFirst` with a start offset.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def getFirstWithOffset(substr: Regex, offset: Int32, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        if (setBounds(start = offset, end = String.length(s), m))
            firstSubmatch(matcherContent, m)
        else
            None
    }

    ///
    /// This is `getLast` with a start offset.
    ///
    /// If the Regex `substr` is not present in `s` return None.
    ///
    pub def getLastWithOffset(substr: Regex, offset: Int32, s: String): Option[String] = region rc {
        let m = newMatcher(rc, substr, s);
        if (setBounds(start = offset, end = String.length(s), m))
            lastSubmatch(matcherContent, m)
        else
            None
    }

    ///
    /// Find the first instance of Regex `substr` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnFirst(substr: Regex, s: String): (String, String) = region rc {
        let m = newMatcher(rc, substr, s);
        match firstSubmatch(matcherRange, m) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(end = r#start, s), String.sliceRight(start = r#start, s))
        }
    }

    ///
    /// Find the first instance of Regex `substr` in string `s`, return a pair of the
    /// prefix of string `s` up to and including `sub` and the rest of string `s` after `sub`.
    ///
    pub def breakAfterFirst(substr: Regex, s: String): (String, String) = region rc {
        let m = newMatcher(rc, substr, s);
        match firstSubmatch(matcherRange, m) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(end = r#end, s), String.sliceRight(start = r#end, s))
        }
    }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnLast(substr: Regex, s: String): (String, String) = region rc {
        let m = newMatcher(rc, substr, s);
        match lastSubmatch(matcherRange, m) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(end = r#end, s), String.sliceRight(start = r#end, s))
        }
    }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakBeforeLast(substr: Regex, s: String): (String, String) = region rc {
        let m = newMatcher(rc, substr, s);
        match lastSubmatch(matcherRange, m) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(end = r#start, s), String.sliceRight(start = r#start, s))
        }
    }

    ///
    /// Returns `true` if the entire string `s` is matched by the Regex `rgx`.
    ///
    /// Returns `false` if the entire string does not match or the bounds are invalid.
    ///
    pub def isMatchWithBounds(rgx: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Bool = region rc {
        match newMatcherWithBounds(rc, rgx, start, end, s) {
            case Some(m) => matches(m)
            case None    => false
        }
    }

    ///
    /// Returns `true` if the string `input` is matched by the Regex `rgx`
    /// at any position within the string `s` that is within the bounds `(start, end)`.
    ///
    /// Returns `false` if there is no match or the bounds are invalid.
    ///
    pub def isSubmatchWithBounds(rgx: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Bool = region rc {
        match newMatcherWithBounds(rc, rgx, start, end, s) {
            case Some(m) => find(m)
            case None    => false
        }
    }

    ///
    /// Returns the positions of the all the occurrences of `substr` in `s`
    /// within the bounds `(start, end)`.
    ///
    /// If `substr` regexp matches the empty string, positions where an empty match
    /// has been recognized will be returned.
    ///
    /// Returns an empty vector if there are no matches or the bounds are invalid.
    ///
    pub def indicesOfWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Vector[Int32] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) =>
                foldSubmatches(Chain.snoc, Chain.empty(), matcherStart, m)
                    |> Chain.toList
                    |> List.toVector
            case None => Vector.empty()
        }
    }

    ///
    /// Returns the contents of the all the occurrences of `substr` in `s`
    /// within the bounds `(start, end)`.
    ///
    /// Returns `Nil` if `substr` is the empty string or the bounds are invalid.
    ///
    pub def submatchesWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): List[String] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) =>
                foldSubmatches(Chain.snoc, Chain.empty(), matcherContent, m) |> Chain.toList
            case None =>
                Nil
        }
    }

    ///
    /// Count the occurrences of `substr` in string `s` within the bounds `(start, end)`.
    ///
    /// Returns 0 if the bounds are invalid.
    ///
    pub def countSubmatchesWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Int32 = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) => foldSubmatches((acc, _) -> acc + 1, 0, matcherStart, m)
            case None    => 0
        }
    }

    ///
    /// Return the index of the first occurence of `substr` in `s` from the left within
    /// the bounds `(start, end)`.
    ///
    /// If the Regex `substr` is not present in `s` or the bounds are invalid return `None`.
    ///
    pub def indexOfFirstWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Option[Int32] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) => firstSubmatch(matcherStart, m)
            case None    => None
        }
    }

    ///
    /// Return the index of the last occurence of `substr` in `s` from the left within
    /// the bounds `(start, end)`.
    ///
    /// If the Regex `substr` is not present in `s` or the bounds are invalid return `None`.
    ///
    pub def indexOfLastWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Option[Int32] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) => lastSubmatch(matcherStart, m)
            case None    => None
        }
    }

    ///
    /// Return the content of the first occurence of `substr` in `s` from the left within
    /// the bounds `(start, end)`.
    ///
    /// If the Regex `substr` is not present in `s` or the bounds are invalid return `None`.
    ///
    pub def getFirstWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Option[String] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) => firstSubmatch(matcherContent, m)
            case None    => None
        }
    }

    ///
    /// Return the content of the last occurence of `substr` in `s` from the left within
    /// the bounds `(start, end)`.
    ///
    /// If the Regex `substr` is not present in `s` or the bounds are invalid return `None`.
    ///
    pub def getLastWithBounds(substr: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Option[String] = region rc {
        match newMatcherWithBounds(rc, substr, start, end, s) {
            case Some(m) => lastSubmatch(matcherContent, m)
            case None    => None
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Internal support for matching using a RegexMatcher handle             //
    ///////////////////////////////////////////////////////////////////////////

    ///
    /// Matcher is imperative - it can be seen as a stepper through a stream
    /// of matches. The matcher is updated with the function find to move to
    /// the next match.
    ///
    type alias Matcher[r: Region] = {
        rc = Region[r],
        handle = Ref[RegexMatcher, r],
        inputLen = Int32
    }

    ///
    /// Create a Matcher for Regex `rgx` on the source String `input`.
    ///
    def newMatcher(rc: Region[r], rgx: Regex, s: String): Matcher[r] \ r =
        { rc = rc, handle = Ref.fresh(rc, %%REGEX_NEW_MATCHER%%((rc, rgx, s))), inputLen = String.length(s) }

    ///
    /// Create a Matcher for Regex `rgx` on the source String `input` with bounds `start` and `end`.
    ///
    /// Returns `Some(matcher)` if the bounds are valid for the input String `s` otherwise returns `None`.
    ///
    def newMatcherWithBounds(rc: Region[r], rgx: Regex, start: {start = Int32}, end: {end = Int32}, s: String): Option[Matcher[r]] \ r =
        let m = newMatcher(rc, rgx, s);
        if (setBounds(start, end, m)) Some(m) else None

    ///
    /// Attempt to find the next match. Returns `true` and moves to the next match
    /// if there is a next match otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    def find(m: Matcher[r]): Bool \ r =
        %%REGEX_MATCHER_FIND%%((m#rc, Ref.get(m#handle)))

    ///
    /// Attempt to find the next match after the supplied position `pos`.
    /// Returns `true` and moves to the next match if there is a next match
    /// otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    def findFrom(pos: Int32, m: Matcher[r]): Bool \ r =
        if (pos < 0 or pos > m#inputLen) false
        else %%REGEX_MATCHER_FIND_FROM%%((m#rc, Ref.get(m#handle), pos))

    ///
    /// Attempt to find the last match by iterating through the input string.
    ///
    /// Returns `true` and moves the matcher to the start of the last match if there are
    /// one-or-more matches otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    /// Note - there are no primitive functions for right-to-left scanning, so the
    /// performance of `findLast` is disadvantaged compared to `find`.
    ///
    def findLast(m: Matcher[r]): Bool \ r =
        def loop(lastPos) = match find(m) {
            case true  => loop(matcherStart(m))
            case false => lastPos
        };
        match find(m) {
            case false => false
            case true  =>
                let startOfLast = loop(matcherStart(m));
                findFrom(startOfLast, m)
        }

    ///
    /// Set the bounds of the matcher's region.
    ///
    def setBounds(start: {start = Int32}, end: {end = Int32}, m: Matcher[r]): Bool \ r =
        if (start#start < 0 or end#end < start#start or end#end > m#inputLen) false else {
            %%REGEX_MATCHER_SET_BOUNDS%%((m#rc, Ref.get(m#handle), start#start, end#end));
            true
        }

    ///
    /// Return the start position of the current match of Matcher `m`.
    ///
    def matcherStart(m: Matcher[r]): Int32 \ r =
        %%REGEX_MATCHER_START%%((m#rc, Ref.get(m#handle)))

    ///
    /// Return the end position of the current match of Matcher `m`.
    ///
    def matcherEnd(m: Matcher[r]): Int32 \ r =
        %%REGEX_MATCHER_END%%((m#rc, Ref.get(m#handle)))

    ///
    /// Return the start and end positions of the matcher `m`.
    ///
    def matcherRange(m: Matcher[r]): {start = Int32, end = Int32} \ r =
        {start = matcherStart(m), end = matcherEnd(m)}

    ///
    /// Return the text content of the current match of matcher `m`.
    ///
    def matcherContent(m: Matcher[r]): String \ r =
        %%REGEX_MATCHER_GROUP%%((m#rc, Ref.get(m#handle), 0))

    ///
    /// Return the text content of the current groups in matcher `m`.
    ///
    def matcherGroup(m: Matcher[r], i:Int32): String \ r =
        let s = %%REGEX_MATCHER_GROUP%%((m#rc, Ref.get(m#handle), i));
        if (not Object.isNull(s)) s else ""

    ///
    /// Return the number of groups in matcher `m`.
    ///
    def groupCount(m: Matcher[r]): Int32 \ r =
            %%REGEX_MATCHER_GROUP_COUNT%%((m#rc, Ref.get(m#handle)))

    ///
    /// `MatchQuery` is a read query applied to the current match of a `Matcher`.
    ///
    pub type alias MatchQuery[a: Type, r: Region] = Matcher[r] -> a \ r

    ///
    /// Returns the first result found by applying the query `asks` to `m` going from left to right.
    ///
    /// Returns `None` if no match is found.
    ///
    def firstSubmatch(asks: MatchQuery[a, r], m: Matcher[r]): Option[a] \ r =
        if (find(m)) Some(asks(m)) else None


    ///
    /// Returns the last result found by applying the query `asks` to `m` going from left to right.
    ///
    /// Returns `None` if no match is found.
    ///
    def lastSubmatch(asks: MatchQuery[a, r], m: Matcher[r]): Option[a] \ r =
        if (findLast(m)) Some(asks(m)) else None

    ///
    /// Fold on all the submatches going from left to right.
    ///
    /// `f` is applied to the result of applying the query `asks` and the folds accumulating value.
    /// The initial accumulating value is `x`.
    ///
    def foldSubmatches(f: (b, a) -> b \ ef, x: b, asks: MatchQuery[a, r], m: Matcher[r]): b \ { r, ef } =
        def loop(acc) = match find(m) {
            case true  => loop(f(acc, asks(m)))
            case false => acc
        };
        loop(x)

    ///
    /// Returns true if the entire input matches.
    ///
    def matches(m: Matcher[r]): Bool \ r =
        %%REGEX_MATCHER_MATCHES%%((m#rc, Ref.get(m#handle)))

    ///
    /// Returns true if the input matches at the beginning (lookingAt).
    ///
    def lookingAt(m: Matcher[r]): Bool \ r =
        %%REGEX_MATCHER_LOOKING_AT%%((m#rc, Ref.get(m#handle)))

    ///
    /// Replaces all matches with `replacement`.
    ///
    def replaceAll(replacement: String, m: Matcher[r]): String \ r =
        %%REGEX_MATCHER_REPLACE_ALL%%((m#rc, Ref.get(m#handle), replacement))

    ///
    /// Replaces the first match with `replacement`.
    ///
    def replaceFirst0(replacement: String, m: Matcher[r]): String \ r =
        %%REGEX_MATCHER_REPLACE_FIRST%%((m#rc, Ref.get(m#handle), replacement))

}
