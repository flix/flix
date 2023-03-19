/*
 * Copyright 2023 Stephen Tetley
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
/// Alternative implementations of functions from `String.flix` that use regular expressions
///

namespace Regex/Text {

    use Regex.Pattern
    use Regex.Pattern.{Pattern}
    use Regex/Internal.Matcher
    use Regex/Internal.Matcher.{Matcher}


    ///
    /// Returns `true` if the entire string `s` is matched by the pattern `patt`.
    ///
    pub def isMatch(patt: Pattern, s: String): Bool = region rc {
        import java.util.regex.Matcher.matches(): Bool \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, patt, s);
        matches(m1)
    }

    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `p`
    /// at any position within the string `s`.
    ///
    pub def isSubmatch(patt: Pattern, s: String): Bool = region rc {
        Regex/Internal/Matcher.new(rc, patt, s) |> Regex/Internal/Matcher.find!
    }

    ///
    /// Returns the positions of the all the occurrences of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string.
    ///
    pub def indices(substr: {substr = Pattern}, s: String): List[Int32] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.foldSubmatches(Chain.snoc, Chain.empty(), Regex/Internal/Matcher.start, m) {
            case Ok(c)  => Chain.toList(c)
            case Err(_) => Nil
        }
    }

    ///
    /// Returns the contents of the all the occurrences of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string.
    ///
    pub def submatches(substr: {substr = Pattern}, s: String): List[String] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.foldSubmatches(Chain.snoc, Chain.empty(), Regex/Internal/Matcher.content, m) {
            case Ok(c)  => Chain.toList(c)
            case Err(_) => Nil
        }
    }

    ///
    ///
    /// Count the occurrences of `substr` in string `s`.
    ///
    pub def countSubmatches(substr: {substr = Pattern}, s: String): Int32 = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.foldSubmatches((acc, _) -> acc + 1, 0, Regex/Internal/Matcher.start, m) {
            case Ok(n)  => n
            case Err(_) => 0
        }
    }

    ///
    /// Splits the string `s` around matches of the pattern `pattern`.
    ///
    pub def split(pattern: {pattern = Pattern}, s: String): List[String] = region rc {
        import java.util.regex.Pattern.split(##java.lang.CharSequence): Array[String, rc] \ Write(rc);
        let _ = rc; // This avoids a redundancy error.
        let Pattern(p1) = pattern.pattern;
        Array.toList(split(p1, unchecked_cast(s as ##java.lang.CharSequence)))
    }


    ///
    /// Returns string `s` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceAll(String): String \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, from.from, s);
        replaceAll(m1, to.to)
    }

    ///
    /// Returns string `s` with the first match of the regular expression `from` replaced by the string `to`.
    ///
    pub def replaceFirst(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceFirst(String): String \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, from.from, s);
        replaceFirst(m1, to.to)
    }

    ///
    /// Returns `true` if the string `s` starts the pattern `prefix`.
    ///
    pub def startsWith(prefix: {prefix = Pattern}, s: String): Bool = region rc {
        import java.util.regex.Matcher.lookingAt(): Bool \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, prefix.prefix, s);
        lookingAt(m1)
    }

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// This will be slower than `startsWith` because there is no primitive Java function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(suffix: {suffix = Pattern}, s: String): Bool = region rc {
        let m = Regex/Internal/Matcher.new(rc, suffix.suffix, s);
        match Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_)  => false
            case Ok(rng) => String.length(s) == rng.end
        }
    }

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `substr`.
    ///
    pub def stripPrefix(substr: {substr = Pattern}, s: String): Option[String] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.firstSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => None
            case Ok(r)  => if (r.start == 0) Some(String.dropLeft(r.end, s)) else None
        }
    }

    ///
    /// Returns `Some(prefix)` of string `s` if its suffix matches `substr`.
    ///
    pub def stripSuffix(substr: {substr = Pattern}, s: String): Option[String] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => None
            case Ok(r)  => if (r.end == String.length(s)) Some(String.takeLeft(r.start, s)) else None
        }
    }

    ///
    /// Return the index of the first occurence of `patt` in `s` from the left.
    ///
    /// If `patt` is not present in `s` return None.
    ///
    /// If `patt` is the empty string return None.
    ///
    pub def indexOfLeft(patt: Pattern, s: String): Option[Int32] = region rc {
        let m = Regex/Internal/Matcher.new(rc, patt, s);
        Regex/Internal/Matcher.firstSubmatch(Regex/Internal/Matcher.start, m) |> Result.toOption
    }

    ///
    /// Find index of rightmost match.
    ///
    pub def indexOfRight(patt: Pattern, s: String): Option[Int32] = region rc {
        let m = Regex/Internal/Matcher.new(rc, patt, s);
        Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.start, m) |> Result.toOption
    }

    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfLeftWithOffset(substr: {substr = Pattern}, offset: {offset = Int32}, s: String): Option[Int32] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.setBounds!(start = offset.offset, end = String.length(s), m) {
            case Ok()   => Regex/Internal/Matcher.firstSubmatch(Regex/Internal/Matcher.start, m) |> Result.toOption
            case Err(_) => None
        }
    }

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfRightWithOffset(substr: {substr = Pattern}, offset: {offset = Int32}, s: String): Option[Int32] = region rc {
        let m = Regex/Internal/Matcher.new(rc, substr.substr, s);
        match Regex/Internal/Matcher.setBounds!(start = offset.offset, end = String.length(s), m) {
            case Ok()   => Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.start, m) |> Result.toOption
            case Err(_) => None
        }
    }

    ///
    /// Find the first instance of `patt` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnLeft(patt: Pattern, s: String): (String, String) = region rc {
        let m = Regex/Internal/Matcher.new(rc, patt, s);
        match Regex/Internal/Matcher.firstSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(end = r.start, s), String.sliceRight(start = r.start, s))
        }
    }

    pub def breakAfterLeft(patt: Pattern, s: String): (String, String) = region rc {
        let m = Regex/Internal/Matcher.new(rc, patt, s);
        match Regex/Internal/Matcher.firstSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(end = r.end, s), String.sliceRight(start = r.end, s))
        }
    }

    ///
    /// Find the last instance of `patt` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(patt: Pattern, s: String): (String, String) = region rc {
        let m = Regex/Internal/Matcher.new(rc, patt, s);
        match Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(end = r.end, s), String.sliceRight(start = r.end, s))
        }
    }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakBeforeRight(p: Pattern, s: String): (String, String) = region rc {
        let m = Regex/Internal/Matcher.new(rc, p, s);
        match Regex/Internal/Matcher.lastSubmatch(Regex/Internal/Matcher.range, m) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(end = r.start, s), String.sliceRight(start = r.start, s))
        }
    }


}
