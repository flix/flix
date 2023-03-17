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
/// Alternative implementations of functions from `String.flix` that use regexs
///

namespace Regex/Text {

    use Regex.Pattern
    use Regex.Pattern.{Pattern}

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
        let m1 = newMatcher(rc, from.from, s);
        replaceAll(m1, to.to)
    }

    ///
    /// Returns string `s` with the first match of the regular expression `from` replaced by the string `to`.
    ///
    pub def replaceFirst(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceFirst(String): String \ rc;
        let m1 = newMatcher(rc, from.from, s);
        replaceFirst(m1, to.to)
    }

    ///
    /// Returns `true` if the string `s` starts the pattern `prefix`.
    ///
    pub def startsWith(prefix: {prefix = Pattern}, s: String): Bool = region rc {
        import java.util.regex.Matcher.lookingAt(): Bool \ rc;
        newMatcher(rc, prefix.prefix, s) |> lookingAt
    }

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// This will be slower than `startsWith` because there is no primitive Java function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(suffix: {suffix = Pattern}, s: String): Bool =
        match lastSubmatch(suffix.suffix, s, range) {
            case Err(_)  => false
            case Ok(rng) => String.length(s) - 1 == rng.end
        }


    ///
    /// Create a Matcher for Pattern `patt` on the source String `input`.
    ///
    def newMatcher(_: Region[r], p: Pattern, s: String): ##java.util.regex.Matcher \ Write(r) =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence): ##java.util.regex.Matcher \ r;
        let Pattern(p1) = p;
        matcher(p1, unchecked_cast(s as ##java.lang.CharSequence))

    ///
    /// Attempt to find the next match. Returns `true` and moves to the next match
    /// if there is a next match otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    def find!(_: Region[r], m: ##java.util.regex.Matcher): Bool \ Write(r) =
        import java.util.regex.Matcher.find(): Bool \ r;
        find(m)

    ///
    /// Attempt to find the next match after the supplied position `pos`.
    /// Returns `true` and moves to the next match if there is a next match
    /// otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    def findFrom!(_: Region[r], pos: Int32, m: ##java.util.regex.Matcher): Result[String, Bool] \ Write(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.find(Int32): Bool \ r;
            find(m, pos)
        })

    pub def findLast!(rc: Region[r], m: ##java.util.regex.Matcher): Bool \ Write(r) =
        def loop(lastPos) = {
            match find!(rc, m) {
                case true  => {let pos1 = start(m); loop(pos1)}
                case false => lastPos
            }
        };
        match find!(rc, m) {
            case false  => false
            case true   => {
                let pos = start(m);
                match loop(pos) {
                    case Err(_)          => false
                    case Ok(startOfLast) => findFrom!(rc, startOfLast - 1, m) |> Result.getWithDefault(false)
                }
            }
        }

    ///
    /// Return the start position of the matcher `m`.
    ///
    def start(m: ##java.util.regex.Matcher): Result[String, Int32] =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.start(): Int32 \ {};
            start(m)
        })

    ///
    /// Return the end position of the matcher `m`.
    ///
    def end(m: ##java.util.regex.Matcher): Result[String, Int32] =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.end(): Int32 \ {};
            end(m)
        })

    ///
    /// Return the start and end positions of the matcher `m`.
    ///
    pub def range(m: ##java.util.regex.Matcher): Result[String, {start = Int32, end = Int32}] =
        use Result.flatMap;
        let* s = start(m);
        let* e = end(m);
        Ok({start = s, end = e})


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    def lastSubmatch(p: Pattern, s: String, asks: ##java.util.regex.Matcher-> Result[String, a]): Result[String, a] = region rc {
        let m = newMatcher(rc, p, s);
        if (findLast!(rc, m))
            asks(m)
        else
            Err("No match")
    }


}
