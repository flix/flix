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





namespace Regex/Internal {

    ///
    /// Matcher is imperative - it can be seen a a stepper through a stream
    /// of matches. The matcher is updated with the function find! to move to
    /// the next match.
    ///
    pub enum Matcher[_: Region](##java.util.regex.Matcher)


}

namespace Regex/Internal/Matcher {

    use Regex/Internal.Matcher
    use Regex/Internal.Matcher.{Matcher}
    use Regex.Pattern
    use Regex.Pattern.{Pattern}

    ///
    /// Create a Matcher for Pattern `patt` on the source String `input`.
    ///
    pub def new(_: Region[r], p: Pattern, s: String): Matcher[r] \ Write(r) =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence): ##java.util.regex.Matcher \ r;
        let Pattern(p1) = p;
        Matcher(matcher(p1, unchecked_cast(s as ##java.lang.CharSequence)))

    ///
    /// Attempt to find the next match. Returns `true` and moves to the next match
    /// if there is a next match otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def find!(m: Matcher[r]): Bool \ Write(r) =
        import java.util.regex.Matcher.find(): Bool \ r;
        let Matcher(m1) = m;
        find(m1)

    ///
    /// Attempt to find the next match after the supplied position `pos`.
    /// Returns `true` and moves to the next match if there is a next match
    /// otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findFrom!(pos: Int32, m: Matcher[r]): Bool \ Write(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.find(Int32): Bool \ r;
            let Matcher(m1) = m;
            find(m1, pos)
        }) |> Result.getWithDefault(false)

    pub def findLast!(m: Matcher[r]): Bool \ Write(r) =
        def loop(lastPos) = {
            match find!(m) {
                case true  => {let pos1 = start(m); loop(pos1)}
                case false => lastPos
            }
        };
        match find!(m) {
            case false  => false
            case true   => {
                let pos = start(m);
                match loop(pos) {
                    case Err(_)          => false
                    case Ok(startOfLast) => findFrom!(startOfLast - 1, m)
                }
            }
        }

    ///
    /// Return the start position of the matcher `m`.
    ///
    pub def start(m: Matcher[r]): Result[String, Int32] \ Read(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.start(): Int32 \ r;
            let Matcher(m1) = m;
            start(m1)
        })

    ///
    /// Return the end position of the matcher `m`.
    ///
    pub def end(m: Matcher[r]): Result[String, Int32] \ Read(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.end(): Int32 \ r;
            let Matcher(m1) = m;
            end(m1)
        })

    ///
    /// Return the start and end positions of the matcher `m`.
    ///
    pub def range(m: Matcher[r]): Result[String, {start = Int32, end = Int32}] \ Read(r) =
        use Result.flatMap;
        let* s = start(m);
        let* e = end(m);
        Ok({start = s, end = e})

    ///
    /// Return the text content of the current match of matcher `m`.
    ///
    pub def content(m: Matcher[r]): Result[String, String] \ Read(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.group(): String \ r;
            let Matcher(m1) = m;
            group(m1)
        })


    pub type alias MatchQuery[a: Type, r: Region] = Matcher[r] -> Result[String, a] \ Read(r)

    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatch(rc: Region[r], p: Pattern, s: String, asks: MatchQuery[a, r]): Result[String, a] \ { Read(r), Write(r) } =
        let m = new(rc, p, s);
        if (findFrom!(0, m))
            asks(m)
        else
            Err("No match")


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatch(rc: Region[r], p: Pattern, s: String, asks: MatchQuery[a, r]): Result[String, a] \ { Read(r), Write(r) } =
        let m = new(rc, p, s);
        if (findLast!(m))
            asks(m)
        else
            Err("No match")


}
