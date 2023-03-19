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
/// The Matcher type and its module are consider considered internal.
/// The API is expected to change significantly.
///



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


    ///
    /// Attempt to find the last match by iterating through the input string.
    ///
    /// Returns `true` and moves the matcher to the start of the last match if there are
    /// one-or-more matches otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    /// Note - there are no primitive Java functions for right-to-left scanning provided
    /// by Java's JDK so the performance of `findLast!` is disadvantaged compared to `find!`.
    ///
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
    /// Set the bounds of the matcher's region.
    ///
    pub def setBounds!(start: {start = Int32}, end: {end = Int32}, m: Matcher[r]): Result[String, Unit] \ Write(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.region(Int32, Int32): ##java.util.regex.Matcher \ r as region1;
            let Matcher(m1) = m;
            discard region1(m1, start.start, end.end)
        })


    ///
    /// Return the start position of the current match of Matcher `m`.
    ///
    pub def start(m: Matcher[r]): Result[String, Int32] \ Read(r) =
        Result.tryCatch(_ -> {
            import java.util.regex.Matcher.start(): Int32 \ r;
            let Matcher(m1) = m;
            start(m1)
        })

    ///
    /// Return the end position of the current match of Matcher `m`.
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

    ///
    /// `MatchQuery` is a read query applied to a the current match of a `Matcher`.
    ///
    pub type alias MatchQuery[a: Type, r: Region] = Matcher[r] -> Result[String, a] \ Read(r)

    ///
    /// Returns the first result found by applying the query `asks` to `m` going from left to right.
    ///
    /// If the query `asks` or no match is found `Err(_)` is returned.
    ///
    pub def firstSubmatch(asks: MatchQuery[a, r], m: Matcher[r]): Result[String, a] \ { Read(r), Write(r) } =
        if (findFrom!(0, m))
            asks(m)
        else
            Err("No match")


    ///
    /// Returns the last result found by applying the query `asks` to `m` going from left to right.
    ///
    /// If the query `asks` or no match is found `Err(_)` is returned.
    ///
    pub def lastSubmatch(asks: MatchQuery[a, r], m: Matcher[r]): Result[String, a] \ { Read(r), Write(r) } =
        if (findLast!(m))
            asks(m)
        else
            Err("No match")

    ///
    /// Fold on all the submatches going from left to right.
    ///
    /// If the query `asks` fails further processing is stopped and `Err(_)` is returned.
    ///
    /// `f` is applied to the result of applying the query `asks` and the folds accumulating value.
    /// The initial accumulating value is `x`.
    ///
    pub def foldSubmatches(f: (b, a) -> b \ ef, x: b, asks: MatchQuery[a, r], m: Matcher[r]): Result[String, b] \ { Read(r), Write(r), ef } =
        def loop(acc) = {
            match find!(m) {
                case true  => match asks(m) {
                    case Ok(a)    => loop(f(acc, a))
                    case Err(msg) => Err(msg)
                }
                case false => Ok(acc)
            }
        };
        loop(x)

}
