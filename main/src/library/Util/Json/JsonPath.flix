/*
 * Copyright 2026 Flix Authors
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

pub mod Util.Json.JsonPath {

    use Util.Json.Json
    use Util.Json.Json.{JArray, JObject}

    ///
    /// A path locating a value inside a `Json` document, as a sequence of
    /// `JsonStep`s.
    ///
    /// The `List[JsonStep]` is stored head-at-root: the first element is the
    /// outermost step and the last element is the leaf. Walking head-to-tail
    /// yields the natural root-to-leaf order used by `toJsonPointer`.
    ///
    pub enum JsonPath with Eq {
        case JsonPath(List[JsonStep])
    }

    ///
    /// A single step in a `JsonPath`: either an array index or an object key.
    ///
    pub enum JsonStep with Eq, ToString {
        case Index(Int32)
        case Key(String)
    }

    instance ToString[JsonPath] {
        pub def toString(p: JsonPath): String = toJsonPointer(p)
    }

    instance Formattable[JsonPath] {
        pub def format(p: JsonPath): RichString =
            let slash = RichString.gray("/");
            List.foldLeft((acc, st) -> match st {
                case JsonStep.Index(i) => acc + slash + RichString.yellow(Int32.toString(i))
                case JsonStep.Key(k)   => acc + slash + RichString.cyan(escapeReferenceToken(k))
            }, RichString.empty(), steps(p))
    }

    ///
    /// Returns the empty path (the root).
    ///
    pub def empty(): JsonPath = JsonPath(Nil)

    ///
    /// Prepends `s` to `p`. O(1).
    ///
    /// This is the decode-side primitive: as a decode error bubbles up, each
    /// outer wrapper `cons`es its own step, leaving the head as the outermost
    /// (root) step.
    ///
    pub def cons(s: JsonStep, p: JsonPath): JsonPath = match p {
        case JsonPath(xs) => JsonPath(s :: xs)
    }

    ///
    /// Appends `s` to `p`. O(n).
    ///
    /// This is the navigation-side primitive: when constructing a path forward
    /// from the root, each new step extends the leaf end.
    ///
    pub def snoc(p: JsonPath, s: JsonStep): JsonPath = match p {
        case JsonPath(xs) => JsonPath(xs ::: (s :: Nil))
    }

    ///
    /// Returns a path with the given steps in root-to-leaf order.
    ///
    pub def fromList(xs: List[JsonStep]): JsonPath = JsonPath(xs)

    ///
    /// Returns the steps of `p` in root-to-leaf order.
    ///
    pub def steps(p: JsonPath): List[JsonStep] = match p {
        case JsonPath(xs) => xs
    }

    ///
    /// Renders `p` as a JSON Pointer (RFC 6901).
    ///
    /// The empty path is rendered as the empty string. A non-empty path is
    /// rendered as `/`-separated reference tokens, where keys are escaped per
    /// RFC 6901 (`~` -> `~0`, `/` -> `~1`) and indices are rendered in decimal.
    ///
    pub def toJsonPointer(p: JsonPath): String = region rc {
        let sb = StringBuilder.empty(rc);
        foreach (st <- steps(p)) {
            StringBuilder.append('/', sb);
            match st {
                case JsonStep.Index(i) => StringBuilder.appendString(Int32.toString(i), sb)
                case JsonStep.Key(k)   => writeEscapedToken(k, sb)
            }
        };
        StringBuilder.toString(sb)
    }

    ///
    /// Parses a JSON Pointer (RFC 6901) into a `JsonPath`.
    ///
    /// Returns `None` if `s` is non-empty and does not start with `/`, or if a
    /// reference token contains an invalid `~` escape.
    ///
    /// All-digit segments without leading zeros (or the lone segment `"0"`)
    /// classify as `Index`; everything else classifies as `Key`. This is a
    /// heuristic — round-tripping a `Key("123")` through `toJsonPointer` then
    /// `fromJsonPointer` yields `Index(123)`.
    ///
    pub def fromJsonPointer(s: String): Option[JsonPath] =
        if (String.isEmpty(s)) Some(empty())
        else if (not String.startsWith(prefix = "/", s)) None
        else {
            let segments = String.splitOn(substr = "/", s);
            match segments {
                case Nil       => None  // unreachable: splitOn always yields >= 1 segment
                case _ :: rest => parseSegments(rest, Nil)
            }
        }

    ///
    /// Returns the value at `p` in `j`, or `None` if any step is missing or
    /// the shape is wrong (e.g. a `Key` step into a non-object).
    ///
    pub def navigate(p: JsonPath, j: Json): Option[Json] = navigateLoop(steps(p), j)

    ///
    /// Recursively descends into `j` step by step.
    ///
    def navigateLoop(xs: List[JsonStep], j: Json): Option[Json] = match xs {
        case Nil       => Some(j)
        case st :: rest => match (st, j) {
            case (JsonStep.Index(i), JArray(v)) =>
                if (i >= 0 and i < Vector.length(v)) navigateLoop(rest, Vector.get(i, v))
                else None
            case (JsonStep.Key(k), JObject(m)) => match Map.get(k, m) {
                case Some(v) => navigateLoop(rest, v)
                case None    => None
            }
            case _ => None
        }
    }

    ///
    /// Recurses through the segments of a JSON Pointer, accumulating decoded
    /// steps in reverse (so `acc` is leaf-to-root). Reverses on success.
    ///
    def parseSegments(xs: List[String], acc: List[JsonStep]): Option[JsonPath] = match xs {
        case Nil      => Some(JsonPath(List.reverse(acc)))
        case s :: rest => match unescapeReferenceToken(s) {
            case None    => None
            case Some(u) => parseSegments(rest, classifyToken(u) :: acc)
        }
    }

    ///
    /// Classifies a reference token as either an `Index` (if it is a valid
    /// non-negative integer with no leading zeros) or a `Key`.
    ///
    def classifyToken(s: String): JsonStep =
        if (isArrayIndex(s)) {
            match Int32.parse(10, s) {
                case Ok(i)  => JsonStep.Index(i)
                case Err(_) => JsonStep.Key(s)
            }
        } else JsonStep.Key(s)

    ///
    /// Returns `true` if `s` is the canonical decimal form of a non-negative
    /// integer: either `"0"` or a sequence of digits with no leading zero.
    ///
    def isArrayIndex(s: String): Bool =
        if (String.isEmpty(s)) false
        else if (s == "0") true
        else if (String.charAt(0, s) == '0') false
        else List.forAll(Char.isDigit, String.toList(s))

    ///
    /// Appends `k` to `sb`, escaped per RFC 6901: `~` becomes `~0`, `/` becomes
    /// `~1`, and all other characters are passed through unchanged.
    ///
    def writeEscapedToken(k: String, sb: StringBuilder[r]): Unit \ r =
        foreach (c <- String.toList(k)) {
            match c {
                case '~' => StringBuilder.appendString("~0", sb)
                case '/' => StringBuilder.appendString("~1", sb)
                case _   => StringBuilder.append(c, sb)
            }
        }

    ///
    /// Returns `s` escaped per RFC 6901, used by `Formattable[JsonPath]` to
    /// render a `Key` step.
    ///
    def escapeReferenceToken(s: String): String = region rc {
        let sb = StringBuilder.empty(rc);
        writeEscapedToken(s, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Decodes the RFC 6901 escapes in `s`: `~0` -> `~`, `~1` -> `/`.
    ///
    /// Returns `None` if `s` contains a `~` not immediately followed by `0` or
    /// `1` (including a trailing `~`).
    ///
    def unescapeReferenceToken(s: String): Option[String] = region rc {
        let sb = StringBuilder.empty(rc);
        unescapeLoop(s, 0, String.length(s), sb)
    }

    def unescapeLoop(s: String, i: Int32, n: Int32, sb: StringBuilder[r]): Option[String] \ r =
        if (i >= n) Some(StringBuilder.toString(sb))
        else {
            let c = String.charAt(i, s);
            if (c == '~') {
                if (i + 1 >= n) None
                else {
                    let next = String.charAt(i + 1, s);
                    match next {
                        case '0' => StringBuilder.append('~', sb); unescapeLoop(s, i + 2, n, sb)
                        case '1' => StringBuilder.append('/', sb); unescapeLoop(s, i + 2, n, sb)
                        case _   => None
                    }
                }
            } else {
                StringBuilder.append(c, sb);
                unescapeLoop(s, i + 1, n, sb)
            }
        }

}
