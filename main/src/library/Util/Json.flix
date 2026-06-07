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

pub mod Util.Json {

    use Util.Json.JsonError
    use Util.Json.JsonError.JsonError.Parse
    use Util.Json.JsonError.ParseErrorKind.{DuplicateKey, InvalidEscape, InvalidNumber, InvalidUnicode, MaxDepthExceeded, UnexpectedChar, UnexpectedEof}
    use Util.Json.FromJson

    ///
    /// Represents a JSON value.
    ///
    /// JSON numbers are stored as `BigDecimal` for lossless round-trip;
    /// JSON objects are stored as `Map[String, Json]`, which gives
    /// canonical (sorted) key order on output.
    ///
    pub enum Json with Eq {
        case JArray(Vector[Json])
        case JBool(Bool)
        case JNull
        case JNumber(BigDecimal)
        case JObject(Map[String, Json])
        case JString(String)
    }

    ///
    /// The maximum nesting depth (number of enclosing arrays/objects) the parser
    /// accepts. Inputs nested deeper are rejected with `MaxDepthExceeded` rather
    /// than overflowing the JVM stack, since the recursive-descent parser uses one
    /// stack frame per level.
    ///
    def maxDepth(): Int32 = 128

    instance ToString[Json] {
        pub def toString(j: Json): String = toCompactString(j)
    }

    ///
    /// Parses `s` as a JSON value.
    ///
    /// Returns `Ok(j)` if `s` is a single well-formed JSON value (with
    /// optional surrounding whitespace) and nothing else. Returns
    /// `Err(JsonError)` otherwise — see `Json.JsonError` for the cases.
    ///
    pub def parse(s: String): Result[JsonError, Json] =
        let n = String.length(s);
        let i0 = skipWs(s, 0, n);
        match parseValue(s, i0, n, 0) {
            case Err(e) => Err(e)
            case Ok((j, i1)) =>
                let i2 = skipWs(s, i1, n);
                if (i2 >= n) Ok(j)
                else Err(Parse(i2, UnexpectedChar(String.charAt(i2, s))))
        }

    ///
    /// Parses `s` as a JSON value and decodes it as `a` via `FromJson[a]`.
    ///
    /// Returns `Err(Parse(...))` if parsing fails or `Err(Decode(...))` if
    /// decoding fails.
    ///
    pub def decode(s: String): Result[JsonError, a] with FromJson[a] = forM (
        j <- parse(s);
        x <- FromJson.fromJson(j)
    ) yield x

    ///
    /// Returns the compact JSON rendering of `j`: a single line with
    /// no extraneous whitespace.
    ///
    pub def toCompactString(j: Json): String = region rc {
        let sb = StringBuilder.empty(rc);
        write(j, 0, Style.Compact, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the pretty-printed JSON rendering of `j`, using `indent` spaces
    /// per nesting level.
    ///
    /// Empty arrays and objects are rendered on a single line as `[]` or `{}`.
    /// Non-empty containers place each element on its own line; object keys
    /// are followed by `": "`.
    ///
    pub def toPrettyString(indent: Int32, j: Json): String = region rc {
        let sb = StringBuilder.empty(rc);
        write(j, 0, Style.Pretty(indent), sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the pretty-printed JSON rendering of `j` as a `RichString`,
    /// with syntax-coloured tokens.
    ///
    /// The layout is identical to `toPrettyString`. Colours: `null` is gray,
    /// booleans are magenta, numbers are yellow, string values are green,
    /// and object keys are cyan. Punctuation and whitespace use the default
    /// colour.
    ///
    pub def toRichString(indent: Int32, j: Json): RichString =
        richPretty(indent, 0, j)

    ///
    /// Controls the layout used by `write`.
    ///
    /// `Compact` emits no extraneous whitespace; `Pretty(indent)` places each
    /// element on its own line using `indent` spaces per nesting level.
    ///
    enum Style {
        case Compact
        case Pretty(Int32)
    }

    ///
    /// Appends the JSON rendering of `j` to `sb` using style `s`.
    ///
    /// `d` is the current nesting level (0 at the root); it is ignored in
    /// `Compact` style. Empty arrays and objects always render as `[]` or `{}`.
    ///
    def write(j: Json, d: Int32, s: Style, sb: StringBuilder[r]): Unit \ r = match j {
        case JArray(v) =>
            if (Vector.isEmpty(v))
                StringBuilder.appendString("[]", sb)
            else {
                StringBuilder.append('[', sb);
                foreach ((i, e) <- ForEach.withIndex(v)) {
                    if (i > 0) { StringBuilder.append(',', sb) };
                    breakIndent(d + 1, s, sb);
                    write(e, d + 1, s, sb)
                };
                breakIndent(d, s, sb);
                StringBuilder.append(']', sb)
            }
        case JBool(b) =>
            StringBuilder.appendString(if (b) "true" else "false", sb)
        case JNull =>
            StringBuilder.appendString("null", sb)
        case JNumber(n) =>
            StringBuilder.appendString(BigDecimal.toPlainString(n), sb)
        case JObject(m) =>
            if (Map.isEmpty(m))
                StringBuilder.appendString("{}", sb)
            else {
                StringBuilder.append('{', sb);
                foreach ((i, (k, x)) <- ForEach.withIndex(m)) {
                    if (i > 0) { StringBuilder.append(',', sb) };
                    breakIndent(d + 1, s, sb);
                    writeEscaped(k, sb);
                    writeColon(s, sb);
                    write(x, d + 1, s, sb)
                };
                breakIndent(d, s, sb);
                StringBuilder.append('}', sb)
            }
        case JString(str) =>
            writeEscaped(str, sb)
    }

    ///
    /// In `Pretty` style, appends a newline followed by the indentation for
    /// nesting level `d`. In `Compact` style, does nothing.
    ///
    def breakIndent(d: Int32, s: Style, sb: StringBuilder[r]): Unit \ r = match s {
        case Style.Compact     => ()
        case Style.Pretty(ind) => StringBuilder.append('\n', sb); writeIndent(ind * d, sb)
    }

    ///
    /// Appends the object key/value separator to `sb`: `":"` in `Compact`
    /// style and `": "` in `Pretty` style.
    ///
    def writeColon(s: Style, sb: StringBuilder[r]): Unit \ r = match s {
        case Style.Compact   => StringBuilder.append(':', sb)
        case Style.Pretty(_) => StringBuilder.appendString(": ", sb)
    }

    ///
    /// Appends `n` spaces to `sb`. Does nothing if `n <= 0`.
    ///
    def writeIndent(n: Int32, sb: StringBuilder[r]): Unit \ r =
        if (n > 0) { StringBuilder.appendString(String.repeat(n, " "), sb) }

    ///
    /// Returns the pretty-printed JSON rendering of `j` as a coloured
    /// `RichString`.
    ///
    /// `indent` is the number of spaces per nesting level; `depth` is the
    /// current nesting level (0 at the root).
    ///
    def richPretty(indent: Int32, depth: Int32, j: Json): RichString = match j {
        case JArray(v) if Vector.isEmpty(v) =>
            RichString.fromString("[]")
        case JArray(v) =>
            let nl = RichString.fromString("\n");
            let inner = richIndent(indent * (depth + 1));
            let outer = richIndent(indent * depth);
            let elements = Vector.map(e -> richPretty(indent, depth + 1, e), v);
            let sep = RichString.fromString(",") + nl + inner;
            RichString.fromString("[") + nl + inner +
                RichString.join(sep, elements) + nl + outer + RichString.fromString("]")
        case JBool(b) =>
            RichString.magenta(if (b) "true" else "false")
        case JNull =>
            RichString.gray("null")
        case JNumber(n) =>
            RichString.yellow(BigDecimal.toPlainString(n))
        case JObject(m) if Map.isEmpty(m) =>
            RichString.fromString("{}")
        case JObject(m) =>
            let nl = RichString.fromString("\n");
            let colon = RichString.fromString(": ");
            let inner = richIndent(indent * (depth + 1));
            let outer = richIndent(indent * depth);
            let entries = Map.toList(m) |> List.map(match (k, x) ->
                RichString.cyan(escapeString(k)) + colon + richPretty(indent, depth + 1, x));
            let sep = RichString.fromString(",") + nl + inner;
            RichString.fromString("{") + nl + inner +
                RichString.join(sep, entries) + nl + outer + RichString.fromString("}")
        case JString(s) =>
            RichString.green(escapeString(s))
    }

    ///
    /// Returns a `RichString` of `n` plain spaces. Returns the empty `RichString`
    /// if `n <= 0`.
    ///
    def richIndent(n: Int32): RichString =
        if (n > 0) RichString.fromString(String.repeat(n, " ")) else RichString.empty()

    ///
    /// Returns `s` as a JSON string literal: enclosed in double quotes, with
    /// backslash, double-quote, and control characters escaped per RFC 8259.
    ///
    def escapeString(s: String): String = region rc {
        let sb = StringBuilder.empty(rc);
        writeEscaped(s, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Appends `s` to `sb` as a JSON string literal: enclosed in double quotes,
    /// with backslash, double-quote, and control characters escaped per RFC 8259.
    ///
    def writeEscaped(s: String, sb: StringBuilder[r]): Unit \ r =
        StringBuilder.append('"', sb);
        foreach (c <- String.toList(s)) {
            writeEscapedChar(c, sb)
        };
        StringBuilder.append('"', sb)

    ///
    /// Appends `c` to `sb`, JSON-escaped if necessary.
    ///
    /// The two-character short escapes are emitted for backslash (0x5C),
    /// double-quote (0x22), backspace (0x08), tab (0x09), newline (0x0A),
    /// form-feed (0x0C), and carriage-return (0x0D). Other characters with
    /// code point below 0x20 are emitted as `\u00XX`. Everything else is
    /// passed through unchanged.
    ///
    def writeEscapedChar(c: Char, sb: StringBuilder[r]): Unit \ r =
        let cp = Char.toBmpCodePoint(c);
        match cp {
            case 0x5C => StringBuilder.appendString("\\\\", sb)
            case 0x22 => StringBuilder.appendString("\\\"", sb)
            case 0x08 => StringBuilder.appendString("\\b", sb)
            case 0x09 => StringBuilder.appendString("\\t", sb)
            case 0x0A => StringBuilder.appendString("\\n", sb)
            case 0x0C => StringBuilder.appendString("\\f", sb)
            case 0x0D => StringBuilder.appendString("\\r", sb)
            case _ if cp < 0x20 => writeControlEscape(cp, sb)
            case _ => StringBuilder.append(c, sb)
        }

    ///
    /// Appends the JSON `\u00XX` escape for the given code point `cp`.
    ///
    /// Precondition: `0 <= cp < 0x20`.
    ///
    def writeControlEscape(cp: Int32, sb: StringBuilder[r]): Unit \ r =
        StringBuilder.appendString("\\u00", sb);
        StringBuilder.append(hexDigit(cp / 16), sb);
        StringBuilder.append(hexDigit(Int32.bitwiseAnd(cp, 15)), sb)

    ///
    /// Returns the lowercase hex digit for `n`.
    ///
    /// Precondition: `0 <= n < 16`. Otherwise unreachable from the call site
    /// in `writeControlEscape`.
    ///
    def hexDigit(n: Int32): Char =
        match Char.forDigit(radix = 16, n) {
            case Some(c) => c
            case None    => unreachable!()
        }

    ///
    /// Returns the position of the first non-whitespace character at or after `i`.
    ///
    /// JSON whitespace per RFC 8259 is space, tab, line feed, and carriage return.
    ///
    def skipWs(s: String, i: Int32, n: Int32): Int32 =
        if (i >= n) i
        else {
            let c = String.charAt(i, s);
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') skipWs(s, i + 1, n)
            else i
        }

    ///
    /// Parses a single JSON value at position `i` (skipping leading whitespace).
    ///
    /// `depth` is the number of arrays/objects already open around this value.
    ///
    def parseValue(s: String, i: Int32, n: Int32, depth: Int32): Result[JsonError, (Json, Int32)] =
        let j = skipWs(s, i, n);
        if (j >= n) Err(Parse(j, UnexpectedEof))
        else {
            let c = String.charAt(j, s);
            match c {
                case '{' if depth >= maxDepth() => Err(Parse(j, MaxDepthExceeded))
                case '[' if depth >= maxDepth() => Err(Parse(j, MaxDepthExceeded))
                case '{' => parseObject(s, j, n, depth)
                case '[' => parseArray(s, j, n, depth)
                case 't' => parseLiteral(s, j, n, "true", JBool(true))
                case 'f' => parseLiteral(s, j, n, "false", JBool(false))
                case 'n' => parseLiteral(s, j, n, "null", JNull)
                case _ if Char.toBmpCodePoint(c) == 0x22 =>
                    Result.map(match (str, k) -> (JString(str), k), parseString(s, j, n))
                case _ if Char.isDigit(c) or c == '-' => parseNumber(s, j, n)
                case _ => Err(Parse(j, UnexpectedChar(c)))
            }
        }

    ///
    /// Matches the literal `lit` at position `i` and returns `(v, i + length(lit))`.
    ///
    def parseLiteral(s: String, i: Int32, n: Int32, lit: String, v: Json): Result[JsonError, (Json, Int32)] =
        let m = String.length(lit);
        if (matchAt(s, i, n, lit, m, 0)) Ok((v, i + m))
        else if (i >= n) Err(Parse(i, UnexpectedEof))
        else Err(Parse(i, UnexpectedChar(String.charAt(i, s))))

    ///
    /// Returns `true` if `s[i..i+m]` equals `lit`. Returns `false` if it differs
    /// or if there are fewer than `m` characters left in `s`.
    ///
    def matchAt(s: String, i: Int32, n: Int32, lit: String, m: Int32, j: Int32): Bool =
        if (j >= m) true
        else if (i + j >= n) false
        else if (String.charAt(i + j, s) != String.charAt(j, lit)) false
        else matchAt(s, i, n, lit, m, j + 1)

    ///
    /// Parses a JSON number starting at `i`. Defers to `BigDecimal.fromString`
    /// for the actual parse; this just identifies the run of number characters.
    ///
    def parseNumber(s: String, i: Int32, n: Int32): Result[JsonError, (Json, Int32)] =
        let end = scanNumberEnd(s, i, n);
        let str = String.slice(start = i, end = end, s);
        match BigDecimal.fromString(str) {
            case Some(bd) => Ok((JNumber(bd), end))
            case None     => Err(Parse(i, InvalidNumber(str)))
        }

    ///
    /// Returns the position after the last character that could be part of a JSON
    /// number starting at `i`.
    ///
    def scanNumberEnd(s: String, i: Int32, n: Int32): Int32 =
        if (i >= n) i
        else {
            let c = String.charAt(i, s);
            if (Char.isDigit(c) or c == '-' or c == '+' or c == '.' or c == 'e' or c == 'E')
                scanNumberEnd(s, i + 1, n)
            else i
        }

    ///
    /// Parses a JSON string literal starting at position `i` (which must point
    /// at the opening double quote).
    ///
    def parseString(s: String, i: Int32, n: Int32): Result[JsonError, (String, Int32)] = region rc {
        if (i >= n) Err(Parse(i, UnexpectedEof))
        else if (Char.toBmpCodePoint(String.charAt(i, s)) != 0x22)
            Err(Parse(i, UnexpectedChar(String.charAt(i, s))))
        else {
            let sb = StringBuilder.empty(rc);
            scanString(s, i + 1, n, sb)
        }
    }

    ///
    /// Scans the body of a JSON string starting at `i` (just past the opening `"`),
    /// appending decoded characters to `sb`. Returns the decoded string and the
    /// position just past the closing `"`.
    ///
    def scanString(s: String, i: Int32, n: Int32, sb: StringBuilder[r]): Result[JsonError, (String, Int32)] \ r =
        if (i >= n) Err(Parse(i, UnexpectedEof))
        else {
            let c = String.charAt(i, s);
            let cp = Char.toBmpCodePoint(c);
            match cp {
                case 0x22 => Ok((StringBuilder.toString(sb), i + 1))
                case 0x5C => scanEscape(s, i + 1, n, sb)
                case _ =>
                    StringBuilder.append(c, sb);
                    scanString(s, i + 1, n, sb)
            }
        }

    ///
    /// Decodes a JSON string escape starting at `i` (just past the backslash).
    ///
    def scanEscape(s: String, i: Int32, n: Int32, sb: StringBuilder[r]): Result[JsonError, (String, Int32)] \ r =
        if (i >= n) Err(Parse(i, UnexpectedEof))
        else {
            let c = String.charAt(i, s);
            let cp = Char.toBmpCodePoint(c);
            match cp {
                case 0x22 => StringBuilder.appendCodePoint(0x22, sb); scanString(s, i + 1, n, sb)
                case 0x5C => StringBuilder.appendCodePoint(0x5C, sb); scanString(s, i + 1, n, sb)
                case 0x2F => StringBuilder.appendCodePoint(0x2F, sb); scanString(s, i + 1, n, sb)
                case 0x62 => StringBuilder.appendCodePoint(0x08, sb); scanString(s, i + 1, n, sb)
                case 0x66 => StringBuilder.appendCodePoint(0x0C, sb); scanString(s, i + 1, n, sb)
                case 0x6E => StringBuilder.appendCodePoint(0x0A, sb); scanString(s, i + 1, n, sb)
                case 0x72 => StringBuilder.appendCodePoint(0x0D, sb); scanString(s, i + 1, n, sb)
                case 0x74 => StringBuilder.appendCodePoint(0x09, sb); scanString(s, i + 1, n, sb)
                case 0x75 => parseUnicode(s, i + 1, n, sb)
                case _    => Err(Parse(i, InvalidEscape(Char.toString(c))))
            }
        }

    ///
    /// Decodes a `\uXXXX` escape starting at `i` (just past the `u`).
    ///
    def parseUnicode(s: String, i: Int32, n: Int32, sb: StringBuilder[r]): Result[JsonError, (String, Int32)] \ r =
        if (i + 4 > n) Err(Parse(i, UnexpectedEof))
        else {
            let hex = String.slice(start = i, end = i + 4, s);
            // A `\uXXXX` escape must be exactly four hex digits. We must reject any other
            // slice up front: `Int32.parse` accepts a leading sign and surrounding whitespace,
            // so e.g. `\u-100` would otherwise parse to a negative value and crash
            // `StringBuilder.appendCodePoint`. Four hex digits are always in 0..0xFFFF.
            if (not String.forAll(Char.isHexDigit, hex))
                Err(Parse(i, InvalidUnicode(hex)))
            else match Int32.parse(16, hex) {
                case Ok(cp) =>
                    StringBuilder.appendCodePoint(cp, sb);
                    scanString(s, i + 4, n, sb)
                case Err(_) =>
                    Err(Parse(i, InvalidUnicode(hex)))
            }
        }

    ///
    /// Parses a JSON array starting at `i` (which must point at the opening `[`).
    ///
    def parseArray(s: String, i: Int32, n: Int32, depth: Int32): Result[JsonError, (Json, Int32)] = region rc {
        let i1 = i + 1;
        let i2 = skipWs(s, i1, n);
        if (i2 >= n) Err(Parse(i2, UnexpectedEof))
        else if (String.charAt(i2, s) == ']') Ok((JArray(Vector#{}), i2 + 1))
        else {
            let items = MutList.empty(rc);
            scanArrayElements(s, i2, n, items, depth + 1)
        }
    }

    ///
    /// Scans a comma-separated list of array elements starting at `i`.
    ///
    /// `depth` is the nesting depth of the elements (one deeper than the array itself).
    ///
    def scanArrayElements(s: String, i: Int32, n: Int32, items: MutList[Json, r], depth: Int32): Result[JsonError, (Json, Int32)] \ r =
        match parseValue(s, i, n, depth) {
            case Err(e) => Err(e)
            case Ok((v, i1)) =>
                MutList.push(v, items);
                let i2 = skipWs(s, i1, n);
                if (i2 >= n) Err(Parse(i2, UnexpectedEof))
                else {
                    let c = String.charAt(i2, s);
                    if (c == ',') scanArrayElements(s, skipWs(s, i2 + 1, n), n, items, depth)
                    else if (c == ']') Ok((JArray(MutList.toVector(items)), i2 + 1))
                    else Err(Parse(i2, UnexpectedChar(c)))
                }
        }

    ///
    /// Parses a JSON object starting at `i` (which must point at the opening `{`).
    ///
    /// Rejects duplicate keys with `DuplicateKey(k)`.
    ///
    def parseObject(s: String, i: Int32, n: Int32, depth: Int32): Result[JsonError, (Json, Int32)] =
        let i1 = i + 1;
        let i2 = skipWs(s, i1, n);
        if (i2 >= n) Err(Parse(i2, UnexpectedEof))
        else if (String.charAt(i2, s) == '}') Ok((JObject(Map#{}), i2 + 1))
        else scanObjectEntries(s, i2, n, Map#{}, depth + 1)

    ///
    /// Scans a comma-separated list of `"key": value` entries starting at `i`.
    ///
    /// `depth` is the nesting depth of the values (one deeper than the object itself).
    ///
    def scanObjectEntries(s: String, i: Int32, n: Int32, m: Map[String, Json], depth: Int32): Result[JsonError, (Json, Int32)] =
        match parseString(s, i, n) {
            case Err(e) => Err(e)
            case Ok((k, i1)) =>
                if (Map.memberOf(k, m)) Err(Parse(i, DuplicateKey(k)))
                else {
                    let i2 = skipWs(s, i1, n);
                    if (i2 >= n) Err(Parse(i2, UnexpectedEof))
                    else if (String.charAt(i2, s) != ':') Err(Parse(i2, UnexpectedChar(String.charAt(i2, s))))
                    else match parseValue(s, i2 + 1, n, depth) {
                        case Err(e) => Err(e)
                        case Ok((v, i3)) =>
                            let m1 = Map.insert(k, v, m);
                            let i4 = skipWs(s, i3, n);
                            if (i4 >= n) Err(Parse(i4, UnexpectedEof))
                            else {
                                let c = String.charAt(i4, s);
                                if (c == ',') scanObjectEntries(s, skipWs(s, i4 + 1, n), n, m1, depth)
                                else if (c == '}') Ok((JObject(m1), i4 + 1))
                                else Err(Parse(i4, UnexpectedChar(c)))
                            }
                    }
                }
        }

}
