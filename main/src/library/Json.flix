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

pub mod Json {

    ///
    /// Represents a JSON value.
    ///
    /// JSON numbers are stored as `BigDecimal` for lossless round-trip;
    /// JSON objects are stored as `Map[String, Json]`, which gives
    /// canonical (sorted) key order on output.
    ///
    pub enum Json with Eq, ToString {
        case JArray(Vector[Json])
        case JBool(Bool)
        case JNull
        case JNumber(BigDecimal)
        case JObject(Map[String, Json])
        case JString(String)
    }

    ///
    /// Returns the compact JSON rendering of `j`: a single line with
    /// no extraneous whitespace.
    ///
    pub def toCompactString(j: Json): String = region rc {
        let sb = StringBuilder.empty(rc);
        writeCompact(j, sb);
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
        writePretty(indent, 0, j, sb);
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
    /// Appends the compact JSON rendering of `j` to `sb`.
    ///
    def writeCompact(j: Json, sb: StringBuilder[r]): Unit \ r = match j {
        case JArray(v) =>
            StringBuilder.append('[', sb);
            foreach ((i, e) <- ForEach.withIndex(v)) {
                if (i > 0) { StringBuilder.append(',', sb) };
                writeCompact(e, sb)
            };
            StringBuilder.append(']', sb)
        case JBool(b) =>
            StringBuilder.appendString(if (b) "true" else "false", sb)
        case JNull =>
            StringBuilder.appendString("null", sb)
        case JNumber(n) =>
            StringBuilder.append(n, sb)
        case JObject(m) =>
            StringBuilder.append('{', sb);
            foreach ((i, (k, x)) <- ForEach.withIndex(m)) {
                if (i > 0) { StringBuilder.append(',', sb) };
                writeEscaped(k, sb);
                StringBuilder.append(':', sb);
                writeCompact(x, sb)
            };
            StringBuilder.append('}', sb)
        case JString(s) =>
            writeEscaped(s, sb)
    }

    ///
    /// Appends the pretty-printed JSON rendering of `j` to `sb`.
    ///
    /// `indent` is the number of spaces per nesting level; `depth` is the
    /// current nesting level (0 at the root).
    ///
    def writePretty(indent: Int32, depth: Int32, j: Json, sb: StringBuilder[r]): Unit \ r = match j {
        case JArray(v) if Vector.isEmpty(v) =>
            StringBuilder.appendString("[]", sb)
        case JArray(v) =>
            StringBuilder.append('[', sb);
            foreach ((i, e) <- ForEach.withIndex(v)) {
                if (i > 0) { StringBuilder.append(',', sb) };
                StringBuilder.append('\n', sb);
                writeIndent(indent * (depth + 1), sb);
                writePretty(indent, depth + 1, e, sb)
            };
            StringBuilder.append('\n', sb);
            writeIndent(indent * depth, sb);
            StringBuilder.append(']', sb)
        case JBool(b) =>
            StringBuilder.appendString(if (b) "true" else "false", sb)
        case JNull =>
            StringBuilder.appendString("null", sb)
        case JNumber(n) =>
            StringBuilder.append(n, sb)
        case JObject(m) if Map.isEmpty(m) =>
            StringBuilder.appendString("{}", sb)
        case JObject(m) =>
            StringBuilder.append('{', sb);
            foreach ((i, (k, x)) <- ForEach.withIndex(m)) {
                if (i > 0) { StringBuilder.append(',', sb) };
                StringBuilder.append('\n', sb);
                writeIndent(indent * (depth + 1), sb);
                writeEscaped(k, sb);
                StringBuilder.appendString(": ", sb);
                writePretty(indent, depth + 1, x, sb)
            };
            StringBuilder.append('\n', sb);
            writeIndent(indent * depth, sb);
            StringBuilder.append('}', sb)
        case JString(s) =>
            writeEscaped(s, sb)
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
            RichString.yellow(n)
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

}
