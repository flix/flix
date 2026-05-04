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

pub mod Util.Json.JsonError {

    use Util.Json.JsonPath.JsonPath
    use Util.Json.JsonPath.JsonStep

    ///
    /// An error that can occur when parsing or decoding JSON.
    ///
    /// `Parse` wraps a parser failure together with the UTF-16 code-unit
    /// offset into the input where the failure was detected.
    ///
    /// `Decode` wraps a decoder failure together with the path of the failing
    /// value inside the parsed `Json` document.
    ///
    pub enum JsonError with Eq {
        case Parse(Int32, ParseErrorKind)
        case Decode(JsonPath, DecodeErrorKind)
    }

    ///
    /// A parser-side failure: produced by `Json.parse` while reading raw JSON
    /// text. Parser failures do not carry a `JsonPath`.
    ///
    pub enum ParseErrorKind with Eq {
        case DuplicateKey(String)
        case InvalidEscape(String)
        case InvalidNumber(String)
        case InvalidUnicode(String)
        case UnexpectedChar(Char)
        case UnexpectedEof
    }

    ///
    /// A decoder-side failure: produced by `FromJson.fromJson` while turning a
    /// parsed `Json` value into a typed value.
    ///
    pub enum DecodeErrorKind with Eq {
        case MissingKey(String)
        case TypeMismatch(String)
    }

    instance ToString[ParseErrorKind] {
        pub def toString(k: ParseErrorKind): String = match k {
            case ParseErrorKind.DuplicateKey(k1)   => "duplicate key '${k1}'"
            case ParseErrorKind.InvalidEscape(s)   => "invalid escape sequence '${s}'"
            case ParseErrorKind.InvalidNumber(s)   => "invalid number '${s}'"
            case ParseErrorKind.InvalidUnicode(s)  => "invalid unicode '${s}'"
            case ParseErrorKind.UnexpectedChar(c)  => "unexpected character '${c}'"
            case ParseErrorKind.UnexpectedEof      => "unexpected end of input"
        }
    }

    instance ToString[DecodeErrorKind] {
        pub def toString(k: DecodeErrorKind): String = match k {
            case DecodeErrorKind.MissingKey(k1)    => "missing key '${k1}'"
            case DecodeErrorKind.TypeMismatch(d)   => "type mismatch: expected ${d}"
        }
    }

    instance ToString[JsonError] {
        pub def toString(e: JsonError): String = match e {
            case JsonError.Parse(o, k)  => "${ToString.toString(k)} at offset ${o}"
            case JsonError.Decode(p, k) => match Util.Json.JsonPath.steps(p) {
                case Nil => ToString.toString(k)
                case _   => "${ToString.toString(k)} at ${p}"
            }
        }
    }

    instance Formattable[ParseErrorKind] {
        pub def format(k: ParseErrorKind): RichString = RichString.fromString(ToString.toString(k))
    }

    instance Formattable[DecodeErrorKind] {
        pub def format(k: DecodeErrorKind): RichString = RichString.fromString(ToString.toString(k))
    }

    instance Formattable[JsonError] {
        pub def format(e: JsonError): RichString = match e {
            case JsonError.Parse(o, k)  =>
                Formattable.format(k) + RichString.fromString(" at offset ") + RichString.yellow(Int32.toString(o))
            case JsonError.Decode(p, k) => match Util.Json.JsonPath.steps(p) {
                case Nil => Formattable.format(k)
                case _   => Formattable.format(k) + RichString.fromString(" at ") + Formattable.format(p)
            }
        }
    }

    ///
    /// Prepends `s` to the path of `e` if `e` is a `Decode`. Leaves `Parse`
    /// errors unchanged.
    ///
    /// Used by `FromJson` helpers and recursive instances to thread the path
    /// of a failing decode lazily — i.e. only on the error path.
    ///
    pub def prependStep(s: JsonStep, e: JsonError): JsonError = match e {
        case JsonError.Decode(p, k) => JsonError.Decode(Util.Json.JsonPath.cons(s, p), k)
        case JsonError.Parse(_, _)  => e
    }

}
