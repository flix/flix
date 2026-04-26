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

pub mod Json.JsonError {

    ///
    /// An error that can occur when parsing or decoding JSON.
    ///
    pub enum JsonError with Eq, ToString {
        case DuplicateKey(String)      // the repeated key
        case InvalidEscape(String)
        case InvalidNumber(String)
        case InvalidUnicode(String)
        case MissingKey(String)        // the absent key
        case TypeMismatch(String)      // description of what was expected
        case UnexpectedChar(Char)
        case UnexpectedEof
    }

    instance Formattable[JsonError] {
        pub def format(e: JsonError): RichString = RichString.fromString(match e {
            case DuplicateKey(k)     => "duplicate key '${k}'"
            case InvalidEscape(s)    => "invalid escape sequence '${s}'"
            case InvalidNumber(s)    => "invalid number '${s}'"
            case InvalidUnicode(s)   => "invalid unicode '${s}'"
            case MissingKey(k)       => "missing key '${k}'"
            case TypeMismatch(d)     => "type mismatch: expected ${d}"
            case UnexpectedChar(c)   => "unexpected character '${c}'"
            case UnexpectedEof       => "unexpected end of input"
        })
    }

}
