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

pub mod Util.Codec {

    import java.lang.IllegalArgumentException
    import java.util.{Base64 => JBase64}
    import java.util.HexFormat

    ///
    /// Encodes the given bytes `v` as a Base64 string.
    ///
    pub def encodeBase64(v: Vector[Int8]): String =
        unsafe IO {
            let encoder = JBase64.getEncoder();
            encoder.encodeToString(v)
        }

    ///
    /// Decodes the given Base64 string `s` to bytes.
    ///
    /// Returns `Err` if `s` is not valid Base64.
    ///
    pub def decodeBase64(s: String): Result[String, Vector[Int8]] =
        try {
            unsafe IO {
                let decoder = JBase64.getDecoder();
                Ok(Array.toVector(decoder.decode(s)))
            }
        } catch {
            case _: IllegalArgumentException => Err("Codec.decodeBase64")
        }

    ///
    /// Encodes the given string `s` (as UTF-8 bytes) as a Base64 string.
    ///
    pub def encodeBase64String(s: String): String =
        encodeBase64(String.toBytes(s))

    ///
    /// Decodes the given Base64 string `s` to a UTF-8 string.
    ///
    /// Returns `Err` if `s` is not valid Base64.
    ///
    pub def decodeBase64String(s: String): Result[String, String] =
        Result.map(String.fromBytes, decodeBase64(s))

    ///
    /// Encodes the given bytes `v` as a lowercase hex string.
    ///
    pub def encodeHex(v: Vector[Int8]): String =
        unsafe IO { HexFormat.of().formatHex(v) }

    ///
    /// Decodes the given hex string `s` to bytes.
    ///
    /// Returns `Err` if `s` is not a valid hex string.
    ///
    pub def decodeHex(s: String): Result[String, Vector[Int8]] =
        try {
            unsafe IO {
                Ok(Array.toVector(HexFormat.of().parseHex(s)))
            }
        } catch {
            case _: IllegalArgumentException => Err("Codec.decodeHex")
        }

    ///
    /// Encodes the given string `s` (as UTF-8 bytes) as a lowercase hex string.
    ///
    pub def encodeHexString(s: String): String =
        encodeHex(String.toBytes(s))

    ///
    /// Decodes the given hex string `s` to a UTF-8 string.
    ///
    /// Returns `Err` if `s` is not a valid hex string.
    ///
    pub def decodeHexString(s: String): Result[String, String] =
        Result.map(String.fromBytes, decodeHex(s))

}
