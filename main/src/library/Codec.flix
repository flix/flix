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

pub mod Codec {

    import java.lang.IllegalArgumentException
    import java.util.{Base64 => JBase64}

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

}
