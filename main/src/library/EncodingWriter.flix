/*
 *  Copyright 2025 Ry Wiese
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.nio.charset.CharsetEncoder

///
/// Adapts a `Writable` of bytes to a `Writable` of chars.
///
/// `byteWriter`: the underlying byte writer, an instance of `Writable` where `Writable.Elm[t] ~ Int8`.
/// `encoder`: used to encode bytes before writing them to the `byteReader`.
///
enum EncodingWriter[t]({encoder = CharsetEncoder, byteWriter = t})

instance Writable[EncodingWriter[t]] with Writable[t] where Writable.Elm[t] ~ Int8 {

    type Elm = Char

    type Aef = Writable.Aef[t]

    pub def write(buffer: Array[Char, r], writer: EncodingWriter[t]): Result[IoError, Int32] \ r + Writable.Aef[t] = EncodingWriter.write(buffer, writer)

}

pub mod EncodingWriter {
    import java.nio.charset.CharsetEncoder
    import java.nio.charset.CodingErrorAction
    import java.nio.ByteBuffer
    import java.nio.CharBuffer

    ///
    /// Wraps `byteWriter` in an `EncodingWriter`.
    ///
    pub def wrap(charset: CharacterSet, byteWriter: t): EncodingWriter[t] \ IO =
        let CharacterSet.CharacterSet(cs) = charset;
        EncodingWriter({
            encoder = cs.newEncoder().onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE),
            byteWriter = byteWriter
        })

    ///
    /// Reads `k` items from `src` and writes them to `writer`.
    ///
    /// Returns `Ok(k)` to signify that `k` items were successfully read and written to `r`.
    ///
    /// Guarantees that `0 <= k <= length(b)`.
    ///
    /// Returns `Err(e)` if some underlying I/O error occurs.
    ///
    pub def write(src: Array[Char, r], writer: EncodingWriter[t]): Result[IoError, Int32] \ r + Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Int8 =
        let EncodingWriter.EncodingWriter({ encoder, byteWriter }) = writer;
        region rc {
            let dest = encoder |> encode(rc, src);
            forM(
                _ <- byteWriter |> writeBytes(rc, dest)
            ) yield {
                src |> Array.length
            }
        }

    ///
    /// Encodes the chars in `src` using `encoder`.
    ///
    /// Returns an array of the encoded bytes whose size is the number of bytes encoded.
    ///
    def encode(rc: Region[rc], src: Array[Char, r], encoder: CharsetEncoder): Array[Int8, rc] \ rc + r =
        // Encoding is effectively a pure operation, so IO is casted away
        def do() = {
            let numChars = Array.length(src);
            let maxBytesPerChar = encoder.maxBytesPerChar() |> Float32.ceil |> Float32.clampToInt32(min = 1, max = Int32.maxValue(), nanValue = 4);
            let dest: Array[Int8, rc] = Array.empty(rc, numChars * maxBytesPerChar);
            let charBuffer = CharBuffer.wrap(src);
            let byteBuffer = ByteBuffer.wrap(dest);
            checked_ecast(encoder |> encodeBuffered(rc, charBuffer, byteBuffer))
        };
        checked_ecast(unsafe IO { do() })

    ///
    /// Encodes the chars from `charBuffer` into `byteBuffer` using `encoder`.
    ///
    /// Returns an array of the encoded bytes whose size is the number of bytes encoded.
    ///
    def encodeBuffered(rc: Region[rc], charBuffer: CharBuffer, byteBuffer: ByteBuffer, encoder: CharsetEncoder): Array[Int8, rc] \ rc =
        // Encoding is effectively a pure operation, so IO is casted away
        def do() = {
            let result = encoder.encode(charBuffer, byteBuffer, false);
            let bytesEncoded = byteBuffer.position();
            let dest = byteBuffer.array() |> Array.take(rc, bytesEncoded);
            if (result.isOverflow()) {
                charBuffer.flip();
                byteBuffer.clear();
                let rest = encoder |> encodeBuffered(rc, charBuffer, byteBuffer);
                Array.append(rc, dest, rest)
            } else {
                dest
            }
        };
        checked_ecast(unsafe IO { do() })

    ///
    /// Writes all bytes from `dest` into `byteWriter`.
    ///
    def writeBytes(rc: Region[r], dest: Array[Int8, r], byteWriter: t): Result[IoError, Unit] \ r + Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Int8 =
        def loop(arr) = {
            if (Array.isEmpty(arr)) {
                Ok(())
            } else {
                forM(
                    bytesWritten <- Writable.write(arr, byteWriter);
                    _ <- loop(arr |> Array.drop(rc, bytesWritten))
                ) yield ()
            }
        };
        loop(dest)

}
