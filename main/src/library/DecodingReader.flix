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

import java.nio.charset.CharsetDecoder
import java.nio.ByteBuffer
import java.nio.CharBuffer

///
/// Adapts a `Readable` of bytes to a `Readable` of chars.
///
/// `byteReader`: the underlying byte reader, an instance of `Readable where Readable.Elm[t] ~ Int8`.
/// `decoder`: used to decode bytes read from `byteReader`.
/// `leftovers`: stores bytes that have been consumed from `byteReader` but have not yet been decoded;
///              either because they are lead bytes whose continuation bytes have not been read, or
///              because `DecodingReader` overestimated the number of bytes needed to fill a char buffer.
///
struct DecodingReader[t, r] {
    rc: Region[r],
    byteReader: t,
    decoder: CharsetDecoder,
    mut leftovers: Array[Int8, r]
}

instance Readable[DecodingReader[t, rr]] with Readable[t] where Readable.Elm[t] ~ Int8 {

    type Elm = Char

    type Aef = Readable.Aef[t] + IO + rr

    pub def read(buffer: Array[Char, rb], reader: DecodingReader[t, rr]): Result[IoError, Int32] \ rb + rr + Readable.Aef[t] + IO = DecodingReader.read(buffer, reader)

}

pub mod DecodingReader {
    import java.nio.charset.CharsetDecoder
    import java.nio.charset.CodingErrorAction
    import java.nio.ByteBuffer
    import java.nio.CharBuffer

    ///
    /// Wraps `byteReader` in a `DecodingReader`.
    ///
    pub def wrap(rc: Region[rr], charset: CharacterSet, byteReader: t): DecodingReader[t, rr] \ rr + IO =
        let CharacterSet.CharacterSet(cs) = charset;
        new DecodingReader @ rc {
            rc = rc,
            decoder = cs.newDecoder().onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE),
            byteReader = byteReader,
            leftovers = Array.empty(rc, 0)
        }

    ///
    /// Reads bytes from the underlying byteReader and decodes the result into `dest`.
    ///
    /// Returns `Ok(k)` to signify that `k` chars were successfully read and written to `dest`.
    ///
    /// Guarantees that `0 <= k <= length(dest)`.
    ///
    /// Returns `Err(e)` if some underlying error occurs.
    ///
    pub def read(dest: Array[Char, rb], reader: DecodingReader[t, rr]): Result[IoError, Int32] \ IO + rr + rb + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ Int8 =
        forM(
            (src, endOfInput) <- {
                // At most 4 bytes per char, guarantees we can fill dest if there are enough bytes to do so
                let totalBytesNeeded = 4 * Array.length(dest);
                readBytes(totalBytesNeeded, reader)
            };
            charsRead <- {
                let charsRead = decode(src, dest, endOfInput, reader);

                // charsRead == 0 would imply end of input to the caller.
                // block again until end of input or we have at least one char to return
                if (not endOfInput and charsRead == 0) read(dest, reader) else Ok(charsRead)
            }
        ) yield charsRead

    ///
    /// Returns `Ok(bytes, endOfInput)` where `bytes` are the next bytes to be decoded. `endOfInput` means that the
    /// underlying reader returned EOF, and that `bytes` is exactly the leftovers.
    ///
    /// `bytes` contains `leftovers` from the last read prepended to new bytes read from the `byteReader` (if necessary).
    ///
    /// `0 <= length(bytes) <= numBytes`.
    ///
    def readBytes(numBytes: Int32, reader: DecodingReader[t, r]): Result[IoError, (Array[Int8, r], Bool)] \ r + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ Int8 =
        let bytesToRead = numBytes - Array.length(reader->leftovers);
        if (bytesToRead > 0) {
            let src = Array.empty(reader->rc, bytesToRead);
            forM(
                bytesRead <- Readable.read(src, reader->byteReader)
            ) yield {
                let newBytes = Array.take(reader->rc, bytesRead, src);
                let bytes = Array.append(reader->rc, reader->leftovers, newBytes);
                let endOfInput = bytesRead == 0;
                (bytes, endOfInput)
            }
        } else {
            Ok((reader->leftovers, false))
        }

    ///
    /// Decodes the bytes in `src`` as far as possible, and stores any remaining `leftovers` back into the `reader`.
    ///
    def decode(src: Array[Int8, rb1], dest: Array[Char, rb2], endOfInput: Bool, reader: DecodingReader[t, rr]): Int32 \ IO + rr + rb1 + rb2 with Readable[t] where Readable.Elm[t] ~ Int8 =
        let byteBuffer = ByteBuffer.wrap(src);
        let charBuffer = CharBuffer.wrap(dest);
        reader->decoder.decode(byteBuffer, charBuffer, endOfInput);
        reader->leftovers = Array.slice(reader->rc, start = byteBuffer.position(), end = byteBuffer.limit(), byteBuffer.array());
        checked_ecast(charBuffer.position())

}
