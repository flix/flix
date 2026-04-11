///
/// Adapts a `Writable` of bytes to a `Writable` of chars.
///
/// The portable profile currently supports UTF-8 encoding with JVM-compatible
/// replacement semantics: malformed UTF-16 input is replaced with the single
/// byte `?` when writing.
///
enum EncodingWriter[t]({
    charset = CharacterSet,
    byteWriter = t
})

instance Writable[EncodingWriter[t]] with Writable[t] where Writable.Elm[t] ~ Int8 {

    type Elm = Char

    type Aef = Writable.Aef[t]

    pub def write(buffer: Array[Char, r], writer: EncodingWriter[t]): Result[IoError, Int32] \ r + Writable.Aef[t] =
        EncodingWriter.write(buffer, writer)

}

pub mod EncodingWriter {

    ///
    /// Wraps `byteWriter` in an `EncodingWriter`.
    ///
    pub def wrap(charset: CharacterSet, byteWriter: t): EncodingWriter[t] =
        EncodingWriter({ charset = charset, byteWriter = byteWriter })

    ///
    /// Reads `k` items from `src` and writes them to `writer`.
    ///
    /// Returns `Ok(k)` to signify that `k` items were successfully read and written.
    ///
    pub def write(src: Array[Char, r], writer: EncodingWriter[t]): Result[IoError, Int32] \ r + Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Int8 =
        let EncodingWriter.EncodingWriter({ charset, byteWriter }) = writer;
        match charset {
            case CharacterSet.Utf8 => region rc {
                let dest = encodeUtf8(rc, src);
                forM(
                    _ <- writeBytes(rc, dest, byteWriter)
                ) yield {
                    Array.length(src)
                }
            }
        }

    ///
    /// Encodes the chars in `src` as UTF-8.
    ///
    def encodeUtf8(rc: Region[rc], src: Array[Char, r]): Array[Int8, rc] \ rc + r =
        let numChars = Array.length(src);
        let dest: Array[Int8, rc] = Array.empty(rc, numChars * 4);
        encodeLoop(rc, src, dest, numChars, 0, 0)

    ///
    /// Writes UTF-8 bytes to `dest`, returning a trimmed array when all chars are consumed.
    ///
    def encodeLoop(rc: Region[rc], src: Array[Char, r1], dest: Array[Int8, r2], numChars: Int32, i: Int32, j: Int32): Array[Int8, rc] \ { r1, r2, rc } =
        if (i >= numChars) {
            Array.take(rc, j, dest)
        } else {
            let ch = Array.get(i, src);
            if (Char.isSurrogate(ch)) {
                if (i + 1 < numChars) {
                    let ch1 = Array.get(i + 1, src);
                    if (Char.isSurrogatePair(high = ch, low = ch1)) {
                        let cp = Char.toSupplementaryCodePoint(high = ch, low = ch1);
                        let next = putUtf8(cp, j, dest);
                        encodeLoop(rc, src, dest, numChars, i + 2, next)
                    } else {
                        let next = putAsciiQuestion(j, dest);
                        encodeLoop(rc, src, dest, numChars, i + 1, next)
                    }
                } else {
                    let next = putAsciiQuestion(j, dest);
                    encodeLoop(rc, src, dest, numChars, i + 1, next)
                }
            } else {
                let cp = Char.toBmpCodePoint(ch);
                let next = putUtf8(cp, j, dest);
                encodeLoop(rc, src, dest, numChars, i + 1, next)
            }
        }

    ///
    /// Encodes the code point `cp` into `dest` at offset `j`.
    ///
    def putUtf8(cp: Int32, j: Int32, dest: Array[Int8, r]): Int32 \ r =
        if (cp <= 0x7F) {
            putByte(cp, j, dest);
            j + 1
        } else if (cp <= 0x7FF) {
            putByte(0xC0 + Int32.rightShift(cp, 6), j, dest);
            putByte(0x80 + Int32.bitwiseAnd(cp, 0x3F), j + 1, dest);
            j + 2
        } else if (cp <= 0xFFFF) {
            putByte(0xE0 + Int32.rightShift(cp, 12), j, dest);
            putByte(0x80 + Int32.bitwiseAnd(Int32.rightShift(cp, 6), 0x3F), j + 1, dest);
            putByte(0x80 + Int32.bitwiseAnd(cp, 0x3F), j + 2, dest);
            j + 3
        } else {
            putByte(0xF0 + Int32.rightShift(cp, 18), j, dest);
            putByte(0x80 + Int32.bitwiseAnd(Int32.rightShift(cp, 12), 0x3F), j + 1, dest);
            putByte(0x80 + Int32.bitwiseAnd(Int32.rightShift(cp, 6), 0x3F), j + 2, dest);
            putByte(0x80 + Int32.bitwiseAnd(cp, 0x3F), j + 3, dest);
            j + 4
        }

    ///
    /// Writes the single-byte `?` replacement used by the JVM UTF-8 encoder.
    ///
    def putAsciiQuestion(j: Int32, dest: Array[Int8, r]): Int32 \ r =
        putByte(0x3F, j, dest);
        j + 1

    ///
    /// Writes the unsigned byte value `n` into `dest`.
    ///
    def putByte(n: Int32, i: Int32, dest: Array[Int8, r]): Unit \ r =
        Array.put(toInt8Unsigned(n), i, dest)

    ///
    /// Converts an unsigned byte value in the range `0..255` to an `Int8`.
    ///
    def toInt8Unsigned(n: Int32): Int8 = {
        let signed = if (n <= 127) n else n - 256;
        match Int32.tryToInt8(signed) {
            case Some(b) => b
            case None    => bug!("EncodingWriter.toInt8Unsigned: invalid byte.")
        }
    }

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
