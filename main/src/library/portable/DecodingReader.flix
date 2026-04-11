///
/// Adapts a `Readable` of bytes to a `Readable` of chars.
///
/// The portable profile currently supports UTF-8 decoding with JVM-compatible
/// replacement semantics: malformed input is replaced with U+FFFD.
///
struct DecodingReader[t, r] {
    rc: Region[r],
    byteReader: t,
    charset: CharacterSet,
    mut leftovers: Array[Int8, r]
}

instance Readable[DecodingReader[t, rr]] with Readable[t] where Readable.Elm[t] ~ Int8 {

    type Elm = Char

    type Aef = Readable.Aef[t] + rr

    pub def read(buffer: Array[Char, rb], reader: DecodingReader[t, rr]): Result[IoError, Int32] \ rb + rr + Readable.Aef[t] =
        DecodingReader.read(buffer, reader)

}

pub mod DecodingReader {

    ///
    /// Wraps `byteReader` in a `DecodingReader`.
    ///
    pub def wrap(rc: Region[rr], charset: CharacterSet, byteReader: t): DecodingReader[t, rr] \ rr =
        new DecodingReader @ rc {
            rc = rc,
            charset = charset,
            byteReader = byteReader,
            leftovers = Array.empty(rc, 0)
        }

    ///
    /// Reads bytes from the underlying byteReader and decodes the result into `dest`.
    ///
    pub def read(dest: Array[Char, rb], reader: DecodingReader[t, rr]): Result[IoError, Int32] \ rr + rb + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ Int8 =
        match reader->charset {
            case CharacterSet.Utf8 => forM(
                (src, endOfInput) <- {
                    // Four bytes are enough to produce any UTF-16 code unit prefix.
                    let totalBytesNeeded = 4 * Array.length(dest);
                    readBytes(totalBytesNeeded, reader)
                };
                charsRead <- {
                    let charsRead = decodeUtf8(src, dest, endOfInput, reader);

                    // A zero-length decode with more input expected means we stopped on an incomplete suffix.
                    if (not endOfInput and charsRead == 0) read(dest, reader) else Ok(charsRead)
                }
            ) yield charsRead
        }

    ///
    /// Returns the next bytes to decode, including any pending leftovers.
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
    /// Decodes as many UTF-8 bytes as possible into `dest` and stores any suffix in `leftovers`.
    ///
    def decodeUtf8(src: Array[Int8, rb1], dest: Array[Char, rb2], endOfInput: Bool, reader: DecodingReader[t, rr]): Int32 \ rr + rb1 + rb2 =
        decodeLoop(src, dest, endOfInput, reader, Array.length(src), Array.length(dest), 0, 0)

    ///
    /// Decoding loop. `i` counts consumed bytes, `j` counts produced UTF-16 code units.
    ///
    def decodeLoop(src: Array[Int8, rb1], dest: Array[Char, rb2], endOfInput: Bool, reader: DecodingReader[t, rr], srcLen: Int32, destLen: Int32, i: Int32, j: Int32): Int32 \ rr + rb1 + rb2 =
        if (i >= srcLen or j >= destLen) {
            reader->leftovers = Array.drop(reader->rc, i, src);
            j
        } else {
            let b0 = unsignedByte(Array.get(i, src));
            if (b0 <= 0x7F) {
                putBmp(0x0000 + b0, j, dest);
                decodeLoop(src, dest, endOfInput, reader, srcLen, destLen, i + 1, j + 1)
            } else {
                match sequenceLength(b0) {
                    case 0 =>
                        putReplacement(j, dest);
                        decodeLoop(src, dest, endOfInput, reader, srcLen, destLen, i + 1, j + 1)

                    case len =>
                        if (i + len > srcLen) {
                            if (endOfInput) {
                                if (j + 1 <= destLen) {
                                    putReplacement(j, dest);
                                    reader->leftovers = Array.empty(reader->rc, 0);
                                    j + 1
                                } else {
                                    reader->leftovers = Array.drop(reader->rc, i, src);
                                    j
                                }
                            } else {
                                reader->leftovers = Array.drop(reader->rc, i, src);
                                j
                            }
                        } else if (not hasContinuationBytes(src, i + 1, len - 1)) {
                            putReplacement(j, dest);
                            decodeLoop(src, dest, endOfInput, reader, srcLen, destLen, i + 1, j + 1)
                        } else {
                            let cp = decodeCodePoint(src, i, len);
                            if (isMalformedCodePoint(cp, len)) {
                                putReplacement(j, dest);
                                decodeLoop(src, dest, endOfInput, reader, srcLen, destLen, i + 1, j + 1)
                            } else {
                                let unitsNeeded = if (cp <= 0xFFFF) 1 else 2;
                                if (j + unitsNeeded > destLen) {
                                    reader->leftovers = Array.drop(reader->rc, i, src);
                                    j
                                } else {
                                    let next = putCodePoint(cp, j, dest);
                                    decodeLoop(src, dest, endOfInput, reader, srcLen, destLen, i + len, next)
                                }
                            }
                        }
                }
            }
        }

    ///
    /// Returns the number of bytes in a UTF-8 sequence starting with `b0`, or zero if invalid.
    ///
    def sequenceLength(b0: Int32): Int32 =
        if (0xC2 <= b0 and b0 <= 0xDF) 2
        else if (0xE0 <= b0 and b0 <= 0xEF) 3
        else if (0xF0 <= b0 and b0 <= 0xF4) 4
        else 0

    ///
    /// Returns `true` if the next `count` bytes are all continuation bytes.
    ///
    def hasContinuationBytes(src: Array[Int8, r], start: Int32, count: Int32): Bool \ r =
        if (count <= 0) {
            true
        } else {
            let b = unsignedByte(Array.get(start, src));
            if (0x80 <= b and b <= 0xBF)
                hasContinuationBytes(src, start + 1, count - 1)
            else
                false
        }

    ///
    /// Decodes a valid-length UTF-8 sequence from `src`.
    ///
    def decodeCodePoint(src: Array[Int8, r], i: Int32, len: Int32): Int32 \ r = match len {
        case 2 =>
            let b0 = unsignedByte(Array.get(i, src));
            let b1 = unsignedByte(Array.get(i + 1, src));
            Int32.leftShift(Int32.bitwiseAnd(b0, 0x1F), 6) + Int32.bitwiseAnd(b1, 0x3F)

        case 3 =>
            let b0 = unsignedByte(Array.get(i, src));
            let b1 = unsignedByte(Array.get(i + 1, src));
            let b2 = unsignedByte(Array.get(i + 2, src));
            Int32.leftShift(Int32.bitwiseAnd(b0, 0x0F), 12) +
                Int32.leftShift(Int32.bitwiseAnd(b1, 0x3F), 6) +
                Int32.bitwiseAnd(b2, 0x3F)

        case 4 =>
            let b0 = unsignedByte(Array.get(i, src));
            let b1 = unsignedByte(Array.get(i + 1, src));
            let b2 = unsignedByte(Array.get(i + 2, src));
            let b3 = unsignedByte(Array.get(i + 3, src));
            Int32.leftShift(Int32.bitwiseAnd(b0, 0x07), 18) +
                Int32.leftShift(Int32.bitwiseAnd(b1, 0x3F), 12) +
                Int32.leftShift(Int32.bitwiseAnd(b2, 0x3F), 6) +
                Int32.bitwiseAnd(b3, 0x3F)

        case _ => 0
    }

    ///
    /// Returns `true` if `cp` is malformed for a sequence of length `len`.
    ///
    def isMalformedCodePoint(cp: Int32, len: Int32): Bool =
        let min = match len {
            case 2 => 0x80
            case 3 => 0x800
            case 4 => 0x10000
            case _ => Int32.maxValue()
        };
        cp < min or cp > 0x10FFFF or (0xD800 <= cp and cp <= 0xDFFF)

    ///
    /// Writes the code point `cp` into `dest`, returning the next output index.
    ///
    def putCodePoint(cp: Int32, j: Int32, dest: Array[Char, r]): Int32 \ r =
        if (cp <= 0xFFFF) {
            putBmp(cp, j, dest);
            j + 1
        } else {
            match CodePoint.toSupplementaryChars(cp) {
                case Some((hi, lo)) =>
                    Array.put(hi, j, dest);
                    Array.put(lo, j + 1, dest);
                    j + 2
                case None =>
                    putReplacement(j, dest);
                    j + 1
            }
        }

    ///
    /// Writes the BMP code point `cp` into `dest`.
    ///
    def putBmp(cp: Int32, j: Int32, dest: Array[Char, r]): Unit \ r =
        match CodePoint.toBmpChar(cp) {
            case Some(ch) => Array.put(ch, j, dest)
            case None     => putReplacement(j, dest)
        }

    ///
    /// Writes U+FFFD into `dest`.
    ///
    def putReplacement(j: Int32, dest: Array[Char, r]): Unit \ r =
        putBmp(0xFFFD, j, dest)

    ///
    /// Converts an `Int8` byte to its unsigned `Int32` value.
    ///
    def unsignedByte(b: Int8): Int32 =
        let n = Int8.toInt32(b);
        if (n >= 0) n else n + 256

}
