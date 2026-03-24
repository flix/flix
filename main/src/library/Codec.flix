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

    ///
    /// Encodes the given bytes `v` as a Base64 string.
    ///
    /// Uses the standard Base64 alphabet (RFC 4648) with `=` padding.
    ///
    pub def encodeBase64(v: Vector[Int8]): String =
        let len = Vector.length(v);
        if (len == 0) ""
        else region rc {
            let sb = StringBuilder.empty(rc);
            def emit(idx) = StringBuilder.appendCodePoint(alphabetCodePoint(idx), sb);
            def pad() = StringBuilder.appendCodePoint(61, sb);
            def loop(i) = {
                if (i + 3 > len) ()
                else {
                    let b0 = toUnsigned(Vector.get(i, v));
                    let b1 = toUnsigned(Vector.get(i + 1, v));
                    let b2 = toUnsigned(Vector.get(i + 2, v));
                    let n = Int32.bitwiseOr(
                        Int32.bitwiseOr(Int32.leftShift(b0, 16), Int32.leftShift(b1, 8)),
                        b2
                    );
                    emit(bits6(18, n));
                    emit(bits6(12, n));
                    emit(bits6(6, n));
                    emit(bits6(0, n));
                    loop(i + 3)
                }
            };
            loop(0);
            let rem = Int32.remainder(len, 3);
            let pos = len - rem;
            if (rem == 1) {
                let b0 = toUnsigned(Vector.get(pos, v));
                let n = Int32.leftShift(b0, 16);
                emit(bits6(18, n));
                emit(bits6(12, n));
                pad();
                pad()
            } else if (rem == 2) {
                let b0 = toUnsigned(Vector.get(pos, v));
                let b1 = toUnsigned(Vector.get(pos + 1, v));
                let n = Int32.bitwiseOr(Int32.leftShift(b0, 16), Int32.leftShift(b1, 8));
                emit(bits6(18, n));
                emit(bits6(12, n));
                emit(bits6(6, n));
                pad()
            } else ();
            StringBuilder.toString(sb)
        }

    ///
    /// Decodes the given Base64 string `s` to bytes.
    ///
    /// Uses the standard Base64 alphabet (RFC 4648).
    ///
    /// Returns `Err` if `s` is not valid Base64.
    ///
    pub def decodeBase64(s: String): Result[String, Vector[Int8]] =
        let len = String.length(s);
        if (len == 0)
            Ok(Vector.empty())
        else if (Int32.remainder(len, 4) != 0)
            Err("Codec.decodeBase64")
        else {
            let pad = countPadding(s, len);
            if (pad > 2)
                Err("Codec.decodeBase64")
            else region rc {
                let out = MutList.empty(rc);
                let dataLen = if (pad > 0) len - 4 else len;
                def loop(i) = {
                    if (i >= dataLen) Ok(())
                    else {
                        let c0 = charToIndex(String.charAt(i, s));
                        let c1 = charToIndex(String.charAt(i + 1, s));
                        let c2 = charToIndex(String.charAt(i + 2, s));
                        let c3 = charToIndex(String.charAt(i + 3, s));
                        if (c0 < 0 or c1 < 0 or c2 < 0 or c3 < 0)
                            Err("Codec.decodeBase64")
                        else {
                            let n = Int32.bitwiseOr(
                                Int32.bitwiseOr(
                                    Int32.bitwiseOr(
                                        Int32.leftShift(c0, 18),
                                        Int32.leftShift(c1, 12)),
                                    Int32.leftShift(c2, 6)),
                                c3
                            );
                            MutList.push(toByte(bits8(16, n)), out);
                            MutList.push(toByte(bits8(8, n)), out);
                            MutList.push(toByte(bits8(0, n)), out);
                            loop(i + 4)
                        }
                    }
                };
                match loop(0) {
                    case Err(e) => Err(e)
                    case Ok(_) =>
                        if (pad == 0)
                            Ok(MutList.toVector(out))
                        else {
                            let i = dataLen;
                            let c0 = charToIndex(String.charAt(i, s));
                            let c1 = charToIndex(String.charAt(i + 1, s));
                            if (c0 < 0 or c1 < 0)
                                Err("Codec.decodeBase64")
                            else if (pad == 2) {
                                // 2 data chars → 1 output byte
                                let n = Int32.bitwiseOr(
                                    Int32.leftShift(c0, 18),
                                    Int32.leftShift(c1, 12)
                                );
                                MutList.push(toByte(bits8(16, n)), out);
                                Ok(MutList.toVector(out))
                            } else {
                                // 3 data chars → 2 output bytes
                                let c2 = charToIndex(String.charAt(i + 2, s));
                                if (c2 < 0)
                                    Err("Codec.decodeBase64")
                                else {
                                    let n = Int32.bitwiseOr(
                                        Int32.bitwiseOr(
                                            Int32.leftShift(c0, 18),
                                            Int32.leftShift(c1, 12)),
                                        Int32.leftShift(c2, 6)
                                    );
                                    MutList.push(toByte(bits8(16, n)), out);
                                    MutList.push(toByte(bits8(8, n)), out);
                                    Ok(MutList.toVector(out))
                                }
                            }
                        }
                }
            }
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
    /// Returns the standard Base64 alphabet (RFC 4648, Table 1).
    ///
    def alphabet(): String =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    ///
    /// Returns the code point of the Base64 alphabet character at index `i` (0–63).
    ///
    def alphabetCodePoint(i: Int32): Int32 =
        Char.toBmpCodePoint(String.charAt(i, alphabet()))

    ///
    /// Maps a character to its 6-bit Base64 index (0–63).
    ///
    /// Returns -1 if the character is not in the Base64 alphabet.
    ///
    def charToIndex(c: Char): Int32 =
        let cp = Char.toBmpCodePoint(c);
        if      (cp >= 65 and cp <= 90)   cp - 65        // A-Z → 0–25
        else if (cp >= 97 and cp <= 122)  cp - 97 + 26   // a-z → 26–51
        else if (cp >= 48 and cp <= 57)   cp - 48 + 52   // 0-9 → 52–61
        else if (cp == 43)                62              // +
        else if (cp == 47)                63              // /
        else                              -1

    ///
    /// Extracts 6 bits from the 24-bit value `n` at the given bit `shift` position.
    ///
    def bits6(shift: Int32, n: Int32): Int32 =
        Int32.bitwiseAnd(Int32.rightShift(n, shift), 0x3F)

    ///
    /// Extracts 8 bits from the 24-bit value `n` at the given bit `shift` position.
    ///
    def bits8(shift: Int32, n: Int32): Int32 =
        Int32.bitwiseAnd(Int32.rightShift(n, shift), 0xFF)

    ///
    /// Converts a signed `Int8` byte to an unsigned `Int32` in the range 0–255.
    ///
    def toUnsigned(b: Int8): Int32 =
        Int32.bitwiseAnd(Int8.toInt32(b), 0xFF)

    ///
    /// Converts an unsigned `Int32` in the range 0–255 to a signed `Int8`.
    ///
    /// Uses the XOR-and-subtract trick from `Net/Ipv4Addr.toSigned8`:
    /// flipping bit 7 then subtracting 128 maps 0–255 to -128–127.
    ///
    def toByte(x: Int32): Int8 =
        let signed = Int32.bitwiseXor(Int32.bitwiseAnd(x, 0xFF), 0x80) - 0x80;
        match Int32.tryToInt8(signed) {
            case Some(b) => b
            case None    => 0i8
        }

    ///
    /// Counts the number of `=` padding characters at the end of the string `s`.
    ///
    def countPadding(s: String, len: Int32): Int32 =
        let c1 = if (len >= 1 and String.charAt(len - 1, s) == '=') 1 else 0;
        let c2 = if (len >= 2 and String.charAt(len - 2, s) == '=') 1 else 0;
        c1 + c2

}
