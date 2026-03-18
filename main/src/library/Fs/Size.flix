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

pub mod Fs.Size {

    use Fs.Size
    use Fs.Size.Size
    use Fs.SizeUnit

    /// Returns the zero size.
    pub def zero(): Size = Size(0i64)

    /// Returns a size of `n` in the given size unit `u`.
    pub def of(n: Int32, u: SizeUnit): Size = match u {
        case SizeUnit.Bytes     => bytes(Int32.toInt64(n)),
        case SizeUnit.KiloBytes => kiloBytes(n),
        case SizeUnit.MegaBytes => megaBytes(n),
        case SizeUnit.GigaBytes => gigaBytes(n),
        case SizeUnit.TeraBytes => teraBytes(n)
    }

    /// Returns a size of `n` bytes.
    pub def bytes(n: Int64): Size = Size(n)

    /// Returns a size of `n` kilobytes.
    pub def kiloBytes(n: Int32): Size = Size(Int32.toInt64(n) * (1_024i64))

    /// Returns a size of `n` megabytes.
    pub def megaBytes(n: Int32): Size = Size(Int32.toInt64(n) * (1_024i64 * 1_024i64))

    /// Returns a size of `n` gigabytes.
    pub def gigaBytes(n: Int32): Size = Size(Int32.toInt64(n) * (1_024i64 * 1_024i64 * 1_024i64))

    /// Returns a size of `n` terabytes.
    pub def teraBytes(n: Int32): Size = Size(Int32.toInt64(n) * (1_024i64 * 1_024i64 * 1_024i64 * 1_024i64))

    /// Returns the total number of bytes in `s`.
    pub def toBytes(s: Size): Int64 =
        let Size(n) = s;
        n

    /// Returns the total number of kilobytes in `s` (truncated).
    pub def toKiloBytes(s: Size): Int64 =
        let Size(n) = s;
        n / (1_024i64)

    /// Returns the total number of megabytes in `s` (truncated).
    pub def toMegaBytes(s: Size): Int64 =
        let Size(n) = s;
        n / (1_024i64 * 1_024i64)

    /// Returns the total number of gigabytes in `s` (truncated).
    pub def toGigaBytes(s: Size): Int64 =
        let Size(n) = s;
        n / (1_024i64 * 1_024i64 * 1_024i64)

    /// Returns the total number of terabytes in `s` (truncated).
    pub def toTeraBytes(s: Size): Int64 =
        let Size(n) = s;
        n / (1_024i64 * 1_024i64 * 1_024i64 * 1_024i64)

    /// Returns `true` if `s` is zero.
    pub def isZero(s: Size): Bool =
        let Size(n) = s;
        n == 0i64

    /// Returns `true` if `s` is positive.
    pub def isPositive(s: Size): Bool =
        let Size(n) = s;
        n > 0i64

    /// Returns `true` if `s` is negative.
    pub def isNegative(s: Size): Bool =
        let Size(n) = s;
        n < 0i64

}
