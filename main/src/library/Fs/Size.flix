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

    use Fs.SizeUnit

    ///
    /// Represents a file size stored as bytes (Int64).
    ///
    pub enum Size(Int64) with Eq, Order, Hash

    instance Add[Size] {
        pub def add(x: Size, y: Size): Size =
            let Size(x1) = x;
            let Size(y1) = y;
            Size(x1 + y1)
    }

    instance Sub[Size] {
        pub def sub(x: Size, y: Size): Size =
            let Size(x1) = x;
            let Size(y1) = y;
            Size(x1 - y1)
    }

    instance ToString[Size] {
        pub def toString(s: Size): String =
            let Size(b) = s;
            if (b == 0i64)
                "0 B"
            else
                let abs = Int64.abs(b);
                let sign = if (b < 0i64) "-" else "";
                let tb1 = 1_024i64 * 1_024i64 * 1_024i64 * 1_024i64;
                let gb1 = 1_024i64 * 1_024i64 * 1_024i64;
                let mb1 = 1_024i64 * 1_024i64;
                let kb1 = 1_024i64;
                if (abs >= tb1)
                    let tb = abs / tb1;
                    let rem = abs - (tb * tb1);
                    let gb = rem / gb1;
                    "${sign}${tb} TB ${gb} GB"
                else if (abs >= gb1)
                    let gb = abs / gb1;
                    let rem = abs - (gb * gb1);
                    let mb = rem / mb1;
                    "${sign}${gb} GB ${mb} MB"
                else if (abs >= mb1)
                    let mb = abs / mb1;
                    let rem = abs - (mb * mb1);
                    let kb = rem / kb1;
                    "${sign}${mb} MB ${kb} KB"
                else if (abs >= kb1)
                    let kb = abs / kb1;
                    let rem = abs - (kb * kb1);
                    "${sign}${kb} KB ${rem} B"
                else
                    "${sign}${abs} B"
    }

    instance Formattable[Size] {
        pub def format(s: Size): RichString = RichString.fromString(ToString.toString(s))
    }

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
