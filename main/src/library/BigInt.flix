/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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

namespace BigInt {

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigInt, y: BigInt): BigInt = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigInt, y: BigInt): BigInt = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigInt): BigInt = if (x >= 0ii) x else -x

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigInt, y: BigInt): BigInt = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigInt, y: BigInt): Int32 = {
        if (x == y)     0
        else if (x < y) -1
        else            1
    }

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigInt): Int32 = compare(x, 0ii)

    ///
    /// Returns the bit of `x` at `position` (either 0 or 1).
    ///
    /// The bits of x have positions: 0 (rightmost bit), 1, 2, ...
    ///
    pub def getBit(x: BigInt, position: Int32): Int32 =
        if ((x >>> position) % 2ii == 0ii) 0 else 1

    ///
    /// Returns the greatest common non-negative divisor of `x` and `y`.
    ///
    pub def gcd(x: BigInt, y: BigInt): BigInt = if (y == 0ii) abs(x) else gcd(y, x % y)

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    ///
    pub def setBit(x: BigInt, position: Int32): BigInt = x ||| (1ii <<< position)

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    ///
    pub def clearBit(x: BigInt, position: Int32): BigInt = x &&& ~~~(1ii <<< position)

    ///
    /// Returns `x` with the bit at position `position` flipped.
    ///
    pub def flipBit(x: BigInt, position: Int32): BigInt = x ^^^ (1ii <<< position)

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: BigInt): String =
        import java.math.BigInteger.toString();
        x.toString() as & Pure

    ///
    /// Parse the string `s` as an BigInt, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def fromString(s: String): Result[BigInt, String] = try {
        import java.lang.String.strip();
        import new java.math.BigInteger(String) as parseBigInt;
        Ok(s.strip().parseBigInt()) as & Pure
     } catch {
         case _: ##java.lang.NumberFormatException => Err("BigInt.fromString")
     }
}
