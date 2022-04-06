/*
 *  Copyright 2021 Matthew Lutze
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

///
/// A type class for Euclidean modulo.
///
pub class Mod[a] {
    ///
    /// Returns the Euclidean modulo of `x` and `n`.
    ///
    pub def mod(x: a, n: a): a
}

instance Mod[Int8] {
    pub def mod(x: Int8, n: Int8): Int8 =
        if (n == 0i8)
            0i8
        else
            $INT8_REM$($INT8_REM$(x, n) + n, n)
}

instance Mod[Int16] {
    pub def mod(x: Int16, n: Int16): Int16 =
        if (n == 0i16)
            0i16
        else
            $INT16_REM$($INT16_REM$(x, n) + n, n)
}

instance Mod[Int32] {
    pub def mod(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$($INT32_REM$(x, n) + n, n)
}

instance Mod[Int64] {
    pub def mod(x: Int64, n: Int64): Int64 =
        if (n == 0i64)
            0i64
        else
            $INT64_REM$($INT64_REM$(x, n) + n, n)
}

instance Mod[BigInt] {
    pub def mod(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            $BIGINT_REM$($BIGINT_REM$(x, n) + n, n)
}
