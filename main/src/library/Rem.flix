/*
 *  Copyright 2020 Esben Bjerre
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
/// A type class for remainder.
///
pub class Rem[a] {
    ///
    /// Returns the remainder of `x / n`.
    ///
    pub def rem(x: a, n: a): a
}

instance Rem[Int8] {
    pub def rem(x: Int8, n: Int8): Int8 =
        if (n == 0i8)
            0i8
        else
            $INT8_REM$(x, n)
}

instance Rem[Int16] {
    pub def rem(x: Int16, n: Int16): Int16 =
        if (n == 0i16)
            0i16
        else
            $INT16_REM$(x, n)
}

instance Rem[Int32] {
    pub def rem(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$(x, n)
}

instance Rem[Int64] {
    pub def rem(x: Int64, n: Int64): Int64 =
        if (n == 0i64)
            0i64
        else
            $INT64_REM$(x, n)
}

instance Rem[BigInt] {
    pub def rem(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            $BIGINT_REM$(x, n)
}
