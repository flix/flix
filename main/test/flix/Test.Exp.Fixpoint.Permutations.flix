/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

mod Test.Exp.Fixpoint.Permutations {

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Int32                               //
    /////////////////////////////////////////////////////////////////////////////
    
    // May not be bigger than 12.
    def smallN(): Int32 = 10

    ///
    /// Returns the result for the first `Int32` tests.
    ///
    def resultForFirstInt32(n: Int32): Vector[(Int32, Int32, Int32, Int32)] = {
        Vector.range(0, n - 2) |> Vector.map(x -> (x, x + 1, x + 2, x + 3))
    }

    @Test
    def testPermutation01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutation02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutation03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutation04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutation05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutation06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstInt32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Float32                             //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns the result for the first `Float32` tests.
    ///
    def resultForFirstFloat32(n: Int32): Vector[(Float32, Float32, Float32, Float32)] = {
        Vector.range(0, n - 2) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32, x + 2f32, x + 3f32))
    }

    @Test
    def testPermutationFloat32_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat32_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat32_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat32_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat32_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat32_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat32) |> Vector.map(x -> (x, x + 1f32)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstFloat32(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Float64                             //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns the result for the first `Float64` tests.
    ///
    def resultForFirstFloat64(n: Int32): Vector[(Float64, Float64, Float64, Float64)] = {
        Vector.range(0, n - 2) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64, x + 2f64, x + 3f64))
    }

    @Test
    def testPermutationFloat64_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat64_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat64_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat64_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat64_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationFloat64_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toFloat64) |> Vector.map(x -> (x, x + 1f64)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstFloat64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Char                                //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns a character representation of the integer `v`.
    ///
    /// Crashes if not possible.
    ///
    def forceToChar(v: Int32): Char = match Char.forDigit(radix=16, v) {
        case Some(c) => c
        case None    => unreachable!()
    }

    ///
    /// Returns the result for the first `Char` tests.
    ///
    def resultForFirstChar(n: Int32): Vector[(Char, Char, Char, Char)] = {
        Vector.range(0, n - 2) |> Vector.map(x -> (x, x + 1, x + 2, x + 3)) |>
            Vector.map(match (v1, v2, v3, v4) -> (forceToChar(v1), forceToChar(v2), forceToChar(v3), forceToChar(v4)))
    }

    @Test
    def testPermutationChar_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationChar_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationChar_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationChar_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationChar_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationChar_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (forceToChar(x), forceToChar(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstChar(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Int64                               //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns the result for the first `Int64` tests.
    ///
    def resultForFirstInt64(n: Int32): Vector[(Int64, Int64, Int64, Int64)] = {
        Vector.range(0, n - 2) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64, x + 2i64, x + 3i64))
    }

    @Test
    def testPermutationInt64_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt64_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt64_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt64_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt64_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt64_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(Int32.toInt64) |> Vector.map(x -> (x, x + 1i64)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstInt64(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Int8                                //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Returns a `v` as an `Int8`.
    ///
    /// Crashes if not possible.
    ///
    def forceToInt8(v: Int32): Int8 = match Int32.tryToInt8(v) {
        case Some(n) => n
        case None    => unreachable!()
    }
    
    ///
    /// Returns the result for the first `Int8` tests.
    ///
    def resultForFirstInt8(n: Int32): Vector[(Int8, Int8, Int8, Int8)] = {
        Vector.range(0, n - 2) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8, x + 2i8, x + 3i8))
    }

    @Test
    def testPermutationInt8_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt8_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt8_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt8_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt8_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt8_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt8) |> Vector.map(x -> (x, x + 1i8)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstInt8(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with Int16                               //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Returns a `v` as an `Int16`.
    ///
    /// Crashes if not possible.
    ///
    def forceToInt16(v: Int32): Int16 = match Int32.tryToInt16(v) {
        case Some(n) => n
        case None    => unreachable!()
    }
    
    ///
    /// Returns the result for the first `Int16` tests.
    ///
    def resultForFirstInt16(n: Int32): Vector[(Int16, Int16, Int16, Int16)] = {
        Vector.range(0, n - 2) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16, x + 2i16, x + 3i16))
    }

    @Test
    def testPermutationInt16_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt16_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt16_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt16_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt16_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationInt16_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(forceToInt16) |> Vector.map(x -> (x, x + 1i16)) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstInt16(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with String                              //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns the result for the first `String` tests.
    ///
    def resultForFirstString(n: Int32): Vector[(String, String, String, String)] = {
        Vector.range(0, n - 2) |>
            Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1), Int32.toString(x + 2), Int32.toString(x + 3)))
    }

    @Test
    def testPermutationString_01(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), B(y, z), C(z, w).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationString_02(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- A(x, y), C(z, w), B(y, z).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationString_03(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), A(x, y), C(z, w).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationString_04(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- B(y, z), C(z, w), A(x, y).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationString_05(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), A(x, y), B(y, z).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }

    @Test
    def testPermutationString_06(): Bool = {
        let n = smallN();
        let f1 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into A/2;
        let f2 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into B/2;
        let f3 = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into C/2;
        let p = #{
            D(x, y, z, w) :- C(z, w), B(y, z), A(x, y).
        };
        resultForFirstString(n) `Assert.eq` (query f1, f2, f3, p select (x, y, z, w) from D(x, y, z, w))
    }


    /////////////////////////////////////////////////////////////////////////////
    // Test permuting body atom order with negation                            //
    /////////////////////////////////////////////////////////////////////////////

    def assertPathExample(n: Int32, solved: #{ ConstPath(Int32, Int32), D(Int32, Int32, Int32, Int32), Edge(Int32, Int32), NoPath(Int32, Int32), Node(Int32), OnlyPathsTo(Int32), Path(Int32, Int32), PathFrom(Int32), ReachLonelyNode(Int32) | t }): Bool = {
        let keepsEdges = Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) `Assert.eq` query solved select (x, y) from Edge(x, y);
        let computedPath = Vector.range(0, n) |> Vector.flatMap(x -> Vector.range(x + 1, n + 1) |> Vector.map(y -> (x, y)))
            `Assert.eq` query solved select (x, y) from Path(x, y);
        let foundMissingPaths = Vector.range(0, n + 1) |>
            Vector.flatMap(x -> Vector.range(0, x + 1) |> Vector.map(y -> (x, y))) |> Vector.sort
            `Assert.eq` query solved select (x, y) from NoPath(x, y);
        let foundVerticesWithPaths = Vector.range(0, n) `Assert.eq` query solved select (x) from PathFrom(x);
        let foundVerticesWithoutPaths = Vector#{n} `Assert.eq` query solved select (x) from OnlyPathsTo(x);
        let foundNodesReachingLast = Vector.range(0, n) `Assert.eq` query solved select (x) from ReachLonelyNode(x);
        keepsEdges and computedPath and foundMissingPaths and foundVerticesWithPaths and
            foundVerticesWithoutPaths and foundNodesReachingLast
    }

    def smallPathN(): Int32 = 5 * smallN()

    @Test
    def testPermutationNeg01(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(x, z), Path(z, y).
            NoPath(x, y)        :- Node(x), Node(y), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExample(n, solved)
    }

    @Test
    def testPermutationNeg02(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(x), Node(y), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExample(n, solved)
    }

    @Test
    def testPermutationNeg03(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(y), Node(x), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExample(n, solved)
    }

    @Test
    def testPermutationNeg04(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(y), Node(x), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- OnlyPathsTo(y), Path(x, y).
        };
        let solved = solve edge, p;
        assertPathExample(n, solved)
    }

    def assertPathExampleString(n: Int32, solved: #{ ConstPath(String, String), D(String, String, String, String), Edge(String, String), NoPath(String, String), Node(String), OnlyPathsTo(String), Path(String, String), PathFrom(String), ReachLonelyNode(String) | t }): Bool = {
        let keepsEdges = Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) |> Vector.sort
            `Assert.eq` query solved select (x, y) from Edge(x, y);
        let computedPath = Vector.range(0, n) |> Vector.flatMap(x -> Vector.range(x + 1, n + 1) |> Vector.map(y -> (Int32.toString(x), Int32.toString(y)))) |> Vector.sort
            `Assert.eq` query solved select (x, y) from Path(x, y);
        let foundMissingPaths = Vector.range(0, n + 1) |>
            Vector.flatMap(x -> Vector.range(0, x + 1) |> Vector.map(y -> (Int32.toString(x), Int32.toString(y)))) |> Vector.sort
            `Assert.eq` query solved select (x, y) from NoPath(x, y);
        let foundVerticesWithPaths = Vector.range(0, n) |> Vector.map(Int32.toString) |> Vector.sort
            `Assert.eq` query solved select (x) from PathFrom(x);
        let foundVerticesWithoutPaths = Vector#{Int32.toString(n)} |> Vector.sort
            `Assert.eq` query solved select (x) from OnlyPathsTo(x);
        let foundNodesReachingLast = Vector.range(0, n) |> Vector.map(Int32.toString) |> Vector.sort
            `Assert.eq` query solved select (x) from ReachLonelyNode(x);
        keepsEdges and computedPath and foundMissingPaths and foundVerticesWithPaths and
            foundVerticesWithoutPaths and foundNodesReachingLast
    }

    @Test
    def testPermutationNegString01(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(x, z), Path(z, y).
            NoPath(x, y)        :- Node(x), Node(y), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExampleString(n, solved)
    }

    @Test
    def testPermutationNegString02(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(x), Node(y), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExampleString(n, solved)
    }

    @Test
    def testPermutationNegString03(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (Int32.toString(x), Int32.toString(x + 1))) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(y), Node(x), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- Path(x, y), OnlyPathsTo(y).
        };
        let solved = solve edge, p;
        assertPathExampleString(n, solved)
    }

    @Test
    def testPermutationNegString04(): Bool = {
        let n = smallPathN();
        let edge = inject Vector.range(0, n) |> Vector.map(x -> (x, x + 1)) into Edge/2;
        let p = #{
            Node(x)             :- Edge(x, _).
            Node(x)             :- Edge(_, x).
            Path(x, y)          :- Edge(x, y).
            Path(x, y)          :- Path(z, y), Path(x, z).
            NoPath(x, y)        :- Node(y), Node(x), not Path(x, y).
            PathFrom(x)         :- Path(x, _).
            OnlyPathsTo(x)      :- Node(x), not PathFrom(x).
            ReachLonelyNode(x)  :- OnlyPathsTo(y), Path(x, y).
        };
        let solved = solve edge, p;
        assertPathExample(n, solved)
    }

}
