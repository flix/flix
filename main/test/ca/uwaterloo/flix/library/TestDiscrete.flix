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

mod TestDiscrete {

    use Assert.{assertEq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // succ                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def succInt8_01(): Unit \ Assert = assertEq(expected = 6i8, Discrete.succ(5i8))

    @Test
    def succInt8_02(): Unit \ Assert = assertEq(expected = 0i8, Discrete.succ(-1i8))

    @Test
    def succInt16_01(): Unit \ Assert = assertEq(expected = 6i16, Discrete.succ(5i16))

    @Test
    def succInt16_02(): Unit \ Assert = assertEq(expected = 0i16, Discrete.succ(-1i16))

    @Test
    def succInt32_01(): Unit \ Assert = assertEq(expected = 6, Discrete.succ(5))

    @Test
    def succInt32_02(): Unit \ Assert = assertEq(expected = 0, Discrete.succ(-1))

    @Test
    def succInt64_01(): Unit \ Assert = assertEq(expected = 6i64, Discrete.succ(5i64))

    @Test
    def succInt64_02(): Unit \ Assert = assertEq(expected = 0i64, Discrete.succ(-1i64))

    @Test
    def succBigInt_01(): Unit \ Assert = assertEq(expected = 6ii, Discrete.succ(5ii))

    @Test
    def succBigInt_02(): Unit \ Assert = assertEq(expected = 0ii, Discrete.succ(-1ii))

    /////////////////////////////////////////////////////////////////////////////
    // prev                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def prevInt8_01(): Unit \ Assert = assertEq(expected = 4i8, Discrete.prev(5i8))

    @Test
    def prevInt8_02(): Unit \ Assert = assertEq(expected = -1i8, Discrete.prev(0i8))

    @Test
    def prevInt16_01(): Unit \ Assert = assertEq(expected = 4i16, Discrete.prev(5i16))

    @Test
    def prevInt16_02(): Unit \ Assert = assertEq(expected = -1i16, Discrete.prev(0i16))

    @Test
    def prevInt32_01(): Unit \ Assert = assertEq(expected = 4, Discrete.prev(5))

    @Test
    def prevInt32_02(): Unit \ Assert = assertEq(expected = -1, Discrete.prev(0))

    @Test
    def prevInt64_01(): Unit \ Assert = assertEq(expected = 4i64, Discrete.prev(5i64))

    @Test
    def prevInt64_02(): Unit \ Assert = assertEq(expected = -1i64, Discrete.prev(0i64))

    @Test
    def prevBigInt_01(): Unit \ Assert = assertEq(expected = 4ii, Discrete.prev(5ii))

    @Test
    def prevBigInt_02(): Unit \ Assert = assertEq(expected = -1ii, Discrete.prev(0ii))

    /////////////////////////////////////////////////////////////////////////////
    // prev(succ(x)) == x                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def roundtripInt8_01(): Unit \ Assert = assertEq(expected = 42i8, Discrete.prev(Discrete.succ(42i8)))

    @Test
    def roundtripInt16_01(): Unit \ Assert = assertEq(expected = 42i16, Discrete.prev(Discrete.succ(42i16)))

    @Test
    def roundtripInt32_01(): Unit \ Assert = assertEq(expected = 42, Discrete.prev(Discrete.succ(42)))

    @Test
    def roundtripInt64_01(): Unit \ Assert = assertEq(expected = 42i64, Discrete.prev(Discrete.succ(42i64)))

    @Test
    def roundtripBigInt_01(): Unit \ Assert = assertEq(expected = 42ii, Discrete.prev(Discrete.succ(42ii)))

    /////////////////////////////////////////////////////////////////////////////
    // wraparound (bounded types)                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def wraparoundInt8_01(): Unit \ Assert = assertEq(expected = Int8.minValue(), Discrete.succ(Int8.maxValue()))

    @Test
    def wraparoundInt8_02(): Unit \ Assert = assertEq(expected = Int8.maxValue(), Discrete.prev(Int8.minValue()))

    @Test
    def wraparoundInt16_01(): Unit \ Assert = assertEq(expected = Int16.minValue(), Discrete.succ(Int16.maxValue()))

    @Test
    def wraparoundInt16_02(): Unit \ Assert = assertEq(expected = Int16.maxValue(), Discrete.prev(Int16.minValue()))

    @Test
    def wraparoundInt32_01(): Unit \ Assert = assertEq(expected = Int32.minValue(), Discrete.succ(Int32.maxValue()))

    @Test
    def wraparoundInt32_02(): Unit \ Assert = assertEq(expected = Int32.maxValue(), Discrete.prev(Int32.minValue()))

    @Test
    def wraparoundInt64_01(): Unit \ Assert = assertEq(expected = Int64.minValue(), Discrete.succ(Int64.maxValue()))

    @Test
    def wraparoundInt64_02(): Unit \ Assert = assertEq(expected = Int64.maxValue(), Discrete.prev(Int64.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // adjacent                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjacent01(): Unit \ Assert = assertTrue(Discrete.adjacent(1, 2))

    @Test
    def adjacent02(): Unit \ Assert = assertTrue(Discrete.adjacent(2, 1))

    @Test
    def adjacent03(): Unit \ Assert = assertFalse(Discrete.adjacent(1, 3))

    @Test
    def adjacent04(): Unit \ Assert = assertFalse(Discrete.adjacent(1, 1))

    @Test
    def adjacent05(): Unit \ Assert = assertTrue(Discrete.adjacent(-1, 0))

    @Test
    def adjacent06(): Unit \ Assert = assertTrue(Discrete.adjacent(5i8, 6i8))

    @Test
    def adjacent07(): Unit \ Assert = assertFalse(Discrete.adjacent(5i8, 5i8))

    @Test
    def adjacent08(): Unit \ Assert = assertTrue(Discrete.adjacent(100ii, 99ii))

    @Test
    def adjacent09(): Unit \ Assert = assertFalse(Discrete.adjacent(100ii, 102ii))

}
