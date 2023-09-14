/*
 * Copyright 2022 Stephen Tetley
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

mod TestBigDecimal {

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = BigDecimal.min(1.0ff, 2.0ff) == 1.0ff

    @Test
    def min02(): Bool = BigDecimal.min(2.0ff, -1.0ff) == -1.0ff

    @Test
    def min03(): Bool = BigDecimal.min(-33.0ff, -66.0ff) == -66.0ff

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = BigDecimal.max(48.0ff, 49.0ff) == 49.0ff

    @Test
    def max02(): Bool = BigDecimal.max(4.0ff, -16.0ff) == 4.0ff

    @Test
    def max03(): Bool = BigDecimal.max(-34.0ff, -16.0ff) == -16.0ff

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def abs01(): Bool = BigDecimal.abs(0.0ff) == 0.0ff

    @Test
    def abs02(): Bool = BigDecimal.abs(10.1ff) == 10.1ff

    @Test
    def abs03(): Bool = BigDecimal.abs(-10.1ff) == 10.1ff

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dist01(): Bool = BigDecimal.dist(0.0ff, 0.0ff) == 0.0ff

    @Test
    def dist02(): Bool = BigDecimal.dist(10.1ff, 10.0ff) == 0.1ff

    @Test
    def dist03(): Bool = BigDecimal.dist(-10.1ff, -10.0ff) == 0.1ff

    @Test
    def dist04(): Bool = BigDecimal.dist(10.01ff, 10.00ff) == 0.01ff

    @Test
    def dist05(): Bool = BigDecimal.dist(-10.01ff, -10.00ff) == 0.01ff

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Bool = BigDecimal.compare(1.0ff, -1.0ff) == 1

    @Test
    def compare02(): Bool = BigDecimal.compare(-9.0ff, -11.0ff) == 1

    @Test
    def compare03(): Bool = BigDecimal.compare(1.0ff, 1.0ff) == 0

    @Test
    def compare04(): Bool = BigDecimal.compare(-1.0ff, -1.0ff) == 0

    @Test
    def compare05(): Bool = BigDecimal.compare(-1.0ff, 1.0ff) == -1

    @Test
    def compare06(): Bool = BigDecimal.compare(11.0ff, 21.0ff) == -1

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Bool = BigDecimal.signum(2.0ff) == 1

    @Test
    def signum02(): Bool = BigDecimal.signum(3.0ff) == 1

    @Test
    def signum03(): Bool = BigDecimal.signum(0.0ff) == 0

    @Test
    def signum04(): Bool = BigDecimal.signum(-2.0ff) == -1

    @Test
    def signum05(): Bool = BigDecimal.signum(-10.0ff) == -1

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = BigDecimal.pow(0.0ff, 0) == 1.0ff

    @Test
    def pow02(): Bool = BigDecimal.pow(0.0ff, 1) == 0.0ff

    @Test
    def pow03(): Bool = BigDecimal.pow(1.0ff, 0) == 1.0ff

    @Test
    def pow04(): Bool = BigDecimal.pow(1.0ff, 1) == 1.0ff

    @Test
    def pow05(): Bool = BigDecimal.pow(5.0ff, 2) == 25.0ff

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = BigDecimal.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = BigDecimal.fromString(" 1.0") == Some(1.0ff)

    @Test
    def fromString03(): Bool = BigDecimal.fromString("1.0 ") == Some(1.0ff)

    @Test
    def fromString04(): Bool = BigDecimal.fromString("0.0") == Some(0.0ff)

    @Test
    def fromString05(): Bool = BigDecimal.fromString("1.0") == Some(1.0ff)

    @Test
    def fromString06(): Bool = BigDecimal.fromString("-1.0") == Some(-1.0ff)

    @Test
    def fromString07(): Bool = BigDecimal.fromString("3.14") == Some(3.14ff)

    @Test
    def fromString08(): Bool = BigDecimal.fromString("-3.14") == Some(-3.14ff)

    /////////////////////////////////////////////////////////////////////////////
    // toPlainString                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toPlainString01(): Bool = BigDecimal.toPlainString(1.1ff) == "1.1"

    @Test
    def toPlainString02(): Bool = BigDecimal.toPlainString(1.01ff) == "1.01"

    @Test
    def toPlainString03(): Bool = BigDecimal.toPlainString(1.001ff) == "1.001"

    @Test
    def toPlainString04(): Bool = BigDecimal.toPlainString(1.000001ff) == "1.000001"

    /////////////////////////////////////////////////////////////////////////////
    // numericEquals                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numericEquals01(): Bool = BigDecimal.numericEquals(1.1ff, 1.100000ff)

    @Test
    def numericEquals02(): Bool = BigDecimal.numericEquals(1.01ff, 1.0100000ff)

    @Test
    def numericEquals03(): Bool = BigDecimal.numericEquals(1.001ff, 1.00100000ff)

    /////////////////////////////////////////////////////////////////////////////
    // scale                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scale01(): Bool = BigDecimal.scale(1.1ff) == 1

    @Test
    def scale02(): Bool = BigDecimal.scale(1.01ff) == 2

    /////////////////////////////////////////////////////////////////////////////
    // precision                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def precision01(): Bool = BigDecimal.precision(0.0ff) == 1

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ceil01(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.5ff), 1.0ff)

    @Test
    def ceil02(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.1ff), 1.0ff)

    @Test
    def ceil03(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(1.0ff), 1.0ff)

    @Test
    def ceil04(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.0ff), 0.0ff)

    @Test
    def ceil05(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(-0.1ff), 0.0ff)

    @Test
    def ceil06(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(-0.9ff), 0.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def floor01(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.5ff), 0.0ff)

    @Test
    def floor02(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.1ff), 0.0ff)

    @Test
    def floor03(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(1.0ff), 1.0ff)

    @Test
    def floor04(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.0ff), 0.0ff)

    @Test
    def floor05(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(-0.1ff), -1.0ff)

    @Test
    def floor06(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(-0.9ff), -1.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def round01(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.5ff), 0.0ff)

    @Test
    def round02(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.1ff), 0.0ff)

    @Test
    def round03(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(1.0ff), 1.0ff)

    @Test
    def round04(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.0ff), 0.0ff)

    @Test
    def round05(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(-0.1ff), 0.0ff)

    @Test
    def round06(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(-0.9ff), -1.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Bool = BigDecimal.tryToInt8(0.0ff) == Some(0i8)

    @Test
    def tryToInt802(): Bool = BigDecimal.tryToInt8(1.0ff) == Some(1i8)

    @Test
    def tryToInt803(): Bool = BigDecimal.tryToInt8(-1.0ff) == Some(-1i8)

    @Test
    def tryToInt804(): Bool = BigDecimal.tryToInt8(127.0ff) == Some(127i8)

    @Test
    def tryToInt805(): Bool = BigDecimal.tryToInt8(128.0ff) == None

    @Test
    def tryToInt806(): Bool = BigDecimal.tryToInt8(-128.0ff) == Some(-128i8)

    @Test
    def tryToInt807(): Bool = BigDecimal.tryToInt8(-129.0ff) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Bool = BigDecimal.tryToInt16(0.0ff) == Some(0i16)

    @Test
    def tryToInt1602(): Bool = BigDecimal.tryToInt16(1.0ff) == Some(1i16)

    @Test
    def tryToInt1603(): Bool = BigDecimal.tryToInt16(-1.0ff) == Some(-1i16)

    @Test
    def tryToInt1604(): Bool = BigDecimal.tryToInt16(32767.0ff) == Some(32767i16)

    @Test
    def tryToInt1605(): Bool = BigDecimal.tryToInt16(32768.0ff) == None

    @Test
    def tryToInt1606(): Bool = BigDecimal.tryToInt16(-32768.0ff) == Some(-32768i16)

    @Test
    def tryToInt1607(): Bool = BigDecimal.tryToInt16(-32769.0ff) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Bool = BigDecimal.tryToInt32(0.0ff) == Some(0)

    @Test
    def tryToInt3202(): Bool = BigDecimal.tryToInt32(1.0ff) == Some(1)

    @Test
    def tryToInt3203(): Bool = BigDecimal.tryToInt32(-1.0ff) == Some(-1)

    @Test
    def tryToInt3204(): Bool = BigDecimal.tryToInt32(2147483647.0ff) == Some(2147483647)

    @Test
    def tryToInt3205(): Bool = BigDecimal.tryToInt32(2147483648.0ff) == None

    @Test
    def tryToInt3206(): Bool = BigDecimal.tryToInt32(-2147483648.0ff) == Some(-2147483648)

    @Test
    def tryToInt3207(): Bool = BigDecimal.tryToInt32(-2147483649.0ff) == None


    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Bool = BigDecimal.tryToInt64(0.0ff) == Some(0i64)

    @Test
    def tryToInt6402(): Bool = BigDecimal.tryToInt64(1.0ff) == Some(1i64)

    @Test
    def tryToInt6403(): Bool = BigDecimal.tryToInt64(-1.0ff) == Some(-1i64)

    @Test
    def tryToInt6404(): Bool = BigDecimal.tryToInt64(9223372036854775807.0ff) == Some(9223372036854775807i64)

    @Test
    def tryToInt6405(): Bool = BigDecimal.tryToInt64(9223372036854780000.0ff) == None        // Out of range

    @Test
    def tryToInt6406(): Bool = BigDecimal.tryToInt64(-9223372036854775808.0ff) == Some(-9223372036854775808i64)

    @Test
    def tryToInt6407(): Bool = BigDecimal.tryToInt64(-9223372036854780000.0ff) == None       // Out of range

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Bool = BigDecimal.toBigInt(0.0ff) == 0ii

    @Test
    def toBigInt02(): Bool = BigDecimal.toBigInt(1.0ff) == 1ii

    @Test
    def toBigInt03(): Bool = BigDecimal.toBigInt(-1.0ff) == -1ii

    @Test
    def toBigInt04(): Bool = BigDecimal.toBigInt(9223372036854775807.0ff) == 9223372036854775807ii

    @Test
    def toBigInt05(): Bool = BigDecimal.toBigInt(9223372036854780000.0ff) == 9223372036854780000ii

    @Test
    def toBigInt06(): Bool = BigDecimal.toBigInt(-9223372036854775808.0ff) == -9223372036854775808ii

    @Test
    def toBigInt07(): Bool = BigDecimal.toBigInt(-9223372036854780000.0ff) == -9223372036854780000ii

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat3201(): Bool = BigDecimal.tryToFloat32(0.0ff) == Some(0.0f32)

    @Test
    def tryToFloat3202(): Bool = BigDecimal.tryToFloat32(1.0ff) == Some(1.0f32)

    @Test
    def tryToFloat3203(): Bool = BigDecimal.tryToFloat32(-1.0ff) == Some(-1.0f32)

    @Test
    def tryToFloat3204(): Bool = match BigDecimal.tryToFloat32(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f32 and d <= 123456800.0f32       // Allow imprecision
    }

    @Test
    def tryToFloat3205(): Bool = match BigDecimal.tryToFloat32(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f32 and d >= -123456800.0f32     // Allow imprecision
    }

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat6401(): Bool = BigDecimal.tryToFloat64(0.0ff) == Some(0.0f64)

    @Test
    def tryToFloat6402(): Bool = BigDecimal.tryToFloat64(1.0ff) == Some(1.0f64)

    @Test
    def tryToFloat6403(): Bool = BigDecimal.tryToFloat64(-1.0ff) == Some(-1.0f64)

    @Test
    def tryToFloat6404(): Bool = match BigDecimal.tryToFloat64(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f64 and d <= 123456800.0f64       // Allow imprecision
    }

    @Test
    def tryToFloat6405(): Bool = match BigDecimal.tryToFloat64(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f64 and d >= -123456800.0f64     // Allow imprecision
    }

}
