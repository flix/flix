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
    @test
    def min01(): Bool = BigDecimal.min(1.0ff, 2.0ff) == 1.0ff

    @test
    def min02(): Bool = BigDecimal.min(2.0ff, -1.0ff) == -1.0ff

    @test
    def min03(): Bool = BigDecimal.min(-33.0ff, -66.0ff) == -66.0ff

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = BigDecimal.max(48.0ff, 49.0ff) == 49.0ff

    @test
    def max02(): Bool = BigDecimal.max(4.0ff, -16.0ff) == 4.0ff

    @test
    def max03(): Bool = BigDecimal.max(-34.0ff, -16.0ff) == -16.0ff

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def abs01(): Bool = BigDecimal.abs(0.0ff) == 0.0ff

    @test
    def abs02(): Bool = BigDecimal.abs(10.1ff) == 10.1ff

    @test
    def abs03(): Bool = BigDecimal.abs(-10.1ff) == 10.1ff

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dist01(): Bool = BigDecimal.dist(0.0ff, 0.0ff) == 0.0ff

    @test
    def dist02(): Bool = BigDecimal.dist(10.1ff, 10.0ff) == 0.1ff

    @test
    def dist03(): Bool = BigDecimal.dist(-10.1ff, -10.0ff) == 0.1ff

    @test
    def dist04(): Bool = BigDecimal.dist(10.01ff, 10.00ff) == 0.01ff

    @test
    def dist05(): Bool = BigDecimal.dist(-10.01ff, -10.00ff) == 0.01ff

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = BigDecimal.compare(1.0ff, -1.0ff) == 1

    @test
    def compare02(): Bool = BigDecimal.compare(-9.0ff, -11.0ff) == 1

    @test
    def compare03(): Bool = BigDecimal.compare(1.0ff, 1.0ff) == 0

    @test
    def compare04(): Bool = BigDecimal.compare(-1.0ff, -1.0ff) == 0

    @test
    def compare05(): Bool = BigDecimal.compare(-1.0ff, 1.0ff) == -1

    @test
    def compare06(): Bool = BigDecimal.compare(11.0ff, 21.0ff) == -1

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = BigDecimal.signum(2.0ff) == 1

    @test
    def signum02(): Bool = BigDecimal.signum(3.0ff) == 1

    @test
    def signum03(): Bool = BigDecimal.signum(0.0ff) == 0

    @test
    def signum04(): Bool = BigDecimal.signum(-2.0ff) == -1

    @test
    def signum05(): Bool = BigDecimal.signum(-10.0ff) == -1

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def pow01(): Bool = BigDecimal.pow(0.0ff, 0) == 1.0ff

    @test
    def pow02(): Bool = BigDecimal.pow(0.0ff, 1) == 0.0ff

    @test
    def pow03(): Bool = BigDecimal.pow(1.0ff, 0) == 1.0ff

    @test
    def pow04(): Bool = BigDecimal.pow(1.0ff, 1) == 1.0ff

    @test
    def pow05(): Bool = BigDecimal.pow(5.0ff, 2) == 25.0ff

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = BigDecimal.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = BigDecimal.fromString(" 1.0") == Some(1.0ff)

    @test
    def fromString03(): Bool = BigDecimal.fromString("1.0 ") == Some(1.0ff)

    @test
    def fromString04(): Bool = BigDecimal.fromString("0.0") == Some(0.0ff)

    @test
    def fromString05(): Bool = BigDecimal.fromString("1.0") == Some(1.0ff)

    @test
    def fromString06(): Bool = BigDecimal.fromString("-1.0") == Some(-1.0ff)

    @test
    def fromString07(): Bool = BigDecimal.fromString("3.14") == Some(3.14ff)

    @test
    def fromString08(): Bool = BigDecimal.fromString("-3.14") == Some(-3.14ff)

    /////////////////////////////////////////////////////////////////////////////
    // toPlainString                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toPlainString01(): Bool = BigDecimal.toPlainString(1.1ff) == "1.1"

    @test
    def toPlainString02(): Bool = BigDecimal.toPlainString(1.01ff) == "1.01"

    @test
    def toPlainString03(): Bool = BigDecimal.toPlainString(1.001ff) == "1.001"

    @test
    def toPlainString04(): Bool = BigDecimal.toPlainString(1.000001ff) == "1.000001"

    /////////////////////////////////////////////////////////////////////////////
    // numericEquals                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numericEquals01(): Bool = BigDecimal.numericEquals(1.1ff, 1.100000ff)

    @test
    def numericEquals02(): Bool = BigDecimal.numericEquals(1.01ff, 1.0100000ff)

    @test
    def numericEquals03(): Bool = BigDecimal.numericEquals(1.001ff, 1.00100000ff)

    /////////////////////////////////////////////////////////////////////////////
    // scale                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def scale01(): Bool = BigDecimal.scale(1.1ff) == 1

    @test
    def scale02(): Bool = BigDecimal.scale(1.01ff) == 2

    /////////////////////////////////////////////////////////////////////////////
    // precision                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def precision01(): Bool = BigDecimal.precision(0.0ff) == 1

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def ceil01(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.5ff), 1.0ff)

    @test
    def ceil02(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.1ff), 1.0ff)

    @test
    def ceil03(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(1.0ff), 1.0ff)

    @test
    def ceil04(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(0.0ff), 0.0ff)

    @test
    def ceil05(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(-0.1ff), 0.0ff)

    @test
    def ceil06(): Bool =
        BigDecimal.numericEquals(BigDecimal.ceil(-0.9ff), 0.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def floor01(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.5ff), 0.0ff)

    @test
    def floor02(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.1ff), 0.0ff)

    @test
    def floor03(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(1.0ff), 1.0ff)

    @test
    def floor04(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(0.0ff), 0.0ff)

    @test
    def floor05(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(-0.1ff), -1.0ff)

    @test
    def floor06(): Bool =
        BigDecimal.numericEquals(BigDecimal.floor(-0.9ff), -1.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def round01(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.5ff), 0.0ff)

    @test
    def round02(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.1ff), 0.0ff)

    @test
    def round03(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(1.0ff), 1.0ff)

    @test
    def round04(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(0.0ff), 0.0ff)

    @test
    def round05(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(-0.1ff), 0.0ff)

    @test
    def round06(): Bool =
        BigDecimal.numericEquals(BigDecimal.round(-0.9ff), -1.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = BigDecimal.tryToInt8(0.0ff) == Some(0i8)

    @test
    def tryToInt802(): Bool = BigDecimal.tryToInt8(1.0ff) == Some(1i8)

    @test
    def tryToInt803(): Bool = BigDecimal.tryToInt8(-1.0ff) == Some(-1i8)

    @test
    def tryToInt804(): Bool = BigDecimal.tryToInt8(127.0ff) == Some(127i8)

    @test
    def tryToInt805(): Bool = BigDecimal.tryToInt8(128.0ff) == None

    @test
    def tryToInt806(): Bool = BigDecimal.tryToInt8(-128.0ff) == Some(-128i8)

    @test
    def tryToInt807(): Bool = BigDecimal.tryToInt8(-129.0ff) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = BigDecimal.tryToInt16(0.0ff) == Some(0i16)

    @test
    def tryToInt1602(): Bool = BigDecimal.tryToInt16(1.0ff) == Some(1i16)

    @test
    def tryToInt1603(): Bool = BigDecimal.tryToInt16(-1.0ff) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = BigDecimal.tryToInt16(32767.0ff) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = BigDecimal.tryToInt16(32768.0ff) == None

    @test
    def tryToInt1606(): Bool = BigDecimal.tryToInt16(-32768.0ff) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = BigDecimal.tryToInt16(-32769.0ff) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt3201(): Bool = BigDecimal.tryToInt32(0.0ff) == Some(0)

    @test
    def tryToInt3202(): Bool = BigDecimal.tryToInt32(1.0ff) == Some(1)

    @test
    def tryToInt3203(): Bool = BigDecimal.tryToInt32(-1.0ff) == Some(-1)

    @test
    def tryToInt3204(): Bool = BigDecimal.tryToInt32(2147483647.0ff) == Some(2147483647)

    @test
    def tryToInt3205(): Bool = BigDecimal.tryToInt32(2147483648.0ff) == None

    @test
    def tryToInt3206(): Bool = BigDecimal.tryToInt32(-2147483648.0ff) == Some(-2147483648)

    @test
    def tryToInt3207(): Bool = BigDecimal.tryToInt32(-2147483649.0ff) == None


    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt6401(): Bool = BigDecimal.tryToInt64(0.0ff) == Some(0i64)

    @test
    def tryToInt6402(): Bool = BigDecimal.tryToInt64(1.0ff) == Some(1i64)

    @test
    def tryToInt6403(): Bool = BigDecimal.tryToInt64(-1.0ff) == Some(-1i64)

    @test
    def tryToInt6404(): Bool = BigDecimal.tryToInt64(9223372036854775807.0ff) == Some(9223372036854775807i64)

    @test
    def tryToInt6405(): Bool = BigDecimal.tryToInt64(9223372036854780000.0ff) == None        // Out of range

    @test
    def tryToInt6406(): Bool = BigDecimal.tryToInt64(-9223372036854775808.0ff) == Some(-9223372036854775808i64)

    @test
    def tryToInt6407(): Bool = BigDecimal.tryToInt64(-9223372036854780000.0ff) == None       // Out of range

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = BigDecimal.toBigInt(0.0ff) == 0ii

    @test
    def toBigInt02(): Bool = BigDecimal.toBigInt(1.0ff) == 1ii

    @test
    def toBigInt03(): Bool = BigDecimal.toBigInt(-1.0ff) == -1ii

    @test
    def toBigInt04(): Bool = BigDecimal.toBigInt(9223372036854775807.0ff) == 9223372036854775807ii

    @test
    def toBigInt05(): Bool = BigDecimal.toBigInt(9223372036854780000.0ff) == 9223372036854780000ii

    @test
    def toBigInt06(): Bool = BigDecimal.toBigInt(-9223372036854775808.0ff) == -9223372036854775808ii

    @test
    def toBigInt07(): Bool = BigDecimal.toBigInt(-9223372036854780000.0ff) == -9223372036854780000ii

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToFloat3201(): Bool = BigDecimal.tryToFloat32(0.0ff) == Some(0.0f32)

    @test
    def tryToFloat3202(): Bool = BigDecimal.tryToFloat32(1.0ff) == Some(1.0f32)

    @test
    def tryToFloat3203(): Bool = BigDecimal.tryToFloat32(-1.0ff) == Some(-1.0f32)

    @test
    def tryToFloat3204(): Bool = match BigDecimal.tryToFloat32(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f32 and d <= 123456800.0f32       // Allow imprecision
    }

    @test
    def tryToFloat3205(): Bool = match BigDecimal.tryToFloat32(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f32 and d >= -123456800.0f32     // Allow imprecision
    }

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToFloat6401(): Bool = BigDecimal.tryToFloat64(0.0ff) == Some(0.0f64)

    @test
    def tryToFloat6402(): Bool = BigDecimal.tryToFloat64(1.0ff) == Some(1.0f64)

    @test
    def tryToFloat6403(): Bool = BigDecimal.tryToFloat64(-1.0ff) == Some(-1.0f64)

    @test
    def tryToFloat6404(): Bool = match BigDecimal.tryToFloat64(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f64 and d <= 123456800.0f64       // Allow imprecision
    }

    @test
    def tryToFloat6405(): Bool = match BigDecimal.tryToFloat64(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f64 and d >= -123456800.0f64     // Allow imprecision
    }

}
