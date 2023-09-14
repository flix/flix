/*
 * Copyright 2017 Liam Palmer
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

mod TestFloat64 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool = mul(1.0f64, 1.0f64) == 1.0f64

    @Test
    def mul02(): Bool = mul(17.0f64, 0.0f64) == 0.0f64

    @Test
    def mul03(): Bool = mul(0.0f64, 32.0f64) == 0.0f64

    @Test
    def mul04(): Bool = mul(123.123f64, 2.0f64) == 246.246f64

    @Test
    def mul05(): Bool = mul(5.0f64, 223.0f64) == 1115.0f64

    @Test
    def mul06(): Bool = mul(0.0f64, 0.0f64) == 0.0f64

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Bool = Float64.size() == 64

    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minExponent01(): Bool = Float64.minExponent() == -1022

    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxExponent01(): Bool = Float64.maxExponent() == 1023

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Bool = Float64.maxValue() == ((2.0f64 - Float64.pow(2.0f64, -52.0f64)) * Float64.pow(2.0f64, 1023.0f64))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Bool = Float64.minValue() == (-(2.0f64 - Float64.pow(2.0f64, -52.0f64)) * Float64.pow(2.0f64, 1023.0f64))

    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minPositiveValue01(): Bool = Float64.minPositiveValue() == Float64.pow(2.0f64, -1074.0f64)

    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nan01(): Bool = Float64.isNan(Float64.nan()) == true

    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def positiveInfinity01(): Bool = Float64.positiveInfinity() == (1.0f64 / 0.0f64)

    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def negativeInfinity01(): Bool = Float64.negativeInfinity() == (-1.0f64 / 0.0f64)

    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isFinite01(): Bool = Float64.isFinite(1.0f64) == true

    @Test
    def isFinite02(): Bool = Float64.isFinite(-1.0f64) == true

    @Test
    def isFinite03(): Bool = Float64.isFinite(Float64.minValue()) == true

    @Test
    def isFinite04(): Bool = Float64.isFinite(Float64.maxValue()) == true

    @Test
    def isFinite05(): Bool = Float64.isFinite(Float64.negativeInfinity()) == false

    @Test
    def isFinite06(): Bool = Float64.isFinite(Float64.positiveInfinity()) == false

    @Test
    def isFinite07(): Bool = Float64.isFinite(Float64.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isInfinite01(): Bool = Float64.isInfinite(1.0f64) == false

    @Test
    def isInfinite02(): Bool = Float64.isInfinite(-1.0f64) == false

    @Test
    def isInfinite03(): Bool = Float64.isInfinite(Float64.minValue()) == false

    @Test
    def isInfinite04(): Bool = Float64.isInfinite(Float64.maxValue()) == false

    @Test
    def isInfinite05(): Bool = Float64.isInfinite(Float64.negativeInfinity()) == true

    @Test
    def isInfinite06(): Bool = Float64.isInfinite(Float64.positiveInfinity()) == true

    @Test
    def isInfinite07(): Bool = Float64.isInfinite(Float64.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isNan01(): Bool = Float64.isNan(Float64.nan()) == true

    @Test
    def isNan02(): Bool = Float64.isNan(0.0f64 / 0.0f64) == true

    @Test
    def isNan03(): Bool = Float64.isNan(Float64.positiveInfinity()) == false

    @Test
    def isNan04(): Bool = Float64.isNan(Float64.negativeInfinity()) == false

    @Test
    def isNan05(): Bool = Float64.isNan(1.0f64) == false

    @Test
    def isNan06(): Bool = Float64.isNan(-1.0f64) == false

    @Test
    def isNan07(): Bool = Float64.isNan(Float64.minValue()) == false

    @Test
    def isNan08(): Bool = Float64.isNan(Float64.maxValue()) == false

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = Float64.min(1.0f64, 2.0f64) == 1.0f64

    @Test
    def min02(): Bool = Float64.min(2.0f64, -1.0f64) == -1.0f64

    @Test
    def min03(): Bool = Float64.min(-33.0f64, -66.0f64) == -66.0f64

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = Float64.max(48.0f64, 49.0f64) == 49.0f64

    @Test
    def max02(): Bool = Float64.max(4.0f64, -16.0f64) == 4.0f64

    @Test
    def max03(): Bool = Float64.max(-34.0f64, -16.0f64) == -16.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Bool = Float64.toString(0.0f64) == "0.0"

    @Test
    def toString02(): Bool = Float64.toString(1.0f64) == "1.0"

    @Test
    def toString03(): Bool = Float64.toString(-1.0f64) == "-1.0"

    @Test
    def toString04(): Bool = Float64.toString(3.14f64) == "3.14"

    @Test
    def toString05(): Bool = Float64.toString(-3.14f64) == "-3.14"

    @Test
    def toString06(): Bool = Float64.toString(Float64.nan()) == "NaN"

    @Test
    def toString07(): Bool = Float64.toString(Float64.positiveInfinity()) == "Infinity"

    @Test
    def toString08(): Bool = Float64.toString(Float64.negativeInfinity()) == "-Infinity"

// TODO
//    /////////////////////////////////////////////////////////////////////////////
//    // toDecimalString                                                         //
//    /////////////////////////////////////////////////////////////////////////////
//    @Test
//    def toDecimalString01(): Bool = Float64.toDecimalString(2, 0.0f64) == "0"
//
//    @Test
//    def toDecimalString02(): Bool = Float64.toDecimalString(2, 3.14159f64) == "3.14"
//
//    @Test
//    def toDecimalString03(): Bool = Float64.toDecimalString(4, 3.14159f64) == "3.1416"
//
//    @Test
//    def toDecimalString04(): Bool = Float64.toDecimalString(2, Float64.nan()) == "NaN"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = Float64.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = Float64.fromString(" 1.0") == Some(1.0f64)

    @Test
    def fromString03(): Bool = Float64.fromString("1.0 ") == Some(1.0f64)

    @Test
    def fromString04(): Bool = Float64.fromString("0.0") == Some(0.0f64)

    @Test
    def fromString05(): Bool = Float64.fromString("1.0") == Some(1.0f64)

    @Test
    def fromString06(): Bool = Float64.fromString("-1.0") == Some(-1.0f64)

    @Test
    def fromString07(): Bool = Float64.fromString("3.14") == Some(3.14f64)

    @Test
    def fromString08(): Bool = Float64.fromString("-3.14") == Some(-3.14f64)

    @Test
    def fromString09(): Bool = match Float64.fromString("NaN") {
        case Some(x)  => Float64.isNan(x)
        case _ => false
    }

    @Test
    def fromString10(): Bool = Float64.fromString("Infinity") == Some(Float64.positiveInfinity())

    @Test
    def fromString11(): Bool = Float64.fromString("-Infinity") == Some(Float64.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Bool = Float64.tryToInt8(0.0f64) == Some(0i8)

    @Test
    def tryToInt802(): Bool = Float64.tryToInt8(1.0f64) == Some(1i8)

    @Test
    def tryToInt803(): Bool = Float64.tryToInt8(-1.0f64) == Some(-1i8)

    @Test
    def tryToInt804(): Bool = Float64.tryToInt8(127.0f64) == Some(127i8)

    @Test
    def tryToInt805(): Bool = Float64.tryToInt8(128.0f64) == None

    @Test
    def tryToInt806(): Bool = Float64.tryToInt8(-128.0f64) == Some(-128i8)

    @Test
    def tryToInt807(): Bool = Float64.tryToInt8(-129.0f64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Bool = Float64.tryToInt16(0.0f64) == Some(0i16)

    @Test
    def tryToInt1602(): Bool = Float64.tryToInt16(1.0f64) == Some(1i16)

    @Test
    def tryToInt1603(): Bool = Float64.tryToInt16(-1.0f64) == Some(-1i16)

    @Test
    def tryToInt1604(): Bool = Float64.tryToInt16(32767.0f64) == Some(32767i16)

    @Test
    def tryToInt1605(): Bool = Float64.tryToInt16(32768.0f64) == None

    @Test
    def tryToInt1606(): Bool = Float64.tryToInt16(-32768.0f64) == Some(-32768i16)

    @Test
    def tryToInt1607(): Bool = Float64.tryToInt16(-32769.0f64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Bool = Float64.tryToInt32(0.0f64) == Some(0)

    @Test
    def tryToInt3202(): Bool = Float64.tryToInt32(1.0f64) == Some(1)

    @Test
    def tryToInt3203(): Bool = Float64.tryToInt32(-1.0f64) == Some(-1)

    @Test
    def tryToInt3204(): Bool = Float64.tryToInt32(2147483647.0f64) != None

    @Test
    def tryToInt3205(): Bool = Float64.tryToInt32(2147483648.0f64) == None

    @Test
    def tryToInt3206(): Bool = Float64.tryToInt32(-2147483648.0f64) != None

    @Test
    def tryToInt3207(): Bool = Float64.tryToInt32(-2147483649.0f64) == None


    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Bool = Float64.tryToInt64(0.0f64) == Some(0i64)

    @Test
    def tryToInt6402(): Bool = Float64.tryToInt64(1.0f64) == Some(1i64)

    @Test
    def tryToInt6403(): Bool = Float64.tryToInt64(-1.0f64) == Some(-1i64)

    @Test
    def tryToInt6404(): Bool = Float64.tryToInt64(9223372036854775807.0f64) != None

    @Test
    def tryToInt6405(): Bool = Float64.tryToInt64(9223372036854780000.0f64) == None        // Allow some imprecision

    @Test
    def tryToInt6406(): Bool = Float64.tryToInt64(-9223372036854775808.0f64) != None

    @Test
    def tryToInt6407(): Bool = Float64.tryToInt64(-9223372036854780000.0f64) == None       // Allow some imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigInt                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigInt01(): Bool = Float64.tryToBigInt(0.0f64) == Some(0ii)

    @Test
    def tryToBigInt02(): Bool = Float64.tryToBigInt(1.0f64) == Some(1ii)

    @Test
    def tryToBigInt03(): Bool = Float64.tryToBigInt(-1.0f64) == Some(-1ii)

    @Test
    def tryToBigInt04(): Bool =
        let step = () -> Float64.maxValue() |> Float64.tryToBigInt;
        step() != None

    @Test
    def tryToBigInt05(): Bool =
        let step = () -> Float64.minValue() |> Float64.tryToBigInt;
        step() != None

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat3201(): Bool = Float64.tryToFloat32(0.0f64) == Some(0.0f32)

    @Test
    def tryToFloat3202(): Bool = Float64.tryToFloat32(1.0f64) == Some(1.0f32)

    @Test
    def tryToFloat3203(): Bool = Float64.tryToFloat32(-1.0f64) == Some(-1.0f32)

    @Test
    def tryToFloat3204(): Bool = match Float64.tryToFloat32(123456789.10f64) {
        case None => false
        case Some(d) => d >= 123456700.0f32 and d <= 123456800.0f32       // Allow imprecision
    }

    @Test
    def tryToFloat3205(): Bool = match Float64.tryToFloat32(-123456789.10f64) {
        case None => false
        case Some(d) => d <= -123456700.0f32 and d >= -123456800.0f32     // Allow imprecision
    }

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigDecimal                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigDecimal01(): Bool = Float64.tryToBigDecimal(0.0f64) == Some(0.0ff)

    @Test
    def tryToBigDecimal02(): Bool = Float64.tryToBigDecimal(1.0f64) == Some(1.0ff)

    @Test
    def tryToBigDecimal03(): Bool = Float64.tryToBigDecimal(-1.0f64) == Some(-1.0ff)

    @Test
    def tryToBigDecimal04(): Bool = match Float64.tryToBigDecimal(123456789.10f64) {
        case None => false
        case Some(d) => d >= 123456700.0ff and d <= 123456800.0ff       // Allow imprecision
    }

    @Test
    def tryToBigDecimal05(): Bool = match Float64.tryToBigDecimal(-123456789.10f64) {
        case None => false
        case Some(d) => d <= -123456700.0ff and d >= -123456800.0ff     // Allow imprecision
    }

    @Test
    def tryToBigDecimal06(): Bool =
        match Float64.tryToBigDecimal(Float64.nan()) {
            case Some(_) => false
            case None    => true
        }

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Bool = Float64.clampToInt8(0.0f64, -100i8, 100i8, -127i8) == 0i8

    @Test
    def clampToInt802(): Bool = Float64.clampToInt8(100.0f64, -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt803(): Bool = Float64.clampToInt8(101.0f64, -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt804(): Bool = Float64.clampToInt8(-100.0f64, -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt805(): Bool = Float64.clampToInt8(-101.0f64, -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt806(): Bool = Float64.clampToInt8(Float64.maxValue(), -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt807(): Bool = Float64.clampToInt8(Float64.minValue(), -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt808(): Bool = Float64.clampToInt8(Float64.nan(), -100i8, 100i8, -127i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Bool = Float64.clampToInt16(0.0f64, -100i16, 100i16, -127i16) == 0i16

    @Test
    def clampToInt1602(): Bool = Float64.clampToInt16(100.0f64, -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1603(): Bool = Float64.clampToInt16(101.0f64, -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1604(): Bool = Float64.clampToInt16(-100.0f64, -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1605(): Bool = Float64.clampToInt16(-101.0f64, -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1606(): Bool = Float64.clampToInt16(Float64.maxValue(), -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1607(): Bool = Float64.clampToInt16(Float64.minValue(), -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1608(): Bool = Float64.clampToInt16(Float64.nan(), -100i16, 100i16, -127i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt3201(): Bool = Float64.clampToInt32(0.0f64, -100i32, 100i32, -127i32) == 0i32

    @Test
    def clampToInt3202(): Bool = Float64.clampToInt32(100.0f64, -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3203(): Bool = Float64.clampToInt32(101.0f64, -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3204(): Bool = Float64.clampToInt32(-100.0f64, -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3205(): Bool = Float64.clampToInt32(-101.0f64, -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3206(): Bool = Float64.clampToInt32(Float64.maxValue(), -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3207(): Bool = Float64.clampToInt32(Float64.minValue(), -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3208(): Bool = Float64.clampToInt32(Float64.nan(), -100i32, 100i32, -127i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt6401(): Bool = Float64.clampToInt64(0.0f64, -100i64, 100i64, -127i64) == 0i64

    @Test
    def clampToInt6402(): Bool = Float64.clampToInt64(100.0f64, -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6403(): Bool = Float64.clampToInt64(101.0f64, -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6404(): Bool = Float64.clampToInt64(-100.0f64, -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6405(): Bool = Float64.clampToInt64(-101.0f64, -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6406(): Bool = Float64.clampToInt64(Float64.maxValue(), -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6407(): Bool = Float64.clampToInt64(Float64.minValue(), -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6408(): Bool = Float64.clampToInt64(Float64.nan(), -100i64, 100i64, -127i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat32                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToFloat3201(): Bool = Float64.clampToFloat32(0.0f64, -100.0f32, 100.0f32) == 0.0f32

    @Test
    def clampToFloat3202(): Bool = Float64.clampToFloat32(100.0f64, -100.0f32, 100.0f32) == 100.0f32

    @Test
    def clampToFloat3203(): Bool = Float64.clampToFloat32(101.0f64, -100.0f32, 100.0f32) == 100.0f32

    @Test
    def clampToFloat3204(): Bool = Float64.clampToFloat32(-100.0f64, -100.0f32, 100.0f32) == -100.0f32

    @Test
    def clampToFloat3205(): Bool = Float64.clampToFloat32(-101.0f64, -100.0f32, 100.0f32) == -100.0f32

    @Test
    def clampToFloat3206(): Bool = Float64.clampToFloat32(Float64.maxValue(), -100.0f32, 100.0f32) == 100.0f32

    @Test
    def clampToFloat3207(): Bool = Float64.clampToFloat32(Float64.minValue(), -100.0f32, 100.0f32) == -100.0f32

    @Test
    def clampToFloat3208(): Bool =
        let ans = Float64.clampToFloat32(Float64.nan(), -100.0f32, 100.0f32);
        Float32.isNan(ans) == true

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    def equalsEps(x: Float64, y: Float64, eps: Float64): Bool =
        let diff = Float64.abs(x - y);
        let x1 = Float64.abs(x);
        let y1 = Float64.abs(y);
        let largest = if (y1 > x1) y1 else x1;
        if (diff <= largest * eps) true else false

    @Test
    def abs01(): Bool =
        equalsEps(Float64.abs(0.0f64), 0.0f64, 0.001f64)

    @Test
    def abs02(): Bool =
        equalsEps(Float64.abs(10.1f64), 10.1f64, 0.001f64)

    @Test
    def abs03(): Bool =
        equalsEps(Float64.abs(-10.1f64), 10.1f64, 0.001f64)

    @Test
    def abs04(): Bool =
        Float64.abs(Float64.nan()) |> Float64.isNan

    @Test
    def abs05(): Bool =
        Float64.abs(Float64.positiveInfinity()) == Float64.positiveInfinity()

    @Test
    def abs06(): Bool =
        Float64.abs(Float64.negativeInfinity()) == Float64.positiveInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ceil01(): Bool =
        equalsEps(Float64.ceil(0.5f64), 1.0f64, 0.001f64)

    @Test
    def ceil02(): Bool =
        equalsEps(Float64.ceil(0.1f64), 1.0f64, 0.001f64)

    @Test
    def ceil03(): Bool =
        equalsEps(Float64.ceil(1.0f64), 1.0f64, 0.001f64)

    @Test
    def ceil04(): Bool =
        equalsEps(Float64.ceil(0.0f64), 0.0f64, 0.001f64)

    @Test
    def ceil05(): Bool =
        equalsEps(Float64.ceil(-0.1f64), 0.0f64, 0.001f64)

    @Test
    def ceil06(): Bool =
        equalsEps(Float64.ceil(-0.9f64), 0.0f64, 0.001f64)

    @Test
    def ceil07(): Bool =
        Float64.ceil(Float64.nan()) |> Float64.isNan

    @Test
    def ceil08(): Bool =
        Float64.ceil(Float64.positiveInfinity()) == Float64.positiveInfinity()

    @Test
    def ceil09(): Bool =
        Float64.ceil(Float64.negativeInfinity()) == Float64.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def floor01(): Bool =
        equalsEps(Float64.floor(0.5f64), 0.0f64, 0.001f64)

    @Test
    def floor02(): Bool =
        equalsEps(Float64.floor(0.1f64), 0.0f64, 0.001f64)

    @Test
    def floor03(): Bool =
        equalsEps(Float64.floor(1.0f64), 1.0f64, 0.001f64)

    @Test
    def floor04(): Bool =
        equalsEps(Float64.floor(0.0f64), 0.0f64, 0.001f64)

    @Test
    def floor05(): Bool =
        equalsEps(Float64.floor(-0.1f64), -1.0f64, 0.001f64)

    @Test
    def floor06(): Bool =
        equalsEps(Float64.floor(-0.9f64), -1.0f64, 0.001f64)

    @Test
    def floor07(): Bool =
        Float64.floor(Float64.nan()) |> Float64.isNan

    @Test
    def floor08(): Bool =
        Float64.floor(Float64.positiveInfinity()) == Float64.positiveInfinity()

    @Test
    def floor09(): Bool =
        Float64.floor(Float64.negativeInfinity()) == Float64.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def round01(): Bool =
        equalsEps(Float64.round(0.5f64), 0.0f64, 0.001f64)

    @Test
    def round02(): Bool =
        equalsEps(Float64.round(0.1f64), 0.0f64, 0.001f64)

    @Test
    def round03(): Bool =
        equalsEps(Float64.round(1.0f64), 1.0f64, 0.001f64)

    @Test
    def round04(): Bool =
        equalsEps(Float64.round(0.0f64), 0.0f64, 0.001f64)

    @Test
    def round05(): Bool =
        equalsEps(Float64.round(-0.1f64), 0.0f64, 0.001f64)

    @Test
    def round06(): Bool =
        equalsEps(Float64.round(-0.9f64), -1.0f64, 0.001f64)

    @Test
    def round07(): Bool =
        Float64.round(Float64.nan()) |> Float64.isNan

    @Test
    def round08(): Bool =
        Float64.round(Float64.positiveInfinity()) == Float64.positiveInfinity()

    @Test
    def round09(): Bool =
        Float64.round(Float64.negativeInfinity()) == Float64.negativeInfinity()

    @Test
    def round10(): Bool =
        equalsEps(Float64.round(1.5f64), 2.0f64, 0.001f64)

    @Test
    def round11(): Bool =
        equalsEps(Float64.round(2.5f64), 2.0f64, 0.001f64)

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = equalsEps(Float64.pow(0.0f64, 0.0f64), 1.0f64, 0.001f64)

    @Test
    def pow02(): Bool = equalsEps(Float64.pow(0.0f64, 1.0f64), 0.0f64, 0.001f64)

    @Test
    def pow03(): Bool = equalsEps(Float64.pow(1.0f64, 0.0f64), 1.0f64, 0.001f64)

    @Test
    def pow04(): Bool = equalsEps(Float64.pow(1.0f64, 1.0f64), 1.0f64, 0.001f64)

    @Test
    def pow05(): Bool = equalsEps(Float64.pow(5.0f64, 2.0f64), 25.0f64, 0.001f64)

    /////////////////////////////////////////////////////////////////////////////
    // exp                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def exp01(): Bool = equalsEps(Float64.exp(0.0f64), 1.0f64, 0.001f64)

    @Test
    def exp02(): Bool = equalsEps(Float64.exp(1.0f64), 2.718281f64, 0.001f64)

    @Test
    def exp03(): Bool = equalsEps(Float64.exp(5.0f64), 148.413159f64, 0.001f64)
    /////////////////////////////////////////////////////////////////////////////
    // doubleValue                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def doubleValue01(): Bool =
        let d = Float64.valueOf(0.0f64);
        Float64.doubleValue(d) == 0.0f64

    @Test
    def doubleValue02(): Bool =
        let d = Float64.valueOf(1.0f64);
        Float64.doubleValue(d) == 1.0f64

    @Test
    def doubleValue03(): Bool =
        let d = Float64.valueOf(-1.0f64);
        Float64.doubleValue(d) == -1.0f64

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        import java.lang.Double.equals(##java.lang.Object): Bool \ {};
        let d = Float64.valueOf(0.0f64);
        equals(d, checked_cast(Float64.valueOf(0.0f64)))

    @Test
    def valueOf02(): Bool =
        import java.lang.Double.equals(##java.lang.Object): Bool \ {};
        let d = Float64.valueOf(1.0f64);
        equals(d, checked_cast(Float64.valueOf(1.0f64)))

    @Test
    def valueOf03(): Bool =
        import java.lang.Double.equals(##java.lang.Object): Bool \ {};
        let d = Float64.valueOf(-1.0f64);
        equals(d, checked_cast(Float64.valueOf(-1.0f64)))

}
