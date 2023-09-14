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

mod TestFloat32 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool = mul(1.0f32, 1.0f32) == 1.0f32

    @Test
    def mul02(): Bool = mul(17.0f32, 0.0f32) == 0.0f32

    @Test
    def mul03(): Bool = mul(0.0f32, 32.0f32) == 0.0f32

    @Test
    def mul04(): Bool = mul(123.123f32, 2.0f32) == 246.246f32

    @Test
    def mul05(): Bool = mul(5.0f32, 223.0f32) == 1115.0f32

    @Test
    def mul06(): Bool = mul(0.0f32, 0.0f32) == 0.0f32

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Bool = Float32.size() == 32

    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minExponent01(): Bool = Float32.minExponent() == -126

    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxExponent01(): Bool = Float32.maxExponent() == 127

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Bool = Float32.maxValue() == ((2.0f32 - Float32.pow(2.0f32, -23.0f32)) * Float32.pow(2.0f32, 127.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Bool = Float32.minValue() == (-(2.0f32 - Float32.pow(2.0f32, -23.0f32)) * Float32.pow(2.0f32, 127.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minPositiveValue01(): Bool = Float32.minPositiveValue() == Float32.pow(2.0f32, -149.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nan01(): Bool = Float32.isNan(Float32.nan()) == true

    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def positiveInfinity01(): Bool = Float32.positiveInfinity() == (1.0f32 / 0.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def negativeInfinity01(): Bool = Float32.negativeInfinity() == (-1.0f32 / 0.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isFinite01(): Bool = Float32.isFinite(1.0f32) == true

    @Test
    def isFinite02(): Bool = Float32.isFinite(-1.0f32) == true

    @Test
    def isFinite03(): Bool = Float32.isFinite(Float32.minValue()) == true

    @Test
    def isFinite04(): Bool = Float32.isFinite(Float32.maxValue()) == true

    @Test
    def isFinite05(): Bool = Float32.isFinite(Float32.negativeInfinity()) == false

    @Test
    def isFinite06(): Bool = Float32.isFinite(Float32.positiveInfinity()) == false

    @Test
    def isFinite07(): Bool = Float32.isFinite(Float32.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isInfinite01(): Bool = Float32.isInfinite(1.0f32) == false

    @Test
    def isInfinite02(): Bool = Float32.isInfinite(-1.0f32) == false

    @Test
    def isInfinite03(): Bool = Float32.isInfinite(Float32.minValue()) == false

    @Test
    def isInfinite04(): Bool = Float32.isInfinite(Float32.maxValue()) == false

    @Test
    def isInfinite05(): Bool = Float32.isInfinite(Float32.negativeInfinity()) == true

    @Test
    def isInfinite06(): Bool = Float32.isInfinite(Float32.positiveInfinity()) == true

    @Test
    def isInfinite07(): Bool = Float32.isInfinite(Float32.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isNan01(): Bool = Float32.isNan(Float32.nan()) == true

    @Test
    def isNan02(): Bool = Float32.isNan(0.0f32 / 0.0f32) == true

    @Test
    def isNan03(): Bool = Float32.isNan(Float32.positiveInfinity()) == false

    @Test
    def isNan04(): Bool = Float32.isNan(Float32.negativeInfinity()) == false

    @Test
    def isNan05(): Bool = Float32.isNan(1.0f32) == false

    @Test
    def isNan06(): Bool = Float32.isNan(-1.0f32) == false

    @Test
    def isNan07(): Bool = Float32.isNan(Float32.minValue()) == false

    @Test
    def isNan08(): Bool = Float32.isNan(Float32.maxValue()) == false

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = Float32.min(1.0f32, 2.0f32) == 1.0f32

    @Test
    def min02(): Bool = Float32.min(2.0f32, -1.0f32) == -1.0f32

    @Test
    def min03(): Bool = Float32.min(-33.0f32, -66.0f32) == -66.0f32

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = Float32.max(48.0f32, 49.0f32) == 49.0f32

    @Test
    def max02(): Bool = Float32.max(4.0f32, -16.0f32) == 4.0f32

    @Test
    def max03(): Bool = Float32.max(-34.0f32, -16.0f32) == -16.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Bool = Float32.toString(0.0f32) == "0.0"

    @Test
    def toString02(): Bool = Float32.toString(1.0f32) == "1.0"

    @Test
    def toString03(): Bool = Float32.toString(-1.0f32) == "-1.0"

    @Test
    def toString04(): Bool = Float32.toString(3.14f32) == "3.14"

    @Test
    def toString05(): Bool = Float32.toString(-3.14f32) == "-3.14"

    @Test
    def toString06(): Bool = Float32.toString(Float32.nan()) == "NaN"

    @Test
    def toString07(): Bool = Float32.toString(Float32.positiveInfinity()) == "Infinity"

    @Test
    def toString08(): Bool = Float32.toString(Float32.negativeInfinity()) == "-Infinity"

// TODO
    /////////////////////////////////////////////////////////////////////////////
    // toDecimalString                                                         //
    /////////////////////////////////////////////////////////////////////////////
//    @Test
//    def toDecimalString01(): Bool = Float32.toDecimalString(2, 0.0f32) == "0"
//
//    @Test
//    def toDecimalString02(): Bool = Float32.toDecimalString(2, 3.14159f32) == "3.14"
//
//    @Test
//    def toDecimalString03(): Bool = Float32.toDecimalString(4, 3.14159f32) == "3.1416"
//
//    @Test
//    def toDecimalString04(): Bool = Float32.toDecimalString(2, Float32.nan()) == "NaN"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = Float32.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = Float32.fromString(" 1.0") == Some(1.0f32)

    @Test
    def fromString03(): Bool = Float32.fromString("1.0 ") == Some(1.0f32)

    @Test
    def fromString04(): Bool = Float32.fromString("0.0") == Some(0.0f32)

    @Test
    def fromString05(): Bool = Float32.fromString("1.0") == Some(1.0f32)

    @Test
    def fromString06(): Bool = Float32.fromString("-1.0") == Some(-1.0f32)

    @Test
    def fromString07(): Bool = Float32.fromString("3.14") == Some(3.14f32)

    @Test
    def fromString08(): Bool = Float32.fromString("-3.14") == Some(-3.14f32)

    @Test
    def fromString09(): Bool = match Float32.fromString("NaN") {
        case Some(x)  => Float32.isNan(x)
        case _ => false
    }

    @Test
    def fromString10(): Bool = Float32.fromString("Infinity") == Some(Float32.positiveInfinity())

    @Test
    def fromString11(): Bool = Float32.fromString("-Infinity") == Some(Float32.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Bool = Float32.tryToInt8(0.0f32) == Some(0i8)

    @Test
    def tryToInt802(): Bool = Float32.tryToInt8(1.0f32) == Some(1i8)

    @Test
    def tryToInt803(): Bool = Float32.tryToInt8(-1.0f32) == Some(-1i8)

    @Test
    def tryToInt804(): Bool = Float32.tryToInt8(127.0f32) == Some(127i8)

    @Test
    def tryToInt805(): Bool = Float32.tryToInt8(128.0f32) == None

    @Test
    def tryToInt806(): Bool = Float32.tryToInt8(-128.0f32) == Some(-128i8)

    @Test
    def tryToInt807(): Bool = Float32.tryToInt8(-129.0f32) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Bool = Float32.tryToInt16(0.0f32) == Some(0i16)

    @Test
    def tryToInt1602(): Bool = Float32.tryToInt16(1.0f32) == Some(1i16)

    @Test
    def tryToInt1603(): Bool = Float32.tryToInt16(-1.0f32) == Some(-1i16)

    @Test
    def tryToInt1604(): Bool = Float32.tryToInt16(32767.0f32) == Some(32767i16)

    @Test
    def tryToInt1605(): Bool = Float32.tryToInt16(32768.0f32) == None

    @Test
    def tryToInt1606(): Bool = Float32.tryToInt16(-32768.0f32) == Some(-32768i16)

    @Test
    def tryToInt1607(): Bool = Float32.tryToInt16(-32769.0f32) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Bool = Float32.tryToInt32(0.0f32) == Some(0)

    @Test
    def tryToInt3202(): Bool = Float32.tryToInt32(1.0f32) == Some(1)

    @Test
    def tryToInt3203(): Bool = Float32.tryToInt32(-1.0f32) == Some(-1)

    @Test
    def tryToInt3204(): Bool = Float32.tryToInt32(2147483647.0f32) != None

    @Test
    def tryToInt3205(): Bool = Float32.tryToInt32(2147484000.0f32) == None        // Allow imprecision

    @Test
    def tryToInt3206(): Bool = Float32.tryToInt32(-2147483648.0f32) != None

    @Test
    def tryToInt3207(): Bool = Float32.tryToInt32(-2147484000.0f32) == None       // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Bool = Float32.tryToInt64(0.0f32) == Some(0i64)

    @Test
    def tryToInt6402(): Bool = Float32.tryToInt64(1.0f32) == Some(1i64)

    @Test
    def tryToInt6403(): Bool = Float32.tryToInt64(-1.0f32) == Some(-1i64)

    @Test
    def tryToInt6404(): Bool = Float32.tryToInt64(9223372036854775807.0f32) != None

    @Test
    def tryToInt6405(): Bool = Float32.tryToInt64(9223373000000000000.0f32) == None        // Allow large imprecision

    @Test
    def tryToInt6406(): Bool = Float32.tryToInt64(-9223372036854775808.0f32) != None

    @Test
    def tryToInt6407(): Bool = Float32.tryToInt64(-9223373000000000000.0f32) == None       // Allow large imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigInt                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigInt01(): Bool = Float32.tryToBigInt(0.0f32) == Some(0ii)

    @Test
    def tryToBigInt02(): Bool = Float32.tryToBigInt(1.0f32) == Some(1ii)

    @Test
    def tryToBigInt03(): Bool = Float32.tryToBigInt(-1.0f32) == Some(-1ii)

    @Test
    def tryToBigInt04(): Bool =
        let step = () -> Float32.maxValue() |> Float32.tryToBigInt;
        step() != None

    @Test
    def tryToBigInt05(): Bool =
        let step = () -> Float32.minValue() |> Float32.tryToBigInt;
        step() != None

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Bool = Float32.toFloat64(0.0f32) == 0.0f64

    @Test
    def toFloat6402(): Bool = Float32.toFloat64(1.0f32) == 1.0f64

    @Test
    def toFloat6403(): Bool = Float32.toFloat64(-1.0f32) == -1.0f64

    @Test
    def toFloat6404(): Bool =
        let d = Float32.toFloat64(123456789.10f32);
        (d >= 123456700.0f64 and d <= 123456800.0f64)       // Allow imprecision

    @Test
    def toFloat6405(): Bool =
        let d = Float32.toFloat64(-123456789.10f32);
        ( d <= -123456700.0f64 and d >= -123456800.0f64)     // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigDecimal                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigDecimal01(): Bool = Float32.tryToBigDecimal(0.0f32) == Some(0.0ff)

    @Test
    def tryToBigDecimal02(): Bool = Float32.tryToBigDecimal(1.0f32) == Some(1.0ff)

    @Test
    def tryToBigDecimal03(): Bool = Float32.tryToBigDecimal(-1.0f32) == Some(-1.0ff)

    @Test
    def tryToBigDecimal04(): Bool =
        match Float32.tryToBigDecimal(123456789.10f32) {
            case Some(d) => (d >= 123456700.0ff and d <= 123456800.0ff)       // Allow imprecision
            case None    => false
        }

    @Test
    def tryToBigDecimal05(): Bool =
        match Float32.tryToBigDecimal(-123456789.10f32) {
            case Some(d) => ( d <= -123456700.0ff and d >= -123456800.0ff)     // Allow imprecision
            case None    => false
        }

    @Test
    def tryToBigDecimal06(): Bool =
        match Float32.tryToBigDecimal(Float32.nan()) {
            case Some(_) => false
            case None    => true
        }

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Bool = Float32.clampToInt8(0.0f32, -100i8, 100i8, -127i8) == 0i8

    @Test
    def clampToInt802(): Bool = Float32.clampToInt8(100.0f32, -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt803(): Bool = Float32.clampToInt8(101.0f32, -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt804(): Bool = Float32.clampToInt8(-100.0f32, -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt805(): Bool = Float32.clampToInt8(-101.0f32, -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt806(): Bool = Float32.clampToInt8(Float32.maxValue(), -100i8, 100i8, -127i8) == 100i8

    @Test
    def clampToInt807(): Bool = Float32.clampToInt8(Float32.minValue(), -100i8, 100i8, -127i8) == -100i8

    @Test
    def clampToInt808(): Bool = Float32.clampToInt8(Float32.nan(), -100i8, 100i8, -127i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Bool = Float32.clampToInt16(0.0f32, -100i16, 100i16, -127i16) == 0i16

    @Test
    def clampToInt1602(): Bool = Float32.clampToInt16(100.0f32, -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1603(): Bool = Float32.clampToInt16(101.0f32, -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1604(): Bool = Float32.clampToInt16(-100.0f32, -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1605(): Bool = Float32.clampToInt16(-101.0f32, -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1606(): Bool = Float32.clampToInt16(Float32.maxValue(), -100i16, 100i16, -127i16) == 100i16

    @Test
    def clampToInt1607(): Bool = Float32.clampToInt16(Float32.minValue(), -100i16, 100i16, -127i16) == -100i16

    @Test
    def clampToInt1608(): Bool = Float32.clampToInt16(Float32.nan(), -100i16, 100i16, -127i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt3201(): Bool = Float32.clampToInt32(0.0f32, -100i32, 100i32, -127i32) == 0i32

    @Test
    def clampToInt3202(): Bool = Float32.clampToInt32(100.0f32, -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3203(): Bool = Float32.clampToInt32(101.0f32, -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3204(): Bool = Float32.clampToInt32(-100.0f32, -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3205(): Bool = Float32.clampToInt32(-101.0f32, -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3206(): Bool = Float32.clampToInt32(Float32.maxValue(), -100i32, 100i32, -127i32) == 100i32

    @Test
    def clampToInt3207(): Bool = Float32.clampToInt32(Float32.minValue(), -100i32, 100i32, -127i32) == -100i32

    @Test
    def clampToInt3208(): Bool = Float32.clampToInt32(Float32.nan(), -100i32, 100i32, -127i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt6401(): Bool = Float32.clampToInt64(0.0f32, -100i64, 100i64, -127i64) == 0i64

    @Test
    def clampToInt6402(): Bool = Float32.clampToInt64(100.0f32, -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6403(): Bool = Float32.clampToInt64(101.0f32, -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6404(): Bool = Float32.clampToInt64(-100.0f32, -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6405(): Bool = Float32.clampToInt64(-101.0f32, -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6406(): Bool = Float32.clampToInt64(Float32.maxValue(), -100i64, 100i64, -127i64) == 100i64

    @Test
    def clampToInt6407(): Bool = Float32.clampToInt64(Float32.minValue(), -100i64, 100i64, -127i64) == -100i64

    @Test
    def clampToInt6408(): Bool = Float32.clampToInt64(Float32.nan(), -100i64, 100i64, -127i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    def equalsEps(x: Float32, y: Float32, eps: Float32): Bool =
        let diff = Float32.abs(x - y);
        let x1 = Float32.abs(x);
        let y1 = Float32.abs(y);
        let largest = if (y1 > x1) y1 else x1;
        if (diff <= largest * eps) true else false

    @Test
    def abs01(): Bool =
        equalsEps(Float32.abs(0.0f32), 0.0f32, 0.001f32)

    @Test
    def abs02(): Bool =
        equalsEps(Float32.abs(10.1f32), 10.1f32, 0.001f32)

    @Test
    def abs03(): Bool =
        equalsEps(Float32.abs(-10.1f32), 10.1f32, 0.001f32)

    @Test
    def abs04(): Bool =
        Float32.abs(Float32.nan()) |> Float32.isNan

    @Test
    def abs05(): Bool =
        Float32.abs(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @Test
    def abs06(): Bool =
        Float32.abs(Float32.negativeInfinity()) == Float32.positiveInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ceil01(): Bool =
        equalsEps(Float32.ceil(0.5f32), 1.0f32, 0.001f32)

    @Test
    def ceil02(): Bool =
        equalsEps(Float32.ceil(0.1f32), 1.0f32, 0.001f32)

    @Test
    def ceil03(): Bool =
        equalsEps(Float32.ceil(1.0f32), 1.0f32, 0.001f32)

    @Test
    def ceil04(): Bool =
        equalsEps(Float32.ceil(0.0f32), 0.0f32, 0.001f32)

    @Test
    def ceil05(): Bool =
        equalsEps(Float32.ceil(-0.1f32), 0.0f32, 0.001f32)

    @Test
    def ceil06(): Bool =
        equalsEps(Float32.ceil(-0.9f32), 0.0f32, 0.001f32)

    @Test
    def ceil07(): Bool =
        Float32.ceil(Float32.nan()) |> Float32.isNan

    @Test
    def ceil08(): Bool =
        Float32.ceil(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @Test
    def ceil09(): Bool =
        Float32.ceil(Float32.negativeInfinity()) == Float32.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def floor01(): Bool =
        equalsEps(Float32.floor(0.5f32), 0.0f32, 0.001f32)

    @Test
    def floor02(): Bool =
        equalsEps(Float32.floor(0.1f32), 0.0f32, 0.001f32)

    @Test
    def floor03(): Bool =
        equalsEps(Float32.floor(1.0f32), 1.0f32, 0.001f32)

    @Test
    def floor04(): Bool =
        equalsEps(Float32.floor(0.0f32), 0.0f32, 0.001f32)

    @Test
    def floor05(): Bool =
        equalsEps(Float32.floor(-0.1f32), -1.0f32, 0.001f32)

    @Test
    def floor06(): Bool =
        equalsEps(Float32.floor(-0.9f32), -1.0f32, 0.001f32)

    @Test
    def floor07(): Bool =
        Float32.floor(Float32.nan()) |> Float32.isNan

    @Test
    def floor08(): Bool =
        Float32.floor(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @Test
    def floor09(): Bool =
        Float32.floor(Float32.negativeInfinity()) == Float32.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def round01(): Bool =
        equalsEps(Float32.round(0.5f32), 0.0f32, 0.001f32)

    @Test
    def round02(): Bool =
        equalsEps(Float32.round(0.1f32), 0.0f32, 0.001f32)

    @Test
    def round03(): Bool =
        equalsEps(Float32.round(1.0f32), 1.0f32, 0.001f32)

    @Test
    def round04(): Bool =
        equalsEps(Float32.round(0.0f32), 0.0f32, 0.001f32)

    @Test
    def round05(): Bool =
        equalsEps(Float32.round(-0.1f32), 0.0f32, 0.001f32)

    @Test
    def round06(): Bool =
        equalsEps(Float32.round(-0.9f32), -1.0f32, 0.001f32)

    @Test
    def round07(): Bool =
        Float32.round(Float32.nan()) |> Float32.isNan

    @Test
    def round08(): Bool =
        Float32.round(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @Test
    def round09(): Bool =
        Float32.round(Float32.negativeInfinity()) == Float32.negativeInfinity()

    @Test
    def round10(): Bool =
        equalsEps(Float32.round(1.5f32), 2.0f32, 0.001f32)

    @Test
    def round11(): Bool =
        equalsEps(Float32.round(2.5f32), 2.0f32, 0.001f32)

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = equalsEps(Float32.pow(0.0f32, 0.0f32), 1.0f32, 0.001f32)

    @Test
    def pow02(): Bool = equalsEps(Float32.pow(0.0f32, 1.0f32), 0.0f32, 0.001f32)

    @Test
    def pow03(): Bool = equalsEps(Float32.pow(1.0f32, 0.0f32), 1.0f32, 0.001f32)

    @Test
    def pow04(): Bool = equalsEps(Float32.pow(1.0f32, 1.0f32), 1.0f32, 0.001f32)

    @Test
    def pow05(): Bool = equalsEps(Float32.pow(5.0f32, 2.0f32), 25.0f32, 0.001f32)

    /////////////////////////////////////////////////////////////////////////////
    // floatValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def floatValue01(): Bool =
        let d = Float32.valueOf(0.0f32);
        Float32.floatValue(d) == 0.0f32

    @Test
    def floatValue02(): Bool =
        let d = Float32.valueOf(1.0f32);
        Float32.floatValue(d) == 1.0f32

    @Test
    def floatValue03(): Bool =
        let d = Float32.valueOf(-1.0f32);
        Float32.floatValue(d) == -1.0f32

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        import java.lang.Float.equals(##java.lang.Object): Bool \ {};
        let d = Float32.valueOf(0.0f32);
        equals(d, checked_cast(Float32.valueOf(0.0f32)))

    @Test
    def valueOf02(): Bool =
        import java.lang.Float.equals(##java.lang.Object): Bool \ {};
        let d = Float32.valueOf(1.0f32);
        equals(d, checked_cast(Float32.valueOf(1.0f32)))

    @Test
    def valueOf03(): Bool =
        import java.lang.Float.equals(##java.lang.Object): Bool \ {};
        let d = Float32.valueOf(-1.0f32);
        equals(d, checked_cast(Float32.valueOf(-1.0f32)))

}
