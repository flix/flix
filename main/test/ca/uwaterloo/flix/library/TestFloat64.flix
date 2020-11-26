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

namespace TestFloat64 {
    
    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Float64.size() == 64
    
    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minExponent01(): Bool = Float64.minExponent() == -1022
    
    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxExponent01(): Bool = Float64.maxExponent() == 1023
    
    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Float64.maxValue() == ((2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64))
    
    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Float64.minValue() == (-(2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64))
    
    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minPositiveValue01(): Bool = Float64.minPositiveValue() == (2.0f64 ** -1074.0f64)
    
    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def nan01(): Bool = Float64.isNan(Float64.nan()) == true
    
    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def positiveInfinity01(): Bool = Float64.positiveInfinity() == (1.0f64 / 0.0f64)
    
    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def negativeInfinity01(): Bool = Float64.negativeInfinity() == (-1.0f64 / 0.0f64)
    
    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isFinite01(): Bool = Float64.isFinite(1.0f64) == true
    
    @test
    def isFinite02(): Bool = Float64.isFinite(-1.0f64) == true
    
    @test
    def isFinite03(): Bool = Float64.isFinite(Float64.minValue()) == true
    
    @test
    def isFinite04(): Bool = Float64.isFinite(Float64.maxValue()) == true
    
    @test
    def isFinite05(): Bool = Float64.isFinite(Float64.negativeInfinity()) == false
    
    @test
    def isFinite06(): Bool = Float64.isFinite(Float64.positiveInfinity()) == false
    
    @test
    def isFinite07(): Bool = Float64.isFinite(Float64.nan()) == false
    
    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isInfinite01(): Bool = Float64.isInfinite(1.0f64) == false
    
    @test
    def isInfinite02(): Bool = Float64.isInfinite(-1.0f64) == false
    
    @test
    def isInfinite03(): Bool = Float64.isInfinite(Float64.minValue()) == false
    
    @test
    def isInfinite04(): Bool = Float64.isInfinite(Float64.maxValue()) == false
    
    @test
    def isInfinite05(): Bool = Float64.isInfinite(Float64.negativeInfinity()) == true
    
    @test
    def isInfinite06(): Bool = Float64.isInfinite(Float64.positiveInfinity()) == true
    
    @test
    def isInfinite07(): Bool = Float64.isInfinite(Float64.nan()) == false
    
    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isNan01(): Bool = Float64.isNan(Float64.nan()) == true
    
    @test
    def isNan02(): Bool = Float64.isNan(0.0f64 / 0.0f64) == true
    
    @test
    def isNan03(): Bool = Float64.isNan(Float64.positiveInfinity()) == false
    
    @test
    def isNan04(): Bool = Float64.isNan(Float64.negativeInfinity()) == false
    
    @test
    def isNan05(): Bool = Float64.isNan(1.0f64) == false
    
    @test
    def isNan06(): Bool = Float64.isNan(-1.0f64) == false
    
    @test
    def isNan07(): Bool = Float64.isNan(Float64.minValue()) == false
    
    @test
    def isNan08(): Bool = Float64.isNan(Float64.maxValue()) == false
    
    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Float64.min(1.0f64, 2.0f64) == 1.0f64
    
    @test
    def min02(): Bool = Float64.min(2.0f64, -1.0f64) == -1.0f64
    
    @test
    def min03(): Bool = Float64.min(-33.0f64, -66.0f64) == -66.0f64
    
    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Float64.max(48.0f64, 49.0f64) == 49.0f64
    
    @test
    def max02(): Bool = Float64.max(4.0f64, -16.0f64) == 4.0f64
    
    @test
    def max03(): Bool = Float64.max(-34.0f64, -16.0f64) == -16.0f64
    
    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Float64.toString(0.0f64) == "0.0"
    
    @test
    def toString02(): Bool = Float64.toString(1.0f64) == "1.0"
    
    @test
    def toString03(): Bool = Float64.toString(-1.0f64) == "-1.0"
    
    @test
    def toString04(): Bool = Float64.toString(3.14f64) == "3.14"
    
    @test
    def toString05(): Bool = Float64.toString(-3.14f64) == "-3.14"
    
    @test
    def toString06(): Bool = Float64.toString(Float64.nan()) == "NaN"
    
    @test
    def toString07(): Bool = Float64.toString(Float64.positiveInfinity()) == "Infinity"
    
    @test
    def toString08(): Bool = Float64.toString(Float64.negativeInfinity()) == "-Infinity"

// TODO
//    /////////////////////////////////////////////////////////////////////////////
//    // toDecimalString                                                         //
//    /////////////////////////////////////////////////////////////////////////////
//    @test
//    def toDecimalString01(): Bool = Float64.toDecimalString(2, 0.0f64) == "0"
//
//    @test
//    def toDecimalString02(): Bool = Float64.toDecimalString(2, 3.14159f64) == "3.14"
//
//    @test
//    def toDecimalString03(): Bool = Float64.toDecimalString(4, 3.14159f64) == "3.1416"
//
//    @test
//    def toDecimalString04(): Bool = Float64.toDecimalString(2, Float64.nan()) == "NaN"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Float64.fromString("Bad") |> Result.isErr
    
    @test
    def fromString02(): Bool = Float64.fromString(" 1.0") == Ok(1.0f64)
    
    @test
    def fromString03(): Bool = Float64.fromString("1.0 ") == Ok(1.0f64)
    
    @test
    def fromString04(): Bool = Float64.fromString("0.0") == Ok(0.0f64)
    
    @test
    def fromString05(): Bool = Float64.fromString("1.0") == Ok(1.0f64)
    
    @test
    def fromString06(): Bool = Float64.fromString("-1.0") == Ok(-1.0f64)
    
    @test
    def fromString07(): Bool = Float64.fromString("3.14") == Ok(3.14f64)
    
    @test
    def fromString08(): Bool = Float64.fromString("-3.14") == Ok(-3.14f64)
    
    @test
    def fromString09(): Bool = match Float64.fromString("NaN") {
        case Ok(x)  => Float64.isNan(x)
        case _ => false
    }
    
    @test
    def fromString10(): Bool = Float64.fromString("Infinity") == Ok(Float64.positiveInfinity())
    
    @test
    def fromString11(): Bool = Float64.fromString("-Infinity") == Ok(Float64.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Float64.tryToInt8(0.0f64) == Some(0i8)

    @test
    def tryToInt802(): Bool = Float64.tryToInt8(1.0f64) == Some(1i8)

    @test
    def tryToInt803(): Bool = Float64.tryToInt8(-1.0f64) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Float64.tryToInt8(127.0f64) == Some(127i8)

    @test
    def tryToInt805(): Bool = Float64.tryToInt8(128.0f64) == None

    @test
    def tryToInt806(): Bool = Float64.tryToInt8(-128.0f64) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Float64.tryToInt8(-129.0f64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = Float64.tryToInt16(0.0f64) == Some(0i16)

    @test
    def tryToInt1602(): Bool = Float64.tryToInt16(1.0f64) == Some(1i16)

    @test
    def tryToInt1603(): Bool = Float64.tryToInt16(-1.0f64) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = Float64.tryToInt16(32767.0f64) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = Float64.tryToInt16(32768.0f64) == None

    @test
    def tryToInt1606(): Bool = Float64.tryToInt16(-32768.0f64) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = Float64.tryToInt16(-32769.0f64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt3201(): Bool = Float64.tryToInt32(0.0f64) == Some(0)

    @test
    def tryToInt3202(): Bool = Float64.tryToInt32(1.0f64) == Some(1)

    @test
    def tryToInt3203(): Bool = Float64.tryToInt32(-1.0f64) == Some(-1)

    @test
    def tryToInt3204(): Bool = Float64.tryToInt32(2147483647.0f64) != None

    @test
    def tryToInt3205(): Bool = Float64.tryToInt32(2147483648.0f64) == None

    @test
    def tryToInt3206(): Bool = Float64.tryToInt32(-2147483648.0f64) != None

    @test
    def tryToInt3207(): Bool = Float64.tryToInt32(-2147483649.0f64) == None


    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt6401(): Bool = Float64.tryToInt64(0.0f64) == Some(0i64)

    @test
    def tryToInt6402(): Bool = Float64.tryToInt64(1.0f64) == Some(1i64)

    @test
    def tryToInt6403(): Bool = Float64.tryToInt64(-1.0f64) == Some(-1i64)

    @test
    def tryToInt6404(): Bool = Float64.tryToInt64(9223372036854775807.0f64) != None

    @test
    def tryToInt6405(): Bool = Float64.tryToInt64(9223372036854780000.0f64) == None        // Allow some imprecision

    @test
    def tryToInt6406(): Bool = Float64.tryToInt64(-9223372036854775808.0f64) != None

    @test
    def tryToInt6407(): Bool = Float64.tryToInt64(-9223372036854780000.0f64) == None       // Allow some imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigInt                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToBigInt01(): Bool = Float64.tryToBigInt(0.0f64) == Some(0ii)

    @test
    def tryToBigInt02(): Bool = Float64.tryToBigInt(1.0f64) == Some(1ii)

    @test
    def tryToBigInt03(): Bool = Float64.tryToBigInt(-1.0f64) == Some(-1ii)

    @test
    def tryToBigInt04(): Bool =
        let step = () -> Float64.maxValue() |> Float64.tryToBigInt;
        step() != None

    @test
    def tryToBigInt05(): Bool =
        let step = () -> Float64.minValue() |> Float64.tryToBigInt;
        step() != None

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToFloat3201(): Bool = Float64.tryToFloat32(0.0f64) == Some(0.0f32)

    @test
    def tryToFloat3202(): Bool = Float64.tryToFloat32(1.0f64) == Some(1.0f32)

    @test
    def tryToFloat3203(): Bool = Float64.tryToFloat32(-1.0f64) == Some(-1.0f32)

    @test
    def tryToFloat3204(): Bool = match Float64.tryToFloat32(123456789.10f64) {
        case None => false
        case Some(d) => d >= 123456700.0f32 and d <= 123456800.0f32       // Allow imprecision
    }

    @test
    def tryToFloat3205(): Bool = match Float64.tryToFloat32(-123456789.10f64) {
        case None => false
        case Some(d) => d <= -123456700.0f32 and d >= -123456800.0f32     // Allow imprecision
    }

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Float64.clampToInt8(0.0f64, -100i8, 100i8, -127i8) == 0i8

    @test
    def clampToInt802(): Bool = Float64.clampToInt8(100.0f64, -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt803(): Bool = Float64.clampToInt8(101.0f64, -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt804(): Bool = Float64.clampToInt8(-100.0f64, -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt805(): Bool = Float64.clampToInt8(-101.0f64, -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt806(): Bool = Float64.clampToInt8(Float64.maxValue(), -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt807(): Bool = Float64.clampToInt8(Float64.minValue(), -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt808(): Bool = Float64.clampToInt8(Float64.nan(), -100i8, 100i8, -127i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = Float64.clampToInt16(0.0f64, -100i16, 100i16, -127i16) == 0i16

    @test
    def clampToInt1602(): Bool = Float64.clampToInt16(100.0f64, -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1603(): Bool = Float64.clampToInt16(101.0f64, -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1604(): Bool = Float64.clampToInt16(-100.0f64, -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1605(): Bool = Float64.clampToInt16(-101.0f64, -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1606(): Bool = Float64.clampToInt16(Float64.maxValue(), -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1607(): Bool = Float64.clampToInt16(Float64.minValue(), -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1608(): Bool = Float64.clampToInt16(Float64.nan(), -100i16, 100i16, -127i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt3201(): Bool = Float64.clampToInt32(0.0f64, -100i32, 100i32, -127i32) == 0i32

    @test
    def clampToInt3202(): Bool = Float64.clampToInt32(100.0f64, -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3203(): Bool = Float64.clampToInt32(101.0f64, -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3204(): Bool = Float64.clampToInt32(-100.0f64, -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3205(): Bool = Float64.clampToInt32(-101.0f64, -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3206(): Bool = Float64.clampToInt32(Float64.maxValue(), -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3207(): Bool = Float64.clampToInt32(Float64.minValue(), -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3208(): Bool = Float64.clampToInt32(Float64.nan(), -100i32, 100i32, -127i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt6401(): Bool = Float64.clampToInt64(0.0f64, -100i64, 100i64, -127i64) == 0i64

    @test
    def clampToInt6402(): Bool = Float64.clampToInt64(100.0f64, -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6403(): Bool = Float64.clampToInt64(101.0f64, -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6404(): Bool = Float64.clampToInt64(-100.0f64, -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6405(): Bool = Float64.clampToInt64(-101.0f64, -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6406(): Bool = Float64.clampToInt64(Float64.maxValue(), -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6407(): Bool = Float64.clampToInt64(Float64.minValue(), -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6408(): Bool = Float64.clampToInt64(Float64.nan(), -100i64, 100i64, -127i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat32                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToFloat3201(): Bool = Float64.clampToFloat32(0.0f64, -100.0f32, 100.0f32) == 0.0f32

    @test
    def clampToFloat3202(): Bool = Float64.clampToFloat32(100.0f64, -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3203(): Bool = Float64.clampToFloat32(101.0f64, -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3204(): Bool = Float64.clampToFloat32(-100.0f64, -100.0f32, 100.0f32) == -100.0f32

    @test
    def clampToFloat3205(): Bool = Float64.clampToFloat32(-101.0f64, -100.0f32, 100.0f32) == -100.0f32

    @test
    def clampToFloat3206(): Bool = Float64.clampToFloat32(Float64.maxValue(), -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3207(): Bool = Float64.clampToFloat32(Float64.minValue(), -100.0f32, 100.0f32) == -100.0f32

    @test
    def clampToFloat3208(): Bool =
        let ans = Float64.clampToFloat32(Float64.nan(), -100.0f32, 100.0f32);
        Float32.isNan(ans) == true

}
