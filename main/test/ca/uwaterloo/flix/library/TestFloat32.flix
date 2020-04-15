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

namespace TestFloat32 {
    
    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Float32.size() == 32
    
    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minExponent01(): Bool = Float32.minExponent() == -126
    
    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxExponent01(): Bool = Float32.maxExponent() == 127
    
    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Float32.maxValue() == ((2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32))
    
    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Float32.minValue() == (-(2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32))
    
    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minPositiveValue01(): Bool = Float32.minPositiveValue() == (2.0f32 ** -149.0f32)
    
    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def nan01(): Bool = assert!(Float32.isNan(Float32.nan()))
    
    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def positiveInfinity01(): Bool = Float32.positiveInfinity() == (1.0f32 / 0.0f32)
    
    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def negativeInfinity01(): Bool = Float32.negativeInfinity() == (-1.0f32 / 0.0f32)
    
    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isFinite01(): Bool = assert!(Float32.isFinite(1.0f32))
    
    @test
    def isFinite02(): Bool = assert!(Float32.isFinite(-1.0f32))
    
    @test
    def isFinite03(): Bool = assert!(Float32.isFinite(Float32.minValue()))
    
    @test
    def isFinite04(): Bool = assert!(Float32.isFinite(Float32.maxValue()))
    
    @test
    def isFinite05(): Bool = assertNot!(Float32.isFinite(Float32.negativeInfinity()))
    
    @test
    def isFinite06(): Bool = assertNot!(Float32.isFinite(Float32.positiveInfinity()))
    
    @test
    def isFinite07(): Bool = assertNot!(Float32.isFinite(Float32.nan()))
    
    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isInfinite01(): Bool = assertNot!(Float32.isInfinite(1.0f32))
    
    @test
    def isInfinite02(): Bool = assertNot!(Float32.isInfinite(-1.0f32))
    
    @test
    def isInfinite03(): Bool = assertNot!(Float32.isInfinite(Float32.minValue()))
    
    @test
    def isInfinite04(): Bool = assertNot!(Float32.isInfinite(Float32.maxValue()))
    
    @test
    def isInfinite05(): Bool = assert!(Float32.isInfinite(Float32.negativeInfinity()))
    
    @test
    def isInfinite06(): Bool = assert!(Float32.isInfinite(Float32.positiveInfinity()))
    
    @test
    def isInfinite07(): Bool = assertNot!(Float32.isInfinite(Float32.nan()))
    
    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isNan01(): Bool = assert!(Float32.isNan(Float32.nan()))
    
    @test
    def isNan02(): Bool = assert!(Float32.isNan(0.0f32 / 0.0f32))
    
    @test
    def isNan03(): Bool = assertNot!(Float32.isNan(Float32.positiveInfinity()))
    
    @test
    def isNan04(): Bool = assertNot!(Float32.isNan(Float32.negativeInfinity()))
    
    @test
    def isNan05(): Bool = assertNot!(Float32.isNan(1.0f32))
    
    @test
    def isNan06(): Bool = assertNot!(Float32.isNan(-1.0f32))
    
    @test
    def isNan07(): Bool = assertNot!(Float32.isNan(Float32.minValue()))
    
    @test
    def isNan08(): Bool = assertNot!(Float32.isNan(Float32.maxValue()))
    
    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Float32.min(1.0f32, 2.0f32) == 1.0f32
    
    @test
    def min02(): Bool = Float32.min(2.0f32, -1.0f32) == -1.0f32
    
    @test
    def min03(): Bool = Float32.min(-33.0f32, -66.0f32) == -66.0f32
    
    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Float32.max(48.0f32, 49.0f32) == 49.0f32
    
    @test
    def max02(): Bool = Float32.max(4.0f32, -16.0f32) == 4.0f32
    
    @test
    def max03(): Bool = Float32.max(-34.0f32, -16.0f32) == -16.0f32
    
    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Float32.toString(0.0f32) == "0.0"
    
    @test
    def toString02(): Bool = Float32.toString(1.0f32) == "1.0"
    
    @test
    def toString03(): Bool = Float32.toString(-1.0f32) == "-1.0"
    
    @test
    def toString04(): Bool = Float32.toString(3.14f32) == "3.14"
    
    @test
    def toString05(): Bool = Float32.toString(-3.14f32) == "-3.14"
    
    @test
    def toString06(): Bool = Float32.toString(Float32.nan()) == "NaN"
    
    @test
    def toString07(): Bool = Float32.toString(Float32.positiveInfinity()) == "Infinity"
    
    @test
    def toString08(): Bool = Float32.toString(Float32.negativeInfinity()) == "-Infinity"
    
    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Float32.fromString("Bad") |> Result.isErr
    
    @test
    def fromString02(): Bool = Float32.fromString(" 1.0") == Ok(1.0f32)
    
    @test
    def fromString03(): Bool = Float32.fromString("1.0 ") == Ok(1.0f32)
    
    @test
    def fromString04(): Bool = Float32.fromString("0.0") == Ok(0.0f32)
    
    @test
    def fromString05(): Bool = Float32.fromString("1.0") == Ok(1.0f32)
    
    @test
    def fromString06(): Bool = Float32.fromString("-1.0") == Ok(-1.0f32)
    
    @test
    def fromString07(): Bool = Float32.fromString("3.14") == Ok(3.14f32)
    
    @test
    def fromString08(): Bool = Float32.fromString("-3.14") == Ok(-3.14f32)
    
    @test
    def fromString09(): Bool = match Float32.fromString("NaN") {
        case Ok(x)  => Float32.isNan(x)
        case _ => false
    }
    
    @test
    def fromString10(): Bool = Float32.fromString("Infinity") == Ok(Float32.positiveInfinity())
    
    @test
    def fromString11(): Bool = Float32.fromString("-Infinity") == Ok(Float32.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // toInt8                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt801(): Bool = Float32.toInt8(0.0f32) == Some(0i8)

    @test
    def toInt802(): Bool = Float32.toInt8(1.0f32) == Some(1i8)

    @test
    def toInt803(): Bool = Float32.toInt8(-1.0f32) == Some(-1i8)

//    @test
//    def toInt804(): Bool = Float32.toInt8(3498457932459234592873452983453245ii) == ??

//    @test
//    def toInt805(): Bool = Float32.toInt8(-3498457932459234592873452983453245ii) == ??

    /////////////////////////////////////////////////////////////////////////////
    // toInt16                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt1601(): Bool = Float32.toInt16(0.0f32) == Some(0i16)

    @test
    def toInt1602(): Bool = Float32.toInt16(1.0f32) == Some(1i16)

    @test
    def toInt1603(): Bool = Float32.toInt16(-1.0f32) == Some(-1i16)

//    @test
//    def toInt1604(): Bool = Float32.toInt16(3498457932459234592873452983453245ii) == ??

//    @test
//    def toInt1605(): Bool = Float32.toInt16(-3498457932459234592873452983453245ii) == ??

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt3201(): Bool = Float32.toInt32(0.0f32) == Some(0)

    @test
    def toInt3202(): Bool = Float32.toInt32(1.0f32) == Some(1)

    @test
    def toInt3203(): Bool = Float32.toInt32(-1.0f32) == Some(-1)

//    @test
//    def toInt3204(): Bool = Float32.toInt32(3498457932459234592873452983453245ii) == ??

//    @test
//    def toInt3205(): Bool = Float32.toInt32(-3498457932459234592873452983453245ii) == ??

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt6401(): Bool = Float32.toInt64(0.0f32) == Some(0i64)

    @test
    def toInt6402(): Bool = Float32.toInt64(1.0f32) == Some(1i64)

    @test
    def toInt6403(): Bool = Float32.toInt64(-1.0f32) == Some(-1i64)

//    @test
//    def toInt6404(): Bool = Float32.toInt64(3498457932459234592873452983453245ii) == ??

//    @test
//    def toInt6405(): Bool = Float32.toInt64(-3498457932459234592873452983453245ii) == ??

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = Float32.toBigInt(0.0f32) == Some(0ii)

    @test
    def toBigInt02(): Bool = Float32.toBigInt(1.0f32) == Some(1ii)

    @test
    def toBigInt03(): Bool = Float32.toBigInt(-1.0f32) == Some(-1ii)

//    @test
//    def toBigInt04(): Bool = Float32.toBigInt(3498457932459234592873452983453245ii) == ??

//    @test
//    def toBigInt05(): Bool = Float32.toBigInt(-3498457932459234592873452983453245ii) == ??

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Float32.toFloat64(0.0f32) == 0.0f64

    @test
    def toFloat6402(): Bool = Float32.toFloat64(1.0f32) == 1.0f64

    @test
    def toFloat6403(): Bool = Float32.toFloat64(-1.0f32) == -1.0f64

//    @test
//    def toFloat6404(): Bool = Float32.toFloat64(3498457932459234592873452983453245ii) == ??

//    @test
//    def toFloat6405(): Bool = Float32.toFloat64(-3498457932459234592873452983453245ii) == ??


}
