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

}
