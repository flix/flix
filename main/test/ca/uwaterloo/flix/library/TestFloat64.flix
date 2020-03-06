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
    def nan01(): Bool = assert!(Float64.isNan(Float64.nan()))
    
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
    def isFinite01(): Bool = assert!(Float64.isFinite(1.0f64))
    
    @test
    def isFinite02(): Bool = assert!(Float64.isFinite(-1.0f64))
    
    @test
    def isFinite03(): Bool = assert!(Float64.isFinite(Float64.minValue()))
    
    @test
    def isFinite04(): Bool = assert!(Float64.isFinite(Float64.maxValue()))
    
    @test
    def isFinite05(): Bool = assertNot!(Float64.isFinite(Float64.negativeInfinity()))
    
    @test
    def isFinite06(): Bool = assertNot!(Float64.isFinite(Float64.positiveInfinity()))
    
    @test
    def isFinite07(): Bool = assertNot!(Float64.isFinite(Float64.nan()))
    
    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isInfinite01(): Bool = assertNot!(Float64.isInfinite(1.0f64))
    
    @test
    def isInfinite02(): Bool = assertNot!(Float64.isInfinite(-1.0f64))
    
    @test
    def isInfinite03(): Bool = assertNot!(Float64.isInfinite(Float64.minValue()))
    
    @test
    def isInfinite04(): Bool = assertNot!(Float64.isInfinite(Float64.maxValue()))
    
    @test
    def isInfinite05(): Bool = assert!(Float64.isInfinite(Float64.negativeInfinity()))
    
    @test
    def isInfinite06(): Bool = assert!(Float64.isInfinite(Float64.positiveInfinity()))
    
    @test
    def isInfinite07(): Bool = assertNot!(Float64.isInfinite(Float64.nan()))
    
    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isNan01(): Bool = assert!(Float64.isNan(Float64.nan()))
    
    @test
    def isNan02(): Bool = assert!(Float64.isNan(0.0f64 / 0.0f64))
    
    @test
    def isNan03(): Bool = assertNot!(Float64.isNan(Float64.positiveInfinity()))
    
    @test
    def isNan04(): Bool = assertNot!(Float64.isNan(Float64.negativeInfinity()))
    
    @test
    def isNan05(): Bool = assertNot!(Float64.isNan(1.0f64))
    
    @test
    def isNan06(): Bool = assertNot!(Float64.isNan(-1.0f64))
    
    @test
    def isNan07(): Bool = assertNot!(Float64.isNan(Float64.minValue()))
    
    @test
    def isNan08(): Bool = assertNot!(Float64.isNan(Float64.maxValue()))
    
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

}
