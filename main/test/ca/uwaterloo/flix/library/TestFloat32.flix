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

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01: Bool = Float32.size() `assertEq!` 32

/////////////////////////////////////////////////////////////////////////////
// minExponent                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def minExponent01: Bool = Float32.minExponent() `assertEq!` -126

/////////////////////////////////////////////////////////////////////////////
// maxExponent                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def maxExponent01: Bool = Float32.maxExponent() `assertEq!` 127

/////////////////////////////////////////////////////////////////////////////
// maxValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def maxValue01: Bool = Float32.maxValue() `assertEq!` ((2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32))

/////////////////////////////////////////////////////////////////////////////
// minValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def minValue01: Bool = Float32.minValue() `assertEq!` (-(2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32))

/////////////////////////////////////////////////////////////////////////////
// minPositiveValue                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def minPositiveValue01: Bool = Float32.minPositiveValue() `assertEq!` (2.0f32 ** -149.0f32)

/////////////////////////////////////////////////////////////////////////////
// undefined                                                               //
/////////////////////////////////////////////////////////////////////////////
//@test
//def undefined01: Bool = Float32.undefined() `assertEq!` (0.0f32 / 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// positiveInfinity                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def positiveInfinity01: Bool = Float32.positiveInfinity() `assertEq!` (1.0f32 / 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// negativeInfinity                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def negativeInfinity01: Bool = Float32.negativeInfinity() `assertEq!` (-1.0f32 / 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// min                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def min01: Bool = Float32.min(1.0f32, 2.0f32) `assertEq!` 1.0f32

@test
def min02: Bool = Float32.min(2.0f32, -1.0f32) `assertEq!` -1.0f32

@test
def min03: Bool = Float32.min(-33.0f32, -66.0f32) `assertEq!` -66.0f32

/////////////////////////////////////////////////////////////////////////////
// max                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def max01: Bool = Float32.max(48.0f32, 49.0f32) `assertEq!` 49.0f32

@test
def max02: Bool = Float32.max(4.0f32, -16.0f32) `assertEq!` 4.0f32

@test
def max03: Bool = Float32.max(-34.0f32, -16.0f32) `assertEq!` -16.0f32