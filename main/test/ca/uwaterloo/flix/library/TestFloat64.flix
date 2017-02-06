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
def size01: Bool = Float64.size() `assertEq!` 64

/////////////////////////////////////////////////////////////////////////////
// minExponent                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def minExponent01: Bool = Float64.minExponent() `assertEq!` -1022

/////////////////////////////////////////////////////////////////////////////
// maxExponent                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def maxExponent01: Bool = Float64.maxExponent() `assertEq!` 1023

/////////////////////////////////////////////////////////////////////////////
// maxValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def maxValue01: Bool = Float64.maxValue() `assertEq!` ((2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64))

/////////////////////////////////////////////////////////////////////////////
// minValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def minValue01: Bool = Float64.minValue() `assertEq!` (-(2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64))

/////////////////////////////////////////////////////////////////////////////
// minPositiveValue                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def minPositiveValue01: Bool = Float64.minPositiveValue() `assertEq!` (2.0f64 ** -1074.0f64)

/////////////////////////////////////////////////////////////////////////////
// undefined                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def undefined01: Bool = Float64.undefined() `assertEq!` (0.0f64 / 0.0f64)

/////////////////////////////////////////////////////////////////////////////
// positiveInfinity                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def positiveInfinity01: Bool = Float64.positiveInfinity() `assertEq!` (1.0f64 / 0.0f64)

/////////////////////////////////////////////////////////////////////////////
// negativeInfinity                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def negativeInfinity01: Bool = Float64.negativeInfinity() `assertEq!` (-1.0f64 / 0.0f64)

/////////////////////////////////////////////////////////////////////////////
// min                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def min01: Bool = Float64.min(1.0f64, 2.0f64) `assertEq!` 1.0f64

@test
def min02: Bool = Float64.min(2.0f64, -1.0f64) `assertEq!` -1.0f64

@test
def min03: Bool = Float64.min(-33.0f64, -66.0f64) `assertEq!` -66.0f64

/////////////////////////////////////////////////////////////////////////////
// max                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def max01: Bool = Float64.max(48.0f64, 49.0f64) `assertEq!` 49.0f64

@test
def max02: Bool = Float64.max(4.0f64, -16.0f64) `assertEq!` 4.0f64

@test
def max03: Bool = Float64.max(-34.0f64, -16.0f64) `assertEq!` -16.0f64