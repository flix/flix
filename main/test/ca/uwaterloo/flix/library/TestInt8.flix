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
// maxValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def maxValue01: Bool = Int8.maxValue() `assertEq!` ~~~(1i8 <<< 7)

/////////////////////////////////////////////////////////////////////////////
// minValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def minValue01: Bool = Int8.minValue() `assertEq!` (1i8 <<< 7)

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01: Bool = Int8.size() `assertEq!` 8

/////////////////////////////////////////////////////////////////////////////
// min                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def min01: Bool = Int8.min(1i8, 2i8) `assertEq!` 1i8

@test
def min02: Bool = Int8.min(2i8, -1i8) `assertEq!` -1i8

@test
def min03: Bool = Int8.min(-33i8, -66i8) `assertEq!` -66i8

/////////////////////////////////////////////////////////////////////////////
// max                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def max01: Bool = Int8.max(48i8, 49i8) `assertEq!` 49i8

@test
def max02: Bool = Int8.max(4i8, -16i8) `assertEq!` 4i8

@test
def max03: Bool = Int8.max(-34i8, -16i8) `assertEq!` -16i8

/////////////////////////////////////////////////////////////////////////////
// abs                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def abs01: Bool = Int8.abs(31i8) `assertEq!` 31i8

@test
def abs02: Bool = Int8.abs(-31i8) `assertEq!` 31i8

@test
def abs03: Bool = Int8.abs(127i8) `assertEq!` 127i8

@test
def abs04: Bool = Int8.abs(-127i8) `assertEq!` 127i8

@test
def abs05: Bool = Int8.abs(0i8) `assertEq!` 0i8

@test
def abs06: Bool = Int8.abs(-128i8) `assertEq!` -1i8

/////////////////////////////////////////////////////////////////////////////
// dist                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def dist01: Bool = Int8.dist(31i8, -7i8) `assertEq!` 38i8

@test
def dist02: Bool = Int8.dist(-44i8, -1i8) `assertEq!` 43i8

@test
def dist03: Bool = Int8.dist(-2i8, -2i8) `assertEq!` 0i8

@test
def dist04: Bool = Int8.dist(-128i8, -1i8) `assertEq!` 127i8

@test
def dist05: Bool = Int8.dist(-128i8, 0i8) `assertEq!` -1i8

@test
def dist06: Bool = Int8.dist(-123i8, 4i8) `assertEq!` 127i8

@test
def dist07: Bool = Int8.dist(-123i8, 5i8) `assertEq!` -1i8

/////////////////////////////////////////////////////////////////////////////
// compare                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def compare01: Bool = Int8.compare(-1i8, 44i8) `assertEq!` -1

@test
def compare02: Bool = Int8.compare(-1i8, -44i8) `assertEq!` 1

@test
def compare03: Bool = Int8.compare(88i8, 88i8) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// signum                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def signum01: Bool = Int8.signum(-22i8) `assertEq!` -1

@test
def signum02: Bool = Int8.signum(22i8) `assertEq!` 1

@test
def signum03: Bool = Int8.signum(0i8) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// highestOneBitPosition                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBitPosition01: Bool = Int8.highestOneBitPosition(-1i8) `assertEq!` 7

@test
def highestOneBitPosition02: Bool = Int8.highestOneBitPosition(-128i8) `assertEq!` 7

@test
def highestOneBitPosition03: Bool = Int8.highestOneBitPosition(1i8) `assertEq!` 0

@test
def highestOneBitPosition04: Bool = Int8.highestOneBitPosition(2i8) `assertEq!` 1

@test
def highestOneBitPosition05: Bool = Int8.highestOneBitPosition(3i8) `assertEq!` 1

@test
def highestOneBitPosition06: Bool = Int8.highestOneBitPosition(20i8) `assertEq!` 4

@test
def highestOneBitPosition07: Bool = Int8.highestOneBitPosition(2i8 ** 6i8) `assertEq!` 6

@test
def highestOneBitPosition08: Bool = Int8.highestOneBitPosition(127i8) `assertEq!` 6

@test
def highestOneBitPosition09: Bool = Int8.highestOneBitPosition(0i8) `assertEq!` -1

@test
def highestOneBitPosition10: Bool = Int8.highestOneBitPosition(2i8 ** 3i8 + 2i8 ** 5i8) `assertEq!` 5

@test
def highestOneBitPosition11: Bool = Int8.highestOneBitPosition(2i8 ** 3i8 + 2i8 ** 4i8) `assertEq!` 4

@test
def highestOneBitPosition12: Bool = Int8.highestOneBitPosition(-10i8) `assertEq!` 7

/////////////////////////////////////////////////////////////////////////////
// highestOneBit                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBit01: Bool = Int8.highestOneBit(-1i8) `assertEq!` (1i8 <<< 7)

@test
def highestOneBit02: Bool = Int8.highestOneBit(-128i8) `assertEq!` (1i8 <<< 7)

@test
def highestOneBit03: Bool = Int8.highestOneBit(-77i8) `assertEq!` (1i8 <<< 7)

@test
def highestOneBit04: Bool = Int8.highestOneBit(2i8) `assertEq!` 2i8

@test
def highestOneBit05: Bool = Int8.highestOneBit(3i8) `assertEq!` 2i8

@test
def highestOneBit06: Bool = Int8.highestOneBit(20i8) `assertEq!` 16i8

@test
def highestOneBit07: Bool = Int8.highestOneBit(2i8 ** 2i8 + 2i8 ** 5i8) `assertEq!` 32i8

@test
def highestOneBit08: Bool = Int8.highestOneBit(127i8) `assertEq!` 64i8

@test
def highestOneBit09: Bool = Int8.highestOneBit(0i8) `assertEq!` 0i8

@test
def highestOneBit10: Bool = Int8.highestOneBit(2i8 ** 5i8 + 2i8 ** 6i8) `assertEq!` 64i8

/////////////////////////////////////////////////////////////////////////////
// numberOfLeadingZeros                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfLeadingZeros01: Bool = Int8.numberOfLeadingZeros(0i8) `assertEq!` 8

@test
def numberOfLeadingZeros02: Bool = Int8.numberOfLeadingZeros(-128i8) `assertEq!` 0

@test
def numberOfLeadingZeros03: Bool = Int8.numberOfLeadingZeros(-1i8) `assertEq!` 0

@test
def numberOfLeadingZeros04: Bool = Int8.numberOfLeadingZeros(127i8) `assertEq!` 1

@test
def numberOfLeadingZeros05: Bool = Int8.numberOfLeadingZeros(2i8 ** 6i8) `assertEq!` 1

@test
def numberOfLeadingZeros06: Bool = Int8.numberOfLeadingZeros(2i8 ** 3i8 + 2i8 ** 6i8) `assertEq!` 1

@test
def numberOfLeadingZeros07: Bool = Int8.numberOfLeadingZeros(2i8 ** 4i8 + 2i8 ** 5i8) `assertEq!` 2

@test
def numberOfLeadingZeros08: Bool = Int8.numberOfLeadingZeros(20i8) `assertEq!` 3

@test
def numberOfLeadingZeros09: Bool = Int8.numberOfLeadingZeros(1i8) `assertEq!` 7

/////////////////////////////////////////////////////////////////////////////
// lowestOneBitPosition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBitPosition01: Bool = Int8.lowestOneBitPosition(-1i8) `assertEq!` 0

@test
def lowestOneBitPosition02: Bool = Int8.lowestOneBitPosition(-128i8) `assertEq!` 7

@test
def lowestOneBitPosition03: Bool = Int8.lowestOneBitPosition(1i8) `assertEq!` 0

@test
def lowestOneBitPosition04: Bool = Int8.lowestOneBitPosition(2i8) `assertEq!` 1

@test
def lowestOneBitPosition05: Bool = Int8.lowestOneBitPosition(3i8) `assertEq!` 0

@test
def lowestOneBitPosition06: Bool = Int8.lowestOneBitPosition(4i8) `assertEq!` 2

@test
def lowestOneBitPosition07: Bool = Int8.lowestOneBitPosition(12i8) `assertEq!` 2

@test
def lowestOneBitPosition08: Bool = Int8.lowestOneBitPosition(2i8 ** 4i8) `assertEq!` 4

@test
def lowestOneBitPosition09: Bool = Int8.lowestOneBitPosition(2i8 ** 4i8 + 2i8 ** 6i8) `assertEq!` 4

@test
def lowestOneBitPosition10: Bool = Int8.lowestOneBitPosition(127i8) `assertEq!` 0

@test
def lowestOneBitPosition11: Bool = Int8.lowestOneBitPosition(0i8) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// lowestOneBit                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBit01: Bool = Int8.lowestOneBit(-1i8) `assertEq!` 1i8

@test
def lowestOneBit02: Bool = Int8.lowestOneBit(-128i8) `assertEq!` (1i8 <<< 7)

@test
def lowestOneBit03: Bool = Int8.lowestOneBit(1i8) `assertEq!` 1i8

@test
def lowestOneBit04: Bool = Int8.lowestOneBit(2i8) `assertEq!` 2i8

@test
def lowestOneBit05: Bool = Int8.lowestOneBit(6i8) `assertEq!` 2i8

@test
def lowestOneBit06: Bool = Int8.lowestOneBit(12i8) `assertEq!` 4i8

@test
def lowestOneBit07: Bool = Int8.lowestOneBit(2i8 ** 6i8) `assertEq!` 64i8

@test
def lowestOneBit08: Bool = Int8.lowestOneBit(2i8 ** 5i8) `assertEq!` 32i8

@test
def lowestOneBit09: Bool = Int8.lowestOneBit(2i8 ** 3i8 + 2i8 ** 5i8) `assertEq!` 8i8

@test
def lowestOneBit10: Bool = Int8.lowestOneBit(127i8) `assertEq!` 1i8

@test
def lowestOneBit11: Bool = Int8.lowestOneBit(0i8) `assertEq!` 0i8

/////////////////////////////////////////////////////////////////////////////
// numberOfTrailingZeros                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfTrailingZeros01: Bool = Int8.numberOfTrailingZeros(0i8) `assertEq!` 8

@test
def numberOfTrailingZeros02: Bool = Int8.numberOfTrailingZeros(3i8) `assertEq!` 0

@test
def numberOfTrailingZeros03: Bool = Int8.numberOfTrailingZeros(42i8) `assertEq!` 1

@test
def numberOfTrailingZeros04: Bool = Int8.numberOfTrailingZeros(2i8 ** 4i8 + 2i8 ** 6i8) `assertEq!` 4

@test
def numberOfTrailingZeros05: Bool = Int8.numberOfTrailingZeros(2i8 ** 6i8) `assertEq!` 6

@test
def numberOfTrailingZeros06: Bool = Int8.numberOfTrailingZeros(-1i8) `assertEq!` 0

@test
def numberOfTrailingZeros07: Bool = Int8.numberOfTrailingZeros(-128i8) `assertEq!` 7

/////////////////////////////////////////////////////////////////////////////
// bit                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def bit01: Bool = Int8.bit(-1i8, 5) `assertEq!` 1

@test
def bit02: Bool = Int8.bit(-100i8, 7) `assertEq!` 1

@test
def bit03: Bool = Int8.bit(2i8 ** 5i8, 5) `assertEq!` 1

@test
def bit04: Bool = Int8.bit(2i8 ** 5i8, 6) `assertEq!` 0

@test
def bit05: Bool = Int8.bit(2i8 ** 5i8, 4) `assertEq!` 0

@test
def bit06: Bool = Int8.bit(-1i8 - 2i8 ** 4i8, 4) `assertEq!` 0

@test
def bit07: Bool = Int8.bit(-1i8 - 2i8 ** 4i8, 5) `assertEq!` 1

@test
def bit08: Bool = Int8.bit(0i8, 7) `assertEq!` 0