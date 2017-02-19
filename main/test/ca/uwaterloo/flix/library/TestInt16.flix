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
def maxValue01: Bool = Int16.maxValue() `assertEq!` ~~~(1i16 <<< 15)

/////////////////////////////////////////////////////////////////////////////
// minValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def minValue01: Bool = Int16.minValue() `assertEq!` (1i16 <<< 15)

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01: Bool = Int16.size() `assertEq!` 16

/////////////////////////////////////////////////////////////////////////////
// min                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def min01: Bool = Int16.min(1i16, 2i16) `assertEq!` 1i16

@test
def min02: Bool = Int16.min(2i16, -1i16) `assertEq!` -1i16

@test
def min03: Bool = Int16.min(-33i16, -66i16) `assertEq!` -66i16

/////////////////////////////////////////////////////////////////////////////
// max                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def max01: Bool = Int16.max(48i16, 49i16) `assertEq!` 49i16

@test
def max02: Bool = Int16.max(4i16, -16i16) `assertEq!` 4i16

@test
def max03: Bool = Int16.max(-34i16, -16i16) `assertEq!` -16i16

/////////////////////////////////////////////////////////////////////////////
// abs                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def abs01: Bool = Int16.abs(31i16) `assertEq!` 31i16

@test
def abs02: Bool = Int16.abs(-31i16) `assertEq!` 31i16

@test
def abs03: Bool = Int16.abs(32767i16) `assertEq!` 32767i16

@test
def abs04: Bool = Int16.abs(-32767i16) `assertEq!` 32767i16

@test
def abs05: Bool = Int16.abs(0i16) `assertEq!` 0i16

@test
def abs06: Bool = Int16.abs(-32768i16) `assertEq!` -1i16

/////////////////////////////////////////////////////////////////////////////
// dist                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def dist01: Bool = Int16.dist(31i16, -7i16) `assertEq!` 38i16

@test
def dist02: Bool = Int16.dist(-44i16, -1i16) `assertEq!` 43i16

@test
def dist03: Bool = Int16.dist(-2i16, -2i16) `assertEq!` 0i16

@test
def dist04: Bool = Int16.dist(-32768i16, -1i16) `assertEq!` 32767i16

@test
def dist05: Bool = Int16.dist(-32768i16, 0i16) `assertEq!` -1i16

@test
def dist06: Bool = Int16.dist(-123i16, 4i16) `assertEq!` 127i16

@test
def dist07: Bool = Int16.dist(-32763i16, 5i16) `assertEq!` -1i16

/////////////////////////////////////////////////////////////////////////////
// compare                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def compare01: Bool = Int16.compare(-1i16, 44i16) `assertEq!` -1

@test
def compare02: Bool = Int16.compare(-1i16, -44i16) `assertEq!` 1

@test
def compare03: Bool = Int16.compare(88i16, 88i16) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// signum                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def signum01: Bool = Int16.signum(-22i16) `assertEq!` -1

@test
def signum02: Bool = Int16.signum(22i16) `assertEq!` 1

@test
def signum03: Bool = Int16.signum(0i16) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// highestOneBitPosition                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBitPosition01: Bool = Int16.highestOneBitPosition(-1i16) `assertEq!` 15

@test
def highestOneBitPosition02: Bool = Int16.highestOneBitPosition(-32768i16) `assertEq!` 15

@test
def highestOneBitPosition03: Bool = Int16.highestOneBitPosition(1i16) `assertEq!` 0

@test
def highestOneBitPosition04: Bool = Int16.highestOneBitPosition(2i16) `assertEq!` 1

@test
def highestOneBitPosition05: Bool = Int16.highestOneBitPosition(3i16) `assertEq!` 1

@test
def highestOneBitPosition06: Bool = Int16.highestOneBitPosition(20i16) `assertEq!` 4

@test
def highestOneBitPosition07: Bool = Int16.highestOneBitPosition(2i16 ** 14i16) `assertEq!` 14

@test
def highestOneBitPosition08: Bool = Int16.highestOneBitPosition(32767i16) `assertEq!` 14

@test
def highestOneBitPosition09: Bool = Int16.highestOneBitPosition(0i16) `assertEq!` -1

@test
def highestOneBitPosition10: Bool = Int16.highestOneBitPosition(2i16 ** 8i16 + 2i16 ** 11i16) `assertEq!` 11

@test
def highestOneBitPosition11: Bool = Int16.highestOneBitPosition(2i16 ** 3i16 + 2i16 ** 7i16) `assertEq!` 7

@test
def highestOneBitPosition12: Bool = Int16.highestOneBitPosition(-32767i16) `assertEq!` 15

/////////////////////////////////////////////////////////////////////////////
// highestOneBit                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBit01: Bool = Int16.highestOneBit(-1i16) `assertEq!` (1i16 <<< 15)

@test
def highestOneBit02: Bool = Int16.highestOneBit(-32768i16) `assertEq!` (1i16 <<< 15)

@test
def highestOneBit03: Bool = Int16.highestOneBit(-77i16) `assertEq!` (1i16 <<< 15)

@test
def highestOneBit04: Bool = Int16.highestOneBit(2i16) `assertEq!` 2i16

@test
def highestOneBit05: Bool = Int16.highestOneBit(3i16) `assertEq!` 2i16

@test
def highestOneBit06: Bool = Int16.highestOneBit(20i16) `assertEq!` 16i16

@test
def highestOneBit07: Bool = Int16.highestOneBit(2i16 ** 11i16 + 2i16 ** 13i16) `assertEq!` 8192i16

@test
def highestOneBit08: Bool = Int16.highestOneBit(32767i16) `assertEq!` 16384i16

@test
def highestOneBit09: Bool = Int16.highestOneBit(0i16) `assertEq!` 0i16

@test
def highestOneBit10: Bool = Int16.highestOneBit(2i16 ** 6i16 + 2i16 ** 9i16) `assertEq!` 512i16

/////////////////////////////////////////////////////////////////////////////
// numberOfLeadingZeros                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfLeadingZeros01: Bool = Int16.numberOfLeadingZeros(0i16) `assertEq!` 16

@test
def numberOfLeadingZeros02: Bool = Int16.numberOfLeadingZeros(-32768i16) `assertEq!` 0

@test
def numberOfLeadingZeros03: Bool = Int16.numberOfLeadingZeros(-1i16) `assertEq!` 0

@test
def numberOfLeadingZeros04: Bool = Int16.numberOfLeadingZeros(32767i16) `assertEq!` 1

@test
def numberOfLeadingZeros05: Bool = Int16.numberOfLeadingZeros(2i16 ** 14i16) `assertEq!` 1

@test
def numberOfLeadingZeros06: Bool = Int16.numberOfLeadingZeros(2i16 ** 6i16 + 2i16 ** 11i16) `assertEq!` 4

@test
def numberOfLeadingZeros07: Bool = Int16.numberOfLeadingZeros(2i16 ** 7i16 + 2i16 ** 6i16) `assertEq!` 8

@test
def numberOfLeadingZeros08: Bool = Int16.numberOfLeadingZeros(20i16) `assertEq!` 11

@test
def numberOfLeadingZeros09: Bool = Int16.numberOfLeadingZeros(1i16) `assertEq!` 15

/////////////////////////////////////////////////////////////////////////////
// lowestOneBitPosition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBitPosition01: Bool = Int16.lowestOneBitPosition(-1i16) `assertEq!` 0

@test
def lowestOneBitPosition02: Bool = Int16.lowestOneBitPosition(-32768i16) `assertEq!` 15

@test
def lowestOneBitPosition03: Bool = Int16.lowestOneBitPosition(1i16) `assertEq!` 0

@test
def lowestOneBitPosition04: Bool = Int16.lowestOneBitPosition(2i16) `assertEq!` 1

@test
def lowestOneBitPosition05: Bool = Int16.lowestOneBitPosition(3i16) `assertEq!` 0

@test
def lowestOneBitPosition06: Bool = Int16.lowestOneBitPosition(4i16) `assertEq!` 2

@test
def lowestOneBitPosition07: Bool = Int16.lowestOneBitPosition(12i16) `assertEq!` 2

@test
def lowestOneBitPosition08: Bool = Int16.lowestOneBitPosition(2i16 ** 11i16) `assertEq!` 11

@test
def lowestOneBitPosition09: Bool = Int16.lowestOneBitPosition(2i16 ** 11i16 + 2i16 ** 6i16) `assertEq!` 6

@test
def lowestOneBitPosition10: Bool = Int16.lowestOneBitPosition(32767i16) `assertEq!` 0

@test
def lowestOneBitPosition11: Bool = Int16.lowestOneBitPosition(0i16) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// lowestOneBit                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBit01: Bool = Int16.lowestOneBit(-1i16) `assertEq!` 1i16

@test
def lowestOneBit02: Bool = Int16.lowestOneBit(-32768i16) `assertEq!` (1i16 <<< 15)

@test
def lowestOneBit03: Bool = Int16.lowestOneBit(1i16) `assertEq!` 1i16

@test
def lowestOneBit04: Bool = Int16.lowestOneBit(2i16) `assertEq!` 2i16

@test
def lowestOneBit05: Bool = Int16.lowestOneBit(6i16) `assertEq!` 2i16

@test
def lowestOneBit06: Bool = Int16.lowestOneBit(12i16) `assertEq!` 4i16

@test
def lowestOneBit07: Bool = Int16.lowestOneBit(2i16 ** 6i16 + 2i16 ** 9i16 + 2i16 ** 14i16) `assertEq!` 64i16

@test
def lowestOneBit08: Bool = Int16.lowestOneBit(2i16 ** 13i16) `assertEq!` 8192i16

@test
def lowestOneBit09: Bool = Int16.lowestOneBit(2i16 ** 3i16 + 2i16 ** 5i16) `assertEq!` 8i16

@test
def lowestOneBit10: Bool = Int16.lowestOneBit(32767i16) `assertEq!` 1i16

@test
def lowestOneBit11: Bool = Int16.lowestOneBit(0i16) `assertEq!` 0i16

/////////////////////////////////////////////////////////////////////////////
// numberOfTrailingZeros                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfTrailingZeros01: Bool = Int16.numberOfTrailingZeros(0i16) `assertEq!` 16

@test
def numberOfTrailingZeros02: Bool = Int16.numberOfTrailingZeros(3i16) `assertEq!` 0

@test
def numberOfTrailingZeros03: Bool = Int16.numberOfTrailingZeros(42i16) `assertEq!` 1

@test
def numberOfTrailingZeros04: Bool = Int16.numberOfTrailingZeros(2i16 ** 10i16 + 2i16 ** 14i16) `assertEq!` 10

@test
def numberOfTrailingZeros05: Bool = Int16.numberOfTrailingZeros(2i16 ** 8i16) `assertEq!` 8

@test
def numberOfTrailingZeros06: Bool = Int16.numberOfTrailingZeros(-1i16) `assertEq!` 0

@test
def numberOfTrailingZeros07: Bool = Int16.numberOfTrailingZeros(-32768i16) `assertEq!` 15

/////////////////////////////////////////////////////////////////////////////
// bit                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def bit01: Bool = Int16.bit(-1i16, 5) `assertEq!` 1

@test
def bit02: Bool = Int16.bit(-10000i16, 15) `assertEq!` 1

@test
def bit03: Bool = Int16.bit(2i16 ** 12i16, 12) `assertEq!` 1

@test
def bit04: Bool = Int16.bit(2i16 ** 12i16, 13) `assertEq!` 0

@test
def bit05: Bool = Int16.bit(2i16 ** 12i16, 11) `assertEq!` 0

@test
def bit06: Bool = Int16.bit(-1i16 - 2i16 ** 9i16, 9) `assertEq!` 0

@test
def bit07: Bool = Int16.bit(-1i16 - 2i16 ** 9i16, 10) `assertEq!` 1

@test
def bit08: Bool = Int16.bit(0i16, 15) `assertEq!` 0