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
// logicalRightShift                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def logicalRightShift01: Bool = Int16.logicalRightShift(2233i16, 4) `assertEq!` (2233i16 >>> 4)

@test
def logicalRightShift02: Bool = Int16.logicalRightShift(32767i16, 33) `assertEq!` (32767i16 >>> 33)

@test
def logicalRightShift03: Bool = Int16.logicalRightShift(2233i16, -8) `assertEq!` (2233i16 >>> -8)

@test
def logicalRightShift04: Bool = Int16.logicalRightShift(2233i16, 4) `assertEq!` (2233i16 >>> 4)

@test
def logicalRightShift05: Bool = Int16.logicalRightShift(-1i16, 1) `assertEq!` 32767i16

@test
def logicalRightShift06: Bool = Int16.logicalRightShift(-35i16, 4) `assertEq!` 4093i16

@test
def logicalRightShift07: Bool = Int16.logicalRightShift(-2777i16, -2777) `assertEq!` 490i16

@test
def logicalRightShift08: Bool = Int16.logicalRightShift(-233i16, 64) `assertEq!` -233i16

@test
def logicalRightShift09: Bool = Int16.logicalRightShift(-233i16, 63) `assertEq!` 0i16

@test
def logicalRightShift10: Bool = Int16.logicalRightShift(-27i16, 0) `assertEq!` -27i16

@test
def logicalRightShift11: Bool = Int16.logicalRightShift(32767i16, 1) `assertEq!` 16383i16

@test
def logicalRightShift12: Bool = Int16.logicalRightShift(32767i16, 17) `assertEq!` 0i16

@test
def logicalRightShift13: Bool = Int16.logicalRightShift(32767i16, 16) `assertEq!` 0i16

@test
def logicalRightShift14: Bool = Int16.logicalRightShift(-32768i16, 17) `assertEq!` 0i16

@test
def logicalRightShift15: Bool = Int16.logicalRightShift(-32768i16, 16) `assertEq!` 0i16

/////////////////////////////////////////////////////////////////////////////
// bitCount                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def bitCount01: Bool = Int16.bitCount(19909i16) `assertEq!` 8

@test
def bitCount02: Bool = Int16.bitCount(-1i16) `assertEq!` 16

@test
def bitCount03: Bool = Int16.bitCount(-451i16) `assertEq!` 12

@test
def bitCount04: Bool = Int16.bitCount(0i16) `assertEq!` 0

@test
def bitCount05: Bool = Int16.bitCount(32767i16) `assertEq!` 15

@test
def bitCount06: Bool = Int16.bitCount(-32768i16) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01: Bool = Int16.rotateRight(0i16, 3) `assertEq!` 0i16

@test
def rotateRight02: Bool = Int16.rotateRight(0i16, 19) `assertEq!` 0i16

@test
def rotateRight03: Bool = Int16.rotateRight(0i16, -3) `assertEq!` 0i16

@test
def rotateRight04: Bool = Int16.rotateRight(-1i16, 3) `assertEq!` -1i16

@test
def rotateRight05: Bool = Int16.rotateRight(-1i16, 19) `assertEq!` -1i16

@test
def rotateRight06: Bool = Int16.rotateRight(-1i16, -3) `assertEq!` -1i16

@test
def rotateRight07: Bool = Int16.rotateRight(8410i16, 3) `assertEq!` 17435i16

@test
def rotateRight08: Bool = Int16.rotateRight(8410i16, 19) `assertEq!` 17435i16

@test
def rotateRight09: Bool = Int16.rotateRight(8410i16, -13) `assertEq!` 17435i16

@test
def rotateRight10: Bool = Int16.rotateRight(-14949i16, 10) `assertEq!` 26353i16

@test
def rotateRight11: Bool = Int16.rotateRight(-14949i16, 26) `assertEq!` 26353i16

@test
def rotateRight12: Bool = Int16.rotateRight(-14949i16, -6) `assertEq!` 26353i16

@test
def rotateRight13: Bool = Int16.rotateRight(-6415i16, 6) `assertEq!` -14437i16

@test
def rotateRight14: Bool = Int16.rotateRight(-6415i16, 22) `assertEq!` -14437i16

@test
def rotateRight15: Bool = Int16.rotateRight(-6415i16, -10) `assertEq!` -14437i16

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01: Bool = Int16.rotateLeft(0i16, 3) `assertEq!` 0i16

@test
def rotateLeft02: Bool = Int16.rotateLeft(0i16, 19) `assertEq!` 0i16

@test
def rotateLeft03: Bool = Int16.rotateLeft(0i16, -3) `assertEq!` 0i16

@test
def rotateLeft04: Bool = Int16.rotateLeft(-1i16, 3) `assertEq!` -1i16

@test
def rotateLeft05: Bool = Int16.rotateLeft(-1i16, 19) `assertEq!` -1i16

@test
def rotateLeft06: Bool = Int16.rotateLeft(-1i16, -3) `assertEq!` -1i16

@test
def rotateLeft07: Bool = Int16.rotateLeft(8410i16, 13) `assertEq!` 17435i16

@test
def rotateLeft08: Bool = Int16.rotateLeft(8410i16, 29) `assertEq!` 17435i16

@test
def rotateLeft09: Bool = Int16.rotateLeft(8410i16, -3) `assertEq!` 17435i16

@test
def rotateLeft10: Bool = Int16.rotateLeft(-14949i16, 6) `assertEq!` 26353i16

@test
def rotateLeft11: Bool = Int16.rotateLeft(-14949i16, 22) `assertEq!` 26353i16

@test
def rotateLeft12: Bool = Int16.rotateLeft(-14949i16, -10) `assertEq!` 26353i16

@test
def rotateLeft13: Bool = Int16.rotateLeft(-6415i16, 10) `assertEq!` -14437i16

@test
def rotateLeft14: Bool = Int16.rotateLeft(-6415i16, 26) `assertEq!` -14437i16

@test
def rotateLeft15: Bool = Int16.rotateLeft(-6415i16, -6) `assertEq!` -14437i16

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01: Bool = Int16.reverse(18140i16) `assertEq!` 15202i16

@test
def reverse02: Bool = Int16.reverse(15202i16) `assertEq!` 18140i16

@test
def reverse03: Bool = Int16.reverse(-17565i16) `assertEq!` -14627i16

@test
def reverse04: Bool = Int16.reverse(-14627i16) `assertEq!` -17565i16

@test
def reverse05: Bool = Int16.reverse(-26552i16) `assertEq!` 4633i16

@test
def reverse06: Bool = Int16.reverse(4633i16) `assertEq!` -26552i16

@test
def reverse07: Bool = Int16.reverse(4i16) `assertEq!` 8192i16

@test
def reverse08: Bool = Int16.reverse(8192i16) `assertEq!` 4i16

@test
def reverse09: Bool = Int16.reverse(-1i16) `assertEq!` -1i16

@test
def reverse10: Bool = Int16.reverse(0i16) `assertEq!` 0i16

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
// getBit                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def getBit01: Bool = Int16.getBit(-1i16, 5) `assertEq!` 1

@test
def getBit02: Bool = Int16.getBit(-10000i16, 15) `assertEq!` 1

@test
def getBit03: Bool = Int16.getBit(2i16 ** 12i16, 12) `assertEq!` 1

@test
def getBit04: Bool = Int16.getBit(2i16 ** 12i16, 13) `assertEq!` 0

@test
def getBit05: Bool = Int16.getBit(2i16 ** 12i16, 11) `assertEq!` 0

@test
def getBit06: Bool = Int16.getBit(-1i16 - 2i16 ** 9i16, 9) `assertEq!` 0

@test
def getBit07: Bool = Int16.getBit(-1i16 - 2i16 ** 9i16, 10) `assertEq!` 1

@test
def getBit08: Bool = Int16.getBit(0i16, 15) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// setBit                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def setBit01: Bool = Int16.setBit(0i16, 0) `assertEq!` 1i16

@test
def setBit02: Bool = Int16.setBit(0i16, 1) `assertEq!` 2i16

@test
def setBit03: Bool = Int16.setBit(0i16, 15) `assertEq!` -32768i16

@test
def setBit04: Bool = Int16.setBit(0i16, 16) `assertEq!` 0i16

@test
def setBit05: Bool = Int16.setBit(-1418i16, 31) `assertEq!` -1418i16

@test
def setBit07: Bool = Int16.setBit(-17i16, 4) `assertEq!` -1i16

@test
def setBit08: Bool = Int16.setBit(-1025i16, 10) `assertEq!` -1i16

@test
def setBit09: Bool = Int16.setBit(-17i16, 63) `assertEq!` -17i16

@test
def setBit10: Bool = Int16.setBit(-17i16, 30) `assertEq!` -17i16

/////////////////////////////////////////////////////////////////////////////
// clearBit                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def clearBit01: Bool = Int16.clearBit(1i16, 0) `assertEq!` 0i16

@test
def clearBit02: Bool = Int16.clearBit(1i16, 1) `assertEq!` 1i16

@test
def clearBit03: Bool = Int16.clearBit(2i16, 1) `assertEq!` 0i16

@test
def clearBit04: Bool = Int16.clearBit(2i16, 2) `assertEq!` 2i16

@test
def clearBit05: Bool = Int16.clearBit(0i16, 31) `assertEq!` 0i16

@test
def clearBit06: Bool = Int16.clearBit(0i16, 30) `assertEq!` 0i16

@test
def clearBit07: Bool = Int16.clearBit(1i16 <<< 15, 15) `assertEq!` 0i16

@test
def clearBit08: Bool = Int16.clearBit(27957i16, 11) `assertEq!` 25909i16

@test
def clearBit09: Bool = Int16.clearBit(-1i16, 0) `assertEq!` -2i16

@test
def clearBit10: Bool = Int16.clearBit(-1i16, 1) `assertEq!` -3i16

@test
def clearBit11: Bool = Int16.clearBit(-1i16, 5) `assertEq!` -33i16

@test
def clearBit12: Bool = Int16.clearBit(-1i16, 15) `assertEq!` ~~~(1i16 <<< 15)

@test
def clearBit13: Bool = Int16.clearBit(-1i16, 32) `assertEq!` -2i16

/////////////////////////////////////////////////////////////////////////////
// flipBit                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flipBit01: Bool = Int16.flipBit(1i16, 0) `assertEq!` 0i16

@test
def flipBit02: Bool = Int16.flipBit(1i16, 1) `assertEq!` 3i16

@test
def flipBit03: Bool = Int16.flipBit(2i16, 1) `assertEq!` 0i16

@test
def flipBit04: Bool = Int16.flipBit(2i16, 2) `assertEq!` 6i16

@test
def flipBit05: Bool = Int16.flipBit(0i16, 15) `assertEq!` -32768i16

@test
def flipBit06: Bool = Int16.flipBit(0i16, 16) `assertEq!` 0i16

@test
def flipBit07: Bool = Int16.flipBit(1i16 <<< 15, 15) `assertEq!` 0i16

@test
def flipBit08: Bool = Int16.flipBit(-1i16, 0) `assertEq!` -2i16

@test
def flipBit9: Bool = Int16.flipBit(-1i16, 1) `assertEq!` -3i16

@test
def flipBit10: Bool = Int16.flipBit(-1i16, 5) `assertEq!` -33i16

@test
def flipBit11: Bool = Int16.flipBit(-1i16, 13) `assertEq!` (-1i16-(1i16 <<< 13))

@test
def flipBit12: Bool = Int16.flipBit(-1i16, 32) `assertEq!` -2i16