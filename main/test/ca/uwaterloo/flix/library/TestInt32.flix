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
def maxValue01(): Bool = Int32.maxValue() `assertEq!` ~~~(1 <<< 31)

/////////////////////////////////////////////////////////////////////////////
// minValue                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def minValue01(): Bool = Int32.minValue() `assertEq!` (1 <<< 31)

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = Int32.size() `assertEq!` 32

/////////////////////////////////////////////////////////////////////////////
// min                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def min01(): Bool = Int32.min(1, 2) `assertEq!` 1

@test
def min02(): Bool = Int32.min(2, -1) `assertEq!` -1

@test
def min03(): Bool = Int32.min(-33, -66) `assertEq!` -66

/////////////////////////////////////////////////////////////////////////////
// max                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def max01(): Bool = Int32.max(48, 49) `assertEq!` 49

@test
def max02(): Bool = Int32.max(4, -16) `assertEq!` 4

@test
def max03(): Bool = Int32.max(-34, -16) `assertEq!` -16

/////////////////////////////////////////////////////////////////////////////
// abs                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def abs01(): Bool = Int32.abs(31) `assertEq!` 31

@test
def abs02(): Bool = Int32.abs(-31) `assertEq!` 31

@test
def abs03(): Bool = Int32.abs(2147483647) `assertEq!` 2147483647

@test
def abs04(): Bool = Int32.abs(-2147483647) `assertEq!` 2147483647

@test
def abs05(): Bool = Int32.abs(0) `assertEq!` 0

@test
def abs06(): Bool = Int32.abs(-2147483648) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// dist                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def dist01(): Bool = Int32.dist(31, -7) `assertEq!` 38

@test
def dist02(): Bool = Int32.dist(-44, -1) `assertEq!` 43

@test
def dist03(): Bool = Int32.dist(-2, -2) `assertEq!` 0

@test
def dist04(): Bool = Int32.dist(-2147483648, -1) `assertEq!` 2147483647

@test
def dist05(): Bool = Int32.dist(-2147483648, 0) `assertEq!` -1

@test
def dist06(): Bool = Int32.dist(-123, 4) `assertEq!` 127

@test
def dist07(): Bool = Int32.dist(-2147483643, 5) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// compare                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def compare01(): Bool = Int32.compare(-1, 44) `assertEq!` -1

@test
def compare02(): Bool = Int32.compare(-1, -44) `assertEq!` 1

@test
def compare03(): Bool = Int32.compare(88, 88) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// signum                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def signum01(): Bool = Int32.signum(-22) `assertEq!` -1

@test
def signum02(): Bool = Int32.signum(22) `assertEq!` 1

@test
def signum03(): Bool = Int32.signum(0) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// logicalRightShift                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def logicalRightShift01(): Bool = Int32.logicalRightShift(2233, 4) `assertEq!` (2233 >>> 4)

@test
def logicalRightShift02(): Bool = Int32.logicalRightShift(223366, 33) `assertEq!` (223366 >>> 33)

@test
def logicalRightShift03(): Bool = Int32.logicalRightShift(2233, -8) `assertEq!` (2233 >>> -8)

@test
def logicalRightShift04(): Bool = Int32.logicalRightShift(2233, 4) `assertEq!` (2233 >>> 4)

@test
def logicalRightShift05(): Bool = Int32.logicalRightShift(-1, 1) `assertEq!` 2147483647

@test
def logicalRightShift06(): Bool = Int32.logicalRightShift(-35, 4) `assertEq!` 268435453

@test
def logicalRightShift07(): Bool = Int32.logicalRightShift(-2777, -2777) `assertEq!` 33554410

@test
def logicalRightShift08(): Bool = Int32.logicalRightShift(-233, 64) `assertEq!` -233

@test
def logicalRightShift09(): Bool = Int32.logicalRightShift(-27, 0) `assertEq!` -27

@test
def logicalRightShift10(): Bool = Int32.logicalRightShift(2147483647, 1) `assertEq!` 1073741823

/////////////////////////////////////////////////////////////////////////////
// bitCount                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def bitCount01(): Bool = Int32.bitCount(19909) `assertEq!` 8

@test
def bitCount02(): Bool = Int32.bitCount(-1) `assertEq!` 32

@test
def bitCount03(): Bool = Int32.bitCount(-999888888) `assertEq!` 12

@test
def bitCount04(): Bool = Int32.bitCount(0) `assertEq!` 0

@test
def bitCount05(): Bool = Int32.bitCount(2147483647) `assertEq!` 31

@test
def bitCount06(): Bool = Int32.bitCount(-2147483648) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01(): Bool = Int32.rotateLeft(-1698265258, 4) `assertEq!` -1402440343

@test
def rotateLeft02(): Bool = Int32.rotateLeft(-1836677290, 5) `assertEq!` 1355868882

@test
def rotateLeft03(): Bool = Int32.rotateLeft(1478293001, 24) `assertEq!` 156769526

@test
def rotateLeft04(): Bool = Int32.rotateLeft(-12267711, -36) `assertEq!` 536104180

@test
def rotateLeft05(): Bool = Int32.rotateLeft(0, 87) `assertEq!` 0

@test
def rotateLeft06(): Bool = Int32.rotateLeft(-1, 87) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01(): Bool = Int32.rotateRight(-1698265258, 4) `assertEq!` 1772906613

@test
def rotateRight02(): Bool = Int32.rotateRight(-1836677290, 5) `assertEq!` -1265355718

@test
def rotateRight03(): Bool = Int32.rotateRight(1478293001, 24) `assertEq!` 485886296

@test
def rotateRight04(): Bool = Int32.rotateRight(-12267711, -36) `assertEq!` -196283361

@test
def rotateRight05(): Bool = Int32.rotateRight(0, 87) `assertEq!` 0

@test
def rotateRight06(): Bool = Int32.rotateRight(-1, 87) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool = Int32.reverse(1789570000) `assertEq!` 198530390

@test
def reverse02(): Bool = Int32.reverse(198530390) `assertEq!` 1789570000

@test
def reverse03(): Bool = Int32.reverse(-1036473682) `assertEq!` 1968512067

@test
def reverse04(): Bool = Int32.reverse(1968512067) `assertEq!` -1036473682

@test
def reverse05(): Bool = Int32.reverse(137) `assertEq!` -1862270976

@test
def reverse06(): Bool = Int32.reverse(-1862270976) `assertEq!` 137

@test
def reverse07(): Bool = Int32.reverse(4) `assertEq!` 536870912

@test
def reverse08(): Bool = Int32.reverse(536870912) `assertEq!` 4

@test
def reverse09(): Bool = Int32.reverse(-1) `assertEq!` -1

@test
def reverse10(): Bool = Int32.reverse(0) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// highestOneBitPosition                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBitPosition01(): Bool = Int32.highestOneBitPosition(-1) `assertEq!` 31

@test
def highestOneBitPosition02(): Bool = Int32.highestOneBitPosition(-2147483648) `assertEq!` 31

@test
def highestOneBitPosition03(): Bool = Int32.highestOneBitPosition(1) `assertEq!` 0

@test
def highestOneBitPosition04(): Bool = Int32.highestOneBitPosition(2) `assertEq!` 1

@test
def highestOneBitPosition05(): Bool = Int32.highestOneBitPosition(3) `assertEq!` 1

@test
def highestOneBitPosition06(): Bool = Int32.highestOneBitPosition(20) `assertEq!` 4

@test
def highestOneBitPosition07(): Bool = Int32.highestOneBitPosition(2 ** 30) `assertEq!` 30

@test
def highestOneBitPosition08(): Bool = Int32.highestOneBitPosition(2147483647) `assertEq!` 30

@test
def highestOneBitPosition09(): Bool = Int32.highestOneBitPosition(0) `assertEq!` -1

@test
def highestOneBitPosition10(): Bool = Int32.highestOneBitPosition(2 ** 18 + 2 ** 23) `assertEq!` 23

@test
def highestOneBitPosition11(): Bool = Int32.highestOneBitPosition(2 ** 3 + 2 ** 7) `assertEq!` 7

@test
def highestOneBitPosition12(): Bool = Int32.highestOneBitPosition(-32767) `assertEq!` 31

/////////////////////////////////////////////////////////////////////////////
// highestOneBit                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def highestOneBit01(): Bool = Int32.highestOneBit(-1) `assertEq!` (1 <<< 31)

@test
def highestOneBit02(): Bool = Int32.highestOneBit(-2147483000) `assertEq!` (1 <<< 31)

@test
def highestOneBit03(): Bool = Int32.highestOneBit(-77) `assertEq!` (1 <<< 31)

@test
def highestOneBit04(): Bool = Int32.highestOneBit(2) `assertEq!` 2

@test
def highestOneBit05(): Bool = Int32.highestOneBit(3) `assertEq!` 2

@test
def highestOneBit06(): Bool = Int32.highestOneBit(20) `assertEq!` 16

@test
def highestOneBit07(): Bool = Int32.highestOneBit(2 ** 30) `assertEq!` 1073741824

@test
def highestOneBit08(): Bool = Int32.highestOneBit(2147483647) `assertEq!` 1073741824

@test
def highestOneBit09(): Bool = Int32.highestOneBit(0) `assertEq!` 0

@test
def highestOneBit10(): Bool = Int32.highestOneBit(2 ** 18 + 2 ** 24) `assertEq!` 16777216

/////////////////////////////////////////////////////////////////////////////
// numberOfLeadingZeros                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfLeadingZeros01(): Bool = Int32.numberOfLeadingZeros(0) `assertEq!` 32

@test
def numberOfLeadingZeros02(): Bool = Int32.numberOfLeadingZeros(1 <<< 31) `assertEq!` 0

@test
def numberOfLeadingZeros03(): Bool = Int32.numberOfLeadingZeros(-1) `assertEq!` 0

@test
def numberOfLeadingZeros04(): Bool = Int32.numberOfLeadingZeros(~~~(1 <<< 31)) `assertEq!` 1

@test
def numberOfLeadingZeros05(): Bool = Int32.numberOfLeadingZeros(2 ** 29) `assertEq!` 2

@test
def numberOfLeadingZeros06(): Bool = Int32.numberOfLeadingZeros(2 ** 24 + 2 ** 18) `assertEq!` 7

@test
def numberOfLeadingZeros07(): Bool = Int32.numberOfLeadingZeros(2 ** 7 + 2 ** 6) `assertEq!` 24

@test
def numberOfLeadingZeros08(): Bool = Int32.numberOfLeadingZeros(20) `assertEq!` 27

@test
def numberOfLeadingZeros09(): Bool = Int32.numberOfLeadingZeros(1) `assertEq!` 31

/////////////////////////////////////////////////////////////////////////////
// lowestOneBitPosition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBitPosition01(): Bool = Int32.lowestOneBitPosition(-1) `assertEq!` 0

@test
def lowestOneBitPosition02(): Bool = Int32.lowestOneBitPosition(-2147483648) `assertEq!` 31

@test
def lowestOneBitPosition03(): Bool = Int32.lowestOneBitPosition(1) `assertEq!` 0

@test
def lowestOneBitPosition04(): Bool = Int32.lowestOneBitPosition(2) `assertEq!` 1

@test
def lowestOneBitPosition05(): Bool = Int32.lowestOneBitPosition(3) `assertEq!` 0

@test
def lowestOneBitPosition06(): Bool = Int32.lowestOneBitPosition(4) `assertEq!` 2

@test
def lowestOneBitPosition07(): Bool = Int32.lowestOneBitPosition(12) `assertEq!` 2

@test
def lowestOneBitPosition08(): Bool = Int32.lowestOneBitPosition(2 ** 30) `assertEq!` 30

@test
def lowestOneBitPosition09(): Bool = Int32.lowestOneBitPosition(2 ** 23 + 2 ** 18) `assertEq!` 18

@test
def lowestOneBitPosition10(): Bool = Int32.lowestOneBitPosition(2147483647) `assertEq!` 0

@test
def lowestOneBitPosition11(): Bool = Int32.lowestOneBitPosition(0) `assertEq!` -1

/////////////////////////////////////////////////////////////////////////////
// lowestOneBit                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def lowestOneBit01(): Bool = Int32.lowestOneBit(-1) `assertEq!` 1

@test
def lowestOneBit02(): Bool = Int32.lowestOneBit(-2147483648) `assertEq!` (1 <<< 31)

@test
def lowestOneBit03(): Bool = Int32.lowestOneBit(1) `assertEq!` 1

@test
def lowestOneBit04(): Bool = Int32.lowestOneBit(2) `assertEq!` 2

@test
def lowestOneBit05(): Bool = Int32.lowestOneBit(6) `assertEq!` 2

@test
def lowestOneBit06(): Bool = Int32.lowestOneBit(12) `assertEq!` 4

@test
def lowestOneBit07(): Bool = Int32.lowestOneBit(2 ** 6 + 2 ** 18 + 2 ** 27) `assertEq!` 64

@test
def lowestOneBit08(): Bool = Int32.lowestOneBit(2 ** 30) `assertEq!` 1073741824

@test
def lowestOneBit09(): Bool = Int32.lowestOneBit(2 ** 18 + 2 ** 23) `assertEq!` 262144

@test
def lowestOneBit10(): Bool = Int32.lowestOneBit(2147483647) `assertEq!` 1

@test
def lowestOneBit11(): Bool = Int32.lowestOneBit(0) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// numberOfTrailingZeros                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def numberOfTrailingZeros01(): Bool = Int32.numberOfTrailingZeros(0) `assertEq!` 32

@test
def numberOfTrailingZeros02(): Bool = Int32.numberOfTrailingZeros(3) `assertEq!` 0

@test
def numberOfTrailingZeros03(): Bool = Int32.numberOfTrailingZeros(42) `assertEq!` 1

@test
def numberOfTrailingZeros04(): Bool = Int32.numberOfTrailingZeros(2 ** 16 + 2 ** 22) `assertEq!` 16

@test
def numberOfTrailingZeros05(): Bool = Int32.numberOfTrailingZeros(2 ** 30) `assertEq!` 30

@test
def numberOfTrailingZeros06(): Bool = Int32.numberOfTrailingZeros(-1) `assertEq!` 0

@test
def numberOfTrailingZeros07(): Bool = Int32.numberOfTrailingZeros(-2147483648) `assertEq!` 31

/////////////////////////////////////////////////////////////////////////////
// getBit                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def getBit01(): Bool = Int32.getBit(-1, 5) `assertEq!` 1

@test
def getBit02(): Bool = Int32.getBit(-1000000, 31) `assertEq!` 1

@test
def getBit03(): Bool = Int32.getBit(2 ** 23, 23) `assertEq!` 1

@test
def getBit04(): Bool = Int32.getBit(2 ** 23, 24) `assertEq!` 0

@test
def getBit05(): Bool = Int32.getBit(2 ** 23, 22) `assertEq!` 0

@test
def getBit06(): Bool = Int32.getBit(-1 - 2 ** 25, 25) `assertEq!` 0

@test
def getBit07(): Bool = Int32.getBit(-1 - 2 ** 25, 26) `assertEq!` 1

@test
def getBit08(): Bool = Int32.getBit(0, 15) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// setBit                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def setBit01(): Bool = Int32.setBit(0, 0) `assertEq!` 1

@test
def setBit02(): Bool = Int32.setBit(0, 1) `assertEq!` 2

@test
def setBit03(): Bool = Int32.setBit(0, 31) `assertEq!` -2147483648

@test
def setBit04(): Bool = Int32.setBit(0, 30) `assertEq!` 1073741824

@test
def setBit05(): Bool = Int32.setBit(-1418638422, 31) `assertEq!` -1418638422

@test
def setBit07(): Bool = Int32.setBit(-17, 4) `assertEq!` -1

@test
def setBit08(): Bool = Int32.setBit(-1025, 10) `assertEq!` -1

@test
def setBit09(): Bool = Int32.setBit(-17, 31) `assertEq!` -17

@test
def setBit10(): Bool = Int32.setBit(-17, 30) `assertEq!` -17

@test
def setBit11(): Bool = Int32.setBit(0, 32) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// clearBit                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def clearBit01(): Bool = Int32.clearBit(1, 0) `assertEq!` 0

@test
def clearBit02(): Bool = Int32.clearBit(1, 1) `assertEq!` 1

@test
def clearBit03(): Bool = Int32.clearBit(2, 1) `assertEq!` 0

@test
def clearBit04(): Bool = Int32.clearBit(2, 2) `assertEq!` 2

@test
def clearBit05(): Bool = Int32.clearBit(0, 31) `assertEq!` 0

@test
def clearBit06(): Bool = Int32.clearBit(0, 30) `assertEq!` 0

@test
def clearBit07(): Bool = Int32.clearBit(-2147483648, 31) `assertEq!` 0

@test
def clearBit08(): Bool = Int32.clearBit(442931848, 27) `assertEq!` 308714120

@test
def clearBit09(): Bool = Int32.clearBit(-1, 0) `assertEq!` -2

@test
def clearBit10(): Bool = Int32.clearBit(-1, 1) `assertEq!` -3

@test
def clearBit11(): Bool = Int32.clearBit(-1, 5) `assertEq!` -33

@test
def clearBit12(): Bool = Int32.clearBit(-1, 31) `assertEq!` 2147483647

@test
def clearBit13(): Bool = Int32.clearBit(-1, 32) `assertEq!` -2

/////////////////////////////////////////////////////////////////////////////
// flipBit                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flipBit01(): Bool = Int32.flipBit(1, 0) `assertEq!` 0

@test
def flipBit02(): Bool = Int32.flipBit(1, 1) `assertEq!` 3

@test
def flipBit03(): Bool = Int32.flipBit(2, 1) `assertEq!` 0

@test
def flipBit04(): Bool = Int32.flipBit(2, 2) `assertEq!` 6

@test
def flipBit05(): Bool = Int32.flipBit(0, 31) `assertEq!` -2147483648

@test
def flipBit06(): Bool = Int32.flipBit(0, 30) `assertEq!` 1073741824

@test
def flipBit07(): Bool = Int32.flipBit(-2147483648, 31) `assertEq!` 0

@test
def flipBit08(): Bool = Int32.flipBit(-1, 0) `assertEq!` -2

@test
def flipBit9(): Bool = Int32.flipBit(-1, 1) `assertEq!` -3

@test
def flipBit10(): Bool = Int32.flipBit(-1, 5) `assertEq!` -33

@test
def flipBit11(): Bool = Int32.flipBit(-1, 31) `assertEq!` 2147483647

@test
def flipBit12(): Bool = Int32.flipBit(-1, 32) `assertEq!` -2