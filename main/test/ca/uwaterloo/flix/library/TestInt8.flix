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

mod TestInt8 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool = mul(1i8, 0i8) == 0i8

    @Test
    def mul02(): Bool = mul(0i8, 1i8) == 0i8

    @Test
    def mul03(): Bool = mul(0i8, 0i8) == 0i8

    @Test
    def mul04(): Bool = mul(-64i8, 2i8) == -128i8

    @Test
    def mul05(): Bool = mul(127i8, 1i8) == 127i8

    @Test
    def mul06(): Bool = mul(5i8, 11i8) == 55i8

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Bool = Int8.maxValue() == Int8.bitwiseNot(Int8.leftShift(1i8, 7))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Bool = Int8.minValue() == Int8.leftShift(1i8, 7)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Bool = Int8.size() == 8

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = Int8.min(1i8, 2i8) == 1i8

    @Test
    def min02(): Bool = Int8.min(2i8, -1i8) == -1i8

    @Test
    def min03(): Bool = Int8.min(-33i8, -66i8) == -66i8

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = Int8.max(48i8, 49i8) == 49i8

    @Test
    def max02(): Bool = Int8.max(4i8, -16i8) == 4i8

    @Test
    def max03(): Bool = Int8.max(-34i8, -16i8) == -16i8

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Bool = Int8.abs(31i8) == 31i8

    @Test
    def abs02(): Bool = Int8.abs(-31i8) == 31i8

    @Test
    def abs03(): Bool = Int8.abs(127i8) == 127i8

    @Test
    def abs04(): Bool = Int8.abs(-127i8) == 127i8

    @Test
    def abs05(): Bool = Int8.abs(0i8) == 0i8

    @Test
    def abs06(): Bool = Int8.abs(-128i8) == -1i8

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mod01(): Bool = (12i8 `Int8.mod` 2i8) == 0i8

    @Test
    def mod02(): Bool = (12i8 `Int8.mod` 5i8) == 2i8

    @Test
    def mod03(): Bool = (-12i8 `Int8.mod` 5i8) == 3i8

    @Test
    def mod04(): Bool = (12i8 `Int8.mod` -5i8) == -3i8

    @Test
    def mod05(): Bool = (-12i8 `Int8.mod` -5i8) == -2i8

    @Test
    def mod06(): Bool = (1i8 `Int8.mod` 0i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rem01(): Bool = (12i8 `Int8.rem` 2i8) == 0i8

    @Test
    def rem02(): Bool = (12i8 `Int8.rem` 5i8) == 2i8

    @Test
    def rem03(): Bool = (-12i8 `Int8.rem` 5i8) == -2i8

    @Test
    def rem04(): Bool = (12i8 `Int8.rem` -5i8) == 2i8

    @Test
    def rem05(): Bool = (-12i8 `Int8.rem` -5i8) == -2i8

    @Test
    def rem06(): Bool = (1i8 `Int8.rem` 0i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Bool = Int8.dist(31i8, -7i8) == 38i8

    @Test
    def dist02(): Bool = Int8.dist(-44i8, -1i8) == 43i8

    @Test
    def dist03(): Bool = Int8.dist(-2i8, -2i8) == 0i8

    @Test
    def dist04(): Bool = Int8.dist(-128i8, -1i8) == 127i8

    @Test
    def dist05(): Bool = Int8.dist(-128i8, 0i8) == -1i8

    @Test
    def dist06(): Bool = Int8.dist(-123i8, 4i8) == 127i8

    @Test
    def dist07(): Bool = Int8.dist(-123i8, 5i8) == -1i8

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Bool = Int8.compare(-1i8, 44i8) == -1

    @Test
    def compare02(): Bool = Int8.compare(-1i8, -44i8) == 1

    @Test
    def compare03(): Bool = Int8.compare(88i8, 88i8) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Bool = Int8.signum(-22i8) == -1

    @Test
    def signum02(): Bool = Int8.signum(22i8) == 1

    @Test
    def signum03(): Bool = Int8.signum(0i8) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = Int8.pow(0i8, 0i8) == 1i8

    @Test
    def pow02(): Bool = Int8.pow(0i8, 1i8) == 0i8

    @Test
    def pow03(): Bool = Int8.pow(1i8, 0i8) == 1i8

    @Test
    def pow04(): Bool = Int8.pow(1i8, 1i8) == 1i8

    @Test
    def pow05(): Bool = Int8.pow(5i8, 2i8) == 25i8

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Bool = Int8.leftShift(8i8, 0) == 8i8

    @Test
    def testBitwiseLeftShift02(): Bool = Int8.leftShift(8i8, 2) == 32i8

    @Test
    def testBitwiseLeftShift03(): Bool = Int8.leftShift(8i8, 4) == -128i8

    @Test
    def testBitwiseLeftShift04(): Bool = Int8.leftShift(8i8, 5) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Bool = Int8.rightShift(120i8, 0) == 120i8

    @Test
    def testBitwiseRightShift02(): Bool = Int8.rightShift(120i8, 2) == 30i8

    @Test
    def testBitwiseRightShift03(): Bool = Int8.rightShift(120i8, 7) == 0i8

    @Test
    def testBitwiseRightShift04(): Bool = Int8.rightShift(-120i8, 2) == -30i8

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Bool = Int8.logicalRightShift(dist = 4, 98i8) == Int8.rightShift(98i8, 4)

    @Test
    def logicalRightShift02(): Bool = Int8.logicalRightShift(dist = 33, 127i8) == Int8.rightShift(127i8, 33)

    @Test
    def logicalRightShift03(): Bool = Int8.logicalRightShift(dist = -8, 93i8) == Int8.rightShift(93i8, -8)

    @Test
    def logicalRightShift04(): Bool = Int8.logicalRightShift(dist = 4, 93i8) == Int8.rightShift(93i8, 4)

    @Test
    def logicalRightShift05(): Bool = Int8.logicalRightShift(dist = 1, -1i8) == 127i8

    @Test
    def logicalRightShift06(): Bool = Int8.logicalRightShift(dist = 4, -35i8) == 13i8

    @Test
    def logicalRightShift07(): Bool = Int8.logicalRightShift(dist = -2780, -44i8) == 13i8

    @Test
    def logicalRightShift08(): Bool = Int8.logicalRightShift(dist = 64, -98i8) == -98i8

    @Test
    def logicalRightShift09(): Bool = Int8.logicalRightShift(dist = 0, -27i8) == -27i8

    @Test
    def logicalRightShift10(): Bool = Int8.logicalRightShift(dist = 1, 127i8) == 63i8

    @Test
    def logicalRightShift11(): Bool = Int8.logicalRightShift(dist = 9, 127i8) == 0i8

    @Test
    def logicalRightShift12(): Bool = Int8.logicalRightShift(dist = 8, 127i8) == 0i8

    @Test
    def logicalRightShift13(): Bool = Int8.logicalRightShift(dist = 9, -128i8) == 0i8

    @Test
    def logicalRightShift14(): Bool = Int8.logicalRightShift(dist = 9, -128i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Bool = Int8.bitCount(70i8) == 3

    @Test
    def bitCount02(): Bool = Int8.bitCount(-1i8) == 8

    @Test
    def bitCount03(): Bool = Int8.bitCount(-99i8) == 5

    @Test
    def bitCount04(): Bool = Int8.bitCount(0i8) == 0

    @Test
    def bitCount05(): Bool = Int8.bitCount(127i8) == 7

    @Test
    def bitCount06(): Bool = Int8.bitCount(-128i8) == 1

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Bool = Int8.rotateRight(dist = 3, 0i8) == 0i8

    @Test
    def rotateRight02(): Bool = Int8.rotateRight(dist = 19, 0i8) == 0i8

    @Test
    def rotateRight03(): Bool = Int8.rotateRight(dist = -3, 0i8) == 0i8

    @Test
    def rotateRight04(): Bool = Int8.rotateRight(dist = 3, -1i8) == -1i8

    @Test
    def rotateRight05(): Bool = Int8.rotateRight(dist = 19, -1i8) == -1i8

    @Test
    def rotateRight06(): Bool = Int8.rotateRight(dist = -3, -1i8) == -1i8

    @Test
    def rotateRight07(): Bool = Int8.rotateRight(dist = 3, 106i8) == 77i8

    @Test
    def rotateRight08(): Bool = Int8.rotateRight(dist = 11, 106i8) == 77i8

    @Test
    def rotateRight09(): Bool = Int8.rotateRight(dist = -5, 106i8) == 77i8

    @Test
    def rotateRight10(): Bool = Int8.rotateRight(dist = 7, -75i8) == 107i8

    @Test
    def rotateRight11(): Bool = Int8.rotateRight(dist = 23, -75i8) == 107i8

    @Test
    def rotateRight12(): Bool = Int8.rotateRight(dist = -1, -75i8) == 107i8

    @Test
    def rotateRight13(): Bool = Int8.rotateRight(dist = 6, -17i8) == -65i8

    @Test
    def rotateRight14(): Bool = Int8.rotateRight(dist = 22, -17i8) == -65i8

    @Test
    def rotateRight15(): Bool = Int8.rotateRight(dist = -10, -17i8) == -65i8

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Bool = Int8.rotateLeft(dist = 5, 0i8) == 0i8

    @Test
    def rotateLeft02(): Bool = Int8.rotateLeft(dist = 13, 0i8) == 0i8

    @Test
    def rotateLeft03(): Bool = Int8.rotateLeft(dist = -3, 0i8) == 0i8

    @Test
    def rotateLeft04(): Bool = Int8.rotateLeft(dist = 3, -1i8) == -1i8

    @Test
    def rotateLeft05(): Bool = Int8.rotateLeft(dist = 19, -1i8) == -1i8

    @Test
    def rotateLeft06(): Bool = Int8.rotateLeft(dist = -3, -1i8) == -1i8

    @Test
    def rotateLeft07(): Bool = Int8.rotateLeft(dist = 5, 106i8) == 77i8

    @Test
    def rotateLeft08(): Bool = Int8.rotateLeft(dist = 13, 106i8) == 77i8

    @Test
    def rotateLeft09(): Bool = Int8.rotateLeft(dist = -3, 106i8) == 77i8

    @Test
    def rotateLeft10(): Bool = Int8.rotateLeft(dist = 1, -75i8) == 107i8

    @Test
    def rotateLeft11(): Bool = Int8.rotateLeft(dist = 9, -75i8) == 107i8

    @Test
    def rotateLeft12(): Bool = Int8.rotateLeft(dist = -7, -75i8) == 107i8

    @Test
    def rotateLeft13(): Bool = Int8.rotateLeft(dist = 2, -17i8) == -65i8

    @Test
    def rotateLeft14(): Bool = Int8.rotateLeft(dist = 10, -17i8) == -65i8

    @Test
    def rotateLeft15(): Bool = Int8.rotateLeft(dist = -6, -17i8) == -65i8

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Bool = Int8.reverse(58i8) == 92i8

    @Test
    def reverse02(): Bool = Int8.reverse(92i8) == 58i8

    @Test
    def reverse03(): Bool = Int8.reverse(-35i8) == -69i8

    @Test
    def reverse04(): Bool = Int8.reverse(-69i8) == -35i8

    @Test
    def reverse05(): Bool = Int8.reverse(-72i8) == 29i8

    @Test
    def reverse06(): Bool = Int8.reverse(29i8) == -72i8

    @Test
    def reverse07(): Bool = Int8.reverse(4i8) == 32i8

    @Test
    def reverse08(): Bool = Int8.reverse(32i8) == 4i8

    @Test
    def reverse09(): Bool = Int8.reverse(-1i8) == -1i8

    @Test
    def reverse10(): Bool = Int8.reverse(0i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Bool = Int8.highestOneBitPosition(-1i8) == 7

    @Test
    def highestOneBitPosition02(): Bool = Int8.highestOneBitPosition(-128i8) == 7

    @Test
    def highestOneBitPosition03(): Bool = Int8.highestOneBitPosition(1i8) == 0

    @Test
    def highestOneBitPosition04(): Bool = Int8.highestOneBitPosition(2i8) == 1

    @Test
    def highestOneBitPosition05(): Bool = Int8.highestOneBitPosition(3i8) == 1

    @Test
    def highestOneBitPosition06(): Bool = Int8.highestOneBitPosition(20i8) == 4

    @Test
    def highestOneBitPosition07(): Bool = Int8.highestOneBitPosition(Int8.pow(2i8, 6i8)) == 6

    @Test
    def highestOneBitPosition08(): Bool = Int8.highestOneBitPosition(127i8) == 6

    @Test
    def highestOneBitPosition09(): Bool = Int8.highestOneBitPosition(0i8) == -1

    @Test
    def highestOneBitPosition10(): Bool = Int8.highestOneBitPosition(Int8.pow(2i8,3i8) + Int8.pow(2i8, 5i8)) == 5

    @Test
    def highestOneBitPosition11(): Bool = Int8.highestOneBitPosition(Int8.pow(2i8, 3i8) + Int8.pow(2i8, 4i8)) == 4

    @Test
    def highestOneBitPosition12(): Bool = Int8.highestOneBitPosition(-10i8) == 7

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Bool = Int8.highestOneBit(-1i8) == Int8.leftShift(1i8, 7)

    @Test
    def highestOneBit02(): Bool = Int8.highestOneBit(-128i8) == Int8.leftShift(1i8, 7)

    @Test
    def highestOneBit03(): Bool = Int8.highestOneBit(-77i8) == Int8.leftShift(1i8, 7)

    @Test
    def highestOneBit04(): Bool = Int8.highestOneBit(2i8) == 2i8

    @Test
    def highestOneBit05(): Bool = Int8.highestOneBit(3i8) == 2i8

    @Test
    def highestOneBit06(): Bool = Int8.highestOneBit(20i8) == 16i8

    @Test
    def highestOneBit07(): Bool = Int8.highestOneBit(Int8.pow(2i8, 2i8) + Int8.pow(2i8, 5i8)) == 32i8

    @Test
    def highestOneBit08(): Bool = Int8.highestOneBit(127i8) == 64i8

    @Test
    def highestOneBit09(): Bool = Int8.highestOneBit(0i8) == 0i8

    @Test
    def highestOneBit10(): Bool = Int8.highestOneBit(Int8.pow(2i8, 5i8) + Int8.pow(2i8, 6i8)) == 64i8

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Bool = Int8.numberOfLeadingZeros(0i8) == 8

    @Test
    def numberOfLeadingZeros02(): Bool = Int8.numberOfLeadingZeros(-128i8) == 0

    @Test
    def numberOfLeadingZeros03(): Bool = Int8.numberOfLeadingZeros(-1i8) == 0

    @Test
    def numberOfLeadingZeros04(): Bool = Int8.numberOfLeadingZeros(127i8) == 1

    @Test
    def numberOfLeadingZeros05(): Bool = Int8.numberOfLeadingZeros(Int8.pow(2i8, 6i8)) == 1

    @Test
    def numberOfLeadingZeros06(): Bool = Int8.numberOfLeadingZeros(Int8.pow(2i8, 3i8) + Int8.pow(2i8, 6i8)) == 1

    @Test
    def numberOfLeadingZeros07(): Bool = Int8.numberOfLeadingZeros(Int8.pow(2i8, 4i8) + Int8.pow(2i8, 5i8)) == 2

    @Test
    def numberOfLeadingZeros08(): Bool = Int8.numberOfLeadingZeros(20i8) == 3

    @Test
    def numberOfLeadingZeros09(): Bool = Int8.numberOfLeadingZeros(1i8) == 7

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Bool = Int8.lowestOneBitPosition(-1i8) == 0

    @Test
    def lowestOneBitPosition02(): Bool = Int8.lowestOneBitPosition(-128i8) == 7

    @Test
    def lowestOneBitPosition03(): Bool = Int8.lowestOneBitPosition(1i8) == 0

    @Test
    def lowestOneBitPosition04(): Bool = Int8.lowestOneBitPosition(2i8) == 1

    @Test
    def lowestOneBitPosition05(): Bool = Int8.lowestOneBitPosition(3i8) == 0

    @Test
    def lowestOneBitPosition06(): Bool = Int8.lowestOneBitPosition(4i8) == 2

    @Test
    def lowestOneBitPosition07(): Bool = Int8.lowestOneBitPosition(12i8) == 2

    @Test
    def lowestOneBitPosition08(): Bool = Int8.lowestOneBitPosition(Int8.pow(2i8, 4i8)) == 4

    @Test
    def lowestOneBitPosition09(): Bool = Int8.lowestOneBitPosition(Int8.pow(2i8, 4i8) + Int8.pow(2i8, 6i8)) == 4

    @Test
    def lowestOneBitPosition10(): Bool = Int8.lowestOneBitPosition(127i8) == 0

    @Test
    def lowestOneBitPosition11(): Bool = Int8.lowestOneBitPosition(0i8) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Bool = Int8.lowestOneBit(-1i8) == 1i8

    @Test
    def lowestOneBit02(): Bool = Int8.lowestOneBit(-128i8) == Int8.leftShift(1i8, 7)

    @Test
    def lowestOneBit03(): Bool = Int8.lowestOneBit(1i8) == 1i8

    @Test
    def lowestOneBit04(): Bool = Int8.lowestOneBit(2i8) == 2i8

    @Test
    def lowestOneBit05(): Bool = Int8.lowestOneBit(6i8) == 2i8

    @Test
    def lowestOneBit06(): Bool = Int8.lowestOneBit(12i8) == 4i8

    @Test
    def lowestOneBit07(): Bool = Int8.lowestOneBit(Int8.pow(2i8, 6i8)) == 64i8

    @Test
    def lowestOneBit08(): Bool = Int8.lowestOneBit(Int8.pow(2i8, 5i8)) == 32i8

    @Test
    def lowestOneBit09(): Bool = Int8.lowestOneBit(Int8.pow(2i8, 3i8) + Int8.pow(2i8, 5i8)) == 8i8

    @Test
    def lowestOneBit10(): Bool = Int8.lowestOneBit(127i8) == 1i8

    @Test
    def lowestOneBit11(): Bool = Int8.lowestOneBit(0i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Bool = Int8.numberOfTrailingZeros(0i8) == 8

    @Test
    def numberOfTrailingZeros02(): Bool = Int8.numberOfTrailingZeros(3i8) == 0

    @Test
    def numberOfTrailingZeros03(): Bool = Int8.numberOfTrailingZeros(42i8) == 1

    @Test
    def numberOfTrailingZeros04(): Bool = Int8.numberOfTrailingZeros(Int8.pow(2i8, 4i8) + Int8.pow(2i8, 6i8)) == 4

    @Test
    def numberOfTrailingZeros05(): Bool = Int8.numberOfTrailingZeros(Int8.pow(2i8, 6i8)) == 6

    @Test
    def numberOfTrailingZeros06(): Bool = Int8.numberOfTrailingZeros(-1i8) == 0

    @Test
    def numberOfTrailingZeros07(): Bool = Int8.numberOfTrailingZeros(-128i8) == 7

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Bool = Int8.getBit(pos = 5, -1i8) == 1

    @Test
    def getBit02(): Bool = Int8.getBit(pos = 7, -100i8) == 1

    @Test
    def getBit03(): Bool = Int8.getBit(pos = 5, Int8.pow(2i8, 5i8)) == 1

    @Test
    def getBit04(): Bool = Int8.getBit(pos = 6, Int8.pow(2i8, 5i8)) == 0

    @Test
    def getBit05(): Bool = Int8.getBit(pos = 4, Int8.pow(2i8, 5i8)) == 0

    @Test
    def getBit06(): Bool = Int8.getBit(pos = 4, -1i8 - Int8.pow(2i8, 4i8)) == 0

    @Test
    def getBit07(): Bool = Int8.getBit(pos = 5, -1i8 - Int8.pow(2i8, 4i8)) == 1

    @Test
    def getBit08(): Bool = Int8.getBit(pos = 7, 0i8) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Bool = Int8.setBit(pos = 0, 0i8) == 1i8

    @Test
    def setBit02(): Bool = Int8.setBit(pos = 1, 0i8) == 2i8

    @Test
    def setBit03(): Bool = Int8.setBit(pos = 7, 0i8) == -128i8

    @Test
    def setBit04(): Bool = Int8.setBit(pos = 8, 0i8) == 0i8

    @Test
    def setBit05(): Bool = Int8.setBit(pos = 31, -101i8) == -101i8

    @Test
    def setBit07(): Bool = Int8.setBit(pos = 4, -17i8) == -1i8

    @Test
    def setBit08(): Bool = Int8.setBit(pos = 6, -65i8) == -1i8

    @Test
    def setBit09(): Bool = Int8.setBit(pos = 15, -17i8) == -17i8

    @Test
    def setBit10(): Bool = Int8.setBit(pos = 30, -17i8) == -17i8

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Bool = Int8.clearBit(pos = 0, 1i8) == 0i8

    @Test
    def clearBit02(): Bool = Int8.clearBit(pos = 1, 1i8) == 1i8

    @Test
    def clearBit03(): Bool = Int8.clearBit(pos = 1, 2i8) == 0i8

    @Test
    def clearBit04(): Bool = Int8.clearBit(pos = 2, 2i8) == 2i8

    @Test
    def clearBit05(): Bool = Int8.clearBit(pos = 31, 0i8) == 0i8

    @Test
    def clearBit06(): Bool = Int8.clearBit(pos = 30, 0i8) == 0i8

    @Test
    def clearBit07(): Bool = Int8.clearBit(pos = 7, Int8.leftShift(1i8, 7)) == 0i8

    @Test
    def clearBit08(): Bool = Int8.clearBit(pos = 3, 109i8) == 101i8

    @Test
    def clearBit09(): Bool = Int8.clearBit(pos = 0, -1i8) == -2i8

    @Test
    def clearBit10(): Bool = Int8.clearBit(pos = 1, -1i8) == -3i8

    @Test
    def clearBit11(): Bool = Int8.clearBit(pos = 5, -1i8) == -33i8

    @Test
    def clearBit12(): Bool = Int8.clearBit(pos = 7, -1i8) == Int8.bitwiseNot(Int8.leftShift(1i8, 7))

    @Test
    def clearBit13(): Bool = Int8.clearBit(pos = 32, -1i8) == -2i8

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Bool = Int8.flipBit(pos = 0, 1i8) == 0i8

    @Test
    def flipBit02(): Bool = Int8.flipBit(pos = 1, 1i8) == 3i8

    @Test
    def flipBit03(): Bool = Int8.flipBit(pos = 1, 2i8) == 0i8

    @Test
    def flipBit04(): Bool = Int8.flipBit(pos = 2, 2i8) == 6i8

    @Test
    def flipBit05(): Bool = Int8.flipBit(pos = 7, 0i8) == -128i8

    @Test
    def flipBit06(): Bool = Int8.flipBit(pos = 8, 0i8) == 0i8

    @Test
    def flipBit07(): Bool = Int8.flipBit(pos = 7, Int8.leftShift(1i8, 7)) == 0i8

    @Test
    def flipBit08(): Bool = Int8.flipBit(pos = 0, -1i8) == -2i8

    @Test
    def flipBit9(): Bool = Int8.flipBit(pos = 1, -1i8) == -3i8

    @Test
    def flipBit10(): Bool = Int8.flipBit(pos = 5, -1i8) == -33i8

    @Test
    def flipBit11(): Bool = Int8.flipBit(pos = 5, -1i8) == (-1i8-Int8.leftShift(1i8, 5))

    @Test
    def flipBit12(): Bool = Int8.flipBit(pos = 32, -1i8) == -2i8

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Bool = Int8.log2(10i8) == 3i8

    @Test
    def log202(): Bool = Int8.log2(8i8) == 3i8

    @Test
    def log203(): Bool = Int8.log2(127i8) == 6i8

    @Test
    def log204(): Bool = Int8.log2(1i8) == 0i8

    @Test
    def log205(): Bool = Int8.log2(20i8) == 4i8

    @Test
    def log206(): Bool = Int8.log2(0i8) == 0i8

    @Test
    def log207(): Bool = Int8.log2(-1i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Bool = Int8.bitwiseAnd(0i8, 0i8) == 0i8

    @Test
    def bitwiseAnd02(): Bool = Int8.bitwiseAnd(127i8, 0i8) == 0i8

    @Test
    def bitwiseAnd03(): Bool = Int8.bitwiseAnd(-128i8, 0i8) == 0i8

    @Test
    def bitwiseAnd04(): Bool = Int8.bitwiseAnd(0i8, 1i8) == 0i8

    @Test
    def bitwiseAnd05(): Bool = Int8.bitwiseAnd(127i8, 1i8) == 1i8

    @Test
    def bitwiseAnd06(): Bool = Int8.bitwiseAnd(-128i8, 1i8) == 0i8

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Bool = Int8.bitwiseNot(0i8) == -1i8

    @Test
    def bitwiseNot02(): Bool = Int8.bitwiseNot(1i8) == -2i8

    @Test
    def bitwiseNot03(): Bool = Int8.bitwiseNot(-1i8) == -0i8

    @Test
    def bitwiseNot04(): Bool = Int8.bitwiseNot(127i8) == -128i8

    @Test
    def bitwiseNot05(): Bool = Int8.bitwiseNot(-128i8) == 127i8

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Bool = Int8.bitwiseOr(0i8, 0i8) == 0i8

    @Test
    def bitwiseOr02(): Bool = Int8.bitwiseOr(127i8, 0i8) == 127i8

    @Test
    def bitwiseOr03(): Bool = Int8.bitwiseOr(-128i8, 0i8) == -128i8

    @Test
    def bitwiseOr04(): Bool = Int8.bitwiseOr(0i8, 1i8) == 1i8

    @Test
    def bitwiseOr05(): Bool = Int8.bitwiseOr(127i8, 1i8) == 127i8

    @Test
    def bitwiseOr06(): Bool = Int8.bitwiseOr(-128i8, 1i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Bool = Int8.bitwiseXor(0i8, 0i8) == 0i8

    @Test
    def bitwiseXor02(): Bool = Int8.bitwiseXor(127i8, 0i8) == 127i8

    @Test
    def bitwiseXor03(): Bool = Int8.bitwiseXor(-128i8, 0i8) == -128i8

    @Test
    def bitwiseXor04(): Bool = Int8.bitwiseXor(0i8, 1i8) == 1i8

    @Test
    def bitwiseXor05(): Bool = Int8.bitwiseXor(127i8, 1i8) == 126i8

    @Test
    def bitwiseXor06(): Bool = Int8.bitwiseXor(-128i8, 1i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Bool = Int8.factorial(0i8) == 1

    @Test
    def factorial02(): Bool = Int8.factorial(1i8) == 1

    @Test
    def factorial03(): Bool = Int8.factorial(2i8) == 2

    @Test
    def factorial04(): Bool = Int8.factorial(3i8) == 6

    @Test
    def factorial05(): Bool = Int8.factorial(4i8) == 24

    @Test
    def factorial06(): Bool = Int8.factorial(5i8) == 120

    @Test
    def factorial07(): Bool = Int8.factorial(10i8) == 3628800

    @Test
    def factorial08(): Bool = Int8.factorial(-1i8) == 0

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Bool = Int8.toString(0i8) == "0"

    @Test
    def toString02(): Bool = Int8.toString(1i8) == "1"

    @Test
    def toString03(): Bool = Int8.toString(-1i8) == "-1"

    @Test
    def toString04(): Bool = Int8.toString(127i8) == "127"

    @Test
    def toString05(): Bool = Int8.toString(-128i8) == "-128"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = Int8.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = Int8.fromString(" 1") == Some(1i8)

    @Test
    def fromString03(): Bool = Int8.fromString("1 ") == Some(1i8)

    @Test
    def fromString04(): Bool = Int8.fromString("0") == Some(0i8)

    @Test
    def fromString05(): Bool = Int8.fromString("1") == Some(1i8)

    @Test
    def fromString06(): Bool = Int8.fromString("-1") == Some(-1i8)

    @Test
    def fromString07(): Bool = Int8.fromString("127") == Some(127i8)

    @Test
    def fromString08(): Bool = Int8.fromString("-128") == Some(-128i8)

    /////////////////////////////////////////////////////////////////////////////
    // toInt16                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt1601(): Bool = Int8.toInt16(0i8) == 0i16

    @Test
    def toInt1602(): Bool = Int8.toInt16(1i8) == 1i16

    @Test
    def toInt1603(): Bool = Int8.toInt16(-1i8) == -1i16

    @Test
    def toInt1604(): Bool = Int8.toInt16(127i8) == 127i16

    @Test
    def toInt1605(): Bool = Int8.toInt16(-128i8) == -128i16

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt3201(): Bool = Int8.toInt32(0i8) == 0

    @Test
    def toInt3202(): Bool = Int8.toInt32(1i8) == 1

    @Test
    def toInt3203(): Bool = Int8.toInt32(-1i8) == -1

    @Test
    def toInt3204(): Bool = Int8.toInt32(127i8) == 127

    @Test
    def toInt3205(): Bool = Int8.toInt32(-128i8) == -128

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt6401(): Bool = Int8.toInt64(0i8) == 0i64

    @Test
    def toInt6402(): Bool = Int8.toInt64(1i8) == 1i64

    @Test
    def toInt6403(): Bool = Int8.toInt64(-1i8) == -1i64

    @Test
    def toInt6404(): Bool = Int8.toInt64(127i8) == 127i64

    @Test
    def toInt6405(): Bool = Int8.toInt64(-128i8) == -128i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Bool = Int8.toBigInt(0i8) == 0ii

    @Test
    def toBigInt02(): Bool = Int8.toBigInt(1i8) == 1ii

    @Test
    def toBigInt03(): Bool = Int8.toBigInt(-1i8) == -1ii

    @Test
    def toBigInt04(): Bool = Int8.toBigInt(127i8) == 127ii

    @Test
    def toBigInt05(): Bool = Int8.toBigInt(-128i8) == -128ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Bool = Int8.toFloat32(0i8) == 0.0f32

    @Test
    def toFloat3202(): Bool = Int8.toFloat32(1i8) == 1.0f32

    @Test
    def toFloat3203(): Bool = Int8.toFloat32(-1i8) == -1.0f32

    @Test
    def toFloat3204(): Bool = Int8.toFloat32(127i8) == 127.0f32

    @Test
    def toFloat3205(): Bool = Int8.toFloat32(-128i8) == -128.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Bool = Int8.toFloat64(0i8) == 0.0f64

    @Test
    def toFloat6402(): Bool = Int8.toFloat64(1i8) == 1.0f64

    @Test
    def toFloat6403(): Bool = Int8.toFloat64(-1i8) == -1.0f64

    @Test
    def toFloat6404(): Bool = Int8.toFloat64(127i8) == 127.0f64

    @Test
    def toFloat6405(): Bool = Int8.toFloat64(-128i8) == -128.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Bool = Int8.toBigDecimal(0i8) == 0.0ff

    @Test
    def toBigDecimal02(): Bool = Int8.toBigDecimal(1i8) == 1.0ff

    @Test
    def toBigDecimal03(): Bool = Int8.toBigDecimal(-1i8) == -1.0ff

    @Test
    def toBigDecimal04(): Bool = Int8.toBigDecimal(127i8) == 127.0ff

    @Test
    def toBigDecimal05(): Bool = Int8.toBigDecimal(-128i8) == -128.0ff

    /////////////////////////////////////////////////////////////////////////////
    // byteValue                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def byteValue01(): Bool =
        let i = Int8.valueOf(0i8);
        Int8.byteValue(i) == 0i8

    @Test
    def byteValue02(): Bool =
        let i = Int8.valueOf(1i8);
        Int8.byteValue(i) == 1i8

    @Test
    def byteValue03(): Bool =
        let i = Int8.valueOf(-1i8);
        Int8.byteValue(i) == -1i8

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        import java.lang.Byte.equals(##java.lang.Object): Bool \ {};
        let i = Int8.valueOf(0i8);
        equals(i, checked_cast(Int8.valueOf(0i8)))

    @Test
    def valueOf02(): Bool =
        import java.lang.Byte.equals(##java.lang.Object): Bool \ {};
        let i = Int8.valueOf(1i8);
        equals(i, checked_cast(Int8.valueOf(1i8)))

    @Test
    def valueOf03(): Bool =
        import java.lang.Byte.equals(##java.lang.Object): Bool \ {};
        let i = Int8.valueOf(-1i8);
        equals(i, checked_cast(Int8.valueOf(-1i8)))

}
