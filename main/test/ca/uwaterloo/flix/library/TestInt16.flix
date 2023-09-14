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

mod TestInt16 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool = mul(1i16, 0i16) == 0i16

    @Test
    def mul02(): Bool = mul(0i16, 1i16) == 0i16

    @Test
    def mul03(): Bool = mul(0i16, 0i16) == 0i16

    @Test
    def mul04(): Bool = mul(4i16, -8192i16) == -32768i16

    @Test
    def mul05(): Bool = mul(7i16, 4681i16) == 32767i16

    @Test
    def mul06(): Bool = mul(5i16, 11i16) == 55i16

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Bool = Int16.maxValue() == Int16.bitwiseNot(Int16.leftShift(1i16, 15))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Bool = Int16.minValue() == Int16.leftShift(1i16, 15)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Bool = Int16.size() == 16

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = Int16.min(1i16, 2i16) == 1i16

    @Test
    def min02(): Bool = Int16.min(2i16, -1i16) == -1i16

    @Test
    def min03(): Bool = Int16.min(-33i16, -66i16) == -66i16

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = Int16.max(48i16, 49i16) == 49i16

    @Test
    def max02(): Bool = Int16.max(4i16, -16i16) == 4i16

    @Test
    def max03(): Bool = Int16.max(-34i16, -16i16) == -16i16

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Bool = Int16.abs(31i16) == 31i16

    @Test
    def abs02(): Bool = Int16.abs(-31i16) == 31i16

    @Test
    def abs03(): Bool = Int16.abs(32767i16) == 32767i16

    @Test
    def abs04(): Bool = Int16.abs(-32767i16) == 32767i16

    @Test
    def abs05(): Bool = Int16.abs(0i16) == 0i16

    @Test
    def abs06(): Bool = Int16.abs(-32768i16) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mod01(): Bool = (12000i16 `Int16.mod` 2000i16) == 0i16

    @Test
    def mod02(): Bool = (12000i16 `Int16.mod` 5000i16) == 2000i16

    @Test
    def mod03(): Bool = (-12000i16 `Int16.mod` 5000i16) == 3000i16

    @Test
    def mod04(): Bool = (12000i16 `Int16.mod` -5000i16) == -3000i16

    @Test
    def mod05(): Bool = (-12000i16 `Int16.mod` -5000i16) == -2000i16

    @Test
    def mod06(): Bool = (1i16 `Int16.mod` 0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rem01(): Bool = (12000i16 `Int16.rem` 2000i16) == 0i16

    @Test
    def rem02(): Bool = (12000i16 `Int16.rem` 5000i16) == 2000i16

    @Test
    def rem03(): Bool = (-12000i16 `Int16.rem` 5000i16) == -2000i16

    @Test
    def rem04(): Bool = (12000i16 `Int16.rem` -5000i16) == 2000i16

    @Test
    def rem05(): Bool = (-12000i16 `Int16.rem` -5000i16) == -2000i16

    @Test
    def rem06(): Bool = (1i16 `Int16.rem` 0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Bool = Int16.dist(31i16, -7i16) == 38i16

    @Test
    def dist02(): Bool = Int16.dist(-44i16, -1i16) == 43i16

    @Test
    def dist03(): Bool = Int16.dist(-2i16, -2i16) == 0i16

    @Test
    def dist04(): Bool = Int16.dist(-32768i16, -1i16) == 32767i16

    @Test
    def dist05(): Bool = Int16.dist(-32768i16, 0i16) == -1i16

    @Test
    def dist06(): Bool = Int16.dist(-123i16, 4i16) == 127i16

    @Test
    def dist07(): Bool = Int16.dist(-32763i16, 5i16) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Bool = Int16.compare(-1i16, 44i16) == -1

    @Test
    def compare02(): Bool = Int16.compare(-1i16, -44i16) == 1

    @Test
    def compare03(): Bool = Int16.compare(88i16, 88i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Bool = Int16.signum(-22i16) == -1

    @Test
    def signum02(): Bool = Int16.signum(22i16) == 1

    @Test
    def signum03(): Bool = Int16.signum(0i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = Int16.pow(0i16, 0i16) == 1i16

    @Test
    def pow02(): Bool = Int16.pow(0i16, 1i16) == 0i16

    @Test
    def pow03(): Bool = Int16.pow(1i16, 0i16) == 1i16

    @Test
    def pow04(): Bool = Int16.pow(1i16, 1i16) == 1i16

    @Test
    def pow05(): Bool = Int16.pow(5i16, 2i16) == 25i16

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Bool = Int16.leftShift(8i16, 0) == 8i16

    @Test
    def testBitwiseLeftShift02(): Bool = Int16.leftShift(8i16, 8) == 2048i16

    @Test
    def testBitwiseLeftShift03(): Bool = Int16.leftShift(8i16, 12) == -32768i16

    @Test
    def testBitwiseLeftShift04(): Bool = Int16.leftShift(8i16, 13) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Bool = Int16.rightShift(12000i16, 0) == 12000i16

    @Test
    def testBitwiseRightShift02(): Bool = Int16.rightShift(12000i16, 2) == 3000i16

    @Test
    def testBitwiseRightShift03(): Bool = Int16.rightShift(12000i16, 15) == 0i16

    @Test
    def testBitwiseRightShift04(): Bool = Int16.rightShift(-12000i16, 2) == -3000i16


    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Bool = Int16.logicalRightShift(dist = 4, 2233i16) == Int16.rightShift(2233i16, 4)

    @Test
    def logicalRightShift02(): Bool = Int16.logicalRightShift(dist = 33, 32767i16) == Int16.rightShift(32767i16, 33)

    @Test
    def logicalRightShift03(): Bool = Int16.logicalRightShift(dist = -8, 2233i16) == Int16.rightShift(2233i16, -8)

    @Test
    def logicalRightShift04(): Bool = Int16.logicalRightShift(dist = 4, 2233i16) == Int16.rightShift(2233i16, 4)

    @Test
    def logicalRightShift05(): Bool = Int16.logicalRightShift(dist = 1, -1i16) == 32767i16

    @Test
    def logicalRightShift06(): Bool = Int16.logicalRightShift(dist = 4, -35i16) == 4093i16

    @Test
    def logicalRightShift07(): Bool = Int16.logicalRightShift(dist = -2777, -2777i16) == 490i16

    @Test
    def logicalRightShift08(): Bool = Int16.logicalRightShift(dist = 64, -233i16) == -233i16

    @Test
    def logicalRightShift09(): Bool = Int16.logicalRightShift(dist = 63, -233i16) == 0i16

    @Test
    def logicalRightShift10(): Bool = Int16.logicalRightShift(dist = 0, -27i16) == -27i16

    @Test
    def logicalRightShift11(): Bool = Int16.logicalRightShift(dist = 1, 32767i16) == 16383i16

    @Test
    def logicalRightShift12(): Bool = Int16.logicalRightShift(dist = 17, 32767i16) == 0i16

    @Test
    def logicalRightShift13(): Bool = Int16.logicalRightShift(dist = 16, 32767i16) == 0i16

    @Test
    def logicalRightShift14(): Bool = Int16.logicalRightShift(dist = 17, -32768i16) == 0i16

    @Test
    def logicalRightShift15(): Bool = Int16.logicalRightShift(dist = 16, -32768i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Bool = Int16.bitCount(19909i16) == 8

    @Test
    def bitCount02(): Bool = Int16.bitCount(-1i16) == 16

    @Test
    def bitCount03(): Bool = Int16.bitCount(-451i16) == 12

    @Test
    def bitCount04(): Bool = Int16.bitCount(0i16) == 0

    @Test
    def bitCount05(): Bool = Int16.bitCount(32767i16) == 15

    @Test
    def bitCount06(): Bool = Int16.bitCount(-32768i16) == 1

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Bool = Int16.rotateRight(dist = 3, 0i16) == 0i16

    @Test
    def rotateRight02(): Bool = Int16.rotateRight(dist = 19, 0i16) == 0i16

    @Test
    def rotateRight03(): Bool = Int16.rotateRight(dist = -3, 0i16) == 0i16

    @Test
    def rotateRight04(): Bool = Int16.rotateRight(dist = -3, -1i16) == -1i16

    @Test
    def rotateRight05(): Bool = Int16.rotateRight(dist = 19, -1i16) == -1i16

    @Test
    def rotateRight06(): Bool = Int16.rotateRight(dist = -3, -1i16) == -1i16

    @Test
    def rotateRight07(): Bool = Int16.rotateRight(dist = 3, 8410i16) == 17435i16

    @Test
    def rotateRight08(): Bool = Int16.rotateRight(dist = 19, 8410i16) == 17435i16

    @Test
    def rotateRight09(): Bool = Int16.rotateRight(dist = -13, 8410i16) == 17435i16

    @Test
    def rotateRight10(): Bool = Int16.rotateRight(dist = 10, -14949i16) == 26353i16

    @Test
    def rotateRight11(): Bool = Int16.rotateRight(dist = 26, -14949i16) == 26353i16

    @Test
    def rotateRight12(): Bool = Int16.rotateRight(dist = -6, -14949i16) == 26353i16

    @Test
    def rotateRight13(): Bool = Int16.rotateRight(dist = 6, -6415i16) == -14437i16

    @Test
    def rotateRight14(): Bool = Int16.rotateRight(dist = 22, -6415i16) == -14437i16

    @Test
    def rotateRight15(): Bool = Int16.rotateRight(dist = -10, -6415i16) == -14437i16

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Bool = Int16.rotateLeft(dist = 3, 0i16) == 0i16

    @Test
    def rotateLeft02(): Bool = Int16.rotateLeft(dist = 19, 0i16) == 0i16

    @Test
    def rotateLeft03(): Bool = Int16.rotateLeft(dist = -3, 0i16) == 0i16

    @Test
    def rotateLeft04(): Bool = Int16.rotateLeft(dist = 3, -1i16) == -1i16

    @Test
    def rotateLeft05(): Bool = Int16.rotateLeft(dist = 19, -1i16) == -1i16

    @Test
    def rotateLeft06(): Bool = Int16.rotateLeft(dist = -3, -1i16) == -1i16

    @Test
    def rotateLeft07(): Bool = Int16.rotateLeft(dist = 13, 8410i16) == 17435i16

    @Test
    def rotateLeft08(): Bool = Int16.rotateLeft(dist = 29, 8410i16) == 17435i16

    @Test
    def rotateLeft09(): Bool = Int16.rotateLeft(dist = -3, 8410i16) == 17435i16

    @Test
    def rotateLeft10(): Bool = Int16.rotateLeft(dist = 6, -14949i16) == 26353i16

    @Test
    def rotateLeft11(): Bool = Int16.rotateLeft(dist = 22, -14949i16) == 26353i16

    @Test
    def rotateLeft12(): Bool = Int16.rotateLeft(dist = -10, -14949i16) == 26353i16

    @Test
    def rotateLeft13(): Bool = Int16.rotateLeft(dist = 10, -6415i16) == -14437i16

    @Test
    def rotateLeft14(): Bool = Int16.rotateLeft(dist = 26, -6415i16) == -14437i16

    @Test
    def rotateLeft15(): Bool = Int16.rotateLeft(dist = -6, -6415i16) == -14437i16

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Bool = Int16.reverse(18140i16) == 15202i16

    @Test
    def reverse02(): Bool = Int16.reverse(15202i16) == 18140i16

    @Test
    def reverse03(): Bool = Int16.reverse(-17565i16) == -14627i16

    @Test
    def reverse04(): Bool = Int16.reverse(-14627i16) == -17565i16

    @Test
    def reverse05(): Bool = Int16.reverse(-26552i16) == 4633i16

    @Test
    def reverse06(): Bool = Int16.reverse(4633i16) == -26552i16

    @Test
    def reverse07(): Bool = Int16.reverse(4i16) == 8192i16

    @Test
    def reverse08(): Bool = Int16.reverse(8192i16) == 4i16

    @Test
    def reverse09(): Bool = Int16.reverse(-1i16) == -1i16

    @Test
    def reverse10(): Bool = Int16.reverse(0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Bool = Int16.highestOneBitPosition(-1i16) == 15

    @Test
    def highestOneBitPosition02(): Bool = Int16.highestOneBitPosition(-32768i16) == 15

    @Test
    def highestOneBitPosition03(): Bool = Int16.highestOneBitPosition(1i16) == 0

    @Test
    def highestOneBitPosition04(): Bool = Int16.highestOneBitPosition(2i16) == 1

    @Test
    def highestOneBitPosition05(): Bool = Int16.highestOneBitPosition(3i16) == 1

    @Test
    def highestOneBitPosition06(): Bool = Int16.highestOneBitPosition(20i16) == 4

    @Test
    def highestOneBitPosition07(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 14i16)) == 14

    @Test
    def highestOneBitPosition08(): Bool = Int16.highestOneBitPosition(32767i16) == 14

    @Test
    def highestOneBitPosition09(): Bool = Int16.highestOneBitPosition(0i16) == -1

    @Test
    def highestOneBitPosition10(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 8i16) + Int16.pow(2i16, 11i16)) == 11

    @Test
    def highestOneBitPosition11(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 3i16) + Int16.pow(2i16, 7i16)) == 7

    @Test
    def highestOneBitPosition12(): Bool = Int16.highestOneBitPosition(-32767i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Bool = Int16.highestOneBit(-1i16) == Int16.leftShift(1i16, 15)

    @Test
    def highestOneBit02(): Bool = Int16.highestOneBit(-32768i16) == Int16.leftShift(1i16, 15)

    @Test
    def highestOneBit03(): Bool = Int16.highestOneBit(-77i16) == Int16.leftShift(1i16, 15)

    @Test
    def highestOneBit04(): Bool = Int16.highestOneBit(2i16) == 2i16

    @Test
    def highestOneBit05(): Bool = Int16.highestOneBit(3i16) == 2i16

    @Test
    def highestOneBit06(): Bool = Int16.highestOneBit(20i16) == 16i16

    @Test
    def highestOneBit07(): Bool = Int16.highestOneBit(Int16.pow(2i16, 11i16) + Int16.pow(2i16, 13i16)) == 8192i16

    @Test
    def highestOneBit08(): Bool = Int16.highestOneBit(32767i16) == 16384i16

    @Test
    def highestOneBit09(): Bool = Int16.highestOneBit(0i16) == 0i16

    @Test
    def highestOneBit10(): Bool = Int16.highestOneBit(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 9i16)) == 512i16

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Bool = Int16.numberOfLeadingZeros(0i16) == 16

    @Test
    def numberOfLeadingZeros02(): Bool = Int16.numberOfLeadingZeros(-32768i16) == 0

    @Test
    def numberOfLeadingZeros03(): Bool = Int16.numberOfLeadingZeros(-1i16) == 0

    @Test
    def numberOfLeadingZeros04(): Bool = Int16.numberOfLeadingZeros(32767i16) == 1

    @Test
    def numberOfLeadingZeros05(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 14i16)) == 1

    @Test
    def numberOfLeadingZeros06(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 11i16)) == 4

    @Test
    def numberOfLeadingZeros07(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 7i16) + Int16.pow(2i16, 6i16)) == 8

    @Test
    def numberOfLeadingZeros08(): Bool = Int16.numberOfLeadingZeros(20i16) == 11

    @Test
    def numberOfLeadingZeros09(): Bool = Int16.numberOfLeadingZeros(1i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Bool = Int16.lowestOneBitPosition(-1i16) == 0

    @Test
    def lowestOneBitPosition02(): Bool = Int16.lowestOneBitPosition(-32768i16) == 15

    @Test
    def lowestOneBitPosition03(): Bool = Int16.lowestOneBitPosition(1i16) == 0

    @Test
    def lowestOneBitPosition04(): Bool = Int16.lowestOneBitPosition(2i16) == 1

    @Test
    def lowestOneBitPosition05(): Bool = Int16.lowestOneBitPosition(3i16) == 0

    @Test
    def lowestOneBitPosition06(): Bool = Int16.lowestOneBitPosition(4i16) == 2

    @Test
    def lowestOneBitPosition07(): Bool = Int16.lowestOneBitPosition(12i16) == 2

    @Test
    def lowestOneBitPosition08(): Bool = Int16.lowestOneBitPosition(Int16.pow(2i16, 11i16)) == 11

    @Test
    def lowestOneBitPosition09(): Bool = Int16.lowestOneBitPosition(Int16.pow(2i16, 11i16) + Int16.pow(2i16, 6i16)) == 6

    @Test
    def lowestOneBitPosition10(): Bool = Int16.lowestOneBitPosition(32767i16) == 0

    @Test
    def lowestOneBitPosition11(): Bool = Int16.lowestOneBitPosition(0i16) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Bool = Int16.lowestOneBit(-1i16) == 1i16

    @Test
    def lowestOneBit02(): Bool = Int16.lowestOneBit(-32768i16) == Int16.leftShift(1i16, 15)

    @Test
    def lowestOneBit03(): Bool = Int16.lowestOneBit(1i16) == 1i16

    @Test
    def lowestOneBit04(): Bool = Int16.lowestOneBit(2i16) == 2i16

    @Test
    def lowestOneBit05(): Bool = Int16.lowestOneBit(6i16) == 2i16

    @Test
    def lowestOneBit06(): Bool = Int16.lowestOneBit(12i16) == 4i16

    @Test
    def lowestOneBit07(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 9i16) + Int16.pow(2i16, 14i16)) == 64i16

    @Test
    def lowestOneBit08(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 13i16)) == 8192i16

    @Test
    def lowestOneBit09(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 3i16) + Int16.pow(2i16, 5i16)) == 8i16

    @Test
    def lowestOneBit10(): Bool = Int16.lowestOneBit(32767i16) == 1i16

    @Test
    def lowestOneBit11(): Bool = Int16.lowestOneBit(0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Bool = Int16.numberOfTrailingZeros(0i16) == 16

    @Test
    def numberOfTrailingZeros02(): Bool = Int16.numberOfTrailingZeros(3i16) == 0

    @Test
    def numberOfTrailingZeros03(): Bool = Int16.numberOfTrailingZeros(42i16) == 1

    @Test
    def numberOfTrailingZeros04(): Bool = Int16.numberOfTrailingZeros(Int16.pow(2i16, 10i16) + Int16.pow(2i16, 14i16)) == 10

    @Test
    def numberOfTrailingZeros05(): Bool = Int16.numberOfTrailingZeros(Int16.pow(2i16, 8i16)) == 8

    @Test
    def numberOfTrailingZeros06(): Bool = Int16.numberOfTrailingZeros(-1i16) == 0

    @Test
    def numberOfTrailingZeros07(): Bool = Int16.numberOfTrailingZeros(-32768i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Bool = Int16.getBit(pos = 5, -1i16) == 1

    @Test
    def getBit02(): Bool = Int16.getBit(pos = 15, -10000i16) == 1

    @Test
    def getBit03(): Bool = Int16.getBit(pos = 12, Int16.pow(2i16, 12i16)) == 1

    @Test
    def getBit04(): Bool = Int16.getBit(pos = 13, Int16.pow(2i16, 12i16)) == 0

    @Test
    def getBit05(): Bool = Int16.getBit(pos = 11, Int16.pow(2i16, 12i16)) == 0

    @Test
    def getBit06(): Bool = Int16.getBit(pos = 9, -1i16 - Int16.pow(2i16, 9i16)) == 0

    @Test
    def getBit07(): Bool = Int16.getBit(pos = 10, -1i16 - Int16.pow(2i16, 9i16)) == 1

    @Test
    def getBit08(): Bool = Int16.getBit(pos = 15, 0i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Bool = Int16.setBit(pos = 0, 0i16) == 1i16

    @Test
    def setBit02(): Bool = Int16.setBit(pos = 1, 0i16) == 2i16

    @Test
    def setBit03(): Bool = Int16.setBit(pos = 15, 0i16) == -32768i16

    @Test
    def setBit04(): Bool = Int16.setBit(pos = 16, 0i16) == 0i16

    @Test
    def setBit05(): Bool = Int16.setBit(pos = 31, -1418i16) == -1418i16

    @Test
    def setBit07(): Bool = Int16.setBit(pos = 4, -17i16) == -1i16

    @Test
    def setBit08(): Bool = Int16.setBit(pos = 10, -1025i16) == -1i16

    @Test
    def setBit09(): Bool = Int16.setBit(pos = 63, -17i16) == -17i16

    @Test
    def setBit10(): Bool = Int16.setBit(pos = 30, -17i16) == -17i16

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Bool = Int16.clearBit(pos = 0, 1i16) == 0i16

    @Test
    def clearBit02(): Bool = Int16.clearBit(pos = 1, 1i16) == 1i16

    @Test
    def clearBit03(): Bool = Int16.clearBit(pos = 1, 2i16) == 0i16

    @Test
    def clearBit04(): Bool = Int16.clearBit(pos = 2, 2i16) == 2i16

    @Test
    def clearBit05(): Bool = Int16.clearBit(pos = 31, 0i16) == 0i16

    @Test
    def clearBit06(): Bool = Int16.clearBit(pos = 30, 0i16) == 0i16

    @Test
    def clearBit07(): Bool = Int16.clearBit(pos = 15, Int16.leftShift(1i16, 15)) == 0i16

    @Test
    def clearBit08(): Bool = Int16.clearBit(pos = 11, 27957i16) == 25909i16

    @Test
    def clearBit09(): Bool = Int16.clearBit(pos = 0, -1i16) == -2i16

    @Test
    def clearBit10(): Bool = Int16.clearBit(pos = 1, -1i16) == -3i16

    @Test
    def clearBit11(): Bool = Int16.clearBit(pos = 5, -1i16) == -33i16

    @Test
    def clearBit12(): Bool = Int16.clearBit(pos = 15, -1i16) == Int16.bitwiseNot(Int16.leftShift(1i16, 15))

    @Test
    def clearBit13(): Bool = Int16.clearBit(pos = 32, -1i16) == -2i16

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Bool = Int16.flipBit(pos = 0, 1i16) == 0i16

    @Test
    def flipBit02(): Bool = Int16.flipBit(pos = 1, 1i16) == 3i16

    @Test
    def flipBit03(): Bool = Int16.flipBit(pos = 1, 2i16) == 0i16

    @Test
    def flipBit04(): Bool = Int16.flipBit(pos = 2, 2i16) == 6i16

    @Test
    def flipBit05(): Bool = Int16.flipBit(pos = 15, 0i16) == -32768i16

    @Test
    def flipBit06(): Bool = Int16.flipBit(pos = 16, 0i16) == 0i16

    @Test
    def flipBit07(): Bool = Int16.flipBit(pos = 15, Int16.leftShift(1i16, 15)) == 0i16

    @Test
    def flipBit08(): Bool = Int16.flipBit(pos = 0, -1i16) == -2i16

    @Test
    def flipBit9(): Bool = Int16.flipBit(pos = 1, -1i16) == -3i16

    @Test
    def flipBit10(): Bool = Int16.flipBit(pos = 5, -1i16) == -33i16

    @Test
    def flipBit11(): Bool = Int16.flipBit(pos = 13, -1i16) == (-1i16-Int16.leftShift(1i16, 13))

    @Test
    def flipBit12(): Bool = Int16.flipBit(pos = 32, -1i16) == -2i16

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Bool = Int16.log2(10i16) == 3i16

    @Test
    def log202(): Bool = Int16.log2(10000i16) == 13i16

    @Test
    def log203(): Bool = Int16.log2(8i16) == 3i16

    @Test
    def log204(): Bool = Int16.log2(256i16) == 8i16

    @Test
    def log205(): Bool = Int16.log2(1i16) == 0i16

    @Test
    def log206(): Bool = Int16.log2(32767i16) == 14i16

    @Test
    def log207(): Bool = Int16.log2(20i16) == 4i16

    @Test
    def log208(): Bool = Int16.log2(0i16) == 0i16

    @Test
    def log209(): Bool = Int16.log2(-1i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Bool = Int16.bitwiseAnd(0i16, 0i16) == 0i16

    @Test
    def bitwiseAnd02(): Bool = Int16.bitwiseAnd(127i16, 0i16) == 0i16

    @Test
    def bitwiseAnd03(): Bool = Int16.bitwiseAnd(-128i16, 0i16) == 0i16

    @Test
    def bitwiseAnd04(): Bool = Int16.bitwiseAnd(0i16, 1i16) == 0i16

    @Test
    def bitwiseAnd05(): Bool = Int16.bitwiseAnd(127i16, 1i16) == 1i16

    @Test
    def bitwiseAnd06(): Bool = Int16.bitwiseAnd(-128i16, 1i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Bool = Int16.bitwiseNot(0i16) == -1i16

    @Test
    def bitwiseNot02(): Bool = Int16.bitwiseNot(1i16) == -2i16

    @Test
    def bitwiseNot03(): Bool = Int16.bitwiseNot(-1i16) == -0i16

    @Test
    def bitwiseNot04(): Bool = Int16.bitwiseNot(127i16) == -128i16

    @Test
    def bitwiseNot05(): Bool = Int16.bitwiseNot(-128i16) == 127i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Bool = Int16.bitwiseOr(0i16, 0i16) == 0i16

    @Test
    def bitwiseOr02(): Bool = Int16.bitwiseOr(127i16, 0i16) == 127i16

    @Test
    def bitwiseOr03(): Bool = Int16.bitwiseOr(-128i16, 0i16) == -128i16

    @Test
    def bitwiseOr04(): Bool = Int16.bitwiseOr(0i16, 1i16) == 1i16

    @Test
    def bitwiseOr05(): Bool = Int16.bitwiseOr(127i16, 1i16) == 127i16

    @Test
    def bitwiseOr06(): Bool = Int16.bitwiseOr(-128i16, 1i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Bool = Int16.bitwiseXor(0i16, 0i16) == 0i16

    @Test
    def bitwiseXor02(): Bool = Int16.bitwiseXor(127i16, 0i16) == 127i16

    @Test
    def bitwiseXor03(): Bool = Int16.bitwiseXor(-128i16, 0i16) == -128i16

    @Test
    def bitwiseXor04(): Bool = Int16.bitwiseXor(0i16, 1i16) == 1i16

    @Test
    def bitwiseXor05(): Bool = Int16.bitwiseXor(127i16, 1i16) == 126i16

    @Test
    def bitwiseXor06(): Bool = Int16.bitwiseXor(-128i16, 1i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Bool = Int16.factorial(0i16) == 1

    @Test
    def factorial02(): Bool = Int16.factorial(1i16) == 1

    @Test
    def factorial03(): Bool = Int16.factorial(2i16) == 2

    @Test
    def factorial04(): Bool = Int16.factorial(3i16) == 6

    @Test
    def factorial05(): Bool = Int16.factorial(4i16) == 24

    @Test
    def factorial06(): Bool = Int16.factorial(5i16) == 120

    @Test
    def factorial07(): Bool = Int16.factorial(10i16) == 3628800

    @Test
    def factorial08(): Bool = Int16.factorial(-1i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Bool = Int16.toString(0i16) == "0"

    @Test
    def toString02(): Bool = Int16.toString(1i16) == "1"

    @Test
    def toString03(): Bool = Int16.toString(-1i16) == "-1"

    @Test
    def toString04(): Bool = Int16.toString(32767i16) == "32767"

    @Test
    def toString05(): Bool = Int16.toString(-32768i16) == "-32768"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = Int16.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = Int16.fromString(" 1") == Some(1i16)

    @Test
    def fromString03(): Bool = Int16.fromString("1 ") == Some(1i16)

    @Test
    def fromString04(): Bool = Int16.fromString("0") == Some(0i16)

    @Test
    def fromString05(): Bool = Int16.fromString("1") == Some(1i16)

    @Test
    def fromString06(): Bool = Int16.fromString("-1") == Some(-1i16)

    @Test
    def fromString07(): Bool = Int16.fromString("32767") == Some(32767i16)

    @Test
    def fromString08(): Bool = Int16.fromString("-32768") == Some(-32768i16)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Bool = Int16.tryToInt8(0i16) == Some(0i8)

    @Test
    def tryToInt802(): Bool = Int16.tryToInt8(1i16) == Some(1i8)

    @Test
    def tryToInt803(): Bool = Int16.tryToInt8(-1i16) == Some(-1i8)

    @Test
    def tryToInt804(): Bool = Int16.tryToInt8(127i16) == Some(127i8)

    @Test
    def tryToInt805(): Bool = Int16.tryToInt8(128i16) == None

    @Test
    def tryToInt806(): Bool = Int16.tryToInt8(-128i16) == Some(-128i8)

    @Test
    def tryToInt807(): Bool = Int16.tryToInt8(-129i16) == None

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt3201(): Bool = Int16.toInt32(0i16) == 0

    @Test
    def toInt3202(): Bool = Int16.toInt32(1i16) == 1

    @Test
    def toInt3203(): Bool = Int16.toInt32(-1i16) == -1

    @Test
    def toInt3204(): Bool = Int16.toInt32(32767i16) == 32767

    @Test
    def toInt3205(): Bool = Int16.toInt32(-32768i16) == -32768

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt6401(): Bool = Int16.toInt64(0i16) == 0i64

    @Test
    def toInt6402(): Bool = Int16.toInt64(1i16) == 1i64

    @Test
    def toInt6403(): Bool = Int16.toInt64(-1i16) == -1i64

    @Test
    def toInt6404(): Bool = Int16.toInt64(32767i16) == 32767i64

    @Test
    def toInt6405(): Bool = Int16.toInt64(-32768i16) == -32768i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Bool = Int16.toBigInt(0i16) == 0ii

    @Test
    def toBigInt02(): Bool = Int16.toBigInt(1i16) == 1ii

    @Test
    def toBigInt03(): Bool = Int16.toBigInt(-1i16) == -1ii

    @Test
    def toBigInt04(): Bool = Int16.toBigInt(32767i16) == 32767ii

    @Test
    def toBigInt05(): Bool = Int16.toBigInt(-32768i16) == -32768ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Bool = Int16.toFloat32(0i16) == 0.0f32

    @Test
    def toFloat3202(): Bool = Int16.toFloat32(1i16) == 1.0f32

    @Test
    def toFloat3203(): Bool = Int16.toFloat32(-1i16) == -1.0f32

    @Test
    def toFloat3204(): Bool = Int16.toFloat32(32767i16) == 32767.0f32

    @Test
    def toFloat3205(): Bool = Int16.toFloat32(-32768i16) == -32768.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Bool = Int16.toFloat64(0i16) == 0.0f64

    @Test
    def toFloat6402(): Bool = Int16.toFloat64(1i16) == 1.0f64

    @Test
    def toFloat6403(): Bool = Int16.toFloat64(-1i16) == -1.0f64

    @Test
    def toFloat6404(): Bool = Int16.toFloat64(32767i16) == 32767.0f64

    @Test
    def toFloat6405(): Bool = Int16.toFloat64(-32768i16) == -32768.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Bool = Int16.toBigDecimal(0i16) == 0.0ff

    @Test
    def toBigDecimal02(): Bool = Int16.toBigDecimal(1i16) == 1.0ff

    @Test
    def toBigDecimal03(): Bool = Int16.toBigDecimal(-1i16) == -1.0ff

    @Test
    def toBigDecimal04(): Bool = Int16.toBigDecimal(32767i16) == 32767.0ff

    @Test
    def toBigDecimal05(): Bool = Int16.toBigDecimal(-32768i16) == -32768.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 0i16) == 0i8

    @Test
    def clampToInt802(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 100i16) == 100i8

    @Test
    def clampToInt803(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 101i16) == 100i8

    @Test
    def clampToInt804(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, -100i16) == -100i8

    @Test
    def clampToInt805(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, -101i16) == -100i8

    @Test
    def clampToInt806(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, Int16.maxValue()) == 100i8

    @Test
    def clampToInt807(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, Int16.minValue()) == -100i8

    @Test
    def clampToInt808(): Bool = Int16.clampToInt8(min = 10i8, max = 0i8, 5i16) == 10i8      // Bad range

    /////////////////////////////////////////////////////////////////////////////
    // shortValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shortValue01(): Bool =
        let i = Int16.valueOf(0i16);
        Int16.shortValue(i) == 0i16

    @Test
    def shortValue02(): Bool =
        let i = Int16.valueOf(1i16);
        Int16.shortValue(i) == 1i16

    @Test
    def shortValue03(): Bool =
        let i = Int16.valueOf(-1i16);
        Int16.shortValue(i) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        import java.lang.Short.equals(##java.lang.Object): Bool \ {};
        let i = Int16.valueOf(0i16);
        equals(i, checked_cast(Int16.valueOf(0i16)))

    @Test
    def valueOf02(): Bool =
        import java.lang.Short.equals(##java.lang.Object): Bool \ {};
        let i = Int16.valueOf(1i16);
        equals(i, checked_cast(Int16.valueOf(1i16)))

    @Test
    def valueOf03(): Bool =
        import java.lang.Short.equals(##java.lang.Object): Bool \ {};
        let i = Int16.valueOf(-1i16);
        equals(i, checked_cast(Int16.valueOf(-1i16)))

}
