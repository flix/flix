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

mod TestInt32 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool = mul(1, 0) == 0

    @Test
    def mul02(): Bool = mul(0, 1) == 0

    @Test
    def mul03(): Bool = mul(0, 0) == 0

    @Test
    def mul04(): Bool = mul(-4, 536870912) == -2147483648

    @Test
    def mul05(): Bool = mul(-1, -2147483647) == 2147483647

    @Test
    def mul06(): Bool = mul(5, 11) == 55

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Bool = Int32.maxValue() == Int32.bitwiseNot(Int32.leftShift(1, 31))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Bool = Int32.minValue() == Int32.leftShift(1, 31)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Bool = Int32.size() == 32

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Bool = Int32.min(1, 2) == 1

    @Test
    def min02(): Bool = Int32.min(2, -1) == -1

    @Test
    def min03(): Bool = Int32.min(-33, -66) == -66

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Bool = Int32.max(48, 49) == 49

    @Test
    def max02(): Bool = Int32.max(4, -16) == 4

    @Test
    def max03(): Bool = Int32.max(-34, -16) == -16

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Bool = Int32.abs(31) == 31

    @Test
    def abs02(): Bool = Int32.abs(-31) == 31

    @Test
    def abs03(): Bool = Int32.abs(2147483647) == 2147483647

    @Test
    def abs04(): Bool = Int32.abs(-2147483647) == 2147483647

    @Test
    def abs05(): Bool = Int32.abs(0) == 0

    @Test
    def abs06(): Bool = Int32.abs(-2147483648) == -1

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mod01(): Bool = (1200000 `Int32.mod` 200000) == 0

    @Test
    def mod02(): Bool = (1200000 `Int32.mod` 500000) == 200000

    @Test
    def mod03(): Bool = (-1200000 `Int32.mod` 500000) == 300000

    @Test
    def mod04(): Bool = (1200000 `Int32.mod` -500000) == -300000

    @Test
    def mod05(): Bool = (-1200000 `Int32.mod` -500000) == -200000

    @Test
    def mod06(): Bool = (1200000i32 `Int32.mod` 200000i32) == 0i32

    @Test
    def mod07(): Bool = (1200000i32 `Int32.mod` 500000i32) == 200000i32

    @Test
    def mod08(): Bool = (-1200000i32 `Int32.mod` 500000i32) == 300000i32

    @Test
    def mod09(): Bool = (1200000i32 `Int32.mod` -500000i32) == -300000i32

    @Test
    def mod10(): Bool = (-1200000i32 `Int32.mod` -500000i32) == -200000i32

    @Test
    def mod11(): Bool = (1 `Int32.mod` 0) == 0

    @Test
    def mod12(): Bool = (1i32 `Int32.mod` 0i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rem01(): Bool = (1200000 `Int32.rem` 200000) == 0

    @Test
    def rem02(): Bool = (1200000 `Int32.rem` 500000) == 200000

    @Test
    def rem03(): Bool = (-1200000 `Int32.rem` 500000) == -200000

    @Test
    def rem04(): Bool = (1200000 `Int32.rem` -500000) == 200000

    @Test
    def rem05(): Bool = (-1200000 `Int32.rem` -500000) == -200000

    @Test
    def rem06(): Bool = (1200000i32 `Int32.rem` 200000i32) == 0i32

    @Test
    def rem07(): Bool = (1200000i32 `Int32.rem` 500000i32) == 200000i32

    @Test
    def rem08(): Bool = (-1200000i32 `Int32.rem` 500000i32) == -200000i32

    @Test
    def rem09(): Bool = (1200000i32 `Int32.rem` -500000i32) == 200000i32

    @Test
    def rem10(): Bool = (-1200000i32 `Int32.rem` -500000i32) == -200000i32

    @Test
    def rem11(): Bool = (1 `Int32.rem` 0) == 0

    @Test
    def rem12(): Bool = (1i32 `Int32.rem` 0i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Bool = Int32.dist(31, -7) == 38

    @Test
    def dist02(): Bool = Int32.dist(-44, -1) == 43

    @Test
    def dist03(): Bool = Int32.dist(-2, -2) == 0

    @Test
    def dist04(): Bool = Int32.dist(-2147483648, -1) == 2147483647

    @Test
    def dist05(): Bool = Int32.dist(-2147483648, 0) == -1

    @Test
    def dist06(): Bool = Int32.dist(-123, 4) == 127

    @Test
    def dist07(): Bool = Int32.dist(-2147483643, 5) == -1

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Bool = Int32.compare(-1, 44) == -1

    @Test
    def compare02(): Bool = Int32.compare(-1, -44) == 1

    @Test
    def compare03(): Bool = Int32.compare(88, 88) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Bool = Int32.signum(-22) == -1

    @Test
    def signum02(): Bool = Int32.signum(22) == 1

    @Test
    def signum03(): Bool = Int32.signum(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Bool = Int32.pow(0i32, 0i32) == 1i32

    @Test
    def pow02(): Bool = Int32.pow(0i32, 1i32) == 0i32

    @Test
    def pow03(): Bool = Int32.pow(1i32, 0i32) == 1i32

    @Test
    def pow04(): Bool = Int32.pow(1i32, 1i32) == 1i32

    @Test
    def pow05(): Bool = Int32.pow(5i32, 2i32) == 25i32

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Bool = Int32.leftShift(8, 0) == 8

    @Test
    def testBitwiseLeftShift02(): Bool = Int32.leftShift(8, 16) == 524288

    @Test
    def testBitwiseLeftShift03(): Bool = Int32.leftShift(8, 28) == -2147483648

    @Test
    def testBitwiseLeftShift04(): Bool = Int32.leftShift(8, 29) == 0

    @Test
    def testBitwiseLeftShift05(): Bool = Int32.leftShift(8i32, 0) == 8i32

    @Test
    def testBitwiseLeftShift06(): Bool = Int32.leftShift(8i32, 16) == 524288i32

    @Test
    def testBitwiseLeftShift07(): Bool = Int32.leftShift(8i32, 28) == -2147483648i32

    @Test
    def testBitwiseLeftShift08(): Bool = Int32.leftShift(8i32, 29) == 0i32


    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Bool = Int32.rightShift(120000, 0) == 120000

    @Test
    def testBitwiseRightShift02(): Bool = Int32.rightShift(120000, 2) == 30000

    @Test
    def testBitwiseRightShift03(): Bool = Int32.rightShift(120000, 31) == 0

    @Test
    def testBitwiseRightShift04(): Bool = Int32.rightShift(-120000, 2) == -30000

    @Test
    def testBitwiseRightShift05(): Bool = Int32.rightShift(120000i32, 0) == 120000i32

    @Test
    def testBitwiseRightShift06(): Bool = Int32.rightShift(120000i32, 2) == 30000i32

    @Test
    def testBitwiseRightShift07(): Bool = Int32.rightShift(120000i32, 31) == 0i32

    @Test
    def testBitwiseRightShift08(): Bool = Int32.rightShift(-120000i32, 2) == -30000i32

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Bool = Int32.logicalRightShift(dist = 4, 2233) == Int32.rightShift(2233, 4)

    @Test
    def logicalRightShift02(): Bool = Int32.logicalRightShift(dist = 33, 223366) == Int32.rightShift(223366, 33)

    @Test
    def logicalRightShift03(): Bool = Int32.logicalRightShift(dist = -8, 2233) == Int32.rightShift(2233, -8)

    @Test
    def logicalRightShift04(): Bool = Int32.logicalRightShift(dist = 4, 2233) == Int32.rightShift(2233, 4)

    @Test
    def logicalRightShift05(): Bool = Int32.logicalRightShift(dist = 1, -1) == 2147483647

    @Test
    def logicalRightShift06(): Bool = Int32.logicalRightShift(dist = 4, -35) == 268435453

    @Test
    def logicalRightShift07(): Bool = Int32.logicalRightShift(dist = -2777, -2777) == 33554410

    @Test
    def logicalRightShift08(): Bool = Int32.logicalRightShift(dist = 64, -233) == -233

    @Test
    def logicalRightShift09(): Bool = Int32.logicalRightShift(dist = 0, -27) == -27

    @Test
    def logicalRightShift10(): Bool = Int32.logicalRightShift(dist = 1, 2147483647) == 1073741823

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Bool = Int32.bitCount(19909) == 8

    @Test
    def bitCount02(): Bool = Int32.bitCount(-1) == 32

    @Test
    def bitCount03(): Bool = Int32.bitCount(-999888888) == 12

    @Test
    def bitCount04(): Bool = Int32.bitCount(0) == 0

    @Test
    def bitCount05(): Bool = Int32.bitCount(2147483647) == 31

    @Test
    def bitCount06(): Bool = Int32.bitCount(-2147483648) == 1

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Bool = Int32.rotateLeft(dist = 4, -1698265258) == -1402440343

    @Test
    def rotateLeft02(): Bool = Int32.rotateLeft(dist = 5, -1836677290) == 1355868882

    @Test
    def rotateLeft03(): Bool = Int32.rotateLeft(dist = 24, 1478293001) == 156769526

    @Test
    def rotateLeft04(): Bool = Int32.rotateLeft(dist = -36, -12267711) == 536104180

    @Test
    def rotateLeft05(): Bool = Int32.rotateLeft(dist = 87, 0) == 0

    @Test
    def rotateLeft06(): Bool = Int32.rotateLeft(dist = 87, -1) == -1

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Bool = Int32.rotateRight(dist = 4, -1698265258) == 1772906613

    @Test
    def rotateRight02(): Bool = Int32.rotateRight(dist = 5, -1836677290) == -1265355718

    @Test
    def rotateRight03(): Bool = Int32.rotateRight(dist = 24, 1478293001) == 485886296

    @Test
    def rotateRight04(): Bool = Int32.rotateRight(dist = -36, -12267711) == -196283361

    @Test
    def rotateRight05(): Bool = Int32.rotateRight(dist = 87, 0) == 0

    @Test
    def rotateRight06(): Bool = Int32.rotateRight(dist = 87, -1) == -1

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Bool = Int32.reverse(1789570000) == 198530390

    @Test
    def reverse02(): Bool = Int32.reverse(198530390) == 1789570000

    @Test
    def reverse03(): Bool = Int32.reverse(-1036473682) == 1968512067

    @Test
    def reverse04(): Bool = Int32.reverse(1968512067) == -1036473682

    @Test
    def reverse05(): Bool = Int32.reverse(137) == -1862270976

    @Test
    def reverse06(): Bool = Int32.reverse(-1862270976) == 137

    @Test
    def reverse07(): Bool = Int32.reverse(4) == 536870912

    @Test
    def reverse08(): Bool = Int32.reverse(536870912) == 4

    @Test
    def reverse09(): Bool = Int32.reverse(-1) == -1

    @Test
    def reverse10(): Bool = Int32.reverse(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Bool = Int32.highestOneBitPosition(-1) == 31

    @Test
    def highestOneBitPosition02(): Bool = Int32.highestOneBitPosition(-2147483648) == 31

    @Test
    def highestOneBitPosition03(): Bool = Int32.highestOneBitPosition(1) == 0

    @Test
    def highestOneBitPosition04(): Bool = Int32.highestOneBitPosition(2) == 1

    @Test
    def highestOneBitPosition05(): Bool = Int32.highestOneBitPosition(3) == 1

    @Test
    def highestOneBitPosition06(): Bool = Int32.highestOneBitPosition(20) == 4

    @Test
    def highestOneBitPosition07(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 30)) == 30

    @Test
    def highestOneBitPosition08(): Bool = Int32.highestOneBitPosition(2147483647) == 30

    @Test
    def highestOneBitPosition09(): Bool = Int32.highestOneBitPosition(0) == -1

    @Test
    def highestOneBitPosition10(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 18) + Int32.pow(2, 23)) == 23

    @Test
    def highestOneBitPosition11(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 3) + Int32.pow(2, 7)) == 7

    @Test
    def highestOneBitPosition12(): Bool = Int32.highestOneBitPosition(-32767) == 31

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Bool = Int32.highestOneBit(-1) == Int32.leftShift(1, 31)

    @Test
    def highestOneBit02(): Bool = Int32.highestOneBit(-2147483000) == Int32.leftShift(1, 31)

    @Test
    def highestOneBit03(): Bool = Int32.highestOneBit(-77) == Int32.leftShift(1, 31)

    @Test
    def highestOneBit04(): Bool = Int32.highestOneBit(2) == 2

    @Test
    def highestOneBit05(): Bool = Int32.highestOneBit(3) == 2

    @Test
    def highestOneBit06(): Bool = Int32.highestOneBit(20) == 16

    @Test
    def highestOneBit07(): Bool = Int32.highestOneBit(Int32.pow(2, 30)) == 1073741824

    @Test
    def highestOneBit08(): Bool = Int32.highestOneBit(2147483647) == 1073741824

    @Test
    def highestOneBit09(): Bool = Int32.highestOneBit(0) == 0

    @Test
    def highestOneBit10(): Bool = Int32.highestOneBit(Int32.pow(2, 18) + Int32.pow(2, 24)) == 16777216

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Bool = Int32.numberOfLeadingZeros(0) == 32

    @Test
    def numberOfLeadingZeros02(): Bool = Int32.numberOfLeadingZeros(Int32.leftShift(1, 31)) == 0

    @Test
    def numberOfLeadingZeros03(): Bool = Int32.numberOfLeadingZeros(-1) == 0

    @Test
    def numberOfLeadingZeros04(): Bool = Int32.numberOfLeadingZeros(Int32.bitwiseNot(Int32.leftShift(1, 31))) == 1

    @Test
    def numberOfLeadingZeros05(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 29)) == 2

    @Test
    def numberOfLeadingZeros06(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 24) + Int32.pow(2, 18)) == 7

    @Test
    def numberOfLeadingZeros07(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 7) + Int32.pow(2, 6)) == 24

    @Test
    def numberOfLeadingZeros08(): Bool = Int32.numberOfLeadingZeros(20) == 27

    @Test
    def numberOfLeadingZeros09(): Bool = Int32.numberOfLeadingZeros(1) == 31

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Bool = Int32.lowestOneBitPosition(-1) == 0

    @Test
    def lowestOneBitPosition02(): Bool = Int32.lowestOneBitPosition(-2147483648) == 31

    @Test
    def lowestOneBitPosition03(): Bool = Int32.lowestOneBitPosition(1) == 0

    @Test
    def lowestOneBitPosition04(): Bool = Int32.lowestOneBitPosition(2) == 1

    @Test
    def lowestOneBitPosition05(): Bool = Int32.lowestOneBitPosition(3) == 0

    @Test
    def lowestOneBitPosition06(): Bool = Int32.lowestOneBitPosition(4) == 2

    @Test
    def lowestOneBitPosition07(): Bool = Int32.lowestOneBitPosition(12) == 2

    @Test
    def lowestOneBitPosition08(): Bool = Int32.lowestOneBitPosition(Int32.pow(2, 30)) == 30

    @Test
    def lowestOneBitPosition09(): Bool = Int32.lowestOneBitPosition(Int32.pow(2, 23) + Int32.pow(2, 18)) == 18

    @Test
    def lowestOneBitPosition10(): Bool = Int32.lowestOneBitPosition(2147483647) == 0

    @Test
    def lowestOneBitPosition11(): Bool = Int32.lowestOneBitPosition(0) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Bool = Int32.lowestOneBit(-1) == 1

    @Test
    def lowestOneBit02(): Bool = Int32.lowestOneBit(-2147483648) == Int32.leftShift(1, 31)

    @Test
    def lowestOneBit03(): Bool = Int32.lowestOneBit(1) == 1

    @Test
    def lowestOneBit04(): Bool = Int32.lowestOneBit(2) == 2

    @Test
    def lowestOneBit05(): Bool = Int32.lowestOneBit(6) == 2

    @Test
    def lowestOneBit06(): Bool = Int32.lowestOneBit(12) == 4

    @Test
    def lowestOneBit07(): Bool = Int32.lowestOneBit(Int32.pow(2, 6) + Int32.pow(2, 18) + Int32.pow(2, 27)) == 64

    @Test
    def lowestOneBit08(): Bool = Int32.lowestOneBit(Int32.pow(2, 30)) == 1073741824

    @Test
    def lowestOneBit09(): Bool = Int32.lowestOneBit(Int32.pow(2, 18) + Int32.pow(2, 23)) == 262144

    @Test
    def lowestOneBit10(): Bool = Int32.lowestOneBit(2147483647) == 1

    @Test
    def lowestOneBit11(): Bool = Int32.lowestOneBit(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Bool = Int32.numberOfTrailingZeros(0) == 32

    @Test
    def numberOfTrailingZeros02(): Bool = Int32.numberOfTrailingZeros(3) == 0

    @Test
    def numberOfTrailingZeros03(): Bool = Int32.numberOfTrailingZeros(42) == 1

    @Test
    def numberOfTrailingZeros04(): Bool = Int32.numberOfTrailingZeros(Int32.pow(2, 16) + Int32.pow(2, 22)) == 16

    @Test
    def numberOfTrailingZeros05(): Bool = Int32.numberOfTrailingZeros(Int32.pow(2, 30)) == 30

    @Test
    def numberOfTrailingZeros06(): Bool = Int32.numberOfTrailingZeros(-1) == 0

    @Test
    def numberOfTrailingZeros07(): Bool = Int32.numberOfTrailingZeros(-2147483648) == 31

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Bool = Int32.getBit(pos = 5, -1) == 1

    @Test
    def getBit02(): Bool = Int32.getBit(pos = 31, -1000000) == 1

    @Test
    def getBit03(): Bool = Int32.getBit(pos = 23, Int32.pow(2, 23)) == 1

    @Test
    def getBit04(): Bool = Int32.getBit(pos = 24, Int32.pow(2, 23)) == 0

    @Test
    def getBit05(): Bool = Int32.getBit(pos = 22, Int32.pow(2, 23)) == 0

    @Test
    def getBit06(): Bool = Int32.getBit(pos = 25, -1 - Int32.pow(2, 25)) == 0

    @Test
    def getBit07(): Bool = Int32.getBit(pos = 26, -1 - Int32.pow(2, 25)) == 1

    @Test
    def getBit08(): Bool = Int32.getBit(pos = 15, 0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Bool = Int32.setBit(pos = 0, 0) == 1

    @Test
    def setBit02(): Bool = Int32.setBit(pos = 1, 0) == 2

    @Test
    def setBit03(): Bool = Int32.setBit(pos = 31, 0) == -2147483648

    @Test
    def setBit04(): Bool = Int32.setBit(pos = 30, 0) == 1073741824

    @Test
    def setBit05(): Bool = Int32.setBit(pos = 31, -1418638422) == -1418638422

    @Test
    def setBit07(): Bool = Int32.setBit(pos = 4, -17) == -1

    @Test
    def setBit08(): Bool = Int32.setBit(pos = 10, -1025) == -1

    @Test
    def setBit09(): Bool = Int32.setBit(pos = 31, -17) == -17

    @Test
    def setBit10(): Bool = Int32.setBit(pos = 30, -17) == -17

    @Test
    def setBit11(): Bool = Int32.setBit(pos = 32, 0) == 1

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Bool = Int32.clearBit(pos = 0, 1) == 0

    @Test
    def clearBit02(): Bool = Int32.clearBit(pos = 1, 1) == 1

    @Test
    def clearBit03(): Bool = Int32.clearBit(pos = 1, 2) == 0

    @Test
    def clearBit04(): Bool = Int32.clearBit(pos = 2, 2) == 2

    @Test
    def clearBit05(): Bool = Int32.clearBit(pos = 31, 0) == 0

    @Test
    def clearBit06(): Bool = Int32.clearBit(pos = 30, 0) == 0

    @Test
    def clearBit07(): Bool = Int32.clearBit(pos = 31, -2147483648) == 0

    @Test
    def clearBit08(): Bool = Int32.clearBit(pos = 27, 442931848) == 308714120

    @Test
    def clearBit09(): Bool = Int32.clearBit(pos = 0, -1) == -2

    @Test
    def clearBit10(): Bool = Int32.clearBit(pos = 1, -1) == -3

    @Test
    def clearBit11(): Bool = Int32.clearBit(pos = 5, -1) == -33

    @Test
    def clearBit12(): Bool = Int32.clearBit(pos = 31, -1) == 2147483647

    @Test
    def clearBit13(): Bool = Int32.clearBit(pos = 32, -1) == -2

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Bool = Int32.flipBit(pos = 0, 1) == 0

    @Test
    def flipBit02(): Bool = Int32.flipBit(pos = 1, 1) == 3

    @Test
    def flipBit03(): Bool = Int32.flipBit(pos = 1, 2) == 0

    @Test
    def flipBit04(): Bool = Int32.flipBit(pos = 2, 2) == 6

    @Test
    def flipBit05(): Bool = Int32.flipBit(pos = 31, 0) == -2147483648

    @Test
    def flipBit06(): Bool = Int32.flipBit(pos = 30, 0) == 1073741824

    @Test
    def flipBit07(): Bool = Int32.flipBit(pos = 31, -2147483648) == 0

    @Test
    def flipBit08(): Bool = Int32.flipBit(pos = 0, -1) == -2

    @Test
    def flipBit9(): Bool = Int32.flipBit(pos = 1, -1) == -3

    @Test
    def flipBit10(): Bool = Int32.flipBit(pos = 5, -1) == -33

    @Test
    def flipBit11(): Bool = Int32.flipBit(pos = 31, -1) == 2147483647

    @Test
    def flipBit12(): Bool = Int32.flipBit(pos = 32, -1) == -2

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Bool = Int32.log2(10) == 3

    @Test
    def log202(): Bool = Int32.log2(10000) == 13

    @Test
    def log203(): Bool = Int32.log2(8) == 3

    @Test
    def log204(): Bool = Int32.log2(256) == 8

    @Test
    def log205(): Bool = Int32.log2(1) == 0

    @Test
    def log206(): Bool = Int32.log2(12345678) == 23

    @Test
    def log207(): Bool = Int32.log2(20) == 4

    @Test
    def log208(): Bool = Int32.log2(0) == 0

    @Test
    def log209(): Bool = Int32.log2(-1) == 0

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Bool = Int32.bitwiseAnd(0i32, 0i32) == 0i32

    @Test
    def bitwiseAnd02(): Bool = Int32.bitwiseAnd(127i32, 0i32) == 0i32

    @Test
    def bitwiseAnd03(): Bool = Int32.bitwiseAnd(-128i32, 0i32) == 0i32

    @Test
    def bitwiseAnd04(): Bool = Int32.bitwiseAnd(0i32, 1i32) == 0i32

    @Test
    def bitwiseAnd05(): Bool = Int32.bitwiseAnd(127i32, 1i32) == 1i32

    @Test
    def bitwiseAnd06(): Bool = Int32.bitwiseAnd(-128i32, 1i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Bool = Int32.bitwiseNot(0i32) == -1i32

    @Test
    def bitwiseNot02(): Bool = Int32.bitwiseNot(1i32) == -2i32

    @Test
    def bitwiseNot03(): Bool = Int32.bitwiseNot(-1i32) == -0i32

    @Test
    def bitwiseNot04(): Bool = Int32.bitwiseNot(127i32) == -128i32

    @Test
    def bitwiseNot05(): Bool = Int32.bitwiseNot(-128i32) == 127i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Bool = Int32.bitwiseOr(0i32, 0i32) == 0i32

    @Test
    def bitwiseOr02(): Bool = Int32.bitwiseOr(127i32, 0i32) == 127i32

    @Test
    def bitwiseOr03(): Bool = Int32.bitwiseOr(-128i32, 0i32) == -128i32

    @Test
    def bitwiseOr04(): Bool = Int32.bitwiseOr(0i32, 1i32) == 1i32

    @Test
    def bitwiseOr05(): Bool = Int32.bitwiseOr(127i32, 1i32) == 127i32

    @Test
    def bitwiseOr06(): Bool = Int32.bitwiseOr(-128i32, 1i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Bool = Int32.bitwiseXor(0i32, 0i32) == 0i32

    @Test
    def bitwiseXor02(): Bool = Int32.bitwiseXor(127i32, 0i32) == 127i32

    @Test
    def bitwiseXor03(): Bool = Int32.bitwiseXor(-128i32, 0i32) == -128i32

    @Test
    def bitwiseXor04(): Bool = Int32.bitwiseXor(0i32, 1i32) == 1i32

    @Test
    def bitwiseXor05(): Bool = Int32.bitwiseXor(127i32, 1i32) == 126i32

    @Test
    def bitwiseXor06(): Bool = Int32.bitwiseXor(-128i32, 1i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Bool = Int32.factorial(0) == 1

    @Test
    def factorial02(): Bool = Int32.factorial(1) == 1

    @Test
    def factorial03(): Bool = Int32.factorial(2) == 2

    @Test
    def factorial04(): Bool = Int32.factorial(3) == 6

    @Test
    def factorial05(): Bool = Int32.factorial(4) == 24

    @Test
    def factorial06(): Bool = Int32.factorial(5) == 120

    @Test
    def factorial07(): Bool = Int32.factorial(10) == 3628800

    @Test
    def factorial08(): Bool = Int32.factorial(-1) == 0

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Bool = Int32.toString(0) == "0"

    @Test
    def toString02(): Bool = Int32.toString(1) == "1"

    @Test
    def toString03(): Bool = Int32.toString(-1) == "-1"

    @Test
    def toString04(): Bool = Int32.toString(2147483647) == "2147483647"

    @Test
    def toString05(): Bool = Int32.toString(-2147483648) == "-2147483648"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Bool = Int32.fromString("Bad") |> Option.isEmpty

    @Test
    def fromString02(): Bool = Int32.fromString(" 1") == Some(1)

    @Test
    def fromString03(): Bool = Int32.fromString("1 ") == Some(1)

    @Test
    def fromString04(): Bool = Int32.fromString("0") == Some(0)

    @Test
    def fromString05(): Bool = Int32.fromString("1") == Some(1)

    @Test
    def fromString06(): Bool = Int32.fromString("-1") == Some(-1)

    @Test
    def fromString07(): Bool = Int32.fromString("2147483647") == Some(2147483647)

    @Test
    def fromString08(): Bool = Int32.fromString("-2147483648") == Some(-2147483648)

    /////////////////////////////////////////////////////////////////////////////
    // parse                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def parse01(): Bool = ("0" |> Int32.parse(10)) == Ok(0)

    @Test
    def parse02(): Bool = ("     0      " |> Int32.parse(10)) == Ok(0)

    @Test
    def parse03(): Bool = ("-FF" |> Int32.parse(16)) == Ok(-255)

    @Test
    def parse04(): Bool = ("1100110" |> Int32.parse(2)) == Ok(102)

    @Test
    def parse05(): Bool = ("Kona" |> Int32.parse(27)) == Ok(411787)

    @Test
    def parse06(): Bool = ("2147483648" |> Int32.parse(10)) |> Result.isErr

    @Test
    def parse07(): Bool = ("Kona" |> Int32.parse(10)) |> Result.isErr

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Bool = Int32.tryToInt8(0) == Some(0i8)

    @Test
    def tryToInt802(): Bool = Int32.tryToInt8(1) == Some(1i8)

    @Test
    def tryToInt803(): Bool = Int32.tryToInt8(-1) == Some(-1i8)

    @Test
    def tryToInt804(): Bool = Int32.tryToInt8(127) == Some(127i8)

    @Test
    def tryToInt805(): Bool = Int32.tryToInt8(128) == None

    @Test
    def tryToInt806(): Bool = Int32.tryToInt8(-128) == Some(-128i8)

    @Test
    def tryToInt807(): Bool = Int32.tryToInt8(-129) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Bool = Int32.tryToInt16(0) == Some(0i16)

    @Test
    def tryToInt1602(): Bool = Int32.tryToInt16(1) == Some(1i16)

    @Test
    def tryToInt1603(): Bool = Int32.tryToInt16(-1) == Some(-1i16)

    @Test
    def tryToInt1604(): Bool = Int32.tryToInt16(32767) == Some(32767i16)

    @Test
    def tryToInt1605(): Bool = Int32.tryToInt16(32768) == None

    @Test
    def tryToInt1606(): Bool = Int32.tryToInt16(-32768) == Some(-32768i16)

    @Test
    def tryToInt1607(): Bool = Int32.tryToInt16(-32769) == None

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt6401(): Bool = Int32.toInt64(0) == 0i64

    @Test
    def toInt6402(): Bool = Int32.toInt64(1) == 1i64

    @Test
    def toInt6403(): Bool = Int32.toInt64(-1) == -1i64

    @Test
    def toInt6404(): Bool = Int32.toInt64(2147483647) == 2147483647i64

    @Test
    def toInt6405(): Bool = Int32.toInt64(-2147483648) == -2147483648i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Bool = Int32.toBigInt(0) == 0ii

    @Test
    def toBigInt02(): Bool = Int32.toBigInt(1) == 1ii

    @Test
    def toBigInt03(): Bool = Int32.toBigInt(-1) == -1ii

    @Test
    def toBigInt04(): Bool = Int32.toBigInt(2147483647) == 2147483647ii

    @Test
    def toBigInt05(): Bool = Int32.toBigInt(-2147483648) == -2147483648ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Bool = Int32.toFloat32(0) == 0.0f32

    @Test
    def toFloat3202(): Bool = Int32.toFloat32(1) == 1.0f32

    @Test
    def toFloat3203(): Bool = Int32.toFloat32(-1) == -1.0f32

    @Test
    def toFloat3204(): Bool = Int32.toFloat32(2147483647) == 2147483647.0f32

    @Test
    def toFloat3205(): Bool = Int32.toFloat32(-2147483648) == -2147483648.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Bool = Int32.toFloat64(0) == 0.0f64

    @Test
    def toFloat6402(): Bool = Int32.toFloat64(1) == 1.0f64

    @Test
    def toFloat6403(): Bool = Int32.toFloat64(-1) == -1.0f64

    @Test
    def toFloat6404(): Bool = Int32.toFloat64(2147483647) == 2147483647.0f64

    @Test
    def toFloat6405(): Bool = Int32.toFloat64(-2147483648) == -2147483648.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Bool = Int32.toBigDecimal(0) == 0.0ff

    @Test
    def toBigDecimal02(): Bool = Int32.toBigDecimal(1) == 1.0ff

    @Test
    def toBigDecimal03(): Bool = Int32.toBigDecimal(-1) == -1.0ff

    @Test
    def toBigDecimal04(): Bool = Int32.toBigDecimal(2147483647) == 2147483647.0ff

    @Test
    def toBigDecimal05(): Bool = Int32.toBigDecimal(-2147483648) == -2147483648.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 0) == 0i8

    @Test
    def clampToInt802(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 100) == 100i8

    @Test
    def clampToInt803(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 101) == 100i8

    @Test
    def clampToInt804(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, -100) == -100i8

    @Test
    def clampToInt805(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, -101) == -100i8

    @Test
    def clampToInt806(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, Int32.maxValue()) == 100i8

    @Test
    def clampToInt807(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, Int32.minValue()) == -100i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 0) == 0i16

    @Test
    def clampToInt1602(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 100) == 100i16

    @Test
    def clampToInt1603(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 101) == 100i16

    @Test
    def clampToInt1604(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, -100) == -100i16

    @Test
    def clampToInt1605(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, -101) == -100i16

    @Test
    def clampToInt1606(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, Int32.maxValue()) == 100i16

    @Test
    def clampToInt1607(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, Int32.minValue()) == -100i16

    /////////////////////////////////////////////////////////////////////////////
    // intValue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intValue01(): Bool =
        let i = Int32.valueOf(0);
        Int32.intValue(i) == 0

    @Test
    def intValue02(): Bool =
        let i = Int32.valueOf(1);
        Int32.intValue(i) == 1

    @Test
    def intValue03(): Bool =
        let i = Int32.valueOf(-1);
        Int32.intValue(i) == -1

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        import java.lang.Integer.equals(##java.lang.Object): Bool \ {};
        let i = Int32.valueOf(0);
        equals(i, checked_cast(Int32.valueOf(0)))

    @Test
    def valueOf02(): Bool =
        import java.lang.Integer.equals(##java.lang.Object): Bool \ {};
        let i = Int32.valueOf(1);
        equals(i, checked_cast(Int32.valueOf(1)))

    @Test
    def valueOf03(): Bool =
        import java.lang.Integer.equals(##java.lang.Object): Bool \ {};
        let i = Int32.valueOf(-1);
        equals(i, checked_cast(Int32.valueOf(-1)))

}
