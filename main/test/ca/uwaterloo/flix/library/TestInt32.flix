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

namespace TestInt32 {
    
    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Int32.maxValue() == ~~~(1 <<< 31)
    
    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Int32.minValue() == (1 <<< 31)
    
    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Int32.size() == 32
    
    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Int32.min(1, 2) == 1
    
    @test
    def min02(): Bool = Int32.min(2, -1) == -1
    
    @test
    def min03(): Bool = Int32.min(-33, -66) == -66
    
    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Int32.max(48, 49) == 49
    
    @test
    def max02(): Bool = Int32.max(4, -16) == 4
    
    @test
    def max03(): Bool = Int32.max(-34, -16) == -16
    
    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def abs01(): Bool = Int32.abs(31) == 31
    
    @test
    def abs02(): Bool = Int32.abs(-31) == 31
    
    @test
    def abs03(): Bool = Int32.abs(2147483647) == 2147483647
    
    @test
    def abs04(): Bool = Int32.abs(-2147483647) == 2147483647
    
    @test
    def abs05(): Bool = Int32.abs(0) == 0
    
    @test
    def abs06(): Bool = Int32.abs(-2147483648) == -1
    
    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def dist01(): Bool = Int32.dist(31, -7) == 38
    
    @test
    def dist02(): Bool = Int32.dist(-44, -1) == 43
    
    @test
    def dist03(): Bool = Int32.dist(-2, -2) == 0
    
    @test
    def dist04(): Bool = Int32.dist(-2147483648, -1) == 2147483647
    
    @test
    def dist05(): Bool = Int32.dist(-2147483648, 0) == -1
    
    @test
    def dist06(): Bool = Int32.dist(-123, 4) == 127
    
    @test
    def dist07(): Bool = Int32.dist(-2147483643, 5) == -1
    
    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = Int32.compare(-1, 44) == -1
    
    @test
    def compare02(): Bool = Int32.compare(-1, -44) == 1
    
    @test
    def compare03(): Bool = Int32.compare(88, 88) == 0
    
    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = Int32.signum(-22) == -1
    
    @test
    def signum02(): Bool = Int32.signum(22) == 1
    
    @test
    def signum03(): Bool = Int32.signum(0) == 0
    
    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def logicalRightShift01(): Bool = Int32.logicalRightShift(2233, 4) == (2233 >>> 4)
    
    @test
    def logicalRightShift02(): Bool = Int32.logicalRightShift(223366, 33) == (223366 >>> 33)
    
    @test
    def logicalRightShift03(): Bool = Int32.logicalRightShift(2233, -8) == (2233 >>> -8)
    
    @test
    def logicalRightShift04(): Bool = Int32.logicalRightShift(2233, 4) == (2233 >>> 4)
    
    @test
    def logicalRightShift05(): Bool = Int32.logicalRightShift(-1, 1) == 2147483647
    
    @test
    def logicalRightShift06(): Bool = Int32.logicalRightShift(-35, 4) == 268435453
    
    @test
    def logicalRightShift07(): Bool = Int32.logicalRightShift(-2777, -2777) == 33554410
    
    @test
    def logicalRightShift08(): Bool = Int32.logicalRightShift(-233, 64) == -233
    
    @test
    def logicalRightShift09(): Bool = Int32.logicalRightShift(-27, 0) == -27
    
    @test
    def logicalRightShift10(): Bool = Int32.logicalRightShift(2147483647, 1) == 1073741823
    
    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def bitCount01(): Bool = Int32.bitCount(19909) == 8
    
    @test
    def bitCount02(): Bool = Int32.bitCount(-1) == 32
    
    @test
    def bitCount03(): Bool = Int32.bitCount(-999888888) == 12
    
    @test
    def bitCount04(): Bool = Int32.bitCount(0) == 0
    
    @test
    def bitCount05(): Bool = Int32.bitCount(2147483647) == 31
    
    @test
    def bitCount06(): Bool = Int32.bitCount(-2147483648) == 1
    
    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateLeft01(): Bool = Int32.rotateLeft(-1698265258, 4) == -1402440343
    
    @test
    def rotateLeft02(): Bool = Int32.rotateLeft(-1836677290, 5) == 1355868882
    
    @test
    def rotateLeft03(): Bool = Int32.rotateLeft(1478293001, 24) == 156769526
    
    @test
    def rotateLeft04(): Bool = Int32.rotateLeft(-12267711, -36) == 536104180
    
    @test
    def rotateLeft05(): Bool = Int32.rotateLeft(0, 87) == 0
    
    @test
    def rotateLeft06(): Bool = Int32.rotateLeft(-1, 87) == -1
    
    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateRight01(): Bool = Int32.rotateRight(-1698265258, 4) == 1772906613
    
    @test
    def rotateRight02(): Bool = Int32.rotateRight(-1836677290, 5) == -1265355718
    
    @test
    def rotateRight03(): Bool = Int32.rotateRight(1478293001, 24) == 485886296
    
    @test
    def rotateRight04(): Bool = Int32.rotateRight(-12267711, -36) == -196283361
    
    @test
    def rotateRight05(): Bool = Int32.rotateRight(0, 87) == 0
    
    @test
    def rotateRight06(): Bool = Int32.rotateRight(-1, 87) == -1
    
    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def reverse01(): Bool = Int32.reverse(1789570000) == 198530390
    
    @test
    def reverse02(): Bool = Int32.reverse(198530390) == 1789570000
    
    @test
    def reverse03(): Bool = Int32.reverse(-1036473682) == 1968512067
    
    @test
    def reverse04(): Bool = Int32.reverse(1968512067) == -1036473682
    
    @test
    def reverse05(): Bool = Int32.reverse(137) == -1862270976
    
    @test
    def reverse06(): Bool = Int32.reverse(-1862270976) == 137
    
    @test
    def reverse07(): Bool = Int32.reverse(4) == 536870912
    
    @test
    def reverse08(): Bool = Int32.reverse(536870912) == 4
    
    @test
    def reverse09(): Bool = Int32.reverse(-1) == -1
    
    @test
    def reverse10(): Bool = Int32.reverse(0) == 0
    
    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBitPosition01(): Bool = Int32.highestOneBitPosition(-1) == 31
    
    @test
    def highestOneBitPosition02(): Bool = Int32.highestOneBitPosition(-2147483648) == 31
    
    @test
    def highestOneBitPosition03(): Bool = Int32.highestOneBitPosition(1) == 0
    
    @test
    def highestOneBitPosition04(): Bool = Int32.highestOneBitPosition(2) == 1
    
    @test
    def highestOneBitPosition05(): Bool = Int32.highestOneBitPosition(3) == 1
    
    @test
    def highestOneBitPosition06(): Bool = Int32.highestOneBitPosition(20) == 4
    
    @test
    def highestOneBitPosition07(): Bool = Int32.highestOneBitPosition(2 ** 30) == 30
    
    @test
    def highestOneBitPosition08(): Bool = Int32.highestOneBitPosition(2147483647) == 30
    
    @test
    def highestOneBitPosition09(): Bool = Int32.highestOneBitPosition(0) == -1
    
    @test
    def highestOneBitPosition10(): Bool = Int32.highestOneBitPosition(2 ** 18 + 2 ** 23) == 23
    
    @test
    def highestOneBitPosition11(): Bool = Int32.highestOneBitPosition(2 ** 3 + 2 ** 7) == 7
    
    @test
    def highestOneBitPosition12(): Bool = Int32.highestOneBitPosition(-32767) == 31
    
    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBit01(): Bool = Int32.highestOneBit(-1) == (1 <<< 31)
    
    @test
    def highestOneBit02(): Bool = Int32.highestOneBit(-2147483000) == (1 <<< 31)
    
    @test
    def highestOneBit03(): Bool = Int32.highestOneBit(-77) == (1 <<< 31)
    
    @test
    def highestOneBit04(): Bool = Int32.highestOneBit(2) == 2
    
    @test
    def highestOneBit05(): Bool = Int32.highestOneBit(3) == 2
    
    @test
    def highestOneBit06(): Bool = Int32.highestOneBit(20) == 16
    
    @test
    def highestOneBit07(): Bool = Int32.highestOneBit(2 ** 30) == 1073741824
    
    @test
    def highestOneBit08(): Bool = Int32.highestOneBit(2147483647) == 1073741824
    
    @test
    def highestOneBit09(): Bool = Int32.highestOneBit(0) == 0
    
    @test
    def highestOneBit10(): Bool = Int32.highestOneBit(2 ** 18 + 2 ** 24) == 16777216
    
    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfLeadingZeros01(): Bool = Int32.numberOfLeadingZeros(0) == 32
    
    @test
    def numberOfLeadingZeros02(): Bool = Int32.numberOfLeadingZeros(1 <<< 31) == 0
    
    @test
    def numberOfLeadingZeros03(): Bool = Int32.numberOfLeadingZeros(-1) == 0
    
    @test
    def numberOfLeadingZeros04(): Bool = Int32.numberOfLeadingZeros(~~~(1 <<< 31)) == 1
    
    @test
    def numberOfLeadingZeros05(): Bool = Int32.numberOfLeadingZeros(2 ** 29) == 2
    
    @test
    def numberOfLeadingZeros06(): Bool = Int32.numberOfLeadingZeros(2 ** 24 + 2 ** 18) == 7
    
    @test
    def numberOfLeadingZeros07(): Bool = Int32.numberOfLeadingZeros(2 ** 7 + 2 ** 6) == 24
    
    @test
    def numberOfLeadingZeros08(): Bool = Int32.numberOfLeadingZeros(20) == 27
    
    @test
    def numberOfLeadingZeros09(): Bool = Int32.numberOfLeadingZeros(1) == 31
    
    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBitPosition01(): Bool = Int32.lowestOneBitPosition(-1) == 0
    
    @test
    def lowestOneBitPosition02(): Bool = Int32.lowestOneBitPosition(-2147483648) == 31
    
    @test
    def lowestOneBitPosition03(): Bool = Int32.lowestOneBitPosition(1) == 0
    
    @test
    def lowestOneBitPosition04(): Bool = Int32.lowestOneBitPosition(2) == 1
    
    @test
    def lowestOneBitPosition05(): Bool = Int32.lowestOneBitPosition(3) == 0
    
    @test
    def lowestOneBitPosition06(): Bool = Int32.lowestOneBitPosition(4) == 2
    
    @test
    def lowestOneBitPosition07(): Bool = Int32.lowestOneBitPosition(12) == 2
    
    @test
    def lowestOneBitPosition08(): Bool = Int32.lowestOneBitPosition(2 ** 30) == 30
    
    @test
    def lowestOneBitPosition09(): Bool = Int32.lowestOneBitPosition(2 ** 23 + 2 ** 18) == 18
    
    @test
    def lowestOneBitPosition10(): Bool = Int32.lowestOneBitPosition(2147483647) == 0
    
    @test
    def lowestOneBitPosition11(): Bool = Int32.lowestOneBitPosition(0) == -1
    
    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBit01(): Bool = Int32.lowestOneBit(-1) == 1
    
    @test
    def lowestOneBit02(): Bool = Int32.lowestOneBit(-2147483648) == (1 <<< 31)
    
    @test
    def lowestOneBit03(): Bool = Int32.lowestOneBit(1) == 1
    
    @test
    def lowestOneBit04(): Bool = Int32.lowestOneBit(2) == 2
    
    @test
    def lowestOneBit05(): Bool = Int32.lowestOneBit(6) == 2
    
    @test
    def lowestOneBit06(): Bool = Int32.lowestOneBit(12) == 4
    
    @test
    def lowestOneBit07(): Bool = Int32.lowestOneBit(2 ** 6 + 2 ** 18 + 2 ** 27) == 64
    
    @test
    def lowestOneBit08(): Bool = Int32.lowestOneBit(2 ** 30) == 1073741824
    
    @test
    def lowestOneBit09(): Bool = Int32.lowestOneBit(2 ** 18 + 2 ** 23) == 262144
    
    @test
    def lowestOneBit10(): Bool = Int32.lowestOneBit(2147483647) == 1
    
    @test
    def lowestOneBit11(): Bool = Int32.lowestOneBit(0) == 0
    
    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfTrailingZeros01(): Bool = Int32.numberOfTrailingZeros(0) == 32
    
    @test
    def numberOfTrailingZeros02(): Bool = Int32.numberOfTrailingZeros(3) == 0
    
    @test
    def numberOfTrailingZeros03(): Bool = Int32.numberOfTrailingZeros(42) == 1
    
    @test
    def numberOfTrailingZeros04(): Bool = Int32.numberOfTrailingZeros(2 ** 16 + 2 ** 22) == 16
    
    @test
    def numberOfTrailingZeros05(): Bool = Int32.numberOfTrailingZeros(2 ** 30) == 30
    
    @test
    def numberOfTrailingZeros06(): Bool = Int32.numberOfTrailingZeros(-1) == 0
    
    @test
    def numberOfTrailingZeros07(): Bool = Int32.numberOfTrailingZeros(-2147483648) == 31
    
    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getBit01(): Bool = Int32.getBit(-1, 5) == 1
    
    @test
    def getBit02(): Bool = Int32.getBit(-1000000, 31) == 1
    
    @test
    def getBit03(): Bool = Int32.getBit(2 ** 23, 23) == 1
    
    @test
    def getBit04(): Bool = Int32.getBit(2 ** 23, 24) == 0
    
    @test
    def getBit05(): Bool = Int32.getBit(2 ** 23, 22) == 0
    
    @test
    def getBit06(): Bool = Int32.getBit(-1 - 2 ** 25, 25) == 0
    
    @test
    def getBit07(): Bool = Int32.getBit(-1 - 2 ** 25, 26) == 1
    
    @test
    def getBit08(): Bool = Int32.getBit(0, 15) == 0
    
    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def setBit01(): Bool = Int32.setBit(0, 0) == 1
    
    @test
    def setBit02(): Bool = Int32.setBit(0, 1) == 2
    
    @test
    def setBit03(): Bool = Int32.setBit(0, 31) == -2147483648
    
    @test
    def setBit04(): Bool = Int32.setBit(0, 30) == 1073741824
    
    @test
    def setBit05(): Bool = Int32.setBit(-1418638422, 31) == -1418638422
    
    @test
    def setBit07(): Bool = Int32.setBit(-17, 4) == -1
    
    @test
    def setBit08(): Bool = Int32.setBit(-1025, 10) == -1
    
    @test
    def setBit09(): Bool = Int32.setBit(-17, 31) == -17
    
    @test
    def setBit10(): Bool = Int32.setBit(-17, 30) == -17
    
    @test
    def setBit11(): Bool = Int32.setBit(0, 32) == 1
    
    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clearBit01(): Bool = Int32.clearBit(1, 0) == 0
    
    @test
    def clearBit02(): Bool = Int32.clearBit(1, 1) == 1
    
    @test
    def clearBit03(): Bool = Int32.clearBit(2, 1) == 0
    
    @test
    def clearBit04(): Bool = Int32.clearBit(2, 2) == 2
    
    @test
    def clearBit05(): Bool = Int32.clearBit(0, 31) == 0
    
    @test
    def clearBit06(): Bool = Int32.clearBit(0, 30) == 0
    
    @test
    def clearBit07(): Bool = Int32.clearBit(-2147483648, 31) == 0
    
    @test
    def clearBit08(): Bool = Int32.clearBit(442931848, 27) == 308714120
    
    @test
    def clearBit09(): Bool = Int32.clearBit(-1, 0) == -2
    
    @test
    def clearBit10(): Bool = Int32.clearBit(-1, 1) == -3
    
    @test
    def clearBit11(): Bool = Int32.clearBit(-1, 5) == -33
    
    @test
    def clearBit12(): Bool = Int32.clearBit(-1, 31) == 2147483647
    
    @test
    def clearBit13(): Bool = Int32.clearBit(-1, 32) == -2
    
    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flipBit01(): Bool = Int32.flipBit(1, 0) == 0
    
    @test
    def flipBit02(): Bool = Int32.flipBit(1, 1) == 3
    
    @test
    def flipBit03(): Bool = Int32.flipBit(2, 1) == 0
    
    @test
    def flipBit04(): Bool = Int32.flipBit(2, 2) == 6
    
    @test
    def flipBit05(): Bool = Int32.flipBit(0, 31) == -2147483648
    
    @test
    def flipBit06(): Bool = Int32.flipBit(0, 30) == 1073741824
    
    @test
    def flipBit07(): Bool = Int32.flipBit(-2147483648, 31) == 0
    
    @test
    def flipBit08(): Bool = Int32.flipBit(-1, 0) == -2
    
    @test
    def flipBit9(): Bool = Int32.flipBit(-1, 1) == -3
    
    @test
    def flipBit10(): Bool = Int32.flipBit(-1, 5) == -33
    
    @test
    def flipBit11(): Bool = Int32.flipBit(-1, 31) == 2147483647
    
    @test
    def flipBit12(): Bool = Int32.flipBit(-1, 32) == -2

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def log201(): Bool = Int32.log2(10) == 3

    @test
    def log202(): Bool = Int32.log2(10000) == 13

    @test
    def log203(): Bool = Int32.log2(8) == 3

    @test
    def log204(): Bool = Int32.log2(256) == 8

    @test
    def log205(): Bool = Int32.log2(1) == 0

    @test
    def log206(): Bool = Int32.log2(12345678) == 23

    @test
    def log207(): Bool = Int32.log2(20) == 4

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def factorial01(): Bool = Int32.factorial(0) == 1

    @test
    def factorial02(): Bool = Int32.factorial(1) == 1

    @test
    def factorial03(): Bool = Int32.factorial(2) == 2

    @test
    def factorial04(): Bool = Int32.factorial(3) == 6

    @test
    def factorial05(): Bool = Int32.factorial(4) == 24

    @test
    def factorial06(): Bool = Int32.factorial(5) == 120

    @test
    def factorial07(): Bool = Int32.factorial(10) == 3628800

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Int32.toString(0) == "0"
    
    @test
    def toString02(): Bool = Int32.toString(1) == "1"
    
    @test
    def toString03(): Bool = Int32.toString(-1) == "-1"
    
    @test
    def toString04(): Bool = Int32.toString(2147483647) == "2147483647"
    
    @test
    def toString05(): Bool = Int32.toString(-2147483648) == "-2147483648"
    
    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Int32.fromString("Bad") |> Result.isErr
    
    @test
    def fromString02(): Bool = Int32.fromString(" 1") == Ok(1)
    
    @test
    def fromString03(): Bool = Int32.fromString("1 ") == Ok(1)
    
    @test
    def fromString04(): Bool = Int32.fromString("0") == Ok(0)
    
    @test
    def fromString05(): Bool = Int32.fromString("1") == Ok(1)
    
    @test
    def fromString06(): Bool = Int32.fromString("-1") == Ok(-1)
    
    @test
    def fromString07(): Bool = Int32.fromString("2147483647") == Ok(2147483647)
    
    @test
    def fromString08(): Bool = Int32.fromString("-2147483648") == Ok(-2147483648)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Int32.tryToInt8(0) == Some(0i8)

    @test
    def tryToInt802(): Bool = Int32.tryToInt8(1) == Some(1i8)

    @test
    def tryToInt803(): Bool = Int32.tryToInt8(-1) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Int32.tryToInt8(127) == Some(127i8)

    @test
    def tryToInt805(): Bool = Int32.tryToInt8(128) == None

    @test
    def tryToInt806(): Bool = Int32.tryToInt8(-128) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Int32.tryToInt8(-129) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = Int32.tryToInt16(0) == Some(0i16)

    @test
    def tryToInt1602(): Bool = Int32.tryToInt16(1) == Some(1i16)

    @test
    def tryToInt1603(): Bool = Int32.tryToInt16(-1) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = Int32.tryToInt16(32767) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = Int32.tryToInt16(32768) == None

    @test
    def tryToInt1606(): Bool = Int32.tryToInt16(-32768) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = Int32.tryToInt16(-32769) == None

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt6401(): Bool = Int32.toInt64(0) == 0i64

    @test
    def toInt6402(): Bool = Int32.toInt64(1) == 1i64

    @test
    def toInt6403(): Bool = Int32.toInt64(-1) == -1i64

    @test
    def toInt6404(): Bool = Int32.toInt64(2147483647) == 2147483647i64

    @test
    def toInt6405(): Bool = Int32.toInt64(-2147483648) == -2147483648i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = Int32.toBigInt(0) == 0ii

    @test
    def toBigInt02(): Bool = Int32.toBigInt(1) == 1ii

    @test
    def toBigInt03(): Bool = Int32.toBigInt(-1) == -1ii

    @test
    def toBigInt04(): Bool = Int32.toBigInt(2147483647) == 2147483647ii

    @test
    def toBigInt05(): Bool = Int32.toBigInt(-2147483648) == -2147483648ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat3201(): Bool = Int32.toFloat32(0) == 0.0f32

    @test
    def toFloat3202(): Bool = Int32.toFloat32(1) == 1.0f32

    @test
    def toFloat3203(): Bool = Int32.toFloat32(-1) == -1.0f32

    @test
    def toFloat3204(): Bool = Int32.toFloat32(2147483647) == 2147483647.0f32

    @test
    def toFloat3205(): Bool = Int32.toFloat32(-2147483648) == -2147483648.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Int32.toFloat64(0) == 0.0f64

    @test
    def toFloat6402(): Bool = Int32.toFloat64(1) == 1.0f64

    @test
    def toFloat6403(): Bool = Int32.toFloat64(-1) == -1.0f64

    @test
    def toFloat6404(): Bool = Int32.toFloat64(2147483647) == 2147483647.0f64

    @test
    def toFloat6405(): Bool = Int32.toFloat64(-2147483648) == -2147483648.0f64

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Int32.clampToInt8(0, -100i8, 100i8) == 0i8

    @test
    def clampToInt802(): Bool = Int32.clampToInt8(100, -100i8, 100i8) == 100i8

    @test
    def clampToInt803(): Bool = Int32.clampToInt8(101, -100i8, 100i8) == 100i8

    @test
    def clampToInt804(): Bool = Int32.clampToInt8(-100, -100i8, 100i8) == -100i8

    @test
    def clampToInt805(): Bool = Int32.clampToInt8(-101, -100i8, 100i8) == -100i8

    @test
    def clampToInt806(): Bool = Int32.clampToInt8(Int32.maxValue(), -100i8, 100i8) == 100i8

    @test
    def clampToInt807(): Bool = Int32.clampToInt8(Int32.minValue(), -100i8, 100i8) == -100i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = Int32.clampToInt16(0, -100i16, 100i16) == 0i16

    @test
    def clampToInt1602(): Bool = Int32.clampToInt16(100, -100i16, 100i16) == 100i16

    @test
    def clampToInt1603(): Bool = Int32.clampToInt16(101, -100i16, 100i16) == 100i16

    @test
    def clampToInt1604(): Bool = Int32.clampToInt16(-100, -100i16, 100i16) == -100i16

    @test
    def clampToInt1605(): Bool = Int32.clampToInt16(-101, -100i16, 100i16) == -100i16

    @test
    def clampToInt1606(): Bool = Int32.clampToInt16(Int32.maxValue(), -100i16, 100i16) == 100i16

    @test
    def clampToInt1607(): Bool = Int32.clampToInt16(Int32.minValue(), -100i16, 100i16) == -100i16

}
