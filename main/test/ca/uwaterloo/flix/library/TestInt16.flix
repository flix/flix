mod TestInt16 {

    use Assert.{assertEq, assertTrue, assertSome, assertFalse};
    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert = assertEq(expected = 0i16, mul(1i16, 0i16))

    @Test
    def mul02(): Unit \ Assert = assertEq(expected = 0i16, mul(0i16, 1i16))

    @Test
    def mul03(): Unit \ Assert = assertEq(expected = 0i16, mul(0i16, 0i16))

    @Test
    def mul04(): Unit \ Assert = assertEq(expected = -32768i16, mul(4i16, -8192i16))

    @Test
    def mul05(): Unit \ Assert = assertEq(expected = 32767i16, mul(7i16, 4681i16))

    @Test
    def mul06(): Unit \ Assert = assertEq(expected = 55i16, mul(5i16, 11i16))

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Unit \ Assert = assertEq(expected = Int16.bitwiseNot(Int16.leftShift(1i16, 15)), Int16.maxValue())

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Unit \ Assert = assertEq(expected = Int16.leftShift(1i16, 15), Int16.minValue())

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = assertEq(expected = 16, Int16.size())

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1i16, Int16.min(1i16, 2i16))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1i16, Int16.min(2i16, -1i16))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66i16, Int16.min(-33i16, -66i16))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49i16, Int16.max(48i16, 49i16))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4i16, Int16.max(4i16, -16i16))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16i16, Int16.max(-34i16, -16i16))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 31i16, Int16.abs(31i16))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 31i16, Int16.abs(-31i16))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 32767i16, Int16.abs(32767i16))

    @Test
    def abs04(): Unit \ Assert = assertEq(expected = 32767i16, Int16.abs(-32767i16))

    @Test
    def abs05(): Unit \ Assert = assertEq(expected = 0i16, Int16.abs(0i16))

    @Test
    def abs06(): Unit \ Assert = assertEq(expected = -1i16, Int16.abs(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def modulo01(): Unit \ Assert = assertEq(expected = 0i16, 12000i16 `Int16.modulo` 2000i16)

    @Test
    def modulo02(): Unit \ Assert = assertEq(expected = 2000i16, 12000i16 `Int16.modulo` 5000i16)

    @Test
    def modulo03(): Unit \ Assert = assertEq(expected = 3000i16, -12000i16 `Int16.modulo` 5000i16)

    @Test
    def modulo04(): Unit \ Assert = assertEq(expected = -3000i16, 12000i16 `Int16.modulo` -5000i16)

    @Test
    def modulo05(): Unit \ Assert = assertEq(expected = -2000i16, -12000i16 `Int16.modulo` -5000i16)

    @Test
    def modulo06(): Unit \ Assert = assertEq(expected = 0i16, 1i16 `Int16.modulo` 0i16)

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def remainder01(): Unit \ Assert = assertEq(expected = 0i16, 12000i16 `Int16.remainder` 2000i16)

    @Test
    def remainder02(): Unit \ Assert = assertEq(expected = 2000i16, 12000i16 `Int16.remainder` 5000i16)

    @Test
    def remainder03(): Unit \ Assert = assertEq(expected = -2000i16, -12000i16 `Int16.remainder` 5000i16)

    @Test
    def remainder04(): Unit \ Assert = assertEq(expected = 2000i16, 12000i16 `Int16.remainder` -5000i16)

    @Test
    def remainder05(): Unit \ Assert = assertEq(expected = -2000i16, -12000i16 `Int16.remainder` -5000i16)

    @Test
    def remainder06(): Unit \ Assert = assertEq(expected = 0i16, 1i16 `Int16.remainder` 0i16)

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 38i16, Int16.dist(31i16, -7i16))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 43i16, Int16.dist(-44i16, -1i16))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0i16, Int16.dist(-2i16, -2i16))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 32767i16, Int16.dist(-32768i16, -1i16))

    @Test
    def dist05(): Unit \ Assert = assertEq(expected = -1i16, Int16.dist(-32768i16, 0i16))

    @Test
    def dist06(): Unit \ Assert = assertEq(expected = 127i16, Int16.dist(-123i16, 4i16))

    @Test
    def dist07(): Unit \ Assert = assertEq(expected = -1i16, Int16.dist(-32763i16, 5i16))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Unit \ Assert = assertEq(expected = -1, Int16.compare(-1i16, 44i16))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, Int16.compare(-1i16, -44i16))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, Int16.compare(88i16, 88i16))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Unit \ Assert = assertEq(expected = -1, Int16.signum(-22i16))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, Int16.signum(22i16))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, Int16.signum(0i16))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1i16, Int16.pow(base = 0i16, 0i16))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0i16, Int16.pow(base = 0i16, 1i16))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1i16, Int16.pow(base = 1i16, 0i16))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1i16, Int16.pow(base = 1i16, 1i16))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25i16, Int16.pow(base = 5i16, 2i16))

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Unit \ Assert = assertEq(expected = 8i16, Int16.leftShift(8i16, 0))

    @Test
    def testBitwiseLeftShift02(): Unit \ Assert = assertEq(expected = 2048i16, Int16.leftShift(8i16, 8))

    @Test
    def testBitwiseLeftShift03(): Unit \ Assert = assertEq(expected = -32768i16, Int16.leftShift(8i16, 12))

    @Test
    def testBitwiseLeftShift04(): Unit \ Assert = assertEq(expected = 0i16, Int16.leftShift(8i16, 13))

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Unit \ Assert = assertEq(expected = 12000i16, Int16.rightShift(12000i16, 0))

    @Test
    def testBitwiseRightShift02(): Unit \ Assert = assertEq(expected = 3000i16, Int16.rightShift(12000i16, 2))

    @Test
    def testBitwiseRightShift03(): Unit \ Assert = assertEq(expected = 0i16, Int16.rightShift(12000i16, 15))

    @Test
    def testBitwiseRightShift04(): Unit \ Assert = assertEq(expected = -3000i16, Int16.rightShift(-12000i16, 2))


    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Unit \ Assert = assertEq(expected = Int16.rightShift(2233i16, 4), Int16.logicalRightShift(dist = 4, 2233i16))

    @Test
    def logicalRightShift02(): Unit \ Assert = assertEq(expected = Int16.rightShift(32767i16, 33), Int16.logicalRightShift(dist = 33, 32767i16))

    @Test
    def logicalRightShift03(): Unit \ Assert = assertEq(expected = Int16.rightShift(2233i16, -8), Int16.logicalRightShift(dist = -8, 2233i16))

    @Test
    def logicalRightShift04(): Unit \ Assert = assertEq(expected = Int16.rightShift(2233i16, 4), Int16.logicalRightShift(dist = 4, 2233i16))

    @Test
    def logicalRightShift05(): Unit \ Assert = assertEq(expected = 32767i16, Int16.logicalRightShift(dist = 1, -1i16))

    @Test
    def logicalRightShift06(): Unit \ Assert = assertEq(expected = 4093i16, Int16.logicalRightShift(dist = 4, -35i16))

    @Test
    def logicalRightShift07(): Unit \ Assert = assertEq(expected = 490i16, Int16.logicalRightShift(dist = -2777, -2777i16))

    @Test
    def logicalRightShift08(): Unit \ Assert = assertEq(expected = -233i16, Int16.logicalRightShift(dist = 64, -233i16))

    @Test
    def logicalRightShift09(): Unit \ Assert = assertEq(expected = 0i16, Int16.logicalRightShift(dist = 63, -233i16))

    @Test
    def logicalRightShift10(): Unit \ Assert = assertEq(expected = -27i16, Int16.logicalRightShift(dist = 0, -27i16))

    @Test
    def logicalRightShift11(): Unit \ Assert = assertEq(expected = 16383i16, Int16.logicalRightShift(dist = 1, 32767i16))

    @Test
    def logicalRightShift12(): Unit \ Assert = assertEq(expected = 0i16, Int16.logicalRightShift(dist = 17, 32767i16))

    @Test
    def logicalRightShift13(): Unit \ Assert = assertEq(expected = 0i16, Int16.logicalRightShift(dist = 16, 32767i16))

    @Test
    def logicalRightShift14(): Unit \ Assert = assertEq(expected = 0i16, Int16.logicalRightShift(dist = 17, -32768i16))

    @Test
    def logicalRightShift15(): Unit \ Assert = assertEq(expected = 0i16, Int16.logicalRightShift(dist = 16, -32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Unit \ Assert = assertEq(expected = 8, Int16.bitCount(19909i16))

    @Test
    def bitCount02(): Unit \ Assert = assertEq(expected = 16, Int16.bitCount(-1i16))

    @Test
    def bitCount03(): Unit \ Assert = assertEq(expected = 12, Int16.bitCount(-451i16))

    @Test
    def bitCount04(): Unit \ Assert = assertEq(expected = 0, Int16.bitCount(0i16))

    @Test
    def bitCount05(): Unit \ Assert = assertEq(expected = 15, Int16.bitCount(32767i16))

    @Test
    def bitCount06(): Unit \ Assert = assertEq(expected = 1, Int16.bitCount(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateRight(dist = 3, 0i16))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateRight(dist = 19, 0i16))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateRight(dist = -3, 0i16))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateRight(dist = -3, -1i16))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateRight(dist = 19, -1i16))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateRight(dist = -3, -1i16))

    @Test
    def rotateRight07(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateRight(dist = 3, 8410i16))

    @Test
    def rotateRight08(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateRight(dist = 19, 8410i16))

    @Test
    def rotateRight09(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateRight(dist = -13, 8410i16))

    @Test
    def rotateRight10(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateRight(dist = 10, -14949i16))

    @Test
    def rotateRight11(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateRight(dist = 26, -14949i16))

    @Test
    def rotateRight12(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateRight(dist = -6, -14949i16))

    @Test
    def rotateRight13(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateRight(dist = 6, -6415i16))

    @Test
    def rotateRight14(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateRight(dist = 22, -6415i16))

    @Test
    def rotateRight15(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateRight(dist = -10, -6415i16))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateLeft(dist = 3, 0i16))

    @Test
    def rotateLeft02(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateLeft(dist = 19, 0i16))

    @Test
    def rotateLeft03(): Unit \ Assert = assertEq(expected = 0i16, Int16.rotateLeft(dist = -3, 0i16))

    @Test
    def rotateLeft04(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateLeft(dist = 3, -1i16))

    @Test
    def rotateLeft05(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateLeft(dist = 19, -1i16))

    @Test
    def rotateLeft06(): Unit \ Assert = assertEq(expected = -1i16, Int16.rotateLeft(dist = -3, -1i16))

    @Test
    def rotateLeft07(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateLeft(dist = 13, 8410i16))

    @Test
    def rotateLeft08(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateLeft(dist = 29, 8410i16))

    @Test
    def rotateLeft09(): Unit \ Assert = assertEq(expected = 17435i16, Int16.rotateLeft(dist = -3, 8410i16))

    @Test
    def rotateLeft10(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateLeft(dist = 6, -14949i16))

    @Test
    def rotateLeft11(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateLeft(dist = 22, -14949i16))

    @Test
    def rotateLeft12(): Unit \ Assert = assertEq(expected = 26353i16, Int16.rotateLeft(dist = -10, -14949i16))

    @Test
    def rotateLeft13(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateLeft(dist = 10, -6415i16))

    @Test
    def rotateLeft14(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateLeft(dist = 26, -6415i16))

    @Test
    def rotateLeft15(): Unit \ Assert = assertEq(expected = -14437i16, Int16.rotateLeft(dist = -6, -6415i16))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = 15202i16, Int16.reverse(18140i16))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = 18140i16, Int16.reverse(15202i16))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = -14627i16, Int16.reverse(-17565i16))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = -17565i16, Int16.reverse(-14627i16))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = 4633i16, Int16.reverse(-26552i16))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = -26552i16, Int16.reverse(4633i16))

    @Test
    def reverse07(): Unit \ Assert = assertEq(expected = 8192i16, Int16.reverse(4i16))

    @Test
    def reverse08(): Unit \ Assert = assertEq(expected = 4i16, Int16.reverse(8192i16))

    @Test
    def reverse09(): Unit \ Assert = assertEq(expected = -1i16, Int16.reverse(-1i16))

    @Test
    def reverse10(): Unit \ Assert = assertEq(expected = 0i16, Int16.reverse(0i16))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Unit \ Assert = assertEq(expected = 15, Int16.highestOneBitPosition(-1i16))

    @Test
    def highestOneBitPosition02(): Unit \ Assert = assertEq(expected = 15, Int16.highestOneBitPosition(-32768i16))

    @Test
    def highestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int16.highestOneBitPosition(1i16))

    @Test
    def highestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int16.highestOneBitPosition(2i16))

    @Test
    def highestOneBitPosition05(): Unit \ Assert = assertEq(expected = 1, Int16.highestOneBitPosition(3i16))

    @Test
    def highestOneBitPosition06(): Unit \ Assert = assertEq(expected = 4, Int16.highestOneBitPosition(20i16))

    @Test
    def highestOneBitPosition07(): Unit \ Assert = assertEq(expected = 14, Int16.highestOneBitPosition(Int16.pow(base = 2i16, 14i16)))

    @Test
    def highestOneBitPosition08(): Unit \ Assert = assertEq(expected = 14, Int16.highestOneBitPosition(32767i16))

    @Test
    def highestOneBitPosition09(): Unit \ Assert = assertEq(expected = -1, Int16.highestOneBitPosition(0i16))

    @Test
    def highestOneBitPosition10(): Unit \ Assert = assertEq(expected = 11, Int16.highestOneBitPosition(Int16.pow(base = 2i16, 8i16) + Int16.pow(base = 2i16, 11i16)))

    @Test
    def highestOneBitPosition11(): Unit \ Assert = assertEq(expected = 7, Int16.highestOneBitPosition(Int16.pow(base = 2i16, 3i16) + Int16.pow(base = 2i16, 7i16)))

    @Test
    def highestOneBitPosition12(): Unit \ Assert = assertEq(expected = 15, Int16.highestOneBitPosition(-32767i16))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Unit \ Assert = assertEq(expected = Int16.leftShift(1i16, 15), Int16.highestOneBit(-1i16))

    @Test
    def highestOneBit02(): Unit \ Assert = assertEq(expected = Int16.leftShift(1i16, 15), Int16.highestOneBit(-32768i16))

    @Test
    def highestOneBit03(): Unit \ Assert = assertEq(expected = Int16.leftShift(1i16, 15), Int16.highestOneBit(-77i16))

    @Test
    def highestOneBit04(): Unit \ Assert = assertEq(expected = 2i16, Int16.highestOneBit(2i16))

    @Test
    def highestOneBit05(): Unit \ Assert = assertEq(expected = 2i16, Int16.highestOneBit(3i16))

    @Test
    def highestOneBit06(): Unit \ Assert = assertEq(expected = 16i16, Int16.highestOneBit(20i16))

    @Test
    def highestOneBit07(): Unit \ Assert = assertEq(expected = 8192i16, Int16.highestOneBit(Int16.pow(base = 2i16, 11i16) + Int16.pow(base = 2i16, 13i16)))

    @Test
    def highestOneBit08(): Unit \ Assert = assertEq(expected = 16384i16, Int16.highestOneBit(32767i16))

    @Test
    def highestOneBit09(): Unit \ Assert = assertEq(expected = 0i16, Int16.highestOneBit(0i16))

    @Test
    def highestOneBit10(): Unit \ Assert = assertEq(expected = 512i16, Int16.highestOneBit(Int16.pow(base = 2i16, 6i16) + Int16.pow(base = 2i16, 9i16)))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Unit \ Assert = assertEq(expected = 16, Int16.numberOfLeadingZeros(0i16))

    @Test
    def numberOfLeadingZeros02(): Unit \ Assert = assertEq(expected = 0, Int16.numberOfLeadingZeros(-32768i16))

    @Test
    def numberOfLeadingZeros03(): Unit \ Assert = assertEq(expected = 0, Int16.numberOfLeadingZeros(-1i16))

    @Test
    def numberOfLeadingZeros04(): Unit \ Assert = assertEq(expected = 1, Int16.numberOfLeadingZeros(32767i16))

    @Test
    def numberOfLeadingZeros05(): Unit \ Assert = assertEq(expected = 1, Int16.numberOfLeadingZeros(Int16.pow(base = 2i16, 14i16)))

    @Test
    def numberOfLeadingZeros06(): Unit \ Assert = assertEq(expected = 4, Int16.numberOfLeadingZeros(Int16.pow(base = 2i16, 6i16) + Int16.pow(base = 2i16, 11i16)))

    @Test
    def numberOfLeadingZeros07(): Unit \ Assert = assertEq(expected = 8, Int16.numberOfLeadingZeros(Int16.pow(base = 2i16, 7i16) + Int16.pow(base = 2i16, 6i16)))

    @Test
    def numberOfLeadingZeros08(): Unit \ Assert = assertEq(expected = 11, Int16.numberOfLeadingZeros(20i16))

    @Test
    def numberOfLeadingZeros09(): Unit \ Assert = assertEq(expected = 15, Int16.numberOfLeadingZeros(1i16))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Unit \ Assert = assertEq(expected = 0, Int16.lowestOneBitPosition(-1i16))

    @Test
    def lowestOneBitPosition02(): Unit \ Assert = assertEq(expected = 15, Int16.lowestOneBitPosition(-32768i16))

    @Test
    def lowestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int16.lowestOneBitPosition(1i16))

    @Test
    def lowestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int16.lowestOneBitPosition(2i16))

    @Test
    def lowestOneBitPosition05(): Unit \ Assert = assertEq(expected = 0, Int16.lowestOneBitPosition(3i16))

    @Test
    def lowestOneBitPosition06(): Unit \ Assert = assertEq(expected = 2, Int16.lowestOneBitPosition(4i16))

    @Test
    def lowestOneBitPosition07(): Unit \ Assert = assertEq(expected = 2, Int16.lowestOneBitPosition(12i16))

    @Test
    def lowestOneBitPosition08(): Unit \ Assert = assertEq(expected = 11, Int16.lowestOneBitPosition(Int16.pow(base = 2i16, 11i16)))

    @Test
    def lowestOneBitPosition09(): Unit \ Assert = assertEq(expected = 6, Int16.lowestOneBitPosition(Int16.pow(base = 2i16, 11i16) + Int16.pow(base = 2i16, 6i16)))

    @Test
    def lowestOneBitPosition10(): Unit \ Assert = assertEq(expected = 0, Int16.lowestOneBitPosition(32767i16))

    @Test
    def lowestOneBitPosition11(): Unit \ Assert = assertEq(expected = -1, Int16.lowestOneBitPosition(0i16))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Unit \ Assert = assertEq(expected = 1i16, Int16.lowestOneBit(-1i16))

    @Test
    def lowestOneBit02(): Unit \ Assert = assertEq(expected = Int16.leftShift(1i16, 15), Int16.lowestOneBit(-32768i16))

    @Test
    def lowestOneBit03(): Unit \ Assert = assertEq(expected = 1i16, Int16.lowestOneBit(1i16))

    @Test
    def lowestOneBit04(): Unit \ Assert = assertEq(expected = 2i16, Int16.lowestOneBit(2i16))

    @Test
    def lowestOneBit05(): Unit \ Assert = assertEq(expected = 2i16, Int16.lowestOneBit(6i16))

    @Test
    def lowestOneBit06(): Unit \ Assert = assertEq(expected = 4i16, Int16.lowestOneBit(12i16))

    @Test
    def lowestOneBit07(): Unit \ Assert = assertEq(expected = 64i16, Int16.lowestOneBit(Int16.pow(base = 2i16, 6i16) + Int16.pow(base = 2i16, 9i16) + Int16.pow(base = 2i16, 14i16)))

    @Test
    def lowestOneBit08(): Unit \ Assert = assertEq(expected = 8192i16, Int16.lowestOneBit(Int16.pow(base = 2i16, 13i16)))

    @Test
    def lowestOneBit09(): Unit \ Assert = assertEq(expected = 8i16, Int16.lowestOneBit(Int16.pow(base = 2i16, 3i16) + Int16.pow(base = 2i16, 5i16)))

    @Test
    def lowestOneBit10(): Unit \ Assert = assertEq(expected = 1i16, Int16.lowestOneBit(32767i16))

    @Test
    def lowestOneBit11(): Unit \ Assert = assertEq(expected = 0i16, Int16.lowestOneBit(0i16))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Unit \ Assert = assertEq(expected = 16, Int16.numberOfTrailingZeros(0i16))

    @Test
    def numberOfTrailingZeros02(): Unit \ Assert = assertEq(expected = 0, Int16.numberOfTrailingZeros(3i16))

    @Test
    def numberOfTrailingZeros03(): Unit \ Assert = assertEq(expected = 1, Int16.numberOfTrailingZeros(42i16))

    @Test
    def numberOfTrailingZeros04(): Unit \ Assert = assertEq(expected = 10, Int16.numberOfTrailingZeros(Int16.pow(base = 2i16, 10i16) + Int16.pow(base = 2i16, 14i16)))

    @Test
    def numberOfTrailingZeros05(): Unit \ Assert = assertEq(expected = 8, Int16.numberOfTrailingZeros(Int16.pow(base = 2i16, 8i16)))

    @Test
    def numberOfTrailingZeros06(): Unit \ Assert = assertEq(expected = 0, Int16.numberOfTrailingZeros(-1i16))

    @Test
    def numberOfTrailingZeros07(): Unit \ Assert = assertEq(expected = 15, Int16.numberOfTrailingZeros(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Unit \ Assert = assertEq(expected = 1, Int16.getBit(pos = 5, -1i16))

    @Test
    def getBit02(): Unit \ Assert = assertEq(expected = 1, Int16.getBit(pos = 15, -10000i16))

    @Test
    def getBit03(): Unit \ Assert = assertEq(expected = 1, Int16.getBit(pos = 12, Int16.pow(base = 2i16, 12i16)))

    @Test
    def getBit04(): Unit \ Assert = assertEq(expected = 0, Int16.getBit(pos = 13, Int16.pow(base = 2i16, 12i16)))

    @Test
    def getBit05(): Unit \ Assert = assertEq(expected = 0, Int16.getBit(pos = 11, Int16.pow(base = 2i16, 12i16)))

    @Test
    def getBit06(): Unit \ Assert = assertEq(expected = 0, Int16.getBit(pos = 9, -1i16 - Int16.pow(base = 2i16, 9i16)))

    @Test
    def getBit07(): Unit \ Assert = assertEq(expected = 1, Int16.getBit(pos = 10, -1i16 - Int16.pow(base = 2i16, 9i16)))

    @Test
    def getBit08(): Unit \ Assert = assertEq(expected = 0, Int16.getBit(pos = 15, 0i16))

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Unit \ Assert = assertEq(expected = 1i16, Int16.setBit(pos = 0, 0i16))

    @Test
    def setBit02(): Unit \ Assert = assertEq(expected = 2i16, Int16.setBit(pos = 1, 0i16))

    @Test
    def setBit03(): Unit \ Assert = assertEq(expected = -32768i16, Int16.setBit(pos = 15, 0i16))

    @Test
    def setBit04(): Unit \ Assert = assertEq(expected = 0i16, Int16.setBit(pos = 16, 0i16))

    @Test
    def setBit05(): Unit \ Assert = assertEq(expected = -1418i16, Int16.setBit(pos = 31, -1418i16))

    @Test
    def setBit07(): Unit \ Assert = assertEq(expected = -1i16, Int16.setBit(pos = 4, -17i16))

    @Test
    def setBit08(): Unit \ Assert = assertEq(expected = -1i16, Int16.setBit(pos = 10, -1025i16))

    @Test
    def setBit09(): Unit \ Assert = assertEq(expected = -17i16, Int16.setBit(pos = 63, -17i16))

    @Test
    def setBit10(): Unit \ Assert = assertEq(expected = -17i16, Int16.setBit(pos = 30, -17i16))

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Unit \ Assert = assertEq(expected = 0i16, Int16.clearBit(pos = 0, 1i16))

    @Test
    def clearBit02(): Unit \ Assert = assertEq(expected = 1i16, Int16.clearBit(pos = 1, 1i16))

    @Test
    def clearBit03(): Unit \ Assert = assertEq(expected = 0i16, Int16.clearBit(pos = 1, 2i16))

    @Test
    def clearBit04(): Unit \ Assert = assertEq(expected = 2i16, Int16.clearBit(pos = 2, 2i16))

    @Test
    def clearBit05(): Unit \ Assert = assertEq(expected = 0i16, Int16.clearBit(pos = 31, 0i16))

    @Test
    def clearBit06(): Unit \ Assert = assertEq(expected = 0i16, Int16.clearBit(pos = 30, 0i16))

    @Test
    def clearBit07(): Unit \ Assert = assertEq(expected = 0i16, Int16.clearBit(pos = 15, Int16.leftShift(1i16, 15)))

    @Test
    def clearBit08(): Unit \ Assert = assertEq(expected = 25909i16, Int16.clearBit(pos = 11, 27957i16))

    @Test
    def clearBit09(): Unit \ Assert = assertEq(expected = -2i16, Int16.clearBit(pos = 0, -1i16))

    @Test
    def clearBit10(): Unit \ Assert = assertEq(expected = -3i16, Int16.clearBit(pos = 1, -1i16))

    @Test
    def clearBit11(): Unit \ Assert = assertEq(expected = -33i16, Int16.clearBit(pos = 5, -1i16))

    @Test
    def clearBit12(): Unit \ Assert = assertEq(expected = Int16.bitwiseNot(Int16.leftShift(1i16, 15)), Int16.clearBit(pos = 15, -1i16))

    @Test
    def clearBit13(): Unit \ Assert = assertEq(expected = -2i16, Int16.clearBit(pos = 32, -1i16))

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Unit \ Assert = assertEq(expected = 0i16, Int16.flipBit(pos = 0, 1i16))

    @Test
    def flipBit02(): Unit \ Assert = assertEq(expected = 3i16, Int16.flipBit(pos = 1, 1i16))

    @Test
    def flipBit03(): Unit \ Assert = assertEq(expected = 0i16, Int16.flipBit(pos = 1, 2i16))

    @Test
    def flipBit04(): Unit \ Assert = assertEq(expected = 6i16, Int16.flipBit(pos = 2, 2i16))

    @Test
    def flipBit05(): Unit \ Assert = assertEq(expected = -32768i16, Int16.flipBit(pos = 15, 0i16))

    @Test
    def flipBit06(): Unit \ Assert = assertEq(expected = 0i16, Int16.flipBit(pos = 16, 0i16))

    @Test
    def flipBit07(): Unit \ Assert = assertEq(expected = 0i16, Int16.flipBit(pos = 15, Int16.leftShift(1i16, 15)))

    @Test
    def flipBit08(): Unit \ Assert = assertEq(expected = -2i16, Int16.flipBit(pos = 0, -1i16))

    @Test
    def flipBit9(): Unit \ Assert = assertEq(expected = -3i16, Int16.flipBit(pos = 1, -1i16))

    @Test
    def flipBit10(): Unit \ Assert = assertEq(expected = -33i16, Int16.flipBit(pos = 5, -1i16))

    @Test
    def flipBit11(): Unit \ Assert = assertEq(expected = (-1i16-Int16.leftShift(1i16, 13)), Int16.flipBit(pos = 13, -1i16))

    @Test
    def flipBit12(): Unit \ Assert = assertEq(expected = -2i16, Int16.flipBit(pos = 32, -1i16))

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Unit \ Assert = assertEq(expected = 3i16, Int16.log2(10i16))

    @Test
    def log202(): Unit \ Assert = assertEq(expected = 13i16, Int16.log2(10000i16))

    @Test
    def log203(): Unit \ Assert = assertEq(expected = 3i16, Int16.log2(8i16))

    @Test
    def log204(): Unit \ Assert = assertEq(expected = 8i16, Int16.log2(256i16))

    @Test
    def log205(): Unit \ Assert = assertEq(expected = 0i16, Int16.log2(1i16))

    @Test
    def log206(): Unit \ Assert = assertEq(expected = 14i16, Int16.log2(32767i16))

    @Test
    def log207(): Unit \ Assert = assertEq(expected = 4i16, Int16.log2(20i16))

    @Test
    def log208(): Unit \ Assert = assertEq(expected = 0i16, Int16.log2(0i16))

    @Test
    def log209(): Unit \ Assert = assertEq(expected = 0i16, Int16.log2(-1i16))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseAnd(0i16, 0i16))

    @Test
    def bitwiseAnd02(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseAnd(127i16, 0i16))

    @Test
    def bitwiseAnd03(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseAnd(-128i16, 0i16))

    @Test
    def bitwiseAnd04(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseAnd(0i16, 1i16))

    @Test
    def bitwiseAnd05(): Unit \ Assert = assertEq(expected = 1i16, Int16.bitwiseAnd(127i16, 1i16))

    @Test
    def bitwiseAnd06(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseAnd(-128i16, 1i16))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Unit \ Assert = assertEq(expected = -1i16, Int16.bitwiseNot(0i16))

    @Test
    def bitwiseNot02(): Unit \ Assert = assertEq(expected = -2i16, Int16.bitwiseNot(1i16))

    @Test
    def bitwiseNot03(): Unit \ Assert = assertEq(expected = -0i16, Int16.bitwiseNot(-1i16))

    @Test
    def bitwiseNot04(): Unit \ Assert = assertEq(expected = -128i16, Int16.bitwiseNot(127i16))

    @Test
    def bitwiseNot05(): Unit \ Assert = assertEq(expected = 127i16, Int16.bitwiseNot(-128i16))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseOr(0i16, 0i16))

    @Test
    def bitwiseOr02(): Unit \ Assert = assertEq(expected = 127i16, Int16.bitwiseOr(127i16, 0i16))

    @Test
    def bitwiseOr03(): Unit \ Assert = assertEq(expected = -128i16, Int16.bitwiseOr(-128i16, 0i16))

    @Test
    def bitwiseOr04(): Unit \ Assert = assertEq(expected = 1i16, Int16.bitwiseOr(0i16, 1i16))

    @Test
    def bitwiseOr05(): Unit \ Assert = assertEq(expected = 127i16, Int16.bitwiseOr(127i16, 1i16))

    @Test
    def bitwiseOr06(): Unit \ Assert = assertEq(expected = -127i16, Int16.bitwiseOr(-128i16, 1i16))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Unit \ Assert = assertEq(expected = 0i16, Int16.bitwiseXor(0i16, 0i16))

    @Test
    def bitwiseXor02(): Unit \ Assert = assertEq(expected = 127i16, Int16.bitwiseXor(127i16, 0i16))

    @Test
    def bitwiseXor03(): Unit \ Assert = assertEq(expected = -128i16, Int16.bitwiseXor(-128i16, 0i16))

    @Test
    def bitwiseXor04(): Unit \ Assert = assertEq(expected = 1i16, Int16.bitwiseXor(0i16, 1i16))

    @Test
    def bitwiseXor05(): Unit \ Assert = assertEq(expected = 126i16, Int16.bitwiseXor(127i16, 1i16))

    @Test
    def bitwiseXor06(): Unit \ Assert = assertEq(expected = -127i16, Int16.bitwiseXor(-128i16, 1i16))

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Unit \ Assert = assertEq(expected = 1, Int16.factorial(0i16))

    @Test
    def factorial02(): Unit \ Assert = assertEq(expected = 1, Int16.factorial(1i16))

    @Test
    def factorial03(): Unit \ Assert = assertEq(expected = 2, Int16.factorial(2i16))

    @Test
    def factorial04(): Unit \ Assert = assertEq(expected = 6, Int16.factorial(3i16))

    @Test
    def factorial05(): Unit \ Assert = assertEq(expected = 24, Int16.factorial(4i16))

    @Test
    def factorial06(): Unit \ Assert = assertEq(expected = 120, Int16.factorial(5i16))

    @Test
    def factorial07(): Unit \ Assert = assertEq(expected = 3628800, Int16.factorial(10i16))

    @Test
    def factorial08(): Unit \ Assert = assertEq(expected = 0, Int16.factorial(-1i16))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0", Int16.toString(0i16))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1", Int16.toString(1i16))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1", Int16.toString(-1i16))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "32767", Int16.toString(32767i16))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-32768", Int16.toString(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertTrue(Int16.fromString("Bad") |> Option.isEmpty)

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1i16), Int16.fromString(" 1"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1i16), Int16.fromString("1 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0i16), Int16.fromString("0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1i16), Int16.fromString("1"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1i16), Int16.fromString("-1"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(32767i16), Int16.fromString("32767"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-32768i16), Int16.fromString("-32768"))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), Int16.tryToInt8(0i16))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), Int16.tryToInt8(1i16))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), Int16.tryToInt8(-1i16))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), Int16.tryToInt8(127i16))

    @Test
    def tryToInt805(): Unit \ Assert = assertEq(expected = None, Int16.tryToInt8(128i16))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), Int16.tryToInt8(-128i16))

    @Test
    def tryToInt807(): Unit \ Assert = assertEq(expected = None, Int16.tryToInt8(-129i16))

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt3201(): Unit \ Assert = assertEq(expected = 0, Int16.toInt32(0i16))

    @Test
    def toInt3202(): Unit \ Assert = assertEq(expected = 1, Int16.toInt32(1i16))

    @Test
    def toInt3203(): Unit \ Assert = assertEq(expected = -1, Int16.toInt32(-1i16))

    @Test
    def toInt3204(): Unit \ Assert = assertEq(expected = 32767, Int16.toInt32(32767i16))

    @Test
    def toInt3205(): Unit \ Assert = assertEq(expected = -32768, Int16.toInt32(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt6401(): Unit \ Assert = assertEq(expected = 0i64, Int16.toInt64(0i16))

    @Test
    def toInt6402(): Unit \ Assert = assertEq(expected = 1i64, Int16.toInt64(1i16))

    @Test
    def toInt6403(): Unit \ Assert = assertEq(expected = -1i64, Int16.toInt64(-1i16))

    @Test
    def toInt6404(): Unit \ Assert = assertEq(expected = 32767i64, Int16.toInt64(32767i16))

    @Test
    def toInt6405(): Unit \ Assert = assertEq(expected = -32768i64, Int16.toInt64(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Unit \ Assert = assertEq(expected = 0ii, Int16.toBigInt(0i16))

    @Test
    def toBigInt02(): Unit \ Assert = assertEq(expected = 1ii, Int16.toBigInt(1i16))

    @Test
    def toBigInt03(): Unit \ Assert = assertEq(expected = -1ii, Int16.toBigInt(-1i16))

    @Test
    def toBigInt04(): Unit \ Assert = assertEq(expected = 32767ii, Int16.toBigInt(32767i16))

    @Test
    def toBigInt05(): Unit \ Assert = assertEq(expected = -32768ii, Int16.toBigInt(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Unit \ Assert = assertEq(expected = 0.0f32, Int16.toFloat32(0i16))

    @Test
    def toFloat3202(): Unit \ Assert = assertEq(expected = 1.0f32, Int16.toFloat32(1i16))

    @Test
    def toFloat3203(): Unit \ Assert = assertEq(expected = -1.0f32, Int16.toFloat32(-1i16))

    @Test
    def toFloat3204(): Unit \ Assert = assertEq(expected = 32767.0f32, Int16.toFloat32(32767i16))

    @Test
    def toFloat3205(): Unit \ Assert = assertEq(expected = -32768.0f32, Int16.toFloat32(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, Int16.toFloat64(0i16))

    @Test
    def toFloat6402(): Unit \ Assert = assertEq(expected = 1.0f64, Int16.toFloat64(1i16))

    @Test
    def toFloat6403(): Unit \ Assert = assertEq(expected = -1.0f64, Int16.toFloat64(-1i16))

    @Test
    def toFloat6404(): Unit \ Assert = assertEq(expected = 32767.0f64, Int16.toFloat64(32767i16))

    @Test
    def toFloat6405(): Unit \ Assert = assertEq(expected = -32768.0f64, Int16.toFloat64(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Unit \ Assert = assertEq(expected = 0.0ff, Int16.toBigDecimal(0i16))

    @Test
    def toBigDecimal02(): Unit \ Assert = assertEq(expected = 1.0ff, Int16.toBigDecimal(1i16))

    @Test
    def toBigDecimal03(): Unit \ Assert = assertEq(expected = -1.0ff, Int16.toBigDecimal(-1i16))

    @Test
    def toBigDecimal04(): Unit \ Assert = assertEq(expected = 32767.0ff, Int16.toBigDecimal(32767i16))

    @Test
    def toBigDecimal05(): Unit \ Assert = assertEq(expected = -32768.0ff, Int16.toBigDecimal(-32768i16))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Unit \ Assert = assertEq(expected = 0i8, Int16.clampToInt8(min = -100i8, max = 100i8, 0i16))

    @Test
    def clampToInt802(): Unit \ Assert = assertEq(expected = 100i8, Int16.clampToInt8(min = -100i8, max = 100i8, 100i16))

    @Test
    def clampToInt803(): Unit \ Assert = assertEq(expected = 100i8, Int16.clampToInt8(min = -100i8, max = 100i8, 101i16))

    @Test
    def clampToInt804(): Unit \ Assert = assertEq(expected = -100i8, Int16.clampToInt8(min = -100i8, max = 100i8, -100i16))

    @Test
    def clampToInt805(): Unit \ Assert = assertEq(expected = -100i8, Int16.clampToInt8(min = -100i8, max = 100i8, -101i16))

    @Test
    def clampToInt806(): Unit \ Assert = assertEq(expected = 100i8, Int16.clampToInt8(min = -100i8, max = 100i8, Int16.maxValue()))

    @Test
    def clampToInt807(): Unit \ Assert = assertEq(expected = -100i8, Int16.clampToInt8(min = -100i8, max = 100i8, Int16.minValue()))

    @Test
    def clampToInt808(): Unit \ Assert = assertEq(expected = 10i8, Int16.clampToInt8(min = 10i8, max = 0i8, 5i16))      // Bad range

    /////////////////////////////////////////////////////////////////////////////
    // shortValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shortValue01(): Unit \ Assert =
        let i = Int16.valueOf(0i16);
        assertEq(expected = 0i16, Int16.shortValue(i))

    @Test
    def shortValue02(): Unit \ Assert =
        let i = Int16.valueOf(1i16);
        assertEq(expected = 1i16, Int16.shortValue(i))

    @Test
    def shortValue03(): Unit \ Assert =
        let i = Int16.valueOf(-1i16);
        assertEq(expected = -1i16, Int16.shortValue(i))

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Unit \ Assert =
        let i = Int16.valueOf(0i16);
        assertTrue(i.equals(Int16.valueOf(0i16)))

    @Test
    def valueOf02(): Unit \ Assert =
        let i = Int16.valueOf(1i16);
        assertTrue(i.equals(Int16.valueOf(1i16)))

    @Test
    def valueOf03(): Unit \ Assert =
        let i = Int16.valueOf(-1i16);
        assertTrue(i.equals(Int16.valueOf(-1i16)))

}
