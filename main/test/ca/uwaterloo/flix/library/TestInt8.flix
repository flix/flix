mod TestInt8 {

    use Assert.{assertEq, assertTrue, assertSome, fail};

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert = assertEq(expected = 0i8, 1i8 * 0i8)

    @Test
    def mul02(): Unit \ Assert = assertEq(expected = 0i8, 0i8 * 1i8)

    @Test
    def mul03(): Unit \ Assert = assertEq(expected = 0i8, 0i8 * 0i8)

    @Test
    def mul04(): Unit \ Assert = assertEq(expected = -128i8, -64i8 * 2i8)

    @Test
    def mul05(): Unit \ Assert = assertEq(expected = 127i8, 127i8 * 1i8)

    @Test
    def mul06(): Unit \ Assert = assertEq(expected = 55i8, 5i8 * 11i8)

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Unit \ Assert = assertEq(expected = Int8.bitwiseNot(Int8.leftShift(1i8, 7)), Int8.maxValue())

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Unit \ Assert = assertEq(expected = Int8.leftShift(1i8, 7), Int8.minValue())

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = assertEq(expected = 8, Int8.size())

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1i8, Int8.min(1i8, 2i8))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1i8, Int8.min(2i8, -1i8))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66i8, Int8.min(-33i8, -66i8))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49i8, Int8.max(48i8, 49i8))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4i8, Int8.max(4i8, -16i8))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16i8, Int8.max(-34i8, -16i8))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 31i8, Int8.abs(31i8))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 31i8, Int8.abs(-31i8))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 127i8, Int8.abs(127i8))

    @Test
    def abs04(): Unit \ Assert = assertEq(expected = 127i8, Int8.abs(-127i8))

    @Test
    def abs05(): Unit \ Assert = assertEq(expected = 0i8, Int8.abs(0i8))

    @Test
    def abs06(): Unit \ Assert = assertEq(expected = -1i8, Int8.abs(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def modulo01(): Unit \ Assert = assertEq(expected = 0i8, 12i8 `Int8.modulo` 2i8)

    @Test
    def modulo02(): Unit \ Assert = assertEq(expected = 2i8, 12i8 `Int8.modulo` 5i8)

    @Test
    def modulo03(): Unit \ Assert = assertEq(expected = 3i8, -12i8 `Int8.modulo` 5i8)

    @Test
    def modulo04(): Unit \ Assert = assertEq(expected = -3i8, 12i8 `Int8.modulo` -5i8)

    @Test
    def modulo05(): Unit \ Assert = assertEq(expected = -2i8, -12i8 `Int8.modulo` -5i8)

    @Test
    def modulo06(): Unit \ Assert = assertEq(expected = 0i8, 1i8 `Int8.modulo` 0i8)

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def remainder01(): Unit \ Assert = assertEq(expected = 0i8, 12i8 `Int8.remainder` 2i8)

    @Test
    def remainder02(): Unit \ Assert = assertEq(expected = 2i8, 12i8 `Int8.remainder` 5i8)

    @Test
    def remainder03(): Unit \ Assert = assertEq(expected = -2i8, -12i8 `Int8.remainder` 5i8)

    @Test
    def remainder04(): Unit \ Assert = assertEq(expected = 2i8, 12i8 `Int8.remainder` -5i8)

    @Test
    def remainder05(): Unit \ Assert = assertEq(expected = -2i8, -12i8 `Int8.remainder` -5i8)

    @Test
    def remainder06(): Unit \ Assert = assertEq(expected = 0i8, 1i8 `Int8.remainder` 0i8)

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 38i8, Int8.dist(31i8, -7i8))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 43i8, Int8.dist(-44i8, -1i8))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0i8, Int8.dist(-2i8, -2i8))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 127i8, Int8.dist(-128i8, -1i8))

    @Test
    def dist05(): Unit \ Assert = assertEq(expected = -1i8, Int8.dist(-128i8, 0i8))

    @Test
    def dist06(): Unit \ Assert = assertEq(expected = 127i8, Int8.dist(-123i8, 4i8))

    @Test
    def dist07(): Unit \ Assert = assertEq(expected = -1i8, Int8.dist(-123i8, 5i8))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Unit \ Assert = assertEq(expected = -1, Int8.compare(-1i8, 44i8))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, Int8.compare(-1i8, -44i8))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, Int8.compare(88i8, 88i8))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Unit \ Assert = assertEq(expected = -1, Int8.signum(-22i8))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, Int8.signum(22i8))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, Int8.signum(0i8))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1i8, Int8.pow(base = 0i8, 0i8))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0i8, Int8.pow(base = 0i8, 1i8))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1i8, Int8.pow(base = 1i8, 0i8))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1i8, Int8.pow(base = 1i8, 1i8))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25i8, Int8.pow(base = 5i8, 2i8))

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Unit \ Assert = assertEq(expected = 8i8, Int8.leftShift(8i8, 0))

    @Test
    def testBitwiseLeftShift02(): Unit \ Assert = assertEq(expected = 32i8, Int8.leftShift(8i8, 2))

    @Test
    def testBitwiseLeftShift03(): Unit \ Assert = assertEq(expected = -128i8, Int8.leftShift(8i8, 4))

    @Test
    def testBitwiseLeftShift04(): Unit \ Assert = assertEq(expected = 0i8, Int8.leftShift(8i8, 5))

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Unit \ Assert = assertEq(expected = 120i8, Int8.rightShift(120i8, 0))

    @Test
    def testBitwiseRightShift02(): Unit \ Assert = assertEq(expected = 30i8, Int8.rightShift(120i8, 2))

    @Test
    def testBitwiseRightShift03(): Unit \ Assert = assertEq(expected = 0i8, Int8.rightShift(120i8, 7))

    @Test
    def testBitwiseRightShift04(): Unit \ Assert = assertEq(expected = -30i8, Int8.rightShift(-120i8, 2))

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Unit \ Assert = assertEq(expected = Int8.rightShift(98i8, 4), Int8.logicalRightShift(dist = 4, 98i8))

    @Test
    def logicalRightShift02(): Unit \ Assert = assertEq(expected = Int8.rightShift(127i8, 33), Int8.logicalRightShift(dist = 33, 127i8))

    @Test
    def logicalRightShift03(): Unit \ Assert = assertEq(expected = Int8.rightShift(93i8, -8), Int8.logicalRightShift(dist = -8, 93i8))

    @Test
    def logicalRightShift04(): Unit \ Assert = assertEq(expected = Int8.rightShift(93i8, 4), Int8.logicalRightShift(dist = 4, 93i8))

    @Test
    def logicalRightShift05(): Unit \ Assert = assertEq(expected = 127i8, Int8.logicalRightShift(dist = 1, -1i8))

    @Test
    def logicalRightShift06(): Unit \ Assert = assertEq(expected = 13i8, Int8.logicalRightShift(dist = 4, -35i8))

    @Test
    def logicalRightShift07(): Unit \ Assert = assertEq(expected = 13i8, Int8.logicalRightShift(dist = -2780, -44i8))

    @Test
    def logicalRightShift08(): Unit \ Assert = assertEq(expected = -98i8, Int8.logicalRightShift(dist = 64, -98i8))

    @Test
    def logicalRightShift09(): Unit \ Assert = assertEq(expected = -27i8, Int8.logicalRightShift(dist = 0, -27i8))

    @Test
    def logicalRightShift10(): Unit \ Assert = assertEq(expected = 63i8, Int8.logicalRightShift(dist = 1, 127i8))

    @Test
    def logicalRightShift11(): Unit \ Assert = assertEq(expected = 0i8, Int8.logicalRightShift(dist = 9, 127i8))

    @Test
    def logicalRightShift12(): Unit \ Assert = assertEq(expected = 0i8, Int8.logicalRightShift(dist = 8, 127i8))

    @Test
    def logicalRightShift13(): Unit \ Assert = assertEq(expected = 0i8, Int8.logicalRightShift(dist = 9, -128i8))

    @Test
    def logicalRightShift14(): Unit \ Assert = assertEq(expected = 0i8, Int8.logicalRightShift(dist = 9, -128i8))

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Unit \ Assert = assertEq(expected = 3, Int8.bitCount(70i8))

    @Test
    def bitCount02(): Unit \ Assert = assertEq(expected = 8, Int8.bitCount(-1i8))

    @Test
    def bitCount03(): Unit \ Assert = assertEq(expected = 5, Int8.bitCount(-99i8))

    @Test
    def bitCount04(): Unit \ Assert = assertEq(expected = 0, Int8.bitCount(0i8))

    @Test
    def bitCount05(): Unit \ Assert = assertEq(expected = 7, Int8.bitCount(127i8))

    @Test
    def bitCount06(): Unit \ Assert = assertEq(expected = 1, Int8.bitCount(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateRight(dist = 3, 0i8))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateRight(dist = 19, 0i8))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateRight(dist = -3, 0i8))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateRight(dist = 3, -1i8))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateRight(dist = 19, -1i8))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateRight(dist = -3, -1i8))

    @Test
    def rotateRight07(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateRight(dist = 3, 106i8))

    @Test
    def rotateRight08(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateRight(dist = 11, 106i8))

    @Test
    def rotateRight09(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateRight(dist = -5, 106i8))

    @Test
    def rotateRight10(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateRight(dist = 7, -75i8))

    @Test
    def rotateRight11(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateRight(dist = 23, -75i8))

    @Test
    def rotateRight12(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateRight(dist = -1, -75i8))

    @Test
    def rotateRight13(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateRight(dist = 6, -17i8))

    @Test
    def rotateRight14(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateRight(dist = 22, -17i8))

    @Test
    def rotateRight15(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateRight(dist = -10, -17i8))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateLeft(dist = 5, 0i8))

    @Test
    def rotateLeft02(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateLeft(dist = 13, 0i8))

    @Test
    def rotateLeft03(): Unit \ Assert = assertEq(expected = 0i8, Int8.rotateLeft(dist = -3, 0i8))

    @Test
    def rotateLeft04(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateLeft(dist = 3, -1i8))

    @Test
    def rotateLeft05(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateLeft(dist = 19, -1i8))

    @Test
    def rotateLeft06(): Unit \ Assert = assertEq(expected = -1i8, Int8.rotateLeft(dist = -3, -1i8))

    @Test
    def rotateLeft07(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateLeft(dist = 5, 106i8))

    @Test
    def rotateLeft08(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateLeft(dist = 13, 106i8))

    @Test
    def rotateLeft09(): Unit \ Assert = assertEq(expected = 77i8, Int8.rotateLeft(dist = -3, 106i8))

    @Test
    def rotateLeft10(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateLeft(dist = 1, -75i8))

    @Test
    def rotateLeft11(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateLeft(dist = 9, -75i8))

    @Test
    def rotateLeft12(): Unit \ Assert = assertEq(expected = 107i8, Int8.rotateLeft(dist = -7, -75i8))

    @Test
    def rotateLeft13(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateLeft(dist = 2, -17i8))

    @Test
    def rotateLeft14(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateLeft(dist = 10, -17i8))

    @Test
    def rotateLeft15(): Unit \ Assert = assertEq(expected = -65i8, Int8.rotateLeft(dist = -6, -17i8))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = 92i8, Int8.reverse(58i8))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = 58i8, Int8.reverse(92i8))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = -69i8, Int8.reverse(-35i8))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = -35i8, Int8.reverse(-69i8))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = 29i8, Int8.reverse(-72i8))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = -72i8, Int8.reverse(29i8))

    @Test
    def reverse07(): Unit \ Assert = assertEq(expected = 32i8, Int8.reverse(4i8))

    @Test
    def reverse08(): Unit \ Assert = assertEq(expected = 4i8, Int8.reverse(32i8))

    @Test
    def reverse09(): Unit \ Assert = assertEq(expected = -1i8, Int8.reverse(-1i8))

    @Test
    def reverse10(): Unit \ Assert = assertEq(expected = 0i8, Int8.reverse(0i8))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Unit \ Assert = assertEq(expected = 7, Int8.highestOneBitPosition(-1i8))

    @Test
    def highestOneBitPosition02(): Unit \ Assert = assertEq(expected = 7, Int8.highestOneBitPosition(-128i8))

    @Test
    def highestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int8.highestOneBitPosition(1i8))

    @Test
    def highestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int8.highestOneBitPosition(2i8))

    @Test
    def highestOneBitPosition05(): Unit \ Assert = assertEq(expected = 1, Int8.highestOneBitPosition(3i8))

    @Test
    def highestOneBitPosition06(): Unit \ Assert = assertEq(expected = 4, Int8.highestOneBitPosition(20i8))

    @Test
    def highestOneBitPosition07(): Unit \ Assert = assertEq(expected = 6, Int8.highestOneBitPosition(Int8.pow(base = 2i8, 6i8)))

    @Test
    def highestOneBitPosition08(): Unit \ Assert = assertEq(expected = 6, Int8.highestOneBitPosition(127i8))

    @Test
    def highestOneBitPosition09(): Unit \ Assert = assertEq(expected = -1, Int8.highestOneBitPosition(0i8))

    @Test
    def highestOneBitPosition10(): Unit \ Assert = assertEq(expected = 5, Int8.highestOneBitPosition(Int8.pow(base = 2i8,3i8) + Int8.pow(base = 2i8, 5i8)))

    @Test
    def highestOneBitPosition11(): Unit \ Assert = assertEq(expected = 4, Int8.highestOneBitPosition(Int8.pow(base = 2i8, 3i8) + Int8.pow(base = 2i8, 4i8)))

    @Test
    def highestOneBitPosition12(): Unit \ Assert = assertEq(expected = 7, Int8.highestOneBitPosition(-10i8))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Unit \ Assert = assertEq(expected = Int8.leftShift(1i8, 7), Int8.highestOneBit(-1i8))

    @Test
    def highestOneBit02(): Unit \ Assert = assertEq(expected = Int8.leftShift(1i8, 7), Int8.highestOneBit(-128i8))

    @Test
    def highestOneBit03(): Unit \ Assert = assertEq(expected = Int8.leftShift(1i8, 7), Int8.highestOneBit(-77i8))

    @Test
    def highestOneBit04(): Unit \ Assert = assertEq(expected = 2i8, Int8.highestOneBit(2i8))

    @Test
    def highestOneBit05(): Unit \ Assert = assertEq(expected = 2i8, Int8.highestOneBit(3i8))

    @Test
    def highestOneBit06(): Unit \ Assert = assertEq(expected = 16i8, Int8.highestOneBit(20i8))

    @Test
    def highestOneBit07(): Unit \ Assert = assertEq(expected = 32i8, Int8.highestOneBit(Int8.pow(base = 2i8, 2i8) + Int8.pow(base = 2i8, 5i8)))

    @Test
    def highestOneBit08(): Unit \ Assert = assertEq(expected = 64i8, Int8.highestOneBit(127i8))

    @Test
    def highestOneBit09(): Unit \ Assert = assertEq(expected = 0i8, Int8.highestOneBit(0i8))

    @Test
    def highestOneBit10(): Unit \ Assert = assertEq(expected = 64i8, Int8.highestOneBit(Int8.pow(base = 2i8, 5i8) + Int8.pow(base = 2i8, 6i8)))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Unit \ Assert = assertEq(expected = 8, Int8.numberOfLeadingZeros(0i8))

    @Test
    def numberOfLeadingZeros02(): Unit \ Assert = assertEq(expected = 0, Int8.numberOfLeadingZeros(-128i8))

    @Test
    def numberOfLeadingZeros03(): Unit \ Assert = assertEq(expected = 0, Int8.numberOfLeadingZeros(-1i8))

    @Test
    def numberOfLeadingZeros04(): Unit \ Assert = assertEq(expected = 1, Int8.numberOfLeadingZeros(127i8))

    @Test
    def numberOfLeadingZeros05(): Unit \ Assert = assertEq(expected = 1, Int8.numberOfLeadingZeros(Int8.pow(base = 2i8, 6i8)))

    @Test
    def numberOfLeadingZeros06(): Unit \ Assert = assertEq(expected = 1, Int8.numberOfLeadingZeros(Int8.pow(base = 2i8, 3i8) + Int8.pow(base = 2i8, 6i8)))

    @Test
    def numberOfLeadingZeros07(): Unit \ Assert = assertEq(expected = 2, Int8.numberOfLeadingZeros(Int8.pow(base = 2i8, 4i8) + Int8.pow(base = 2i8, 5i8)))

    @Test
    def numberOfLeadingZeros08(): Unit \ Assert = assertEq(expected = 3, Int8.numberOfLeadingZeros(20i8))

    @Test
    def numberOfLeadingZeros09(): Unit \ Assert = assertEq(expected = 7, Int8.numberOfLeadingZeros(1i8))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Unit \ Assert = assertEq(expected = 0, Int8.lowestOneBitPosition(-1i8))

    @Test
    def lowestOneBitPosition02(): Unit \ Assert = assertEq(expected = 7, Int8.lowestOneBitPosition(-128i8))

    @Test
    def lowestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int8.lowestOneBitPosition(1i8))

    @Test
    def lowestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int8.lowestOneBitPosition(2i8))

    @Test
    def lowestOneBitPosition05(): Unit \ Assert = assertEq(expected = 0, Int8.lowestOneBitPosition(3i8))

    @Test
    def lowestOneBitPosition06(): Unit \ Assert = assertEq(expected = 2, Int8.lowestOneBitPosition(4i8))

    @Test
    def lowestOneBitPosition07(): Unit \ Assert = assertEq(expected = 2, Int8.lowestOneBitPosition(12i8))

    @Test
    def lowestOneBitPosition08(): Unit \ Assert = assertEq(expected = 4, Int8.lowestOneBitPosition(Int8.pow(base = 2i8, 4i8)))

    @Test
    def lowestOneBitPosition09(): Unit \ Assert = assertEq(expected = 4, Int8.lowestOneBitPosition(Int8.pow(base = 2i8, 4i8) + Int8.pow(base = 2i8, 6i8)))

    @Test
    def lowestOneBitPosition10(): Unit \ Assert = assertEq(expected = 0, Int8.lowestOneBitPosition(127i8))

    @Test
    def lowestOneBitPosition11(): Unit \ Assert = assertEq(expected = -1, Int8.lowestOneBitPosition(0i8))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Unit \ Assert = assertEq(expected = 1i8, Int8.lowestOneBit(-1i8))

    @Test
    def lowestOneBit02(): Unit \ Assert = assertEq(expected = Int8.leftShift(1i8, 7), Int8.lowestOneBit(-128i8))

    @Test
    def lowestOneBit03(): Unit \ Assert = assertEq(expected = 1i8, Int8.lowestOneBit(1i8))

    @Test
    def lowestOneBit04(): Unit \ Assert = assertEq(expected = 2i8, Int8.lowestOneBit(2i8))

    @Test
    def lowestOneBit05(): Unit \ Assert = assertEq(expected = 2i8, Int8.lowestOneBit(6i8))

    @Test
    def lowestOneBit06(): Unit \ Assert = assertEq(expected = 4i8, Int8.lowestOneBit(12i8))

    @Test
    def lowestOneBit07(): Unit \ Assert = assertEq(expected = 64i8, Int8.lowestOneBit(Int8.pow(base = 2i8, 6i8)))

    @Test
    def lowestOneBit08(): Unit \ Assert = assertEq(expected = 32i8, Int8.lowestOneBit(Int8.pow(base = 2i8, 5i8)))

    @Test
    def lowestOneBit09(): Unit \ Assert = assertEq(expected = 8i8, Int8.lowestOneBit(Int8.pow(base = 2i8, 3i8) + Int8.pow(base = 2i8, 5i8)))

    @Test
    def lowestOneBit10(): Unit \ Assert = assertEq(expected = 1i8, Int8.lowestOneBit(127i8))

    @Test
    def lowestOneBit11(): Unit \ Assert = assertEq(expected = 0i8, Int8.lowestOneBit(0i8))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Unit \ Assert = assertEq(expected = 8, Int8.numberOfTrailingZeros(0i8))

    @Test
    def numberOfTrailingZeros02(): Unit \ Assert = assertEq(expected = 0, Int8.numberOfTrailingZeros(3i8))

    @Test
    def numberOfTrailingZeros03(): Unit \ Assert = assertEq(expected = 1, Int8.numberOfTrailingZeros(42i8))

    @Test
    def numberOfTrailingZeros04(): Unit \ Assert = assertEq(expected = 4, Int8.numberOfTrailingZeros(Int8.pow(base = 2i8, 4i8) + Int8.pow(base = 2i8, 6i8)))

    @Test
    def numberOfTrailingZeros05(): Unit \ Assert = assertEq(expected = 6, Int8.numberOfTrailingZeros(Int8.pow(base = 2i8, 6i8)))

    @Test
    def numberOfTrailingZeros06(): Unit \ Assert = assertEq(expected = 0, Int8.numberOfTrailingZeros(-1i8))

    @Test
    def numberOfTrailingZeros07(): Unit \ Assert = assertEq(expected = 7, Int8.numberOfTrailingZeros(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Unit \ Assert = assertEq(expected = 1, Int8.getBit(pos = 5, -1i8))

    @Test
    def getBit02(): Unit \ Assert = assertEq(expected = 1, Int8.getBit(pos = 7, -100i8))

    @Test
    def getBit03(): Unit \ Assert = assertEq(expected = 1, Int8.getBit(pos = 5, Int8.pow(base = 2i8, 5i8)))

    @Test
    def getBit04(): Unit \ Assert = assertEq(expected = 0, Int8.getBit(pos = 6, Int8.pow(base = 2i8, 5i8)))

    @Test
    def getBit05(): Unit \ Assert = assertEq(expected = 0, Int8.getBit(pos = 4, Int8.pow(base = 2i8, 5i8)))

    @Test
    def getBit06(): Unit \ Assert = assertEq(expected = 0, Int8.getBit(pos = 4, -1i8 - Int8.pow(base = 2i8, 4i8)))

    @Test
    def getBit07(): Unit \ Assert = assertEq(expected = 1, Int8.getBit(pos = 5, -1i8 - Int8.pow(base = 2i8, 4i8)))

    @Test
    def getBit08(): Unit \ Assert = assertEq(expected = 0, Int8.getBit(pos = 7, 0i8))

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Unit \ Assert = assertEq(expected = 1i8, Int8.setBit(pos = 0, 0i8))

    @Test
    def setBit02(): Unit \ Assert = assertEq(expected = 2i8, Int8.setBit(pos = 1, 0i8))

    @Test
    def setBit03(): Unit \ Assert = assertEq(expected = -128i8, Int8.setBit(pos = 7, 0i8))

    @Test
    def setBit04(): Unit \ Assert = assertEq(expected = 0i8, Int8.setBit(pos = 8, 0i8))

    @Test
    def setBit05(): Unit \ Assert = assertEq(expected = -101i8, Int8.setBit(pos = 31, -101i8))

    @Test
    def setBit07(): Unit \ Assert = assertEq(expected = -1i8, Int8.setBit(pos = 4, -17i8))

    @Test
    def setBit08(): Unit \ Assert = assertEq(expected = -1i8, Int8.setBit(pos = 6, -65i8))

    @Test
    def setBit09(): Unit \ Assert = assertEq(expected = -17i8, Int8.setBit(pos = 15, -17i8))

    @Test
    def setBit10(): Unit \ Assert = assertEq(expected = -17i8, Int8.setBit(pos = 30, -17i8))

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Unit \ Assert = assertEq(expected = 0i8, Int8.clearBit(pos = 0, 1i8))

    @Test
    def clearBit02(): Unit \ Assert = assertEq(expected = 1i8, Int8.clearBit(pos = 1, 1i8))

    @Test
    def clearBit03(): Unit \ Assert = assertEq(expected = 0i8, Int8.clearBit(pos = 1, 2i8))

    @Test
    def clearBit04(): Unit \ Assert = assertEq(expected = 2i8, Int8.clearBit(pos = 2, 2i8))

    @Test
    def clearBit05(): Unit \ Assert = assertEq(expected = 0i8, Int8.clearBit(pos = 31, 0i8))

    @Test
    def clearBit06(): Unit \ Assert = assertEq(expected = 0i8, Int8.clearBit(pos = 30, 0i8))

    @Test
    def clearBit07(): Unit \ Assert = assertEq(expected = 0i8, Int8.clearBit(pos = 7, Int8.leftShift(1i8, 7)))

    @Test
    def clearBit08(): Unit \ Assert = assertEq(expected = 101i8, Int8.clearBit(pos = 3, 109i8))

    @Test
    def clearBit09(): Unit \ Assert = assertEq(expected = -2i8, Int8.clearBit(pos = 0, -1i8))

    @Test
    def clearBit10(): Unit \ Assert = assertEq(expected = -3i8, Int8.clearBit(pos = 1, -1i8))

    @Test
    def clearBit11(): Unit \ Assert = assertEq(expected = -33i8, Int8.clearBit(pos = 5, -1i8))

    @Test
    def clearBit12(): Unit \ Assert = assertEq(expected = Int8.bitwiseNot(Int8.leftShift(1i8, 7)), Int8.clearBit(pos = 7, -1i8))

    @Test
    def clearBit13(): Unit \ Assert = assertEq(expected = -2i8, Int8.clearBit(pos = 32, -1i8))

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Unit \ Assert = assertEq(expected = 0i8, Int8.flipBit(pos = 0, 1i8))

    @Test
    def flipBit02(): Unit \ Assert = assertEq(expected = 3i8, Int8.flipBit(pos = 1, 1i8))

    @Test
    def flipBit03(): Unit \ Assert = assertEq(expected = 0i8, Int8.flipBit(pos = 1, 2i8))

    @Test
    def flipBit04(): Unit \ Assert = assertEq(expected = 6i8, Int8.flipBit(pos = 2, 2i8))

    @Test
    def flipBit05(): Unit \ Assert = assertEq(expected = -128i8, Int8.flipBit(pos = 7, 0i8))

    @Test
    def flipBit06(): Unit \ Assert = assertEq(expected = 0i8, Int8.flipBit(pos = 8, 0i8))

    @Test
    def flipBit07(): Unit \ Assert = assertEq(expected = 0i8, Int8.flipBit(pos = 7, Int8.leftShift(1i8, 7)))

    @Test
    def flipBit08(): Unit \ Assert = assertEq(expected = -2i8, Int8.flipBit(pos = 0, -1i8))

    @Test
    def flipBit9(): Unit \ Assert = assertEq(expected = -3i8, Int8.flipBit(pos = 1, -1i8))

    @Test
    def flipBit10(): Unit \ Assert = assertEq(expected = -33i8, Int8.flipBit(pos = 5, -1i8))

    @Test
    def flipBit11(): Unit \ Assert = assertEq(expected = (-1i8-Int8.leftShift(1i8, 5)), Int8.flipBit(pos = 5, -1i8))

    @Test
    def flipBit12(): Unit \ Assert = assertEq(expected = -2i8, Int8.flipBit(pos = 32, -1i8))

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Unit \ Assert = assertEq(expected = 3i8, Int8.log2(10i8))

    @Test
    def log202(): Unit \ Assert = assertEq(expected = 3i8, Int8.log2(8i8))

    @Test
    def log203(): Unit \ Assert = assertEq(expected = 6i8, Int8.log2(127i8))

    @Test
    def log204(): Unit \ Assert = assertEq(expected = 0i8, Int8.log2(1i8))

    @Test
    def log205(): Unit \ Assert = assertEq(expected = 4i8, Int8.log2(20i8))

    @Test
    def log206(): Unit \ Assert = assertEq(expected = 0i8, Int8.log2(0i8))

    @Test
    def log207(): Unit \ Assert = assertEq(expected = 0i8, Int8.log2(-1i8))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseAnd(0i8, 0i8))

    @Test
    def bitwiseAnd02(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseAnd(127i8, 0i8))

    @Test
    def bitwiseAnd03(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseAnd(-128i8, 0i8))

    @Test
    def bitwiseAnd04(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseAnd(0i8, 1i8))

    @Test
    def bitwiseAnd05(): Unit \ Assert = assertEq(expected = 1i8, Int8.bitwiseAnd(127i8, 1i8))

    @Test
    def bitwiseAnd06(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseAnd(-128i8, 1i8))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Unit \ Assert = assertEq(expected = -1i8, Int8.bitwiseNot(0i8))

    @Test
    def bitwiseNot02(): Unit \ Assert = assertEq(expected = -2i8, Int8.bitwiseNot(1i8))

    @Test
    def bitwiseNot03(): Unit \ Assert = assertEq(expected = -0i8, Int8.bitwiseNot(-1i8))

    @Test
    def bitwiseNot04(): Unit \ Assert = assertEq(expected = -128i8, Int8.bitwiseNot(127i8))

    @Test
    def bitwiseNot05(): Unit \ Assert = assertEq(expected = 127i8, Int8.bitwiseNot(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseOr(0i8, 0i8))

    @Test
    def bitwiseOr02(): Unit \ Assert = assertEq(expected = 127i8, Int8.bitwiseOr(127i8, 0i8))

    @Test
    def bitwiseOr03(): Unit \ Assert = assertEq(expected = -128i8, Int8.bitwiseOr(-128i8, 0i8))

    @Test
    def bitwiseOr04(): Unit \ Assert = assertEq(expected = 1i8, Int8.bitwiseOr(0i8, 1i8))

    @Test
    def bitwiseOr05(): Unit \ Assert = assertEq(expected = 127i8, Int8.bitwiseOr(127i8, 1i8))

    @Test
    def bitwiseOr06(): Unit \ Assert = assertEq(expected = -127i8, Int8.bitwiseOr(-128i8, 1i8))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Unit \ Assert = assertEq(expected = 0i8, Int8.bitwiseXor(0i8, 0i8))

    @Test
    def bitwiseXor02(): Unit \ Assert = assertEq(expected = 127i8, Int8.bitwiseXor(127i8, 0i8))

    @Test
    def bitwiseXor03(): Unit \ Assert = assertEq(expected = -128i8, Int8.bitwiseXor(-128i8, 0i8))

    @Test
    def bitwiseXor04(): Unit \ Assert = assertEq(expected = 1i8, Int8.bitwiseXor(0i8, 1i8))

    @Test
    def bitwiseXor05(): Unit \ Assert = assertEq(expected = 126i8, Int8.bitwiseXor(127i8, 1i8))

    @Test
    def bitwiseXor06(): Unit \ Assert = assertEq(expected = -127i8, Int8.bitwiseXor(-128i8, 1i8))

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Unit \ Assert = assertEq(expected = 1, Int8.factorial(0i8))

    @Test
    def factorial02(): Unit \ Assert = assertEq(expected = 1, Int8.factorial(1i8))

    @Test
    def factorial03(): Unit \ Assert = assertEq(expected = 2, Int8.factorial(2i8))

    @Test
    def factorial04(): Unit \ Assert = assertEq(expected = 6, Int8.factorial(3i8))

    @Test
    def factorial05(): Unit \ Assert = assertEq(expected = 24, Int8.factorial(4i8))

    @Test
    def factorial06(): Unit \ Assert = assertEq(expected = 120, Int8.factorial(5i8))

    @Test
    def factorial07(): Unit \ Assert = assertEq(expected = 3628800, Int8.factorial(10i8))

    @Test
    def factorial08(): Unit \ Assert = assertEq(expected = 0, Int8.factorial(-1i8))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0", Int8.toString(0i8))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1", Int8.toString(1i8))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1", Int8.toString(-1i8))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "127", Int8.toString(127i8))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-128", Int8.toString(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertTrue(Int8.fromString("Bad") |> Option.isEmpty)

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1i8), Int8.fromString(" 1"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1i8), Int8.fromString("1 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0i8), Int8.fromString("0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1i8), Int8.fromString("1"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1i8), Int8.fromString("-1"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(127i8), Int8.fromString("127"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-128i8), Int8.fromString("-128"))

    /////////////////////////////////////////////////////////////////////////////
    // toInt16                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt1601(): Unit \ Assert = assertEq(expected = 0i16, Int8.toInt16(0i8))

    @Test
    def toInt1602(): Unit \ Assert = assertEq(expected = 1i16, Int8.toInt16(1i8))

    @Test
    def toInt1603(): Unit \ Assert = assertEq(expected = -1i16, Int8.toInt16(-1i8))

    @Test
    def toInt1604(): Unit \ Assert = assertEq(expected = 127i16, Int8.toInt16(127i8))

    @Test
    def toInt1605(): Unit \ Assert = assertEq(expected = -128i16, Int8.toInt16(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt3201(): Unit \ Assert = assertEq(expected = 0, Int8.toInt32(0i8))

    @Test
    def toInt3202(): Unit \ Assert = assertEq(expected = 1, Int8.toInt32(1i8))

    @Test
    def toInt3203(): Unit \ Assert = assertEq(expected = -1, Int8.toInt32(-1i8))

    @Test
    def toInt3204(): Unit \ Assert = assertEq(expected = 127, Int8.toInt32(127i8))

    @Test
    def toInt3205(): Unit \ Assert = assertEq(expected = -128, Int8.toInt32(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toInt6401(): Unit \ Assert = assertEq(expected = 0i64, Int8.toInt64(0i8))

    @Test
    def toInt6402(): Unit \ Assert = assertEq(expected = 1i64, Int8.toInt64(1i8))

    @Test
    def toInt6403(): Unit \ Assert = assertEq(expected = -1i64, Int8.toInt64(-1i8))

    @Test
    def toInt6404(): Unit \ Assert = assertEq(expected = 127i64, Int8.toInt64(127i8))

    @Test
    def toInt6405(): Unit \ Assert = assertEq(expected = -128i64, Int8.toInt64(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Unit \ Assert = assertEq(expected = 0ii, Int8.toBigInt(0i8))

    @Test
    def toBigInt02(): Unit \ Assert = assertEq(expected = 1ii, Int8.toBigInt(1i8))

    @Test
    def toBigInt03(): Unit \ Assert = assertEq(expected = -1ii, Int8.toBigInt(-1i8))

    @Test
    def toBigInt04(): Unit \ Assert = assertEq(expected = 127ii, Int8.toBigInt(127i8))

    @Test
    def toBigInt05(): Unit \ Assert = assertEq(expected = -128ii, Int8.toBigInt(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Unit \ Assert = assertEq(expected = 0.0f32, Int8.toFloat32(0i8))

    @Test
    def toFloat3202(): Unit \ Assert = assertEq(expected = 1.0f32, Int8.toFloat32(1i8))

    @Test
    def toFloat3203(): Unit \ Assert = assertEq(expected = -1.0f32, Int8.toFloat32(-1i8))

    @Test
    def toFloat3204(): Unit \ Assert = assertEq(expected = 127.0f32, Int8.toFloat32(127i8))

    @Test
    def toFloat3205(): Unit \ Assert = assertEq(expected = -128.0f32, Int8.toFloat32(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, Int8.toFloat64(0i8))

    @Test
    def toFloat6402(): Unit \ Assert = assertEq(expected = 1.0f64, Int8.toFloat64(1i8))

    @Test
    def toFloat6403(): Unit \ Assert = assertEq(expected = -1.0f64, Int8.toFloat64(-1i8))

    @Test
    def toFloat6404(): Unit \ Assert = assertEq(expected = 127.0f64, Int8.toFloat64(127i8))

    @Test
    def toFloat6405(): Unit \ Assert = assertEq(expected = -128.0f64, Int8.toFloat64(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Unit \ Assert = assertEq(expected = 0.0ff, Int8.toBigDecimal(0i8))

    @Test
    def toBigDecimal02(): Unit \ Assert = assertEq(expected = 1.0ff, Int8.toBigDecimal(1i8))

    @Test
    def toBigDecimal03(): Unit \ Assert = assertEq(expected = -1.0ff, Int8.toBigDecimal(-1i8))

    @Test
    def toBigDecimal04(): Unit \ Assert = assertEq(expected = 127.0ff, Int8.toBigDecimal(127i8))

    @Test
    def toBigDecimal05(): Unit \ Assert = assertEq(expected = -128.0ff, Int8.toBigDecimal(-128i8))

    /////////////////////////////////////////////////////////////////////////////
    // byteValue                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def byteValue01(): Unit \ Assert =
        let i = Int8.valueOf(0i8);
        assertEq(expected = 0i8, Int8.byteValue(i))

    @Test
    def byteValue02(): Unit \ Assert =
        let i = Int8.valueOf(1i8);
        assertEq(expected = 1i8, Int8.byteValue(i))

    @Test
    def byteValue03(): Unit \ Assert =
        let i = Int8.valueOf(-1i8);
        assertEq(expected = -1i8, Int8.byteValue(i))

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Unit \ Assert =
        let i = Int8.valueOf(0i8);
        assertTrue(i.equals(Int8.valueOf(0i8)))

    @Test
    def valueOf02(): Unit \ Assert =
        let i = Int8.valueOf(1i8);
        assertTrue(i.equals(Int8.valueOf(1i8)))

    @Test
    def valueOf03(): Unit \ Assert =
        let i = Int8.valueOf(-1i8);
        assertTrue(i.equals(Int8.valueOf(-1i8)))

}
