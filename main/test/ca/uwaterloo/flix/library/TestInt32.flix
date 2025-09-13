mod TestInt32 {

    use Mul.mul
    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertSome, assertNone, assertErr, assertOk}

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert = assertEq(expected = 0, mul(1, 0))

    @Test
    def mul02(): Unit \ Assert = assertEq(expected = 0, mul(0, 1))

    @Test
    def mul03(): Unit \ Assert = assertEq(expected = 0, mul(0, 0))

    @Test
    def mul04(): Unit \ Assert = assertEq(expected = -2147483648, mul(-4, 536870912))

    @Test
    def mul05(): Unit \ Assert = assertEq(expected = 2147483647, mul(-1, -2147483647))

    @Test
    def mul06(): Unit \ Assert = assertEq(expected = 55, mul(5, 11))

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maxValue01(): Unit \ Assert = assertEq(expected = Int32.bitwiseNot(Int32.leftShift(1, 31)), Int32.maxValue())

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minValue01(): Unit \ Assert = assertEq(expected = Int32.leftShift(1, 31), Int32.minValue())

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = assertEq(expected = 32, Int32.size())

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1, Int32.min(1, 2))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1, Int32.min(2, -1))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66, Int32.min(-33, -66))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49, Int32.max(48, 49))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4, Int32.max(4, -16))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16, Int32.max(-34, -16))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 31, Int32.abs(31))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 31, Int32.abs(-31))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 2147483647, Int32.abs(2147483647))

    @Test
    def abs04(): Unit \ Assert = assertEq(expected = 2147483647, Int32.abs(-2147483647))

    @Test
    def abs05(): Unit \ Assert = assertEq(expected = 0, Int32.abs(0))

    @Test
    def abs06(): Unit \ Assert = assertEq(expected = -1, Int32.abs(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def modulo01(): Unit \ Assert = assertEq(expected = 0, 1200000 `Int32.modulo` 200000)

    @Test
    def modulo02(): Unit \ Assert = assertEq(expected = 200000, 1200000 `Int32.modulo` 500000)

    @Test
    def modulo03(): Unit \ Assert = assertEq(expected = 300000, -1200000 `Int32.modulo` 500000)

    @Test
    def modulo04(): Unit \ Assert = assertEq(expected = -300000, 1200000 `Int32.modulo` -500000)

    @Test
    def modulo05(): Unit \ Assert = assertEq(expected = -200000, -1200000 `Int32.modulo` -500000)

    @Test
    def modulo06(): Unit \ Assert = assertEq(expected = 0i32, 1200000i32 `Int32.modulo` 200000i32)

    @Test
    def modulo07(): Unit \ Assert = assertEq(expected = 200000i32, 1200000i32 `Int32.modulo` 500000i32)

    @Test
    def modulo08(): Unit \ Assert = assertEq(expected = 300000i32, -1200000i32 `Int32.modulo` 500000i32)

    @Test
    def modulo09(): Unit \ Assert = assertEq(expected = -300000i32, 1200000i32 `Int32.modulo` -500000i32)

    @Test
    def modulo10(): Unit \ Assert = assertEq(expected = -200000i32, -1200000i32 `Int32.modulo` -500000i32)

    @Test
    def modulo11(): Unit \ Assert = assertEq(expected = 0, 1 `Int32.modulo` 0)

    @Test
    def modulo12(): Unit \ Assert = assertEq(expected = 0i32, 1i32 `Int32.modulo` 0i32)

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remainder01(): Unit \ Assert = assertEq(expected = 0, 1200000 `Int32.remainder` 200000)

    @Test
    def remainder02(): Unit \ Assert = assertEq(expected = 200000, 1200000 `Int32.remainder` 500000)

    @Test
    def remainder03(): Unit \ Assert = assertEq(expected = -200000, -1200000 `Int32.remainder` 500000)

    @Test
    def remainder04(): Unit \ Assert = assertEq(expected = 200000, 1200000 `Int32.remainder` -500000)

    @Test
    def remainder05(): Unit \ Assert = assertEq(expected = -200000, -1200000 `Int32.remainder` -500000)

    @Test
    def remainder06(): Unit \ Assert = assertEq(expected = 0i32, 1200000i32 `Int32.remainder` 200000i32)

    @Test
    def remainder07(): Unit \ Assert = assertEq(expected = 200000i32, 1200000i32 `Int32.remainder` 500000i32)

    @Test
    def remainder08(): Unit \ Assert = assertEq(expected = -200000i32, -1200000i32 `Int32.remainder` 500000i32)

    @Test
    def remainder09(): Unit \ Assert = assertEq(expected = 200000i32, 1200000i32 `Int32.remainder` -500000i32)

    @Test
    def remainder10(): Unit \ Assert = assertEq(expected = -200000i32, -1200000i32 `Int32.remainder` -500000i32)

    @Test
    def remainder11(): Unit \ Assert = assertEq(expected = 0, 1 `Int32.remainder` 0)

    @Test
    def remainder12(): Unit \ Assert = assertEq(expected = 0i32, 1i32 `Int32.remainder` 0i32)

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 38, Int32.dist(31, -7))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 43, Int32.dist(-44, -1))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0, Int32.dist(-2, -2))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 2147483647, Int32.dist(-2147483648, -1))

    @Test
    def dist05(): Unit \ Assert = assertEq(expected = -1, Int32.dist(-2147483648, 0))

    @Test
    def dist06(): Unit \ Assert = assertEq(expected = 127, Int32.dist(-123, 4))

    @Test
    def dist07(): Unit \ Assert = assertEq(expected = -1, Int32.dist(-2147483643, 5))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Unit \ Assert = assertEq(expected = -1, Int32.compare(-1, 44))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, Int32.compare(-1, -44))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, Int32.compare(88, 88))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def signum01(): Unit \ Assert = assertEq(expected = -1, Int32.signum(-22))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, Int32.signum(22))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, Int32.signum(0))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1i32, Int32.pow(base = 0i32, 0i32))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0i32, Int32.pow(base = 0i32, 1i32))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1i32, Int32.pow(base = 1i32, 0i32))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1i32, Int32.pow(base = 1i32, 1i32))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25i32, Int32.pow(base = 5i32, 2i32))

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testBitwiseLeftShift01(): Unit \ Assert = assertEq(expected = 8, Int32.leftShift(8, 0))

    @Test
    def testBitwiseLeftShift02(): Unit \ Assert = assertEq(expected = 524288, Int32.leftShift(8, 16))

    @Test
    def testBitwiseLeftShift03(): Unit \ Assert = assertEq(expected = -2147483648, Int32.leftShift(8, 28))

    @Test
    def testBitwiseLeftShift04(): Unit \ Assert = assertEq(expected = 0, Int32.leftShift(8, 29))

    @Test
    def testBitwiseLeftShift05(): Unit \ Assert = assertEq(expected = 8i32, Int32.leftShift(8i32, 0))

    @Test
    def testBitwiseLeftShift06(): Unit \ Assert = assertEq(expected = 524288i32, Int32.leftShift(8i32, 16))

    @Test
    def testBitwiseLeftShift07(): Unit \ Assert = assertEq(expected = -2147483648i32, Int32.leftShift(8i32, 28))

    @Test
    def testBitwiseLeftShift08(): Unit \ Assert = assertEq(expected = 0i32, Int32.leftShift(8i32, 29))


    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testBitwiseRightShift01(): Unit \ Assert = assertEq(expected = 120000, Int32.rightShift(120000, 0))

    @Test
    def testBitwiseRightShift02(): Unit \ Assert = assertEq(expected = 30000, Int32.rightShift(120000, 2))

    @Test
    def testBitwiseRightShift03(): Unit \ Assert = assertEq(expected = 0, Int32.rightShift(120000, 31))

    @Test
    def testBitwiseRightShift04(): Unit \ Assert = assertEq(expected = -30000, Int32.rightShift(-120000, 2))

    @Test
    def testBitwiseRightShift05(): Unit \ Assert = assertEq(expected = 120000i32, Int32.rightShift(120000i32, 0))

    @Test
    def testBitwiseRightShift06(): Unit \ Assert = assertEq(expected = 30000i32, Int32.rightShift(120000i32, 2))

    @Test
    def testBitwiseRightShift07(): Unit \ Assert = assertEq(expected = 0i32, Int32.rightShift(120000i32, 31))

    @Test
    def testBitwiseRightShift08(): Unit \ Assert = assertEq(expected = -30000i32, Int32.rightShift(-120000i32, 2))

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def logicalRightShift01(): Unit \ Assert = assertEq(expected = Int32.rightShift(2233, 4), Int32.logicalRightShift(dist = 4, 2233))

    @Test
    def logicalRightShift02(): Unit \ Assert = assertEq(expected = Int32.rightShift(223366, 33), Int32.logicalRightShift(dist = 33, 223366))

    @Test
    def logicalRightShift03(): Unit \ Assert = assertEq(expected = Int32.rightShift(2233, -8), Int32.logicalRightShift(dist = -8, 2233))

    @Test
    def logicalRightShift04(): Unit \ Assert = assertEq(expected = Int32.rightShift(2233, 4), Int32.logicalRightShift(dist = 4, 2233))

    @Test
    def logicalRightShift05(): Unit \ Assert = assertEq(expected = 2147483647, Int32.logicalRightShift(dist = 1, -1))

    @Test
    def logicalRightShift06(): Unit \ Assert = assertEq(expected = 268435453, Int32.logicalRightShift(dist = 4, -35))

    @Test
    def logicalRightShift07(): Unit \ Assert = assertEq(expected = 33554410, Int32.logicalRightShift(dist = -2777, -2777))

    @Test
    def logicalRightShift08(): Unit \ Assert = assertEq(expected = -233, Int32.logicalRightShift(dist = 64, -233))

    @Test
    def logicalRightShift09(): Unit \ Assert = assertEq(expected = -27, Int32.logicalRightShift(dist = 0, -27))

    @Test
    def logicalRightShift10(): Unit \ Assert = assertEq(expected = 1073741823, Int32.logicalRightShift(dist = 1, 2147483647))

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitCount01(): Unit \ Assert = assertEq(expected = 8, Int32.bitCount(19909))

    @Test
    def bitCount02(): Unit \ Assert = assertEq(expected = 32, Int32.bitCount(-1))

    @Test
    def bitCount03(): Unit \ Assert = assertEq(expected = 12, Int32.bitCount(-999888888))

    @Test
    def bitCount04(): Unit \ Assert = assertEq(expected = 0, Int32.bitCount(0))

    @Test
    def bitCount05(): Unit \ Assert = assertEq(expected = 31, Int32.bitCount(2147483647))

    @Test
    def bitCount06(): Unit \ Assert = assertEq(expected = 1, Int32.bitCount(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Unit \ Assert = assertEq(expected = -1402440343, Int32.rotateLeft(dist = 4, -1698265258))

    @Test
    def rotateLeft02(): Unit \ Assert = assertEq(expected = 1355868882, Int32.rotateLeft(dist = 5, -1836677290))

    @Test
    def rotateLeft03(): Unit \ Assert = assertEq(expected = 156769526, Int32.rotateLeft(dist = 24, 1478293001))

    @Test
    def rotateLeft04(): Unit \ Assert = assertEq(expected = 536104180, Int32.rotateLeft(dist = -36, -12267711))

    @Test
    def rotateLeft05(): Unit \ Assert = assertEq(expected = 0, Int32.rotateLeft(dist = 87, 0))

    @Test
    def rotateLeft06(): Unit \ Assert = assertEq(expected = -1, Int32.rotateLeft(dist = 87, -1))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = 1772906613, Int32.rotateRight(dist = 4, -1698265258))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = -1265355718, Int32.rotateRight(dist = 5, -1836677290))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = 485886296, Int32.rotateRight(dist = 24, 1478293001))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = -196283361, Int32.rotateRight(dist = -36, -12267711))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = 0, Int32.rotateRight(dist = 87, 0))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = -1, Int32.rotateRight(dist = 87, -1))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = 198530390, Int32.reverse(1789570000))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = 1789570000, Int32.reverse(198530390))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = 1968512067, Int32.reverse(-1036473682))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = -1036473682, Int32.reverse(1968512067))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = -1862270976, Int32.reverse(137))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = 137, Int32.reverse(-1862270976))

    @Test
    def reverse07(): Unit \ Assert = assertEq(expected = 536870912, Int32.reverse(4))

    @Test
    def reverse08(): Unit \ Assert = assertEq(expected = 4, Int32.reverse(536870912))

    @Test
    def reverse09(): Unit \ Assert = assertEq(expected = -1, Int32.reverse(-1))

    @Test
    def reverse10(): Unit \ Assert = assertEq(expected = 0, Int32.reverse(0))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def highestOneBitPosition01(): Unit \ Assert = assertEq(expected = 31, Int32.highestOneBitPosition(-1))

    @Test
    def highestOneBitPosition02(): Unit \ Assert = assertEq(expected = 31, Int32.highestOneBitPosition(-2147483648))

    @Test
    def highestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int32.highestOneBitPosition(1))

    @Test
    def highestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int32.highestOneBitPosition(2))

    @Test
    def highestOneBitPosition05(): Unit \ Assert = assertEq(expected = 1, Int32.highestOneBitPosition(3))

    @Test
    def highestOneBitPosition06(): Unit \ Assert = assertEq(expected = 4, Int32.highestOneBitPosition(20))

    @Test
    def highestOneBitPosition07(): Unit \ Assert = assertEq(expected = 30, Int32.highestOneBitPosition(Int32.pow(base = 2, 30)))

    @Test
    def highestOneBitPosition08(): Unit \ Assert = assertEq(expected = 30, Int32.highestOneBitPosition(2147483647))

    @Test
    def highestOneBitPosition09(): Unit \ Assert = assertEq(expected = -1, Int32.highestOneBitPosition(0))

    @Test
    def highestOneBitPosition10(): Unit \ Assert = assertEq(expected = 23, Int32.highestOneBitPosition(Int32.pow(base = 2, 18) + Int32.pow(base = 2, 23)))

    @Test
    def highestOneBitPosition11(): Unit \ Assert = assertEq(expected = 7, Int32.highestOneBitPosition(Int32.pow(base = 2, 3) + Int32.pow(base = 2, 7)))

    @Test
    def highestOneBitPosition12(): Unit \ Assert = assertEq(expected = 31, Int32.highestOneBitPosition(-32767))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def highestOneBit01(): Unit \ Assert = assertEq(expected = Int32.leftShift(1, 31), Int32.highestOneBit(-1))

    @Test
    def highestOneBit02(): Unit \ Assert = assertEq(expected = Int32.leftShift(1, 31), Int32.highestOneBit(-2147483000))

    @Test
    def highestOneBit03(): Unit \ Assert = assertEq(expected = Int32.leftShift(1, 31), Int32.highestOneBit(-77))

    @Test
    def highestOneBit04(): Unit \ Assert = assertEq(expected = 2, Int32.highestOneBit(2))

    @Test
    def highestOneBit05(): Unit \ Assert = assertEq(expected = 2, Int32.highestOneBit(3))

    @Test
    def highestOneBit06(): Unit \ Assert = assertEq(expected = 16, Int32.highestOneBit(20))

    @Test
    def highestOneBit07(): Unit \ Assert = assertEq(expected = 1073741824, Int32.highestOneBit(Int32.pow(base = 2, 30)))

    @Test
    def highestOneBit08(): Unit \ Assert = assertEq(expected = 1073741824, Int32.highestOneBit(2147483647))

    @Test
    def highestOneBit09(): Unit \ Assert = assertEq(expected = 0, Int32.highestOneBit(0))

    @Test
    def highestOneBit10(): Unit \ Assert = assertEq(expected = 16777216, Int32.highestOneBit(Int32.pow(base = 2, 18) + Int32.pow(base = 2, 24)))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def numberOfLeadingZeros01(): Unit \ Assert = assertEq(expected = 32, Int32.numberOfLeadingZeros(0))

    @Test
    def numberOfLeadingZeros02(): Unit \ Assert = assertEq(expected = 0, Int32.numberOfLeadingZeros(Int32.leftShift(1, 31)))

    @Test
    def numberOfLeadingZeros03(): Unit \ Assert = assertEq(expected = 0, Int32.numberOfLeadingZeros(-1))

    @Test
    def numberOfLeadingZeros04(): Unit \ Assert = assertEq(expected = 1, Int32.numberOfLeadingZeros(Int32.bitwiseNot(Int32.leftShift(1, 31))))

    @Test
    def numberOfLeadingZeros05(): Unit \ Assert = assertEq(expected = 2, Int32.numberOfLeadingZeros(Int32.pow(base = 2, 29)))

    @Test
    def numberOfLeadingZeros06(): Unit \ Assert = assertEq(expected = 7, Int32.numberOfLeadingZeros(Int32.pow(base = 2, 24) + Int32.pow(base = 2, 18)))

    @Test
    def numberOfLeadingZeros07(): Unit \ Assert = assertEq(expected = 24, Int32.numberOfLeadingZeros(Int32.pow(base = 2, 7) + Int32.pow(base = 2, 6)))

    @Test
    def numberOfLeadingZeros08(): Unit \ Assert = assertEq(expected = 27, Int32.numberOfLeadingZeros(20))

    @Test
    def numberOfLeadingZeros09(): Unit \ Assert = assertEq(expected = 31, Int32.numberOfLeadingZeros(1))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lowestOneBitPosition01(): Unit \ Assert = assertEq(expected = 0, Int32.lowestOneBitPosition(-1))

    @Test
    def lowestOneBitPosition02(): Unit \ Assert = assertEq(expected = 31, Int32.lowestOneBitPosition(-2147483648))

    @Test
    def lowestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int32.lowestOneBitPosition(1))

    @Test
    def lowestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int32.lowestOneBitPosition(2))

    @Test
    def lowestOneBitPosition05(): Unit \ Assert = assertEq(expected = 0, Int32.lowestOneBitPosition(3))

    @Test
    def lowestOneBitPosition06(): Unit \ Assert = assertEq(expected = 2, Int32.lowestOneBitPosition(4))

    @Test
    def lowestOneBitPosition07(): Unit \ Assert = assertEq(expected = 2, Int32.lowestOneBitPosition(12))

    @Test
    def lowestOneBitPosition08(): Unit \ Assert = assertEq(expected = 30, Int32.lowestOneBitPosition(Int32.pow(base = 2, 30)))

    @Test
    def lowestOneBitPosition09(): Unit \ Assert = assertEq(expected = 18, Int32.lowestOneBitPosition(Int32.pow(base = 2, 23) + Int32.pow(base = 2, 18)))

    @Test
    def lowestOneBitPosition10(): Unit \ Assert = assertEq(expected = 0, Int32.lowestOneBitPosition(2147483647))

    @Test
    def lowestOneBitPosition11(): Unit \ Assert = assertEq(expected = -1, Int32.lowestOneBitPosition(0))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lowestOneBit01(): Unit \ Assert = assertEq(expected = 1, Int32.lowestOneBit(-1))

    @Test
    def lowestOneBit02(): Unit \ Assert = assertEq(expected = Int32.leftShift(1, 31), Int32.lowestOneBit(-2147483648))

    @Test
    def lowestOneBit03(): Unit \ Assert = assertEq(expected = 1, Int32.lowestOneBit(1))

    @Test
    def lowestOneBit04(): Unit \ Assert = assertEq(expected = 2, Int32.lowestOneBit(2))

    @Test
    def lowestOneBit05(): Unit \ Assert = assertEq(expected = 2, Int32.lowestOneBit(6))

    @Test
    def lowestOneBit06(): Unit \ Assert = assertEq(expected = 4, Int32.lowestOneBit(12))

    @Test
    def lowestOneBit07(): Unit \ Assert = assertEq(expected = 64, Int32.lowestOneBit(Int32.pow(base = 2, 6) + Int32.pow(base = 2, 18) + Int32.pow(base = 2, 27)))

    @Test
    def lowestOneBit08(): Unit \ Assert = assertEq(expected = 1073741824, Int32.lowestOneBit(Int32.pow(base = 2, 30)))

    @Test
    def lowestOneBit09(): Unit \ Assert = assertEq(expected = 262144, Int32.lowestOneBit(Int32.pow(base = 2, 18) + Int32.pow(base = 2, 23)))

    @Test
    def lowestOneBit10(): Unit \ Assert = assertEq(expected = 1, Int32.lowestOneBit(2147483647))

    @Test
    def lowestOneBit11(): Unit \ Assert = assertEq(expected = 0, Int32.lowestOneBit(0))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def numberOfTrailingZeros01(): Unit \ Assert = assertEq(expected = 32, Int32.numberOfTrailingZeros(0))

    @Test
    def numberOfTrailingZeros02(): Unit \ Assert = assertEq(expected = 0, Int32.numberOfTrailingZeros(3))

    @Test
    def numberOfTrailingZeros03(): Unit \ Assert = assertEq(expected = 1, Int32.numberOfTrailingZeros(42))

    @Test
    def numberOfTrailingZeros04(): Unit \ Assert = assertEq(expected = 16, Int32.numberOfTrailingZeros(Int32.pow(base = 2, 16) + Int32.pow(base = 2, 22)))

    @Test
    def numberOfTrailingZeros05(): Unit \ Assert = assertEq(expected = 30, Int32.numberOfTrailingZeros(Int32.pow(base = 2, 30)))

    @Test
    def numberOfTrailingZeros06(): Unit \ Assert = assertEq(expected = 0, Int32.numberOfTrailingZeros(-1))

    @Test
    def numberOfTrailingZeros07(): Unit \ Assert = assertEq(expected = 31, Int32.numberOfTrailingZeros(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getBit01(): Unit \ Assert = assertEq(expected = 1, Int32.getBit(pos = 5, -1))

    @Test
    def getBit02(): Unit \ Assert = assertEq(expected = 1, Int32.getBit(pos = 31, -1000000))

    @Test
    def getBit03(): Unit \ Assert = assertEq(expected = 1, Int32.getBit(pos = 23, Int32.pow(base = 2, 23)))

    @Test
    def getBit04(): Unit \ Assert = assertEq(expected = 0, Int32.getBit(pos = 24, Int32.pow(base = 2, 23)))

    @Test
    def getBit05(): Unit \ Assert = assertEq(expected = 0, Int32.getBit(pos = 22, Int32.pow(base = 2, 23)))

    @Test
    def getBit06(): Unit \ Assert = assertEq(expected = 0, Int32.getBit(pos = 25, -1 - Int32.pow(base = 2, 25)))

    @Test
    def getBit07(): Unit \ Assert = assertEq(expected = 1, Int32.getBit(pos = 26, -1 - Int32.pow(base = 2, 25)))

    @Test
    def getBit08(): Unit \ Assert = assertEq(expected = 0, Int32.getBit(pos = 15, 0))

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setBit01(): Unit \ Assert = assertEq(expected = 1, Int32.setBit(pos = 0, 0))

    @Test
    def setBit02(): Unit \ Assert = assertEq(expected = 2, Int32.setBit(pos = 1, 0))

    @Test
    def setBit03(): Unit \ Assert = assertEq(expected = -2147483648, Int32.setBit(pos = 31, 0))

    @Test
    def setBit04(): Unit \ Assert = assertEq(expected = 1073741824, Int32.setBit(pos = 30, 0))

    @Test
    def setBit05(): Unit \ Assert = assertEq(expected = -1418638422, Int32.setBit(pos = 31, -1418638422))

    @Test
    def setBit07(): Unit \ Assert = assertEq(expected = -1, Int32.setBit(pos = 4, -17))

    @Test
    def setBit08(): Unit \ Assert = assertEq(expected = -1, Int32.setBit(pos = 10, -1025))

    @Test
    def setBit09(): Unit \ Assert = assertEq(expected = -17, Int32.setBit(pos = 31, -17))

    @Test
    def setBit10(): Unit \ Assert = assertEq(expected = -17, Int32.setBit(pos = 30, -17))

    @Test
    def setBit11(): Unit \ Assert = assertEq(expected = 1, Int32.setBit(pos = 32, 0))

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clearBit01(): Unit \ Assert = assertEq(expected = 0, Int32.clearBit(pos = 0, 1))

    @Test
    def clearBit02(): Unit \ Assert = assertEq(expected = 1, Int32.clearBit(pos = 1, 1))

    @Test
    def clearBit03(): Unit \ Assert = assertEq(expected = 0, Int32.clearBit(pos = 1, 2))

    @Test
    def clearBit04(): Unit \ Assert = assertEq(expected = 2, Int32.clearBit(pos = 2, 2))

    @Test
    def clearBit05(): Unit \ Assert = assertEq(expected = 0, Int32.clearBit(pos = 31, 0))

    @Test
    def clearBit06(): Unit \ Assert = assertEq(expected = 0, Int32.clearBit(pos = 30, 0))

    @Test
    def clearBit07(): Unit \ Assert = assertEq(expected = 0, Int32.clearBit(pos = 31, -2147483648))

    @Test
    def clearBit08(): Unit \ Assert = assertEq(expected = 308714120, Int32.clearBit(pos = 27, 442931848))

    @Test
    def clearBit09(): Unit \ Assert = assertEq(expected = -2, Int32.clearBit(pos = 0, -1))

    @Test
    def clearBit10(): Unit \ Assert = assertEq(expected = -3, Int32.clearBit(pos = 1, -1))

    @Test
    def clearBit11(): Unit \ Assert = assertEq(expected = -33, Int32.clearBit(pos = 5, -1))

    @Test
    def clearBit12(): Unit \ Assert = assertEq(expected = 2147483647, Int32.clearBit(pos = 31, -1))

    @Test
    def clearBit13(): Unit \ Assert = assertEq(expected = -2, Int32.clearBit(pos = 32, -1))

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flipBit01(): Unit \ Assert = assertEq(expected = 0, Int32.flipBit(pos = 0, 1))

    @Test
    def flipBit02(): Unit \ Assert = assertEq(expected = 3, Int32.flipBit(pos = 1, 1))

    @Test
    def flipBit03(): Unit \ Assert = assertEq(expected = 0, Int32.flipBit(pos = 1, 2))

    @Test
    def flipBit04(): Unit \ Assert = assertEq(expected = 6, Int32.flipBit(pos = 2, 2))

    @Test
    def flipBit05(): Unit \ Assert = assertEq(expected = -2147483648, Int32.flipBit(pos = 31, 0))

    @Test
    def flipBit06(): Unit \ Assert = assertEq(expected = 1073741824, Int32.flipBit(pos = 30, 0))

    @Test
    def flipBit07(): Unit \ Assert = assertEq(expected = 0, Int32.flipBit(pos = 31, -2147483648))

    @Test
    def flipBit08(): Unit \ Assert = assertEq(expected = -2, Int32.flipBit(pos = 0, -1))

    @Test
    def flipBit9(): Unit \ Assert = assertEq(expected = -3, Int32.flipBit(pos = 1, -1))

    @Test
    def flipBit10(): Unit \ Assert = assertEq(expected = -33, Int32.flipBit(pos = 5, -1))

    @Test
    def flipBit11(): Unit \ Assert = assertEq(expected = 2147483647, Int32.flipBit(pos = 31, -1))

    @Test
    def flipBit12(): Unit \ Assert = assertEq(expected = -2, Int32.flipBit(pos = 32, -1))

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Unit \ Assert = assertEq(expected = 3, Int32.log2(10))

    @Test
    def log202(): Unit \ Assert = assertEq(expected = 13, Int32.log2(10000))

    @Test
    def log203(): Unit \ Assert = assertEq(expected = 3, Int32.log2(8))

    @Test
    def log204(): Unit \ Assert = assertEq(expected = 8, Int32.log2(256))

    @Test
    def log205(): Unit \ Assert = assertEq(expected = 0, Int32.log2(1))

    @Test
    def log206(): Unit \ Assert = assertEq(expected = 23, Int32.log2(12345678))

    @Test
    def log207(): Unit \ Assert = assertEq(expected = 4, Int32.log2(20))

    @Test
    def log208(): Unit \ Assert = assertEq(expected = 0, Int32.log2(0))

    @Test
    def log209(): Unit \ Assert = assertEq(expected = 0, Int32.log2(-1))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseAnd(0i32, 0i32))

    @Test
    def bitwiseAnd02(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseAnd(127i32, 0i32))

    @Test
    def bitwiseAnd03(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseAnd(-128i32, 0i32))

    @Test
    def bitwiseAnd04(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseAnd(0i32, 1i32))

    @Test
    def bitwiseAnd05(): Unit \ Assert = assertEq(expected = 1i32, Int32.bitwiseAnd(127i32, 1i32))

    @Test
    def bitwiseAnd06(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseAnd(-128i32, 1i32))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Unit \ Assert = assertEq(expected = -1i32, Int32.bitwiseNot(0i32))

    @Test
    def bitwiseNot02(): Unit \ Assert = assertEq(expected = -2i32, Int32.bitwiseNot(1i32))

    @Test
    def bitwiseNot03(): Unit \ Assert = assertEq(expected = -0i32, Int32.bitwiseNot(-1i32))

    @Test
    def bitwiseNot04(): Unit \ Assert = assertEq(expected = -128i32, Int32.bitwiseNot(127i32))

    @Test
    def bitwiseNot05(): Unit \ Assert = assertEq(expected = 127i32, Int32.bitwiseNot(-128i32))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseOr(0i32, 0i32))

    @Test
    def bitwiseOr02(): Unit \ Assert = assertEq(expected = 127i32, Int32.bitwiseOr(127i32, 0i32))

    @Test
    def bitwiseOr03(): Unit \ Assert = assertEq(expected = -128i32, Int32.bitwiseOr(-128i32, 0i32))

    @Test
    def bitwiseOr04(): Unit \ Assert = assertEq(expected = 1i32, Int32.bitwiseOr(0i32, 1i32))

    @Test
    def bitwiseOr05(): Unit \ Assert = assertEq(expected = 127i32, Int32.bitwiseOr(127i32, 1i32))

    @Test
    def bitwiseOr06(): Unit \ Assert = assertEq(expected = -127i32, Int32.bitwiseOr(-128i32, 1i32))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Unit \ Assert = assertEq(expected = 0i32, Int32.bitwiseXor(0i32, 0i32))

    @Test
    def bitwiseXor02(): Unit \ Assert = assertEq(expected = 127i32, Int32.bitwiseXor(127i32, 0i32))

    @Test
    def bitwiseXor03(): Unit \ Assert = assertEq(expected = -128i32, Int32.bitwiseXor(-128i32, 0i32))

    @Test
    def bitwiseXor04(): Unit \ Assert = assertEq(expected = 1i32, Int32.bitwiseXor(0i32, 1i32))

    @Test
    def bitwiseXor05(): Unit \ Assert = assertEq(expected = 126i32, Int32.bitwiseXor(127i32, 1i32))

    @Test
    def bitwiseXor06(): Unit \ Assert = assertEq(expected = -127i32, Int32.bitwiseXor(-128i32, 1i32))

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Unit \ Assert = assertEq(expected = 1, Int32.factorial(0))

    @Test
    def factorial02(): Unit \ Assert = assertEq(expected = 1, Int32.factorial(1))

    @Test
    def factorial03(): Unit \ Assert = assertEq(expected = 2, Int32.factorial(2))

    @Test
    def factorial04(): Unit \ Assert = assertEq(expected = 6, Int32.factorial(3))

    @Test
    def factorial05(): Unit \ Assert = assertEq(expected = 24, Int32.factorial(4))

    @Test
    def factorial06(): Unit \ Assert = assertEq(expected = 120, Int32.factorial(5))

    @Test
    def factorial07(): Unit \ Assert = assertEq(expected = 3628800, Int32.factorial(10))

    @Test
    def factorial08(): Unit \ Assert = assertEq(expected = 0, Int32.factorial(-1))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0", Int32.toString(0))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1", Int32.toString(1))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1", Int32.toString(-1))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "2147483647", Int32.toString(2147483647))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-2147483648", Int32.toString(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromString01(): Unit \ Assert = assertNone(Int32.fromString("Bad"))

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1), Int32.fromString(" 1"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1), Int32.fromString("1 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0), Int32.fromString("0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1), Int32.fromString("1"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1), Int32.fromString("-1"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(2147483647), Int32.fromString("2147483647"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-2147483648), Int32.fromString("-2147483648"))

    /////////////////////////////////////////////////////////////////////////////
    // parse                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse01(): Unit \ Assert = assertEq(expected = Ok(0), "0" |> Int32.parse(10))

    @Test
    def parse02(): Unit \ Assert = assertEq(expected = Ok(0), "     0      " |> Int32.parse(10))

    @Test
    def parse03(): Unit \ Assert = assertEq(expected = Ok(-255), "-FF" |> Int32.parse(16))

    @Test
    def parse04(): Unit \ Assert = assertEq(expected = Ok(102), "1100110" |> Int32.parse(2))

    @Test
    def parse05(): Unit \ Assert = assertEq(expected = Ok(411787), "Kona" |> Int32.parse(27))

    @Test
    def parse06(): Unit \ Assert = assertTrue(Result.isErr("2147483648" |> Int32.parse(10)))

    @Test
    def parse07(): Unit \ Assert = assertTrue(Result.isErr("Kona" |> Int32.parse(10)))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), Int32.tryToInt8(0))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), Int32.tryToInt8(1))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), Int32.tryToInt8(-1))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), Int32.tryToInt8(127))

    @Test
    def tryToInt805(): Unit \ Assert = assertNone(Int32.tryToInt8(128))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), Int32.tryToInt8(-128))

    @Test
    def tryToInt807(): Unit \ Assert = assertNone(Int32.tryToInt8(-129))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tryToInt1601(): Unit \ Assert = assertEq(expected = Some(0i16), Int32.tryToInt16(0))

    @Test
    def tryToInt1602(): Unit \ Assert = assertEq(expected = Some(1i16), Int32.tryToInt16(1))

    @Test
    def tryToInt1603(): Unit \ Assert = assertEq(expected = Some(-1i16), Int32.tryToInt16(-1))

    @Test
    def tryToInt1604(): Unit \ Assert = assertEq(expected = Some(32767i16), Int32.tryToInt16(32767))

    @Test
    def tryToInt1605(): Unit \ Assert = assertNone(Int32.tryToInt16(32768))

    @Test
    def tryToInt1606(): Unit \ Assert = assertEq(expected = Some(-32768i16), Int32.tryToInt16(-32768))

    @Test
    def tryToInt1607(): Unit \ Assert = assertNone(Int32.tryToInt16(-32769))

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toInt6401(): Unit \ Assert = assertEq(expected = 0i64, Int32.toInt64(0))

    @Test
    def toInt6402(): Unit \ Assert = assertEq(expected = 1i64, Int32.toInt64(1))

    @Test
    def toInt6403(): Unit \ Assert = assertEq(expected = -1i64, Int32.toInt64(-1))

    @Test
    def toInt6404(): Unit \ Assert = assertEq(expected = 2147483647i64, Int32.toInt64(2147483647))

    @Test
    def toInt6405(): Unit \ Assert = assertEq(expected = -2147483648i64, Int32.toInt64(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toBigInt01(): Unit \ Assert = assertEq(expected = 0ii, Int32.toBigInt(0))

    @Test
    def toBigInt02(): Unit \ Assert = assertEq(expected = 1ii, Int32.toBigInt(1))

    @Test
    def toBigInt03(): Unit \ Assert = assertEq(expected = -1ii, Int32.toBigInt(-1))

    @Test
    def toBigInt04(): Unit \ Assert = assertEq(expected = 2147483647ii, Int32.toBigInt(2147483647))

    @Test
    def toBigInt05(): Unit \ Assert = assertEq(expected = -2147483648ii, Int32.toBigInt(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toFloat3201(): Unit \ Assert = assertEq(expected = 0.0f32, Int32.toFloat32(0))

    @Test
    def toFloat3202(): Unit \ Assert = assertEq(expected = 1.0f32, Int32.toFloat32(1))

    @Test
    def toFloat3203(): Unit \ Assert = assertEq(expected = -1.0f32, Int32.toFloat32(-1))

    @Test
    def toFloat3204(): Unit \ Assert = assertEq(expected = 2147483647.0f32, Int32.toFloat32(2147483647))

    @Test
    def toFloat3205(): Unit \ Assert = assertEq(expected = -2147483648.0f32, Int32.toFloat32(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, Int32.toFloat64(0))

    @Test
    def toFloat6402(): Unit \ Assert = assertEq(expected = 1.0f64, Int32.toFloat64(1))

    @Test
    def toFloat6403(): Unit \ Assert = assertEq(expected = -1.0f64, Int32.toFloat64(-1))

    @Test
    def toFloat6404(): Unit \ Assert = assertEq(expected = 2147483647.0f64, Int32.toFloat64(2147483647))

    @Test
    def toFloat6405(): Unit \ Assert = assertEq(expected = -2147483648.0f64, Int32.toFloat64(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toBigDecimal01(): Unit \ Assert = assertEq(expected = 0.0ff, Int32.toBigDecimal(0))

    @Test
    def toBigDecimal02(): Unit \ Assert = assertEq(expected = 1.0ff, Int32.toBigDecimal(1))

    @Test
    def toBigDecimal03(): Unit \ Assert = assertEq(expected = -1.0ff, Int32.toBigDecimal(-1))

    @Test
    def toBigDecimal04(): Unit \ Assert = assertEq(expected = 2147483647.0ff, Int32.toBigDecimal(2147483647))

    @Test
    def toBigDecimal05(): Unit \ Assert = assertEq(expected = -2147483648.0ff, Int32.toBigDecimal(-2147483648))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clampToInt801(): Unit \ Assert = assertEq(expected = 0i8, Int32.clampToInt8(min = -100i8, max = 100i8, 0))

    @Test
    def clampToInt802(): Unit \ Assert = assertEq(expected = 100i8, Int32.clampToInt8(min = -100i8, max = 100i8, 100))

    @Test
    def clampToInt803(): Unit \ Assert = assertEq(expected = 100i8, Int32.clampToInt8(min = -100i8, max = 100i8, 101))

    @Test
    def clampToInt804(): Unit \ Assert = assertEq(expected = -100i8, Int32.clampToInt8(min = -100i8, max = 100i8, -100))

    @Test
    def clampToInt805(): Unit \ Assert = assertEq(expected = -100i8, Int32.clampToInt8(min = -100i8, max = 100i8, -101))

    @Test
    def clampToInt806(): Unit \ Assert = assertEq(expected = 100i8, Int32.clampToInt8(min = -100i8, max = 100i8, Int32.maxValue()))

    @Test
    def clampToInt807(): Unit \ Assert = assertEq(expected = -100i8, Int32.clampToInt8(min = -100i8, max = 100i8, Int32.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clampToInt1601(): Unit \ Assert = assertEq(expected = 0i16, Int32.clampToInt16(min = -100i16, max = 100i16, 0))

    @Test
    def clampToInt1602(): Unit \ Assert = assertEq(expected = 100i16, Int32.clampToInt16(min = -100i16, max = 100i16, 100))

    @Test
    def clampToInt1603(): Unit \ Assert = assertEq(expected = 100i16, Int32.clampToInt16(min = -100i16, max = 100i16, 101))

    @Test
    def clampToInt1604(): Unit \ Assert = assertEq(expected = -100i16, Int32.clampToInt16(min = -100i16, max = 100i16, -100))

    @Test
    def clampToInt1605(): Unit \ Assert = assertEq(expected = -100i16, Int32.clampToInt16(min = -100i16, max = 100i16, -101))

    @Test
    def clampToInt1606(): Unit \ Assert = assertEq(expected = 100i16, Int32.clampToInt16(min = -100i16, max = 100i16, Int32.maxValue()))

    @Test
    def clampToInt1607(): Unit \ Assert = assertEq(expected = -100i16, Int32.clampToInt16(min = -100i16, max = 100i16, Int32.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // intValue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intValue01(): Unit \ Assert =
        let i = Int32.valueOf(0);
        assertEq(expected = 0, Int32.intValue(i))

    @Test
    def intValue02(): Unit \ Assert =
        let i = Int32.valueOf(1);
        assertEq(expected = 1, Int32.intValue(i))

    @Test
    def intValue03(): Unit \ Assert =
        let i = Int32.valueOf(-1);
        assertEq(expected = -1, Int32.intValue(i))

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Unit \ Assert =
        let i = Int32.valueOf(0);
        assertTrue(i.equals(Int32.valueOf(0)))

    @Test
    def valueOf02(): Unit \ Assert =
        let i = Int32.valueOf(1);
        assertTrue(i.equals(Int32.valueOf(1)))

    @Test
    def valueOf03(): Unit \ Assert =
        let i = Int32.valueOf(-1);
        assertTrue(i.equals(Int32.valueOf(-1)))

}

