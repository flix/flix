mod TestInt64 {

    use Assert.{assertEq, assertTrue};
    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert = assertEq(expected = 0i64, mul(1i64, 0i64))

    @Test
    def mul02(): Unit \ Assert = assertEq(expected = 0i64, mul(0i64, 1i64))

    @Test
    def mul03(): Unit \ Assert = assertEq(expected = 0i64, mul(0i64, 0i64))

    @Test
    def mul04(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, mul(-32i64, 288230376151711744i64))

    @Test
    def mul05(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, mul(6223i64, 1482142380982609i64))

    @Test
    def mul06(): Unit \ Assert = assertEq(expected = 55i64, mul(5i64, 11i64))

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Unit \ Assert = assertEq(expected = Int64.bitwiseNot(Int64.leftShift(1i64, 63)), Int64.maxValue())

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.minValue())

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = assertEq(expected = 64, Int64.size())

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1i64, Int64.min(1i64, 2i64))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1i64, Int64.min(2i64, -1i64))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66i64, Int64.min(-33i64, -66i64))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49i64, Int64.max(48i64, 49i64))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4i64, Int64.max(4i64, -16i64))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16i64, Int64.max(-34i64, -16i64))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 63i64, Int64.abs(63i64))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 63i64, Int64.abs(-63i64))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, Int64.abs(9223372036854775807i64))

    @Test
    def abs04(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, Int64.abs(-9223372036854775807i64))

    @Test
    def abs05(): Unit \ Assert = assertEq(expected = 0i64, Int64.abs(0i64))

    @Test
    def abs06(): Unit \ Assert = assertEq(expected = -1i64, Int64.abs(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def modulo01(): Unit \ Assert = assertEq(expected = 0i64, (120000000000i64 `Int64.modulo` 20000000000i64))

    @Test
    def modulo02(): Unit \ Assert = assertEq(expected = 20000000000i64, (120000000000i64 `Int64.modulo` 50000000000i64))

    @Test
    def modulo03(): Unit \ Assert = assertEq(expected = 30000000000i64, (-120000000000i64 `Int64.modulo` 50000000000i64))

    @Test
    def modulo04(): Unit \ Assert = assertEq(expected = -30000000000i64, (120000000000i64 `Int64.modulo` -50000000000i64))

    @Test
    def modulo05(): Unit \ Assert = assertEq(expected = -20000000000i64, (-120000000000i64 `Int64.modulo` -50000000000i64))

    @Test
    def modulo06(): Unit \ Assert = assertEq(expected = 0i64, (1i64 `Int64.modulo` 0i64))

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def remainder01(): Unit \ Assert = assertEq(expected = 0i64, (120000000000i64 `Int64.remainder` 20000000000i64))

    @Test
    def remainder02(): Unit \ Assert = assertEq(expected = 20000000000i64, (120000000000i64 `Int64.remainder` 50000000000i64))

    @Test
    def remainder03(): Unit \ Assert = assertEq(expected = -20000000000i64, (-120000000000i64 `Int64.remainder` 50000000000i64))

    @Test
    def remainder04(): Unit \ Assert = assertEq(expected = 20000000000i64, (120000000000i64 `Int64.remainder` -50000000000i64))

    @Test
    def remainder05(): Unit \ Assert = assertEq(expected = -20000000000i64, (-120000000000i64 `Int64.remainder` -50000000000i64))

    @Test
    def remainder06(): Unit \ Assert = assertEq(expected = 0i64, (1i64 `Int64.remainder` 0i64))

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 38i64, Int64.dist(31i64, -7i64))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 43i64, Int64.dist(-44i64, -1i64))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0i64, Int64.dist(-2i64, -2i64))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, Int64.dist(-9223372036854775808i64, -1i64))

    @Test
    def dist05(): Unit \ Assert = assertEq(expected = -1i64, Int64.dist(-9223372036854775808i64, 0i64))

    @Test
    def dist06(): Unit \ Assert = assertEq(expected = 127i64, Int64.dist(-123i64, 4i64))

    @Test
    def dist07(): Unit \ Assert = assertEq(expected = -1i64, Int64.dist(-9223372036854775803i64, 5i64))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Unit \ Assert = assertEq(expected = -1, Int64.compare(-1i64, 44i64))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, Int64.compare(-1i64, -44i64))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, Int64.compare(88i64, 88i64))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Unit \ Assert = assertEq(expected = -1, Int64.signum(-22i64))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, Int64.signum(22i64))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, Int64.signum(0i64))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1i64, Int64.pow(base = 0i64, 0i64))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0i64, Int64.pow(base = 0i64, 1i64))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1i64, Int64.pow(base = 1i64, 0i64))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1i64, Int64.pow(base = 1i64, 1i64))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25i64, Int64.pow(base = 5i64, 2i64))

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Unit \ Assert = assertEq(expected = 8i64, Int64.leftShift(8i64, 0))

    @Test
    def testBitwiseLeftShift02(): Unit \ Assert = assertEq(expected = 34359738368i64, Int64.leftShift(8i64, 32))

    @Test
    def testBitwiseLeftShift03(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, Int64.leftShift(8i64, 60))

    @Test
    def testBitwiseLeftShift04(): Unit \ Assert = assertEq(expected = 0i64, Int64.leftShift(8i64, 61))

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift01(): Unit \ Assert = assertEq(expected = 12000000000i64, Int64.rightShift(12000000000i64, 0))

    @Test
    def testBitwiseRightShift02(): Unit \ Assert = assertEq(expected = 3000000000i64, Int64.rightShift(12000000000i64, 2))

    @Test
    def testBitwiseRightShift03(): Unit \ Assert = assertEq(expected = 0i64, Int64.rightShift(12000000000i64, 63))

    @Test
    def testBitwiseRightShift04(): Unit \ Assert = assertEq(expected = -3000000000i64, Int64.rightShift(-12000000000i64, 2))

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def logicalRightShift01(): Unit \ Assert = assertEq(expected = Int64.rightShift(2233i64, 4), Int64.logicalRightShift(dist = 4, 2233i64))

    @Test
    def logicalRightShift02(): Unit \ Assert = assertEq(expected = Int64.rightShift(22336688888i64, 33), Int64.logicalRightShift(dist = 33, 22336688888i64))

    @Test
    def logicalRightShift03(): Unit \ Assert = assertEq(expected = Int64.rightShift(2233i64, -8), Int64.logicalRightShift(dist = -8, 2233i64))

    @Test
    def logicalRightShift04(): Unit \ Assert = assertEq(expected = Int64.rightShift(2233i64, 4), Int64.logicalRightShift(dist = 4, 2233i64))

    @Test
    def logicalRightShift05(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, Int64.logicalRightShift(dist = 1, -1i64))

    @Test
    def logicalRightShift06(): Unit \ Assert = assertEq(expected = 1152921504606846973i64, Int64.logicalRightShift(dist = 4, -35i64))

    @Test
    def logicalRightShift07(): Unit \ Assert = assertEq(expected = 33554431i64, Int64.logicalRightShift(dist = -2777, -2777i64))

    @Test
    def logicalRightShift08(): Unit \ Assert = assertEq(expected = -23328384858i64, Int64.logicalRightShift(dist = 64, -23328384858i64))

    @Test
    def logicalRightShift09(): Unit \ Assert = assertEq(expected = -27000000000000i64, Int64.logicalRightShift(dist = 0, -27000000000000i64))

    @Test
    def logicalRightShift10(): Unit \ Assert = assertEq(expected = 4611686018427387903i64, Int64.logicalRightShift(dist = 1, 9223372036854775807i64))

    @Test
    def logicalRightShift11(): Unit \ Assert = assertEq(expected = 4611686018427387904i64, Int64.logicalRightShift(dist = 1, -9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bitCount01(): Unit \ Assert = assertEq(expected = 25, Int64.bitCount(1990911889900134i64))

    @Test
    def bitCount02(): Unit \ Assert = assertEq(expected = 64, Int64.bitCount(-1i64))

    @Test
    def bitCount03(): Unit \ Assert = assertEq(expected = 35, Int64.bitCount(-999888888114322199i64))

    @Test
    def bitCount04(): Unit \ Assert = assertEq(expected = 0, Int64.bitCount(0i64))

    @Test
    def bitCount05(): Unit \ Assert = assertEq(expected = 1, Int64.bitCount(-9223372036854775808i64))

    @Test
    def bitCount06(): Unit \ Assert = assertEq(expected = 3, Int64.bitCount(-9223372036854775805i64))

    @Test
    def bitCount07(): Unit \ Assert = assertEq(expected = 63, Int64.bitCount(9223372036854775807i64))

    @Test
    def bitCount08(): Unit \ Assert = assertEq(expected = 62, Int64.bitCount(9223372036854775806i64))

    @Test
    def bitCount09(): Unit \ Assert = assertEq(expected = 62, Int64.bitCount(9223372036854775791i64))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateLeft01(): Unit \ Assert = assertEq(expected = 5647895911756442879i64, Int64.rotateLeft(dist = 7, -99991001265258663i64))

    @Test
    def rotateLeft02(): Unit \ Assert = assertEq(expected = 1943538110990123007i64, Int64.rotateLeft(dist = 24, -983667729064i64))

    @Test
    def rotateLeft03(): Unit \ Assert = assertEq(expected = 1943538110990123007i64, Int64.rotateLeft(dist = -104, -983667729064i64))

    @Test
    def rotateLeft04(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateLeft(dist = 23, 1478293001i64))

    @Test
    def rotateLeft05(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateLeft(dist = -41, 1478293001i64))

    @Test
    def rotateLeft06(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateLeft(dist = 87, 1478293001i64))

    @Test
    def rotateLeft07(): Unit \ Assert = assertEq(expected = 0i64, Int64.rotateLeft(dist = 87, 0i64))

    @Test
    def rotateLeft08(): Unit \ Assert = assertEq(expected = -1i64, Int64.rotateLeft(dist = 87, -1i64))

    @Test
    def rotateLeft09(): Unit \ Assert = assertEq(expected = 4i64, Int64.rotateLeft(dist = 3, -9223372036854775808i64))

    @Test
    def rotateLeft10(): Unit \ Assert = assertEq(expected = -3i64, Int64.rotateLeft(dist = 2, 9223372036854775807i64))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = 5647895911756442879i64, Int64.rotateRight(dist = 57, -99991001265258663i64))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = 1943538110990123007i64, Int64.rotateRight(dist = 40, -983667729064i64))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = 1943538110990123007i64, Int64.rotateRight(dist = -88, -983667729064i64))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateRight(dist = 41, 1478293001i64))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateRight(dist = -23, 1478293001i64))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = 12400820494532608i64, Int64.rotateRight(dist = 105, 1478293001i64))

    @Test
    def rotateRight07(): Unit \ Assert = assertEq(expected = 0i64, Int64.rotateRight(dist = 87, 0i64))

    @Test
    def rotateRight08(): Unit \ Assert = assertEq(expected = -1i64, Int64.rotateRight(dist = 87, -1i64))

    @Test
    def rotateRight09(): Unit \ Assert = assertEq(expected = 1152921504606846976i64, Int64.rotateRight(dist = 3, -9223372036854775808i64))

    @Test
    def rotateRight10(): Unit \ Assert = assertEq(expected = -2305843009213693953i64, Int64.rotateRight(dist = 2, 9223372036854775807i64))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = 6149336169419208034i64, Int64.reverse(5093163785157044906i64))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = 5093163785157044906i64, Int64.reverse(6149336169419208034i64))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = 6245943384048626350i64, Int64.reverse(8455181377513485674i64))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = 8455181377513485674i64, Int64.reverse(6245943384048626350i64))

    @Test
    def reverse05(): Unit \ Assert = assertEq(expected = 1i64, Int64.reverse(-9223372036854775808i64))

    @Test
    def reverse06(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, Int64.reverse(1i64))

    @Test
    def reverse07(): Unit \ Assert = assertEq(expected = 2305843009213693952i64, Int64.reverse(4i64))

    @Test
    def reverse08(): Unit \ Assert = assertEq(expected = 4i64, Int64.reverse(2305843009213693952i64))

    @Test
    def reverse09(): Unit \ Assert = assertEq(expected = -1i64, Int64.reverse(-1i64))

    @Test
    def reverse10(): Unit \ Assert = assertEq(expected = 0i64, Int64.reverse(0i64))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBitPosition01(): Unit \ Assert = assertEq(expected = 63, Int64.highestOneBitPosition(-1i64))

    @Test
    def highestOneBitPosition02(): Unit \ Assert = assertEq(expected = 63, Int64.highestOneBitPosition(-9223372036854775808i64))

    @Test
    def highestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int64.highestOneBitPosition(1i64))

    @Test
    def highestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int64.highestOneBitPosition(2i64))

    @Test
    def highestOneBitPosition05(): Unit \ Assert = assertEq(expected = 1, Int64.highestOneBitPosition(3i64))

    @Test
    def highestOneBitPosition06(): Unit \ Assert = assertEq(expected = 4, Int64.highestOneBitPosition(20i64))

    @Test
    def highestOneBitPosition07(): Unit \ Assert = assertEq(expected = 30, Int64.highestOneBitPosition(Int64.pow(base = 2i64, 30i64)))

    @Test
    def highestOneBitPosition08(): Unit \ Assert = assertEq(expected = 62, Int64.highestOneBitPosition(9223372036854775807i64))

    @Test
    def highestOneBitPosition09(): Unit \ Assert = assertEq(expected = -1, Int64.highestOneBitPosition(0i64))

    @Test
    def highestOneBitPosition10(): Unit \ Assert = assertEq(expected = 60, Int64.highestOneBitPosition(Int64.pow(base = 2i64, 23i64) + Int64.pow(base = 2i64, 60i64)))

    @Test
    def highestOneBitPosition11(): Unit \ Assert = assertEq(expected = 59, Int64.highestOneBitPosition(Int64.pow(base = 2i64, 20i64) + Int64.pow(base = 2i64, 59i64)))

    @Test
    def highestOneBitPosition12(): Unit \ Assert = assertEq(expected = 63, Int64.highestOneBitPosition(-44556677i64))

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def highestOneBit01(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.highestOneBit(-1i64))

    @Test
    def highestOneBit02(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.highestOneBit(-9223372036854775808i64))

    @Test
    def highestOneBit03(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.highestOneBit(-778899i64))

    @Test
    def highestOneBit04(): Unit \ Assert = assertEq(expected = 2i64, Int64.highestOneBit(2i64))

    @Test
    def highestOneBit05(): Unit \ Assert = assertEq(expected = 2i64, Int64.highestOneBit(3i64))

    @Test
    def highestOneBit06(): Unit \ Assert = assertEq(expected = 16i64, Int64.highestOneBit(20i64))

    @Test
    def highestOneBit07(): Unit \ Assert = assertEq(expected = 144115188075855872i64, Int64.highestOneBit(Int64.pow(base = 2i64, 57i64) + Int64.pow(base = 2i64, 32i64)))

    @Test
    def highestOneBit08(): Unit \ Assert = assertEq(expected = 4611686018427387904i64, Int64.highestOneBit(9223372036854775807i64))

    @Test
    def highestOneBit09(): Unit \ Assert = assertEq(expected = 0i64, Int64.highestOneBit(0i64))

    @Test
    def highestOneBit10(): Unit \ Assert = assertEq(expected = 17592186044416i64, Int64.highestOneBit(Int64.pow(base = 2i64, 44i64) + Int64.pow(base = 2i64, 18i64)))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfLeadingZeros01(): Unit \ Assert = assertEq(expected = 64, Int64.numberOfLeadingZeros(0i64))

    @Test
    def numberOfLeadingZeros02(): Unit \ Assert = assertEq(expected = 0, Int64.numberOfLeadingZeros(-9223372036854775808i64))

    @Test
    def numberOfLeadingZeros03(): Unit \ Assert = assertEq(expected = 0, Int64.numberOfLeadingZeros(-1i64))

    @Test
    def numberOfLeadingZeros04(): Unit \ Assert = assertEq(expected = 1, Int64.numberOfLeadingZeros(Int64.bitwiseNot(Int64.leftShift(1i64, 63))))

    @Test
    def numberOfLeadingZeros05(): Unit \ Assert = assertEq(expected = 1, Int64.numberOfLeadingZeros(Int64.pow(base = 2i64, 62i64)))

    @Test
    def numberOfLeadingZeros06(): Unit \ Assert = assertEq(expected = 2, Int64.numberOfLeadingZeros(Int64.pow(base = 2i64, 61i64) + Int64.pow(base = 2i64, 18i64)))

    @Test
    def numberOfLeadingZeros07(): Unit \ Assert = assertEq(expected = 43, Int64.numberOfLeadingZeros(Int64.pow(base = 2i64, 20i64) + Int64.pow(base = 2i64, 18i64)))

    @Test
    def numberOfLeadingZeros08(): Unit \ Assert = assertEq(expected = 59, Int64.numberOfLeadingZeros(20i64))

    @Test
    def numberOfLeadingZeros09(): Unit \ Assert = assertEq(expected = 63, Int64.numberOfLeadingZeros(1i64))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBitPosition01(): Unit \ Assert = assertEq(expected = 0, Int64.lowestOneBitPosition(-1i64))

    @Test
    def lowestOneBitPosition02(): Unit \ Assert = assertEq(expected = 63, Int64.lowestOneBitPosition(-9223372036854775808i64))

    @Test
    def lowestOneBitPosition03(): Unit \ Assert = assertEq(expected = 0, Int64.lowestOneBitPosition(1i64))

    @Test
    def lowestOneBitPosition04(): Unit \ Assert = assertEq(expected = 1, Int64.lowestOneBitPosition(2i64))

    @Test
    def lowestOneBitPosition05(): Unit \ Assert = assertEq(expected = 0, Int64.lowestOneBitPosition(3i64))

    @Test
    def lowestOneBitPosition06(): Unit \ Assert = assertEq(expected = 2, Int64.lowestOneBitPosition(4i64))

    @Test
    def lowestOneBitPosition07(): Unit \ Assert = assertEq(expected = 2, Int64.lowestOneBitPosition(12i64))

    @Test
    def lowestOneBitPosition08(): Unit \ Assert = assertEq(expected = 30, Int64.lowestOneBitPosition(Int64.pow(base = 2i64, 30i64)))

    @Test
    def lowestOneBitPosition09(): Unit \ Assert = assertEq(expected = 18, Int64.lowestOneBitPosition(Int64.pow(base = 2i64, 57i64) + Int64.pow(base = 2i64, 18i64)))

    @Test
    def lowestOneBitPosition10(): Unit \ Assert = assertEq(expected = 0, Int64.lowestOneBitPosition(9223372036854775807i64))

    @Test
    def lowestOneBitPosition11(): Unit \ Assert = assertEq(expected = -1, Int64.lowestOneBitPosition(0i64))

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def lowestOneBit01(): Unit \ Assert = assertEq(expected = 1i64, Int64.lowestOneBit(-1i64))

    @Test
    def lowestOneBit02(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.lowestOneBit(-9223372036854775808i64))

    @Test
    def lowestOneBit03(): Unit \ Assert = assertEq(expected = 1i64, Int64.lowestOneBit(1i64))

    @Test
    def lowestOneBit04(): Unit \ Assert = assertEq(expected = 2i64, Int64.lowestOneBit(2i64))

    @Test
    def lowestOneBit05(): Unit \ Assert = assertEq(expected = 2i64, Int64.lowestOneBit(6i64))

    @Test
    def lowestOneBit06(): Unit \ Assert = assertEq(expected = 4i64, Int64.lowestOneBit(12i64))

    @Test
    def lowestOneBit07(): Unit \ Assert = assertEq(expected = 64i64, Int64.lowestOneBit(Int64.pow(base = 2i64, 6i64) + Int64.pow(base = 2i64, 18i64) + Int64.pow(base = 2i64, 27i64)))

    @Test
    def lowestOneBit08(): Unit \ Assert = assertEq(expected = 18014398509481984i64, Int64.lowestOneBit(Int64.pow(base = 2i64, 54i64)))

    @Test
    def lowestOneBit09(): Unit \ Assert = assertEq(expected = 262144i64, Int64.lowestOneBit(Int64.pow(base = 2i64, 62i64) + Int64.pow(base = 2i64, 18i64)))

    @Test
    def lowestOneBit10(): Unit \ Assert = assertEq(expected = 1i64, Int64.lowestOneBit(9223372036854775807i64))

    @Test
    def lowestOneBit11(): Unit \ Assert = assertEq(expected = 0i64, Int64.lowestOneBit(0i64))

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numberOfTrailingZeros01(): Unit \ Assert = assertEq(expected = 64, Int64.numberOfTrailingZeros(0i64))

    @Test
    def numberOfTrailingZeros02(): Unit \ Assert = assertEq(expected = 0, Int64.numberOfTrailingZeros(3i64))

    @Test
    def numberOfTrailingZeros03(): Unit \ Assert = assertEq(expected = 1, Int64.numberOfTrailingZeros(42i64))

    @Test
    def numberOfTrailingZeros04(): Unit \ Assert = assertEq(expected = 16, Int64.numberOfTrailingZeros(Int64.pow(base = 2i64, 16i64) + Int64.pow(base = 2i64, 22i64)))

    @Test
    def numberOfTrailingZeros05(): Unit \ Assert = assertEq(expected = 57, Int64.numberOfTrailingZeros(Int64.pow(base = 2i64, 57i64)))

    @Test
    def numberOfTrailingZeros06(): Unit \ Assert = assertEq(expected = 0, Int64.numberOfTrailingZeros(-1i64))

    @Test
    def numberOfTrailingZeros07(): Unit \ Assert = assertEq(expected = 63, Int64.numberOfTrailingZeros(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Unit \ Assert = assertEq(expected = 1, Int64.getBit(pos = 5, -1i64))

    @Test
    def getBit02(): Unit \ Assert = assertEq(expected = 1, Int64.getBit(pos = 63, -1000000i64))

    @Test
    def getBit03(): Unit \ Assert = assertEq(expected = 1, Int64.getBit(pos = 59, Int64.pow(base = 2i64, 59i64)))

    @Test
    def getBit04(): Unit \ Assert = assertEq(expected = 0, Int64.getBit(pos = 60, Int64.pow(base = 2i64, 59i64)))

    @Test
    def getBit05(): Unit \ Assert = assertEq(expected = 0, Int64.getBit(pos = 58, Int64.pow(base = 2i64, 59i64)))

    @Test
    def getBit06(): Unit \ Assert = assertEq(expected = 0, Int64.getBit(pos = 61, -1i64 - Int64.pow(base = 2i64, 61i64)))

    @Test
    def getBit07(): Unit \ Assert = assertEq(expected = 1, Int64.getBit(pos = 60, -1i64 - Int64.pow(base = 2i64, 61i64)))

    @Test
    def getBit08(): Unit \ Assert = assertEq(expected = 0, Int64.getBit(pos = 55, 0i64))

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Unit \ Assert = assertEq(expected = 1i64, Int64.setBit(pos = 0, 0i64))

    @Test
    def setBit02(): Unit \ Assert = assertEq(expected = 2i64, Int64.setBit(pos = 1, 0i64))

    @Test
    def setBit03(): Unit \ Assert = assertEq(expected = 2147483648i64, Int64.setBit(pos = 31, 0i64))

    @Test
    def setBit04(): Unit \ Assert = assertEq(expected = 4294967296i64, Int64.setBit(pos = 32, 0i64))

    @Test
    def setBit05(): Unit \ Assert = assertEq(expected = -1418638422666i64, Int64.setBit(pos = 31, -1418638422666i64))

    @Test
    def setBit07(): Unit \ Assert = assertEq(expected = -1i64, Int64.setBit(pos = 4, -17i64))

    @Test
    def setBit08(): Unit \ Assert = assertEq(expected = -1i64, Int64.setBit(pos = 10, -1025i64))

    @Test
    def setBit09(): Unit \ Assert = assertEq(expected = -17i64, Int64.setBit(pos = 63, -17i64))

    @Test
    def setBit10(): Unit \ Assert = assertEq(expected = -17i64, Int64.setBit(pos = 30, -17i64))

    @Test
    def setBit11(): Unit \ Assert = assertEq(expected = Int64.leftShift(1i64, 63), Int64.setBit(pos = 63, 0i64))

    @Test
    def setBit12(): Unit \ Assert = assertEq(expected = 1i64, Int64.setBit(pos = 64, 0i64))

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Unit \ Assert = assertEq(expected = 0i64, Int64.clearBit(pos = 0, 1i64))

    @Test
    def clearBit02(): Unit \ Assert = assertEq(expected = 1i64, Int64.clearBit(pos = 1, 1i64))

    @Test
    def clearBit03(): Unit \ Assert = assertEq(expected = 0i64, Int64.clearBit(pos = 1, 2i64))

    @Test
    def clearBit04(): Unit \ Assert = assertEq(expected = 2i64, Int64.clearBit(pos = 2, 2i64))

    @Test
    def clearBit05(): Unit \ Assert = assertEq(expected = 0i64, Int64.clearBit(pos = 31, 0i64))

    @Test
    def clearBit06(): Unit \ Assert = assertEq(expected = 0i64, Int64.clearBit(pos = 30, 0i64))

    @Test
    def clearBit07(): Unit \ Assert = assertEq(expected = 0i64, Int64.clearBit(pos = 63, Int64.leftShift(1i64, 63)))

    @Test
    def clearBit08(): Unit \ Assert = assertEq(expected = 308714120i64, Int64.clearBit(pos = 27, 442931848i64))

    @Test
    def clearBit09(): Unit \ Assert = assertEq(expected = -2i64, Int64.clearBit(pos = 0, -1i64))

    @Test
    def clearBit10(): Unit \ Assert = assertEq(expected = -3i64, Int64.clearBit(pos = 1, -1i64))

    @Test
    def clearBit11(): Unit \ Assert = assertEq(expected = -33i64, Int64.clearBit(pos = 5, -1i64))

    @Test
    def clearBit12(): Unit \ Assert = assertEq(expected = Int64.bitwiseNot(Int64.leftShift(1i64, 63)), Int64.clearBit(pos = 63, -1i64))

    @Test
    def clearBit13(): Unit \ Assert = assertEq(expected = -2i64, Int64.clearBit(pos = 64, -1i64))

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Unit \ Assert = assertEq(expected = 0i64, Int64.flipBit(pos = 0, 1i64))

    @Test
    def flipBit02(): Unit \ Assert = assertEq(expected = 3i64, Int64.flipBit(pos = 1, 1i64))

    @Test
    def flipBit03(): Unit \ Assert = assertEq(expected = 0i64, Int64.flipBit(pos = 1, 2i64))

    @Test
    def flipBit04(): Unit \ Assert = assertEq(expected = 6i64, Int64.flipBit(pos = 2, 2i64))

    @Test
    def flipBit05(): Unit \ Assert = assertEq(expected = 2147483648i64, Int64.flipBit(pos = 31, 0i64))

    @Test
    def flipBit06(): Unit \ Assert = assertEq(expected = 1073741824i64, Int64.flipBit(pos = 30, 0i64))

    @Test
    def flipBit07(): Unit \ Assert = assertEq(expected = 0i64, Int64.flipBit(pos = 63, Int64.leftShift(1i64, 63)))

    @Test
    def flipBit08(): Unit \ Assert = assertEq(expected = -2i64, Int64.flipBit(pos = 0, -1i64))

    @Test
    def flipBit9(): Unit \ Assert = assertEq(expected = -3i64, Int64.flipBit(pos = 1, -1i64))

    @Test
    def flipBit10(): Unit \ Assert = assertEq(expected = -33i64, Int64.flipBit(pos = 5, -1i64))

    @Test
    def flipBit11(): Unit \ Assert = assertEq(expected = (-1i64-Int64.leftShift(1i64, 31)), Int64.flipBit(pos = 31, -1i64))

    @Test
    def flipBit12(): Unit \ Assert = assertEq(expected = -2i64, Int64.flipBit(pos = 64, -1i64))

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def log201(): Unit \ Assert = assertEq(expected = 3i64, Int64.log2(10i64))

    @Test
    def log202(): Unit \ Assert = assertEq(expected = 13i64, Int64.log2(10000i64))

    @Test
    def log203(): Unit \ Assert = assertEq(expected = 3i64, Int64.log2(8i64))

    @Test
    def log204(): Unit \ Assert = assertEq(expected = 8i64, Int64.log2(256i64))

    @Test
    def log205(): Unit \ Assert = assertEq(expected = 0i64, Int64.log2(1i64))

    @Test
    def log206(): Unit \ Assert = assertEq(expected = 23i64, Int64.log2(12345678i64))

    @Test
    def log207(): Unit \ Assert = assertEq(expected = 4i64, Int64.log2(20i64))

    @Test
    def log208(): Unit \ Assert = assertEq(expected = 62i64, Int64.log2(9_223_372_036_854_775_807i64))

    @Test
    def log209(): Unit \ Assert = assertEq(expected = 0i64, Int64.log2(0i64))

    @Test
    def log210(): Unit \ Assert = assertEq(expected = 0i64, Int64.log2(-1i64))


    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseAnd(0i64, 0i64))

    @Test
    def bitwiseAnd02(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseAnd(127i64, 0i64))

    @Test
    def bitwiseAnd03(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseAnd(-128i64, 0i64))

    @Test
    def bitwiseAnd04(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseAnd(0i64, 1i64))

    @Test
    def bitwiseAnd05(): Unit \ Assert = assertEq(expected = 1i64, Int64.bitwiseAnd(127i64, 1i64))

    @Test
    def bitwiseAnd06(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseAnd(-128i64, 1i64))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Unit \ Assert = assertEq(expected = -1i64, Int64.bitwiseNot(0i64))

    @Test
    def bitwiseNot02(): Unit \ Assert = assertEq(expected = -2i64, Int64.bitwiseNot(1i64))

    @Test
    def bitwiseNot03(): Unit \ Assert = assertEq(expected = -0i64, Int64.bitwiseNot(-1i64))

    @Test
    def bitwiseNot04(): Unit \ Assert = assertEq(expected = -128i64, Int64.bitwiseNot(127i64))

    @Test
    def bitwiseNot05(): Unit \ Assert = assertEq(expected = 127i64, Int64.bitwiseNot(-128i64))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseOr(0i64, 0i64))

    @Test
    def bitwiseOr02(): Unit \ Assert = assertEq(expected = 127i64, Int64.bitwiseOr(127i64, 0i64))

    @Test
    def bitwiseOr03(): Unit \ Assert = assertEq(expected = -128i64, Int64.bitwiseOr(-128i64, 0i64))

    @Test
    def bitwiseOr04(): Unit \ Assert = assertEq(expected = 1i64, Int64.bitwiseOr(0i64, 1i64))

    @Test
    def bitwiseOr05(): Unit \ Assert = assertEq(expected = 127i64, Int64.bitwiseOr(127i64, 1i64))

    @Test
    def bitwiseOr06(): Unit \ Assert = assertEq(expected = -127i64, Int64.bitwiseOr(-128i64, 1i64))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Unit \ Assert = assertEq(expected = 0i64, Int64.bitwiseXor(0i64, 0i64))

    @Test
    def bitwiseXor02(): Unit \ Assert = assertEq(expected = 127i64, Int64.bitwiseXor(127i64, 0i64))

    @Test
    def bitwiseXor03(): Unit \ Assert = assertEq(expected = -128i64, Int64.bitwiseXor(-128i64, 0i64))

    @Test
    def bitwiseXor04(): Unit \ Assert = assertEq(expected = 1i64, Int64.bitwiseXor(0i64, 1i64))

    @Test
    def bitwiseXor05(): Unit \ Assert = assertEq(expected = 126i64, Int64.bitwiseXor(127i64, 1i64))

    @Test
    def bitwiseXor06(): Unit \ Assert = assertEq(expected = -127i64, Int64.bitwiseXor(-128i64, 1i64))

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def factorial01(): Unit \ Assert = assertEq(expected = 1i64, Int64.factorial(0i64))

    @Test
    def factorial02(): Unit \ Assert = assertEq(expected = 1i64, Int64.factorial(1i64))

    @Test
    def factorial03(): Unit \ Assert = assertEq(expected = 2i64, Int64.factorial(2i64))

    @Test
    def factorial04(): Unit \ Assert = assertEq(expected = 6i64, Int64.factorial(3i64))

    @Test
    def factorial05(): Unit \ Assert = assertEq(expected = 24i64, Int64.factorial(4i64))

    @Test
    def factorial06(): Unit \ Assert = assertEq(expected = 120i64, Int64.factorial(5i64))

    @Test
    def factorial07(): Unit \ Assert = assertEq(expected = 3628800i64, Int64.factorial(10i64))

    @Test
    def factorial08(): Unit \ Assert = assertEq(expected = 0i64, Int64.factorial(-1i64))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0", Int64.toString(0i64))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1", Int64.toString(1i64))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1", Int64.toString(-1i64))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "9223372036854775807", Int64.toString(9223372036854775807i64))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-9223372036854775808", Int64.toString(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertTrue(Int64.fromString("Bad") |> Option.isEmpty)

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1i64), Int64.fromString(" 1"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1i64), Int64.fromString("1 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0i64), Int64.fromString("0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1i64), Int64.fromString("1"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1i64), Int64.fromString("-1"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(9223372036854775807i64), Int64.fromString("9223372036854775807"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-9223372036854775808i64), Int64.fromString("-9223372036854775808"))

    /////////////////////////////////////////////////////////////////////////////
    // parse                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def parse01(): Unit \ Assert = assertEq(expected = Ok(0i64), ("0" |> Int64.parse(10)))

    @Test
    def parse02(): Unit \ Assert = assertEq(expected = Ok(0i64), ("     0      " |> Int64.parse(10)))

    @Test
    def parse03(): Unit \ Assert = assertEq(expected = Ok(-255i64), ("-FF" |> Int64.parse(16)))

    @Test
    def parse04(): Unit \ Assert = assertEq(expected = Ok(102i64), ("1100110" |> Int64.parse(2)))

    @Test
    def parse05(): Unit \ Assert = assertEq(expected = Ok(411787i64), ("Kona" |> Int64.parse(27)))

    @Test
    def parse06(): Unit \ Assert = assertTrue(("Kona" |> Int64.parse(10)) |> Result.isErr)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), Int64.tryToInt8(0i64))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), Int64.tryToInt8(1i64))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), Int64.tryToInt8(-1i64))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), Int64.tryToInt8(127i64))

    @Test
    def tryToInt805(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt8(128i64))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), Int64.tryToInt8(-128i64))

    @Test
    def tryToInt807(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt8(-129i64))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Unit \ Assert = assertEq(expected = Some(0i16), Int64.tryToInt16(0i64))

    @Test
    def tryToInt1602(): Unit \ Assert = assertEq(expected = Some(1i16), Int64.tryToInt16(1i64))

    @Test
    def tryToInt1603(): Unit \ Assert = assertEq(expected = Some(-1i16), Int64.tryToInt16(-1i64))

    @Test
    def tryToInt1604(): Unit \ Assert = assertEq(expected = Some(32767i16), Int64.tryToInt16(32767i64))

    @Test
    def tryToInt1605(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt16(32768i64))

    @Test
    def tryToInt1606(): Unit \ Assert = assertEq(expected = Some(-32768i16), Int64.tryToInt16(-32768i64))

    @Test
    def tryToInt1607(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt16(-32769i64))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Unit \ Assert = assertEq(expected = Some(0), Int64.tryToInt32(0i64))

    @Test
    def tryToInt3202(): Unit \ Assert = assertEq(expected = Some(1), Int64.tryToInt32(1i64))

    @Test
    def tryToInt3203(): Unit \ Assert = assertEq(expected = Some(-1), Int64.tryToInt32(-1i64))

    @Test
    def tryToInt3204(): Unit \ Assert = assertEq(expected = Some(2147483647), Int64.tryToInt32(2147483647i64))

    @Test
    def tryToInt3205(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt32(2147483648i64))

    @Test
    def tryToInt3206(): Unit \ Assert = assertEq(expected = Some(-2147483648), Int64.tryToInt32(-2147483648i64))

    @Test
    def tryToInt3207(): Unit \ Assert = assertEq(expected = None, Int64.tryToInt32(-2147483649i64))

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Unit \ Assert = assertEq(expected = 0ii, Int64.toBigInt(0i64))

    @Test
    def toBigInt02(): Unit \ Assert = assertEq(expected = 1ii, Int64.toBigInt(1i64))

    @Test
    def toBigInt03(): Unit \ Assert = assertEq(expected = -1ii, Int64.toBigInt(-1i64))

    @Test
    def toBigInt04(): Unit \ Assert = assertEq(expected = 9223372036854775807ii, Int64.toBigInt(9223372036854775807i64))

    @Test
    def toBigInt05(): Unit \ Assert = assertEq(expected = -9223372036854775808ii, Int64.toBigInt(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat3201(): Unit \ Assert = assertEq(expected = 0.0f32, Int64.toFloat32(0i64))

    @Test
    def toFloat3202(): Unit \ Assert = assertEq(expected = 1.0f32, Int64.toFloat32(1i64))

    @Test
    def toFloat3203(): Unit \ Assert = assertEq(expected = -1.0f32, Int64.toFloat32(-1i64))

    @Test
    def toFloat3204(): Unit \ Assert = assertEq(expected = 9223372036854775807.0f32, Int64.toFloat32(9223372036854775807i64))

    @Test
    def toFloat3205(): Unit \ Assert = assertEq(expected = -9223372036854775808.0f32, Int64.toFloat32(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, Int64.toFloat64(0i64))

    @Test
    def toFloat6402(): Unit \ Assert = assertEq(expected = 1.0f64, Int64.toFloat64(1i64))

    @Test
    def toFloat6403(): Unit \ Assert = assertEq(expected = -1.0f64, Int64.toFloat64(-1i64))

    @Test
    def toFloat6404(): Unit \ Assert = assertEq(expected = 9223372036854775807.0f64, Int64.toFloat64(9223372036854775807i64))

    @Test
    def toFloat6405(): Unit \ Assert = assertEq(expected = -9223372036854775808.0f64, Int64.toFloat64(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Unit \ Assert = assertEq(expected = 0.0ff, Int64.toBigDecimal(0i64))

    @Test
    def toBigDecimal02(): Unit \ Assert = assertEq(expected = 1.0ff, Int64.toBigDecimal(1i64))

    @Test
    def toBigDecimal03(): Unit \ Assert = assertEq(expected = -1.0ff, Int64.toBigDecimal(-1i64))

    @Test
    def toBigDecimal04(): Unit \ Assert = assertEq(expected = 9223372036854775807.0ff, Int64.toBigDecimal(9223372036854775807i64))

    @Test
    def toBigDecimal05(): Unit \ Assert = assertEq(expected = -9223372036854775808.0ff, Int64.toBigDecimal(-9223372036854775808i64))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Unit \ Assert = assertEq(expected = 0i8, Int64.clampToInt8(min = -100i8, max = 100i8, 0i64))

    @Test
    def clampToInt802(): Unit \ Assert = assertEq(expected = 100i8, Int64.clampToInt8(min = -100i8, max = 100i8, 100i64))

    @Test
    def clampToInt803(): Unit \ Assert = assertEq(expected = 100i8, Int64.clampToInt8(min = -100i8, max = 100i8, 101i64))

    @Test
    def clampToInt804(): Unit \ Assert = assertEq(expected = -100i8, Int64.clampToInt8(min = -100i8, max = 100i8, -100i64))

    @Test
    def clampToInt805(): Unit \ Assert = assertEq(expected = -100i8, Int64.clampToInt8(min = -100i8, max = 100i8, -101i64))

    @Test
    def clampToInt806(): Unit \ Assert = assertEq(expected = 100i8, Int64.clampToInt8(min = -100i8, max = 100i8, Int64.maxValue()))

    @Test
    def clampToInt807(): Unit \ Assert = assertEq(expected = -100i8, Int64.clampToInt8(min = -100i8, max = 100i8, Int64.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Unit \ Assert = assertEq(expected = 0i16, Int64.clampToInt16(min = -100i16, max = 100i16, 0i64))

    @Test
    def clampToInt1602(): Unit \ Assert = assertEq(expected = 100i16, Int64.clampToInt16(min = -100i16, max = 100i16, 100i64))

    @Test
    def clampToInt1603(): Unit \ Assert = assertEq(expected = 100i16, Int64.clampToInt16(min = -100i16, max = 100i16, 101i64))

    @Test
    def clampToInt1604(): Unit \ Assert = assertEq(expected = -100i16, Int64.clampToInt16(min = -100i16, max = 100i16, -100i64))

    @Test
    def clampToInt1605(): Unit \ Assert = assertEq(expected = -100i16, Int64.clampToInt16(min = -100i16, max = 100i16, -101i64))

    @Test
    def clampToInt1606(): Unit \ Assert = assertEq(expected = 100i16, Int64.clampToInt16(min = -100i16, max = 100i16, Int64.maxValue()))

    @Test
    def clampToInt1607(): Unit \ Assert = assertEq(expected = -100i16, Int64.clampToInt16(min = -100i16, max = 100i16, Int64.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt3201(): Unit \ Assert = assertEq(expected = 0, Int64.clampToInt32(min = -100, max = 100, 0i64))

    @Test
    def clampToInt3202(): Unit \ Assert = assertEq(expected = 100, Int64.clampToInt32(min = -100, max = 100, 100i64))

    @Test
    def clampToInt3203(): Unit \ Assert = assertEq(expected = 100, Int64.clampToInt32(min = -100, max = 100, 101i64))

    @Test
    def clampToInt3204(): Unit \ Assert = assertEq(expected = -100, Int64.clampToInt32(min = -100, max = 100, -100i64))

    @Test
    def clampToInt3205(): Unit \ Assert = assertEq(expected = -100, Int64.clampToInt32(min = -100, max = 100, -101i64))

    @Test
    def clampToInt3206(): Unit \ Assert = assertEq(expected = 100, Int64.clampToInt32(min = -100, max = 100, Int64.maxValue()))

    @Test
    def clampToInt3207(): Unit \ Assert = assertEq(expected = -100, Int64.clampToInt32(min = -100, max = 100, Int64.minValue()))

    /////////////////////////////////////////////////////////////////////////////
    // longValue                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def longValue01(): Bool =
        let i = Int64.valueOf(0i64);
        Int64.longValue(i) == 0i64

    @Test
    def longValue02(): Bool =
        let i = Int64.valueOf(1i64);
        Int64.longValue(i) == 1i64

    @Test
    def shortValue03(): Bool =
        let i = Int64.valueOf(-1i64);
        Int64.longValue(i) == -1i64

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Bool =
        let i = Int64.valueOf(0i64);
        i.equals(Int64.valueOf(0i64))

    @Test
    def valueOf02(): Bool =
        let i = Int64.valueOf(1i64);
        i.equals(Int64.valueOf(1i64))

    @Test
    def valueOf03(): Bool =
        let i = Int64.valueOf(-1i64);
        i.equals(Int64.valueOf(-1i64))

}
