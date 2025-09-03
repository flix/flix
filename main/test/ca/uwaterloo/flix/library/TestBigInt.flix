mod TestBigInt {

    use Assert.{assertEq, assertTrue, assertNone, assertSome};

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1234ii, BigInt.min(1234ii, 12123123123124151532146469083259682341ii))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -191283756ii, BigInt.min(22737461919238461234728137461283412ii, -191283756ii))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66ii, BigInt.min(-33ii, -66ii))

    @Test
    def min04(): Unit \ Assert = assertEq(expected = -922337203685477580889008ii, BigInt.min(-44ii, -922337203685477580889008ii))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49ii, BigInt.max(48ii, 49ii))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4ii, BigInt.max(4ii, -169879871293847921347918234912394789123ii))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16ii, BigInt.max(-3498457932459234592873452983453245ii, -16ii))

    @Test
    def max04(): Unit \ Assert = assertEq(expected = 92233720368547758078990ii, BigInt.max(-34998734957235ii, 92233720368547758078990ii))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.abs(0ii))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 1ii, BigInt.abs(1ii))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 1ii, BigInt.abs(-1ii))

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def modulo01(): Unit \ Assert = assertEq(expected = 0ii, 1200000000000000000000ii `BigInt.modulo` 200000000000000000000ii)

    @Test
    def modulo02(): Unit \ Assert = assertEq(expected = 200000000000000000000ii, 1200000000000000000000ii `BigInt.modulo` 500000000000000000000ii)

    @Test
    def modulo03(): Unit \ Assert = assertEq(expected = 300000000000000000000ii, -1200000000000000000000ii `BigInt.modulo` 500000000000000000000ii)

    @Test
    def modulo04(): Unit \ Assert = assertEq(expected = -300000000000000000000ii, 1200000000000000000000ii `BigInt.modulo` -500000000000000000000ii)

    @Test
    def modulo05(): Unit \ Assert = assertEq(expected = -200000000000000000000ii, -1200000000000000000000ii `BigInt.modulo` -500000000000000000000ii)

    @Test
    def modulo06(): Unit \ Assert = assertEq(expected = 0ii, 1ii `BigInt.modulo` 0ii)

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def remainder01(): Unit \ Assert = assertEq(expected = 0ii, 1200000000000000000000ii `BigInt.remainder` 200000000000000000000ii)

    @Test
    def remainder02(): Unit \ Assert = assertEq(expected = 200000000000000000000ii, 1200000000000000000000ii `BigInt.remainder` 500000000000000000000ii)

    @Test
    def remainder03(): Unit \ Assert = assertEq(expected = -200000000000000000000ii, -1200000000000000000000ii `BigInt.remainder` 500000000000000000000ii)

    @Test
    def remainder04(): Unit \ Assert = assertEq(expected = 200000000000000000000ii, 1200000000000000000000ii `BigInt.remainder` -500000000000000000000ii)

    @Test
    def remainder05(): Unit \ Assert = assertEq(expected = -200000000000000000000ii, -1200000000000000000000ii `BigInt.remainder` -500000000000000000000ii)

    @Test
    def remainder06(): Unit \ Assert = assertEq(expected = 0ii, 1ii `BigInt.remainder` 0ii)

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 2ii, BigInt.dist(1ii, -1ii))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 2ii, BigInt.dist(-1ii, 1ii))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0ii, BigInt.dist(1ii, 1ii))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 0ii, BigInt.dist(-1ii, -1ii))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Unit \ Assert = assertEq(expected = 1, BigInt.compare(1ii, -1ii))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, BigInt.compare(-9ii, -11ii))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, BigInt.compare(1ii, 1ii))

    @Test
    def compare04(): Unit \ Assert = assertEq(expected = 0, BigInt.compare(-1ii, -1ii))

    @Test
    def compare05(): Unit \ Assert = assertEq(expected = -1, BigInt.compare(-1ii, 1ii))

    @Test
    def compare06(): Unit \ Assert = assertEq(expected = -1, BigInt.compare(11ii, 21ii))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Unit \ Assert = assertEq(expected = 1, BigInt.signum(2ii))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, BigInt.signum(3ii))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, BigInt.signum(0ii))

    @Test
    def signum04(): Unit \ Assert = assertEq(expected = -1, BigInt.signum(-2ii))

    @Test
    def signum05(): Unit \ Assert = assertEq(expected = -1, BigInt.signum(-10ii))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1ii, BigInt.pow(base = 0ii, 0))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0ii, BigInt.pow(base = 0ii, 1))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1ii, BigInt.pow(base = 1ii, 0))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1ii, BigInt.pow(base = 1ii, 1))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25ii, BigInt.pow(base = 5ii, 2))

    /////////////////////////////////////////////////////////////////////////////
    // gcd                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def gcd01(): Unit \ Assert = assertEq(expected = 3ii, BigInt.gcd(9ii, 3ii))

    @Test
    def gcd02(): Unit \ Assert = assertEq(expected = 3ii, BigInt.gcd(21ii, 15ii))

    @Test
    def gcd03(): Unit \ Assert = assertEq(expected = 9233ii, BigInt.gcd(9233ii, 0ii))

    @Test
    def gcd04(): Unit \ Assert = assertEq(expected = 9233ii, BigInt.gcd(0ii, 9233ii))

    @Test
    def gcd05(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(9223ii, 33ii))

    @Test
    def gcd06(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(32462531054272512000001ii, 578837438475345ii))

    @Test
    def gcd07(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(32462531054272512000001ii, 578837982572398457234545ii))

    @Test
    def gcd08(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(32462531054272512000001ii, 92371823432134ii))

    @Test
    def gcd09(): Unit \ Assert = assertEq(expected = 3ii, BigInt.gcd(-9ii, -3ii))

    @Test
    def gcd010(): Unit \ Assert = assertEq(expected = 3ii, BigInt.gcd(21ii, -15ii))

    @Test
    def gcd011(): Unit \ Assert = assertEq(expected = 9233ii, BigInt.gcd(-9233ii, 0ii))

    @Test
    def gcd012(): Unit \ Assert = assertEq(expected = 9233ii, BigInt.gcd(0ii, -9233ii))

    @Test
    def gcd013(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(9223ii, -33ii))

    @Test
    def gcd014(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(-32462531054272512000001ii, 578837438475345ii))

    @Test
    def gcd015(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(-32462531054272512000001ii, 578837982572398457234545ii))

    @Test
    def gcd016(): Unit \ Assert = assertEq(expected = 1ii, BigInt.gcd(32462531054272512000001ii, -92371823432134ii))

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getBit01(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(1ii, 0))

    @Test
    def getBit02(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(1ii, 1))

    @Test
    def getBit03(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(8388608ii, 23))

    @Test
    def getBit04(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(8388608ii, 22))

    @Test
    def getBit05(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(8388608ii, 24))

    @Test
    def getBit06(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(9223372036854775808ii, 63))

    @Test
    def getBit07(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(9223372036854775808ii, 62))

    @Test
    def getBit08(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(9223372036854775808ii, 64))

    @Test
    def getBit09(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(154742504910672534362390528ii, 87))

    @Test
    def getBit10(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(154742504910672534362390528ii, 86))

    @Test
    def getBit11(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(154742504910672534362390528ii, 88))

    @Test
    def getBit12(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(0ii, 7))

    @Test
    def getBit13(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-1ii, 17))

    @Test
    def getBit14(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(-17ii, 4))

    @Test
    def getBit15(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-17ii, 3))

    @Test
    def getBit16(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-17ii, 5))

    @Test
    def getBit17(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(-898274123413412341ii, 4))

    @Test
    def getBit18(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-898274123413412341ii, 3))

    @Test
    def getBit19(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(-898274123413412341ii, 35))

    @Test
    def getBit20(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-898274123413412341ii, 36))

    @Test
    def getBit21(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-898274123413412341ii, 37))

    @Test
    def getBit22(): Unit \ Assert = assertEq(expected = 0, BigInt.getBit(-8982741234134123419879712341ii, 92))

    @Test
    def getBit23(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-8982741234134123419879712341ii, 93))

    @Test
    def getBit24(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-8982741234134123419879712341ii, 94))

    @Test
    def getBit25(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-8982741234134123419879712341ii, 95))

    @Test
    def getBit26(): Unit \ Assert = assertEq(expected = 1, BigInt.getBit(-8982741234134123419879712341ii, 96))

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setBit01(): Unit \ Assert = assertEq(expected = 1ii, BigInt.setBit(0ii, 0))

    @Test
    def setBit02(): Unit \ Assert = assertEq(expected = 2ii, BigInt.setBit(0ii, 1))

    @Test
    def setBit03(): Unit \ Assert = assertEq(expected = 2147483648ii, BigInt.setBit(0ii, 31))

    @Test
    def setBit04(): Unit \ Assert = assertEq(expected = 4294967296ii, BigInt.setBit(0ii, 32))

    @Test
    def setBit05(): Unit \ Assert = assertEq(expected = 2341871806232657920ii, BigInt.setBit(2305843009213693952ii, 55))

    @Test
    def setBit06(): Unit \ Assert = assertEq(expected = 6917529027641081856ii, BigInt.setBit(2305843009213693952ii, 62))

    @Test
    def setBit07(): Unit \ Assert = assertEq(expected = -1ii, BigInt.setBit(-17ii, 4))

    @Test
    def setBit08(): Unit \ Assert = assertEq(expected = -1ii, BigInt.setBit(-1025ii, 10))

    @Test
    def setBit09(): Unit \ Assert = assertEq(expected = -17ii, BigInt.setBit(-17ii, 31))

    @Test
    def setBit10(): Unit \ Assert = assertEq(expected = -17ii, BigInt.setBit(-17ii, 32))

    @Test
    def setBit11(): Unit \ Assert = assertEq(expected = -17ii, BigInt.setBit(-17ii, 101))

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clearBit01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(1ii, 0))

    @Test
    def clearBit02(): Unit \ Assert = assertEq(expected = 1ii, BigInt.clearBit(1ii, 1))

    @Test
    def clearBit03(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(2ii, 1))

    @Test
    def clearBit04(): Unit \ Assert = assertEq(expected = 2ii, BigInt.clearBit(2ii, 2))

    @Test
    def clearBit05(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(0ii, 31))

    @Test
    def clearBit06(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(0ii, 32))

    @Test
    def clearBit07(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(2147483648ii, 31))

    @Test
    def clearBit08(): Unit \ Assert = assertEq(expected = 0ii, BigInt.clearBit(618970019642690137449562112ii, 89))

    @Test
    def clearBit09(): Unit \ Assert = assertEq(expected = -2ii, BigInt.clearBit(-1ii, 0))

    @Test
    def clearBit10(): Unit \ Assert = assertEq(expected = -3ii, BigInt.clearBit(-1ii, 1))

    @Test
    def clearBit11(): Unit \ Assert = assertEq(expected = -33ii, BigInt.clearBit(-1ii, 5))

    @Test
    def clearBit12(): Unit \ Assert = assertEq(expected = -2147483649ii, BigInt.clearBit(-1ii, 31))

    @Test
    def clearBit13(): Unit \ Assert = assertEq(expected = -4294967297ii, BigInt.clearBit(-1ii, 32))

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flipBit01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.flipBit(1ii, 0))

    @Test
    def flipBit02(): Unit \ Assert = assertEq(expected = 3ii, BigInt.flipBit(1ii, 1))

    @Test
    def flipBit03(): Unit \ Assert = assertEq(expected = 0ii, BigInt.flipBit(2ii, 1))

    @Test
    def flipBit04(): Unit \ Assert = assertEq(expected = 6ii, BigInt.flipBit(2ii, 2))

    @Test
    def flipBit05(): Unit \ Assert = assertEq(expected = 2147483648ii, BigInt.flipBit(0ii, 31))

    @Test
    def flipBit06(): Unit \ Assert = assertEq(expected = 4294967296ii, BigInt.flipBit(0ii, 32))

    @Test
    def flipBit07(): Unit \ Assert = assertEq(expected = 0ii, BigInt.flipBit(2147483648ii, 31))

    @Test
    def flipBit08(): Unit \ Assert = assertEq(expected = 0ii, BigInt.flipBit(618970019642690137449562112ii, 89))

    @Test
    def flipBit09(): Unit \ Assert = assertEq(expected = -2ii, BigInt.flipBit(-1ii, 0))

    @Test
    def flipBit10(): Unit \ Assert = assertEq(expected = -3ii, BigInt.flipBit(-1ii, 1))

    @Test
    def flipBit11(): Unit \ Assert = assertEq(expected = -33ii, BigInt.flipBit(-1ii, 5))

    @Test
    def flipBit12(): Unit \ Assert = assertEq(expected = -2147483649ii, BigInt.flipBit(-1ii, 31))

    @Test
    def flipBit13(): Unit \ Assert = assertEq(expected = -4294967297ii, BigInt.flipBit(-1ii, 32))


    /////////////////////////////////////////////////////////////////////////////
    // leftShift
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseLeftShift01(): Unit \ Assert = assertEq(expected = 8ii, BigInt.leftShift(8ii, 0))

    @Test
    def testBitwiseLeftShift02(): Unit \ Assert = assertEq(expected = 34359738368ii, BigInt.leftShift(8ii, 32))

    @Test
    def testBitwiseLeftShift03(): Unit \ Assert = assertEq(expected = 147573952589676412928ii, BigInt.leftShift(8ii, 64))


    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testBitwiseRightShift04(): Unit \ Assert = assertEq(expected = 1200000000000000000000ii, BigInt.rightShift(1200000000000000000000ii, 0))

    @Test
    def testBitwiseRightShift05(): Unit \ Assert = assertEq(expected = 300000000000000000000ii, BigInt.rightShift(1200000000000000000000ii, 2))

    @Test
    def testBitwiseRightShift06(): Unit \ Assert = assertEq(expected = 0ii, BigInt.rightShift(1200000000000000000000ii, 72))

    @Test
    def testBitwiseRightShift07(): Unit \ Assert = assertEq(expected = -300000000000000000000ii, BigInt.rightShift(-1200000000000000000000ii, 2))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseAnd01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseAnd(0ii, 0ii))

    @Test
    def bitwiseAnd02(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseAnd(127ii, 0ii))

    @Test
    def bitwiseAnd03(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseAnd(-128ii, 0ii))

    @Test
    def bitwiseAnd04(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseAnd(0ii, 1ii))

    @Test
    def bitwiseAnd05(): Unit \ Assert = assertEq(expected = 1ii, BigInt.bitwiseAnd(127ii, 1ii))

    @Test
    def bitwiseAnd06(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseAnd(-128ii, 1ii))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseNot01(): Unit \ Assert = assertEq(expected = -1ii, BigInt.bitwiseNot(0ii))

    @Test
    def bitwiseNot02(): Unit \ Assert = assertEq(expected = -2ii, BigInt.bitwiseNot(1ii))

    @Test
    def bitwiseNot03(): Unit \ Assert = assertEq(expected = -0ii, BigInt.bitwiseNot(-1ii))

    @Test
    def bitwiseNot04(): Unit \ Assert = assertEq(expected = -128ii, BigInt.bitwiseNot(127ii))

    @Test
    def bitwiseNot05(): Unit \ Assert = assertEq(expected = 127ii, BigInt.bitwiseNot(-128ii))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseOr01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseOr(0ii, 0ii))

    @Test
    def bitwiseOr02(): Unit \ Assert = assertEq(expected = 127ii, BigInt.bitwiseOr(127ii, 0ii))

    @Test
    def bitwiseOr03(): Unit \ Assert = assertEq(expected = -128ii, BigInt.bitwiseOr(-128ii, 0ii))

    @Test
    def bitwiseOr04(): Unit \ Assert = assertEq(expected = 1ii, BigInt.bitwiseOr(0ii, 1ii))

    @Test
    def bitwiseOr05(): Unit \ Assert = assertEq(expected = 127ii, BigInt.bitwiseOr(127ii, 1ii))

    @Test
    def bitwiseOr06(): Unit \ Assert = assertEq(expected = -127ii, BigInt.bitwiseOr(-128ii, 1ii))

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bitwiseXor01(): Unit \ Assert = assertEq(expected = 0ii, BigInt.bitwiseXor(0ii, 0ii))

    @Test
    def bitwiseXor02(): Unit \ Assert = assertEq(expected = 127ii, BigInt.bitwiseXor(127ii, 0ii))

    @Test
    def bitwiseXor03(): Unit \ Assert = assertEq(expected = -128ii, BigInt.bitwiseXor(-128ii, 0ii))

    @Test
    def bitwiseXor04(): Unit \ Assert = assertEq(expected = 1ii, BigInt.bitwiseXor(0ii, 1ii))

    @Test
    def bitwiseXor05(): Unit \ Assert = assertEq(expected = 126ii, BigInt.bitwiseXor(127ii, 1ii))

    @Test
    def bitwiseXor06(): Unit \ Assert = assertEq(expected = -127ii, BigInt.bitwiseXor(-128ii, 1ii))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0", BigInt.toString(0ii))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1", BigInt.toString(1ii))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1", BigInt.toString(-1ii))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "3498457932459234592873452983453245", BigInt.toString(3498457932459234592873452983453245ii))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-3498457932459234592873452983453245", BigInt.toString(-3498457932459234592873452983453245ii))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertNone(BigInt.fromString("Bad"))

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1ii), BigInt.fromString(" 1"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1ii), BigInt.fromString("1 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0ii), BigInt.fromString("0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1ii), BigInt.fromString("1"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1ii), BigInt.fromString("-1"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(3498457932459234592873452983453245ii), BigInt.fromString("3498457932459234592873452983453245"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-3498457932459234592873452983453245ii), BigInt.fromString("-3498457932459234592873452983453245"))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), BigInt.tryToInt8(0ii))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), BigInt.tryToInt8(1ii))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), BigInt.tryToInt8(-1ii))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), BigInt.tryToInt8(127ii))

    @Test
    def tryToInt805(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt8(128ii))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), BigInt.tryToInt8(-128ii))

    @Test
    def tryToInt807(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt8(-129ii))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Unit \ Assert = assertEq(expected = Some(0i16), BigInt.tryToInt16(0ii))

    @Test
    def tryToInt1602(): Unit \ Assert = assertEq(expected = Some(1i16), BigInt.tryToInt16(1ii))

    @Test
    def tryToInt1603(): Unit \ Assert = assertEq(expected = Some(-1i16), BigInt.tryToInt16(-1ii))

    @Test
    def tryToInt1604(): Unit \ Assert = assertEq(expected = Some(32767i16), BigInt.tryToInt16(32767ii))

    @Test
    def tryToInt1605(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt16(32768ii))

    @Test
    def tryToInt1606(): Unit \ Assert = assertEq(expected = Some(-32768i16), BigInt.tryToInt16(-32768ii))

    @Test
    def tryToInt1607(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt16(-32769ii))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Unit \ Assert = assertEq(expected = Some(0), BigInt.tryToInt32(0ii))

    @Test
    def tryToInt3202(): Unit \ Assert = assertEq(expected = Some(1), BigInt.tryToInt32(1ii))

    @Test
    def tryToInt3203(): Unit \ Assert = assertEq(expected = Some(-1), BigInt.tryToInt32(-1ii))

    @Test
    def tryToInt3204(): Unit \ Assert = assertEq(expected = Some(2147483647), BigInt.tryToInt32(2147483647ii))

    @Test
    def tryToInt3205(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt32(2147483648ii))

    @Test
    def tryToInt3206(): Unit \ Assert = assertEq(expected = Some(-2147483648), BigInt.tryToInt32(-2147483648ii))

    @Test
    def tryToInt3207(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt32(-2147483649ii))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Unit \ Assert = assertEq(expected = Some(0i64), BigInt.tryToInt64(0ii))

    @Test
    def tryToInt6402(): Unit \ Assert = assertEq(expected = Some(1i64), BigInt.tryToInt64(1ii))

    @Test
    def tryToInt6403(): Unit \ Assert = assertEq(expected = Some(-1i64), BigInt.tryToInt64(-1ii))

    @Test
    def tryToInt6404(): Unit \ Assert = assertEq(expected = Some(9223372036854775807i64), BigInt.tryToInt64(9223372036854775807ii))

    @Test
    def tryToInt6405(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt64(9223372036854775808ii))

    @Test
    def tryToInt6406(): Unit \ Assert = assertEq(expected = Some(-9223372036854775808i64), BigInt.tryToInt64(-9223372036854775808ii))

    @Test
    def tryToInt6407(): Unit \ Assert = assertEq(expected = None, BigInt.tryToInt64(-9223372036854775809ii))

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat3201(): Unit \ Assert = assertEq(expected = Some(0.0f32), BigInt.tryToFloat32(0ii))

    @Test
    def tryToFloat3202(): Unit \ Assert = assertEq(expected = Some(1.0f32), BigInt.tryToFloat32(1ii))

    @Test
    def tryToFloat3203(): Unit \ Assert = assertEq(expected = Some(-1.0f32), BigInt.tryToFloat32(-1ii))

    @Test
    def tryToFloat3204(): Unit \ Assert =
        let step = () -> {
            forM (
                max <- Float32.maxValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(max)
            ) yield res
        };
        assertSome(step())

    @Test
    def tryToFloat3205(): Unit \ Assert =
        let step = () -> {
            forM (
                max <- Float32.maxValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(max + 1ii)
            ) yield res
        };
        assertEq(expected = None, step())

    @Test
    def tryToFloat3206(): Unit \ Assert =
        let step = () -> {
            forM (
                min <- Float32.minValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(min)
            ) yield res
        };
        assertSome(step())

    @Test
    def tryToFloat3207(): Unit \ Assert =
        let step = () -> {
            forM (
                min <- Float32.minValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(min - 1ii)
            ) yield res
        };
        assertEq(expected = None, step())

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat6401(): Unit \ Assert = assertEq(expected = Some(0.0f64), BigInt.tryToFloat64(0ii))

    @Test
    def tryToFloat6402(): Unit \ Assert = assertEq(expected = Some(1.0f64), BigInt.tryToFloat64(1ii))

    @Test
    def tryToFloat6403(): Unit \ Assert = assertEq(expected = Some(-1.0f64), BigInt.tryToFloat64(-1ii))

    @Test
    def tryToFloat6404(): Unit \ Assert =
        let step = () -> {
            forM (
                max <- Float64.maxValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(max)
            ) yield res
        };
        assertSome(step())

    @Test
    def tryToFloat6405(): Unit \ Assert =
        let step = () -> {
            forM (
                max <- Float64.maxValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(max + 1ii)
            ) yield res
        };
        assertEq(expected = None, step())

    @Test
    def tryToFloat6406(): Unit \ Assert =
        let step = () -> {
            forM (
                min <- Float64.minValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(min)
            ) yield res
        };
        assertSome(step())

    @Test
    def tryToFloat6407(): Unit \ Assert =
        let step = () -> {
            forM (
                min <- Float64.minValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(min - 1ii)
            ) yield res
        };
        assertEq(expected = None, step())

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigDecimal01(): Unit \ Assert = assertEq(expected = 0.0ff, BigInt.toBigDecimal(0ii))

    @Test
    def toBigDecimal02(): Unit \ Assert = assertEq(expected = 1.0ff, BigInt.toBigDecimal(1ii))

    @Test
    def toBigDecimal03(): Unit \ Assert = assertEq(expected = -1.0ff, BigInt.toBigDecimal(-1ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Unit \ Assert = assertEq(expected = 0i8, BigInt.clampToInt8(min = -100i8, max = 100i8, 0ii))

    @Test
    def clampToInt802(): Unit \ Assert = assertEq(expected = 100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, 100ii))

    @Test
    def clampToInt803(): Unit \ Assert = assertEq(expected = 100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, 101ii))

    @Test
    def clampToInt804(): Unit \ Assert = assertEq(expected = -100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, -100ii))

    @Test
    def clampToInt805(): Unit \ Assert = assertEq(expected = -100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, -101ii))

    @Test
    def clampToInt806(): Unit \ Assert = assertEq(expected = 100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, 8982741234134123419879712341ii))

    @Test
    def clampToInt807(): Unit \ Assert = assertEq(expected = -100i8, BigInt.clampToInt8(min = -100i8, max = 100i8, -8982741234134123419879712341ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Unit \ Assert = assertEq(expected = 0i16, BigInt.clampToInt16(min = -100i16, max = 100i16, 0ii))

    @Test
    def clampToInt1602(): Unit \ Assert = assertEq(expected = 100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, 100ii))

    @Test
    def clampToInt1603(): Unit \ Assert = assertEq(expected = 100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, 101ii))

    @Test
    def clampToInt1604(): Unit \ Assert = assertEq(expected = -100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, -100ii))

    @Test
    def clampToInt1605(): Unit \ Assert = assertEq(expected = -100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, -101ii))

    @Test
    def clampToInt1606(): Unit \ Assert = assertEq(expected = 100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, 8982741234134123419879712341ii))

    @Test
    def clampToInt1607(): Unit \ Assert = assertEq(expected = -100i16, BigInt.clampToInt16(min = -100i16, max = 100i16, -8982741234134123419879712341ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt3201(): Unit \ Assert = assertEq(expected = 0, BigInt.clampToInt32(min = -100, max = 100, 0ii))

    @Test
    def clampToInt3202(): Unit \ Assert = assertEq(expected = 100, BigInt.clampToInt32(min = -100, max = 100, 100ii))

    @Test
    def clampToInt3203(): Unit \ Assert = assertEq(expected = 100, BigInt.clampToInt32(min = -100, max = 100, 101ii))

    @Test
    def clampToInt3204(): Unit \ Assert = assertEq(expected = -100, BigInt.clampToInt32(min = -100, max = 100, -100ii))

    @Test
    def clampToInt3205(): Unit \ Assert = assertEq(expected = -100, BigInt.clampToInt32(min = -100, max = 100, -101ii))

    @Test
    def clampToInt3206(): Unit \ Assert = assertEq(expected = 100i32, BigInt.clampToInt32(min = -100i32, max = 100i32, 8982741234134123419879712341ii))

    @Test
    def clampToInt3207(): Unit \ Assert = assertEq(expected = -100i32, BigInt.clampToInt32(min = -100i32, max = 100i32, -8982741234134123419879712341ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt6401(): Unit \ Assert = assertEq(expected = 0i64, BigInt.clampToInt64(min = -100i64, max = 100i64, 0ii))

    @Test
    def clampToInt6402(): Unit \ Assert = assertEq(expected = 100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, 100ii))

    @Test
    def clampToInt6403(): Unit \ Assert = assertEq(expected = 100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, 101ii))

    @Test
    def clampToInt6404(): Unit \ Assert = assertEq(expected = -100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, -100ii))

    @Test
    def clampToInt6405(): Unit \ Assert = assertEq(expected = -100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, -101ii))

    @Test
    def clampToInt6406(): Unit \ Assert = assertEq(expected = 100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, 8982741234134123419879712341ii))

    @Test
    def clampToInt6407(): Unit \ Assert = assertEq(expected = -100i64, BigInt.clampToInt64(min = -100i64, max = 100i64, -8982741234134123419879712341ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat32                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToFloat3201(): Unit \ Assert = assertEq(expected = 0.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, 0ii))

    @Test
    def clampToFloat3202(): Unit \ Assert = assertEq(expected = 100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, 100ii))

    @Test
    def clampToFloat3203(): Unit \ Assert = assertEq(expected = 100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, 101ii))

    @Test
    def clampToFloat3204(): Unit \ Assert = assertEq(expected = -100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, -100ii))

    @Test
    def clampToFloat3205(): Unit \ Assert = assertEq(expected = -100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, -101ii))

    @Test
    def clampToFloat3206(): Unit \ Assert = assertEq(expected = 100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, 8982741234134123419879712341ii))

    @Test
    def clampToFloat3207(): Unit \ Assert = assertEq(expected = -100.0f32, BigInt.clampToFloat32(min = -100.0f32, max = 100.0f32, -8982741234134123419879712341ii))

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat64                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, 0ii))

    @Test
    def clampToFloat6402(): Unit \ Assert = assertEq(expected = 100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, 100ii))

    @Test
    def clampToFloat6403(): Unit \ Assert = assertEq(expected = 100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, 101ii))

    @Test
    def clampToFloat6404(): Unit \ Assert = assertEq(expected = -100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, -100ii))

    @Test
    def clampToFloat6405(): Unit \ Assert = assertEq(expected = -100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, -101ii))

    @Test
    def clampToFloat6406(): Unit \ Assert = assertEq(expected = 100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, 8982741234134123419879712341ii))

    @Test
    def clampToFloat6407(): Unit \ Assert = assertEq(expected = -100.0f64, BigInt.clampToFloat64(min = -100.0f64, max = 100.0f64, -8982741234134123419879712341ii))

}
