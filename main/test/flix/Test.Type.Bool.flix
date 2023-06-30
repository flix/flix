mod Test.Type.Bool {

    enum B[_: Bool] with Eq {
        case B
    }

    @Test
    def testTrue01(): Bool = assertTrue(mkTrue())

    @Test
    def testTrue02(): Bool = assertEqual(mkTrue(), mkTrue())

    @Test
    def testFalse01(): Bool = assertFalse(mkFalse())

    @Test
    def testFalse02(): Bool = assertEqual(mkFalse(), mkFalse())

    @Test
    def testTrueFalse01(): Bool = assertXor(mkTrue(), mkFalse())

    @Test
    def testTrueFalse02(): Bool = assertXor(mkFalse(), mkTrue())

    @Test
    def testNot01(): Bool = assertFalse(mkNot(mkTrue()))

    @Test
    def testNot02(): Bool = assertTrue(mkNot(mkFalse()))

    @Test
    def testNot03(): Bool = assertTrue(mkNot(mkNot(mkTrue())))

    @Test
    def testNot04(): Bool = assertFalse(mkNot(mkNot(mkFalse())))

    @Test
    def testNot05(): Bool = assertFalse(mkNot(mkNot(mkNot(mkTrue()))))

    @Test
    def testNot06(): Bool = assertTrue(mkNot(mkNot(mkNot(mkFalse()))))

    @Test
    def testAnd01(): Bool = assertFalse(mkAnd(mkFalse(), mkFalse()))

    @Test
    def testAnd02(): Bool = assertFalse(mkAnd(mkFalse(), mkTrue()))

    @Test
    def testAnd03(): Bool = assertFalse(mkAnd(mkTrue(), mkFalse()))

    @Test
    def testAnd04(): Bool = assertTrue(mkAnd(mkTrue(), mkTrue()))

    @Test
    def testAnd05(): Bool = assertTrue(mkAnd(mkAnd(mkTrue(), mkTrue()), mkAnd(mkTrue(), mkTrue())))

    @Test
    def testAnd06(): Bool = assertFalse(mkAnd(mkAnd(mkTrue(), mkTrue()), mkAnd(mkTrue(), mkFalse())))

    @Test
    def testAnd07(): Bool = assertFalse(mkAnd(mkAnd(mkAnd(mkTrue(), mkTrue()), mkTrue()), mkFalse()))

    @Test
    def testAnd08(): Bool = assertFalse(mkAnd(mkAnd(mkFalse(), mkTrue()), mkAnd(mkTrue(), mkTrue())))

    @Test
    def testOr01(): Bool = assertFalse(mkOr(mkFalse(), mkFalse()))

    @Test
    def testOr02(): Bool = assertTrue(mkOr(mkTrue(), mkFalse()))

    @Test
    def testOr03(): Bool = assertTrue(mkOr(mkFalse(), mkTrue()))

    @Test
    def testOr04(): Bool = assertTrue(mkOr(mkTrue(), mkTrue()))

    @Test
    def testOr05(): Bool = assertTrue(mkOr(mkOr(mkFalse(), mkFalse()), mkOr(mkFalse(), mkTrue())))

    @Test
    def testOr06(): Bool = assertFalse(mkOr(mkOr(mkFalse(), mkFalse()), mkOr(mkFalse(), mkFalse())))

    @Test
    def testOr07(): Bool = assertTrue(mkOr(mkOr(mkOr(mkTrue(), mkFalse()), mkFalse()), mkFalse()))

    @Test
    def testOr08(): Bool = assertTrue(mkOr(mkOr(mkTrue(), mkTrue()), mkOr(mkTrue(), mkTrue())))

    @Test
    def testXor01(): Bool = assertTrue(mkXor(mkFalse(), mkTrue()))

    @Test
    def testXor02(): Bool = assertTrue(mkXor(mkTrue(), mkFalse()))

    @Test
    def testXor03(): Bool = assertFalse(mkXor(mkFalse(), mkFalse()))

    @Test
    def testXor04(): Bool = assertFalse(mkXor(mkTrue(), mkTrue()))

    @Test
    def testXor05(): Bool = assertTrue(mkXor(mkXor(mkFalse(), mkTrue()), mkXor(mkTrue(), mkTrue())))

    @Test
    def testXor06(): Bool = assertFalse(mkXor(mkXor(mkTrue(), mkFalse()), mkXor(mkFalse(), mkTrue())))

    @Test
    def testXor07(): Bool = assertTrue(mkXor(mkXor(mkXor(mkFalse(), mkTrue()), mkFalse()), mkFalse()))

    @Test
    def testXor08(): Bool = assertFalse(mkXor(mkTrue(), mkXor(mkTrue(), mkXor(mkTrue(), mkTrue()))))

    @Test
    def testNestedFormula01(): Bool = assertTrue(mkNot(mkAnd(mkTrue(), mkFalse())))

    @Test
    def testNestedFormula02(): Bool = assertFalse(mkNot(mkAnd(mkTrue(), mkTrue())))

    @Test
    def testNestedFormula03(): Bool = assertTrue(mkNot(mkOr(mkFalse(), mkFalse())))

    @Test
    def testNestedFormula04(): Bool = assertFalse(mkNot(mkOr(mkFalse(), mkTrue())))

    @Test
    def testNestedFormula05(): Bool = assertEqual(mkOr(mkFalse(), mkFalse()), mkAnd(mkTrue(), mkFalse()))

    @Test
    def testNestedFormula06(): Bool = assertEqual(mkOr(mkFalse(), mkTrue()), mkAnd(mkTrue(), mkTrue()))

    @Test
    def testNestedFormula07(): Bool =
      assertEqual(mkNot(mkAnd(mkTrue(), mkFalse())), mkNot(mkOr(mkFalse(), mkFalse())))

    @Test
    def testNestedFormula08(): Bool =
      assertTrue(mkAnd(mkAnd(mkTrue(), mkTrue()), mkOr(mkFalse(), mkNot(mkNot(mkTrue())))))

    @Test
    def testNestedFormula09(): Bool =
      assertTrue(mkXor(mkXor(mkAnd(mkTrue(), mkTrue()), mkOr(mkFalse(), mkFalse())), mkFalse()))

    @Test
    def testNestedFormula10(): Bool =
      assertTrue(mkNot(mkXor(mkNot(mkFalse()), mkXor(mkNot(mkTrue()), mkXor(mkTrue(), mkFalse())))))

    @Test
    def testFuncComposition01(): Bool = let id = mkNot >> mkNot; assertTrue(id(mkTrue()))

    @Test
    def testFuncComposition02(): Bool = let notId = mkNot >> mkNot >> mkNot; assertFalse(notId(mkTrue()))

    @Test
    def testFuncComposition03(): Bool = let notId = mkNot >> mkNot >> mkNot; assertFalse(notId(mkTrue()))

    ///
    /// A type-level representation for 2-bit integers.
    ///
    enum I2[_: Bool, _: Bool] with Eq {
      case I2
    }

    @Test
    def testTwoBitInt01(): Bool = isZero2(mkZero2())

    @Test
    def test2BitInt02(): Bool = isOne2(mkOne2())

    @Test
    def test2BitInt03(): Bool = isTwo2(mkTwo2())

    @Test
    def test2BitInt04(): Bool = isThree2(mkThree2())

    @Test
    def test2BitwiseNot01(): Bool = isZero2(bitwiseNot2(mkThree2()))

    @Test
    def test2BitwiseNot02(): Bool = isOne2(bitwiseNot2(mkTwo2()))

    @Test
    def test2BitwiseNot03(): Bool = isTwo2(bitwiseNot2(mkOne2()))

    @Test
    def test2BitwiseNot04(): Bool = isThree2(bitwiseNot2(mkZero2()))

    @Test
    def test2BitwiseNot05(): Bool = isZero2(bitwiseNot2(bitwiseNot2(mkZero2())))

    @Test
    def test2BitwiseNot06(): Bool = isThree2(bitwiseNot2(bitwiseNot2(mkThree2())))

    @Test
    def test2BitwiseAnd01(): Bool = isZero2(bitwiseAnd2(mkThree2(), mkZero2()))

    @Test
    def test2BitwiseAnd02(): Bool = isZero2(bitwiseAnd2(mkZero2(), mkThree2()))

    @Test
    def test2BitwiseAnd03(): Bool = isZero2(bitwiseAnd2(mkOne2(), mkTwo2()))

    @Test
    def test2BitwiseAnd04(): Bool = isOne2(bitwiseAnd2(mkOne2(), mkThree2()))

    @Test
    def test2BitwiseAnd05(): Bool = isTwo2(bitwiseAnd2(mkThree2(), mkTwo2()))

    @Test
    def test2BitwiseAnd06(): Bool = isThree2(bitwiseAnd2(mkThree2(), mkThree2()))

    @Test
    def test2BitwiseOr01(): Bool = isZero2(bitwiseOr2(mkZero2(), mkZero2()))

    @Test
    def test2BitwiseOr02(): Bool = isOne2(bitwiseOr2(mkZero2(), mkOne2()))

    @Test
    def test2BitwiseOr03(): Bool = isThree2(bitwiseOr2(mkOne2(), mkTwo2()))

    @Test
    def test2BitwiseOr04(): Bool = isThree2(bitwiseOr2(mkTwo2(), mkOne2()))

    @Test
    def test2BitwiseOr05(): Bool = isThree2(bitwiseOr2(mkThree2(), mkZero2()))

    @Test
    def test2BitwiseOr06(): Bool = isThree2(bitwiseOr2(mkZero2(), mkThree2()))

    @Test
    def test2BitwiseOr07(): Bool = isThree2(bitwiseOr2(mkThree2(), mkOne2()))

    @Test
    def test2BitwiseOr08(): Bool = isThree2(bitwiseOr2(mkTwo2(), mkThree2()))

    @Test
    def test2Swap01(): Bool = isZero2(swap2(mkZero2()))

    @Test
    def test2Swap02(): Bool = isTwo2(swap2(mkOne2()))

    @Test
    def test2Swap03(): Bool = isOne2(swap2(mkTwo2()))

    @Test
    def test2Swap04(): Bool = isThree2(swap2(mkThree2()))

    @Test
    def test2NestedBitwiseNotAndOr01(): Bool =
      isThree2(bitwiseOr2(bitwiseAnd2(mkThree2(), mkOne2()), bitwiseNot2(mkOne2())))

    @Test
    def test2NestedBitwiseNotAndOr02(): Bool =
      isThree2(bitwiseOr2(bitwiseAnd2(mkThree2(), bitwiseNot2(mkThree2())), mkThree2()))

    @Test
    def test2NestedBitwiseNotAndOr03(): Bool =
      isTwo2(bitwiseAnd2(bitwiseOr2(mkThree2(), mkOne2()), bitwiseNot2(bitwiseNot2(mkTwo2()))))

    @Test
    def test2NestedBitwiseNotAndOr04(): Bool =
      isZero2(bitwiseNot2(bitwiseAnd2(bitwiseOr2(mkThree2(), mkZero2()), mkThree2())))

    @Test
    def test2NestedBitwiseNotAndOr05(): Bool =
      isThree2(bitwiseNot2(bitwiseAnd2(bitwiseAnd2(mkThree2(), mkTwo2()), bitwiseAnd2(mkOne2(), mkTwo2()))))

    @Test
    def test2NestedBitwiseNotAndOr06(): Bool =
      isThree2(bitwiseOr2(bitwiseOr2(mkThree2(), mkZero2()), bitwiseOr2(bitwiseNot2(mkTwo2()), mkTwo2())))

    @Test
    def test2NestedBitwiseNotAndOr07(): Bool = {
      let three = bitwiseOr2(bitwiseAnd2(mkThree2(), mkOne2()), mkTwo2());
      let two = bitwiseAnd2(bitwiseOr2(mkThree2(), mkOne2()), mkTwo2());
      let one = bitwiseAnd2(bitwiseNot2(mkTwo2()), bitwiseNot2(bitwiseNot2(mkThree2())));
      let zero = bitwiseAnd2(three, bitwiseAnd2(two, bitwiseAnd2(one, mkZero2())));
      isThree2(bitwiseNot2(bitwiseAnd2(zero, bitwiseOr2(one, bitwiseOr2(two, three)))))
    }

    @Test
    def test2inc01(): Bool = isOne2(inc2(mkZero2()))

    @Test
    def test2inc02(): Bool = isTwo2(inc2(mkOne2()))

    @Test
    def test2inc03(): Bool = isThree2(inc2(mkTwo2()))

    @Test
    def test2inc04(): Bool = isZero2(inc2(mkThree2()))

    @Test
    def test2inc05(): Bool = isTwo2(inc2(inc2(mkZero2())))

    @Test
    def test2inc06(): Bool = isOne2(inc2(inc2(inc2(mkTwo2()))))

    @Test
    def test2inc07(): Bool = isZero2(inc2(inc2(inc2(inc2(mkZero2())))))

    @Test
    def test2Add01(): Bool = isZero2(add2(mkZero2(), mkZero2()))

    @Test
    def test2Add02(): Bool = isOne2(add2(mkZero2(), mkOne2()))

    @Test
    def test2Add03(): Bool = isOne2(add2(mkOne2(), mkZero2()))

    @Test
    def test2Add04(): Bool = isZero2(add2(mkTwo2(), mkTwo2()))

    @Test
    def test2Add05(): Bool = isThree2(add2(mkTwo2(), mkOne2()))

    @Test
    def test2Add06(): Bool = isThree2(add2(mkOne2(), mkTwo2()))

    @Test
    def test2Add07(): Bool = isOne2(add2(mkThree2(), mkTwo2()))

    @Test
    def test2Add08(): Bool = isOne2(add2(mkTwo2(), mkThree2()))

    @Test
    def test2Add09(): Bool = isZero2(add2(mkThree2(), mkOne2()))

    @Test
    def test2Add10(): Bool = isZero2(add2(mkOne2(), mkThree2()))

    @Test
    def test2Add11(): Bool = isThree2(add2(mkThree2(), mkZero2()))

    @Test
    def test2Add12(): Bool = isThree2(add2(mkZero2(), mkThree2()))

    @Test
    def test2Add13(): Bool = isOne2(add2(mkThree2(), add2(mkThree2(), mkThree2())))

    @Test
    def test2Add14(): Bool = isOne2(add2(add2(mkThree2(), mkThree2()), mkThree2()))

    @Test
    def test2Add15(): Bool = isTwo2(add2(mkTwo2(), add2(mkTwo2(), mkTwo2())))

    @Test
    def test2Add16(): Bool = isTwo2(add2(add2(mkTwo2(), mkTwo2()), mkTwo2()))

    @Test
    def test2Add17(): Bool = isTwo2(add2(add2(mkZero2(), mkOne2()), add2(mkTwo2(), mkThree2())))

    @Test
    def test2Add18(): Bool = isThree2(add2(add2(mkTwo2(), mkOne2()), add2(mkOne2(), mkThree2())))

    @Test
    def test2Add19(): Bool = isOne2(add2(mkZero2(), add2(mkZero2(), mkOne2())))

    @Test
    def test2Add20(): Bool = isThree2(add2(mkThree2(), add2(add2(mkThree2(), mkThree2()), add2(mkThree2(), mkThree2()))))

    @Test
    def test2Mul01(): Bool = isZero2(mul2(mkZero2(), mkZero2()))

    @Test
    def test2Mul02(): Bool = isOne2(mul2(mkOne2(), mkOne2()))

    @Test
    def test2Mul03(): Bool = isZero2(mul2(mkTwo2(), mkTwo2()))

    @Test
    def test2Mul04(): Bool = isOne2(mul2(mkThree2(), mkThree2()))

    @Test
    def test2Mul05(): Bool = isZero2(mul2(mkZero2(), mkOne2()))

    @Test
    def test2Mul06(): Bool = isZero2(mul2(mkOne2(), mkZero2()))

    @Test
    def test2Mul07(): Bool = isZero2(mul2(mkTwo2(), mkZero2()))

    @Test
    def test2Mul08(): Bool = isZero2(mul2(mkZero2(), mkTwo2()))

    @Test
    def test2Mul09(): Bool = isZero2(mul2(mkZero2(), mkThree2()))

    @Test
    def test2Mul10(): Bool = isZero2(mul2(mkThree2(), mkZero2()))

    @Test
    def test2Mul11(): Bool = isTwo2(mul2(mkOne2(), mkTwo2()))

    @Test
    def test2Mul12(): Bool = isTwo2(mul2(mkTwo2(), mkOne2()))

    @Test
    def test2Mul13(): Bool = isTwo2(mul2(mkThree2(), mkTwo2()))

    @Test
    def test2Mul14(): Bool = isTwo2(mul2(mkTwo2(), mkThree2()))

    @Test
    def test2Mul15(): Bool = isThree2(mul2(mkThree2(), mkOne2()))

    @Test
    def test2Mul16(): Bool = isThree2(mul2(mkOne2(), mkThree2()))

    @Test
    def test2Mul17(): Bool = isTwo2(mul2(mul2(mkOne2(), mkThree2()), mkTwo2()))

    @Test
    def test2Mul18(): Bool = isOne2(mul2(mul2(mkThree2(), mkThree2()), mul2(mkThree2(), mkThree2())))

    @Test
    def test2Mul19(): Bool = isZero2(mul2(mul2(mul2(mkOne2(), mkThree2()), mkTwo2()), mkZero2()))

    @Test
    def test2Mul20(): Bool = isTwo2(mul2(mul2(mkTwo2(), mul2(mkOne2(), mkThree2())), mul2(mkThree2(), mkOne2())))

    enum I4[_: Bool, _: Bool, _: Bool, _: Bool] with Eq {
      case I4
    }

    @Test
    def test4BitInt01(): Bool = isZero4(mkZero4())

    @Test
    def test4BitInt02(): Bool = isOne4(mkOne4())

    @Test
    def test4BitInt03(): Bool = isTwo4(mkTwo4())

    @Test
    def test4BitInt04(): Bool = isThree4(mkThree4())

    @Test
    def test4BitInt05(): Bool = isFour4(mkFour4())

    @Test
    def test4BitInt06(): Bool = isFive4(mkFive4())

    @Test
    def test4BitInt07(): Bool = isSix4(mkSix4())

    @Test
    def test4BitInt08(): Bool = isSeven4(mkSeven4())

    @Test
    def test4BitInt09(): Bool = isEight4(mkEight4())

    @Test
    def test4BitInt10(): Bool = isNine4(mkNine4())

    @Test
    def test4BitInt11(): Bool = isTen4(mkTen4())

    @Test
    def test4BitInt12(): Bool = isEleven4(mkEleven4())

    @Test
    def test4BitInt13(): Bool = isTwelve4(mkTwelve4())

    @Test
    def test4BitInt14(): Bool = isThirteen4(mkThirteen4())

    @Test
    def test4BitInt15(): Bool = isFourteen4(mkFourteen4())

    @Test
    def test4BitInt16(): Bool = isFifteen4(mkFifteen4())

    @Test
    def test4Inc01(): Bool = isOne4(inc4(mkZero4()))

    @Test
    def test4Inc02(): Bool = isTwo4(inc4(mkOne4()))

    @Test
    def test4Inc03(): Bool = isThree4(inc4(mkTwo4()))

    @Test
    def test4Inc04(): Bool = isFour4(inc4(mkThree4()))

    @Test
    def test4Inc05(): Bool = isFive4(inc4(mkFour4()))

    @Test
    def test4Inc06(): Bool = isSix4(inc4(mkFive4()))

    @Test
    def test4Inc07(): Bool = isSeven4(inc4(mkSix4()))

    @Test
    def test4Inc08(): Bool = isEight4(inc4(mkSeven4()))

    @Test
    def test4Inc09(): Bool = isNine4(inc4(mkEight4()))

    @Test
    def test4Inc10(): Bool = isTen4(inc4(mkNine4()))

    @Test
    def test4Inc11(): Bool = isEleven4(inc4(mkTen4()))

    @Test
    def test4Inc12(): Bool = isTwelve4(inc4(mkEleven4()))

    @Test
    def test4Inc13(): Bool = isThirteen4(inc4(mkTwelve4()))

    @Test
    def test4Inc14(): Bool = isFourteen4(inc4(mkThirteen4()))

    @Test
    def test4Inc15(): Bool = isFifteen4(inc4(mkFourteen4()))

    @Test
    def test4Inc16(): Bool = isZero4(inc4(mkFifteen4()))

    @Test
    def test4Add01(): Bool = isZero4(add4(mkZero4(), mkZero4()))

    @Test
    def test4Add02(): Bool = isOne4(add4(mkZero4(), mkOne4()))

    @Test
    def test4Add03(): Bool = isOne4(add4(mkOne4(), mkZero4()))

    @Test
    def test4Add04(): Bool = isTwo4(add4(mkOne4(), mkOne4()))

    @Test
    def test4Add05(): Bool = isSix4(add4(mkFour4(), mkTwo4()))

    @Test
    def test4Add06(): Bool = isEight4(add4(mkSeven4(), mkOne4()))

    @Test
    def test4Add07(): Bool = isEight4(add4(mkSeven4(), mkOne4()))

    @Test
    def test4Add08(): Bool = isEight4(add4(mkOne4(), mkSeven4()))

    @Test
    def test4Add09(): Bool = isEight4(add4(mkFour4(), mkFour4()))

    @Test
    def test4Add10(): Bool = isNine4(add4(mkFour4(), mkFive4()))

    @Test
    def test4Add11(): Bool = isTen4(add4(mkSeven4(), mkThree4()))

    @Test
    def test4Add12(): Bool = isTen4(add4(mkTwo4(), mkEight4()))

    @Test
    def test4Add13(): Bool = isFifteen4(add4(mkSeven4(), mkEight4()))

    @Test
    def test4Add14(): Bool = isFifteen4(add4(mkEight4(), mkSeven4()))

    @Test
    def test4Add15(): Bool = isFourteen4(add4(mkFourteen4(), mkZero4()))

    @Test
    def test4Add16(): Bool = isSeven4(add4(mkSeven4(), mkZero4()))

    @Test
    def test4Add17(): Bool = isEight4(add4(mkZero4(), mkEight4()))

    @Test
    def test4Add18(): Bool = isThirteen4(add4(mkFifteen4(), add4(mkFifteen4(), mkFifteen4())))

    @Test
    def test4Add19(): Bool = isTwelve4(add4(add4(mkSeven4(), mkSeven4()), add4(mkSeven4(), mkSeven4())))

    @Test
    def test4Add20(): Bool = isZero4(add4(add4(mkZero4(), mkZero4()), add4(mkZero4(), mkZero4())))

    @Test
    def test4Add21(): Bool = isZero4(add4(add4(mkEight4(), mkEight4()), add4(mkEight4(), mkEight4())))

    @Test
    def test4Add24(): Bool = isFourteen4(add4(add4(mkTen4(), mkEleven4()), add4(mkTwelve4(), mkThirteen4())))

    @Test
    def test4Add25(): Bool = isFifteen4(add4(add4(add4(add4(mkOne4(), mkTwo4()), mkThree4()), mkFour4()), mkFive4()))

    ///
    /// Flix's type system can be used to prove laws of propositional calculus.
    ///
    def _deMorgan01(x: B[a], y: B[b]): Bool = mkNot(mkAnd(x, y)) == mkOr(mkNot(x), mkNot(y))
    def _deMorgan02(x: B[a], y: B[b]): Bool = mkNot(mkOr(x, y)) == mkAnd(mkNot(x), mkNot(y))

    def _assoc01(x: B[a], y: B[b], z: B[c]): Bool = mkOr(x, mkOr(y, z)) == mkOr(mkOr(x, y), z)
    def _assoc02(x: B[a], y: B[b], z: B[c]): Bool = mkAnd(x, mkAnd(y, z)) == mkAnd(mkAnd(x, y), z)

    def _distr01(x: B[a], y: B[b], z: B[c]): Bool = mkAnd(x, mkOr(y, z)) == mkOr(mkAnd(x, y), mkAnd(x, z))
    def _distr02(x: B[a], y: B[b], z: B[c]): Bool = mkOr(x, mkAnd(y, z)) == mkAnd(mkOr(x, y), mkOr(x, z))

    def _tautologyAnd(x: B[a]): Bool = x == mkAnd(x, x)
    def _tautologyOr(x: B[a]): Bool = x == mkOr(x, x)

    def assertEqual(_: B[a], _: B[a]): Bool = true
    def assertTrue(_: B[true]): Bool = true
    def assertFalse(_: B[false]): Bool = true
    def assertXor(_: B[a], _: B[not a]): Bool = true

    def mkTrue(): B[true] = B.B
    def mkFalse(): B[false] = B.B
    def mkNot(_: B[b]): B[not b] = B.B
    def mkOr(_: B[a], _: B[b]): B[b or a] = B.B
    def mkAnd(_: B[a], _: B[b]): B[b and a] = B.B
    def mkXor(_: B[a], _: B[b]): B[(b and (not a)) or (a and (not b))] = B.B

    def mkZero2(): I2[false, false] = I2.I2
    def mkOne2(): I2[false, true] = I2.I2
    def mkTwo2(): I2[true, false] = I2.I2
    def mkThree2(): I2[true, true] = I2.I2

    def isZero2(_: I2[false, false]): Bool = true
    def isOne2(_: I2[false, true]): Bool = true
    def isTwo2(_: I2[true, false]): Bool = true
    def isThree2(_: I2[true, true]): Bool = true

    def bitwiseNot2(_: I2[a, b]): I2[not a, not b] = I2.I2
    def bitwiseOr2(_: I2[a1, b1], _: I2[a2, b2]): I2[a1 or a2, b1 or b2] = I2.I2
    def bitwiseAnd2(_: I2[a1, b1], _: I2[a2, b2]): I2[a1 and a2, b1 and b2] = I2.I2
    def swap2(_: I2[a, b]): I2[b, a] = I2.I2
    def inc2(_: I2[a, b]): I2[((a and (not b)) or (b and (not a))), (not b)] = I2.I2
    def add2(_: I2[a1, b1], _: I2[a2, b2]):
      I2[((((a1 and (not a2)) or (a2 and (not a1))) and (not (b1 and b2))) or (((a1 and a2) or ((not a1) and (not a2))) and (b1 and b2))),
      ((b1 and (not b2)) or (b2 and (not b1)))] = I2.I2
    def mul2(_: I2[a1, b1], _: I2[a2, b2]):
      I2[(((a1 and b2) and (not (b1 and a2))) or ((b1 and a2) and (not (a1 and b2)))),
      (b1 and b2)] = I2.I2

    def mkZero4(): I4[false, false, false, false] = I4.I4
    def mkOne4(): I4[false, false, false, true] = I4.I4
    def mkTwo4(): I4[false, false, true, false] = I4.I4
    def mkThree4(): I4[false, false, true, true] = I4.I4
    def mkFour4(): I4[false, true, false, false] = I4.I4
    def mkFive4(): I4[false, true, false, true] = I4.I4
    def mkSix4(): I4[false, true, true, false] = I4.I4
    def mkSeven4(): I4[false, true, true, true] = I4.I4
    def mkEight4(): I4[true, false, false, false] = I4.I4
    def mkNine4(): I4[true, false, false, true] = I4.I4
    def mkTen4(): I4[true, false, true, false] = I4.I4
    def mkEleven4(): I4[true, false, true, true] = I4.I4
    def mkTwelve4(): I4[true, true, false, false] = I4.I4
    def mkThirteen4(): I4[true, true, false, true] = I4.I4
    def mkFourteen4(): I4[true, true, true, false] = I4.I4
    def mkFifteen4(): I4[true, true, true, true] = I4.I4

    def isZero4(_: I4[false, false, false, false]): Bool = true
    def isOne4(_: I4[false, false, false, true]): Bool = true
    def isTwo4(_: I4[false, false, true, false]): Bool = true
    def isThree4(_: I4[false, false, true, true]): Bool = true
    def isFour4(_: I4[false, true, false, false]): Bool = true
    def isFive4(_: I4[false, true, false, true]): Bool = true
    def isSix4(_: I4[false, true, true, false]): Bool = true
    def isSeven4(_: I4[false, true, true, true]): Bool = true
    def isEight4(_: I4[true, false, false, false]): Bool = true
    def isNine4(_: I4[true, false, false, true]): Bool = true
    def isTen4(_: I4[true, false, true, false]): Bool = true
    def isEleven4(_: I4[true, false, true, true]): Bool = true
    def isTwelve4(_: I4[true, true, false, false]): Bool = true
    def isThirteen4(_: I4[true, true, false, true]): Bool = true
    def isFourteen4(_: I4[true, true, true, false]): Bool = true
    def isFifteen4(_: I4[true, true, true, true]): Bool = true

    def _bitwiseNot4(_: I4[a, b, c, d]): I4[not a, not b, not c, not d] = I4.I4
    def _bitwiseOr4(_: I4[a1, b1, c1, d1], _: I4[a2, b2, c2, d2]):
      I4[a1 or a2, b1 or b2, c1 or c2, d1 or d2] = I4.I4
    def _bitwiseAnd4(_: I4[a1, b1, c1, d1], _: I4[a2, b2, c2, d2]):
      I4[a1 and a2, b1 and b2, c1 and c2, d1 and d2] = I4.I4
    def inc4(_: I4[a, b, c, d]): I4[a xor (b and c and d), b xor (c and d), c xor d, not d] = I4.I4
    def add4(_: I4[a1, b1, c1, d1], _: I4[a2, b2, c2, d2]):
      I4[(a1 xor a2 xor ((b1 and b2) or ((b1 xor b2) and ((c1 and c2) or ((c1 xor c2) and (d1 and d2)))))),
        (b1 xor b2 xor ((c1 and c2) or ((c1 xor c2) and (d1 and d2)))),
        (d1 and d2) xor c1 xor c2,
        (d1 xor d2)] = I4.I4
}
