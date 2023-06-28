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
    def testTwoBitInt01(): Bool = isZero(mkZero())

    @Test
    def testTwoBitInt02(): Bool = isOne(mkOne())

    @Test
    def testTwoBitInt03(): Bool = isTwo(mkTwo())

    @Test
    def testTwoBitInt04(): Bool = isThree(mkThree())

    @Test
    def testBitwiseNot01(): Bool = isZero(bitwiseNot(mkThree()))

    @Test
    def testBitwiseNot02(): Bool = isOne(bitwiseNot(mkTwo()))

    @Test
    def testBitwiseNot03(): Bool = isTwo(bitwiseNot(mkOne()))

    @Test
    def testBitwiseNot04(): Bool = isThree(bitwiseNot(mkZero()))

    @Test
    def testBitwiseNot05(): Bool = isZero(bitwiseNot(bitwiseNot(mkZero())))

    @Test
    def testBitwiseNot06(): Bool = isThree(bitwiseNot(bitwiseNot(mkThree())))

    @Test
    def testBitwiseAnd01(): Bool = isZero(bitwiseAnd(mkThree(), mkZero()))

    @Test
    def testBitwiseAnd02(): Bool = isZero(bitwiseAnd(mkZero(), mkThree()))

    @Test
    def testBitwiseAnd03(): Bool = isZero(bitwiseAnd(mkOne(), mkTwo()))

    @Test
    def testBitwiseAnd04(): Bool = isOne(bitwiseAnd(mkOne(), mkThree()))

    @Test
    def testBitwiseAnd05(): Bool = isTwo(bitwiseAnd(mkThree(), mkTwo()))

    @Test
    def testBitwiseAnd06(): Bool = isThree(bitwiseAnd(mkThree(), mkThree()))

    @Test
    def testBitwiseOr01(): Bool = isZero(bitwiseOr(mkZero(), mkZero()))

    @Test
    def testBitwiseOr02(): Bool = isOne(bitwiseOr(mkZero(), mkOne()))

    @Test
    def testBitwiseOr03(): Bool = isThree(bitwiseOr(mkOne(), mkTwo()))

    @Test
    def testBitwiseOr04(): Bool = isThree(bitwiseOr(mkTwo(), mkOne()))

    @Test
    def testBitwiseOr05(): Bool = isThree(bitwiseOr(mkThree(), mkZero()))

    @Test
    def testBitwiseOr06(): Bool = isThree(bitwiseOr(mkZero(), mkThree()))

    @Test
    def testBitwiseOr07(): Bool = isThree(bitwiseOr(mkThree(), mkOne()))

    @Test
    def testBitwiseOr08(): Bool = isThree(bitwiseOr(mkTwo(), mkThree()))

    @Test
    def testSwap01(): Bool = isZero(swap(mkZero()))

    @Test
    def testSwap02(): Bool = isTwo(swap(mkOne()))

    @Test
    def testSwap03(): Bool = isOne(swap(mkTwo()))

    @Test
    def testSwap04(): Bool = isThree(swap(mkThree()))

    @Test
    def testNestedBitwiseNotAndOr01(): Bool =
      isThree(bitwiseOr(bitwiseAnd(mkThree(), mkOne()), bitwiseNot(mkOne())))

    @Test
    def testNestedBitwiseNotAndOr02(): Bool =
      isThree(bitwiseOr(bitwiseAnd(mkThree(), bitwiseNot(mkThree())), mkThree()))

    @Test
    def testNestedBitwiseNotAndOr03(): Bool =
      isTwo(bitwiseAnd(bitwiseOr(mkThree(), mkOne()), bitwiseNot(bitwiseNot(mkTwo()))))

    @Test
    def testNestedBitwiseNotAndOr04(): Bool =
      isZero(bitwiseNot(bitwiseAnd(bitwiseOr(mkThree(), mkZero()), mkThree())))

    @Test
    def testNestedBitwiseNotAndOr05(): Bool =
      isThree(bitwiseNot(bitwiseAnd(bitwiseAnd(mkThree(), mkTwo()), bitwiseAnd(mkOne(), mkTwo()))))

    @Test
    def testNestedBitwiseNotAndOr06(): Bool =
      isThree(bitwiseOr(bitwiseOr(mkThree(), mkZero()), bitwiseOr(bitwiseNot(mkTwo()), mkTwo())))

    @Test
    def testNestedBitwiseNotAndOr07(): Bool = {
      let three = bitwiseOr(bitwiseAnd(mkThree(), mkOne()), mkTwo());
      let two = bitwiseAnd(bitwiseOr(mkThree(), mkOne()), mkTwo());
      let one = bitwiseAnd(bitwiseNot(mkTwo()), bitwiseNot(bitwiseNot(mkThree())));
      let zero = bitwiseAnd(three, bitwiseAnd(two, bitwiseAnd(one, mkZero())));
      isThree(bitwiseNot(bitwiseAnd(zero, bitwiseOr(one, bitwiseOr(two, three)))))
    }

    @Test
    def testInc01(): Bool = isOne(inc(mkZero()))

    @Test
    def testInc02(): Bool = isTwo(inc(mkOne()))

    @Test
    def testInc03(): Bool = isThree(inc(mkTwo()))

    @Test
    def testInc04(): Bool = isZero(inc(mkThree()))

    @Test
    def testInc05(): Bool = isTwo(inc(inc(mkZero())))

    @Test
    def testInc06(): Bool = isOne(inc(inc(inc(mkTwo()))))

    @Test
    def testInc07(): Bool = isZero(inc(inc(inc(inc(mkZero())))))

    @Test
    def testAdd01(): Bool = isZero(add(mkZero(), mkZero()))

    @Test
    def testAdd02(): Bool = isOne(add(mkZero(), mkOne()))

    @Test
    def testAdd03(): Bool = isOne(add(mkOne(), mkZero()))

    @Test
    def testAdd04(): Bool = isZero(add(mkTwo(), mkTwo()))

    @Test
    def testAdd05(): Bool = isThree(add(mkTwo(), mkOne()))

    @Test
    def testAdd06(): Bool = isThree(add(mkOne(), mkTwo()))

    @Test
    def testAdd07(): Bool = isOne(add(mkThree(), mkTwo()))

    @Test
    def testAdd08(): Bool = isOne(add(mkTwo(), mkThree()))

    @Test
    def testAdd09(): Bool = isZero(add(mkThree(), mkOne()))

    @Test
    def testAdd10(): Bool = isZero(add(mkOne(), mkThree()))

    @Test
    def testAdd11(): Bool = isThree(add(mkThree(), mkZero()))

    @Test
    def testAdd12(): Bool = isThree(add(mkZero(), mkThree()))

    @Test
    def testAdd13(): Bool = isOne(add(mkThree(), add(mkThree(), mkThree())))

    @Test
    def testAdd14(): Bool = isOne(add(add(mkThree(), mkThree()), mkThree()))

    @Test
    def testAdd15(): Bool = isTwo(add(mkTwo(), add(mkTwo(), mkTwo())))

    @Test
    def testAdd16(): Bool = isTwo(add(add(mkTwo(), mkTwo()), mkTwo()))

    @Test
    def testAdd17(): Bool = isTwo(add(add(mkZero(), mkOne()), add(mkTwo(), mkThree())))

    @Test
    def testAdd18(): Bool = isThree(add(add(mkTwo(), mkOne()), add(mkOne(), mkThree())))

    @Test
    def testAdd19(): Bool = isOne(add(mkZero(), add(mkZero(), mkOne())))

    @Test
    def testAdd20(): Bool = isThree(add(mkThree(), add(add(mkThree(), mkThree()), add(mkThree(), mkThree()))))

    @Test
    def testMul01(): Bool = isZero(mul(mkZero(), mkZero()))

    @Test
    def testMul02(): Bool = isOne(mul(mkOne(), mkOne()))

    @Test
    def testMul03(): Bool = isZero(mul(mkTwo(), mkTwo()))

    @Test
    def testMul04(): Bool = isOne(mul(mkThree(), mkThree()))

    @Test
    def testMul05(): Bool = isZero(mul(mkZero(), mkOne()))

    @Test
    def testMul06(): Bool = isZero(mul(mkOne(), mkZero()))

    @Test
    def testMul07(): Bool = isZero(mul(mkTwo(), mkZero()))

    @Test
    def testMul08(): Bool = isZero(mul(mkZero(), mkTwo()))

    @Test
    def testMul09(): Bool = isZero(mul(mkZero(), mkThree()))

    @Test
    def testMul10(): Bool = isZero(mul(mkThree(), mkZero()))

    @Test
    def testMul11(): Bool = isTwo(mul(mkOne(), mkTwo()))

    @Test
    def testMul12(): Bool = isTwo(mul(mkTwo(), mkOne()))

    @Test
    def testMul13(): Bool = isTwo(mul(mkThree(), mkTwo()))

    @Test
    def testMul14(): Bool = isTwo(mul(mkTwo(), mkThree()))

    @Test
    def testMul15(): Bool = isThree(mul(mkThree(), mkOne()))

    @Test
    def testMul16(): Bool = isThree(mul(mkOne(), mkThree()))

    @Test
    def testMul17(): Bool = isTwo(mul(mul(mkOne(), mkThree()), mkTwo()))

    @Test
    def testMul18(): Bool = isOne(mul(mul(mkThree(), mkThree()), mul(mkThree(), mkThree())))

    @Test
    def testMul19(): Bool = isZero(mul(mul(mul(mkOne(), mkThree()), mkTwo()), mkZero()))

    @Test
    def testMul20(): Bool = isTwo(mul(mul(mkTwo(), mul(mkOne(), mkThree())), mul(mkThree(), mkOne())))

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

    def mkZero(): I2[false, false] = I2.I2
    def mkOne(): I2[false, true] = I2.I2
    def mkTwo(): I2[true, false] = I2.I2
    def mkThree(): I2[true, true] = I2.I2

    def isZero(_: I2[false, false]): Bool = true
    def isOne(_: I2[false, true]): Bool = true
    def isTwo(_: I2[true, false]): Bool = true
    def isThree(_: I2[true, true]): Bool = true

    def bitwiseNot(_: I2[a, b]): I2[not a, not b] = I2.I2
    def bitwiseOr(_: I2[a1, b1], _: I2[a2, b2]): I2[a1 or a2, b1 or b2] = I2.I2
    def bitwiseAnd(_: I2[a1, b1], _: I2[a2, b2]): I2[a1 and a2, b1 and b2] = I2.I2
    def swap(_: I2[a, b]): I2[b, a] = I2.I2
    def inc(_: I2[a, b]): I2[((a and (not b)) or (b and (not a))), (not b)] = I2.I2
    def add(_: I2[a1, b1], _: I2[a2, b2]):
      I2[((((a1 and (not a2)) or (a2 and (not a1))) and (not (b1 and b2))) or (((a1 and a2) or ((not a1) and (not a2))) and (b1 and b2))),
      ((b1 and (not b2)) or (b2 and (not b1)))] = I2.I2
    def mul(_: I2[a1, b1], _: I2[a2, b2]):
      I2[(((a1 and b2) and (not (b1 and a2))) or ((b1 and a2) and (not (a1 and b2)))),
      (b1 and b2)] = I2.I2

}
