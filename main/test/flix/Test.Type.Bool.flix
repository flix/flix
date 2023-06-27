mod Test.Type.Bool {

    enum B[_: Bool] with Eq {
        case A
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


    def assertEqual(_: B[a], _: B[a]): Bool = true
    def assertTrue(_: B[true]): Bool = true
    def assertFalse(_: B[false]): Bool = true
    def assertXor(_: B[a], _: B[not a]): Bool = true

    def mkTrue(): B[true] = B.A
    def mkFalse(): B[false] = B.A
    def mkNot(_: B[b]): B[not b] = B.A
    def mkOr(_: B[a], _: B[b]): B[b or a] = B.A
    def mkAnd(_: B[a], _: B[b]): B[b and a] = B.A
    def mkXor(_: B[a], _: B[b]): B[(b and (not a)) or (a and (not b))] = B.A

}
