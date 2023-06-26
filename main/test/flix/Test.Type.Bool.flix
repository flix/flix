
enum B[_: Bool] with Eq {
    case A
}

def assertEqual(_: B[a], _: B[a]): Bool = true
def assertUnequal(_ : B[a], _ : B[not a]): Bool = true
def assertTrue(_: B[true]): Bool = true
def assertFalse(_: B[false]): Bool = true
def assertXor(_: B[a], _: B[not a]): Bool = true

def mkTrue(): B[true] = B.A
def mkFalse(): B[false] = B.A
def mkNot(_: B[b]): B[not b] = B.A
def mkOr(_: B[a], _: B[b]): B[b or a] = B.A
def mkAnd(_: B[a], _: B[b]): B[b and a] = B.A
def mkXor(_: B[a], _: B[b]): B[(b and (not a)) or (a and (not b))] = B.A

def boolToString(b: Bool): String = match b {
    case true => "true"
    case false => "false"
}


@Test
def testmkTrue01(): Bool = assertTrue(mkTrue())

@Test
def testmkTrue02(): Bool = assertEqual(mkTrue(), mkTrue())

@Test
def testmkFalse01(): Bool = assertFalse(mkFalse())

@Test
def testmkFalse02(): Bool = assertEqual(mkFalse(), mkFalse())


@Test
def testTrueFalse01(): Bool = assertUnequal(mkTrue(), mkFalse())

@Test
def testTrueFalse02(): Bool = assertUnequal(mkFalse(), mkTrue())

@Test
def testmkNot01(): Bool = assertFalse(mkNot(mkTrue()))

@Test
def testmkNot02(): Bool = assertTrue(mkNot(mkFalse()))

@Test
def testmkNot03(): Bool = assertTrue(mkNot(mkNot(mkTrue())))

@Test
def testmkNot04(): Bool = assertFalse(mkNot(mkNot(mkFalse())))

@Test
def testtestmkNot05(): Bool = assertFalse(mkNot(mkNot(mkNot(mkTrue()))))

@Test
def testmkNot06(): Bool = assertTrue(mkNot(mkNot(mkNot(mkFalse()))))

@Test
def testFuncComposition01(): Bool = let id = mkNot >> mkNot; assertTrue(id(mkTrue()))

@Test
def testFuncComposition02(): Bool = let notId = mkNot >> mkNot >> mkNot; assertFalse(notId(mkTrue()))

@Test
def testMkAnd01(): Bool = assertFalse(mkAnd(mkFalse(), mkFalse()))

@Test
def testMkAnd02(): Bool = assertFalse(mkAnd(mkFalse(), mkTrue()))

@Test
def testMkAnd03(): Bool = assertFalse(mkAnd(mkTrue(), mkFalse()))

@Test
def testMkAnd04(): Bool = assertTrue(mkAnd(mkTrue(), mkTrue()))

@Test
def testMkOr01(): Bool = assertFalse(mkOr(mkFalse(), mkFalse()))

@Test
def testMkOr02(): Bool = assertTrue(mkOr(mkTrue(), mkFalse()))

@Test
def testMkOr03(): Bool = assertTrue(mkOr(mkFalse(), mkTrue()))

@Test
def testMkOr04(): Bool = assertTrue(mkOr(mkTrue(), mkTrue()))

@Test
def testMkXor01(): Bool = assertTrue(mkXor(mkFalse(), mkTrue()))

@Test
def testMkXor02(): Bool = assertTrue(mkXor(mkTrue(), mkFalse()))

@Test
def testMkXor03(): Bool = assertFalse(mkXor(mkFalse(), mkFalse()))

@Test
def mkXor04(): Bool = assertFalse(mkXor(mkTrue(), mkTrue()))

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




enum D[_: Bool, _: Bool] with Eq {
  case A
}

///2-bit integers
def mkZero(): D[false, false] = D.A
def mkOne(): D[false, true] = D.A
def mkTwo(): D[true, false] = D.A
def mkThree(): D[true, true] = D.A

def isZero(_: D[false, false]): Bool = true
def isOne(_: D[false, true]): Bool = true
def isTwo(_: D[true, false]): Bool = true
def isThree(_: D[true, true]): Bool = true

def bitwiseNot(_: D[a, b]): D[not a, not b] = D.A
def bitwiseOr(_: D[a, b], _: D[c, d]): D[a or c, b or d] = D.A
def bitwiseAnd(_: D[a, b], _: D[c, d]): D[a and c, b and d] = D.A

@Test
def testReadNum01(): Bool = isZero(mkZero())

@Test
def testReadNum02(): Bool = isOne(mkOne())

@Test
def testReadNum03(): Bool = isTwo(mkTwo())

@Test
def testReadNum04(): Bool = isThree(mkThree())

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
