enum B[_: Bool] with Eq {
    case A
}

def assertEqual(_: B[a], _: B[a]): Bool = true
def assertUnequal(_ : B[a], _ : B[not a]): Bool = true
def assertTrue(_: B[true]): Bool = true
def assertFalse(_: B[false]): Bool = true

def assertAllTrue(xs: List[B[true]]): Bool = List.forAll(x -> assertTrue(x), xs)
def assertAllFalse(xs: List[B[false]]): Bool = List.forAll(x -> assertFalse(x), xs)

def mkTrue(): B[true] = B.A
def mkFalse(): B[false] = B.A
def mkNot(_: B[b]): B[not b] = B.A
def mkOr(_: B[a], _: B[b]): B[b or a] = B.A
def mkAnd(_: B[a], _: B[b]): B[b and a] = B.A

def boolToString(b: Bool): String = match b {
    case true => "true"
    case false => "false"
}


@Test
def test01(): Bool = assertTrue(mkTrue())

@Test
def test02(): Bool = assertFalse(mkFalse())

@Test
def test03(): Bool = assertEqual(mkFalse(), mkFalse())

@Test
def test04(): Bool = assertEqual(mkTrue(), mkTrue())

@Test
def test05(): Bool = assertUnequal(mkTrue(), mkFalse())

@Test
def test06(): Bool = assertUnequal(mkFalse(), mkTrue())

@Test
def test07(): Bool = assertFalse(mkNot(mkTrue()))

@Test
def test08(): Bool = assertTrue(mkNot(mkFalse()))

@Test
def test09(): Bool = assertTrue(mkNot(mkNot(mkTrue())))

@Test
def test10(): Bool = assertFalse(mkNot(mkNot(mkFalse())))

@Test
def test11(): Bool = assertFalse(mkNot(mkNot(mkNot(mkTrue()))))

@Test
def test12(): Bool = assertTrue(mkNot(mkNot(mkNot(mkFalse()))))

//Function composition

@Test
def test13(): Bool = let id = mkNot >> mkNot; assertTrue(id(mkTrue()))

@Test
def test14(): Bool = let id = mkNot >> mkNot; assertTrue(id(mkTrue()))

@Test
def test15(): Bool = let notId = mkNot >> mkNot >> mkNot; assertFalse(notId(mkTrue()))

@Test
def test16(): Bool = let notId = mkNot >> mkNot >> mkNot; assertFalse(notId(mkTrue()))


def doubleSwaps(index: Int32, k: B[b] -> B[not b]): B[b] -> B[not b] = {
    if (index == 0) k else doubleSwaps(index - 1, mkNot >> mkNot >> k)
}

@Test
def test17(): Bool = let f = doubleSwaps(0, mkNot); assertTrue(f(mkFalse()))

@Test
def test18(): Bool = let f = doubleSwaps(5, mkNot); assertTrue(f(mkFalse()))

@Test
def test19(): Bool = let f = doubleSwaps(100, mkNot); assertTrue(f(mkFalse()))

@Test
def test20(): Bool = {
    let f = doubleSwaps(20, mkNot);
    let g = doubleSwaps(21, mkNot);
    assertTrue(f(g(mkTrue())))
}

//mkAnd() and mkOr() cases

@Test
def test21(): Bool = assertFalse(mkAnd(mkFalse(), mkFalse()))

@Test
def test22(): Bool = assertFalse(mkAnd(mkFalse(), mkTrue()))

@Test
def test23(): Bool = assertFalse(mkAnd(mkTrue(), mkFalse()))

@Test
def test24(): Bool = assertTrue(mkAnd(mkTrue(), mkTrue()))

@Test
def test25(): Bool = assertFalse(mkOr(mkFalse(), mkFalse()))

@Test
def test26(): Bool = assertTrue(mkOr(mkTrue(), mkFalse()))

@Test
def test27(): Bool = assertTrue(mkOr(mkFalse(), mkTrue()))

@Test
def test28(): Bool = assertTrue(mkOr(mkFalse(), mkTrue()))


//Nested mkAnd(), mkOr(), mkNot() cases
@Test
def test29(): Bool = assertTrue(mkNot(mkAnd(mkTrue(), mkFalse())))

@Test
def test30(): Bool = assertFalse(mkNot(mkAnd(mkTrue(), mkTrue())))

@Test
def test31(): Bool = assertTrue(mkNot(mkOr(mkFalse(), mkFalse())))

@Test
def test32(): Bool = assertFalse(mkNot(mkOr(mkFalse(), mkTrue())))

@Test
def test33(): Bool = assertEqual(mkOr(mkFalse(), mkFalse()), mkAnd(mkTrue(), mkFalse()))

@Test
def test34(): Bool = assertEqual(mkOr(mkFalse(), mkTrue()), mkAnd(mkTrue(), mkTrue()))

@Test
def test35(): Bool = assertEqual(mkNot(mkAnd(mkTrue(), mkFalse())), mkNot(mkOr(mkFalse(), mkFalse())))

@Test
def test36(): Bool = assertTrue(mkAnd(mkAnd(mkTrue(), mkTrue()), mkOr(mkFalse(), mkNot(mkNot(mkTrue())))))


//Operations on lists

@Test
def test37(): Bool = {
    let xs = mkTrue() :: mkTrue() :: mkTrue() :: mkTrue() :: Nil;
    assertAllTrue(xs)
}

@Test
def test38(): Bool = {
    let f = doubleSwaps(10, mkNot);
    let xs = mkTrue() :: mkNot(mkFalse()) :: f(mkFalse()) :: Nil;
    assertAllTrue(xs)
}

@Test
def test39(): Bool = {
    let xs = mkTrue() :: mkTrue() :: mkTrue() :: mkTrue() :: Nil;
    assertAllFalse(List.map(x->mkNot(x), xs))
}


def notList (xs: List[B[b]]): List[B[not b]] = match xs{
    case Nil     => Nil
    case y :: ys => mkNot(y) :: notList(ys)
}

@Test
def test40(): Bool = {
    let xs = mkTrue() :: mkTrue() :: mkTrue() :: mkTrue() :: Nil;
    assertAllFalse(notList(xs))
}


// almost a spicy test, but ran into bugs. could be possible?

// @Test
// def test41(): Bool = {
//     let f = (c, k) -> if (c == 0) k else f(c - 1, k >> k >> k);
//     let xs = mkTrue() :: mkTrue() :: mkTrue() :: mkTrue() :: Nil;
//     let g = f(5, notList);
//     assertAllFalse(g(xs))
// }
