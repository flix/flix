mod Test.Exp.Force {

    use Assert.{assertEq, fail}

    @Test
    def testUnitForce01(): Unit \ Assert =
        let x = lazy ();
        assertEq(expected = (), force x)

    @Test
    def testBoolForce01(): Unit \ Assert =
        let x = lazy true;
        assertEq(expected = true, force x)

    @Test
    def testBoolForce02(): Unit \ Assert =
        let x = lazy false;
        assertEq(expected = false, force x)

    @Test
    def testCharForce01(): Unit \ Assert =
        let x = lazy 'a';
        assertEq(expected = 'a', force x)

    @Test
    def testFloat32Force01(): Unit \ Assert =
        let v = 12.34f32;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testFloat64Force01(): Unit \ Assert =
        let v = 12.34f64;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testBigDecimalForce01(): Unit \ Assert =
        let v = 12.34ff;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testInt8Force01(): Unit \ Assert =
        let v = 12i8;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testInt16Force01(): Unit \ Assert =
        let v = 12i16;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testInt32Force01(): Unit \ Assert =
        let v = 12i32;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testInt64Force01(): Unit \ Assert =
        let v = 12i64;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testBigIntForce01(): Unit \ Assert =
        let v = 12ii;
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testStringForce01(): Unit \ Assert =
        let v = "string";
        let x = lazy v;
        assertEq(expected = v, force x)

    @Test
    def testArrayOfIntForce01(): Unit \ (Assert + IO) =
        let v = Array#{1, 2} @ Static;
        let x = lazy v;
        assertEq(expected = %%ARRAY_LOAD%%(v, 0), %%ARRAY_LOAD%%(force x, 0))

    @Test
    def testArrayOfStringForce01(): Unit \ (Assert + IO) =
        let v = Array#{"str", "str2"} @ Static;
        let x = lazy v;
        assertEq(expected = %%ARRAY_LOAD%%(v, 0), %%ARRAY_LOAD%%(force x, 0))

    @Test
    def testTupleForce01(): Unit \ Assert =
        let x = lazy ("str", "str2");
        let y = force x;
        match y {
            case (s, _) => assertEq(expected = "str", s)
            case _      => fail("unexpected value")
        }

    @Test
    def testNestedEnumForce01(): Unit \ Assert =
        let x = lazy (lazy 123);
        assertEq(expected = 123, force (force x))

    @Test
    def testNestedForce02(): Unit \ Assert =
        let x = lazy (n -> if ((force n) < 0) lazy "neg" else lazy "pos");
        assertEq(expected = "neg", force ((force x)(lazy -2)))

    @Test
    def testNestedForce03(): Unit \ Assert =
        let x = Poly.Both(lazy 2, lazy "2");
        match x {
            case Poly.Both(a, _) => assertEq(expected = 2, force a)
            case _               => fail("unexpected value")
        }

    @Test
    def testNestedForce04(): Unit \ Assert =
        let x = lazy 15;
        assertEq(expected = Poly.Both(20, 25), Poly.Both(5 + force x, 10 + force x))

    @Test
    def testNestedForce05(): Unit \ Assert =
        let x = lazy (Poly.This(lazy 5));
        match (force x) {
            case Poly.This(a) => assertEq(expected = 5, force a)
            case _            => fail("unexpected value")
        }

    @Test
    def testNestedForce01(): Unit \ Assert =
        let x = lazy Card.Card(Suit.Hearts, Rank.N(4));
        match (force x) {
            case Card.Card(Suit.Hearts, Rank.N(n)) => assertEq(expected = 4, n)
            case _                                 => fail("unexpected value")
        }

    @Test
    def testPolyForce01(): Unit \ Assert =
        let x = lazy Poly.This(123);
        let y = force x;
        match y {
            case Poly.This(n) => assertEq(expected = 123, n)
            case _            => fail("unexpected value")
        }

    @Test
    def testPolyForce02(): Unit \ Assert =
        let x = lazy Poly.That("str");
        let y = force x;
        match y {
            case Poly.That(s) => assertEq(expected = "str", s)
            case _            => fail("unexpected value")
        }

    @Test
    def testPolyForce03(): Unit \ Assert =
        let x = lazy Poly.Both("str", 123);
        let y = force x;
        match y {
            case Poly.Both(_, n) => assertEq(expected = 123, n)
            case _               => fail("unexpected value")
        }

    @Test
    def testOptionForce01(): Unit \ Assert =
        let x = lazy Poly.None;
        assertEq(expected = (Poly.None: Poly[Unit, Unit]), force x)

    @Test
    def testPrecedenceForce01(): Unit \ Assert =
        let x = lazy 2;
        let y = force x + 1;
        assertEq(expected = 3, y);
        assertEq(expected = 2, force x)

    @Test
    def testPrecedenceForce02(): Unit \ Assert =
        let x = {f = 2};
        let y = lazy x#f;
        let z = force y;
        assertEq(expected = 2, z)

    @Test
    def testPrecedenceForce03(): Unit \ Assert =
        let x = {l = lazy 2};
        let y = force x#l;
        assertEq(expected = 2, y)

    @Test
    def testPrecedenceForce04(): Unit \ Assert =
        let x = ((lazy 2) : Lazy[Int32]);
        let y = ((force x) : Int32);
        assertEq(expected = 2, y)

    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int32), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    instance Eq[Poly[a, b]] with Eq[a], Eq[b] {
        pub def eq(p1: Poly[a, b], p2: Poly[a, b]): Bool = match (p1, p2) {
            case (Poly.None, Poly.None) => true
            case (Poly.This(x1), Poly.This(y1)) => x1 == y1
            case (Poly.That(x1), Poly.That(y1)) => x1 == y1
            case (Poly.Both(x1, x2), Poly.Both(y1, y2)) => x1 == y1 and x2 == y2
            case _ => false
        }
    }

}
