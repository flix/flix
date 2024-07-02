mod Test.Exp.Force {

    @test
    def testUnitForce01(): Bool =
        let x = lazy ();
        () == force x

    @test
    def testBoolForce01(): Bool =
        let x = lazy true;
        true == force x

    @test
    def testBoolForce02(): Bool =
        let x = lazy false;
        false == force x

    @test
    def testCharForce01(): Bool =
        let x = lazy 'a';
        'a' == force x

    @test
    def testFloat32Force01(): Bool =
        let v = 12.34f32;
        let x = lazy v;
        v == force x

    @test
    def testFloat64Force01(): Bool =
        let v = 12.34f64;
        let x = lazy v;
        v == force x

    @test
    def testBigDecimalForce01(): Bool =
        let v = 12.34ff;
        let x = lazy v;
        v == force x

    @test
    def testInt8Force01(): Bool =
        let v = 12i8;
        let x = lazy v;
        v == force x

    @test
    def testInt16Force01(): Bool =
        let v = 12i16;
        let x = lazy v;
        v == force x

    @test
    def testInt32Force01(): Bool =
        let v = 12i32;
        let x = lazy v;
        v == force x

    @test
    def testInt64Force01(): Bool =
        let v = 12i64;
        let x = lazy v;
        v == force x

    @test
    def testBigIntForce01(): Bool =
        let v = 12ii;
        let x = lazy v;
        v == force x

    @test
    def testStringForce01(): Bool =
        let v = "string";
        let x = lazy v;
        v == force x

    @test
    def testArrayOfIntForce01(): Bool \ IO =
        let v = Array#{1, 2} @ Static;
        let x = lazy v;
        $ARRAY_LOAD$(v, 0) == $ARRAY_LOAD$(force x, 0)

    @test
    def testArrayOfStringForce01(): Bool \ IO =
        let v = Array#{"str", "str2"} @ Static;
        let x = lazy v;
        $ARRAY_LOAD$(v, 0) == $ARRAY_LOAD$(force x, 0)

    @test
    def testTupleForce01(): Bool =
        let x = lazy ("str", "str2");
        let y = force x;
        match y {
            case (s, _) => s == "str"
            case _ => false
        }

    @test
    def testNestedEnumForce01(): Bool =
        let x = lazy (lazy 123);
        123 == force (force x)

    @test
    def testNestedForce02(): Bool =
        let x = lazy (n -> if ((force n) < 0) lazy "neg" else lazy "pos");
        "neg" == force ((force x)(lazy -2))

    @test
    def testNestedForce03(): Bool =
        let x = Poly.Both(lazy 2, lazy "2");
        2 == match x {
            case Poly.Both(a, _) => force a
            case _ => 0
        }

    @test
    def testNestedForce04(): Bool =
        let x = lazy 15;
        Poly.Both(20, 25) == Poly.Both(5 + force x, 10 + force x)

    @test
    def testNestedForce05(): Bool =
        let x = lazy (Poly.This(lazy 5));
        5 == match (force x) {
            case Poly.This(a) => force a
            case _ => -1
        }

    @test
    def testNestedForce01(): Bool =
        let x = lazy Card.Card(Suit.Hearts, Rank.N(4));
        match (force x) {
            case Card.Card(Suit.Hearts, Rank.N(n)) => n == 4
            case _ => false
        }

    @test
    def testPolyForce01(): Bool =
        let x = lazy Poly.This(123);
        let y = force x;
        match y {
            case Poly.This(n) => n == 123
            case _ => false
        }

    @test
    def testPolyForce02(): Bool =
        let x = lazy Poly.That("str");
        let y = force x;
        match y {
            case Poly.That(s) => s == "str"
            case _ => false
        }

    @test
    def testPolyForce03(): Bool =
        let x = lazy Poly.Both("str", 123);
        let y = force x;
        match y {
            case Poly.Both(_, n) => n == 123
            case _ => false
        }

    @test
    def testOptionForce01(): Bool =
        let x = lazy Poly.None;
        (Poly.None: Poly[Unit, Unit]) == force x

    @test
    def testPrecedenceForce01(): Bool =
        let x = lazy 2;
        let y = force x + 1;
        y == 3 and force x == 2

    @test
    def testPrecedenceForce02(): Bool =
        let x = {f = 2};
        let y = lazy x#f;
        let z = force y;
        z == 2

    @test
    def testPrecedenceForce03(): Bool =
        let x = {l = lazy 2};
        let y = force x#l;
        y == 2

    @test
    def testPrecedenceForce04(): Bool =
        let x = ((lazy 2) : Lazy[Int32]);
        let y = ((force x) : Int32);
        y == 2

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
