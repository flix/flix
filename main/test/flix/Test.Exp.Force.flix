namespace Test/Exp/Force {

    @test
    def testUnitForce01(): Bool & Pure =
        let x = lazy ();
        () == force x

    @test
    def testBoolForce01(): Bool & Pure =
        let x = lazy true;
        true == force x

    @test
    def testCharForce01(): Bool & Pure =
        let x = lazy 'a';
        'a' == force x

    @test
    def testFloat32Force01(): Bool & Pure =
        let value = 12.34f32;
        let x = lazy value;
        value == force x

    @test
    def testFloat64Force01(): Bool & Pure =
        let value = 12.34f64;
        let x = lazy value;
        value == force x

    @test
    def testInt8Force01(): Bool & Pure =
        let value = 12i8;
        let x = lazy value;
        value == force x

    @test
    def testInt16Force01(): Bool & Pure =
        let value = 12i16;
        let x = lazy value;
        value == force x

    @test
    def testInt32Force01(): Bool & Pure =
        let value = 12i32;
        let x = lazy value;
        value == force x

    @test
    def testInt64Force01(): Bool & Pure =
        let value = 12i64;
        let x = lazy value;
        value == force x

    @test
    def testBigIntForce01(): Bool & Pure =
        let value = 12ii;
        let x = lazy value;
        value == force x

    @test
    def testStringForce01(): Bool & Pure =
        let value = "string";
        let x = lazy value;
        value == force x

    @test
    def testArrayOfIntForce01(): Bool & Impure =
        let array = [1, 2];
        let x = lazy array;
        array[0] == (force x)[0]

    @test
    def testArrayOfStringForce01(): Bool & Impure =
        let array = ["str", "str2"];
        let x = lazy array;
        array[0] == (force x)[0]

    @test
    def testTupleForce01(): Bool & Pure =
        let x = lazy ("str", "str2");
        let y = force x;
        match y {
            case (s, _) => s == "str"
            case _ => false
        }

    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    @test
    def testNestedForce01(): Bool & Pure =
        let x = lazy Card(Hearts, N(4));
        match (force x) {
            case Card(Hearts, N(y)) => y == 4
            case _ => false
        }

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @test
    def testPolyForce01(): Bool & Pure =
        let x = lazy This(123);
        let y = force x;
        match y {
            case This(n) => n == 123
            case _ => false
        }

    @test
    def testPolyForce02(): Bool & Pure =
        let x = lazy That("str");
        let y = force x;
        match y {
            case That(s) => s == "str"
            case _ => false
        }

    @test
    def testPolyForce03(): Bool & Pure =
        let x = lazy Both("str", 123);
        let y = force x;
        match y {
            case Both(_, n) => n == 123
            case _ => false
        }
}