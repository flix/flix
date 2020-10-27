namespace Test/Exp/Lazy {

    // TODO: make better names a la Test.Exp.Force.flix

    @test
    def testLazyCreation01(): Bool & Pure =
        let _ = lazy ();
        true

    @test
    def testLazyCreation02(): Bool & Pure =
        let _ = lazy true;
        true

    @test
    def testLazyCreation03(): Bool & Pure =
        let _ = lazy 'a';
        true

    @test
    def testLazyCreation04(): Bool & Pure =
        let _ = lazy 12.34f32;
        true

    @test
    def testLazyCreation05(): Bool & Pure =
        let _ = lazy 12.34f64;
        true

    @test
    def testLazyCreation06(): Bool & Pure =
        let _ = lazy 12i8;
        true

    @test
    def testLazyCreation07(): Bool & Pure =
        let _ = lazy 12i16;
        true

    @test
    def testLazyCreation08(): Bool & Pure =
        let _ = lazy 12i32;
        true

    @test
    def testLazyCreation09(): Bool & Pure =
        let _ = lazy 12i64;
        true

    @test
    def testLazyCreation10(): Bool & Pure =
        let _ = lazy 12ii;
        true

    @test
    def testLazyCreation11(): Bool & Pure =
        let _ = lazy "string";
        true

    @test
    def testLazyCreation12(): Bool & Impure =
        let array = [1, 2];
        let _ = lazy array;
        true

    @test
    def testLazyCreation13(): Bool & Impure =
        let array = ["str", "str2"];
        let _ = lazy array;
        true

    @test
    def testLazyCreation14(): Bool & Pure =
        let _ = lazy ("str", "str2");
        true

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
    def testLazyCreation15(): Bool & Pure =
        let _ = lazy Card(Hearts, N(4));
        true

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @test
    def testLazyCreation16(): Bool & Pure =
        let _ = lazy This(123);
        true

    @test
    def testLazyCreation17(): Bool & Pure =
        let _ = lazy That("str");
        true

    @test
    def testLazyCreation18(): Bool & Pure =
        let _ = lazy Both("str", 123);
        true
}