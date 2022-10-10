namespace Test/Exp/Debug {

    @test
    def testUnitDebug01(): Unit = debug(())


    @test
    def testBoolDebug01(): Unit = debug(true)

    @test
    def testBoolDebug02(): Unit = debug(false)


    @test
    def testCharDebug01(): Unit = debug('a')

    @test
    def testCharDebug02(): Unit = debug('\u2194')


    @test
    def testFloat32Debug01(): Unit = debug(21.42f32)


    @test
    def testInt8Debug01(): Unit = debug(42i8)


    @test
    def testInt16Debug01(): Unit = debug(42i16)


    @test
    def testInt32Debug01(): Unit = debug(42i32)


    @test
    def testInt64Debug01(): Unit = debug(42i64)


    @test
    def testBigIntDebug01(): Unit = debug(42ii)


    @test
    def testStringDebug01(): Unit = debug("Hello World!")


    @test
    def testArrayOfIntDebug01(): Unit \ IO = debug([1, 2, 3])


    @test
    def testArrayOfStringDebug01(): Unit \ IO = debug(["a", "b", "c"])


    @test
    def testTupleDebug01(): Unit = debug((true, 'a', "Hello World!"))


    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int32), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    @test
    def testEnumDebug01(): Unit = debug(Card(Hearts, N(5)))


    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @test
    def testPolyDebug01(): Unit = debug(Poly.None)

    @test
    def testPolyDebug02(): Unit = debug(This(123))

    @test
    def testPolyDebug03(): Unit = debug(That("Hello World!"))

    @test
    def testPolyDebug04(): Unit = debug(Both(123, "Hello World!"))

}
