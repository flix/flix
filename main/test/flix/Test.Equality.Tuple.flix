mod Test.Equality.Tuple {

    @test
    def testEqTuple01(): Bool = () == ()

    @test
    def testEqTuple02(): Bool = (1, 2) == (1, 2)

    @test
    def testEqTuple03(): Bool = (1, 2, 3) == (1, 2, 3)

    @test
    def testEqTuple04(): Bool = (1, 2, 3, 4) == (1, 2, 3, 4)

    @test
    def testEqTuple05(): Bool = (1, 2, 3, 4, 5) == (1, 2, 3, 4, 5)

    @test
    def testEqTuple06(): Bool = (true, "foo") == (true, "foo")

    @test
    def testEqTuple07(): Bool = (true, 'a', "foo") == (true, 'a', "foo")

    @test
    def testEqTuple08(): Bool = (true, 'a', 42, "foo", ()) == (true, 'a', 42, "foo", ())

    @test
    def testEqTuple09(): Bool = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) == (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64)

    @test
    def testEqTuple10(): Bool = (1, (2, 3)) == (1, (2, 3))

    @test
    def testEqTuple11(): Bool = (1, (2, (3, 4))) == (1, (2, (3, 4)))

    @test
    def testEqTuple12(): Bool = ((1, 2), (3, 4)) == ((1, 2), (3, 4))

    @test
    def testEqTuple13(): Bool = ((1, 2), (3, (4, 5))) == ((1, 2), (3, (4, 5)))

    @test
    def testNeqTuple01(): Bool = (() != ()) == false

    @test
    def testNeqTuple02(): Bool = (1, 2) != (2, 1)

    @test
    def testNeqTuple03(): Bool = (1, 2, 3) != (3, 2, 1)

    @test
    def testNeqTuple04(): Bool = (1, 2, 3, 4) != (1, 2, 4, 3)

    @test
    def testNeqTuple05(): Bool = (1, 2, 3, 4, 5) != (1, 5, 3, 4, 2)

    @test
    def testNeqTuple06(): Bool = (true, "foo") != (false, "foo")

    @test
    def testNeqTuple07(): Bool = (true, 'a', "foo") != (true, 'b', "foo")

    @test
    def testNeqTuple08(): Bool = (true, 'a', 42, "foo", ()) != (true, 'a', 21, "foo", ())

    @test
    def testNeqTuple09(): Bool = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) != (1i8, 2i16, 3i32, 4i64, 5.0f32, 42.0f64)

    @test
    def testNeqTuple10(): Bool = (1, (2, 3)) != (1, (3, 2))

    @test
    def testNeqTuple11(): Bool = (1, (2, (3, 4))) != (1, (3, (2, 4)))

    @test
    def testNeqTuple12(): Bool = ((1, 2), (3, 4)) != ((1, 2), (42, 4))

    @test
    def testNeqTuple13(): Bool = ((1, 2), (3, (4, 5))) != ((1, 2), (3, (42, 5)))

}
