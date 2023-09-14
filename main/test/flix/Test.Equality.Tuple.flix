mod Test.Equality.Tuple {

    @Test
    def testEqTuple01(): Bool = () == ()

    @Test
    def testEqTuple02(): Bool = (1, 2) == (1, 2)

    @Test
    def testEqTuple03(): Bool = (1, 2, 3) == (1, 2, 3)

    @Test
    def testEqTuple04(): Bool = (1, 2, 3, 4) == (1, 2, 3, 4)

    @Test
    def testEqTuple05(): Bool = (1, 2, 3, 4, 5) == (1, 2, 3, 4, 5)

    @Test
    def testEqTuple06(): Bool = (true, "foo") == (true, "foo")

    @Test
    def testEqTuple07(): Bool = (true, 'a', "foo") == (true, 'a', "foo")

    @Test
    def testEqTuple08(): Bool = (true, 'a', 42, "foo", ()) == (true, 'a', 42, "foo", ())

    @Test
    def testEqTuple09(): Bool = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) == (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64)

    @Test
    def testEqTuple10(): Bool = (1, (2, 3)) == (1, (2, 3))

    @Test
    def testEqTuple11(): Bool = (1, (2, (3, 4))) == (1, (2, (3, 4)))

    @Test
    def testEqTuple12(): Bool = ((1, 2), (3, 4)) == ((1, 2), (3, 4))

    @Test
    def testEqTuple13(): Bool = ((1, 2), (3, (4, 5))) == ((1, 2), (3, (4, 5)))

    @Test
    def testNeqTuple01(): Bool = (() != ()) == false

    @Test
    def testNeqTuple02(): Bool = (1, 2) != (2, 1)

    @Test
    def testNeqTuple03(): Bool = (1, 2, 3) != (3, 2, 1)

    @Test
    def testNeqTuple04(): Bool = (1, 2, 3, 4) != (1, 2, 4, 3)

    @Test
    def testNeqTuple05(): Bool = (1, 2, 3, 4, 5) != (1, 5, 3, 4, 2)

    @Test
    def testNeqTuple06(): Bool = (true, "foo") != (false, "foo")

    @Test
    def testNeqTuple07(): Bool = (true, 'a', "foo") != (true, 'b', "foo")

    @Test
    def testNeqTuple08(): Bool = (true, 'a', 42, "foo", ()) != (true, 'a', 21, "foo", ())

    @Test
    def testNeqTuple09(): Bool = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) != (1i8, 2i16, 3i32, 4i64, 5.0f32, 42.0f64)

    @Test
    def testNeqTuple10(): Bool = (1, (2, 3)) != (1, (3, 2))

    @Test
    def testNeqTuple11(): Bool = (1, (2, (3, 4))) != (1, (3, (2, 4)))

    @Test
    def testNeqTuple12(): Bool = ((1, 2), (3, 4)) != ((1, 2), (42, 4))

    @Test
    def testNeqTuple13(): Bool = ((1, 2), (3, (4, 5))) != ((1, 2), (3, (42, 5)))

}
