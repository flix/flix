mod Test.Equality.Tuple {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqTuple01(): Unit \ Assert =
        assertEq(expected = (), ())

    @Test
    def testEqTuple02(): Unit \ Assert =
        assertEq(expected = (1, 2), (1, 2))

    @Test
    def testEqTuple03(): Unit \ Assert =
        assertEq(expected = (1, 2, 3), (1, 2, 3))

    @Test
    def testEqTuple04(): Unit \ Assert =
        assertEq(expected = (1, 2, 3, 4), (1, 2, 3, 4))

    @Test
    def testEqTuple05(): Unit \ Assert =
        assertEq(expected = (1, 2, 3, 4, 5), (1, 2, 3, 4, 5))

    @Test
    def testEqTuple06(): Unit \ Assert =
        assertEq(expected = (true, "foo"), (true, "foo"))

    @Test
    def testEqTuple07(): Unit \ Assert =
        assertEq(expected = (true, 'a', "foo"), (true, 'a', "foo"))

    @Test
    def testEqTuple08(): Unit \ Assert =
        assertEq(expected = (true, 'a', 42, "foo", ()), (true, 'a', 42, "foo", ()))

    @Test
    def testEqTuple09(): Unit \ Assert =
        assertEq(expected = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64), (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64))

    @Test
    def testEqTuple10(): Unit \ Assert =
        assertEq(expected = (1, (2, 3)), (1, (2, 3)))

    @Test
    def testEqTuple11(): Unit \ Assert =
        assertEq(expected = (1, (2, (3, 4))), (1, (2, (3, 4))))

    @Test
    def testEqTuple12(): Unit \ Assert =
        assertEq(expected = ((1, 2), (3, 4)), ((1, 2), (3, 4)))

    @Test
    def testEqTuple13(): Unit \ Assert =
        assertEq(expected = ((1, 2), (3, (4, 5))), ((1, 2), (3, (4, 5))))

    @Test
    def testNeqTuple02(): Unit \ Assert =
        assertNeq(unexpected = (1, 2), (2, 1))

    @Test
    def testNeqTuple03(): Unit \ Assert =
        assertNeq(unexpected = (1, 2, 3), (3, 2, 1))

    @Test
    def testNeqTuple04(): Unit \ Assert =
        assertNeq(unexpected = (1, 2, 3, 4), (1, 2, 4, 3))

    @Test
    def testNeqTuple05(): Unit \ Assert =
        assertNeq(unexpected = (1, 2, 3, 4, 5), (1, 5, 3, 4, 2))

    @Test
    def testNeqTuple06(): Unit \ Assert =
        assertNeq(unexpected = (true, "foo"), (false, "foo"))

    @Test
    def testNeqTuple07(): Unit \ Assert =
        assertNeq(unexpected = (true, 'a', "foo"), (true, 'b', "foo"))

    @Test
    def testNeqTuple08(): Unit \ Assert =
        assertNeq(unexpected = (true, 'a', 42, "foo", ()), (true, 'a', 21, "foo", ()))

    @Test
    def testNeqTuple09(): Unit \ Assert =
        assertNeq(unexpected = (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64), (1i8, 2i16, 3i32, 4i64, 5.0f32, 42.0f64))

    @Test
    def testNeqTuple10(): Unit \ Assert =
        assertNeq(unexpected = (1, (2, 3)), (1, (3, 2)))

    @Test
    def testNeqTuple11(): Unit \ Assert =
        assertNeq(unexpected = (1, (2, (3, 4))), (1, (3, (2, 4))))

    @Test
    def testNeqTuple12(): Unit \ Assert =
        assertNeq(unexpected = ((1, 2), (3, 4)), ((1, 2), (42, 4)))

    @Test
    def testNeqTuple13(): Unit \ Assert =
        assertNeq(unexpected = ((1, 2), (3, (4, 5))), ((1, 2), (3, (42, 5))))

}
