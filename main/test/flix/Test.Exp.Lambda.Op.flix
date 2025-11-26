mod Test.Exp.Lambda.Op {

    use Assert.{assertEq, assertTrue, assertFalse};

    //
    // Arithmetic Operators
    //

    @Test
    def testLambdaOpPlus01(): Unit \ Assert =
        let f = (+);
        assertEq(expected = 5, f(2, 3))

    @Test
    def testLambdaOpPlus02(): Unit \ Assert =
        assertEq(expected = 10, List.foldLeft((+), 0, 1 :: 2 :: 3 :: 4 :: Nil))

    @Test
    def testLambdaOpMinus01(): Unit \ Assert =
        let f = (-);
        assertEq(expected = 7, f(10, 3))

    @Test
    def testLambdaOpMinus02(): Unit \ Assert =
        assertEq(expected = 4, List.foldLeft((-), 10, 1 :: 2 :: 3 :: Nil))

    @Test
    def testLambdaOpStar01(): Unit \ Assert =
        let f = (*);
        assertEq(expected = 12, f(3, 4))

    @Test
    def testLambdaOpStar02(): Unit \ Assert =
        assertEq(expected = 24, List.foldLeft((*), 1, 2 :: 3 :: 4 :: Nil))

    @Test
    def testLambdaOpSlash01(): Unit \ Assert =
        let f = (/);
        assertEq(expected = 4, f(12, 3))

    @Test
    def testLambdaOpSlash02(): Unit \ Assert =
        assertEq(expected = 2, List.foldLeft((/), 24, 3 :: 2 :: 2 :: Nil))

    //
    // Comparison Operators
    //

    @Test
    def testLambdaOpLess01(): Unit \ Assert =
        let f = (<);
        assertTrue(f(2, 3))

    @Test
    def testLambdaOpLess02(): Unit \ Assert =
        assertEq(expected = 7 :: 9 :: Nil, List.filter((<)(5), 1 :: 3 :: 5 :: 7 :: 9 :: Nil))

    @Test
    def testLambdaOpLessEqual01(): Unit \ Assert =
        let f = (<=);
        assertTrue(f(3, 3))

    @Test
    def testLambdaOpLessEqual02(): Unit \ Assert =
        let f = (<=);
        assertFalse(f(4, 3))

    @Test
    def testLambdaOpGreater01(): Unit \ Assert =
        let f = (>);
        assertTrue(f(5, 3))

    @Test
    def testLambdaOpGreater02(): Unit \ Assert =
        let f = (>);
        assertFalse(f(3, 3))

    @Test
    def testLambdaOpGreaterEqual01(): Unit \ Assert =
        let f = (>=);
        assertTrue(f(5, 3))

    @Test
    def testLambdaOpGreaterEqual02(): Unit \ Assert =
        let f = (>=);
        assertFalse(f(2, 3))

    @Test
    def testLambdaOpCompare01(): Unit \ Assert =
        let f = (<=>);
        assertEq(expected = Comparison.LessThan, f(2, 3))

    @Test
    def testLambdaOpCompare02(): Unit \ Assert =
        let f = (<=>);
        assertEq(expected = Comparison.EqualTo, f(3, 3))

    @Test
    def testLambdaOpCompare03(): Unit \ Assert =
        let f = (<=>);
        assertEq(expected = Comparison.GreaterThan, f(5, 3))

    //
    // Equality Operators
    //

    @Test
    def testLambdaOpEqual01(): Unit \ Assert =
        let f = (==);
        assertTrue(f(3, 3))

    @Test
    def testLambdaOpEqual02(): Unit \ Assert =
        let f = (==);
        assertFalse(f(3, 5))

    @Test
    def testLambdaOpNotEqual01(): Unit \ Assert =
        let f = (!=);
        assertTrue(f(3, 5))

    @Test
    def testLambdaOpNotEqual02(): Unit \ Assert =
        assertEq(expected = 1 :: 3 :: 4 :: Nil, List.filter((!=)(2), 1 :: 2 :: 3 :: 2 :: 4 :: Nil))

    //
    // List Operators
    //

    @Test
    def testLambdaOpAppend01(): Unit \ Assert =
        let f = (:::);
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, f(1 :: 2 :: Nil, 3 :: 4 :: Nil))

    @Test
    def testLambdaOpAppend02(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil,
                 List.foldLeft((:::), Nil, (1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil))

    @Test
    def testLambdaOpAppend03(): Unit \ Assert =
        let append = (:::);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, append(1 :: Nil, 2 :: 3 :: Nil))

    //
    // User-Defined Operators
    //

    def +++(x: Int32, y: Int32): Int32 = x + y + 1

    @Test
    def testLambdaOpUserDefined01(): Unit \ Assert =
        let f = (+++);
        assertEq(expected = 6, f(2, 3))

    @Test
    def testLambdaOpUserDefined02(): Unit \ Assert =
        assertEq(expected = 16, (+++)(5, 10))

    @Test
    def testLambdaOpUserDefined03(): Unit \ Assert =
        assertEq(expected = 9, List.foldLeft((+++), 0, 1 :: 2 :: 3 :: Nil))

    //
    // Higher-Order & Type Tests
    //

    @Test
    def testLambdaOpType01(): (Int32, Int32) -> Int32 = (+)

    @Test
    def testLambdaOpHigherOrder01(): Unit \ Assert =
        let sum = List.foldLeft((+), 0);
        assertEq(expected = 10, sum(1 :: 2 :: 3 :: 4 :: Nil))

}
