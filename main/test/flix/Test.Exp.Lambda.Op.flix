mod Test.Exp.Lambda.Op {

    //
    // Arithmetic Operators
    //

    @Test
    def testLambdaOpPlus01(): Bool =
        let f = (+);
        f(2, 3) == 5

    @Test
    def testLambdaOpPlus02(): Bool =
        List.foldLeft((+), 0, 1 :: 2 :: 3 :: 4 :: Nil) == 10

    @Test
    def testLambdaOpMinus01(): Bool =
        let f = (-);
        f(10, 3) == 7

    @Test
    def testLambdaOpMinus02(): Bool =
        List.map(x -> (-)(10, x), 1 :: 2 :: 3 :: Nil) == (9 :: 8 :: 7 :: Nil)

    @Test
    def testLambdaOpStar01(): Bool =
        let f = (*);
        f(3, 4) == 12

    @Test
    def testLambdaOpStar02(): Bool =
        List.foldLeft((*), 1, 2 :: 3 :: 4 :: Nil) == 24

    @Test
    def testLambdaOpSlash01(): Bool =
        let f = (/);
        f(12, 3) == 4

    @Test
    def testLambdaOpSlash02(): Bool =
        let f = ((/) : (Float64, Float64) -> Float64);
        f(10.0, 4.0) == 2.5

    //
    // Comparison Operators
    //

    @Test
    def testLambdaOpLess01(): Bool =
        let f = (<);
        f(2, 3) == true

    @Test
    def testLambdaOpLess02(): Bool =
        List.filter((<)(5), 1 :: 3 :: 5 :: 7 :: 9 :: Nil) == (7 :: 9 :: Nil)

    @Test
    def testLambdaOpLessEqual01(): Bool =
        let f = (<=);
        f(3, 3) == true

    @Test
    def testLambdaOpLessEqual02(): Bool =
        let f = (<=);
        f(4, 3) == false

    @Test
    def testLambdaOpGreater01(): Bool =
        let f = (>);
        f(5, 3) == true

    @Test
    def testLambdaOpGreater02(): Bool =
        let f = (>);
        f(3, 3) == false

    @Test
    def testLambdaOpGreaterEqual01(): Bool =
        let f = (>=);
        f(5, 3) == true

    @Test
    def testLambdaOpGreaterEqual02(): Bool =
        let f = (>=);
        f(2, 3) == false

    @Test
    def testLambdaOpCompare01(): Bool =
        let f = (<=>);
        f(2, 3) == Comparison.LessThan

    @Test
    def testLambdaOpCompare02(): Bool =
        let f = (<=>);
        f(3, 3) == Comparison.EqualTo

    @Test
    def testLambdaOpCompare03(): Bool =
        let f = (<=>);
        f(5, 3) == Comparison.GreaterThan

    //
    // Equality Operators
    //

    @Test
    def testLambdaOpEqual01(): Bool =
        let f = (==);
        f(3, 3) == true

    @Test
    def testLambdaOpEqual02(): Bool =
        let f = (==);
        f(3, 5) == false

    @Test
    def testLambdaOpNotEqual01(): Bool =
        let f = (!=);
        f(3, 5) == true

    @Test
    def testLambdaOpNotEqual02(): Bool =
        List.filter((!=)(2), 1 :: 2 :: 3 :: 2 :: 4 :: Nil) == (1 :: 3 :: 4 :: Nil)

    //
    // User-Defined Operators
    //

    def +++(x: Int32, y: Int32): Int32 = x + y + 1

    @Test
    def testLambdaOpUserDefined01(): Bool =
        let f = (+++);
        f(2, 3) == 6

    @Test
    def testLambdaOpUserDefined02(): Bool =
        (+++)(5, 10) == 16

    @Test
    def testLambdaOpUserDefined03(): Bool =
        List.foldLeft((+++), 0, 1 :: 2 :: 3 :: Nil) == 9

    //
    // Higher-Order & Type Tests
    //

    @Test
    def testLambdaOpType01(): (Int32, Int32) -> Int32 = (+)

    @Test
    def testLambdaOpHigherOrder01(): Bool =
        let sum = List.foldLeft((+), 0);
        sum(1 :: 2 :: 3 :: 4 :: Nil) == 10

}
