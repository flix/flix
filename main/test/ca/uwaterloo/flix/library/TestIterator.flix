namespace TestIterator {

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testIsEmpty01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.isEmpty

    @test
    def testIsEmpty02(): Bool & Impure =
        not ((1 :: Nil) |> Iterator.toIter |> Iterator.isEmpty)

    @test
    def testIsEmpty03(): Bool & Impure =
        not ((1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.isEmpty)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testCount01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.count(constant(true)) == 0

    @test
    def testCount02(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.count(constant(true)) == 1

    @test
    def testCount03(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.count(constant(true)) == 2

    @test
    def testCount04(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount05(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount06(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount07(): Bool & Impure =
        (1 :: 2 :: 3 ::  Nil) |> Iterator.toIter |> Iterator.count(x -> x < 2) == 1

}
