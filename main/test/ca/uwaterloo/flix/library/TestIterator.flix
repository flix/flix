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


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testDrop01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.drop(0) |> Iterator.isEmpty

    @test
    def testDrop02(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(0) |> Iterator.toList) == 1 :: 2 :: 3 :: Nil

    @test
    def testDrop03(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(1) |> Iterator.toList) == 2 :: 3 :: Nil

    @test
    def testDrop04(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(2) |> Iterator.toList) == 3 :: Nil

    @test
    def testDrop05(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(3) |> Iterator.toList) == Nil

    @test
    def testDrop06(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(4) |> Iterator.toList) == Nil

}
