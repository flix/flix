namespace Test/Exp/ForYield {

    @test
    def testForYield01(): Bool =
        let l =
            for (x <- 1 :: 2 :: 3 :: Nil)
                yield x + 1;
        l == 2 :: 3 :: 4 :: Nil

}
