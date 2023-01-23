namespace Test/Exp/ForEachYield {

    @test
    def testForEachYield01(): List[Int32] =
        foreach (x <- 1 :: 2 :: Nil)
            yield x

}
