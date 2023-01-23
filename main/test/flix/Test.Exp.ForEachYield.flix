namespace Test/Exp/Foreach {

    @test
    def testForEachYield01(): List[Int32] =
        foreach (x <- 1 :: 2 :: Nil)
            yield x

}
