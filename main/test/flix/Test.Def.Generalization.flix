namespace Test/Def/Generalization {

    // NB: Due to monomorphization some of these test cases are erased and never executed.
    // That is acceptable; we just want to ensure that they type check.

    @test
    def testGen01(): a -> a = x -> x

    @test
    def testGen02(): Bool -> Bool = x -> x

    @test
    def testGen03(): Int32 -> Int32 = x -> x

    @test
    def testGen04(): String -> String = x -> x

    @test
    def testGen05(): Array[a] -> Array[a] = x -> x

    @test
    def testGen06(): Option[a] -> Option[a] = x -> x

    @test
    def testGen07(): Result[t, e] -> Result[t, e] = x -> x

    @test
    def testGen09(): {f: a} -> {f: a} = x -> x

    @test
    def testGen10(): {f: a | r} -> {f: a | r} = x -> x

    @test
    def testGen11(): #{Edge(Int, Int)} -> #{Edge(Int, Int)} = x -> x

    @test
    def testGen12(): #{Edge(Int, Int) | r} -> #{Edge(Int, Int) | r} = x -> x

    @test
    def testGen13(): (a -> a) -> (a -> a) = x -> x

    @test
    def testGen14(): (a ~> a) -> (a ~> a) = x -> x

    @test
    def testGen15(): (a -> a & e) -> (a -> a & e) = x -> x

}
