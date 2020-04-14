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

    @test
    def testLeq01(): List[a] = Nil

    @test
    def testLeq02(): List[Bool] = Nil

    @test
    def testLeq03(): List[String] = Nil

    @test
    def testLeq04(): List[Option[a]] = Nil

    @test
    def testLeq05(): List[Result[t, e]] = Nil

    @test
    def testLeq06(): Result[Int, a] = Ok(21)

    @test
    def testLeq07(): Result[Int, Bool] = Ok(21)

    @test
    def testLeq08(): Result[Int, String] = Ok(21)

    @test
    def testLeq09(): Result[Int, Result[t, e]] = Ok(21)

    @test
    def testLeq10(): Result[a, Int] = Err(21)

    @test
    def testLeq11(): Result[Bool, Int] = Err(21)

    @test
    def testLeq12(): Result[String, Int] = Err(21)

    @test
    def testLeq13(): Result[Result[t, e], Int] = Err(21)

    @test
    def testLeq14(): List[Result[Int, Option[a]]] = Ok(21) :: Nil

    @test
    def testLeq15(): List[Result[Int, Result[t, e]]] = Ok(21) :: Nil

    @test
    def testLeq16(): List[Result[Int, Map[k, v]]] = Ok(21) :: Nil

}
