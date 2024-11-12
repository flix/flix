mod Test.Def.Generalization {

    // NB: Some of these test cases are never executed.
    // That is acceptable; we just want to ensure that they type check.

    def _testGen01(): a -> a = x -> x

    @test
    def testGen02(): Bool -> Bool = x -> x

    @test
    def testGen03(): Int32 -> Int32 = x -> x

    @test
    def testGen04(): String -> String = x -> x

    def _testGen05(): Array[a, Static] -> Array[a, Static] = x -> x

    def _testGen06(): Option[a] -> Option[a] = x -> x

    def _testGen07(): Result[e, t] -> Result[e, t] = x -> x

    def _testGen09(): {f = a} -> {f = a} = x -> x

    def _testGen10(): {f = a | r} -> {f = a | r} = x -> x

    @test
    def testGen11(): #{Edge(Int32, Int32)} -> #{Edge(Int32, Int32)} = x -> x

    def _testGen12(): #{Edge(Int32, Int32) | r} -> #{Edge(Int32, Int32) | r} = x -> x

    def _testGen13(): (a -> a) -> (a -> a) = x -> x

    def _testGen14(): (a -> a \ IO) -> (a -> a \ IO) = x -> x

    def _testGen15(): (a -> a \ ef) -> (a -> a \ ef) = x -> x

    def _testGen16(): a -> (b -> a) = x -> (_ -> x)

    def _testGen17(): { | r} -> {x = Int32 | r} = r -> {+x = 21 | r}

    def _testGen18(): {x = Int32 | r} -> { | r} = r -> { -x | r}

    def _testGen19(): {x = Int32 | r} -> {x = Int32 | r}  = r -> { x = 21 | r}

    @test
    def testGen20(): #{A(Int32)} = #{}

    @test
    def testGen21(): #{ A(Int32), B(Int32)} = #{A(21). }

    @test
    def testGen22(): #{ A(Int32), B(Int32), C(Int32)} = #{A(21). B(42). }

    def _testGen23(): #{A(Int32) | r} = #{}

    def _testGen24(): #{ A(Int32), B(Int32) | r} = #{A(21). }

    def _testGen25(): #{ A(Int32), B(Int32), C(Int32) | r} = #{A(21). B(42). }

    def _testGen26(): #{A(Int32), B(Int32) | r} -> #{A(Int32), B(Int32) | r} -> #{A(Int32), B(Int32) | r} = x -> y -> x <+> y

    def _testLeq01(): List[a] = Nil

    @test
    def testLeq02(): List[Bool] = Nil

    @test
    def testLeq03(): List[String] = Nil

    def _testLeq04(): List[Option[a]] = Nil

    def _testLeq05(): List[Result[e, t]] = Nil

    def _testLeq06(): Result[e, Int32] = Ok(21)

    @test
    def testLeq07(): Result[Bool, Int32] = Ok(21)

    @test
    def testLeq08(): Result[String, Int32] = Ok(21)

    def _testLeq09(): Result[Result[e, t], Int32] = Ok(21)

    def _testLeq10(): Result[Int32, a] = Err(21)

    @test
    def testLeq11(): Result[Int32, Bool] = Err(21)

    @test
    def testLeq12(): Result[Int32, String] = Err(21)

    def _testLeq13(): Result[Int32, Result[e, t]] = Err(21)

    def _testLeq14(): List[Result[Option[a], Int32]] = Ok(21) :: Nil

    def _testLeq15(): List[Result[Result[e, t], Int32]] = Ok(21) :: Nil

    def _testLeq16(): List[Result[Map[k, v], Int32]] = Ok(21) :: Nil

    def _testLeq17(): a -> (b -> a) = x -> (_ -> x)

    def _testLeq18(): a -> (a -> a) = x -> (_ -> x)

    def _testLeq19(): Int32 -> (b -> Int32) = x -> (_ -> x)

    def _testLeq20(): a -> (Int32 -> a) = x -> (_ -> x)

    @test
    def testLeq21(): Int32 -> (Int32 -> Int32) = x -> (_ -> x)

    @test
    def testLeq22(): {|} -> {x = Int32} = r -> {+x = 21 | r}

    @test
    def testLeq23(): {x = Int32} -> {|} = r -> {-x | r}

    @test
    def testLeq24(): {x = Int32} -> {x = Int32} = r -> {x = 21 | r}

    def _testLeq25(): (Unit -> b \ ef) -> b \ ef = f -> f()

    def _testLeq26(): (Unit -> b) -> b = f -> f()

    def _testLeq27(): (Unit -> b \ IO) -> b \ IO = f -> f()

    def _testLeq28(): (Unit -> b \ {e1, e2, e3}) -> b \ {e3, e2, e1} = f -> f()

}
