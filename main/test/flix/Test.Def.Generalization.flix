mod Test.Def.Generalization {

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
    def testGen05(): Array[a, Static] -> Array[a, Static] = x -> x

    @test
    def testGen06(): Option[a] -> Option[a] = x -> x

    @test
    def testGen07(): Result[e, t] -> Result[e, t] = x -> x

    @test
    def testGen09(): {f = a} -> {f = a} = x -> x

    @test
    def testGen10(): {f = a | r} -> {f = a | r} = x -> x

    @test
    def testGen11(): #{Edge(Int32, Int32)} -> #{Edge(Int32, Int32)} = x -> x

    @test
    def testGen12(): #{Edge(Int32, Int32) | r} -> #{Edge(Int32, Int32) | r} = x -> x

    @test
    def testGen13(): (a -> a) -> (a -> a) = x -> x

    @test
    def testGen14(): (a -> a \ IO) -> (a -> a \ IO) = x -> x

    @test
    def testGen15(): (a -> a \ ef) -> (a -> a \ ef) = x -> x

    @test
    def testGen16(): a -> (b -> a) = x -> (_ -> x)

    @test
    def testGen17(): { | r} -> {x = Int32 | r} = r -> {+x = 21 | r}

    @test
    def testGen18(): {x = Int32 | r} -> { | r} = r -> { -x | r}

    @test
    def testGen19(): {x = Int32 | r} -> {x = Int32 | r}  = r -> { x = 21 | r}

    @test
    def testGen20(): #{A(Int32)} = #{}

    @test
    def testGen21(): #{A(Int32), B(Int32)} = #{A(21).}

    @test
    def testGen22(): #{A(Int32), B(Int32), C(Int32)} = #{A(21). B(42).}

    @test
    def testGen23(): #{A(Int32) | r} = #{}

    @test
    def testGen24(): #{A(Int32), B(Int32) | r} = #{A(21).}

    @test
    def testGen25(): #{A(Int32), B(Int32), C(Int32) | r} = #{A(21). B(42).}

    // TODO: Broken
    //@test
    //def testGen26(): #{A(Int32) | r1} -> #{B(Int32) | r2} -> #{A(Int32), B(Int32) | r} = x -> y -> x <+> y

    @test
    def testLeq01(): List[a] = Nil

    @test
    def testLeq02(): List[Bool] = Nil

    @test
    def testLeq03(): List[String] = Nil

    @test
    def testLeq04(): List[Option[a]] = Nil

    @test
    def testLeq05(): List[Result[e, t]] = Nil

    @test
    def testLeq06(): Result[e, Int32] = Ok(21)

    @test
    def testLeq07(): Result[Bool, Int32] = Ok(21)

    @test
    def testLeq08(): Result[String, Int32] = Ok(21)

    @test
    def testLeq09(): Result[Result[e, t], Int32] = Ok(21)

    @test
    def testLeq10(): Result[Int32, a] = Err(21)

    @test
    def testLeq11(): Result[Int32, Bool] = Err(21)

    @test
    def testLeq12(): Result[Int32, String] = Err(21)

    @test
    def testLeq13(): Result[Int32, Result[e, t]] = Err(21)

    @test
    def testLeq14(): List[Result[Option[a], Int32]] = Ok(21) :: Nil

    @test
    def testLeq15(): List[Result[Result[e, t], Int32]] = Ok(21) :: Nil

    @test
    def testLeq16(): List[Result[Map[k, v], Int32]] = Ok(21) :: Nil

    @test
    def testLeq17(): a -> (b -> a) = x -> (_ -> x)

    @test
    def testLeq18(): a -> (a -> a) = x -> (_ -> x)

    @test
    def testLeq19(): Int32 -> (b -> Int32) = x -> (_ -> x)

    @test
    def testLeq20(): a -> (Int32 -> a) = x -> (_ -> x)

    @test
    def testLeq21(): Int32 -> (Int32 -> Int32) = x -> (_ -> x)

    @test
    def testLeq22(): {} -> {x = Int32} = r -> {+x = 21 | r}

    @test
    def testLeq23(): {x = Int32} -> {} = r -> {-x | r}

    @test
    def testLeq24(): {x = Int32} -> {x = Int32} = r -> {x = 21 | r}

    @test
    def testLeq25(): (Unit -> b \ ef) -> b \ ef = f -> f()

    @test
    def testLeq26(): (Unit -> b) -> b = f -> f()

    @test
    def testLeq27(): (Unit -> b \ IO) -> b \ IO = f -> f()

    @test
    def testLeq28(): (Unit -> b \ {e1, e2, e3}) -> b \ {e3, e2, e1} = f -> f()

    // TODO: Broken
    /*@test
    def testLeq29(x: Choice[String, true, true]): Int32 =
        relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }*/

    @test
    def testLeq30(): Choice[String, _, true] -> Int32 =
        x -> relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    @test
    def testLeq31(): Choice[String, true, _] -> Int32 =
        x -> relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    @test
    def testLeq32(): Choice[String, _, _] -> Int32 =
        x -> relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

}
