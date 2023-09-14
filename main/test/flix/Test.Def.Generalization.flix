mod Test.Def.Generalization {

    // NB: Due to monomorphization some of these test cases are erased and never executed.
    // That is acceptable; we just want to ensure that they type check.

    @Test
    def testGen01(): a -> a = x -> x

    @Test
    def testGen02(): Bool -> Bool = x -> x

    @Test
    def testGen03(): Int32 -> Int32 = x -> x

    @Test
    def testGen04(): String -> String = x -> x

    @Test
    def testGen05(): Array[a, Static] -> Array[a, Static] = x -> x

    @Test
    def testGen06(): Option[a] -> Option[a] = x -> x

    @Test
    def testGen07(): Result[e, t] -> Result[e, t] = x -> x

    @Test
    def testGen09(): {f = a} -> {f = a} = x -> x

    @Test
    def testGen10(): {f = a | r} -> {f = a | r} = x -> x

    @Test
    def testGen11(): #{Edge(Int32, Int32)} -> #{Edge(Int32, Int32)} = x -> x

    @Test
    def testGen12(): #{Edge(Int32, Int32) | r} -> #{Edge(Int32, Int32) | r} = x -> x

    @Test
    def testGen13(): (a -> a) -> (a -> a) = x -> x

    @Test
    def testGen14(): (a -> a \ IO) -> (a -> a \ IO) = x -> x

    @Test
    def testGen15(): (a -> a \ ef) -> (a -> a \ ef) = x -> x

    @Test
    def testGen16(): a -> (b -> a) = x -> (_ -> x)

    @Test
    def testGen17(): { | r} -> {x = Int32 | r} = r -> {+x = 21 | r}

    @Test
    def testGen18(): {x = Int32 | r} -> { | r} = r -> { -x | r}

    @Test
    def testGen19(): {x = Int32 | r} -> {x = Int32 | r}  = r -> { x = 21 | r}

    @Test
    def testGen20(): #{A(Int32)} = #{}

    @Test
    def testGen21(): #{A(Int32), B(Int32)} = #{A(21).}

    @Test
    def testGen22(): #{A(Int32), B(Int32), C(Int32)} = #{A(21). B(42).}

    @Test
    def testGen23(): #{A(Int32) | r} = #{}

    @Test
    def testGen24(): #{A(Int32), B(Int32) | r} = #{A(21).}

    @Test
    def testGen25(): #{A(Int32), B(Int32), C(Int32) | r} = #{A(21). B(42).}

    // TODO: Broken
    //@Test
    //def testGen26(): #{A(Int32) | r1} -> #{B(Int32) | r2} -> #{A(Int32), B(Int32) | r} = x -> y -> x <+> y

    @Test
    def testLeq01(): List[a] = Nil

    @Test
    def testLeq02(): List[Bool] = Nil

    @Test
    def testLeq03(): List[String] = Nil

    @Test
    def testLeq04(): List[Option[a]] = Nil

    @Test
    def testLeq05(): List[Result[e, t]] = Nil

    @Test
    def testLeq06(): Result[e, Int32] = Ok(21)

    @Test
    def testLeq07(): Result[Bool, Int32] = Ok(21)

    @Test
    def testLeq08(): Result[String, Int32] = Ok(21)

    @Test
    def testLeq09(): Result[Result[e, t], Int32] = Ok(21)

    @Test
    def testLeq10(): Result[Int32, a] = Err(21)

    @Test
    def testLeq11(): Result[Int32, Bool] = Err(21)

    @Test
    def testLeq12(): Result[Int32, String] = Err(21)

    @Test
    def testLeq13(): Result[Int32, Result[e, t]] = Err(21)

    @Test
    def testLeq14(): List[Result[Option[a], Int32]] = Ok(21) :: Nil

    @Test
    def testLeq15(): List[Result[Result[e, t], Int32]] = Ok(21) :: Nil

    @Test
    def testLeq16(): List[Result[Map[k, v], Int32]] = Ok(21) :: Nil

    @Test
    def testLeq17(): a -> (b -> a) = x -> (_ -> x)

    @Test
    def testLeq18(): a -> (a -> a) = x -> (_ -> x)

    @Test
    def testLeq19(): Int32 -> (b -> Int32) = x -> (_ -> x)

    @Test
    def testLeq20(): a -> (Int32 -> a) = x -> (_ -> x)

    @Test
    def testLeq21(): Int32 -> (Int32 -> Int32) = x -> (_ -> x)

    @Test
    def testLeq22(): {} -> {x = Int32} = r -> {+x = 21 | r}

    @Test
    def testLeq23(): {x = Int32} -> {} = r -> {-x | r}

    @Test
    def testLeq24(): {x = Int32} -> {x = Int32} = r -> {x = 21 | r}

    @Test
    def testLeq25(): (Unit -> b \ ef) -> b \ ef = f -> f()

    @Test
    def testLeq26(): (Unit -> b) -> b = f -> f()

    @Test
    def testLeq27(): (Unit -> b \ IO) -> b \ IO = f -> f()

    @Test
    def testLeq28(): (Unit -> b \ {e1, e2, e3}) -> b \ {e3, e2, e1} = f -> f()

    // TODO: Broken
    /*@Test
    def testLeq29(x: Choice[String, true, true]): Int32 =
        relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }*/

    def _testLeq30(x: Choice[String, _, true]): Int32 =
        relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    def _testLeq31(x: Choice[String, true, _]): Int32 =
        relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    def _testLeq32(x: Choice[String, _, _]): Int32 =
        relational_choose x {
            case Absent     => 1
            case Present(_) => 2
        }

}
