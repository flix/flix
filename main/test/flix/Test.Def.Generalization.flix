// TODO: Unused?

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
    def testGen16(): a -> (b -> a) = x -> (_ -> x)

    @test
    def testGen17(): r -> {x: Int | r} = r -> {+x = 21 | r}

    @test
    def testGen18(): {x: Int | r} -> r = r -> { -x | r}

    @test
    def testGen19(): {x: Int | r} -> {x: Int | r}  = r -> { x = 21 | r}

    @test
    def testGen20(): #{A(Int)} = #{}

    @test
    def testGen21(): #{A(Int), B(Int)} = #{A(21).}

    @test
    def testGen22(): #{A(Int), B(Int), C(Int)} = #{A(21). B(42).}

    @test
    def testGen23(): #{A(Int) | r} = #{}

    @test
    def testGen24(): #{A(Int), B(Int) | r} = #{A(21).}

    @test
    def testGen25(): #{A(Int), B(Int), C(Int) | r} = #{A(21). B(42).}

    @test
    def testGen26(): #{A(Int) | r1} -> #{B(Int) | r2} -> #{A(Int), B(Int) | r} = x -> y -> x <+> y

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

    @test
    def testLeq17(): a -> (b -> a) = x -> (_ -> x)

    @test
    def testLeq18(): a -> (a -> a) = x -> (_ -> x)

    @test
    def testLeq19(): Int -> (b -> Int) = x -> (_ -> x)

    @test
    def testLeq20(): a -> (Int -> a) = x -> (_ -> x)

    @test
    def testLeq21(): Int -> (Int -> Int) = x -> (_ -> x)

    @test
    def testLeq22(): {} -> {x: Int} = r -> {+x = 21 | r}

    @test
    def testLeq23(): {x: Int} -> {} = r -> {-x | r}

    @test
    def testLeq24(): {x: Int} -> {x: Int} = r -> {x = 21 | r}

    @test
    def testLeq25(): (Unit -> b & e) -> b & e = f -> f()

    @test
    def testLeq26(): (Unit -> b) -> b = f -> f()

    @test
    def testLeq27(): (Unit ~> b) ~> b = f -> f()

    @test
    def testLeq28(): (Unit -> b & {e1, e2, e3}) -> b & {e3, e2, e1} = f -> f()

    @test
    def testLeq29(x: Choice[String, true, true]): Int32 =
        choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    @test
    def testLeq30(x: Choice[String, _, true]): Int32 =
        choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    @test
    def testLeq31(x: Choice[String, true, _]): Int32 =
        choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    @test
    def testLeq32(x: Choice[String, _, _]): Int32 =
        choose x {
            case Absent     => 1
            case Present(_) => 2
        }

    pub enum Choice[a, _isAbsent : Bool, _isPresent : Bool] {
        case Absent
        case Present(a)
    }

}
