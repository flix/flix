mod Test.Def.Generalization {

    ////////////////////////////////////////////////////////////////////////////////
    // Enumerations
    ////////////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testEnum01(): List[a] = Nil

    @CompileTest
    def testEnum02(): List[Bool] = Nil

    @CompileTest
    def testEnum03(): List[String] = Nil

    @CompileTest
    def testEnum04(): List[Option[a]] = Nil

    @CompileTest
    def testEnum05(): List[Result[e, t]] = Nil

    @CompileTest
    def testEnum06(): Result[e, Int32] = Ok(21)

    @CompileTest
    def testEnum07(): Result[Bool, Int32] = Ok(21)

    @CompileTest
    def testEnum08(): Result[String, Int32] = Ok(21)

    @CompileTest
    def testEnum09(): Result[Result[e, t], Int32] = Ok(21)

    @CompileTest
    def testEnum10(): Result[Int32, a] = Err(21)

    @CompileTest
    def testEnum11(): Result[Int32, Bool] = Err(21)

    @CompileTest
    def testEnum12(): Result[Int32, String] = Err(21)

    @CompileTest
    def testEnum13(): Result[Int32, Result[e, t]] = Err(21)

    @CompileTest
    def testEnum14(): List[Result[Option[a], Int32]] = Ok(21) :: Nil

    @CompileTest
    def testEnum15(): List[Result[Result[e, t], Int32]] = Ok(21) :: Nil

    @CompileTest
    def testEnum16(): List[Result[Map[k, v], Int32]] = Ok(21) :: Nil

    ////////////////////////////////////////////////////////////////////////////////
    // Functions
    ////////////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testFun01(): a -> a = x -> x

    @CompileTest
    def testFun02(): Bool -> Bool = x -> x

    @CompileTest
    def testFun03(): Int32 -> Int32 = x -> x

    @CompileTest
    def testFun04(): String -> String = x -> x

    @CompileTest
    def testFun05(): Array[a, Static] -> Array[a, Static] = x -> x

    @CompileTest
    def testFun06(): Option[a] -> Option[a] = x -> x

    @CompileTest
    def testFun07(): Result[e, t] -> Result[e, t] = x -> x

    @CompileTest
    def testFun08(): (a -> a) -> (a -> a) = x -> x

    @CompileTest
    def testFun09(): (a -> a \ IO) -> (a -> a \ IO) = x -> x

    @CompileTest
    def testFun10(): (a -> a \ ef) -> (a -> a \ ef) = x -> x

    @CompileTest
    def testFun11(): a -> (b -> a) = x -> (_ -> x)

    @CompileTest
    def testFun12(): a -> (a -> a) = x -> (_ -> x)

    @CompileTest
    def testFun13(): Int32 -> (b -> Int32) = x -> (_ -> x)

    @CompileTest
    def testFun14(): a -> (Int32 -> a) = x -> (_ -> x)

    @CompileTest
    def testFun15(): Int32 -> (Int32 -> Int32) = x -> (_ -> x)

    @CompileTest
    def testFun16(): (Unit -> b \ ef) -> b \ ef = f -> f()

    @CompileTest
    def testFun17(): (Unit -> b) -> b = f -> f()

    @CompileTest
    def testFun18(): (Unit -> b \ IO) -> b \ IO = f -> f()

    @CompileTest
    def testFun19(): (Unit -> b \ {e1, e2, e3}) -> b \ {e3, e2, e1} = f -> f()

    ////////////////////////////////////////////////////////////////////////////////
    // Records
    ////////////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testRecord01(): { f = a } -> { f = a } = x -> x

    @CompileTest
    def testRecord02(): { f = a | r } -> { f = a | r } = x -> x

    @CompileTest
    def testRecord03(): { | r } -> { x = Int32 | r } = r -> { +x = 21 | r }

    @CompileTest
    def testRecord04(): { x = Int32 | r } -> { | r } = r -> { -x | r }

    @CompileTest
    def testRecord05(): { x = Int32 | r } -> { x = Int32 | r } = r -> { x = 21 | r }

    @CompileTest
    def testRecord06(): { | } -> { x = Int32 } = r -> { +x = 21 | r }

    @CompileTest
    def testRecord07(): { x = Int32 } -> { | } = r -> { -x | r }

    @CompileTest
    def testRecord08(): { x = Int32 } -> { x = Int32 } = r -> { x = 21 | r }

    ////////////////////////////////////////////////////////////////////////////////
    // Schemas
    ////////////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testSchema01(): #{ Edge(Int32, Int32) } -> #{ Edge(Int32, Int32) } = x -> x

    @CompileTest
    def testSchema02(): #{ Edge(Int32, Int32) | r } -> #{ Edge(Int32, Int32) | r } = x -> x

    @CompileTest
    def testSchema03(): #{ A(Int32) } = #{}

    @CompileTest
    def testSchema04(): #{ A(Int32), B(Int32) } = #{ A(21). }

    @CompileTest
    def testSchema05(): #{ A(Int32), B(Int32), C(Int32) } = #{ A(21). B(42). }

    @CompileTest
    def testSchema06(): #{ A(Int32) | r } = #{}

    @CompileTest
    def testSchema07(): #{ A(Int32), B(Int32) | r } = #{ A(21). }

    @CompileTest
    def testSchema08(): #{ A(Int32), B(Int32), C(Int32) | r } = #{ A(21). B(42). }

    @CompileTest
    def testSchema09(): #{ A(Int32), B(Int32) | r } -> #{ A(Int32), B(Int32) | r } -> #{ A(Int32), B(Int32) | r } = x -> y -> x <+> y

}
