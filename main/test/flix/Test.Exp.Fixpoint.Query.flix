namespace Test/Exp/Fixpoint/Query {

    @test
    def testQuery01(): Bool & Impure =
        query db() select f from Food(f) |> Array.toSet == Set#{"Apple", "Banana", "Burger", "Carrot", "Grapes", "Pizza", "Potato"}

    @test
    def testQuery02(): Bool & Impure =
        query db() select f from Food(f), Fastfood(f) |> Array.toSet == Set#{"Banana", "Burger", "Pizza"}

    @test
    def testQuery03(): Bool & Impure =
        query db() select f from Food(f), Snack(f) |> Array.toSet == Set#{"Banana", "Carrot", "Grapes"}

    @test
    def testQuery04(): Bool & Impure =
        query db() select f from Food(f), Vegetable(f) |> Array.toSet == Set#{"Apple", "Carrot", "Grapes", "Potato"}

    @test
    def testQuery05(): Bool & Impure =
        query db() select f from Healthy(f), not Food(f) |> Array.toSet == Set#{"Water"}

    @test
    def testQuery06(): Bool & Impure =
        query db() select f from Fastfood(f), Snack(f) |> Array.toSet == Set#{"Banana"}

    @test
    def testQuery07(): Bool & Impure =
        query db() select f from Snack(f), Healthy(f) |> Array.toSet == Set#{"Carrot", "Grapes"}

    @test
    def testQuery08(): Bool & Impure =
        query db() select (main, side) from Fastfood(main), Vegetable(side) |> Array.memberOf(("Burger", "Potato"))


    def db(): #{Food(String), Fastfood(String), Healthy(String), Snack(String), Vegetable(String) | r} = #{
        Fastfood("Banana").
        Fastfood("Burger").
        Fastfood("Pizza").

        Snack("Banana").
        Snack("Carrot").
        Snack("Grapes").

        Vegetable("Apple").
        Vegetable("Carrot").
        Vegetable("Grapes").
        Vegetable("Potato").

        Healthy("Water").
        Healthy(x) :- Vegetable(x).

        Food(x) :- Fastfood(x).
        Food(x) :- Snack(x).
        Food(x) :- Vegetable(x).
    }

}
