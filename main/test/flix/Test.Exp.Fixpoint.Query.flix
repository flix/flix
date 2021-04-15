namespace Test/Exp/Fixpoint/Query {

    @test
    def testQuery01(): Bool & Impure =
        query db() select f from Food(f) |> Array.memberOf("Burger")


    def db(): #{Food(String), Fastfood(String), Healthy(String), Snack(String), Vegetable(String) | r} = #{
        Fastfood("Burger").
        Fastfood("Pizza").

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
