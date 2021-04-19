namespace Test/Exp/Fixpoint/Query {

    @test
    def testQuery01(): Bool & Impure =
        query foodDb() select f from Food(f) |> Array.toSet == Set#{"Apple", "Banana", "Burger", "Carrot", "Grapes", "Pizza", "Potato"}

    @test
    def testQuery02(): Bool & Impure =
        query foodDb() select f from Food(f), Fastfood(f) |> Array.toSet == Set#{"Banana", "Burger", "Pizza"}

    @test
    def testQuery03(): Bool & Impure =
        query foodDb() select f from Food(f), Snack(f) |> Array.toSet == Set#{"Banana", "Carrot", "Grapes"}

    @test
    def testQuery04(): Bool & Impure =
        query foodDb() select f from Food(f), Vegetable(f) |> Array.toSet == Set#{"Apple", "Carrot", "Grapes", "Potato"}

    @test
    def testQuery05(): Bool & Impure =
        query foodDb() select f from Healthy(f), not Food(f) |> Array.toSet == Set#{"Water"}

    @test
    def testQuery06(): Bool & Impure =
        query foodDb() select f from Fastfood(f), Snack(f) |> Array.toSet == Set#{"Banana"}

    @test
    def testQuery07(): Bool & Impure =
        query foodDb() select f from Snack(f), Healthy(f) |> Array.toSet == Set#{"Carrot", "Grapes"}

    @test
    def testQuery08(): Bool & Impure =
        query foodDb() select (main, side) from Fastfood(main), Vegetable(side) |> Array.memberOf(("Burger", "Potato"))

    @test
    def testQuery09(): Bool & Impure =
        query foodDb() select (String.toUpperCase(main), side) from Fastfood(main), Vegetable(side) |> Array.memberOf(("BURGER", "Potato"))

    @test
    def testQuery10(): Bool & Impure =
        let p1 = pathDb();
        let p2 = #{
            Path(y, x) :- Path(x, y).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> Array.memberOf((9, 1))

    @test
    def testQuery11(): Bool & Impure =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 1).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> Array.memberOf((5, 5))

    @test
    def testQuery12(): Bool & Impure =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 10).
        };
        (query p1, p2 select (src, dst) from Path(src, dst) where dst >= 10) |> Array.memberOf((1, 10))

    def foodDb(): #{Food(String), Fastfood(String), Healthy(String), Snack(String), Vegetable(String) | r} = #{
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

    def pathDb(): #{Edge(Int, Int), Path(Int, Int) | r} = #{
        Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 5).
        Edge(5, 6). Edge(6, 7). Edge(7, 8). Edge(8, 9).

        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    }

}
