mod Test.Exp.Fixpoint.Query {

    @test
    def testQuery01(): Bool =
        query foodDb() select f from Food(f) |> List.toSet == Set#{"Apple", "Banana", "Burger", "Carrot", "Grapes", "Pizza", "Potato"}

    @test
    def testQuery02(): Bool =
        query foodDb() select f from Food(f), Fastfood(f) |> List.toSet == Set#{"Banana", "Burger", "Pizza"}

    @test
    def testQuery03(): Bool =
        query foodDb() select f from Food(f), Snack(f) |> List.toSet == Set#{"Banana", "Carrot", "Grapes"}

    @test
    def testQuery04(): Bool =
        query foodDb() select f from Food(f), Vegetable(f) |> List.toSet == Set#{"Apple", "Carrot", "Grapes", "Potato"}

    @test
    def testQuery05(): Bool =
        query foodDb() select f from Healthy(f), not Food(f) |> List.toSet == Set#{"Water"}

    @test
    def testQuery06(): Bool =
        query foodDb() select f from Fastfood(f), Snack(f) |> List.toSet == Set#{"Banana"}

    @test
    def testQuery07(): Bool =
        query foodDb() select f from Snack(f), Healthy(f) |> List.toSet == Set#{"Carrot", "Grapes"}

    @test
    def testQuery08(): Bool =
        query foodDb() select (main, side) from Fastfood(main), Vegetable(side) |> List.memberOf(("Burger", "Potato"))

    @test
    def testQuery09(): Bool =
        query foodDb() select (String.toUpperCase(main), side) from Fastfood(main), Vegetable(side) |> List.memberOf(("BURGER", "Potato"))

    @test
    def testQuery10(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Path(y, x) :- Path(x, y).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> List.memberOf((9, 1))

    @test
    def testQuery11(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 1).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> List.memberOf((5, 5))

    @test
    def testQuery12(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 10).
        };
        (query p1, p2 select (src, dst) from Path(src, dst) where dst >= 10) |> List.memberOf((1, 10))

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

    def pathDb(): #{Edge(Int32, Int32), Path(Int32, Int32) | r} = #{
        Edge(1, 2). Edge(2, 3). Edge(3, 4). Edge(4, 5).
        Edge(5, 6). Edge(6, 7). Edge(7, 8). Edge(8, 9).

        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    }

}
