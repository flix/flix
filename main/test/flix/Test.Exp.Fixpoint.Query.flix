mod Test.Exp.Fixpoint.Query {

    @Test
    def testQuery01(): Bool =
        query foodDb() select f from Food(f) |> Vector.toSet == Set#{"Apple", "Banana", "Burger", "Carrot", "Grapes", "Pizza", "Potato"}

    @Test
    def testQuery02(): Bool =
        query foodDb() select f from Food(f), Fastfood(f) |> Vector.toSet == Set#{"Banana", "Burger", "Pizza"}

    @Test
    def testQuery03(): Bool =
        query foodDb() select f from Food(f), Snack(f) |> Vector.toSet == Set#{"Banana", "Carrot", "Grapes"}

    @Test
    def testQuery04(): Bool =
        query foodDb() select f from Food(f), Vegetable(f) |> Vector.toSet == Set#{"Apple", "Carrot", "Grapes", "Potato"}

    @Test
    def testQuery05(): Bool =
        query foodDb() select f from Healthy(f), not Food(f) |> Vector.toSet == Set#{"Water"}

    @Test
    def testQuery06(): Bool =
        query foodDb() select f from Fastfood(f), Snack(f) |> Vector.toSet == Set#{"Banana"}

    @Test
    def testQuery07(): Bool =
        query foodDb() select f from Snack(f), Healthy(f) |> Vector.toSet == Set#{"Carrot", "Grapes"}

    @Test
    def testQuery08(): Bool =
        query foodDb() select (main, side) from Fastfood(main), Vegetable(side) |> Vector.memberOf(("Burger", "Potato"))

    @Test
    def testQuery09(): Bool =
        query foodDb() select (String.toUpperCase(main), side) from Fastfood(main), Vegetable(side) |> Vector.memberOf(("BURGER", "Potato"))

    @Test
    def testQuery10(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Path(y, x) :- Path(x, y).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> Vector.memberOf((9, 1))

    @Test
    def testQuery11(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 1).
        };
        query p1, p2 select (src, dst) from Path(src, dst) |> Vector.memberOf((5, 5))

    @Test
    def testQuery12(): Bool =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 10).
        };
        (query p1, p2 select (src, dst) from Path(src, dst) where dst >= 10) |> Vector.memberOf((1, 10))

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
