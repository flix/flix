mod Test.Exp.Fixpoint.Query {

    use Assert.{assertEq, assertMemberOf}

    @Test
    def testQuery01(): Unit \ Assert =
        let r = query foodDb() select f from Food(f) |> Vector.toSet;
        assertEq(expected = Set#{"Apple", "Banana", "Burger", "Carrot", "Grapes", "Pizza", "Potato"}, r)

    @Test
    def testQuery02(): Unit \ Assert =
        let r = query foodDb() select f from Food(f), Fastfood(f) |> Vector.toSet;
        assertEq(expected = Set#{"Banana", "Burger", "Pizza"}, r)

    @Test
    def testQuery03(): Unit \ Assert =
        let r = query foodDb() select f from Food(f), Snack(f) |> Vector.toSet;
        assertEq(expected = Set#{"Banana", "Carrot", "Grapes"}, r)

    @Test
    def testQuery04(): Unit \ Assert =
        let r = query foodDb() select f from Food(f), Vegetable(f) |> Vector.toSet;
        assertEq(expected = Set#{"Apple", "Carrot", "Grapes", "Potato"}, r)

    @Test
    def testQuery05(): Unit \ Assert =
        let r = query foodDb() select f from Healthy(f), not Food(f) |> Vector.toSet;
        assertEq(expected = Set#{"Water"}, r)

    @Test
    def testQuery06(): Unit \ Assert =
        let r = query foodDb() select f from Fastfood(f), Snack(f) |> Vector.toSet;
        assertEq(expected = Set#{"Banana"}, r)

    @Test
    def testQuery07(): Unit \ Assert =
        let r = query foodDb() select f from Snack(f), Healthy(f) |> Vector.toSet;
        assertEq(expected = Set#{"Carrot", "Grapes"}, r)

    @Test
    def testQuery08(): Unit \ Assert =
        let r = query foodDb() select (main, side) from Fastfood(main), Vegetable(side);
        assertMemberOf(("Burger", "Potato"), r)

    @Test
    def testQuery09(): Unit \ Assert =
        let r = query foodDb() select (String.toUpperCase(main), side) from Fastfood(main), Vegetable(side);
        assertMemberOf(("BURGER", "Potato"), r)

    @Test
    def testQuery10(): Unit \ Assert =
        let p1 = pathDb();
        let p2 = #{
            Path(y, x) :- Path(x, y).
        };
        let r = query p1, p2 select (src, dst) from Path(src, dst);
        assertMemberOf((9, 1), r)

    @Test
    def testQuery11(): Unit \ Assert =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 1).
        };
        let r = query p1, p2 select (src, dst) from Path(src, dst);
        assertMemberOf((5, 5), r)

    @Test
    def testQuery12(): Unit \ Assert =
        let p1 = pathDb();
        let p2 = #{
            Edge(9, 10).
        };
        let r = query p1, p2 select (src, dst) from Path(src, dst) where dst >= 10;
        assertMemberOf((1, 10), r)

    @Test
    def testQuery13(): Unit \ Assert =
        let p = #{
            P((1, 2)).
        };
        let r = query p select x from P(x);
        assertEq(expected = Vector#{(1, 2)}, r)

    @Test
    def testQuery14(): Unit \ Assert =
        let p = #{
            Q((1, 2)).
            P(x) :- Q(x).
        };
        let r = query p select x from P(x);
        assertEq(expected = Vector#{(1, 2)}, r)

    @Test
    def testQuery15(): Unit \ Assert =
        let p = #{
            P((1, 2)).
            P((3, 4)).
        };
        let r = query p select x from P(x) where fst(x) > 2;
        assertEq(expected = Vector#{(3, 4)}, r)

    @Test
    def testQuery16(): Unit \ Assert =
        let p = #{
            P((3, 4), 1, 2).
        };
        let r = query p select (x, y, z) from P(x, y, z);
        assertEq(expected = Vector#{((3, 4), 1, 2)}, r)

    @Test
    def testQuery17(): Unit \ Assert =
        let p = #{
            P((3, 4)).
        };
        let r = query p select (x, 1, 2) from P(x);
        assertEq(expected = Vector#{((3, 4), 1, 2)}, r)

    @Test
    def testQuery18(): Unit \ Assert =
        let p = #{
            P(("s", 1i64, 1f64, (2, 3))).
        };
        let r = query p select x from P(x);
        assertEq(expected = Vector#{("s", 1i64, 1f64, (2, 3))}, r)

    @Test
    def testQuery19(): Unit \ Assert =
        let p = #{
            P((1, 2)).
            P((3, 4)).
            P((3, 5)).
        };
        let r = query p select x from P(x);
        assertEq(expected = Vector#{(1, 2), (3, 4), (3, 5)}, r)

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
