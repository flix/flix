namespace Test/Exp/Fixpoint {

    @test
    def testSemiNaive01(): Bool & Impure =
        let db = #{
            Goal("x").
            Edge("y", "x").
            Edge("w", "x").
            Edge("z", "y").
            Edge("w", "y").
            Edge("z", "v").
            Edge("v", "z").
            A("x", 0).
            A("y", 1).
            A("z", 2).
            A("w", 2).
            A("v", 1).
        };
        let pr = #{
            Win(x) :- Goal(x).
            L(y; Set#{x}) :- Edge(y, x), Win(x).
            Win(x) :- A(x, n), L(x; s), if Set.size(s) >= n.
        };
        let result = query db, pr select x from Win(x) |> Array.toSet;
        result == Set#{"x", "y", "w"}

}
