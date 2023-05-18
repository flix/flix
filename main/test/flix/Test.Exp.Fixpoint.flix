mod Test.Exp.Fixpoint {

    @test
    def testSemiNaive01(): Bool =
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
        let result = query db, pr select x from Win(x) |> List.toSet;
        result == Set#{"x", "y", "w"}

    @test
    def testOpenSchema01(): #{} -> #{ | r} = p1

    @test
    def testOpenSchema02(): #{A()} -> #{A() | r} = p1 -> p1

    @test
    def testOpenSchema03(): #{A(), B()} -> #{A(), B() | r} = p1 -> p1

    @test
    def testOpenSchema04(): #{} -> #{A()} -> #{A() | r} = p1 -> p2 -> p1 <+> p2

    @test
    def testOpenSchema05(): #{A()} -> #{} -> #{A() | r} = p1 -> p2 -> p1 <+> p2

    @test
    def testOpenSchema06(): #{A()} -> #{B()} -> #{A(), B() | r} = p1 -> p2 -> p1 <+> p2

    @test
    def testOpenSchema07(): #{A()} -> #{B()} -> #{B(), A() | r} = p1 -> p2 -> p1 <+> p2

    @test
    def testOpenSchema08(): #{A(), B()} -> #{B(), C()} -> #{A(), B(), C() | r} = p1 -> p2 -> p1 <+> p2

    @test
    def testOpenSchema09(): #{A(), B()} -> #{B(), C()} -> #{C(), B(), A() | r} = p1 -> p2 -> p1 <+> p2

}
