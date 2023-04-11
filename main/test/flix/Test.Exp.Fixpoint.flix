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

    @Test
    def testOpenSchema01(p1: #{}): #{ | r} = p1

    @Test
    def testOpenSchema02(p1: #{A()}): #{A() | r} = p1

    @Test
    def testOpenSchema03(p1: #{A(), B()}): #{A(), B() | r} = p1

    @Test
    def testOpenSchema04(p1: #{}, p2: #{A()}): #{A() | r} = p1 <+> p2

    @Test
    def testOpenSchema05(p1: #{A()}, p2: #{}): #{A() | r} = p1 <+> p2

    @Test
    def testOpenSchema06(p1: #{A()}, p2: #{B()}): #{A(), B() | r} = p1 <+> p2

    @Test
    def testOpenSchema07(p1: #{A()}, p2: #{B()}): #{B(), A() | r} = p1 <+> p2

    @Test
    def testOpenSchema08(p1: #{A(), B()}, p2: #{B(), C()}): #{A(), B(), C() | r} = p1 <+> p2

    @Test
    def testOpenSchema09(p1: #{A(), B()}, p2: #{B(), C()}): #{C(), B(), A() | r} = p1 <+> p2

}
