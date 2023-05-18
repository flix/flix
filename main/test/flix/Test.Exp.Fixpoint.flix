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
    def testOpenSchema01(): #{ | r} = testOpenSchema01Helper(#{})
    def testOpenSchema01Helper(p1: #{}): #{ | r} = p1

    @test
    def testOpenSchema02(): #{A() | r} = testOpenSchema02Helper()
    def testOpenSchema02Helper(p1: #{A()}): #{A() | r} = p1

    @test
    def testOpenSchema03():  #{A(), B() | r} = testOpenSchema03Helper(#{A(), B()})
    def testOpenSchema03Helper(p1: #{A(), B()}): #{A(), B() | r} = p1

    @test
    def testOpenSchema04(): #{A() | r} = testOpenSchema04Helper(#{}, #{A()})
    def testOpenSchema04Helper(p1: #{}, p2: #{A()}): #{A() | r} = p1 <+> p2

    @test
    def testOpenSchema05(): #{A() | r} = testOpenSchema05Helper(#{A()}, #{})
    def testOpenSchema05Helper(p1: #{A()}, p2: #{}): #{A() | r} = p1 <+> p2

    @test
    def testOpenSchema06(): #{A(), B() | r} = testOpenSchema06Helper(#{A()}, #{B()})
    def testOpenSchema06Helper(p1: #{A()}, p2: #{B()}): #{A(), B() | r} = p1 <+> p2

    @test
    def testOpenSchema07(): #{B(), A() | r} = testOpenSchema07Helper(#{A()}, #{B()})
    def testOpenSchema07Helper(p1: #{A()}, p2: #{B()}): #{B(), A() | r} = p1 <+> p2

    @test
    def testOpenSchema08(): #{A(), B(), C() | r} = testOpenSchema08Helper(#{A(), B()}, #{B(), C()})
    def testOpenSchema08Helper(p1: #{A(), B()}, p2: #{B(), C()}): #{A(), B(), C() | r} = p1 <+> p2

    @test
    def testOpenSchema09(): #{C(), B(), A() | r} = testOpenSchema09Helper(#{A(), B()}, #{B(), C()})
    def testOpenSchema09Helper(p1: #{A(), B()}, p2: #{B(), C()}): #{C(), B(), A() | r} = p1 <+> p2

}
