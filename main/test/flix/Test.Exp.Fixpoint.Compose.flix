mod Test.Exp.Fixpoint.Compose {

    @test
    def testFixpointCompose01(): #{ A(Int32, Int32, Int32) } =
        let c1 = #{ A(1, 1, 1). };
        solve c1 <+> c1

    @test
    def testFixpointCompose02(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        solve c1 <+> c1 <+> c1

    @test
    def testFixpointCompose03(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        solve (solve c1 <+> c1) <+> (solve c1 <+> c1)

    @test
    def testFixpointCompose04(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        solve c1 <+> c2 <+> c3

    @test
    def testFixpointCompose05(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        solve (solve (c1 <+> c2) <+> c3)

    @test
    def testFixpointCompose06(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        solve (c1 <+> solve (c2 <+> c3))

    @test
    def testFixpointCompose07(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        let c4 = #{ A(z, y, x) :- A(x, y, z). };
        solve c1 <+> c2 <+> c3 <+> c4

    @test
    def testFixpointCompose08(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        let c4 = #{ A(z, y, x) :- A(x, y, z). };
        solve (solve (c1 <+> c2)) <+> (c3 <+> c4)

    @test
    def testFixpointCompose09(): #{ A(Int32, Int32, Int32)  } =
        let c1 = #{ A(1, 1, 1). };
        let c2 = #{ A(2, 2, 2). };
        let c3 = #{ A(x, y, z) :- A(x, y, z). };
        let c4 = #{ A(z, y, x) :- A(x, y, z). };
        solve (c1 <+> c2) <+> (c2 <+> c1) <+> (c3 <+> c4) <+> (c4 <+> c3)

    @test
    def testFixpointCompose10(): #{ P(Int32, Int32, Int32) } =
        let c1 = #{ P(1, 123, 1). };
        let c2 = #{ P(2, 123, 2). };
        solve c1 <+> c2

    @test
    def testFixpointCompose11(): (#{ P(Int32, Int32, Int32) }, #{ P(Int32, String, Int32) }) =
        let c1 = #{ P(1, 123, 1). };
        let c2 = #{ P(2, 123, 2). };
        let c3 = #{ P(1, "a", 1). };
        let c4 = #{ P(2, "a", 2). };
        let ra = solve c1 <+> c2;
        let rb = solve c3 <+> c4;
        (ra, rb)

    @test
    def testFixpointCompose12(): (#{ P(Int32, Int32, Int32) }, #{ P(Int32, String, Int32) }) =
        let c1 = #{ P(1, 123, 1). };
        let c2 = #{ P(x, 555, z) :- P(x, 555, z). };
        let c3 = #{ P(1, "a", 1). };
        let c4 = #{ P(x, "a", z) :- P(x, "a", z). };
        let ra = solve c1 <+> c2;
        let rb = solve c3 <+> c4;
        (ra, rb)

    @test
    def testFixpointCompose13(): Bool = {
        let ans = query coolioQuack(#{Coolio("quack!").}) select s from Duck(s);
        ans == List#{"quack!"}
    }

    def coolioQuack(p: #{Coolio(String)}): #{Coolio(String), Duck(String) | r} = {
        p <+> #{Duck(s) :- Coolio(s).}
    }

    @test
    def testFixpointCompose14(): Bool = {
        let ans = query duckOutOfNothing(#{}) select s from Duck(s);
        ans == List#{"quack!"}
    }

    def duckOutOfNothing(p: #{}): #{Duck(String) | r} = {
        p <+> #{Duck("quack!").}
    }

    @test
    def testFixpointCompose15(): Bool = {
        let ans = query duckVille(#{Rip(1). Rap(2). Rup(3).}) select s from Duck(s);
        ans == List#{"quack!"}
    }

    def duckVille(p: #{Rip(Int32), Rap(Int32), Rup(Int32)}): #{Rip(Int32), Rap(Int32), Rup(Int32), Duck(String) | r} = {
        p <+> #{Duck("quack!").}
    }

    @test
    def testFixpointCompose16(): Bool = {
        let ans = query noComparison(#{Apples(()).}, #{Oranges(()).}) select s from Apples(s);
        ans == List#{()}
    }

    def noComparison(p1: #{Apples(Unit)}, p2: #{Oranges(Unit)}): #{Apples(Unit), Oranges(Unit) | r} = {
        p1 <+> p2
    }

}
