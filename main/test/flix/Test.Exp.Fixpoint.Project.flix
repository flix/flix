mod Test.Exp.Fixpoint.Inject {

    @test
    def testInject01(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p inject A;
        ((query q select x from A(x)) |> Vector.length) == 1

    @test
    def testInject02(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p inject B;
        ((query q select x from B(x)) |> Vector.length) == 1

    @test
    def testInject03(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p inject C;
        ((query q select x from C(x)) |> Vector.length) == 1

}
