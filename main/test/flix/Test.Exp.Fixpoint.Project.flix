mod Test.Exp.Fixpoint.Project {

    @test
    def testProject01(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project A/1;
        ((query q select x from A(x)) |> Vector.length) == 1

    @test
    def testProject02(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project B/1;
        ((query q select x from B(x)) |> Vector.length) == 1

    @test
    def testProject03(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project C/1;
        ((query q select x from C(x)) |> Vector.length) == 1

}
